#!/usr/bin/env python3
"""Analyze a repo's git worktrees for cleanup candidates.

Emits JSON to stdout describing the resolved paths and each worktree's status.
The skill (SKILL.md) consumes this output, presents it to the user, confirms,
and performs removals. This script is read-only.

The repo is resolved the same way the ~/dotfiles/bin worktree scripts resolve
it: --repo, then WORKTREE_MAIN_REPO in the environment, then the repo
containing the current directory, then WORKTREE_MAIN_REPO in
~/.config/worktrees/config. Worktrees are expected at
<WORKTREE_ROOT>/<repo-name>/, defaulting to ~/worktrees/<repo-name>/.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Literal

Status = Literal[
    "safe",              # PR merged, clean tree, HEAD at merged tip
    "safe-with-dirty",   # PR merged but uncommitted/untracked files present
    "diverged",          # PR merged but HEAD has commits past the merged tip
    "open-pr",           # open PR — still active
    "closed-pr",         # PR closed without merging
    "no-pr",             # no PR — likely local-only work
    "prunable",          # worktree dir missing; git worktree prune will clean it up
]


@dataclass(frozen=True)
class Config:
    """The resolved paths every other step works from."""

    main_repo: Path
    worktree_base: Path


@dataclass(frozen=True)
class Worktree:
    path: str
    branch: str
    head: str
    is_prunable: bool


@dataclass(frozen=True)
class PullRequest:
    number: int
    title: str
    url: str
    state: str
    merged_at: str | None
    head_ref_oid: str | None


@dataclass(frozen=True)
class Analysis:
    path: str
    name: str
    branch: str
    status: Status
    pr: PullRequest | None
    has_modified: bool
    has_untracked: bool
    commits_past_merge: int
    rationale: str


def run(cmd: list[str], cwd: Path | None = None) -> tuple[int, str, str]:
    res = subprocess.run(cmd, cwd=cwd, capture_output=True, text=True)
    return res.returncode, res.stdout, res.stderr


# ----------------------------------------------------------------- config


def config_file() -> Path:
    if override := os.environ.get("WORKTREE_CONFIG"):
        return Path(override)
    base = os.environ.get("XDG_CONFIG_HOME") or str(Path.home() / ".config")
    return Path(base) / "worktrees" / "config"


def expand_home(value: str) -> str:
    """Expand a leading ~/ or $HOME/ — the only expansion the config format has."""
    for prefix in ("~/", "$HOME/"):
        if value.startswith(prefix):
            return str(Path.home() / value[len(prefix):])
    return value


def config_values(path: Path) -> dict[str, str]:
    """Parse the KEY=value config file. A missing file is an empty mapping."""
    if not path.is_file():
        return {}
    values: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        line = line.split("#", 1)[0]
        if "=" not in line:
            continue
        key, _, value = line.partition("=")
        value = value.strip().strip("\"'")
        if value:
            values[key.strip()] = expand_home(value)
    return values


def repo_from_cwd() -> Path | None:
    """The main checkout of the repo containing the cwd, or None."""
    code, out, _ = run(["git", "rev-parse", "--path-format=absolute", "--git-common-dir"])
    if code != 0 or not out.strip():
        return None
    common = Path(out.strip())
    if common.name == ".git":
        return common.parent
    code, out, _ = run(["git", "rev-parse", "--show-toplevel"])
    return Path(out.strip()) if code == 0 and out.strip() else None


def resolve_config(repo_arg: str | None) -> Config | None:
    """Resolve the repo and its worktree directory, or None when no repo is found."""
    values = config_values(config_file())

    candidates = [
        repo_arg,
        os.environ.get("WORKTREE_MAIN_REPO"),
    ]
    repo = next((Path(expand_home(c)) for c in candidates if c), None)
    if repo is None:
        repo = repo_from_cwd() or (
            Path(values["WORKTREE_MAIN_REPO"]) if "WORKTREE_MAIN_REPO" in values else None
        )
    if repo is None:
        return None

    root = os.environ.get("WORKTREE_ROOT") or values.get("WORKTREE_ROOT")
    worktree_root = Path(expand_home(root)) if root else Path.home() / "worktrees"
    return Config(main_repo=repo, worktree_base=worktree_root / repo.name)


# ---------------------------------------------------------------- analysis


def list_worktrees(config: Config) -> list[Worktree]:
    code, out, err = run(["git", "worktree", "list", "--porcelain"], cwd=config.main_repo)
    if code != 0:
        print(f"git worktree list failed: {err}", file=sys.stderr)
        return []
    worktrees: list[Worktree] = []
    cur: dict[str, str] = {}
    prunable = False
    lines = out.splitlines() + [""]  # trailing blank to flush last record
    for line in lines:
        if not line.strip():
            if cur.get("worktree") and str(config.worktree_base) in cur["worktree"]:
                worktrees.append(Worktree(
                    path=cur["worktree"],
                    branch=cur.get("branch", "").removeprefix("refs/heads/"),
                    head=cur.get("HEAD", ""),
                    is_prunable=prunable,
                ))
            cur = {}
            prunable = False
            continue
        if line == "prunable":
            prunable = True
            continue
        if line == "detached":
            cur["detached"] = ""
            continue
        key, _, val = line.partition(" ")
        cur[key] = val
    return worktrees


def find_pr(branch: str, main_repo: Path) -> PullRequest | None:
    if not branch:
        return None
    code, out, _ = run([
        "gh", "pr", "list",
        "--head", branch,
        "--state", "all",
        "--json", "number,title,url,state,mergedAt,headRefOid",
        "--limit", "10",
    ], cwd=main_repo)
    if code != 0:
        return None
    try:
        prs = json.loads(out)
    except json.JSONDecodeError:
        return None
    if not prs:
        return None
    # Prefer MERGED > OPEN > CLOSED, then newest first.
    priority = {"MERGED": 0, "OPEN": 1, "CLOSED": 2}
    prs.sort(key=lambda p: (priority.get(p["state"], 3), -p["number"]))
    pr = prs[0]
    return PullRequest(
        number=pr["number"],
        title=pr["title"],
        url=pr["url"],
        state=pr["state"],
        merged_at=pr.get("mergedAt"),
        head_ref_oid=pr.get("headRefOid"),
    )


def working_tree_status(path: Path) -> tuple[bool, bool]:
    """Return (has_modified_or_staged, has_untracked)."""
    code, out, _ = run(["git", "status", "--porcelain"], cwd=path)
    if code != 0:
        return False, False
    has_modified = False
    has_untracked = False
    for line in out.splitlines():
        if line.startswith("??"):
            has_untracked = True
        elif line.strip():
            has_modified = True
    return has_modified, has_untracked


def count_commits_past(path: Path, base_sha: str) -> int:
    code, out, _ = run(["git", "rev-list", "--count", f"{base_sha}..HEAD"], cwd=path)
    if code != 0:
        return -1
    try:
        return int(out.strip())
    except ValueError:
        return -1


def analyze(wt: Worktree, main_repo: Path) -> Analysis:
    name = Path(wt.path).name
    if wt.is_prunable or not Path(wt.path).exists():
        return Analysis(
            path=wt.path, name=name, branch=wt.branch,
            status="prunable", pr=None,
            has_modified=False, has_untracked=False, commits_past_merge=0,
            rationale="Worktree directory missing; git worktree prune will clean it up.",
        )

    path = Path(wt.path)
    pr = find_pr(wt.branch, main_repo)
    has_mod, has_untr = working_tree_status(path)

    if pr is None:
        return Analysis(
            path=wt.path, name=name, branch=wt.branch,
            status="no-pr", pr=None,
            has_modified=has_mod, has_untracked=has_untr, commits_past_merge=0,
            rationale="No PR found for this branch — likely active local work.",
        )

    if pr.state == "OPEN":
        return Analysis(
            path=wt.path, name=name, branch=wt.branch,
            status="open-pr", pr=pr,
            has_modified=has_mod, has_untracked=has_untr, commits_past_merge=0,
            rationale=f"PR #{pr.number} is still open.",
        )

    if pr.state == "CLOSED":
        return Analysis(
            path=wt.path, name=name, branch=wt.branch,
            status="closed-pr", pr=pr,
            has_modified=has_mod, has_untracked=has_untr, commits_past_merge=0,
            rationale=f"PR #{pr.number} closed without merging.",
        )

    # MERGED
    commits = count_commits_past(path, pr.head_ref_oid) if pr.head_ref_oid else 0
    if commits > 0:
        return Analysis(
            path=wt.path, name=name, branch=wt.branch,
            status="diverged", pr=pr,
            has_modified=has_mod, has_untracked=has_untr, commits_past_merge=commits,
            rationale=(
                f"PR #{pr.number} merged, but {commits} commit(s) on HEAD past the merged tip "
                "— possible unpushed follow-up work."
            ),
        )
    if has_mod or has_untr:
        parts = []
        if has_mod:
            parts.append("modified/staged")
        if has_untr:
            parts.append("untracked")
        return Analysis(
            path=wt.path, name=name, branch=wt.branch,
            status="safe-with-dirty", pr=pr,
            has_modified=has_mod, has_untracked=has_untr, commits_past_merge=0,
            rationale=f"PR #{pr.number} merged. Working tree has {' and '.join(parts)} files.",
        )
    return Analysis(
        path=wt.path, name=name, branch=wt.branch,
        status="safe", pr=pr,
        has_modified=False, has_untracked=False, commits_past_merge=0,
        rationale=f"PR #{pr.number} merged; clean tree at the merged tip.",
    )


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--repo",
        help="Path to the repo's main checkout (default: resolved from the environment, cwd, or config)",
    )
    args = parser.parse_args()

    config = resolve_config(args.repo)
    if config is None:
        print(
            "No repo to analyze. Pass --repo, run from inside a checkout, or set "
            f"WORKTREE_MAIN_REPO in the environment or {config_file()}.",
            file=sys.stderr,
        )
        raise SystemExit(2)

    worktrees = list_worktrees(config)
    print(json.dumps({
        "main_repo": str(config.main_repo),
        "worktree_base": str(config.worktree_base),
        "worktrees": [asdict(analyze(wt, config.main_repo)) for wt in worktrees],
    }, indent=2))


if __name__ == "__main__":
    main()

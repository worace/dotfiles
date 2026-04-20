#!/usr/bin/env python3
"""Analyze worktrees under ~/worktrees/agent_services/ for cleanup candidates.

Emits JSON to stdout describing each worktree's status. The skill (SKILL.md)
consumes this output, presents it to the user, confirms, and performs removals.
This script is read-only.
"""

from __future__ import annotations

import json
import subprocess
import sys
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Literal

MAIN_REPO = Path.home() / "code" / "agent_services"
WORKTREE_BASE = Path.home() / "worktrees" / "agent_services"

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


def list_worktrees() -> list[Worktree]:
    code, out, err = run(["git", "worktree", "list", "--porcelain"], cwd=MAIN_REPO)
    if code != 0:
        print(f"git worktree list failed: {err}", file=sys.stderr)
        return []
    worktrees: list[Worktree] = []
    cur: dict[str, str] = {}
    prunable = False
    lines = out.splitlines() + [""]  # trailing blank to flush last record
    for line in lines:
        if not line.strip():
            if cur.get("worktree") and str(WORKTREE_BASE) in cur["worktree"]:
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


def find_pr(branch: str) -> PullRequest | None:
    if not branch:
        return None
    code, out, _ = run([
        "gh", "pr", "list",
        "--head", branch,
        "--state", "all",
        "--json", "number,title,url,state,mergedAt,headRefOid",
        "--limit", "10",
    ], cwd=MAIN_REPO)
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


def analyze(wt: Worktree) -> Analysis:
    name = Path(wt.path).name
    if wt.is_prunable or not Path(wt.path).exists():
        return Analysis(
            path=wt.path, name=name, branch=wt.branch,
            status="prunable", pr=None,
            has_modified=False, has_untracked=False, commits_past_merge=0,
            rationale="Worktree directory missing; git worktree prune will clean it up.",
        )

    path = Path(wt.path)
    pr = find_pr(wt.branch)
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
    worktrees = list_worktrees()
    analyses = [analyze(wt) for wt in worktrees]
    print(json.dumps([asdict(a) for a in analyses], indent=2))


if __name__ == "__main__":
    main()

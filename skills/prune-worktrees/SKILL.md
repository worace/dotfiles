---
name: prune-worktrees
description: Find and remove finished git worktrees under the shared worktree directory (~/worktrees/<repo-name>/). Use whenever the user wants to clean up, prune, audit, or remove old worktrees — especially after PRs have been merged. Classifies each worktree (merged-and-clean, merged-but-dirty, merged-but-diverged, open-PR, no-PR, prunable), presents candidates, confirms with the user, then removes worktree directories and local branches.
---

# Prune worktrees

Clean up worktrees that have served their purpose. Default signal of "done": the branch's PR was merged and there's no work past the merged tip.

Which repo is meant comes from the analyzer, not from you: it resolves `--repo`, then `WORKTREE_MAIN_REPO` in the environment, then the repo containing the cwd, then `~/.config/worktrees/config`. Never hardcode a repo path — read `main_repo` and `worktree_base` out of the analyzer's output and use those.

## When to use

Trigger when the user asks to:
- "clean up worktrees", "prune worktrees", "remove old worktrees"
- "which worktrees can I delete?"
- anything involving a worktree audit or cleanup

## Steps

### 1. Analyze

Run the analyzer and parse its JSON output. Add `--repo <path>` when the user named a repo other than the one the cwd is in:

```bash
python3 ~/.claude/skills/prune-worktrees/scripts/analyze_worktrees.py
```

The output is an object with `main_repo`, `worktree_base`, and `worktrees`. If the analyzer exits 2, it found no repo — ask the user which one they mean rather than guessing.

Each entry in `worktrees` has:
- `path`, `name`, `branch`
- `status` — one of: `safe`, `safe-with-dirty`, `diverged`, `open-pr`, `closed-pr`, `no-pr`, `prunable`
- `pr` — PR metadata (number, title, url, state, merged_at) if a PR was found
- `has_modified`, `has_untracked`, `commits_past_merge`
- `rationale` — human-readable one-line reason

Status meanings:

| status | meaning | default action |
|---|---|---|
| `safe` | PR merged, clean tree, HEAD at merged tip | **include for removal** |
| `prunable` | worktree dir already missing | **include for removal** (just prune) |
| `safe-with-dirty` | PR merged, but uncommitted/untracked files | surface with warning, ask per-item |
| `diverged` | PR merged, but commits past merge tip | surface as "do you want to do more work here?" — do NOT include by default |
| `open-pr` | PR still open | skip (mention briefly) |
| `closed-pr` | PR closed without merging | skip (mention briefly) |
| `no-pr` | no PR for this branch | skip (mention briefly) |

### 2. Present

Show the user a grouped summary. Keep it tight:

```
Safe to remove:
  - codex-review-tweak    PR #1234 merged
  - stats-tweaks          PR #1240 merged
  - report-stats-page     (already prunable)

Merged but has uncommitted changes — review:
  - coordinator-experiments   PR #1230 merged, 3 modified files

Merged but HEAD is past the merge — do you want to keep working here?
  - graph-test-speed   PR #1220 merged, 2 commits past merge tip

Still active (skipping):
  - python-modularization   open PR #1244
  - user-outbox-integration no PR
```

For `safe-with-dirty`, list the dirty files briefly (run `git -C <path> status --short`) so the user sees what'd be lost.

For `diverged`, ask explicitly: "Keep working in `<name>`, or include for removal?" If the user says remove, treat it like `safe` going forward.

### 3. Confirm

Ask which to remove. Default selection = everything in `safe` + `prunable`. Let the user add, remove, or take the whole default.

### 4. Remove

For each worktree the user confirmed, run these as **separate** bash calls so one failure doesn't block the rest. `<main-repo>` is the `main_repo` value from step 1; all paths are absolute, so run from any cwd.

```bash
git -C <main-repo> worktree remove <path>
```

If the worktree is dirty and the user authorized removal anyway, use `--force`:

```bash
git -C <main-repo> worktree remove --force <path>
```

Then delete the local branch. Because PRs are usually squash-merged, `git branch -d` will often reject it as "not merged"; since we've already confirmed via `gh` that the PR was merged, use `-D`:

```bash
git -C <main-repo> branch -D <branch>
```

Finally, once at the end:

```bash
git -C <main-repo> worktree prune
```

Report a one-line summary: `Removed N worktrees, deleted N branches, pruned admin entries.`

## Scope and safety

- Only operate on worktrees the analyzer returned. It filters to `worktree_base`, which deliberately excludes the main checkout and any agent-managed worktree pool (`<repo>/.claude/worktrees/`, `~/.codex/worktrees/`).
- Never skip the confirmation step, even if everything looks safe.
- If `gh` isn't authenticated (the analyzer returns `no-pr` for everything), stop and tell the user to run `gh auth status` rather than deleting based on missing PR data.

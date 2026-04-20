---
name: prune-worktrees
description: Find and remove finished git worktrees under ~/worktrees/agent_services/. Use whenever the user wants to clean up, prune, audit, or remove old worktrees — especially after PRs have been merged. Classifies each worktree (merged-and-clean, merged-but-dirty, merged-but-diverged, open-PR, no-PR, prunable), presents candidates, confirms with the user, then removes worktree directories and local branches.
---

# Prune Agent Services Worktrees

Clean up worktrees in `~/worktrees/agent_services/` that have served their purpose. Default signal of "done": the branch's PR was merged and there's no work past the merged tip.

## When to use

Trigger when the user asks to:
- "clean up worktrees", "prune worktrees", "remove old worktrees"
- "which worktrees can I delete?"
- anything involving a worktree audit or cleanup

## Steps

### 1. Analyze

Run the analyzer and parse its JSON output:

```bash
python3 ~/.claude/skills/prune-worktrees/scripts/analyze_worktrees.py
```

Each entry has:
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
  - kg-stats-page         (already prunable)

Merged but has uncommitted changes — review:
  - kg-coordinator-experiments   PR #1230 merged, 3 modified files

Merged but HEAD is past the merge — do you want to keep working here?
  - neo4j-test-speed   PR #1220 merged, 2 commits past merge tip

Still active (skipping):
  - tach-python-modularization   open PR #1244
  - user-outbox-integration      no PR
```

For `safe-with-dirty`, list the dirty files briefly (run `git -C <path> status --short`) so the user sees what'd be lost.

For `diverged`, ask explicitly: "Keep working in `<name>`, or include for removal?" If the user says remove, treat it like `safe` going forward.

### 3. Confirm

Ask which to remove. Default selection = everything in `safe` + `prunable`. Let the user add, remove, or take the whole default.

### 4. Remove

For each worktree the user confirmed, run these as **separate** bash calls so one failure doesn't block the rest. All paths are absolute; run from any cwd.

```bash
git -C /Users/horace/code/agent_services worktree remove <path>
```

If the worktree is dirty and the user authorized removal anyway, use `--force`:

```bash
git -C /Users/horace/code/agent_services worktree remove --force <path>
```

Then delete the local branch. Because PRs are usually squash-merged, `git branch -d` will often reject it as "not merged"; since we've already confirmed via `gh` that the PR was merged, use `-D`:

```bash
git -C /Users/horace/code/agent_services branch -D <branch>
```

Finally, once at the end:

```bash
git -C /Users/horace/code/agent_services worktree prune
```

Report a one-line summary: `Removed N worktrees, deleted N branches, pruned admin entries.`

## Scope and safety

- Only operate on worktrees under `~/worktrees/agent_services/`. Ignore `~/code/agent_services/.claude/worktrees/`, `~/.codex/worktrees/`, and the main checkout at `~/code/agent_services`. The analyzer already filters to the right scope.
- Never skip the confirmation step, even if everything looks safe.
- If `gh` isn't authenticated (the analyzer returns `no-pr` for everything), stop and tell the user to run `gh auth status` rather than deleting based on missing PR data.

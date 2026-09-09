# Recover after a supervisor interruption

Use this runbook when the Codex/Claude supervisor disconnects, its cwd changes, or its
coordinating worktree is pruned. Treat implementer worktrees, GitHub PRs, and tmux panes as
the durable state; do not recreate work from memory.

Until the inventory below is complete, do not send keys, restart a worker harness, provision worktrees,
push branches, or create PRs.

## 1. Re-establish a safe control directory

Move to the canonical repository worktree (usually the main checkout), then read its current
`AGENTS.md`/`CLAUDE.md` and this skill again. Confirm the worktree is clean before fetching and
updating main. A replacement supervisor may inherit different repository instructions from
the lost cwd.

## 2. Inventory before provisioning anything

Gather all three views:

```bash
git worktree list --porcelain
tmux list-windows -t dev -F '#{window_name} #{pane_current_path} #{pane_current_command}'
gh pr list --state all --search '<epic-or-branch-prefix>' \
  --json number,title,url,state,isDraft,baseRefName,headRefName,headRefOid
```

Match by branch/window name. Never run `new-worktree` merely because the supervisor's local
scratchpad disappeared.

For every surviving implementer worktree, record:

```bash
git -C <worktree> status --short
git -C <worktree> log --oneline --decorate -3
git -C <worktree> rev-parse --abbrev-ref HEAD
git -C <worktree> rev-list --left-right --count origin/<expected-base>...HEAD
gh pr list --state all --head <branch> --json number,url,state,isDraft,baseRefName,headRefOid
```

This prevents duplicate commits, pushes, and PRs after reconnecting.

## 3. Recover each lane by observed state

- **Worktree + tmux pane survive:** capture at least 80 lines of scrollback. Classify busy,
  idle, question, background shell, or composer draft using `tmux-driving.md`; continue the
  existing session with its warm context.
- **Worktree survives, tmux pane is gone:** inspect Git/PR state first, recover the original
  harness/model/policy from the launch spec (or ask the user if absent), then open a new pane in
  that exact worktree and start it through `scripts/launch-worker.sh` with a recovery prompt
  containing the ticket, locked decisions, current branch/PR/SHA, completed checks, and remaining
  exit criteria. Do not create a replacement branch.
- **PR survives but local worktree is gone:** recreate a worktree from the PR head branch only
  after verifying no other registered worktree owns it. Resume from the remote head; do not
  replay remembered edits.
- **Only an unpushed dirty worktree survives:** preserve it. Restart the implementer there and
  have it inspect the diff before acting.
- **Nothing survives:** only then provision a replacement from the verified dependency/base.

Do not send keys into a pane whose cwd no longer exists. Do not rewrite a published branch
unless the user explicitly authorizes rebase/force-push.

## 4. Rebuild supervision

Recreate the monitor from the live window list and put its state under a stable temporary
directory such as `/private/tmp/<epic>-supervisor/watch-state`. Rebuild its per-window harness
map from launch specs; unknown/missing adapters are observe-only. Stale `.pr` markers from the
lost worktree are not authoritative; verify PRs via `gh`.

If an idle composer contains a stale draft from before the disconnect, decide whether it is
still correct. Clear and retype only via the safe procedure in `tmux-driving.md`; never submit
it blindly.

## 5. Reconcile dependencies and tell the user

Fetch current main, verify which base PRs merged during the interruption, and inspect migration
heads/file collisions again before starting downstream work. Published descendants may require
retargeting or an explicitly authorized rebase; unpublished descendants can normally rebase
before their first push.

Report what survived, what was resumed, any lost transient checks/monitor state, and any branch
whose history or base now needs a user-authorized change.

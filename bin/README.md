# bin

Scripts symlinked into `~/.local/bin`. Most stand alone; the worktree and tmux
group shares `lib/worktree-lib.sh`.

## Worktree scripts

| script | does |
|---|---|
| `new-worktree <name\|linear-url> [base-ref]` | create a worktree, branch, and bootstrap it |
| `remove-worktree <name>` / `--list` | remove a worktree and optionally its branch |
| `oneshot <linear-url\|ticket-id>` | provision a worktree and launch a Claude session in it that implements the ticket |
| `trwt` | rename the current tmux window to the current worktree's name |
| `tmux-workspace` | bring up the working set of tmux sessions |
| `tmux-snapshot` / `tmux-restore` | save and restore the open session/window layout |

Worktrees live at `$WORKTREE_ROOT/<repo-name>/<branch>`, i.e.
`~/worktrees/<repo-name>/<branch>` by default.

### Which repo they act on

None of these scripts names a project. `new-worktree`, `remove-worktree`, and
`oneshot` resolve the repo in this order:

1. `WORKTREE_MAIN_REPO` in the environment
2. the repo containing the current directory — from a linked worktree this
   resolves to its main checkout, so the next worktree lands alongside it
3. `WORKTREE_MAIN_REPO` from the config file

`tmux-workspace` skips step 2: it runs at login, where the cwd carries no
intent about which repo is meant, so it uses the configured repo only. With
none configured it builds just the dotfiles session.

### Config file

`~/.config/worktrees/config` (or `$WORKTREE_CONFIG`, or under
`$XDG_CONFIG_HOME`). Uncommitted and per-machine — this is where a project's
name and paths live instead of in this repo.

```
# every key optional; `#` starts a comment
WORKTREE_MAIN_REPO=~/code/my-repo   # default repo when cwd isn't in one
WORKTREE_ROOT=~/worktrees           # holds one subdirectory per repo
```

Values take a leading `~/` or `$HOME/` and nothing else — the parser doesn't
eval. Environment variables of the same name win over the file.

### Bootstrap hooks

After creating a worktree, `new-worktree` looks for a setup hook, in order:

1. `<repo>/.worktree-setup` — for a project that can own its own setup
2. `~/.config/worktrees/hooks/<repo-name>` — for one that can't, or whose setup
   shouldn't be committed

Either must be executable. It runs with the new worktree as its cwd and
`WORKTREE_PATH`, `WORKTREE_BRANCH`, and `WORKTREE_MAIN_REPO` in its
environment; a nonzero exit fails the whole provision. A hook owns bootstrap
entirely, so anything project-shaped — private env symlinks, monorepo library
syncing, typecheck smoke tests — belongs in one.

With no hook, `new-worktree` installs from whichever lockfiles the repo has
(pnpm, yarn, npm, uv, poetry, cargo) and reports on what it produced.

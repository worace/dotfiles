# New machine setup

Getting this configuration onto a fresh machine: symlinks, `bin` on `PATH`,
agent skills, and the handful of files that are deliberately **not** in this
repo.

`README.md` covers installing the underlying packages (zsh, tmux, ripgrep,
Emacs, Rust, fonts, terminal themes) on Ubuntu. This file covers wiring the
dotfiles up once those exist, and applies to macOS and Linux alike.

## 1. Clone

```bash
git clone git@github.com:worace/dotfiles.git ~/dotfiles
```

Everything below assumes `~/dotfiles`. The skill symlinks are relative
(`../../dotfiles/skills/<name>`), so that path is load-bearing — a clone
somewhere else needs those links rebuilt by hand.

## 2. Symlinks

`setup.sh` covers the first group but is Ubuntu-flavoured (it also installs
packages), so on macOS run these directly:

```bash
ln -s ~/dotfiles/.zshrc            ~/.zshrc
ln -s ~/dotfiles/.gitconfig        ~/.gitconfig
ln -s ~/dotfiles/.system_gitignore ~/.gitignore
ln -s ~/dotfiles/.gemrc            ~/.gemrc
ln -s ~/dotfiles/.lein             ~/.lein
ln -s ~/dotfiles/emacs             ~/.emacs.d
```

tmux — the system file differs per platform, and `tmux.conf` sources both by
their `~` names:

```bash
ln -s ~/dotfiles/tmux.conf         ~/.tmux.conf
ln -s ~/dotfiles/basic.tmuxtheme   ~/.tmux.theme
ln -s ~/dotfiles/tmux-osx.conf     ~/.tmux-system.conf   # macOS
ln -s ~/dotfiles/tmux-linux.conf   ~/.tmux-system.conf   # Linux
```

Zed:

```bash
mkdir -p ~/.config/zed
ln -s ~/dotfiles/zed/settings.json ~/.config/zed/settings.json
ln -s ~/dotfiles/zed/keymap.json   ~/.config/zed/keymap.json
ln -s ~/dotfiles/zed/tasks.json    ~/.config/zed/tasks.json
```

Claude Code — **commands only**. See the warning in step 5 about
`settings.json`:

```bash
mkdir -p ~/.claude/commands
ln -s ~/dotfiles/claude/commands/start-ticket.md ~/.claude/commands/start-ticket.md
```

`.zshrc` needs nothing else installed: `antigen.zsh`, `z.sh`, and the
`worace` prompt theme are all vendored in this repo and sourced from
`~/dotfiles` directly.

## 3. `bin` on PATH

`~/dotfiles/bin` is never added to `PATH`. `.zshrc` adds `~/.local/bin`, and
the scripts are symlinked into it individually. Link all of them — several are
invoked by bare name from tmux bindings and Claude hooks, so a partial set
fails silently:

```bash
mkdir -p ~/.local/bin
for f in ~/dotfiles/bin/*; do
  [[ -f "$f" && -x "$f" ]] || continue          # skips lib/ and README.md
  t=~/.local/bin/"$(basename "$f")"
  [[ -e "$t" || -L "$t" ]] || ln -s "$f" "$t"
done
```

Safe to re-run: it creates only what's missing and never clobbers something
already there — including a link you pointed elsewhere on purpose.

| script | what it's for |
|---|---|
| `new-worktree`, `remove-worktree` | create/remove a worktree for any repo (see `bin/README.md`) |
| `oneshot` | provision a worktree and launch a Claude session that implements a ticket |
| `trwt` | rename the tmux window to the current worktree (`prefix r`) |
| `tmux-workspace`, `tmux-snapshot`, `tmux-restore` | save/rebuild the session layout |
| `claude-tmux-bell`, `tmux-bell-mark`, `tmux-bell-clear` | bell + `🔔` marker on windows where Claude wants input |
| `dev-here`, `push_and_pr`, `histo`, `screenshot`, `polypomo` | assorted one-offs |

`claude-tmux-bell` is referenced by bare name in `~/.claude/settings.json`'s
Notification hook, and `tmux.conf` calls `tmux-bell-clear` by absolute path
from a `pane-focus-in` hook.

## 4. Agent skills

`skills/` is the single source of truth. Claude Code doesn't read
`~/.agents/skills` and Codex doesn't read `~/.claude/skills`, so each skill
needs a link in both:

```bash
~/dotfiles/skills/install.sh
```

Idempotent — creates only missing links, leaves correct ones alone, and warns
rather than overwriting anything unexpected. Re-run it whenever a skill is
added or is missing from one agent.

To remove a skill, delete its directory here and clear the dangling links:

```bash
find ~/.claude/skills ~/.agents/skills -type l ! -exec test -e {} \; -delete
```

## 5. Machine-local files (not in this repo)

This repo is public. These live outside it on purpose — recreate them per
machine.

### `~/.gitconfig.local` — required

`.gitconfig` includes it last, so it can override anything above it. Without
it git has no email and will refuse to commit.

```bash
cat > ~/.gitconfig.local <<'EOF'
[user]
	email = you@company.com
EOF
```

### `~/.config/worktrees/config` — required for `tmux-workspace`

The primary repo for the worktree scripts. `new-worktree`, `remove-worktree`,
and `oneshot` fall back to the repo containing the cwd, so this file is only
strictly needed by `tmux-workspace`, which runs at login where the cwd means
nothing. Full format in `bin/README.md`.

```bash
mkdir -p ~/.config/worktrees
cat > ~/.config/worktrees/config <<'EOF'
WORKTREE_MAIN_REPO=~/code/<repo>
WORKTREE_ROOT=~/worktrees
EOF
```

### `~/.secrets.sh` — optional

API keys and tokens, sourced by `.zshrc` behind an existence check, so a
machine without it starts shells fine.

### `~/.claude/settings.json` — do not link

**Leave this a real file.** The live one holds org-internal detail: GitHub
org, Teleport proxy, container registry, internal domains, k8s namespaces,
and paths to sensitive files. `dotfiles/claude/settings.json` is an old
stripped copy that is intentionally linked to nothing.

Never symlink `dotfiles/claude/settings.json` over it and never copy the live
one into this repo — either move publishes all of that.

### Per-repo worktree bootstrap — lives in the repo

Not a machine-local file any more. `new-worktree` looks for
`<repo>/.worktree-setup` before `~/.config/worktrees/hooks/<repo-name>`, so a
project's bootstrap is committed to that project and arrives with the clone.
Use the config-directory hook only for a repo you can't add a file to. Contract
is in `bin/README.md`.

## 6. Verify

```bash
exec zsh                                        # theme loads, no source errors
git config --show-origin --get user.email       # resolves via ~/.gitconfig.local
command -v new-worktree claude-tmux-bell        # bin is linked
ls ~/.claude/skills ~/.agents/skills            # same list in both

# tmux: theme applies and `prefix r` renames the window to the worktree.
# Use a throwaway session — don't kill-server on a machine with live work.
tmux new -s probe -c ~/dotfiles                 # then: prefix r, prefix d
tmux kill-session -t probe

cd ~/code/<repo>
oneshot ENG-1 --slug smoke --dry-run            # names the right repo and worktree path
python3 ~/.claude/skills/prune-worktrees/scripts/analyze_worktrees.py | head -3
```

## Appendix: prompt for an agent

To have an agent do step 5 (the part that needs your input):

```
Set up the machine-local config my dotfiles deliberately don't commit.

Read ~/dotfiles/NEW-MACHINE.md section 5 and ~/dotfiles/bin/README.md first.
Follow them rather than guessing.

1. ~/.gitconfig.local with a [user] section holding my work email. Ask me for
   it — don't infer it from commits in any repo on this machine.

2. ~/.config/worktrees/config — set WORKTREE_MAIN_REPO to my primary work
   repo's main checkout and WORKTREE_ROOT to ~/worktrees. If there isn't
   exactly one plausible candidate under ~/code, ask me which.

3. A bootstrap hook, only if needed. Check the work repo's root for a
   committed .worktree-setup first — if it's there you're done, it takes
   precedence. Otherwise ask me what a fresh worktree needs (private env files
   to symlink from the main checkout, monorepo library syncing, a typecheck
   smoke test) and write ~/.config/worktrees/hooks/<repo-name>, executable,
   `set -uo pipefail`. It runs with the new worktree as cwd and WORKTREE_PATH /
   WORKTREE_BRANCH / WORKTREE_MAIN_REPO in its environment; a nonzero exit
   fails the whole provision, so only hard-fail on genuinely fatal problems.

Then run ~/dotfiles/skills/install.sh.

Rules:
- Never overwrite any of these. If one exists, show me it and ask.
- Write nothing under ~/dotfiles. Keeping project names out of that public
  repo is the entire point of these files.
- Do not copy the live ~/.claude/settings.json into the dotfiles repo, and do
  not symlink dotfiles/claude/settings.json over it. It holds org-internal
  detail and must stay machine-local.

Finish with the checks in NEW-MACHINE.md section 6, then tell me what you
created and anything you had to guess.
```

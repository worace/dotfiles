# skills

Agent skills shared across Claude Code, Codex CLI, and any other agent that
reads a `skills/` directory. This directory is the single source of truth;
the agent-specific directories only hold symlinks into it.

## Layout

```
~/dotfiles/skills/<name>/SKILL.md     # the skill (plus any scripts/ or references/)
~/.claude/skills/<name>  -> ../../dotfiles/skills/<name>   # Claude Code
~/.agents/skills/<name>  -> ../../dotfiles/skills/<name>   # Codex and the ~/.agents convention
```

Claude Code does not read `~/.agents/skills`, and Codex does not read
`~/.claude/skills`, so each skill needs a link in both places. Links are
relative so the tree survives a home-directory move as long as `dotfiles`
stays at `~/dotfiles`.

## Adding a skill

1. Create `~/dotfiles/skills/<name>/SKILL.md` (a directory without a
   `SKILL.md` is ignored).
2. Run `~/dotfiles/skills/install.sh`.

The script is idempotent: it creates only missing links, never touches a
correct one, and warns instead of overwriting anything unexpected. Re-run it
on a new machine or whenever a skill is missing from one agent.

## Removing a skill

Delete the directory here, then remove the dangling links:

```
find ~/.claude/skills ~/.agents/skills -type l ! -exec test -e {} \; -delete
```

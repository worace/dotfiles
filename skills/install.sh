#!/usr/bin/env bash
# Link every skill in ~/dotfiles/skills into each agent's skills directory.
# Idempotent: creates only the links that are missing, leaves correct links
# alone, and warns about anything in the way. Safe to re-run any time.
set -euo pipefail

SKILLS_SRC="$HOME/dotfiles/skills"
TARGETS=(
  "$HOME/.claude/skills"   # Claude Code
  "$HOME/.agents/skills"   # Codex CLI and other agents using the ~/.agents convention
)

linked=0
for target in "${TARGETS[@]}"; do
  mkdir -p "$target"
  # Relative link: <target>/<name> -> ../../dotfiles/skills/<name>
  rel="../../dotfiles/skills"
  for skill_dir in "$SKILLS_SRC"/*/; do
    name="$(basename "$skill_dir")"
    [[ -f "$skill_dir/SKILL.md" ]] || continue
    link="$target/$name"
    want="$rel/$name"
    if [[ -L "$link" ]]; then
      have="$(readlink "$link")"
      if [[ "$have" != "$want" ]]; then
        echo "warn: $link -> $have (expected $want); leaving as-is" >&2
      fi
    elif [[ -e "$link" ]]; then
      echo "warn: $link exists and is not a symlink; leaving as-is" >&2
    else
      ln -s "$want" "$link"
      echo "linked $link -> $want"
      linked=$((linked + 1))
    fi
  done
done

echo "done: $linked new link(s)"

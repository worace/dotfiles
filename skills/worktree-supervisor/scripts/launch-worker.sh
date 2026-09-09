#!/usr/bin/env bash
# Launch one interactive worker in its already-provisioned worktree.
set -euo pipefail

harness=""
worktree=""
model=""
prompt=""
claude_permission="auto"
codex_sandbox="workspace-write"
codex_approval="on-request"

usage() {
  cat <<'EOF'
Usage: launch-worker.sh --harness claude|codex --worktree PATH --model MODEL --prompt FILE [options]

Options:
  --claude-permission POLICY    Claude permission mode (default: auto)
  --codex-sandbox POLICY        Codex sandbox (default: workspace-write)
  --codex-approval POLICY       Codex approval policy (default: on-request)
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --harness) harness="$2"; shift 2 ;;
    --worktree) worktree="$2"; shift 2 ;;
    --model) model="$2"; shift 2 ;;
    --prompt) prompt="$2"; shift 2 ;;
    --claude-permission) claude_permission="$2"; shift 2 ;;
    --codex-sandbox) codex_sandbox="$2"; shift 2 ;;
    --codex-approval) codex_approval="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
done

[[ "$harness" == "claude" || "$harness" == "codex" ]] || { echo "--harness must be claude or codex" >&2; exit 2; }
[[ -d "$worktree" ]] || { echo "Worktree does not exist: $worktree" >&2; exit 2; }
[[ -n "$model" ]] || { echo "--model is required" >&2; exit 2; }
[[ -f "$prompt" ]] || { echo "Prompt file does not exist: $prompt" >&2; exit 2; }

cd "$worktree"
case "$harness" in
  claude)
    command -v claude >/dev/null || { echo "claude is not installed" >&2; exit 127; }
    exec claude --model "$model" --permission-mode "$claude_permission" "$(<"$prompt")"
    ;;
  codex)
    command -v codex >/dev/null || { echo "codex is not installed" >&2; exit 127; }
    exec codex --cd "$worktree" --model "$model" --sandbox "$codex_sandbox" \
      --ask-for-approval "$codex_approval" --no-alt-screen "$(<"$prompt")"
    ;;
esac

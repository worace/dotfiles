# Worker harness adapters

Choose and record one adapter per ticket. The supervisor's own model/harness is independent of this choice.

| Adapter | Launch profile | tmux interaction policy |
|---|---|---|
| `claude` | `--model <model> --permission-mode <policy>` | Use the proven monitor and safe-send procedure in `tmux-driving.md`. |
| `codex` | `--cd <worktree> --model <model> --sandbox <policy> --ask-for-approval <policy> --no-alt-screen` | Launches the local interactive TUI. `--no-alt-screen` keeps scrollback useful when the user zooms in. Capture panes freely; inject only after a manual visual check that it is idle and has no draft. No generic Codex busy/draft regex is assumed. |

## Codex policy mapping

`auto` is an intent, not a Codex CLI value. Use a user-approved combination such as:

```bash
--sandbox workspace-write --ask-for-approval on-request
```

Use `--ask-for-approval never` only when the user has explicitly authorized that level of autonomy. Never substitute `--yolo` / `--dangerously-bypass-approvals-and-sandbox` for normal worker execution.

## Direct zoom-in contract

Every worker must be a local interactive process in its named tmux window. To inspect or steer it directly:

```bash
tmux select-window -t "dev:<ticket-window>"
tmux attach -t dev
```

The supervisor must not type while a user may be interacting. Before sending a follow-up, check copy mode, capture scrollback, and verify no composer draft. For Codex, the supervisor should prefer asking the user to zoom in whenever the TUI state is ambiguous.

## Adding an adapter

Do not copy Claude's escape keys, prompt glyph, or busy regex into another TUI. First document a version-tested launch command, idle/busy/draft signals, safe interrupt behavior, and a manual fallback. Until then the adapter is launchable but observe-only for automated intervention.

---
name: worktree-supervisor
description: Supervise a fleet of interactive Claude Code or Codex CLI sessions implementing an epic's tickets in parallel — one git worktree + tmux window per ticket. Use when the user asks to "supervise" an epic, "fan out" tickets to sub-agents, orchestrate parallel ticket implementation, or run the worktree/tmux supervisor pattern. Covers wave planning, harness-aware launch, tmux monitoring, intervention, and cross-PR consolidation.
---

# Worktree Supervisor

Run N tickets in parallel: each gets a worktree (`new-worktree`), a tmux window in the `dev` session, and an interactive worker harness. The supervisor itself may be Claude or Codex; that choice does not change this protocol. Each worker explicitly chooses `claude` or `codex` — never silently assume one from the supervisor.

The user can zoom into any worker window and talk to that worker directly. This only applies to terminal-launched interactive workers: API-managed subagents are useful but are not a substitute when the user wants tmux access.

Reference files (read on demand, not upfront):
- [reference/harness-adapters.md](reference/harness-adapters.md) — supported launch profiles and the safety limits of each TUI. **Read this before choosing a worker harness.**
- [reference/tmux-driving.md](reference/tmux-driving.md) — the keystroke-injection protocol, every known trap, and the monitor script. **Read this before your first `send-keys`.**
- [reference/templates.md](reference/templates.md) — kickoff-prompt skeleton and follow-up dispatch templates (adversarial acceptance assessment, prompt-review).
- [reference/recovery.md](reference/recovery.md) — reconnect after the supervisor session, cwd, or coordinating worktree disappears while implementer worktrees/tmux sessions may still exist. **Read this before provisioning replacements after any interruption.**

## Phase 1 — Plan waves

1. Pull every sub-ticket's full description (Linear MCP; list views truncate — `get_issue` each one).
2. Build a file-collision map: which tickets touch the same package/module. Collision ⇒ same lane (serialize or stack); no collision ⇒ same wave.
3. Order rules:
   - "Do X first" tickets whose output is *criteria/docs* (not code others import) do NOT block fan-out — start them in wave 1 alongside the rest and treat their output as a review-time gate.
   - Same-package pairs: pick the pattern-setter first; the second branches from the first's branch (stacked) or waits for its merge.
   - Save the largest/vaguest ticket for last so it can crib patterns from sibling PR diffs — tell it explicitly which PRs to read.
4. Identify epic-level cross-cutting decisions (budgets, cadences, shared caps). Do not let sub-agents decide these: every kickoff prompt says **estimate and flag, don't decide**, and you consolidate the numbers into one decision for the user at the end.
5. Confirm with the user before launching: wave composition, commit/PR policy (implement-only vs commit-and-stop vs full ship), and a worker profile per lane (harness, model, permission/sandbox policy). “Auto” is a policy intent, not a universal CLI flag: translate it through the selected adapter.

## Phase 2 — Provision and launch

```bash
new-worktree <TICKET-ID>-kebab-keywords [base-ref]   # creates worktree + dev-session tmux window
```
- Sequential creation avoids git/pnpm lock contention; with a warm pnpm cache each takes ~15s, so don't bother parallelizing.
- Requires the `dev` tmux session to exist (`tmux has-session -t dev`) — the window is created by the script.
- For a stacked lane, pass the sibling branch as base-ref.

Write each kickoff prompt to a scratchpad file (see templates) and record a launch spec beside it: ticket, branch, worktree, tmux target, base, harness, model, policy values, prompt path, and state-adapter. This is the durable recovery record.

Launch through the bundled wrapper; it avoids shell quoting bugs with large prompts and makes the selected harness explicit:
```bash
tmux send-keys -t "dev:<TICKET-ID>-..." \
  "/Users/horace/dotfiles/skills/worktree-supervisor/scripts/launch-worker.sh \\
    --harness codex --worktree <path> --model <model> \\
    --prompt $SCRATCH/kickoff-<TICKET-ID>.md --codex-sandbox workspace-write \\
    --codex-approval on-request" Enter
```
For Claude use `--harness claude --claude-permission auto`; see the adapter reference for complete profiles. Verify each session booted into its **selected** TUI before declaring the wave launched.

## Phase 3 — Monitor

Arm a persistent Monitor running the poll script in reference/tmux-driving.md. It emits one line per transition: `<window>: idle`, `<window>: needs-input`, `<window>: PR OPENED <url>`. The Claude adapter has a reliable busy marker; Codex is observation-first unless its installed version has been deliberately verified with an adapter marker. `unknown` means capture and inspect, never infer idle.

- Restart the monitor whenever the active-window set changes (it reads its window list at start), and re-arm it after any worker restart (session teardown kills it silently).
- `N shells still running` next to an idle prompt = benign: the session backgrounded a long command and will auto-resume on its completion. Do nothing.
- Keep monitor state and reusable kickoff prompts outside a disposable supervisor worktree (for example under `/private/tmp/<epic>-supervisor/`). The coordinating worktree may be pruned while implementer worktrees and tmux panes remain healthy.

## Phase 4 — Intervene on events

On every idle/needs-input event, capture the pane **with scrollback** (`capture-pane -p -S -60`) and classify before acting:

| Pane shows | Action |
|---|---|
| Final summary + PR link | Verify PR scope vs ticket (diff stat + body); mark done; update tracking |
| A question / decision fork | Answer it yourself if it's within already-made decisions; escalate to the user only for genuine product forks |
| Blocked on user-only auth (browser SSO etc.) | Verify the claim yourself first (e.g. `tsh status` → read the `Valid until` line, not the header), then ask the user; push-notify if they may have walked away |
| A stuck draft the user typed | See tmux-driving.md — only use the adapter's verified clear/retype procedure, or execute the instruction directly yourself if you can do it faster (e.g. Linear updates) |
| Working on something now moot (e.g. PR already merged) | Interrupt only with the selected adapter's verified control, state why, ask for a wrap-up handoff summary |
| Permission prompt (rare in auto mode) | Approve routine ones via send-keys; escalate destructive/surprising ones |

Never inject into a window where the composer has a draft or the user is plausibly mid-conversation — flag it to the user instead. For an observe-only adapter, the supervisor may capture panes but must have the user zoom in or manually verify a safe idle state before typing.

## Phase 5 — Ship-time coordination

- Stacked PRs: instruct the agent to check the base PR's merge state at ship time — merged ⇒ rebase onto main, PR base main; not merged ⇒ PR base = sibling branch, marked "stacked on #N, retarget after merge".
- If two open PRs need the same new helper: both reuse machinery already on main; forbid new shared helpers that would conflict across branches.
- Watch for two PRs adding alembic migrations (sibling-heads); verify with `gh pr diff N --name-only | grep alembic` before assuming.
- Follow-up passes ride the same sessions (context is warm): send the harness-appropriate review instruction as a literal message. Apply resulting fixes through the session, not by hand — it re-greens and pushes itself.
- After any push that moves a reviewed head, have the session re-stamp its review status so the user's queue shows reviewed-at-head.

## Phase 6 — Consolidate for the user

You own the cross-PR picture the individual agents can't see: recommend a review order (smallest-risk/criteria-setting first, stack bases early, judgment-heavy last), name the cross-PR inconsistencies (e.g. two PRs making the same posture call with different conservatism), and assemble flagged epic-level numbers into one decision. Relay agent handoff notes into the tickets (Linear comment on Done) so nothing lives only in a tmux scrollback.

## Verify

- `tmux list-windows -t dev` shows one window per active ticket; each pane shows its recorded Claude or Codex TUI.
- The Monitor task is running and has emitted at least one event per session lifecycle.
- Every finished ticket: PR exists, the repo's green command claimed in-session, ticket updated, task list updated.
- No window left with an unsubmitted draft or an unanswered question.

# Kickoff and dispatch templates

## Kickoff prompt skeleton

One scratchpad file per ticket (`kickoff-<TICKET-ID>.md`), passed to the selected interactive harness by `scripts/launch-worker.sh`. Record its matching launch spec (harness, model, policy, worktree, tmux target, prompt path) in the same durable scratchpad. Sections, in order:

1. **Framing.** "You are implementing <ticket> in this dedicated worktree (branch <name>). Part of epic <epic>." If the ticket is late in the epic: name the sibling PRs whose patterns it should reuse and tell it to read their diffs.
2. **Ticket text, verbatim.** Embed the full description — don't assume the session can reach the tracker.
3. **Coordination constraints** (only when real): "keep your diff scoped to X; ticket Y stacks on you"; the stacked-PR ship rule (check base PR merge state at ship time: merged ⇒ rebase onto main + PR base main; else PR base = sibling branch, marked stacked, retarget after merge); "reuse machinery from main only, no new shared helpers".
4. **Epic-level guardrails.** For every cross-cutting knob (budget, cadence, cap): estimate the delta, report it in the PR description, implement as a named constant/config flag, and **flag recommendations rather than silently applying them**.
5. **Acceptance criteria.** Name the specific cases and where their definitions live. If the source artifact isn't on main, give the fallback ("validate against the case description below" / "read the file from origin/<branch>").
6. **Process.** Repo conventions load automatically; run the repo's green command; **explicit authorization to commit/push/PR** (repo rules require an explicit request — the kickoff is it) with the agreed ship verb (e.g. /send-it); escape hatch: "if you hit a genuinely ambiguous product decision, state the fork clearly and pause — the supervisor or the user will respond in this session."

## Adversarial acceptance assessment (post-implementation dispatch)

Sent to each session after its PR is up; produces a PR comment that self-grades honestly. Fill `<PR>`, `<cases>`, `<where definitions live>`:

> Your new task: post an adversarial acceptance assessment as a single comment on <PR>. Your assigned case(s): <cases>, defined in <where definitions live>. For each assigned case, argue AGAINST your own implementation: (1) restate the expected observable; (2) cite the concrete evidence your change produces it (test names, rendered output, measurements, file:line); (3) actively hunt for ways the original miss could still occur — inputs, caps, code paths, config/flag states, tenant conditions; (4) give an honest per-case verdict: closed / partially closed / unverified, and state what only a live run could prove. Be genuinely adversarial — a verdict of partially closed with sharp caveats is more valuable than a soft closed. Title the comment: ## Adversarial acceptance assessment - <cases>. Then stop.

Observed value: uniformly honest "partially closed" verdicts; surfaced unowned gaps (an upstream gate no ticket touched, dead code a design silently depended on, cross-ticket couplings the acceptance table under-specified). Consolidate the new gaps for the user — they're the real output.

## Follow-up quality passes

- Send the implementing session an explicit review request (for example, invoke its installed `send-it`/review workflow; do not assume a Claude-only slash command). Context is warm; scope the request to its PR. On findings: apply concerns through the session (it re-greens and pushes), skip nits unless the user's prompt philosophy says otherwise, and post-merge findings become a small follow-up PR from the same session.
- After any push that moves a reviewed head: have the session re-run/re-stamp its review so the PR shows reviewed-at-head.
- When one session's investigation invalidates another PR's claim, dispatch the same correction to both sessions with the shared-machinery rule (reuse from main only) so their fixes can't conflict.

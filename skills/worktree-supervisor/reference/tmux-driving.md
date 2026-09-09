# Driving interactive worker sessions via tmux

Every trap below was hit in real supervision runs. Read [harness-adapters.md](harness-adapters.md) first: the detailed Claude controls are not automatically safe for Codex or a future harness.

## Reading state

- **Claude adapter only — busy:** `esc to interrupt` appears in the visible pane. Absent ⇒ idle (turn ended).
- **Other adapters:** treat a missing busy marker as `unknown`, not idle. Capture scrollback and do not automate keystrokes until the adapter's documented/manual idle check succeeds.
- Always capture with scrollback when classifying an idle event: `tmux capture-pane -p -S -60 -t "dev:<win>"`. The visible screen alone misses the summary above the fold.
- `N shells still running` on an idle pane = the session backgrounded a command and will auto-resume when it exits. Not stuck; don't touch.
- A `1: Bad 2: Fine 3: Good 0: Dismiss` overlay is a passive feedback widget, not a blocking prompt.

## Traps

1. **Copy-mode eats everything.** If the user scrolled a window, the pane is in copy-mode: `send-keys -l` floods `not in a mode` errors (a long message can hang your script for minutes) and nothing reaches the composer. Check `tmux display-message -t <pane> -p '#{pane_in_mode}'` first; if `1`, exit safely with `tmux send-keys -t <pane> -X cancel` (touches nothing else).

2. **Stuck drafts (Claude adapter only).** Text the *user* typed while the session was busy or a question dialog was up renders at `❯` but will NOT submit via injected Enter — plain `Enter`, `C-m`, and kitty CSI-u all bounce. Freshly injected text submits fine. Fix: `send-keys C-u`, retype with `send-keys -l '<text>'`, brief pause, `Enter`. For another harness, do not apply this recipe without an adapter-specific verification.

3. **Never send Escape speculatively.** Its meaning is harness-dependent and can delete a draft, decline a dialog, or interrupt a turn. Use it only when the selected adapter documents it and you deliberately intend that outcome.

4. **Verification races.** capture-pane ~2s after Enter can catch a mid-redraw frame and read as idle/unsubmitted when the message actually went through. Never branch on one immediate read. Verify by (a) waiting 5s+ and re-capturing, and (b) grepping the transcript for your message text. A retry-Enter is harmless only when the composer is empty — don't loop it.

5. **Don't clobber the user.** Before any send: skip the window if the composer already has a draft (unless the draft is the stuck-draft case above and the session is idle) or if the session is busy. The user may be typing in that window right now.

6. **Quoting long messages.** One `send-keys -l "$MSG"` handles multi-KB messages fine. Write messages without apostrophes so shell quoting stays trivial. Slash commands (`/prompt-review`) are sent as literal text + Enter and trigger the skill normally.

7. **bash 3.2.** macOS stock bash has no `declare -A`. Poll scripts use state files, not associative arrays.

8. **Monitors die silently.** A worker restart or Monitor timeout leaves no reliable marker. After any restart, assume the monitor is gone and re-arm. The monitor also reads its window list once at start — stop/edit/restart it when the active set changes.

9. **Verify agent claims cheaply when stakes exist.** Example: `tsh status | head` shows a logged-in-looking header even when expired — the truth is the `Valid until: ... [EXPIRED]` line. When a session (or your own memory) asserts an auth state, check the discriminating line, not the vibe.

## Safe send procedure (Claude adapter)

```bash
send() {  # send <window> <message>  — bash-3.2 safe
  local t="dev:$1.0" msg="$2" pane
  [[ "$(tmux display-message -t "$t" -p '#{pane_in_mode}')" == "1" ]] && { tmux send-keys -t "$t" -X cancel; sleep 1; }
  pane=$(tmux capture-pane -p -t "$t")
  echo "$pane" | grep -q 'esc to interrupt' && { echo "SKIP $1: busy"; return; }
  echo "$pane" | sed -n 's/^❯ \(..*\)$/\1/p' | grep -q . && { echo "SKIP $1: composer draft"; return; }
  tmux send-keys -t "$t" -l "$msg"; sleep 1
  tmux send-keys -t "$t" Enter
  # verify later (5s+), via transcript grep — not an immediate busy-check
}
```

## Monitor script (Claude windows; other adapters are observe-only unless verified)

```bash
#!/bin/bash
WINDOWS="<space-separated window names>"   # edit + restart monitor when this changes
STATE_DIR="<scratchpad>/watch-state"; mkdir -p "$STATE_DIR"
while true; do
  for name in $WINDOWS; do
    sf="$STATE_DIR/$name.state"; pf="$STATE_DIR/$name.pr"
    pane=$(tmux capture-pane -p -t "dev:$name" 2>/dev/null)
    if [[ -z "$pane" ]]; then
      [[ "$(cat "$sf" 2>/dev/null)" != "gone" ]] && echo "$name: WINDOW GONE"; echo gone > "$sf"; continue
    fi
    if echo "$pane" | grep -q 'esc to interrupt'; then state=busy
    elif echo "$pane" | grep -qE 'Do you want|Allow this|1\. Yes'; then state=needs-input
    else state=idle; fi
    prev=$(cat "$sf" 2>/dev/null || echo busy)
    [[ "$prev" != "$state" && "$state" != busy ]] && echo "$name: $state"
    echo "$state" > "$sf"
    if [[ ! -f "$pf" ]]; then
      pr=$(tmux capture-pane -p -S -400 -t "dev:$name" 2>/dev/null | grep -oE 'https://github.com/[^ ]+/pull/[0-9]+' | tail -1)
      [[ -n "$pr" ]] && { echo "$name: PR OPENED $pr"; echo "$pr" > "$pf"; }
    fi
  done
  sleep 45
done
```

Run it with the Monitor tool (`persistent: true`). Stale `.pr` state files suppress re-detection of already-reported PRs across restarts — usually what you want; delete a window's `.pr` file if it will open a second PR.

# Start Linear Ticket

Takes a Linear ticket (ID or URL) and sets up a fresh worktree with a descriptive branch.

## Input

$ARGUMENTS - A Linear ticket ID (e.g. `ENG-123`) or full URL (e.g. `https://linear.app/team/issue/ENG-123/...`)

## Steps

1. **Extract the ticket ID** from `$ARGUMENTS`. If it's a URL, parse out the issue identifier (e.g. `ENG-123`).

2. **Fetch the ticket details** from Linear using the Linear MCP server or by running:
   ```
   curl -s -X POST https://api.linear.app/graphql \
     -H "Content-Type: application/json" \
     -H "Authorization: $LINEAR_API_KEY" \
     -d '{"query": "{ issue(id: \"$TICKET_ID\") { title description state { name } priority labels { nodes { name } } assignee { name } team { key } } }"}'
   ```
   Extract the title, description, labels, priority, and current state.

3. **Generate a branch name** from the ticket. Use this format:
   - `<ticket-id>-<short-slug>` (hyphen-separated, no nested directories)
   - Example: `eng-123-add-user-avatar-upload`
   - Lowercase the ticket ID, take the title, lowercase it, replace spaces/special chars with hyphens, and truncate to ~50 chars.
   - The branch name must be valid for git (no spaces, no special chars besides `-`).

4. **Set up the worktree.** The shared worktree directory is `~/worktrees/<repo-name>/`.

   **Important:** Run each command as a separate bash call (not chained with `&&` or `$()` substitution) so that pre-approved permission rules match cleanly.

   ```bash
   # Step 4a: Get the remote URL
   git config --get remote.origin.url
   ```
   ```bash
   # Step 4b: Extract repo name from the URL
   basename -s .git "<REMOTE_URL>"
   ```
   ```bash
   # Step 4c: Check if worktree already exists
   ls -d "$HOME/worktrees/<REPO_NAME>/<BRANCH_NAME>" 2>/dev/null
   ```
   ```bash
   # Step 4d: Fetch latest main
   git fetch origin main
   ```
   ```bash
   # Step 4e: Create worktree with a new branch based on origin/main
   git worktree add -b "<BRANCH_NAME>" "$HOME/worktrees/<REPO_NAME>/<BRANCH_NAME>" origin/main
   ```

5. **Open the new worktree** by changing the working directory:
   ```bash
   cd "$WORKTREE_DIR"
   ```
   Let the user know the path so they can also open it in their editor.

6. **Install dependencies** in the new worktree:
   ```bash
   pnpm install
   ```
   ```bash
   pnpm sync:libs
   ```

7. **Set the tmux window name** so it's identifiable in `prefix + w`. If running inside tmux (`$TMUX` is set), rename the current window:
   ```bash
   tmux rename-window "$TICKET_ID - $SHORT_TITLE"
   ```
   Where `$SHORT_TITLE` is the ticket title truncated to keep the total name reasonable (~60 chars). Replace `.` and `:` with `-` in the name.

8. **Propose a plan.** Based on the ticket title, description, and labels:
   - Summarize what the ticket is asking for.
   - Identify the likely files/areas of the codebase involved (look at the repo structure).
   - Propose a step-by-step implementation plan.
   - Call out any open questions or ambiguities in the ticket.
   - Suggest a rough order of operations.

## Notes

- If the worktree directory already exists, ask the user if they want to reuse it or remove and recreate it.
- If `LINEAR_API_KEY` is not set, ask the user to provide it or set it in their environment.
- After setup, remind the user they can clean up old worktrees with:
  ```
  git worktree list
  git worktree remove ~/worktrees/<repo>/<branch>
  ```
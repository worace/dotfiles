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
   - `<ticket-id>/<short-slug>`
   - Example: `eng-123/add-user-avatar-upload`
   - Lowercase the ticket ID, take the title, lowercase it, replace spaces/special chars with hyphens, and truncate to ~50 chars.
   - The branch name must be valid for git (no spaces, no special chars besides `-` and `/`).

4. **Set up the worktree.** The shared worktree directory is `~/worktrees/<repo-name>/`. Run:
   ```bash
   # Get the repo name from the current git remote
   REPO_NAME=$(basename -s .git $(git config --get remote.origin.url))
   WORKTREE_DIR="$HOME/worktrees/$REPO_NAME/$BRANCH_NAME"

   # Make sure we have latest main
   git fetch origin main

   # Create worktree with a new branch based on origin/main
   git worktree add -b "$BRANCH_NAME" "$WORKTREE_DIR" origin/main
   ```

5. **Open the new worktree** by changing the working directory:
   ```bash
   cd "$WORKTREE_DIR"
   ```
   Let the user know the path so they can also open it in their editor.

6. **Propose a plan.** Based on the ticket title, description, and labels:
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
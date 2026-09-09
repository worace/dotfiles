## Claude Preferences

### Notes for Working on Code

* Use ADTs as much as possible
* Always annotate types
* Use explicit schema
* Prefer total functions
* Prefer pure functions
* Parse don't validate
* `get_blah()` is for I/O. Name functions for the data they return.
* Isolate logic from I/O.
* Immutability as much as possible
* Write tests without hitting the database as much as possible.
* Push functionality into functions based on domain data models
* Write small functions
* Parameterize tests to cover the input space
* Always confirm before committing

## Notes for Working on Agent Prompts

When writing or editing agent prompts, use the `agent-prompt-authoring` skill (~/.claude/skills/agent-prompt-authoring/SKILL.md).

## Bash Commands and `Read` Deny Rules

Never chain `cd` before a command that reads files. In any repo whose
`.claude/settings.json` carries a `Read(...)` deny rule, a `cd` in a compound
command followed by a *relative* path to `grep`, `rg`, `diff`, `git`, `cp`, or `mv`
cannot be resolved statically, so it always asks for approval and auto mode
cannot clear it. Which path the deny rule names is never checked — any one of
them arms this.

* Write `grep -rn foo /abs/path/app`, not `cd /abs/path && grep -rn foo app`.
* A relative path with no `cd` is fine. So is a `cd` in front of an absolute path.
* `cat`/`head`/`sed` on a relative path after a `cd` trips a softer version of the
  same check, which the classifier can sometimes clear. Don't rely on it.

Written repo-agnostically on purpose, since it's the deny rule that arms it, not this
codebase. If you want a lighter touch for a first pass, the load-bearing sentence alone works
as a single bullet under your existing "Notes for Working on Code":

* Never chain `cd` before a file-reading command; pass absolute paths. A
  `cd X && grep rel/path` compound can't be auto-approved wherever a `Read()`
  deny rule exists.

## Linear Ticket and Github PR Conventions

ALWAYS Include a Link when referencing Linear Tickets or Github PRs

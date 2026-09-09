---
name: agent-prompt-authoring
description: Horace's rules for writing or editing agent prompts / LLM system prompts. Load whenever creating, reviewing, or modifying an agent prompt, prompt template, or prompt partial.
---

# Notes for Working on Agent Prompts

* Write for failures you've actually seen; run with no instruction first, add guidance only for recurring ones
* Don't instruct what the model already does well
* Shorter and focused beats long and comprehensive
* Prefer deleting weak instructions to adding new ones
* Procedural, not declarative — tell it how, not what something is; cut background
* Measure each addition against the no-prompt baseline (quality + cost)
* Review anything model-written; models draft prompts worse than they consume them
* Triggering language ("use this when…") goes in the description/opening, not the body
* One section = one repeatable task class; no broad "best practices" grabbags
* Teach decisions, not examples: "if X, inspect Y, choose A/B on signal Z"
* Name the observable signal at every branch
* Abstract guidance over filled-in templates; no hardcoded type/field/file/port names
* Name negative cases and their condition, not just the positive
* End with concrete verification: run this, confirm this file, validate this schema
* Main file is a control plane; push detail to separate files loaded on demand
* When editing: re-pull main, read the whole file, dedupe before adding
* Generalize to one rule; use enumerations only as non-exhaustive examples
* Fix the mechanism, not the symptom — anchor on how it went wrong, not one phrase/pattern
* On "minimal/prompt-only" scope: drop out-of-scope layers, don't defer them
* Checklists only when failures are omissions; ~5 verifiable items the model would skip
* Eval gotcha: worked examples anchor on their literal names — run a different-names control

# Pattern Catalog Pointer

Applies to: `**/*`

A curated catalog of reusable interaction-design patterns lives at `~/code/rulesets/patterns/`. Each pattern is one file capturing a small principle with wide surface area — discovered in one project, written down so the next project doesn't re-derive it.

## When to consult it

Before designing any user-facing interaction choice, read the relevant pattern instead of reinventing the shape:

- A prompt or a chain of prompts (a completing-read, a wizard, a multi-step form)
- A picker over N candidates where the kind of candidate matters, not just the name
- A default for a yes/no or multiple-choice prompt
- A confirmation whose friction should match its consequence
- Any "should this be one prompt or several?" decision

These patterns came out of Emacs/Elisp prompt design, but the principles are interface-general — they apply to a CLI flow, a web form, or a TUI just as well.

## How to load it

Don't carry the whole catalog in context. Read `~/code/rulesets/patterns/README.org` for the index and the one-line principle of each pattern, then read the single pattern file you need (the patterns are `.org` files). Humans grep `patterns/` directly.

## The root principle

The patterns converge on one idea: the choices the user has should all be on screen, accurately labeled, ordered by what they'll most often want, with friction sized to the cost of being wrong. Each pattern file is one worked shape of that principle.

## Adding to it

A new pattern is captured as a raw note in a project's `docs/design/` when it lands, then promoted into `patterns/` in a batched review (see `patterns/README.md` for the cadence and the frontmatter contract). Don't formalize every rough note inline — capture on landing, promote on review.

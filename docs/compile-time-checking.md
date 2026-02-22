# Compile-Time Checking Limits and Strategy

This note captures what can be checked at compile time in this DSL direction, and where the limits are.

## Short Answer

Yes, strong compile-time validation is possible for HTML/CSS/SQL forms when inputs are static and schema/style metadata is available.

No, compile-time checking cannot fully guarantee correctness for dynamic/runtime-built expressions.

## What Is Feasible

- HTML DSL checks:
  - node/attribute shape validation
  - required/forbidden attribute rules (if you define them)
  - project-specific component contracts
- CSS/class checks:
  - known token/class validation against a registry/snapshot
  - typo detection for structured class forms
- SQL DSL checks:
  - table/column existence
  - type compatibility in predicates/projections
  - join key compatibility
  - query shape validation against known schema metadata

## Hard Limits

- Dynamic expressions reduce certainty:
  - runtime string concatenation
  - conditionally generated SQL/class tokens
  - values only known at execution time
- External-state drift:
  - database schema can change after compile
  - style config/tokens can change after compile
- Environment dependency:
  - checks that require a live DB or generated metadata may fail in offline/CI contexts
- Cost tradeoff:
  - deep checking can slow compile times if done unconditionally

## Practical Model

Use a hybrid model:

- `permissive mode`:
  - keeps current stringly escape hatches
  - best-effort checks only
- `strict mode`:
  - structured forms (for class/query DSL inputs)
  - compile-time errors/warnings for invalid static forms
  - explicit fallback for dynamic/uncheckable segments

This preserves flexibility while enabling real diagnostics where syntax is statically knowable.

## Editor Diagnostics (VS Code)

Compile-time checks can propagate to editor diagnostics when the Lisp toolchain/LSP surfaces compiler messages.

In practice:

- DSL must signal errors at macroexpansion/compile time for invalid structured forms.
- Editor integration must consume those compiler diagnostics and map them to file/line/column.

SBCL can report source positions for many compile/load-time errors, which is the basis for inline squigglies.

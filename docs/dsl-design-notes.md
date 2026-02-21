# HTML DSL Design Notes

This captures the design discussion behind `clisp-web-dsl`, separate from the strict behavior contract in `docs/html-dsl-scope.md`.

## Are We Rebuilding Hunchentoot?

No. Hunchentoot is an HTTP server/router layer.  
This project is an HTML generation DSL.

## Allowlist vs Permissive Tags

Two valid approaches:

- Allowlist:
    - Predictable subset
    - Clear supported contract
    - More maintenance (must add tags/attrs manually)
- Permissive tags:
    - Any `:tag` can render
    - Lower maintenance
    - Closer to generic emitters

Current implementation is allowlist-based for a minimal subset.

## What Is Actually Risky?

The main risk is usually not tag names themselves. The high-impact issues are:

- Unescaped content/attribute values (XSS and broken markup)
- Malformed attribute lists
- Invalid output around HTML edge-cases (notably void elements)

## Why Keep Void-Tag Knowledge?

Without void-tag handling, output tends to become non-standard:

- `<br></br>`
- `<img ...></img>`

Browsers may recover, but this is sloppy and can be surprising.

Practical rule:

- Keep a small void-tag set
- Allow attributes on void tags
- Ignore or reject content on void tags

## CL-WHO vs Spinneret Framing

These represent two ends of the design spectrum:

- CL-WHO-ish:
    - More permissive/configurable
    - Emphasis on ergonomic generation
- Spinneret-ish:
    - Stricter HTML5-aware validation
    - More opinionated correctness checks

This DSL can intentionally pick one mode, or add strict/permissive modes later.

## Modernization Path (If Expanded)

A realistic path is test-first, incremental implementation:

1. Define target mode(s): permissive, strict, or both.
2. Build a conformance-oriented test suite for escaping, attrs, void tags, and malformed input.
3. Implement in small slices behind tests.
4. Use AI as an accelerator, not as the sole source of truth.
5. Differential-test behavior against existing libraries/parsers where useful.

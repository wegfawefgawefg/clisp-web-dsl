# HTML DSL Scope

This document defines what the current HTML DSL in `src/dsl.lisp` does and does not cover.

## Goal

Provide a minimal, predictable subset of HTML generation for experimentation and learning.

## Current Supported API Shape

Nodes are expressed as:

```lisp
(:tag "content" '(:attr "value" :attr2 "value2"))
```

And rendered through:

```lisp
(html
    (:h1 "Hello" '(:class "title"))
    (:p "Body" '(:id "main")))
```

## Supported Tags

- Headings/text: `:h1`, `:h2`, `:h3`, `:p`, `:strong`, `:em`, `:code`
- Layout: `:div`, `:span`
- Links/lists: `:a`, `:ul`, `:ol`, `:li`
- Void elements: `:br`, `:hr`, `:img`

Any other tag is rejected (renders as empty output for that node).

## Supported Attributes

Global attributes (all supported tags):

- `:class`
- `:id`
- `:style`
- `:title`

Tag-specific attributes:

- `:a`: `:href`, `:target`, `:rel`
- `:img`: `:src`, `:alt`, `:width`, `:height`

Any other attribute is rejected (the node is dropped).

## Rendering Rules

- Tag and attribute names are rendered lowercase.
- Attributes are emitted in the order provided.
- Output uses single quotes around attribute values.
- Text and attribute values are HTML-escaped (`&`, `<`, `>`, `"`, `'`).
- Void elements render without closing tags.
- Unsupported tag/attribute combinations are skipped, not partially rendered.

## Non-Goals (Current)

- Full HTML5 coverage
- Nested child node trees beyond current simple form usage
- Browser or W3C-level compliance guarantees

## Source of Truth

- Implementation: `src/dsl.lisp`
- Behavioral tests: `tests/test.lisp`

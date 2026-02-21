# HTML DSL Scope

This document defines what the current HTML DSL in `src/dsl.lisp` does and does not cover.

Direction: permissive-by-default HTML with lightweight CSS helpers.

## Goal

Provide a minimal, flexible HTML/CSS generation layer for experimentation and learning.

## Current Supported API Shape

HTML nodes are expressed as:

```lisp
(:tag "content" '(:attr "value" :attr2 "value2"))
```

And rendered through:

```lisp
(html
    (:any-tag "Body" '(:any-attr "value")))
```

CSS rules are expressed as:

```lisp
(css
    (css-rule ".hero" '(:font-size "48px" :line-height "1.1"))
    (css-rule ".cta" '(:background "#111")))
```

## Supported Tags

Tag names are permissive:

- Any symbol/string tag name is rendered.
- A small void-tag set is recognized (`:br`, `:hr`, `:img`) to avoid invalid closing-tag output.

## Supported Attributes

Attribute names are permissive:

- Any symbol/string attribute name is rendered.
- Attribute plist must be even-length.

Special coercions:

- `:class` with list value joins by spaces.
- Boolean true (`t`) renders as presence attribute (`disabled`).
- `nil` attribute value is omitted.

## Rendering Rules

- Tag and attribute names are rendered lowercase.
- Attributes are emitted in the order provided.
- Output uses single quotes around attribute values.
- Text and attribute values are HTML-escaped (`&`, `<`, `>`, `"`, `'`).
- Trusted raw content can be injected via `(raw "...")` (escape bypass).
- Void elements render without closing tags.
- Nested content is supported.
- `pretty-html` provides a lightweight formatted view for inspection.

## CSS Rules

- `css-rule` renders `selector { declarations }`.
- `css` concatenates rules with newlines.
- CSS declaration plist must be even-length.
- `nil` declaration values are omitted.

## Non-Goals (Current)

- Full HTML5 coverage
- Browser or W3C-level compliance guarantees
- Full CSS parsing/validation/minification pipeline

## Source of Truth

- Implementation: `src/dsl.lisp`
- Behavioral tests: `tests/test.lisp`

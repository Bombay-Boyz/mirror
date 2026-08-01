# Mirror

A document-to-HTML converter for Markdown, DOCX, and JSON, built around
one canonical intermediate representation that every source format's
parser is validated against identically.

All three formats now cover the full intermediate representation:
headings, paragraphs, bold/italic, inline code, links, ordered and
unordered lists, block quotes, fenced code blocks, tables, images, and
horizontal rules. A few deliberate simplifications are called out
where they apply — notably that lists are single-level (not nested) in
both Markdown and DOCX, and that DOCX heading/quote detection relies on
Word's built-in style IDs.

The design rationale lives in the module Haddocks themselves — start
with `Mirror.Document` (the canonical IR and its smart constructors),
then `Mirror.Document.Raw` (the shared raw-to-validated staging layer),
then whichever parser or renderer module you're changing. This README
is just how to build and run it.

## Build

Requires GHC 9.4 and `cabal-install` 3.x or newer.

```
cabal build
```

The first build will need network access to resolve dependencies
(aeson, xml-conduit, zip-archive, modern-uri, optparse-applicative,
and a few others — the full list, with version bounds, is in
`mirror.cabal`).

## Run

```
cabal run mirror -- path/to/input.md
cabal run mirror -- path/to/input.docx
cabal run mirror -- path/to/input.json
```

Format is detected from the file extension (`.md`/`.markdown`,
`.docx`, `.json`). By default, output is written alongside the input
file with a `.html` extension, plus a sibling `.css` file (Mirror's
embedded default stylesheet, unless overridden).

Options:

```
cabal run mirror -- INPUT [--css FILE] [--output FILE | -o FILE]
```

- `--css FILE` — embed this stylesheet instead of Mirror's built-in default.
- `--output FILE` / `-o FILE` — write the HTML here instead of the default path.

## Test

```
cabal test
```

Runs the hspec/QuickCheck suite in `test/Spec.hs` — 35 examples,
including property tests for HTML escaping and table rectangularity,
and named regression tests for the specific historical defects §14 of
the spec document catalogues.

## Project layout

```
mirror.cabal
app/Main.hs                    -- CLI entry point
src/Mirror/
  Error.hs                     -- the closed MirrorError vocabulary
  Document.hs                  -- the canonical IR + smart constructors
  Html.hs                      -- phantom-typed escaping
  Css.hs, Css/Default.css      -- the embedded default stylesheet
  Parser/Markdown.hs
  Parser/Docx.hs
  Parser/Json.hs
  Renderer/Html.hs
  Pipeline.hs                   -- read -> parse -> render -> write
test/Spec.hs
```

## Extending Mirror

Adding a fourth source format is a checklist the compiler enforces —
worked through concretely, with a hypothetical reStructuredText
example, in §13 of the spec document.

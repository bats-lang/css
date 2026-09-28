# css

CSS value types and rendering for the [Bats](https://github.com/bats-lang) programming language.

## Features

- CSS units (`PX`, `EM`, `REM`, `PERCENT`, `VW`, `DEG`, `MS`, ...)
- Colors (`RGB`, `RGBA`, `Named`), values, declarations
- Selectors (`Class`, `Id`, `Tag`, `Pseudo`, `Child`, `Descendant`) and
  rules, size-indexed so emitting into a builder needs no runtime check
- `class_text`: generated class names (`caa` .. `czz`), allocation-free

## Usage

There is no GC: colors, values, selectors, declarations and rules are
linear. The emitters borrow them; free each with its `*_free`.

```bats
#use array as A
#use builder as B
#use css as C

val r = $C.Rule($C.Class($A.text_lit("card"), 4),
  $C.Decl($A.text_lit("color"), 5, $C.Color($C.RGB(255, 0, 0))))
val b = $B.create()
val () = $C.emit_rule(b, r)
val () = $C.css_rule_free(r)
```

## Proven colours

A theme's colours can be proven at compile time; a colour is a static
`int` 0xRRGGBB.

- `src/contrast.bats`: WCAG 2 relative luminance (`LUM`) and contrast
  (`CONTRAST(a, b, k)`: at least k/10:1), so text that is hard to read
  does not type-check.
- `src/harmony.bats`: rules for a harmonious theme, over the channels
  (`RGB`): hue arcs (`HUE`), chroma, HSV saturation (`CALM`,
  `SATNEAR`), the brightest channel (`PEAK`) and relative lightness
  (`LIGHTER`); hue families (`FAMILIES`, `IN3`: at most three arcs, each
  at most 30 degrees wide), neutrals (`NEUTRAL`), and text/ground pairs
  that do not vibrate (`NOVIB`). The module's header gives the source of
  each rule.

`tests/static` holds a theme that is accepted and one rejected fixture
per rule.

## API

See [docs/lib.md](docs/lib.md) for the full API reference.

## Safety

Safe library — `unsafe = false`.

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

## API

See [docs/lib.md](docs/lib.md) for the full API reference.

## Safety

Safe library — `unsafe = false`.

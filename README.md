# css

CSS value types and rendering for the [Bats](https://github.com/bats-lang) programming language.

## Features

- CSS units: `px`, `em`, `rem`, `percent`, `vh`, `vw`, `auto`
- CSS colors: `rgb`, `rgba`, `hex`, named colors
- CSS values: `font_size`, `margin`, `padding`, `border`, `display`, `position`, etc.
- CSS selectors: element, class, id, pseudo-class
- Renders CSS values to builders for DOM style application

## Usage

```bats
#use css as C

val red = $C.Rgb(255, 0, 0)
val margin = $C.Px(16)
```

## API

See [docs/lib.md](docs/lib.md) for the full API reference.

## Safety

Safe library — `unsafe = false`.

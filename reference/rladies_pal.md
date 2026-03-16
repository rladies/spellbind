# RLadies+ colour palettes

Returns a colour palette function or vector from the RLadies+ brand.

## Usage

``` r
rladies_pal(palette = "main", reverse = FALSE, ...)
```

## Arguments

- palette:

  Name of the palette. One of `"main"`, `"full"`, `"purple"`, `"blue"`,
  `"rose"`, `"neutral"`, `"diverging"`, or `"light"`.

- reverse:

  Logical. Reverse the palette order?

- ...:

  Additional arguments passed to
  [`grDevices::colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html).

## Value

A function that takes an integer `n` and returns `n` interpolated
colours.

## Examples

``` r
rladies_pal("main")(3)
#> [1] "#881EF9" "#146AF9" "#FF5B92"
rladies_pal("purple")(6)
#> [1] "#5009A8" "#6B0FD4" "#881EF9" "#A152F8" "#BB86F7" "#D4B9F5"
rladies_pal("diverging", reverse = TRUE)(7)
#> [1] "#FB80AB" "#FF5B92" "#F6A4C3" "#EDEDF4" "#BB86F7" "#881EF9" "#6B0FD4"
```

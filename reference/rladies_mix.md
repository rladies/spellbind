# Tint or shade an RLadies+ brand colour

Mix a brand colour with lavender (tint) or charcoal (shade), following
the RLadies+ brand guidelines. This is the same approach used in the
brand SCSS — all tints mix towards lavender white, all shades mix
towards Bastille Black.

## Usage

``` r
rladies_mix(colour, amount = 0.5, mode = c("tint", "shade"))
```

## Arguments

- colour:

  A colour name from the brand palette (e.g. `"purple"`, `"blue"`,
  `"rose"`) or any hex colour string.

- amount:

  Numeric between 0 and 1. The proportion of the original colour to
  keep. `amount = 0.75` gives a 75% tint (mostly colour, lightly mixed
  with lavender/charcoal).

- mode:

  Either `"tint"` (mix towards lavender) or `"shade"` (mix towards
  charcoal).

## Value

A hex colour string.

## Examples

``` r
rladies_mix("purple", 0.75)
#> [1] "#A152F8"
rladies_mix("purple", 0.50, mode = "shade")
#> [1] "#5C2694"

scales::show_col(vapply(
  c(0.25, 0.50, 0.75, 1),
  function(x) rladies_mix("blue", x),
  character(1)
))
```

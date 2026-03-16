# RLadies+ colour and fill scales for ggplot2

Discrete and continuous colour/fill scales using the RLadies+ brand
palettes.

## Usage

``` r
scale_colour_rladies(palette = "main", reverse = FALSE, ...)

scale_color_rladies(palette = "main", reverse = FALSE, ...)

scale_fill_rladies(palette = "main", reverse = FALSE, ...)

scale_colour_rladies_c(palette = "purple", reverse = FALSE, ...)

scale_color_rladies_c(palette = "purple", reverse = FALSE, ...)

scale_fill_rladies_c(palette = "purple", reverse = FALSE, ...)
```

## Arguments

- palette:

  Name of the palette (see
  [`rladies_pal()`](https://rladies.org/spellbind/reference/rladies_pal.md)
  for options).

- reverse:

  Logical. Reverse the palette order?

- ...:

  Additional arguments passed to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)
  or
  [`ggplot2::scale_colour_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

## Examples

``` r
library(ggplot2)

ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
  geom_point(size = 3) +
  scale_colour_rladies() +
  theme_rladies()


ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_tile() +
  scale_fill_rladies_c("purple") +
  theme_rladies()
```

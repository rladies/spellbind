# RLadies+ brand colours

Access individual colours or the full palette from the RLadies+ brand.

## Usage

``` r
rladies_cols(...)
```

## Arguments

- ...:

  Character names of colours to retrieve. If empty, returns all colours
  as a named character vector.

## Value

A named character vector of hex colour values.

## Examples

``` r
rladies_cols()
#>         purple   purple_light      purple_50 purple_lighter    purple_dark 
#>      "#881ef9"      "#a152f8"      "#bb86f7"      "#d4b9f5"      "#6b0fd4" 
#>  purple_darker           blue        blue_75        blue_50        blue_25 
#>      "#5009a8"      "#146af9"      "#4a8bf8"      "#80acf6"      "#b7ccf5" 
#>           rose        rose_75        rose_50        rose_25       charcoal 
#>      "#ff5b92"      "#fb80ab"      "#f6a4c3"      "#f2c9dc"      "#2f2f30" 
#>    charcoal_75    charcoal_50    charcoal_25       lavender        surface 
#>      "#5f5f61"      "#8e8e92"      "#bebec3"      "#ededf4"      "#f8f8fc" 
#>    surface_alt 
#>      "#f3f0fa" 
rladies_cols("purple", "rose", "blue")
#>    purple      rose      blue 
#> "#881ef9" "#ff5b92" "#146af9" 
rladies_cols("charcoal")
#>  charcoal 
#> "#2f2f30" 
```

# RLadies+ R Markdown output formats

Output format functions for creating branded RLadies+ documents. Use
these in the `output` field of your R Markdown YAML header, or select
them from RStudio's "From Template" menu.

## Usage

``` r
rladies_html(toc = TRUE, toc_float = TRUE, ...)

rladies_pdf(toc = FALSE, ...)
```

## Arguments

- toc:

  Logical. Include a table of contents?

- toc_float:

  Logical. Float the table of contents (HTML only)?

- ...:

  Additional arguments passed to the underlying format function.

## Examples

``` r
if (FALSE) { # \dontrun{
# In YAML header:
# output: spellbind::rladies_html
# output: spellbind::rladies_pdf
} # }
```

#' RLadies+ R Markdown output formats
#'
#' Output format functions for creating branded RLadies+ documents.
#' Use these in the `output` field of your R Markdown YAML header,
#' or select them from RStudio's "From Template" menu.
#'
#' @param toc Logical. Include a table of contents?
#' @param toc_float Logical. Float the table of contents (HTML only)?
#' @param ... Additional arguments passed to the underlying format function.
#'
#' @name rladies_formats
#'
#' @examples
#' \dontrun{
#' # In YAML header:
#' # output: spellbind::rladies_html
#' # output: spellbind::rladies_pdf
#' }
NULL

#' @rdname rladies_formats
#' @export
rladies_html <- function(toc = TRUE, toc_float = TRUE, ...) {
  css <- system.file(
    "rmarkdown/templates/rladies-html/resources/rladies-plus.css",
    package = "spellbind"
  )
  rmarkdown::html_document(
    toc = toc,
    toc_float = toc_float,
    css = css,
    ...
  )
}

#' @rdname rladies_formats
#' @export
rladies_pdf <- function(toc = FALSE, ...) {
  header <- system.file(
    "rmarkdown/templates/rladies-pdf/resources/header.tex",
    package = "spellbind"
  )
  rmarkdown::pdf_document(
    toc = toc,
    includes = rmarkdown::includes(in_header = header),
    ...
  )
}

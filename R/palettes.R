rladies_colours <- list(
  purple        = "#881ef9",
  purple_light  = "#a152f8",
  purple_50     = "#bb86f7",
  purple_lighter = "#d4b9f5",
  purple_dark   = "#6b0fd4",
  purple_darker = "#5009a8",
  blue          = "#146af9",
  blue_75       = "#4f8ffb",
  blue_50       = "#8ab5fc",
  blue_25       = "#c4dafe",
  rose          = "#ff5b92",
  rose_75       = "#fb80ab",
  rose_50       = "#f6a4c3",
  rose_25       = "#f2c9dc",
  charcoal      = "#2f2f30",
  charcoal_75   = "#5f5f61",
  charcoal_50   = "#8e8e92",
  charcoal_25   = "#bebec3",
  lavender      = "#ededf4",
  surface       = "#f8f8fc",
  surface_alt   = "#f3f0fa"
)

#' RLadies+ brand colours
#'
#' Access individual colours or the full palette from the RLadies+ brand.
#'
#' @param ... Character names of colours to retrieve.
#'   If empty, returns all colours as a named character vector.
#'
#' @return A named character vector of hex colour values.
#'
#' @examples
#' rladies_cols()
#' rladies_cols("purple", "rose", "blue")
#' rladies_cols("charcoal")
#' @export
rladies_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols)) {
    return(unlist(rladies_colours))
  }
  unlist(rladies_colours[cols])
}

rladies_palettes <- list(
  main     = unname(unlist(rladies_colours[c("purple", "blue", "rose")])),
  full     = unname(unlist(rladies_colours[c(
    "purple", "blue", "rose", "purple_light", "blue_75", "rose_75"
  )])),
  purple   = unname(unlist(rladies_colours[c(
    "purple_darker", "purple_dark", "purple", "purple_light",
    "purple_50", "purple_lighter"
  )])),
  blue     = unname(unlist(rladies_colours[c(
    "blue", "blue_75", "blue_50", "blue_25"
  )])),
  rose     = unname(unlist(rladies_colours[c(
    "rose", "rose_75", "rose_50", "rose_25"
  )])),
  neutral  = unname(unlist(rladies_colours[c(
    "charcoal", "charcoal_75", "charcoal_50", "charcoal_25", "lavender"
  )])),
  diverging = unname(unlist(rladies_colours[c(
    "purple_dark", "purple", "purple_lighter", "lavender",
    "rose_25", "rose", "rose"
  )])),
  light    = unname(unlist(rladies_colours[c(
    "purple_lighter", "blue_25", "rose_25",
    "purple_50", "blue_50", "rose_50"
  )]))
)

#' RLadies+ colour palettes
#'
#' Returns a colour palette function or vector from the RLadies+ brand.
#'
#' @param palette Name of the palette. One of `"main"`, `"full"`, `"purple"`,
#'   `"blue"`, `"rose"`, `"neutral"`, `"diverging"`, or `"light"`.
#' @param reverse Logical. Reverse the palette order?
#' @param ... Additional arguments passed to [grDevices::colorRampPalette()].
#'
#' @return A function that takes an integer `n` and returns `n` interpolated
#'   colours.
#'
#' @examples
#' rladies_pal("main")(3)
#' rladies_pal("purple")(6)
#' rladies_pal("diverging", reverse = TRUE)(7)
#' @export
rladies_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- rladies_palettes[[palette]]
  if (is.null(pal)) {
    cli_abort("Unknown palette: {.val {palette}}. Choose from: {.val {names(rladies_palettes)}}")
  }
  if (reverse) pal <- rev(pal)
  grDevices::colorRampPalette(pal, ...)
}

cli_abort <- function(...) stop(..., call. = FALSE)

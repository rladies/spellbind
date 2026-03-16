rladies_colours <- list(
  purple        = "#881ef9",
  purple_light  = "#a152f8",
  purple_50     = "#bb86f7",
  purple_lighter = "#d4b9f5",
  purple_dark   = "#6b0fd4",
  purple_darker = "#5009a8",
  blue          = "#146af9",
  blue_75       = "#4a8bf8",
  blue_50       = "#80acf6",
  blue_25       = "#b7ccf5",
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

mix_hex <- function(colour, base, amount) {
  col_rgb <- grDevices::col2rgb(colour)[, 1]
  base_rgb <- grDevices::col2rgb(base)[, 1]
  mixed <- round(col_rgb * amount + base_rgb * (1 - amount))
  grDevices::rgb(mixed[1], mixed[2], mixed[3], maxColorValue = 255)
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
    "purple_dark", "purple", "purple_50", "lavender",
    "rose_50", "rose", "rose_75"
  )])),
  light    = unname(unlist(rladies_colours[c(
    "purple_50", "blue_50", "rose_50",
    "purple_lighter", "blue_25", "rose_25"
  )])),
  dark     = unname(c(
    rladies_colours[["purple_darker"]],
    mix_hex(rladies_colours[["blue"]],   rladies_colours[["charcoal"]], 0.75),
    mix_hex(rladies_colours[["rose"]],   rladies_colours[["charcoal"]], 0.75),
    rladies_colours[["purple_dark"]],
    mix_hex(rladies_colours[["blue"]],   rladies_colours[["charcoal"]], 0.5),
    mix_hex(rladies_colours[["rose"]],   rladies_colours[["charcoal"]], 0.5)
  ))
)

#' RLadies+ colour palettes
#'
#' Returns a colour palette function or vector from the RLadies+ brand.
#'
#' @param palette Name of the palette. One of `"main"`, `"full"`, `"purple"`,
#'   `"blue"`, `"rose"`, `"neutral"`, `"diverging"`, `"light"`, or `"dark"`.
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

#' Tint or shade an RLadies+ brand colour
#'
#' Mix a brand colour with lavender (tint) or charcoal (shade), following
#' the RLadies+ brand guidelines. This is the same approach used in the
#' brand SCSS — all tints mix towards lavender white, all shades mix
#' towards Bastille Black.
#'
#' @param colour A colour name from the brand palette (e.g. `"purple"`,
#'   `"blue"`, `"rose"`) or any hex colour string.
#' @param amount Numeric between 0 and 1. The proportion of the original
#'   colour to keep. `amount = 0.75` gives a 75% tint (mostly colour,
#'   lightly mixed with lavender/charcoal).
#' @param mode Either `"tint"` (mix towards lavender) or
#'   `"shade"` (mix towards charcoal).
#'
#' @return A hex colour string.
#'
#' @examples
#' rladies_mix("purple", 0.75)
#' rladies_mix("purple", 0.50, mode = "shade")
#'
#' scales::show_col(vapply(
#'   c(0.25, 0.50, 0.75, 1),
#'   function(x) rladies_mix("blue", x),
#'   character(1)
#' ))
#' @export
rladies_mix <- function(colour, amount = 0.5, mode = c("tint", "shade")) {
  mode <- match.arg(mode)
  if (colour %in% names(rladies_colours)) {
    colour <- rladies_colours[[colour]]
  }
  base <- if (mode == "tint") rladies_colours[["lavender"]] else rladies_colours[["charcoal"]]
  mix_hex(colour, base, amount)
}

cli_abort <- function(...) stop(..., call. = FALSE)

#' RLadies+ colour and fill scales for ggplot2
#'
#' Discrete and continuous colour/fill scales using the RLadies+ brand
#' palettes.
#'
#' @param palette Name of the palette (see [rladies_pal()] for options).
#' @param reverse Logical. Reverse the palette order?
#' @param ... Additional arguments passed to [ggplot2::discrete_scale()] or
#'   [ggplot2::scale_colour_gradientn()].
#'
#' @name scale_rladies
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
#'   geom_point(size = 3) +
#'   scale_colour_rladies() +
#'   theme_rladies()
#'
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_tile() +
#'   scale_fill_rladies_c("purple") +
#'   theme_rladies()
NULL

#' @rdname scale_rladies
#' @export
scale_colour_rladies <- function(palette = "main", reverse = FALSE, ...) {
  pal <- rladies_pal(palette = palette, reverse = reverse)
  ggplot2::discrete_scale("colour", "rladies", palette = pal, ...)
}

#' @rdname scale_rladies
#' @export
scale_color_rladies <- scale_colour_rladies

#' @rdname scale_rladies
#' @export
scale_fill_rladies <- function(palette = "main", reverse = FALSE, ...) {
  pal <- rladies_pal(palette = palette, reverse = reverse)
  ggplot2::discrete_scale("fill", "rladies", palette = pal, ...)
}

#' @rdname scale_rladies
#' @export
scale_colour_rladies_c <- function(palette = "purple", reverse = FALSE, ...) {
  pal <- rladies_pal(palette = palette, reverse = reverse)
  ggplot2::scale_colour_gradientn(colours = pal(256), ...)
}

#' @rdname scale_rladies
#' @export
scale_color_rladies_c <- scale_colour_rladies_c

#' @rdname scale_rladies
#' @export
scale_fill_rladies_c <- function(palette = "purple", reverse = FALSE, ...) {
  pal <- rladies_pal(palette = palette, reverse = reverse)
  ggplot2::scale_fill_gradientn(colours = pal(256), ...)
}

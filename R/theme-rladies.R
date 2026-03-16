#' RLadies+ ggplot2 theme
#'
#' A clean ggplot2 theme using the RLadies+ brand palette. Supports
#' light and dark modes with transparent backgrounds for seamless
#' embedding in branded documents and dashboards.
#'
#' @param base_size Base font size in points.
#' @param mode Either `"light"` (charcoal text on transparent background)
#'   or `"dark"` (lavender text on transparent background).
#' @param grid Logical. Show major gridlines?
#'
#' @return A [ggplot2::theme()] object.
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(colour = rladies_cols("purple"), size = 3) +
#'   labs(title = "Weight vs Fuel Efficiency") +
#'   theme_rladies()
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(colour = rladies_cols("purple_50"), size = 3) +
#'   labs(title = "Dark mode") +
#'   theme_rladies(mode = "dark")
#' @export
theme_rladies <- function(base_size = 13, mode = c("light", "dark"),
                          grid = TRUE) {
  mode <- match.arg(mode)

  purple <- rladies_colours[["purple"]]
  charcoal <- rladies_colours[["charcoal"]]
  lavender <- rladies_colours[["lavender"]]

  if (mode == "dark") {
    fg <- lavender
    grid_col <- if (grid) grDevices::adjustcolor(lavender, alpha.f = 0.15) else "transparent"
    purple <- rladies_colours[["purple_50"]]
  } else {
    fg <- charcoal
    grid_col <- if (grid) lavender else "transparent"
  }

  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = fg),
      plot.title = ggplot2::element_text(
        colour = purple, face = "bold", size = ggplot2::rel(1.2)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = fg, size = ggplot2::rel(0.9)
      ),
      plot.background = ggplot2::element_rect(
        fill = "transparent", colour = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = "transparent", colour = NA
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = grid_col),
      axis.text = ggplot2::element_text(colour = fg),
      axis.title = ggplot2::element_text(colour = fg, face = "bold"),
      legend.text = ggplot2::element_text(colour = fg),
      legend.title = ggplot2::element_text(colour = fg, face = "bold"),
      legend.background = ggplot2::element_rect(
        fill = "transparent", colour = NA
      ),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      strip.text = ggplot2::element_text(
        colour = purple, face = "bold", size = ggplot2::rel(0.9)
      ),
      plot.caption = ggplot2::element_text(colour = fg, hjust = 0)
    )
}

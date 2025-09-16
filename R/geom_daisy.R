#' daisy
#' 
#' @description
#' `geom_daisy` creates a ggplot daisy.
#' @import ggplot2
#' @import ggforce
#'
#' @param x = 1 x position
#' @param y = 1 y position
#' @param petal petal colour
#' @param center center colour
#' @param scale scale of daisy
#'
#' @return ggplot daisy
#' @export
#'
#' @examples
#' 
#' library(ggplot2)
#' library(ggforce)
#' 
#' ggplot() +
#'  geom_daisy() +
#'  coord_fixed() +
#'  theme_void()
#' 
#' 
geom_daisy <- function(
  x = 1,
  y = 1,
  petal = "pink",
  center = "yellow",
  scale = 1
) {
  list(
    # petals
    ggforce::geom_ellipse(
      aes(
        x0 = x + 1.2 * scale,
        y0 = y,
        a = .5 * scale,
        b = 1 * scale,
        angle = 4 * pi / 8
      ),
      colour = "transparent",
      fill = petal
    ),
    ggforce::geom_ellipse(
      aes(
        x0 = x - 1.2 * scale,
        y0 = y,
        a = .5 * scale,
        b = 1 * scale,
        angle = 4 * pi / 8
      ),
      colour = "transparent",
      fill = petal
    ),
    ggforce::geom_ellipse(
      aes(
        x0 = x,
        y0 = y + 1.2 * scale,
        a = .5 * scale,
        b = 1 * scale,
        angle = 8 * pi / 8
      ),
      colour = "transparent",
      fill = petal
    ),
    ggforce::geom_ellipse(
      aes(
        x0 = x,
        y0 = y - 1.2 * scale,
        a = .5 * scale,
        b = 1 * scale,
        angle = 8 * pi / 8
      ),
      colour = "transparent",
      fill = petal
    ),
    ggforce::geom_ellipse(
      aes(
        x0 = x - 1 * scale,
        y0 = y - 1 * scale,
        a = .5 * scale,
        b = 1 * scale,
        angle = 6 * pi / 8
      ),
      colour = "transparent",
      fill = petal
    ),
    ggforce::geom_ellipse(
      aes(
        x0 = x + 1 * scale,
        y0 = y + 1 * scale,
        a = .5 * scale,
        b = 1 * scale,
        angle = 6 * pi / 8
      ),
      colour = "transparent",
      fill = petal
    ),
    ggforce::geom_ellipse(
      aes(
        x0 = x - 1 * scale,
        y0 = y + 1 * scale,
        a = .5 * scale,
        b = 1 * scale,
        angle = 2 * pi / 8
      ),
      colour = "transparent",
      fill = petal
    ),
    ggforce::geom_ellipse(
      aes(
        x0 = x + 1 * scale,
        y0 = y - 1 * scale,
        a = .5 * scale,
        b = 1 * scale,
        angle = 2 * pi / 8
      ),
      colour = "transparent",
      fill = petal
    ),
    # centre
    ggforce::geom_circle(
      aes(
        x0 = x, y0 = y, r = 0.7 * scale
      ),
      colour = "transparent",
      fill = center
    )
  )
}
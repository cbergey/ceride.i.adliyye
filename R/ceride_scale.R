#' @param n Number of colours in palette
#'
#' @param palette Choose from 'my_palettes' list
#'
#' @param alpha transparency
#'
#' @param reverse If TRUE, the direction of the colours is reversed
#'
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' library(ceride.i.adliyye)
#' image(volcano, col = ceride_col("four_colors", 20))
#' @export


ceride_col <- function(palette = "four_colors", n, alpha = 1, reverse = FALSE) {

  pal <- ceride_palettes[[palette]]

  if (is.null(pal))
    stop("Palette not found.")

  if(missing(n)) {
    n <- length(pal)
  }

  if (reverse) {
    pal <- rev(pal)
  }

  pal <- colorRampPalette(pal, alpha)(n)

  return(pal)

}

#' Creates a palette from your specified colours
#'
#' @param palette Choose from 'ceride_palettes' list
#'
#' @param alpha transparency
#'
#' @param reverse If TRUE, the direction of the colours is reversed.
#'
#' @importFrom grDevices colorRampPalette


make_ceride_palette <- function(palette = "four_colors", alpha = 1, reverse = FALSE) {

  function(n) {
    ceride_col(palette, n, alpha, reverse)
  }

}


#' Creates a colour scale for ggplot2
#'
#'
#' @param palette Choose from 'ceride_palettes' list
#'
#' @param reverse logical, Reverse the order of the colours?
#'
#' @param alpha transparency
#'
#' @param discrete whether to use a discrete colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @importFrom ggplot2 scale_colour_manual
#'
#' @examples
#' library(ggplot2)
#' library(ceride.i.adliyye)
#'
#' ggplot(mpg) +
#'  geom_point(aes(x = hwy, y = cty, color = manufacturer)) +
#'  scale_color_me("five_colors")
#'
#' @export
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
#'


scale_colour_me <- function(palette = "five_colors", discrete = TRUE, alpha = 1, reverse = FALSE, ...) {

  if (discrete) {
    discrete_scale("colour", "ceride_col", make_ceride_palette(palette, alpha = alpha, reverse = reverse), ...)
  }
  else {
    scale_color_gradientn(colours = ceride_col(palette, 256, alpha = alpha, reverse = reverse), ...)
  }
}



#' Creates a  fill scale for ggplot2
#'
#' @param palette Choose from 'ceride_palettes' list
#'
#' @inheritParams viridis::scale_fill_viridis
#' @inheritParams ceride_palettes
#'
#' @param discrete whether to use a discrete colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @importFrom ggplot2 scale_fill_manual discrete_scale scale_fill_gradientn
#'
#' @examples
#' library(ggplot2)
#' library(ceride.i.adliyye)
#'
#' ggplot(aes(x = manufacturer, fill = manufacturer), data = mpg) +
#'   geom_bar() +
#'   scale_fill_me()
#' @export
#'


scale_fill_me <- function(palette = "five_colors", discrete = TRUE, alpha = 1, reverse = FALSE, ...) {

  if (discrete) {
    discrete_scale("fill", "ceride_col", make_ceride_palette(palette, alpha = alpha, reverse = reverse), ...)
  }
  else {
    scale_fill_gradientn(colours = ceride_col(palette, 256, alpha = alpha, reverse = reverse), ...)
  }
}

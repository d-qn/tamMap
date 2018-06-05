##' ggplot2 bare theme for map
##' 
##' A basic ggplot2 theme to use for map without graticule/grid lines
##' @import ggplot2
##' @inheritParams ggplot2::theme_minimal
##' @export
##' @examples
##' require(ggplot2)
##' ggplot() + 
##'   geom_sf(data = tilemap_ch()) + 
##'   theme_map()
theme_map = function(base_size=10, base_family="")
{
  theme_minimal(base_size=base_size, base_family = base_family) %+replace%
    theme(
      legend.position = "top", 
      panel.background = element_rect(fill = "transparent", colour = "white"),
      panel.grid = element_line(colour = 'transparent'),
      axis.ticks = element_blank(), 
      axis.title = element_blank(),
      axis.text = element_blank()
    )
}
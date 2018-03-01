##' Tilemap Switzerland
##' 
##' A data.frame (or spatial object) of Switzerland's tilemap 
##' 
##' @param fortified, a logical a data.frame instead of spatial object to be returned
##' @param withLake, a logical have Leman and Bodensee lakes?
##' @source \url{https://github.com/ernstbaslerpartner/Switzerland_Tilemap}
##' @import rgdal dplyr 
##' @return a data.frame or a spatial object tilemap
##' @export
##' @examples
##' tmap_ch <- tilemap_ch()
##' \dontrun{
##' require(tidyverse)
##' # cantonal label
##' tmap.label <- tmap_ch %>% 
##'   group_by(Name) %>% 
##'   summarise(
##'     long = (nth(long, 1) + nth(long, 2) + nth(long, 3)) / 3, 
##'     latmax = max(lat),
##'     latmin = min(lat),
##'     group = first(group)) %>%
##'   ungroup() %>%
##'   mutate(lat = ifelse(Name %in% c('BL', 'AI', 'OW'), latmin + 0.1, latmax- 0.1)) %>%
##'   select(-latmax, -latmin)
##' map_theme <- theme(
##'   legend.position = "none",
##'   panel.grid = element_blank(),
##'   axis.ticks = element_blank(),
##'   axis.title = element_blank(),
##'   axis.text = element_blank()
##' )  
##' # PLOT
##' map_CH <- map_data("world") %>% 
##'   filter(region == "Switzerland") 
##' ggplot(tmap_ch, aes(x = long, y = lat, group = group)) + 
##' geom_polygon(data= map_CH, aes(x = long, y = lat, group = group), fill = NA, colour = "lightgrey") +
##' geom_polygon(aes(fill = Name), colour = "white") + 
##' theme_minimal() + map_theme
##' # add canton 2 letters labels
##' geom_text(data = tmap.label, aes(x = long, y = lat, label = Name), hjust = 0.5, vjust = 1) +
##' ## Plot ballot map, Loi fédérale sur la radio et la télévision 5950
##' fballot_canton <- loadCantonsCHFederalBallot() 
##' vote <- fballot_canton[,which(colnames(fballot_canton) == "5950")] %>% enframe()
##' tmap_ch <- left_join(tmap_ch, vote, by = c("Name" = "name"))
##' ggplot(tmap_ch, aes(x = long, y = lat, group = group)) + 
##' geom_polygon(aes(fill = value), colour = "white") + 
##'  theme_minimal() +
##'  # add canton 2 letters labels
##'  geom_text(data = tmap.label, aes(x = long, y = lat, label = Name), 
##'  hjust = 0.5, vjust = 0.5, colour = "white", size = 2.5) +
##'  map_theme
##' }
tilemap_ch <- function(fortified =T, withLake = F) {
  geojson.file <- list.files(system.file("extdata/geojson", package="swiMap"), 
                             'Switzerland_Tiles_EPSG4326_WGS1984.geojson', full.names = T)
  geojson <- readOGR(geojson.file, "OGRGeoJSON")
  if(fortified) {
    df <- formatShp(geojson)
    if(!withLake) {
      df <- df %>% filter(!Name %in% c('Lac Léman', 'Bodensee'))
    }
    df %>% group_by(Name) %>% mutate(fullCanton = ifelse(length(long) == 4, F, T)) %>% ungroup()   
  } else {
    geojson
  }
}
##' A simple helper that returns a rectangle coordinates that encompasses predominently germanic speaking cantons
##' @export
# rect_germanic <- function() {
#   data.frame(
#     xmin = (7.557853 + 7.484785) / 2, 
#     xmax = 10.106556 + 0.036534, 
#     ymin = , 
#     ymax = 47.57695 + 0.036534
#   )
# }


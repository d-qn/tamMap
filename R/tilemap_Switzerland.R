##' Tilemap Switzerland
##' 
##' @param withLake, a logical have Leman and Bodensee lakes?
##' @source \url{https://github.com/ernstbaslerpartner/Switzerland_Tilemap}
##' @import rgdal dplyr 
##' @examples
##' tmap_ch <- tilemap_ch()
##' \dontrun{
##' # cantonal label
##' tmap.label <- tmap_ch %>% 
##'   group_by(Name) %>% 
##'   summarise(long = mean(long), lat = mean(lat), group = first(group)) %>%
##'   ungroup()
##' ggplot(tmap_ch, aes(x = long, y = lat, group = group)) + 
##' geom_polygon(aes(fill = Name)) + 
##' theme_minimal() +
##' geom_text(data = tmap.label, aes(x = long, y = lat, label = Name), hjust = 0.5, vjust = 0.5)
##' }
##' @export
tilemap_ch <- function(withLake = F) {
  geojson.file <- list.files(system.file("extdata/geojson", package="swiMap"), 
                             'Switzerland_Tiles_EPSG4326_WGS1984.geojson', full.names = T)
  geojson <- readOGR(geojson.file, "OGRGeoJSON")
  df <- formatShp(geojson)
  if(!withLake) {
    df <- df %>% filter(!Name %in% c('Lac Léman', 'Bodensee'))
  }
  df
}
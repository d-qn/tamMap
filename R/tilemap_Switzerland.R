#' Tilemap Switzerland
#' 
#' A data.frame (or spatial object) of Switzerland's tilemap 
#' 
#' @name tilemap_ch
#' @param fortified, a logical a data.frame instead of spatial object to be returned
#' @param withLake, a logical have Leman and Bodensee lakes?
#' @source \url{https://github.com/ernstbaslerpartner/Switzerland_Tilemap}
#' @import sf 
#' @return a data.frame of class simple feature
#' @export
#' @examples
#' tmap_ch <- tilemap_ch()
#' \dontrun{
#' require(sf)
#' require(tidyverse)
#' 
#' # cantonal label, as centroid of the tiles. Needs to be a simple (non-sf) object to be plotted as geom_text
#' tmap.label <- data.frame(label = as.character(tmap_ch$Name), tmap_ch %>% st_centroid() %>% st_coordinates())
#' 
#' # PLOT
#' tm_ch <- ggplot() + 
#'   geom_sf(data = tmap_ch) + 
#'   theme_map()
#' 
#' # add canton 2 letters labels
#' tm_ch + geom_text(data = tmap.label, aes(x = X, y = Y, label = label), hjust = 0.5, vjust = 0.5)
#' 
#' ## Plot ballot map, Loi fédérale sur la radio et la télévision 5950
#' fballot_canton <- loadCantonsCHFederalBallot() 
#' vote <- fballot_canton[,which(colnames(fballot_canton) == "5950")] %>% enframe()
#' tmap_ch <- left_join(tmap_ch, vote, by = c("Name" = "name"))
#' ggplot() + 
#'   geom_sf(data = tmap_ch, aes(fill = value)) +
#'   theme_map() +
#'   # add canton 2 letters labels
#'    geom_text(data = tmap.label, aes(x = X, y = Y, label = label), hjust = 0.5, vjust = 0.5, colour = "white", size = 2.5)
#' }
tilemap_ch <- function(fortified = T, withLake = F) {
  shp.file <- list.files(system.file("extdata/tilemapch", package="tamMap"), 
                             'Switzerland_Tiles_CH1903LV95.shp$', full.names = T)
 
  result <- st_read(shp.file)

  if(!withLake) {
    result <- result %>% filter(!Name %in% c('Lac Léman', 'Bodensee'))
  }
  # add a feature/column with cantonal weight
  result %>% 
    mutate(canton_weight = ifelse(
      Name %in% c('OW', 'NW', 'AI', 'AR', 'BL', 'BS'), 0.5, 
      ifelse(Name %in% c("Lac Léman", "Bodensee"), 0, 1)))
}


##' Return path to OFS geospatial data
##'
##' NOTE: Should be deprecated as soon the same data is available from swisstopo API!!
##'
##' Manual work, download all the G2 resolutions shp (non-vz) locally. 
##' Rmove Lichenstein data:
##'   li_files <- dir("inst/extdata/shp/CH", '.*_li.*', full.names = T) 
##'   file.remove(li_files)
##' So far 2018 until 2012 downloaded and processed.
##' ALl ll the downloaded geodata are in:
##' inst/extdata/shp/CH/
##' 
##' @seealso \url{https://www.bfs.admin.ch/bfs/fr/home/services/geostat/geodonnees-statistique-federale/limites-administratives/limites-communales-generalisees.html}
##' @param y, a numeric of lenghth 1. The year (as of the 1st of Jan) of geo data to get
##' @param features, a string vector with the geographical levels' paths to returns, one of municipalities, municipalities_encl, lakes, agglomerations, cantons, largeRegions and country
##' @param dirGeo, a string of length 1 the directory in the package inst/extdata/shp/ where to look for geo spatial data
##' @import stringr dplyr tibble
##' @return a named vector with the full path to the shapefiles, name the geographical levels
##' @export
##' @examples
##' shp_ch_paths_2018 <- shp_path(2018)
##' \dontrun{
##' require(tidyverse)
##' require(sf)
##' # loop and load the geo data in a named list
##' shp_ch_geodata <- shp_ch_paths_2018 %>% map(function(x) {
##'   layerName <- st_layers(x)
##'   st_read(x, layer = layerName$name) 
##'   }
##' )
##' 
##' theme_map <- theme(
##' legend.position = "bottom", 
##' panel.background = element_rect(fill = "transparent"),
##' panel.grid = element_line(colour = 'transparent'),
##' axis.ticks = element_blank(), axis.title = element_blank(),
##' axis.text = element_blank()
##' )
##' 
##' # plot
##' gp <- ggplot() +
##'   geom_sf(data = shp_ch_geodata$municipalities, aes(fill = AREA_HA), lwd = 0.05, colour = "#0d0d0d") +
##'   geom_sf(data = shp_ch_geodata$cantons, lwd = 0.15, colour = "#333333", fill = NA) +
##'   geom_sf(data = shp_ch_geodata$country, lwd = 0.25, colour = "#000d1a", fill = NA) +
##'   geom_sf(data = shp_ch_geodata$lakes, lwd = 0, fill = "#0066cc")
##'
##'   gp + theme_map +
##'     scale_fill_viridis_c() +
##'     coord_sf(datum = NA, expand = F)
##' }
shp_path <- function(y = 2018, features = c('municipalities', 'cantons', 'lakes', 'country'), dirGeo = 'CH') {
  
  if(is.null(y) || (length(y) > 1) || !is.numeric(y)) {
    stop("\n arg y should be a numeric of lenghth 1")
  }
  stopifnot(is.character(features))
  if(!all(features %in% 
     c('municipalities', 'municipalities_encl', 'lakes', 'agglomerations', 
       'cantons', 'largeRegions', 'country'))) {
    stop("\narg features has to be one of: municipalities, municipalities_encl, lakes, agglomerations, cantons, largeRegions and country!\n")
  }
  
  geo.path <- file.path("extdata/shp", dirGeo)
  files <- dir(system.file(geo.path, package="tamMap"), 
    '.*shp$', full.names = T) 
  
  files_parsed <- str_match_all(basename(files), pattern = "^g2(.)(\\d{2})(.*)\\.shp$")
  files_parsed <- do.call(rbind, files_parsed) %>% 
    as_tibble()
  colnames(files_parsed) <- c('ori', 'type', 'year' , 'suffix')
  
  files_parsed <- files_parsed %>% 
    mutate(level = as.factor(case_when(
      type == 'a' ~ 'agglomerations',
      type == 'b' ~ 'districts',
      type == 'g' & suffix == "" ~ 'municipalities',
      type == 'g' & suffix == "_encl" ~ 'municipalities_encl',
      type == 'k' ~ 'cantons',
      type == 'l' ~ 'country',
      type == 'r' ~ 'largeRegions',
      type == 's' ~ 'lakes',
      TRUE ~ 'unknown'
    ))) %>% 
    mutate(
      year = str_c("20", year) %>% as.numeric()
    ) %>% 
    mutate(ori = files) %>% 
    arrange(desc(year))

  files_parsed <- files_parsed %>% 
    filter(year == y)

  files_parsed %>% 
    filter(level %in% features) %>% 
    select(level, ori) %>% 
    deframe()
}
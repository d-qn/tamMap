##' Return path to (mainly OFS) geospatial data
##'
##' NOTE: Should be deprecated as soon the same data is available from swisstopo API!!
##'
##' Manual work, download all the G2 resolutions shp (non-vz, i.e. on the 01.01) locally. 
##' The MN95 (LV95) was used, not LV03
##' Rmove Lichenstein data:
##'   li_files <- dir("inst/extdata/shp/CH", '.*_li.*', full.names = T) 
##'   file.remove(li_files)
##' So far 2018 until 2012 downloaded and processed.
##' ALl ll the downloaded geodata are in:
##' inst/extdata/shp/CH/
##' 
##' @details For the CH/quartiers Les limites de quartiers sont fournies pour les communes suivantes : Winterthour (230), Zurich (261), Berne (351), Bienne (371), Lucerne (1061), Bâle (2701), Saint-Gall (3203), Lugano (5192), Lausanne (5586) et Genève (6621)
##' @seealso \href{https://www.bfs.admin.ch/bfs/fr/home/services/geostat/geodonnees-statistique-federale/limites-administratives/limites-communales-generalisees.html}{OFS Limites communales généralisées} & \href{https://www.bfs.admin.ch/bfs/fr/home/services/geostat/geodonnees-statistique-federale/limites-administratives/limites-quartiers-villes-suisses.assetdetail.4082002.html}{https://www.bfs.admin.ch/bfs/fr/home/services/geostat/geodonnees-statistique-federale/limites-administratives/limites-quartiers-villes-suisses.assetdetail.4082002.html}
##' @param y, a numeric of lenghth 1. The year (as of the 1st of Jan) of geo data to get. It is currently used for dirGeo CH, ignored for the other options
##' @param features, a string vector with the geographical levels' paths to returns, one of municipalities, municipalities_encl, lakes, agglomerations, cantons, largeRegions and country
##' @param dirGeo, a string of length 1 the directory in the package inst/extdata/shp/ where to look for geo spatial data. Currently: CH, CH/quartiers or World
##' @import stringr dplyr tibble
##' @return a named vector with the full path to the shapefiles, name the geographical levels
##' @seealso \code{processSwissCities} to generate the shapefiles for Swiss cities
##' @export
##' @examples
##' shp_ch_paths_2018 <- shp_path(2018)
##' shp_world <- shp_path(2018, dirGeo = "World")
##' shp_quartiers <- shp_path(dirGeo = "CH/quartiers")
##' shp_villes <- shp_path(dirGeo = "CH/villes")
##' \dontrun{
##' require(tidyverse)
##' require(sf)
##' 
##' ## 1. Plot municipalities, cantons, lakes and national border with the main cities
##' 
##' # loop and load the Swiss geographical levels data in a named list
##' shp_ch_geodata <- shp_ch_paths_2018 %>% map(function(x) {
##'   layerName <- st_layers(x)
##'   st_read(x, layer = layerName$name) %>% 
##'   select(ends_with("NR"), ends_with("NAME"))
##' })
##' # 5 largest Swiss cites
##' villes <- st_read(shp_villes, layer = "swiss_cities") %>% 
##' slice(1:5)
##' # cities' label, as centroid of the tiles. Needs to be a simple (non-sf) object to be plotted as geom_text
##' villes_labels <- data.frame(label = as.character(villes$name), villes %>% st_centroid() %>% st_coordinates())
##' 
##' # plot
##' gp <- ggplot() +
##'   geom_sf(data = shp_ch_geodata$municipalities, aes(fill = GMDNR), lwd = 0.05, colour = "#0d0d0d") +
##'   geom_sf(data = shp_ch_geodata$cantons, lwd = 0.15, colour = "#333333", fill = NA) +
##'   geom_sf(data = shp_ch_geodata$country, lwd = 0.25, colour = "#000d1a", fill = NA) +
##'   geom_sf(data = shp_ch_geodata$lakes, lwd = 0, fill = "#0066cc")
##'
##' gp + 
##'     geom_sf(data = villes, aes(size = pop), fill = NA, shape = 1, colour = "darkred", stroke = 1, alpha = 0.8) +
##'     geom_text(data = villes_labels, aes(x = X, y = Y, label = label), hjust = 0.5, vjust = -1, colour = "darkred", size = 4, alpha = 0.8) +
##'     theme_map() +
##'     scale_fill_viridis_c() +
##'     scale_size_continuous(range = c(4, 9), guide = "none") +
##'     coord_sf(datum = NA, expand = F)
##'     
##' ## 2. Plot quartiers Geneve 
##' 
##' geq <- st_read(shp_quartiers, layer = "quart17") %>% 
##' filter(GMDE == 6621) %>% 
##' select(NR, NAME, GMDE) 
##' plot(geq)
##' 
##' }
shp_path <- function(y = 2018, features = c('municipalities', 'cantons', 'lakes', 'country'), dirGeo = 'CH') {
  
  geo.path <- file.path("extdata/shp", dirGeo)
  files <- dir(system.file(geo.path, package="tamMap"), 
    '.*shp$', full.names = T) 
  
  if(dirGeo == 'CH') {
    if(is.null(y) || (length(y) > 1) || !is.numeric(y)) {
      stop("\n arg y should be a numeric of lenghth 1")
    }
    stopifnot(is.character(features))
    if(!all(features %in% 
            c('municipalities', 'municipalities_encl', 'lakes', 'agglomerations', 
              'cantons', 'largeRegions', 'country'))) {
      stop("\narg features has to be one of: municipalities, municipalities_encl, lakes, agglomerations, cantons, largeRegions and country!\n")
    }
    
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
  } else if(dirGeo == "CH/quartiers") {
    structure(files, names = basename(dirGeo))
  } else if(dirGeo == "CH/villes") {
    structure(files, names = basename(dirGeo))    
  } else if(dirGeo == "World") {
    structure(files, names = dirGeo)
  } else {
    stop("\n The dirGeo provided ", dirGeo, 
         " unknown! It must be one of: CH, CH/quartiers, CH/villes, World\n")
  }
}
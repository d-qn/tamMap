##' Main Swiss cities as simple feature object
##' 
##' Script use to generate a shapefile with Swiss cities, their population and their LV95 coordinates. 
##' Its output needs to be saved in inst/extdata/CH/villes !
##' To be used by \code{shp_path}
##' 
##' @param input the xslx file name Population from the Swiss cities Union in inst/extdata
##' @return NULL it will save 
##' @details From the \href{https://uniondesvilles.ch/fr/Info/Documentation/Statistiques_des_villes_suisses}{Union des villes suisses} the excel file (Chapitre population) was used a population source (31.12.2016)
##' @import readxl dplyr sf ggmap
processSwissCities <- function(input = "T_STST_2018_01_1.xlsx", output = "swiss_cities.shp") {
  file <- dir(file.path(system.file(package="tamMap"), "extdata"), input, full.names = T)
  cities <- read_xlsx(file, skip = 6, col_names = F) %>% 
    select(X__1, X__2) %>% 
    filter(!is.na(X__2)) %>% 
    rename(name = X__1, pop = X__2) %>% 
    # remove asterisk in name
    mutate(name = gsub("\\*", "", name))


  # geocode
  cities_geo <- geocode(paste0(cities$name, ", Switzerland"), output = c("latlon"), messaging = T, source = "google")
  cities <- cbind(cities, cities_geo)
  while(any(is.na(cities$lon))) {
    idx <- which(is.na(cities$lon))
    cat("\n\n", length(idx), " NA lat-lon, rerunning geocode\n")
    cities[idx,c('lon', 'lat')] <- geocode(paste0(cities$name[idx], ", Switzerland"), output = c("latlon"), messaging = T, source = "google")
  }
  
  # reproject coordinates in swisstopo LV95 CRS
  cities_sfc <- st_as_sf(cities, coords = c("lon", "lat"), crs = 4326) %>% 
    st_transform("+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +units=m +no_defs") %>% 
    arrange(desc(pop))
  
  cities_sfc %>% st_write(dsn = output, layer = "cities.shp")
}
##' Swiss communes geographical data
##'
##' Load the Swiss statistical office geographical levels communes data from: \url{https://www.bfs.admin.ch/bfs/fr/home/bases-statistiques/niveaux-geographiques.html}. 
##' They have now an interactive app, choose the Excel file \url{https://www.bfs.admin.ch/bfs/fr/home/bases-statistiques/niveaux-geographiques.html}
##'
##' @name loadCommunesCHgeographicalLevels
##' @return a data.frame with tons of geographical features, check the source excel for more details
##' @details The portrait includes non political OFS ID, i.e. lakes
##' @seealso Typologie des communes et typologie urbain-rural 2012: \url{https://www.bfs.admin.ch/bfs/fr/home/actualites/quoi-de-neuf.assetdetail.2543324.html}
##' @import readxl dplyr tibble
##' @export
##' @examples
##' data <- loadCommunesCHgeographicalLevels()
##' 
##' # Plot map of typologie urbainrural 2012 at the municipality level
##' \dontrun{
##' require(tidyverse)
##' require(sf)
##' 
##' glimpse(data)
##' data <- data %>% 
##'   select(ofsID, name, Canton, 
##'   `Typologie des communes 2012 (9 types) (Typologie des communes 2012 (25, 9 catégories et typologie urbain-rural))`)
##' 
##' # get the geographical data
##' require(tidyverse)
##' require(sf)
##' # loop and load the geo data in a named list
##' shp_ch_paths_2018 <- shp_path(2018)
##' shp_ch_geodata <- shp_ch_paths_2018 %>% map(function(x) {
##'   layerName <- st_layers(x)
##'   st_read(x, layer = layerName$name) %>% 
##'   select(ends_with("NR"), ends_with("NAME"))
##' })
##' 
##' shp_ch_geodata$municipalities <- left_join(
##'   shp_ch_geodata$municipalities, data, by= c('GMDNR' = 'ofsID')) %>% 
##' rename(`Typologie urbainrural 2012` = 
##' `Typologie des communes 2012 (9 types) (Typologie des communes 2012 (25, 9 catégories et typologie urbain-rural))`) %>% 
##' mutate(`Typologie urbainrural 2012` = as.factor(`Typologie urbainrural 2012`))
##' 
##' ggplot() +
##'   geom_sf(data = shp_ch_geodata$municipalities, aes(fill = `Typologie urbainrural 2012`), lwd = 0.05, colour = "#0d0d0d") +
##'   geom_sf(data = shp_ch_geodata$cantons, lwd = 0.15, colour = "#333333", fill = NA) +
##'   geom_sf(data = shp_ch_geodata$country, lwd = 0.25, colour = "#000d1a", fill = NA) +
##'   geom_sf(data = shp_ch_geodata$lakes, lwd = 0, fill = "#0066cc") +
##'   theme_map()
##' }
loadCommunesCHgeographicalLevels <- function() {
  data.path <- dir(system.file("extdata", package="tamMap"), "^be-f-00.04-rgs-01\\.xlsx", full.names = T)
  
  # get the data date
  metadata <- readxl::read_excel(data.path, sheet = "Métadonnées", range = "A10:A10", col_names = F)
  if(length(metadata) == 0) {
    warning(paste0("Metadata of ", data.path, " could not be parsed!\n"))
  } else {
    date <- gsub(".*( \\d+\\.\\d+\\.\\d+).*", "\\1", metadata)
    cat("\n\n----------  Load: Niveaux geographiques de la Suisse, au ", 
        date, "  ----------\n\n")
  }
  data.read <- readxl::read_excel(data.path, skip = 3, col_names = F)

  stopifnot(!any(is.na(data.read[,1])))
  
  # colnames are on 2 lines, concatenate
  coln1 <- readxl::read_excel(data.path, skip = 1, n_max = 1, col_names = F) %>% 
    unlist(use.names = F) 
  coln2 <- readxl::read_excel(data.path, skip = 0, n_max = 1, col_names = F) %>% 
    unlist(use.names = F) 
  coln3 <- readxl::read_excel(data.path, skip = 2, n_max = 1, col_names = F) %>% 
    unlist(use.names = F) 
  coln2 <- tidyr::fill(enframe(coln2), value) %>% .$value
  coln2 <- c(rep("", length(coln1) - length(coln2)), coln2)
  colnames(data.read) <- paste0(coln1, " (", coln2, ")")
  colnames(data.read) <- gsub("\ \\(\\)", "", colnames(data.read))

  #rename columns
  data.read <- data.read %>% 
    rename(ofsID = `Numéro de la commune`, name = `Nom de la commune`) %>% 
    select(-Canton, -`Numéro du district`, -`Nom du district`)

  #Assign factors instead of numeric to the factors
  colnames2sheet <- colnames(data.read)[-c(1,2)]
  names(colnames2sheet) <- coln3
  
  for(i in 1:length(colnames2sheet)) {
    st <- colnames2sheet[i]
    stopifnot(st %in% colnames(data.read))
    
    fname <- readxl::read_excel(data.path, sheet = names(st), skip = 1, col_names = T)
    
    idx <- match(unlist(data.read[,st], use.names = F), fname$Code)
    stopifnot(!any(is.na(idx)))
    # overwrite with factor
    data.read[,st] <- factor(fname$Label[idx])
  }
  
  data.read
}
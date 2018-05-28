##' Swiss communes geographical data
##'
##' Load the Swiss statistical office communes data from: \url{https://www.bfs.admin.ch/bfs/fr/home/bases-statistiques/niveaux-geographiques.html}
##'
##' @return a data.frame with tons of geographical features, check the source excel for more details
##' @details The portrait includes non political OFS ID, i.e. lakes
##' @seealso Typologie des communes et typologie urbain-rural 2012: \url{https://www.bfs.admin.ch/bfs/fr/home/actualites/quoi-de-neuf.assetdetail.2543324.html}
##' @import readxl dplyr
##' @export
##' @examples
##' data <- loadCommunesCHgeographicalLevels()
##' head(data)
##' \dontrun{
##' 
##' }
loadCommunesCHgeographicalLevels <- function() {
  data.path <- dir(system.file("extdata", package="tamMap"), "^be-b-00.04-rgs-01\\.xlsx", full.names = T)
  
  # get the data date
  metadata <- readxl::read_excel(data.path, range = "N1:N1", col_names = F)
  if(length(metadata) == 0) {
    warning(paste0("Metadata of ", data.path, " could not be parsed!\n"))
  } else {
    date <- gsub(".*( \\d+\\.\\d+\\.\\d+).*", "\\1", metadata)
    cat("\n\n----------  Load: Niveaux geographiques de la Suisse, au ", 
        date, "  ----------\n\n")
  }
  data.read <- readxl::read_excel(data.path, skip = 4)

  #discard row without OFS #
  data.read <- data.read[!is.na(data.read[,1]),]

  #reformat some columns
  data.read$Canton <- as.numeric(data.read$Canton)

  #rename columns
  data.read <- data.read %>% rename(ofsID = `N° \r\nOFS`, name = `Nom de la commune`)
  # colnames might have line break and hypen
  colnames(data.read) <- gsub("(\n|-|\\*$)", "", colnames(data.read))

  data.read
}
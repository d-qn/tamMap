##' Swiss communes geographical data
##'
##' Load the Swiss statistical office communes data from: \url{http://www.bfs.admin.ch/bfs/portal/fr/index/regionen/thematische_karten/01/01.html}
##'
##' @return a data.frame with tons of geographical features, check the source excel for more details
##' @details The portrait includes non political OFS ID, i.e. lakes
##' @import readxl dplyr
##' @export
##' @examples
##' data <- loadCommunesCHgeographicalLevels()
##' head(data)
loadCommunesCHgeographicalLevels <- function() {
  data.path <- dir(system.file("extdata", package="swiMap"), "^be-b-00.04-rgs-01\\.xlsx", full.names = T)
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
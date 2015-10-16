##' Swiss communes geographical data
##'
##' Load the Swiss statistical office communes data from: \url{http://www.bfs.admin.ch/bfs/portal/fr/index/regionen/thematische_karten/01/01.html}
##' 
##' @return a data.frame with tons of geographical features, check the source excel for more details
##' @import readxl dplyr
##' @export
##' @examples
##' data <- loadCommunesCHgeographicalLevels()
##' head(data)
loadCommunesCHgeographicalLevels <- function() {
  data.path <- dir(system.file("extdata", package="swiMap"), "be-b-00.04-rgs-01\\.xls", full.names = T)
  data.read <- read_excel(data.path, skip = 13)

  #discard row without OFS #
  data.read <- data.read[!is.na(data.read[,1]),]
  
  #reformat some columns
  data.read$Canton <- as.numeric(data.read$Canton)
  
  #rename columns
  data.read <- data.read %>% rename(ofsID = `N° \nOFS`, name = `Nom de la commune`)
  # colnames might have line break and hypen
  colnames(data.read) <- gsub("(\n|-|\\*$)", "", colnames(data.read))
  
  data.read
}
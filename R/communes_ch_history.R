##' Swiss communes history
##'
##' Helper functions to reconstruct the merging, scission, name changes of Swiss communes
##' 
##' @name communes_CH_history
##' @param start,end Date objets: the timeframe at which to query the list of communes changes
##' @return a data.frame
##' @details
##' 
##' \url{http://www.bfs.admin.ch/bfs/portal/fr/index/news/publikationen.Document.92111.pdf}
##' 
##' GINIART code:
##' 11 Commune politique
##' 12 Territoire non attribué à une commune
##' 13 Partie cantonale de lac
##' 15 District
##' 16 Canton sans districts
##' 17 Territoire non attribué à un district
##' 20 Première saisie commune/district
##' 21 Création commune/district
##' 22 Changement de nom du district
##' 23 Changement de nom de la commune
##' 24 Rattachement à un autre district/canton
##' 26 Modification du territoire de la commune
##' 27 Renumérotation formelle de la commune/du district
##' 29 Radiation commune/district
##' 30 Annulation de la mutation
##' @export
##' @examples
##' start <- as.Date("2012-01-01")
##' data <- loadCommunesCHdata()
##' ## Get all the communes "Fusion"  [A] + [B] = [C]
##' # get the ID of the new communes created 
##' ginimut <- data[which(data$GINIART == 21 & data$GINIDAT >= start),'GINIMUT']
##' # get all the communes "radiated" radiée/suprimée
##' data[which(data$GFINMUT %in% ginimut & data$GFINART == 29),]
##' # Get for each commune created between start & end time, the GBFSNR of the merged communes
##' result <- lapply(ginimut, function(i) {
##'   # get the nr and name of the created commune
##'   nr_name <- data[which(data$GINIART == 21 & data$GINIMUT == i), c('GBFSNR', 'GNAME')]
##'   # get the nr of the radiated communes
##'   radiatedN <- data[which(data$GFINART == 29 & data$GFINMUT == i),'GBFSNR']
##'   if(identical(radiatedN, integer(0))) {
##'     list(nr = unlist(nr_name[1]), name = unlist(nr_name[2]), radiated = NULL)
##'   } else {
##'     list(nr = unlist(nr_name[1]), name = unlist(nr_name[2]), radiated = unlist(radiatedN))
##'   }
##' })
##' names(result) <- ginimut
##' result
loadCommunesCHdata <- function(start = "2012-01-01", end = Sys.Date()) {
  start <- as.Date(start)
  end <- as.Date(end)
  
  # get the path to communes data txt file 
  data.path <- dir(system.file("extdata", package="swiMap"), "GDEHist_GDE\\.txt", full.names = T)
  data <- read.csv(data.path, sep ="\t",  header = FALSE, stringsAsFactors = F, encoding = "latin1")
  # colnames
  colnames(data) <- c('GHSTNR', 'BHSTNR', 'KTKZ', 'GBFSNR', 'GNAME', 'GNAMK', 'GARTE', 
                      'GSTAT', 'GINIMUT', 'GINIART', 'GINIDAT', 'GFINMUT', 'GFINART', 
                      'GFINDAT','GMUTDAT')
  # tranform to dates
  data$GINIDAT <- as.Date(data$GINIDAT, format = "%d.%m.%Y")
  data$GFINDAT <- as.Date(data$GFINDAT, format = "%d.%m.%Y")
  data$GMUTDAT <- as.Date(data$GMUTDAT, format = "%d.%m.%Y")
  
  data[which((data$GFINDAT >= start | data$GINIDAT >= start) & (data$GFINDAT <= end | data$GINIDAT <= end)),]
}
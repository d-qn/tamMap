##' Swiss communes socio-demographic indicators
##'
##' Load socio-demographic data by the Swiss Statistical Office.  
##' 
##' @name communes_CH_indicators
##' @return a matrix
##' @seealso Office fédéral de la statistique > Les Régions > Communes > Données et explications (portraits): \url{http://www.bfs.admin.ch/bfs/portal/fr/index/regionen/02/daten.html} \cr
##' The cleaning up of raw xls file: \itemize{
##'   \item Remove all the empty lines (header has 3 lines: Indicator type, indicator, year)
##'   \item Reorder the header for: indicator type, year, indicator
##'   \item Remove in the data table, the line for Switzerland 
##'   \item Remove the text at the bottom
##'   \item Save as csv
##' }
##' @details The matrix has the commune BFS code as rowname and 3 attributes: \itemize{
##' \item \code{communeName} the text name of the commune (same length as the matrix length)
##' \item \code{indicatorYear} & \code{indicatorGroup} respectively the year and the category of the communal indicator (both of same length as the matrix width). See \url{http://www.bfs.admin.ch/bfs/portal/fr/index/regionen/02/key.html}
##' }
##' @export
##' @examples
##' communeData <- loadCommunesCHportraits()
##' colnames(communeData)
##' rownames(communeData)
##' 
##' # Select only "surface" indicators
##' colIdx <- which(attr(communeData, "indicatorGroup") == "Surface")
##' head(communeData[,colIdx])
##' 
##' zipcode <- loadCHzipcode()
##' match(zipcode$Gemeindename, attr(communeData, "communeName"))
loadCommunesCHportraits <- function() {

  # get the path to communes data txt file 
  data.path <- dir(system.file("extdata", package="swiMap"), "communesCH_2015_indicators_je-f-21.03.01.csv", full.names = T)
  data.read <- read.csv(data.path, skip = 2, header = TRUE, stringsAsFactors = F, check.names = FALSE)
 
   # save ony the indicator values as a matrix
  data <- data.matrix(data.read[,-c(1:2)])
  
  # rownames are commune BFS code
  rownames(data) <- data.read[,1]
  # attr communeName is the text name
  attr(data, "communeName") <- data.read[,2]
  
  metadata <- read.csv(data.path, nrows = 1, header = TRUE, stringsAsFactors = F, check.names = FALSE)

  attr(data, "indicatorYear") <- unlist(metadata)[-c(1:2)]
  attr(data, "indicatorGroup")<- names(metadata)[-c(1:2)]
  
  data
}

##' Load the NPA/PLZ/ Zip code for Switzerland  
##' 
##' @rdname communes_CH_indicators
##' @return a data.frame
##' @seealso \url{http://opendata.admin.ch/de/dataset/ch-swisstopo-vd-ortschaftenverzeichnis_plz/resource/35001b61-e7c1-4124-89fa-17fac7b1139e} from: \url{http://opendata.admin.ch/de/dataset/ch-swisstopo-vd-ortschaftenverzeichnis_plz}
##' @export
loadCHzipcode <- function() {
  
  # get the path to communes data txt file 
  data.path <- dir(system.file("extdata", package="swiMap"), "PLZO_CSV_LV03.csv", full.names = T)
  read.csv(data.path, sep = ";", header = TRUE, stringsAsFactors = F, check.names = FALSE, encoding = "latin1")
}
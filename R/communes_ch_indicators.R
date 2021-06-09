#' Swiss communes socio-demographic indicators
#'
#' Load socio-demographic data by the Swiss Statistical Office.  
#' 
#' Run \code{processPortraitsRegionauxCommune()} to generate a readable csv file for \code{loadCommunesCHportraits}
#' 
#' @name loadCommunesCHportraits
#' @rdname communes_CH_indicators
#' @return a matrix
#' @seealso Office fédéral de la statistique > Les Régions > Communes > Données et explications (portraits): \url{https://www.bfs.admin.ch/bfs/fr/home/statistiques/statistique-regions/portraits-regionaux-chiffres-cles/communes.assetdetail.328133.html} \cr
#' The cleaning up of raw xls file: \itemize{
#'   \item Remove all the empty lines (header has 3 lines: Indicator type, indicator, year)
#'   \item Reorder the header for: indicator type, year, indicator
#'   \item Remove in the data table, the line for Switzerland 
#'   \item Remove the text at the bottom
#'   \item Save as csv
#' }
#' @details The matrix has the commune BFS code as rowname and 3 attributes: \itemize{
#' \item \code{communeName} the text name of the commune (same length as the matrix length)
#' \item \code{indicatorYear} & \code{indicatorGroup} respectively the year and the category of the communal indicator (both of same length as the matrix width). See \url{http://www.bfs.admin.ch/bfs/portal/fr/index/regionen/02/key.html}
#' }
#' @export
#' @examples
#' communeData <- loadCommunesCHportraits()
#' colnames(communeData)
#' head(rownames(communeData))
#' attr(communeData, "indicatorYear")
#' attr(communeData, "indicatorGroup")
#' 
#' # Select only "surface" indicators
#' colIdx <- which(attr(communeData, "indicatorGroup") == "Surface")
#' head(communeData[,colIdx])
#' 
#' zipcode <- loadCHzipcode()
#' # get the zipcode of each commune
#' zp <- zipcode[match( attr(communeData, "communeName"), zipcode$Gemeindename),]
#' 
#' \dontrun{
#' library(tidyverse)
#'
#' ## 1. Show the %foreigners vs % vote UDC
#'   g1 <- ggplot(data = as.data.frame(communeData), aes(x = `Etrangers en %`, y = UDC)) + 
#'   geom_point(aes(size = Habitants), alpha = 0.5)
#'   g1
#' 
#' ## 2. Barcode chart: Densité de la population par km²" 
#' ## ------------- TO DO add some text what is highlighted,  better colour use, ..
#' communesList <- communes_list() %>% 
#'   select(-`Numéro d'historisation`, -`Numéro du district`, -`Date de l'inscription`)
#' 
#' dd <- communeData %>% as.data.frame() %>% 
#'   rownames_to_column(var = "Numéro de la commune") %>% 
#'   select(`Numéro de la commune`, `Densité de la population par km²`, 
#'   `Surface agricole en %`, `PS`) %>% 
#'   mutate(`Numéro de la commune` = as.integer(`Numéro de la commune`))
#' dd <- left_join(dd, communesList) %>% 
#'   gather(feature, value, -`Numéro de la commune`, 
#'   -Canton, -`Nom du district`, -`Nom de la commune`)
#' ## colour group
#' ## 6617 Cologny < GE < remaining
#' ddd <- dd %>% mutate(
#'   cgroup = as.factor(ifelse(`Numéro de la commune` == 6617, 1,
#'     ifelse(Canton == "GE", 2, 3))),
#'   alpha = case_when(cgroup == 1 ~ 1, cgroup == 2 ~ 0.7, cgroup == 3 ~ 0.1),
#'   size  = case_when(cgroup == 1 ~ 1, cgroup == 2 ~ 0.5, cgroup == 3 ~ 0.3),
#'   )
#' 
#' barcode <- ggplot(ddd) +
#'   geom_segment(aes(x = value, xend = value, colour = cgroup, alpha = alpha, size = size), 
#'   y = -0.5, yend = 0.5) +
#'   scale_y_continuous(expand = c(0,0), limits=c(-0.5, 0.5), labels = NULL) +
#'   theme_minimal() +
#'   facet_wrap(~feature, ncol = 1, scales = "free_x") +
#'   scale_alpha_identity() +
#'   scale_size_identity() +
#'   scale_colour_manual(values = c('#003333', '#008080', '#00e6e6'))
#' 
#' barcode + theme(strip.text = element_text(hjust = 0), panel.grid = element_blank())
#' }
loadCommunesCHportraits <- function() {

  # get the path to communes data txt file 
  data.path <- dir(system.file("extdata", package="tamMap"), 
                   "communesCH_2021_indicators_je-f-21.03.01.csv", full.names = T)
  data.read <- read.csv(data.path, skip = 3, header = TRUE, 
                        stringsAsFactors = F, check.names = FALSE)

  # save ony the indicator values as a matrix
  data <- data.matrix(
    data.frame(apply(data.read[,-c(1:2)], 2, function(x) suppressWarnings(as.numeric(x))))
  )
  colnames(data) <- colnames(data.read[-c(1,2)])
  
  # rownames are commune BFS code
  rownames(data) <- data.read[,1]
  # attr communeName is the text name
  attr(data, "communeName") <- data.read[,2]
  
  metadata <- read.csv(data.path, nrows = 2, header = T, stringsAsFactors = F, check.names = FALSE)

  attr(data, "indicatorYear") <- unlist(metadata[2, -c(1:2)], use.names = F)
  attr(data, "indicatorGroup")<- unlist(metadata[1, -c(1:2)], use.names = F)
  
  stopifnot(
    length(attr(data, "communeName")) == nrow(data), 
    length(attr(data, "indicatorYear")) == ncol(data),
    length(attr(data, "indicatorYear")) == ncol(data)
  )
  data
}

#' Process Portraits régionaux de la Suisse commune xls
#' 
#' Useful to update the source data of commune socio-economic indicators loaded by \code{loadCommunesCHportraits}
#' Warning of possible break if the OFS changes the data structure of the xls file!!
#' 
#' @rdname communes_CH_indicators
#' @name processPortraitsRegionauxCommune
#' @param file the raw excel file name from the Swiss Statistical office with indicators by commune
#' @param output the output file name to be saved in the package inst/extdata
#' @return NULL
#' @seealso \url{http://opendata.admin.ch/de/dataset/ch-swisstopo-vd-ortschaftenverzeichnis_plz/resource/35001b61-e7c1-4124-89fa-17fac7b1139e} from: \url{http://opendata.admin.ch/de/dataset/ch-swisstopo-vd-ortschaftenverzeichnis_plz}
#' @import tidyr dplyr readxl
#' @importFrom utils write.csv
#' @export
#' @examples
#' \dontrun{
#' processPortraitsRegionauxCommune()
#' }
processPortraitsRegionauxCommune <- function(
  file = 'je-f-21.03.01.xlsx', 
  output = 'communesCH_2021_indicators_je-f-21.03.01.csv'
) {
  
  out.path <- paste0(getwd(), "/inst/extdata/", output)
  
  # get the path to communes data txt file 
  data.path <- dir(system.file("extdata", package="tamMap"), file, full.names = T)
  data.read <- readxl::read_excel(data.path, skip = 3, col_names = F)
  # discard empty lines
  row.idx <- apply(data.read, 1, function(ll) !all(is.na(ll)))
  data.read <- data.read[row.idx,]
  
  #headers
  header1 <- unlist(data.read[1,], use.names = F)
  header1 <- as.data.frame(header1) %>% 
    fill(header1) %>% 
    select(header1) %>% 
    unlist(use.names =F) %>% 
    as.character()
  header1 <- gsub(" 1)", "", header1, fixed = T)
  
  header2 <- unlist(data.read[2,], use.names = F)
  years <- unlist(data.read[3,], use.names = F)
  years <- suppressWarnings(ifelse(is.na(as.numeric(years)), years, as.numeric(years)))
  
  # reload data skipping everything until first commune
  data.read <- readxl::read_excel(data.path, skip = 9, col_names = F)
  # discard empty lines
  row.idx <- apply(data.read, 1, function(ll) !all(is.na(ll)))
  data.read <- data.read[row.idx,]
  
  data <- rbind(
    ifelse(is.na(header1), "", header1), 
    ifelse(is.na(years), "", years), 
    header2, data.read
  )
  write.csv(as.data.frame(data), file = out.path, row.names = F)
  cat("\n\n ------ \n Saved in:" , out.path)
}

#' Load the NPA/PLZ/ Zip code for Switzerland  
#' 
#' @name loadCHzipcode
#' @return a data.frame
#' @importFrom utils read.csv2
#' @seealso \url{http://opendata.admin.ch/de/dataset/ch-swisstopo-vd-ortschaftenverzeichnis_plz/resource/35001b61-e7c1-4124-89fa-17fac7b1139e} from: \url{http://opendata.admin.ch/de/dataset/ch-swisstopo-vd-ortschaftenverzeichnis_plz}
#' @export
loadCHzipcode <- function() {
  # get the path to communes data txt file 
  data.path <- dir(system.file("extdata", package="tamMap"), "PLZO_CSV_LV03.csv", full.names = T)
  return(read.csv2(data.path, header = TRUE, stringsAsFactors = F, check.names = FALSE, encoding = "latin1"))
}

##' Swiss names
##'
##' Functions to convert Switzerland's cantons, districts and communes names to their abbreviations and official BFS ID
##' TODO: expand countrycode for more languages (see in inst/ countrynames.csv )
##' 
##' @name swi_CHnames
##' @examples
##' print(canton_CH)
##' @export
canton_CH  <- read.csv(system.file("extdata", "CantonCH_iso.csv", package="swiMap"))


##' Convert Swiss canton names to different languages and abbreviations
##' 
##' Convert Swiss canton names to different languages and abbreviations
##' 
##' @rdname swi_CHnames
##' @param query a character vector of Swiss canton names in any language
##' @param output a character string, the ouput format 
##' @details See \code{canton_CH}
##' @export
##' @examples
##' canton_query <- c('Argovie', 'Appenzell Rh.-I.', 'St. Gallen', 'Zürich', 'randomChar', 'Vaudx')
##' canton_namesStrict(canton_query)
##' canton_namesStrict(canton_query, output = "english")
canton_namesStrict <- function(query, output = 'iso2') {
  stopifnot(exists("canton_CH"))
  if (!output %in% colnames(canton_CH) || length(output) != 1) {
    stop ("output needs to be one of:", paste(colnames(canton_CH), collapse =" "))
  }
  
  result <- query
  for(i in 1:length(query)) {
    q <- query[i]
    nrow <- which(q == canton_CH, T)
    
    if(length(nrow) == 0) {
      warning("\n", q, " could not be matched!\t", "NA returned instead!")
    }
    if(length(unique(nrow[,1])) > 1) {
      warning("\n", q, " matched multiple cantons!\t", "NA returned instead!")
    }
    if(length(unique(nrow[,1])) == 1) {
      q <- as.character(canton_CH[unique(nrow[,1]),output])
    } else {
      q <- NA
    }
    result[i] <- q
  }
  result
}

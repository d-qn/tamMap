##' country translation
##'
##' Functions to convert country names given in iso2 or iso3 to 10 different languages
##' 
##' @name countryTranslation
##' @examples
##' print(countryTranslations)
##' @export
countryTranslations  <- read.csv(system.file("extdata", "countrynames_ordered.csv", package="swiMap"), stringsAsFactors = FALSE, check.names = F,  na.strings = "")

##' Convert iso country codes to their full names in 10 possible languages
##' 
##' Convert country names to 10 possible languages
##' 
##' @rdname countryTranslation
##' @param query a character vector of iso2, iso3 or iso numeric country codes
##' @param output a character vector containing the iso2 output languages ("EN", "DE", "FR", "IT", "ES", "PT","RU", "ZH", "JA","AR")
##' @return a data.frame of dimension \code{query} by \code{output}
##' @export
##' @examples
##' countryTranslation(query = c('AF', 'FR', 'US', 'CH', 'JP'), output = "EN")
countryTranslation <- function (query, output) {
  # check format query and output
  idx <- which(!is.na(query) | query != "")
  stopifnot(length(idx) > 0, sapply(query[idx], nchar) <= 3)
  stopifnot(output %in% colnames(countryTranslations))
  
  if(all(sapply(query[idx], nchar) == 2)) {
    idx.row <- match(query, countryTranslations[,1])   
  } else if (all(sapply(query[idx], nchar) == 3) || !all(grepl("^\\d+", query[idx]))) {
    idx.row <- match(query, countryTranslations[,2])  
  } else if (all(sapply(query[idx], nchar) == 3) && all(grepl("^\\d+", query[idx]))) {
    idx.row <- match(query, countryTranslations[,3])  
  } else {
    stop("query format not valid!")
  }
  
  result <- cbind(code = query, countryTranslations[idx.row, match(output, colnames(countryTranslations))])
  colnames(result)[-1] <- output
  result
}
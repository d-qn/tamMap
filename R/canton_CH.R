#' Swiss canton names - convert them to different languages and abbreviations
#' 
#' Functions to convert Switzerland's cantons, districts and communes names to their abbreviations and official BFS ID
#' @rdname canton_CH
#' @name canton_CH
#' @details a data.frame used by \code{canton_namesStrict}
#' @examples
#' \dontrun{
#'   canton_CH()
#' }
#' @export
canton_CH  <- function() {
  read.csv(system.file("extdata", "CantonCH_iso.csv", package="tamMap"))
}

#' @name canton_namesStrict
#' @rdname canton_CH
#' @param query a character vector of Swiss canton names in any language
#' @param output a character string, the ouput format 
#' @details See \code{canton_CH}
#' @export
#' @examples
#' canton_query <- c('Argovie', 'Appenzell Rh.-I.', 'St. Gallen', 'Zürich', 'Vaud')
#' canton_namesStrict(canton_query)
#' canton_namesStrict(canton_query, output = "eng")
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

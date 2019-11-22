#' Helper function to query OFS commune application
#'
#' Basic function to query ofs commune app
#' @param name, a character string the name of the muncipality to query
#' @param start_time,end_time a character string the start and end dates as dd.mm.yyy
#' @param minDist minimum Levenshtein distance for string matching
#' @export
#' @import rvest xml2 dplyr RecordLinkage
#' @seealso https://www.agvchapp.bfs.admin.ch/fr/communes/query
#' @examples
#' \dontrun{
#' commune_suisse_api(name = "Morges")
#' }
commune_suisse_api <- function(
  name = '',
  start_time = "01.01.2019",
  end_time   = "01.01.2019",
  minDist = 0.95
  ) {
  
  base_url <- "https://www.agvchapp.bfs.admin.ch/fr/communes/results"
  
  query <- paste0(
    base_url,
    "?Name=", name,
    "&EntriesFrom=", start_time,
    "&EntriesTo=", end_time
  )
  
  tb <- read_html(query) %>% 
    html_table()
  if(!identical(tb, list())) {
    tb <- tb[[1]]
    
    if(nrow(tb) > 0) {
      
      distance <- levenshteinSim(name, tb$`Nom de la commune`)
      if(max(distance) >= minDist) {
        ii <- which.max(distance)
        tb[ii,c(5,6)]
      } 
    }
  }
  
}
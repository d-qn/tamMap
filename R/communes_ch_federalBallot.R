#' Federal ballot results by communes
#'
#' Load any federal ballots resuts by communes: \url{https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1703030000_101}. 
#' Run \code{processFederalBallotByCommunes()} to generate a readable csv file for \code{loadCommunesCHFederalBallot}
#' This will download the px file from \url{https://www.bfs.admin.ch/bfs/en/home/statistics/politics/popular-votes.assetdetail.1363949.html}, 
#' process it and save it as a Rdata file. Useful to update the ballot results loaded by  \code{loadCommunesCHFederalBallot}
#' Warning of possible break if the OFS changes the data structure of the px file!!
#' 
#' @rdname communes_ch_federalBallot
#' @name loadCommunesCHFederalBallot
#' @param file the name of the csv file processed by processFederalBallotByCommunes to load
#' @return a matrix, rownames are communes and colnames are federal ballot IDs. Check the attributes ballotName and date (same length as ncol) and communeName (same lenghth as nrow)
#' @export
#' @examples
#' \dontrun{
#' require(tidyverse)
#' fBallot <- loadCommunesCHFederalBallot()
#' attr(fBallot, "ballotName")
#' # get only naturalisation facilitée ballots
#' cidx <- match(c("3150", "4110", "5100", "5110"), colnames(fBallot))
#' 
#' ballot_nat <- fBallot[,cidx]
#' colnames(ballot_nat) <- attr(fBallot, "ballotName")[cidx]
#' ballot_nat <- ballot_nat %>% 
#'   data.frame() %>% 
#'   rownames_to_column(var = "GMDNR") %>% 
#'   mutate(GMDNR = as.integer(GMDNR))
#' 
#' # Plot maps
#' require(sf)
#' 
#' # Get municipalities geodata
#' shp_ch_paths_2016 <- shp_path(2016, features=c('municipalities'))
#' shp_ch_geodata <- shp_ch_paths_2016 %>% map(function(x) {
#'   layerName <- st_layers(x)
#'   st_read(x, layer = layerName[[1]]) %>% 
#'   select(ends_with("NR"), ends_with("NAME"))
#' }) 
#' municipalities <- shp_ch_geodata[[1]]
#' 
#' # Bind ballot data and make it long (duplicate)
#' municipalities <- left_join(municipalities, ballot_nat) %>% 
#'   select(-BZNR, -GRNR) %>% 
#'   gather(ballot, percYes, -GMDNR, -KTNR, -GMDNAME, -geometry)
#'   
#' # discretize data with breaks
#' brks <- seq(from = 0, to = 1, length.out = 11) * 100
#' municipalities <- municipalities %>% 
#'   mutate(bins = cut(percYes, breaks = brks, right = F))
#' 
#'  ggplot() +
#'    geom_sf(data = municipalities, aes(fill = bins), lwd = 0) +
#'    facet_wrap(~ ballot) + 
#'    scale_fill_brewer(palette = "BrBG" , drop = F) +
#'    coord_sf(datum = NA, expand = F) +
#'    theme_map()
#' }
loadCommunesCHFederalBallot <- function(
  file = "federalBallot_communes.RData") 
{
  data.path <- dir(system.file("extdata", package="tamMap"), file, full.names = T)
  load(data.path)
  ddd
}

#' @name processFederalBallotByCommunes
#' @rdname communes_ch_federalBallot
#' @param url the URL to the px file with all federal ballots
#' @param output the output file name to be saved in the package inst/extdata
#' @return NULL
#' @import tidyr dplyr pxR stringr
#' @seealso \url{https://www.bfs.admin.ch/bfs/en/home/statistics/politics/popular-votes.assetdetail.1363949.html}
#' @export
#' @examples
#' \dontrun{
#' processFederalBallotByCommunes()
#' }
processFederalBallotByCommunes <- function(
  url = 'https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1703030000_101', 
  output = 'federalBallot_communes.RData'  
) {
  out.path <- paste0(getwd(), "/inst/extdata/", output)
  
  tmpdir <- tempdir()
  px.file <- download.file(url, paste0(tmpdir, "/", "federalBallot.px"))
  # px.read <- read.px(filename = paste0(tmpdir, "/", "federalBallot.px"))
  # 
  # data <- px.read$DATA[[1]]
  # data_old <- data
  
  data <- ofsPx_wrangle(
    px.file = paste0(tmpdir, "/", "federalBallot.px"),
    langout = 'fr', 
    attachCode = c(
      "Canton.......District........Commune.........",
      "Date.et.objet"))
  
  ## subset to take only %oui and municipality results
  dd <- data %>% 
    dplyr::filter(Résultat == 'Oui en %', 
                  grepl("......", `Canton.......District........Commune.........`, fixed = T)
    ) %>% 
    dplyr::select(-Résultat) %>% 
    dplyr::rename(commune = `Canton.......District........Commune.........`) %>% 
    dplyr::rename(communeID = `Canton.......District........Commune........._code`) %>% 
    dplyr::rename(ballot = `Date.et.objet_code`) %>% 
    dplyr::mutate(ballot = as.numeric(ballot))

  # split date and ballot name
  xx <- as.character(dd$`Date.et.objet`)
  dd$date <- as.Date(gsub("(\\d{4}\\-\\d{2}\\-\\d{2}) ", "\\1", xx, perl = T), format = "%Y-%m-%d")
  dd$ballotName <- gsub("(\\d{4}\\-\\d{2}\\-\\d{2}) (.*$)", "\\2", xx, perl = T)
  rm(xx)
  
  # make it wide as a matrix
  # row as muni, col as ballot
  ddd <- dd %>% 
    dplyr::select(communeID, ballot, value) %>% 
    tidyr::spread(key = ballot, value = value)
  rownames(ddd) <- ddd$communeID
  ddd <- data.matrix( ddd %>% dplyr::select(-communeID))
  
  # for attributes 
  ballot2ballotName <-dd %>% 
    dplyr::select(ballot, ballotName) %>% 
    distinct() %>% deframe()
  
  ballot2date <- dd %>% 
    dplyr::select(ballot, date) %>% 
    distinct() %>% deframe()
  
  code2communeName <- dd %>% 
    dplyr::select(communeID, commune) %>% 
    mutate(commune = gsub("......", "", commune, fixed = T)) %>% 
    distinct() %>% deframe()    
  
  attr(ddd, "ballotName") <- ballot2ballotName[match(colnames(ddd), names(ballot2ballotName))] %>% unname()
  attr(ddd, "date") <- ballot2date[match(colnames(ddd), names(ballot2date))] %>% unname()

  attr(ddd, "communeName") <- code2communeName[match(rownames(ddd), code2communeName)]
  rownames(ddd) <- as.numeric(rownames(ddd))
  
  stopifnot(
    ncol(ddd) == length(attr(ddd, "ballotName")), 
    length(attr(ddd, "ballotName")) == length(attr(ddd, "date")),
    nrow(ddd) == length(attr(ddd, "communeName"))
  )

  save(ddd, file = out.path)
  cat("\n\n ------ \n Saved in:" , out.path, "\n\n")
}

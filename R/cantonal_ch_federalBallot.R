#' Federal ballot results by cantons
#'
#' Load all federal ballots resuts by cantons: \url{https://www.pxweb.bfs.admin.ch/Selection.aspx?px_language=fr&px_db=px-x-1703030000_100&px_tableid=px-x-1703030000_100\px-x-1703030000_100.px&px_type=PX}
#'
#' Run \code{processFederalBallotByCantons()} to generate a readable csv file for loadCantonsCHFederalBallot
#'
#' @rdname cantonal_ch_federalBallot
#' @name loadCantonsCHFederalBallot
#' @param file the name of the csv file processed by processFederalBallotByCantons to load
#' @return a matrix, rownames are cantons (2 letters name) and colnames are federal ballot IDs. Check the attributes ballotName and date
#' @export
#' @examples
#' \dontrun{
#' fBallot <- loadCantonsCHFederalBallot()
#' attr(fBallot, "ballotName")
#' cidx <- grep("naturalisation", attr(fBallot, "ballotName"), ignore.case = T)
#' attr(fBallot, "ballotName")[cidx]
#' attr(fBallot, "date")[cidx]
#' # get only naturalisation facilitée ballots
#' cidx <- match(c("3150", "4110", "5100", "5110"), colnames(fBallot))
#' ballot_nat <- fBallot[,cidx]
#' colnames(ballot_nat) <- attr(fBallot, "ballotName")[cidx]
#' ballot_nat <- ballot_nat %>% 
#'   data.frame() %>% 
#'   rownames_to_column(var = "iso2")
#' # get the cantonal IDs
#' ballot_nat <- left_join(ballot_nat, canton_CH %>% select(order, iso2)) %>% select(-iso2)
#¿
#' require(sf)
#' require(tidyverse)
#'
#' # get canton shapefiles as a sf data.frame
#' path.cantons <-  shp_path(2018, features = "cantons")
#' ca <- st_read(path.cantons, layer = "g2k18") %>%
#'   select(KTNR, KTNAME)
#'   
#' # Bind ballot data and make it long (duplicate for each ballot)
#' ca <- left_join(ca, ballot_nat, by=c("KTNR" = "order")) %>% 
#'   gather(ballot, percYes, -KTNR, -KTNAME, -geometry)
#'   
#' # Create breaks
#' brks <- seq(from = 0, to = 1, length.out = 11) * 100
#' ca <- ca %>% mutate(
#'   bins = cut(percYes, breaks = brks, right = F)
#' )
#' 
#' # plot
#' gp <- ggplot(ca) +
#'   geom_sf(aes(fill = bins), lwd = 0) +
#'   facet_wrap(~ ballot) + 
#'   theme_map()+ 
#'   scale_fill_brewer(palette = "BrBG" , drop = F)
#'   
#'  gp + ggtitle('Votes naturalisation')
#' }
loadCantonsCHFederalBallot <- function(file = "federalBallot_cantons.RData") {
  data.path <- dir(system.file("extdata", package="tamMap"), file, full.names = T)
  load(data.path)
  ddd
}

#' Process Portraits regionaux de la Suisse canton px file to be used by loadCantonsCHFederalBallot
#' This will download the px file from \url{https://www.bfs.admin.ch/bfs/en/home/statistics/politics/popular-votes.assetdetail.1363949.html}, 
#' process it and save it as a Rdata file in inst/extdata. 
#' Run it when new data are available. 
#  Warning of possible break if the OFS changes the data structure of the px file!!
#' @rdname cantonal_ch_federalBallot
#' @name processFederalBallotByCantons
#' @param url the URL to the px file with all federal ballots
#' @param output the output file name to be saved in the package inst/extdata
#' @return NULL
#' @import tidyr dplyr pxR stringr
#' @seealso \url{https://www.bfs.admin.ch/bfs/en/home/statistics/politics/popular-votes.assetdetail.1363949.html}
#' @export
#' @examples
#' \dontrun{
#' processPortraitsRegionauxCantons()
#' }
processFederalBallotByCantons <- function(
  url = 'https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1703030000_100',
  output = 'federalBallot_cantons.RData'
) {
  out.path <- paste0(getwd(), "/inst/extdata/", output)

  tmpdir <- tempdir()
  px.file <- download.file(url, paste0(tmpdir, "/", "federalBallot.px"))
  px.read <- read.px(filename = paste0(tmpdir, "/", "federalBallot.px"))

  data <- px.read$DATA[[1]]

  # get the French terms
  fr <- px.read$VALUES.fr.
  de <- px.read$VALUES

  # get French colnames
  colnames(data)[-ncol(data)] <- rev(names(fr))

  ## helper to translate PX file
  translate <- function(colname = 'Result.variable', data, fr, de) {
    # find which colname idx
    i <- which(names(fr) == colname)
    # split single string to a string vector
    translations <- unlist(strsplit(fr[[i]], '", ?"'))
    stopifnot(length(translations) == length(de[[i]]))

    # match each term of the data to the levels
    idx <- match(data[[colname]], de[[i]])
    stopifnot(all(!is.na(idx)))

    factor(translations[idx])
  }
  # apply translation
  for(coln in colnames(data)[-ncol(data)]) {
    data[,coln]<- translate(coln, data, fr, de)
  }
  # get the canton and ballot ID
  code <- px.read$CODES.fr.
  cantons <- structure(
    unlist(strsplit(code[['Canton']], '", ?"')),
    names = unlist(strsplit(fr[['Canton']], '", ?"'))
  )
  ballot <- structure(
    unlist(strsplit(code[['Date.et.objet']], '", ?"')),
    names = unlist(strsplit(fr[['Date.et.objet']], '", ?"'))
  )

  ## subset to take only %oui and not results over the whole Switzerland
  dd <- data %>% filter(Résultat == 'Oui en %', Canton != "Suisse") %>%
    select(-Résultat)

   # get canton 2 letters code
  dd$Canton <- cantons[match(dd$Canton, names(cantons))]
  # get ballot id
  dd$ballot <- as.numeric(ballot[match(dd$`Date.et.objet`, names(ballot))])
  # split date and ballot name
  xx <- as.character(dd$`Date.et.objet`)
  dd$date <- as.Date(gsub("(\\d{4}\\-\\d{2}\\-\\d{2}) .*", "\\1", xx, perl = T), format = "%Y-%m-%d")
  dd$ballotName <- gsub("(\\d{4}\\-\\d{2}\\-\\d{2}) (.*$)", "\\2", xx, perl = T)

  ddd <- dd %>% select(Canton, ballot, value) %>%
    tidyr::spread(key = ballot, value = value)
  rownames(ddd) <- ddd$Canton
  ddd <- data.matrix( ddd %>% select(-Canton))

  attr(ddd, "ballotName") <- dd[match(colnames(ddd), dd$ballot), 'ballotName']
  attr(ddd, "date") <- dd[match(colnames(ddd), dd$ballot), 'date']
  stopifnot(
    ncol(ddd) == length(attr(ddd, "ballotName")),
    length(attr(ddd, "ballotName")) == length(attr(ddd, "date"))
  )
  save(ddd, file = out.path)
  cat("\n\n ------ \n Saved in:" , out.path, "\n\n")
}


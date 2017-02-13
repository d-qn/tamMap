##' Federal ballot results by communes
##'
##' Load any federal ballots resuts by communes: https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1703030000_101
##' 
##' Run processFederalBallotByCommunes() to generate a readable csv file for loadCommunesCHFederalBallot
##'
##' @rdname communes_ch_federalBallot
##' @param file the name of the csv file processed by processFederalBallotByCommunes to load
##' @return a matrix, rownames are cantons (2 letters name) and colnames are federal ballot IDs. Check the attributes ballotName and date (same length as ncol) and communeName (same lenghth as nrow)
##' @export
##' @examples
##' \dontrun{
##' fBallot <- loadCommunesCHFederalBallot()
##' attr(fBallot, "ballotName")
##' # get only naturalisation facilitée ballots
##' cidx <- match(c("3150", "4110", "5100", "5110"), colnames(fBallot))
##' attr(fBallot, "ballotName")[cidx]
##' fBallot[,cidx]
##' # Plot maps
##' require(rgdal)
##' require(rgeos)
##' require(ggplot2)
##' require(maptools)
##' require(dplyr)
##' 
##' # get canton shapefiles as a data.frame
##' path.ch <- getPathShp('CH')
##' layers <-  ogrListLayers(path.ch)
##' mu <- readOGR(path.ch, layer = 'municipalities')
##' mu.df <- formatShp(mu) %>% select(long, lat, group, BFS_NUMMER)
##' r.idx <- match(mu.df$BFS_NUMMER, rownames(fBallot))
##' # duplicate commune data.frame for each ballot
##' df <- do.call(rbind, lapply(cidx, function(idx) {
##'   value <- fBallot[,idx]
##'   res <- mu.df
##'   res$value <- value[r.idx]
##'   res$ballot <- attr(fBallot, "ballotName")[idx]
##'   res$date <- attr(fBallot, "date")[idx]
##'   res
##' }))
##' # plot maps
##' brks <- seq(from = 0, to = 1, length.out = 11) * 100
##' df$bins <- cut(df$value, breaks = brks, right = F)
##' df$ballot <- factor(df$ballot, levels = attr(fBallot, "ballotName")[cidx])
##' ggplot(df, aes(x = long, y = lat, group = group)) + geom_polygon(size = 0, aes(fill = bins)) +
##' theme_minimal() + theme(legend.position = "bottom", panel.grid = element_blank(), 
##' axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
##' facet_wrap(~ ballot) + scale_fill_brewer(palette = "BrBG" , drop = F) + 
##' coord_quickmap(expand = F)
##' }
loadCommunesCHFederalBallot <- function(file = "federalBallot_communes.RData") {
  data.path <- dir(system.file("extdata", package="swiMap"), file, full.names = T)
  load(data.path)
  ddd
}

##' Process Portraits régionaux de la Suisse commune px file
##' This will download the px file from \url{https://www.bfs.admin.ch/bfs/en/home/statistics/politics/popular-votes.assetdetail.1363949.html}, process it and save it as a Rdata file
##' Necessary to run it before using loadCommunesCHFederalBallot
##' @rdname communes_ch_federalBallot
##' @param url the URL to the px file with all federal ballots
##' @param output the output file name to be saved in the package inst/extdata
##' @return NULL
##' @import tidyr dplyr pxR stringr
##' @seealso \url{https://www.bfs.admin.ch/bfs/en/home/statistics/politics/popular-votes.assetdetail.1363949.html}
##' @export
##' @examples
##' \dontrun{
##' processFederalBallotByCommunes()
##' }
processFederalBallotByCommunes <- function(
  url = 'https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1703030000_101', 
  output = 'federalBallot_communes.RData'  
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
    unlist(strsplit(code[['Canton.......District........Commune.........']], '", ?"')), 
    names = unlist(strsplit(fr[['Canton.......District........Commune.........']], '", ?"'))
  )
  ballot <- structure(
    unlist(strsplit(code[['Date.et.objet']], '", ?"')), 
    names = unlist(strsplit(fr[['Date.et.objet']], '", ?"'))   
  )
  
  ## subset to take only %oui and municipality results
  dd <- data %>% filter(Résultats == 'Oui en %', grepl("...... ", `Canton.......District........Commune.........`, fixed = T)) %>%
    select(-Résultats) %>% rename(commune = `Canton.......District........Commune.........`)
 
   # get commune 2 letters code
  dd$communeID <- cantons[match(dd$commune, names(cantons))]
  # get ballot id
  dd$ballot <- as.numeric(ballot[match(dd$`Date.et.objet`, names(ballot))])
  # split date and ballot name
  xx <- as.character(dd$`Date.et.objet`)
  dd$date <- as.Date(gsub("(\\d{2}\\.\\d{2}\\.\\d{4}) .*", "\\1", xx, perl = T), format = "%d.%m.%Y")
  dd$ballotName <- gsub("(\\d{2}\\.\\d{2}\\.\\d{4}) (.*$)", "\\2", xx, perl = T)
  
  ddd <- dd %>% select(communeID, ballot, value) %>% 
    tidyr::spread(key = ballot, value = value)
  rownames(ddd) <- ddd$communeID
  ddd <- data.matrix( ddd %>% select(-communeID))
  
  attr(ddd, "ballotName") <- dd[match(colnames(ddd), dd$ballot), 'ballotName']
  attr(ddd, "date") <- dd[match(colnames(ddd), dd$ballot), 'date']
  attr(ddd, "communeName") <- gsub("...... ", "", names(cantons)[match(rownames(ddd), cantons)], fixed = T)
  rownames(ddd) <- as.numeric(rownames(ddd))

  stopifnot(
    ncol(ddd) == length(attr(ddd, "ballotName")), 
    length(attr(ddd, "ballotName")) == length(attr(ddd, "date")),
    nrow(ddd) == length(attr(ddd, "communeName"))
  )
  
  save(ddd, file = out.path)
  cat("\n\n ------ \n Saved in:" , out.path, "\n\n")
}


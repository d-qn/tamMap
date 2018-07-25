##' Get the French translation from an OFS typical px data file
##' 
##' A highlevel wrapper that translates the column names and 
##' values from German to French. It can also fetch OFS' internal 
##' code identifiers (useful for mapping). It assumes the provided data 
##' follows a well defined structure and that it is in German 
##' 
##' @details BFS-STAT TAB px data cubes \url{https://www.pxweb.bfs.admin.ch/pxweb/fr/?rxid=e8ebe1b6-5756-49f0-8bfb-9eaa1cf758b2}
##' @param px.file a string, the path to an OFS px file
##' @param langout a string, the output language. Has to be one of fr, it, en
##' @param attachCode a vector of strings, the column names (in the output language) for which to add the code identifiers 
##' @import pxR
##' @return a data.frame 
##' @export
##' @examples 
##' \dontrun{
##' }

ofsPx_wrangle <- function(px.file, langout = 'fr', attachCode = "") {
  stopifnot(langout %in% c('fr', 'it', 'en'))
  
  px.read <- read.px(px.file)
  languagesAvailable <- str_extract_all(names(px.read), "\\.[:alpha:]{2}\\.$") %>% 
    unlist() %>% unique() %>% 
    str_replace_all("\\.", "")
  
  cat("\n", "Languages available in this px file :", languagesAvailable)  
  stopifnot(langout %in% languagesAvailable)
  
  pxdata <- px.read$DATA[[1]]
  # get initial and translations
  trpx <- px.read[[paste0("VALUES.", langout, ".")]]
  oripx <- px.read$VALUES
  
  # Overwrite column names with translation
  colnames(pxdata)[-ncol(pxdata)] <- rev(names(trpx))  
  
  # Translate the values, by overwriting each column (except the last numerical column)
  for(coln in colnames(pxdata)[-ncol(pxdata)]) {
    pxdata[,coln] <- px_translate(coln, pxdata, trpx, oripx)
  }
  
  # Attach code
  if(!is.null(attachCode)) {
    if(!all(attachCode %in% colnames(pxdata))) {
      stop("\n", "Not all column names to attach code are present. 
           Please provide only one of, ", colnames(pxdata))
    }
    for(coln in attachCode) {
      pxdata <- cbind(pxdata, px_code(coln, pxdata, px.read))
      colnames(pxdata)[ncol(pxdata)] <- str_c(coln, "_code")
    }
  }
  pxdata
}

px_translate <- function(colname = 'Result.variable', pxdata, trpx, oripx) {
  #find which column idx
  i <- which(names(trpx) == colname)
  #split single string to a string vector
  translations <- unlist(strsplit(trpx[[i]], '", ?"'))
  stopifnot(length(translations) == length(oripx[[i]]))
  # match each term of the pxdata to the levels
  idx <- match(pxdata[[colname]], oripx[[i]])
  stopifnot(all(!is.na(idx)))
  factor(translations[idx])
}

px_code <- function(colname = "", trad_df, px.read) {
  fr <- px.read$`VALUES.fr.`[[colname]] %>% 
    str_split('", ?"') %>% 
    unlist()
  code <- px.read$`CODES.fr`[[colname]] %>% 
    str_split('", ?"') %>% 
    unlist()
  stopifnot(length(fr) == length(code))
  
  idx <- match(trad_df[[colname]], fr)
  stopifnot(all(!is.na(idx)))
  code[idx]
}
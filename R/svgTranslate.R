
parseSVG <- function(input = NULL) {
  if(!file.exists(input)) stop (input, " cannot be found")
  if(!grepl("\\.svg$", input)) stop(input, " needs to be a svg file")
  xmlTreeParse(input, useInternalNodes = T)
}

parseTxt <- function(xmlTree) {
  # get all the tspan within text elements
  # I don't really understand the parsing, adapted from: http://www.omegahat.org/SVGAnnotation/SVGAnnotationPaper/SVGAnnotationPaper.html#bib:XPathXPointer
  getNodeSet(xmlTree, "//x:text/x:tspan", "x")
}

get_nonNumericNonEmptyString <- function(textNodes) {
  # Get the index of text which are not empty and not only numeric
  !sapply(textNodes, xmlValue) %in% c("", " ", "\n") & !grepl("^\\d+$", sapply(textNodes, xmlValue)) & sapply(textNodes, xmlSize) == 1
}

get_NestedNonNumericNonEmptyString <- function(textNodes) {
  # Get the index of text which are not empty and not only numeric
  !sapply(textNodes, xmlValue) %in% c("", " ", "\n") & !grepl("^\\d+$", sapply(textNodes, xmlValue)) & sapply(textNodes, xmlSize) == 1
}

##' Collection of functions to get and replace text in a svg file
##' 
##' createTextToTranslate: get the path to a svg file and return a csv file with all text elements (which are not pure numerics)
##' 
##' @name svgTranslate
##' @param input path to a svg file
##' @param ouputFileAppend suffix to append at the end of the created SVG file, if NULL returns a vector of text elements
##' @return the output file path or a character vector if ouputFileAppend is NULL
##' @examples
##' require(XML)
##' test <- createTextToTranslate(system.file("extdata", "slopegraph_test.svg", package="swiMap"), ouputFileAppend = NULL)
##' test
##' @export
createTextToTranslate <- function(input, ouputFileAppend = "_text.csv") {
  textNodes <- parseTxt(parseSVG(input))
  idx <- get_nonNumericNonEmptyString(textNodes)
  text.ori <- sapply(textNodes[idx], xmlValue)
  
  # Get the nested nodes with text  ## not sure when this happens...
  idx.nest <- which(sapply(textNodes, xmlSize) > 1)
  if(!identical(idx.nest, integer(0))) {
    text.ori <- c(text.ori, sapply(xmlChildren(textNodes[[idx.nest]]), xmlValue))    
  }

  # Get the value of text elements
  if(!is.null(ouputFileAppend)) {
    output <- basename(gsub("\\.svg", ouputFileAppend, input))
    write.csv(unique(text.ori), file = output, row.names = FALSE)
    output
  } else {
    text.ori
  }
}

##' createTranslatedSVG: get a svg file and its text translations, output the translated svgs
##' 
##' @name svgTranslate
##' @param input path to the original svg file to translate
##' @param tradFile path to the csv translations
##' @param inDirectory path to a directory where all the output files are saved 
##' @param overwrite a boolean, output svg files to be overwritten?
##' @export
createTranslatedSVG <- function(input = NULL, tradFile = NULL, inDirectory = "trad", overwrite = TRUE) {
  if(!file.exists(input)) stop (input, " cannot be found")
  if(!grepl("\\.svg$", input)) stop(input, " needs to be a svg file")
  if(!file.exists(tradFile)) stop (tradFile, " cannot be found")
  if(!grepl("\\.csv$", tradFile)) stop(tradFile, " needs to be a csv file")
  
  if(inDirectory != "") {
    if(!file.exists(inDirectory)) dir.create(inDirectory)
  }
  
  # Parse the XML
  xmlTree <- parseSVG(input)
  
  # Get the text from the input svg to translate
  textNodes <- parseTxt(xmlTree)
  
  idx <- get_nonNumericNonEmptyString(textNodes)
  text.ori <- sapply(textNodes[idx], xmlValue)
  
  # Get the nested nodes with text  ## not sure when this happens...
  idx.nest <- which(sapply(textNodes, xmlSize) > 1)
  if(!identical(idx.nest, integer(0))) {
    text.ori <- c(text.ori, sapply(xmlChildren(textNodes[[idx.nest]]), xmlValue))    
  }
  
  # Get the translations
  trad <- read.csv(tradFile, header = TRUE, stringsAsFactors = FALSE)
  
  if(ncol(trad) <= 1) stop("Translation file needs at least 2 columns")
  
  # Match the translation first column to the original text
  ori2trad <- match(text.ori, trad[,1])
  if(any(is.na(ori2trad))) {
    cat("\n", "These text elements are not present in trad file!!", "\n", text.ori[is.na(ori2trad)])
    stop("Translation file do not match the text from the svg!")
  }
  
  # Loop trough the different columns and create svg files
  for(i in 2:ncol(trad)) {
    lang <- colnames(trad)[i]
    
    for(j in 1:length(textNodes[idx])) {
      xmlValue(textNodes[idx][[j]]) <- trad[ori2trad[j], lang]
    }
    # if there are nested node text
    if(length(idx.nest) > 0) {
      for(k in 1:length(idx.nest)) {
        sapply(xmlChildren(textNodes[[idx.nest[k]]]), function(node) {
          xmlValue(node) <- trad[match(xmlValue(node), trad[,1]),lang]
        })
      }
    }
  
    outfile <- paste(gsub("(^.*)\\.svg$", "\\1", input), "_", lang, ".svg", sep = "")
    if(inDirectory != "") outfile <- paste(inDirectory, "/", outfile, sep = "")
    if(file.exists(outfile) && !overwrite) {
      stop(outfile, " already exists! Delete it or use 'overwrite = TRUE")
    } else {
      saveXML(xmlTree, outfile)
      cat("\n", outfile, " saved!", "\n")
    }
  }
}
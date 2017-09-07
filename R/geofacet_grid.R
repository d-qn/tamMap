##' grid to feed ggplot2 geofacet
##'
##' Some grid (data.frame definitions) to be used with the package geofacet
##' 
##' @name geofacet_grid
##' @import ggplot2, geofacet
##' @seealso https://hafen.github.io/geofacet/
##' @examples
##' dontrun{
##' regions <- unique(countrycode::countrycode(countryTranslations[,1], "iso2c", "region"))
##' write.csv(data.frame(code))
##' }
##' @export
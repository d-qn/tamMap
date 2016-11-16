##' Geographical map with shapefiles
##'
##' Helper functions to draw geographical map based on shapefiles
##'
##' @name mapShapefiles
##' @param geo a character, either 'CH' or 'world'
##' @param year a numeric, year for data to use (default 2015)
##' @return a character, the path to the shapefiles folder
##' @export
##' @examples
##' require(rgdal)
##' require(rgeos)
##' require(ggplot2)
##' require(maptools)
##' path <- getPathShp('world')
##' layers <-  ogrListLayers(path)
##' # read shapefiles for given layer
##' world <- readOGR(path, layer = layers[1])
##' world.df <- formatShp(world)
##' # plot world map
##' wm <- ggplot(world.df, aes(x = long, y = lat, group = group)) +
##'   geom_polygon(size = 0.01, aes(fill = FIPS)) +
##'   theme_minimal() +
##'   theme(
##'     legend.position = "none",
##'     panel.grid = element_blank(),
##'     axis.ticks = element_blank(),
##'     axis.title = element_blank(),
##'     axis.text = element_blank()
##'   )
##'  wm
##'  # With nice orthographic projection
##'  wm + coord_map("ortho")
##' # Same with swiss commune map (slow!)
##' \dontrun{
##' path.ch <- getPathShp('CH', year = 2015)
##' ch <- readOGR(path.ch, layer = 'municipalities-without-lakes')
##' ch.df <- formatShp(ch)
##' # plot swiss commune map
##' ch.communes <- ggplot(ch.df, aes(x = long, y = lat, group = group)) + geom_polygon(size = 0, aes(fill = GEMNAME)) +
##' theme_minimal() + theme(legend.position = "none", panel.grid = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank())
##' ch.communes
##' # add the lake
##' lake.df <- formatShp(readOGR(path.ch, layer = "lakes"))
##' ch.communes + geom_polygon(data = lake.df, fill = "blue", colour = "blue")
##' # get all the id and name of communes from the map
##' bfsnName <- ch.df[!duplicated(ch.df$BFSNR),c('BFSNR', 'GEMNAME')]
##' bfsnName[order(bfsnName[,1]),]
##' }
getPathShp <- function(geo = "CH", year = 2016) {
  # check
  if(!geo %in% c('CH', 'world')) {
    stop ("geo has to be one of 'CH' or 'world'!")
  }
  if(geo == 'CH') {
    years.available <- as.numeric(list.files(system.file("extdata/shp/CH", package="swiMap")))
    if(!year %in% years.available) {
      stop("\nThe year provided ", year, " does not have any data/folder available!\n")
    }

    if(year == 2016) {
      path <- 'CH/2016'
    } else if(year == 2015) {
      path <- 'CH/2015/ch'
    } else {
      path <- 'CH/2014/ch'
    }
  } else {
    path <- 'TM_WORLD_BORDERS_SIMPL-0'
  }

  system.file(paste("extdata/shp/", path, sep = ""), package="swiMap")
}
##' @rdname mapShapefiles
##' @param shp shapefile contents loaded ojbect, as returned by \code{readOGR}
##' @import ggplot2
##' @seealso https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles
##' @return a dataframe object
##' @export
formatShp <- function(shp) {
  shp@data$id <- rownames(shp@data)
  shp.points <- fortify(shp, region = "id")
  shp.df <- plyr::join(shp.points, shp@data, by = "id")
  shp.df
}





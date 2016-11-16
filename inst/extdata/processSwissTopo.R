require(rgdal)
require(rgeos)
require(maptools)
library(ggplot2)
#library(swiMap)


maps <- c('BEZIRKSGEBIET', 'HOHEITSGEBIET', 'KANTONSGEBIET', 'LANDESGEBIET')
names(maps) <- c('districts', 'municipalities', 'cantons', 'country')



path.shp <- ("/Users/nguyendu/Downloads/BOUNDARIES_2016/DATEN/swissBOUNDARIES3D/SHAPEFILE_LV03_LN02/")
output.path <- "/Users/nguyendu/swissinfo/_helpers/swiMap/inst/extdata/shp/CH/2016"


layers <- ogrListLayers(path.shp)
layers <- layers[grepl("GEBIET", layers)]

sapply(1:length(layers), function(i) {
  shp <- readOGR(path.shp, layer = layers[i])

  idx <- match(gsub("swissBOUNDARIES3D.*_", "", layers[i]), maps)
  # reproject coordintes in the standard projection: http://gis.stackexchange.com/questions/45263/converting-geographic-coordinate-system-in-r
  shp <- spTransform(shp, CRS("+init=epsg:4326"))

  writeOGR(obj=shp, dsn=output.path, layer=names(maps)[idx], driver="ESRI Shapefile") #

  shp.df <- formatShp(shp)
  wm <- ggplot(shp.df, aes(x = long, y = lat, group = group)) +
    geom_polygon(size = 0.01, aes(fill = id)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank()
    ) + ggtitle(paste0(layers[i], " - ", maps[idx]))
  print(wm)
})

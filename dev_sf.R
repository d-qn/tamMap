require(sf)
require(dplyr)
require(tamMap)
require(ggplot2)

data <- loadCommunesCHgeographicalLevels() %>% 
  select(ofsID, name, Canton, `Typologie urbainrural 2012`)

## Download latest 2018 geographical data from OFS, 
## warning large dataset to be downloaded!!

# create a temporary directory
td <- tempdir()
# create the placeholder file
tf <- tempfile(tmpdir=td, fileext=".zip")

# download into the placeholder file
download.file("https://www.bfs.admin.ch/bfsstatic/dam/assets/5247306/master", tf)
# unzip
unzip(tf, exdir=td, overwrite = TRUE)

shp18 <- file.path(td, 'ggg_2018-LV03', 'shp', 'g1g18.shp')
lakes18 <- file.path(td, 'ggg_2018-LV03', 'shp', 'g1s18.shp')
cantons18 <- file.path(td, 'ggg_2018-LV03', 'shp', 'g1k18.shp')
country18 <- file.path(td, 'ggg_2018-LV03', 'shp', 'g1l18.shp')
stopifnot(file.exists(shp18), file.exists(lakes18), file.exists(cantons18))

# GET LOCAL SHP FILES
# get commune shapefiles as a sf data.frame
# path.muni.geo <-  dir(system.file("extdata/shp/CH/2016", package="tamMap"), "municipalities.shp", full.names = T)
# # get lakes
# path.lakes.geo <- dir(system.file("extdata/shp/CH/2016", package="tamMap"), "municipalities.shp", full.names = T)
# # get cantons

# list layer
# st_layers(shp18)
# st_layers(lakes18)
# st_layers(cantons18)
muni <- st_read(shp18, layer = "g1g18") %>%
  select(GMDNR, GMDNAME)
cantons <- st_read(cantons18, layer = 'g1k18') %>% 
  select(KTNAME)
country <- st_read(country18, layer = 'g1l18')
lakes <- st_read(lakes18, layer = 'g1s18') %>% 
  select(SEENAME)
# delete temp folder
unlink(td, recursive = TRUE)

## Theme map
theme_map <- theme(
  legend.position = "bottom", 
  panel.background = element_rect(fill = "transparent"),
  panel.grid = element_line(colour = 'transparent'),
  axis.ticks = element_blank(), axis.title = element_blank(),
  axis.text = element_blank()
)

## JOIN
munid <- left_join(muni, data, by= c('GMDNR' = 'ofsID')) %>% 
  mutate(`Typologie urbainrural 2012` = as.factor(`Typologie urbainrural 2012`))

munid <- munid %>% st_simplify(preserveTopology = T, dTolerance = 1)
cantons <- cantons %>% st_simplify(preserveTopology = T, dTolerance = 1)
lakes <- lakes %>%  st_simplify(preserveTopology = T, dTolerance = 1)

gp <- ggplot() +
  geom_sf(data = munid, aes(fill = `Typologie urbainrural 2012`), lwd = 0.05, colour = "#0d0d0d") +
  geom_sf(data = cantons, lwd = 0.15, colour = "#333333", fill = NA) +
  geom_sf(data = country, lwd = 0.25, colour = "#000d1a", fill = NA) +
  geom_sf(data = lakes, lwd = 0, fill = "#0066cc") +
  theme_map
gp + coord_sf(datum = NA, expand = F) 
# hack to remove grid/graticule lines: https://github.com/tidyverse/ggplot2/issues/2071


### Functions for inlet maps

geo_subset <- munid %>% filter(Canton == 25)

## returns a named list with centre, radius to encompass the geometries provided in a circle
encircle_coord <- function(geo_subset) {
  stopifnot(any(class(geo_subset) %in% c('sf', 'sfc')))
  
  pol <- geo_subset %>%
    st_bbox() %>% 
    st_as_sfc() 
  
  radius <- pol %>%  
    st_cast("POINT") %>% 
    st_distance(st_centroid(pol)) %>% 
    max() %>% 
    as.numeric()
  
  list(centre = st_centroid(pol), radius = radius)
}

# list(
#   circle = st_buffer(st_centroid(pol), radius) %>% st_sf(),
#   line = c(st_centroid(pol) - radius, st_centroid(pol)  + radius) %>% st_cast("POINT") 
# )
# inlet_zoomInBg <- inletMap_zoomInBg(geo_subset)

# shiftM <- c(-13000, 120000)
# scaleF <- 3
inletMap_shiftScale <- function(geo_subset, shiftM = c(-9000, 150000), scaleF = 3) {
  stopifnot(any(class(geo_subset) %in% c('sf', 'sfc')))
  stopifnot(is.list(layers))
  stopifnot(length(shiftM) == 2, is.numeric(shiftM))
  stopifnot(is.numeric(scaleF))
  
  
  # shift
  geo_shift <- st_set_geometry(geo_subset, st_geometry(geo_subset) + shiftM)
  geo_shift_sf <- st_geometry(geo_shift)
  geo_shift_sf <- st_set_crs(geo_shift_sf, st_crs(gvad))
  
  # scale up
  geo_shift_scaled_sf <- geo_shift_sf * 3
  geo_shift_scaled_sf <- st_set_crs(geo_shift_scaled_sf, st_crs(gvad))
  
  # shift back
  #  get the centroids, to get the shifting coordinates
  geo_shift_scaled_sf.centroid <- st_centroid(st_combine(geo_shift_scaled_sf))
  geo_shift_sf.centroid <- st_centroid(st_combine(geo_shift_sf))
  scaling_shift <- c(geo_shift_scaled_sf.centroid[[1]][1] - geo_shift_sf.centroid[[1]][1],
                     geo_shift_scaled_sf.centroid[[1]][2] - geo_shift_sf.centroid[[1]][2])
  
  inlet <- geo_shift
  inlet <- st_set_geometry(gvad_inlet, geo_shift_scaled_sf - scaling_shift)
  inlet <- st_set_crs(gvad_inlet, st_crs(gvad))
  
  inlet
}

## Create the sf spatial ojbects for the 2 circles zoom in region and inlet
inlet_zoomInBg_coords <- encircle_coord(geo_subset)
inlet_zoomInBg <- st_buffer(inlet_zoomInBg_coords$centre, inlet_zoomInBg_coords$radius) %>% st_sf()

inletMap <- inletMap_shiftScale(geo_subset)
inletMap_encircled <- encircle_coord(inletMap)
inletMap_cicrleBg <- st_buffer(inletMap_encircled$centre, inletMap_encircled$radius) %>% st_sf()

## create the polygon 
inlet_bgCone <- c(inlet_zoomInBg_coords$centre - c(inlet_zoomInBg_coords$radius, 0), 
  inlet_zoomInBg_coords$centre + c(inlet_zoomInBg_coords$radius, 0), 
  inletMap_encircled$centre + c(inletMap_encircled$radius, 0),
  inletMap_encircled$centre - c(inletMap_encircled$radius,0)
) %>% st_set_crs(st_crs(inlet_zoomInBg_coords$centre)) %>% 
  st_cast("POINT") %>% 
  st_combine() %>% 
  st_cast("POLYGON") %>% 
  st_sf() %>% 
  # crop
  st_difference(inletMap_cicrleBg) %>%
  st_difference(inlet_zoomInBg)


ggplot() +
  geom_sf(data = inlet_bgCone, fill = "lightgrey", lwd = 0, alpha = 0.2) +  
  geom_sf(data = inlet_zoomInBg, fill = "lightgrey", lwd = 0, alpha = 0.5) +
  geom_sf(data = inletMap_cicrleBg, fill = "lightgrey", lwd = 0, alpha = 0.5) +
  geom_sf(data = munid, aes(fill = `Typologie urbainrural 2012`), lwd = 0.05, colour = "white") +
  geom_sf(data = cantons, lwd = 0.15, colour = "#333333", fill = NA) +
  geom_sf(data = country, lwd = 0.25, colour = "#000d1a", fill = NA) +
  geom_sf(data = lakes, lwd = 0, fill = "#0066cc") +
  geom_sf(data = inletMap, aes(fill = `Typologie urbainrural 2012`), lwd = 0.2, colour = "white") + 
  theme_map + 
  coord_sf(datum = NA, expand = F) 



















### INLET ###

## create a zoom map for Geneva, making a vertical shift
gvad <- munid %>% filter(Canton == 25)
###### With rotating
gvad_shift <- st_set_geometry(gvad, st_geometry(gvad) + c(-13000, 120000))
gvad_shift_sf <- st_geometry(gvad_shift)
gvad_shift_sf <- st_set_crs(gvad_shift_sf, st_crs(gvad))

# scale up
gvad_shift_scaled_sf <- gvad_shift_sf * 3
gvad_shift_scaled_sf <- st_set_crs(gvad_shift_scaled_sf, st_crs(gvad))

# shift back
#   get the centroids, to get the shifting coordinates
gvad_shift_scaled_sf.centroid <- st_centroid(st_combine(gvad_shift_scaled_sf))
gvad_shift_sf.centroid <- st_centroid(st_combine(gvad_shift_sf))
scaling_shift <- c(gvad_shift_scaled_sf.centroid[[1]][1] - gvad_shift_sf.centroid[[1]][1],
  gvad_shift_scaled_sf.centroid[[1]][2] - gvad_shift_sf.centroid[[1]][2])

gvad_inlet <- gvad_shift
gvad_inlet <- st_set_geometry(gvad_inlet, gvad_shift_scaled_sf - scaling_shift)
gvad_inlet <- st_set_crs(gvad_inlet, st_crs(gvad))

### add round grey shading
# https://walkerke.github.io/2016/06/reproducing-the-washington-post-housing-price-maps-with-r-and-ggplot2/
inlet_bg.centr <- gvad_inlet %>% st_bbox() %>% st_as_sfc() %>% st_centroid()
# compute max distance

# compute distances 
# https://stackoverflow.com/questions/48447680/how-to-compute-the-greatest-distance-between-a-centroid-and-the-edge-of-the-poly?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
pol <- gvad_inlet %>%
  st_bbox() %>% 
  st_as_sfc() 
radius <- pol %>%  
  st_cast("POINT") %>% 
  st_distance(st_centroid(pol)) %>% 
  max() %>% 
  as.numeric()
#plot(st_buffer(inlet_bg.centr, radius))

inlet.bg <- st_sf(data.frame(bonker = 1, geom = st_buffer(inlet_bg.centr, radius)))
list(centre = st_centroid(pol), radius = radius)

# list(
#   circle = st_buffer(st_centroid(pol), radius) %>% st_sf(),
#   line = c(st_centroid(pol) - radius, st_centroid(pol)  + radius) %>% st_cast("POINT")
# )
inlet_zoomInBg_coords <- encircle_coord(geo_subset)
inlet_zoomInBg <- st_buffer(inlet_zoomInBg_coords$centre, inlet_zoomInBg_coords$radius) %>% st_sf()



ggplot() +
  geom_sf(data = inlet_zoomInBg, fill = "lightgrey", lwd = 0, alpha = 0.6) +  
  geom_sf(data = inlet.bg, fill = "lightgrey", lwd = 0, alpha = 0.5) + 
  geom_sf(data = munid, aes(fill = `Typologie urbainrural 2012`), lwd = 0.05, colour = "white") +
  geom_sf(data = cantons, lwd = 0.15, colour = "#333333", fill = NA) +
  geom_sf(data = country, lwd = 0.25, colour = "#000d1a", fill = NA) +
  geom_sf(data = lakes, lwd = 0, fill = "#0066cc") +
  geom_sf(data = gvad_inlet, aes(fill = `Typologie urbainrural 2012`), lwd = 0.2) + 
  theme_map() + 
  coord_sf(datum = NA, expand = F) 


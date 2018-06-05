##' Convenience functions to create an inlet zoom map
##' 
##' The steps are:
##'   1. Create a sf ojbect with shifted geospatial coordinates and/or scaled-up/down
##'   2. Create a sf ojbect that encirle 1, totally unnecessary but aesthically pleasing. 
##'   3. Create a sf ojbect that encircle the original region that was shifted & scaled-up
##'   4. Create a sf ojbect that connects both circles to convey it a zoomed inlet map
##' \code{encircle_coord} finds the centre and radius to completely encompass the provided geospatial object
##' \code{encircle_geo} takes a named list with centre and radius and return a simple feature object of the circle
##' 
##' @rdname inlet_helpers
##' @param geo a simple feature data.frame 
##' @param shiftM a numeric of length 2, the distances in meters to shift in longitude and latitude respectively
##' @param scaleF a numeric of length 1, the scaling factor
##' @return \code{shiftScale_geo} a simple feature data.frame  
##' @import sf
##' @export
##' @examples 
##' \dontrun{
##' require(sf)
##' require(tidyverse)
##' 
##' shp_ch_paths_2018 <- shp_path(2018)
##' # loop and load the geo data in a named list
##' shp_ch_geodata <- shp_ch_paths_2018 %>% map(function(x) {
##'   layerName <- st_layers(x)
##'   st_read(x, layer = layerName$name) %>% 
##'   select(ends_with("NR"), ends_with("NAME"))
##' })
##' 
##' # shift and scale up canton Geneva for the inlet map
##' geo_subset <- shp_ch_geodata$municipalities %>% filter(KTNR == 25)
##' inlet_geo <- shiftScale_geo(geo = geo_subset, scaleF = 4.4)
##' 
##' # Create circles sf to encompass the inlet map and its original location
##' geo_subset_coord <- encircle_coord(geo_subset)
##' inlet_coord <- encircle_coord(inlet_geo)
##' 
##' cone_sfc <- cone_geo(geo_subset_coord, inlet_coord)
##' circle_zoom_sfc <- encircle_geo(geo_subset_coord)
##' circle_inlet_sfc <- encircle_geo(inlet_coord)
##' 
##' # plot
##' gp <- ggplot() +
##'   geom_sf(data = cone_sfc, fill = "lightgrey", lwd = 0, alpha = 0.2) +  
##'   geom_sf(data = circle_zoom_sfc, fill = "lightgrey", lwd = 0, alpha = 0.5) +
##'   geom_sf(data = circle_inlet_sfc, fill = "lightgrey", lwd = 0, alpha = 0.5) +
##'   geom_sf(data = shp_ch_geodata$municipalities, aes(fill = GMDNR), 
##'     lwd = 0.05, colour = "#0d0d0d") +
##'   geom_sf(data = inlet_geo, aes(fill = GMDNR), lwd = 0.1, colour = "#0d0d0d") +
##'   geom_sf(data = shp_ch_geodata$cantons, lwd = 0.15, colour = "#333333", fill = NA) +
##'   geom_sf(data = shp_ch_geodata$country, lwd = 0.25, colour = "#000d1a", fill = NA) +
##'   geom_sf(data = shp_ch_geodata$lakes, lwd = 0, fill = "#0066cc")
##'
##'   gp + 
##'     theme_map() +
##'     scale_fill_viridis_c()  +
##'     coord_sf(datum = NA, expand = F)
##' }
shiftScale_geo <- function(geo, shiftM = c(-9000, 150000), scaleF = 3) {
  stopifnot(any(class(geo) %in% c('sf', 'sfc')))
  stopifnot(length(shiftM) == 2, is.numeric(shiftM))
  stopifnot(length(scaleF) ==1, is.numeric(scaleF))
  
  # shift
  geo_shift <- st_set_geometry(geo, st_geometry(geo) + shiftM)
  geo_shift_sf <- st_geometry(geo_shift)
  geo_shift_sf <- st_set_crs(geo_shift_sf, st_crs(geo))
  
  # scale up
  geo_shift_scaled_sf <- geo_shift_sf * scaleF
  geo_shift_scaled_sf <- st_set_crs(geo_shift_scaled_sf, st_crs(geo))
  
  # shift back
  #  get the centroids, to get the shifting coordinates
  geo_shift_scaled_sf.centroid <- st_centroid(st_combine(geo_shift_scaled_sf))
  geo_shift_sf.centroid <- st_centroid(st_combine(geo_shift_sf))
  scaling_shift <- c(geo_shift_scaled_sf.centroid[[1]][1] - geo_shift_sf.centroid[[1]][1],
                     geo_shift_scaled_sf.centroid[[1]][2] - geo_shift_sf.centroid[[1]][2])
  
  inlet <- geo_shift
  inlet <- st_set_geometry(inlet, geo_shift_scaled_sf - scaling_shift)
  st_set_crs(inlet, st_crs(geo))
} 
##' @rdname inlet_helpers
##' @return \code{encircle_coord} a named list with a simple feature point (centre) and the radius in meters
##' @import sf
##' @export
encircle_coord <- function(geo) {
  stopifnot(any(class(geo) %in% c('sf', 'sfc')))
  
  pol <- geo %>%
    st_bbox() %>% 
    st_as_sfc() %>% 
    st_set_crs(st_crs(geo))
  
  radius <- pol %>%  
    st_cast("POINT") %>% 
    st_distance(st_centroid(pol)) %>% 
    max() %>% 
    as.numeric()
  
  list(centre = st_centroid(pol), radius = radius)  
}
##' @rdname inlet_helpers
##' @param centreRadius,centreRadius_ori,centreRadius_inlet a named list with centre (a sfc_POINT) and radius
##' @return \code{encircle_geo} a simple feature object
##' @import sf
##' @export
encircle_geo <- function(centreRadius) {
  stopifnot(is.list(centreRadius), names(centreRadius) == c('centre', 'radius'), "sfc" %in% class(centreRadius[[1]]))
  
  st_buffer(centreRadius$centre, centreRadius$radius) %>% 
    st_sf()
}
##' @rdname inlet_helpers
##' @return \code{cone_geo} a simple feature object
##' @import sf
##' @export
cone_geo <- function(centreRadius_ori, centreRadius_inlet) {
  c(centreRadius_ori$centre - c(centreRadius_ori$radius, 0), 
    centreRadius_ori$centre + c(centreRadius_ori$radius, 0), 
    centreRadius_inlet$centre + c(centreRadius_inlet$radius, 0),
    centreRadius_inlet$centre - c(centreRadius_inlet$radius, 0)
  ) %>% 
    st_set_crs(st_crs(centreRadius_ori$centre)) %>% 
    st_cast("POINT") %>% 
    st_combine() %>% 
    st_cast("POLYGON") %>% 
    st_sf() %>% 
    # crop
    st_difference(encircle_geo(centreRadius_ori)) %>%
    st_difference(encircle_geo(centreRadius_inlet))
}

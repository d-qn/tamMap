library(sf)
library(tilemaps)
library(tamMap)
library(tidyverse)


load("tms_muni_ch_2021_0_0.1_0.1_0.5_0_10_0_0_0_0.5.RData")
ch_2021 <- st_read(shp_path(2021)[[1]])

plot_many_maps(tms_2021$map, labels = "")

tmk <- tms_2021$map %>% 
  map(function(tt) {
    tt <- tt %>% 
      as_tibble() %>% 
      mutate(
        GMDNR = ch_2021$GMDNR
      ) %>% 
      st_as_sf()
    
    tt <- left_join(
      tt, 
      ch_2021 %>% 
        st_set_geometry(NULL)
    )
    tt %>% 
      group_by(KTNR) %>% 
      summarise(geometry = st_union(geometry)) 
  })


tmk %>% 
  map(function(tt) {
    ggplot(tt) +
      geom_sf(lwd = 0.1, aes(fill = KTNR)) +
      theme_map() +
      scale_fill_viridis_c() 
  })

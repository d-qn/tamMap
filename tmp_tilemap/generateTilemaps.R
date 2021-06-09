
library(sf)
library(tilemaps)
library(tamMap)
library(tidyverse)



ch_2021 <- st_read(shp_path(2021)[[1]])

#https://kaerosen.github.io/tilemaps/articles/tilemaps.html#creating-many-tile-maps-1

start_time <- Sys.time()
tms_2021 <- many_maps(ch_2021$geometry, ch_2021$GMDNR,
                      square = FALSE, flat_topped = TRUE,
                     prop = c(0, 0.1), interpolate = c(0.5, 1),
                     smoothness = c(0, 20), shift = list(c(0,0), c(0,0.5)))
Sys.time() - start_time
# 7h...
# Preferred version 
prop <- c(0, 0.1) # no influence
interpolate <- 0.5
smoothness <- 0
shift <- "0     0, 0      27.89270
#15   31.18504  0.1         0.5          0 0.0, 0.5"

save(tms_2021, file = "tms_muni_ch_2021.RData")

plot_many_maps(tms_2021$map, labels = "")

#ToDO plot each cantonal map

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
    tmlj %>% 
      group_by(KTNR) %>% 
      summarise(geometry = st_union(geometry)) 
  })


tmk[[12]] %>% 
  ggplot() +
  geom_sf(lwd = 0.1, aes(fill = KTNR))



# export map 16
tm <- tms_2021$map[[14]] %>% 
  as_tibble() %>% 
  mutate(
    GMDNR = ch_2021$GMDNR
    ) %>% 
  st_as_sf()

tm <- left_join(
  tm, 
  ch_2021 %>% 
    st_set_geometry(NULL) %>% 
    select(GMDNR, GMDNAME)) 

st_transform(tm, 4326) %>% 
  st_write("tm_muni_ch2021.geojson")

tmlj <- left_join(
  tm, 
  ch_2021 %>% 
    st_set_geometry(NULL)
)

library(ggplot2)
tmlj %>% 
  ggplot() +
  geom_sf(lwd = 0.1, aes(fill = KTNR))

tmk <- tmlj %>% 
  group_by(KTNR) %>% 
  summarise(geometry = st_union(geometry)) 


tmk %>% 
  ggplot() +
  geom_sf(lwd = 0.1, aes(fill = KTNR))

tms_2021 %>% plot()colnames()

tms_2021$map[[13]] %>% plot()


tm_2021 <- generate_map(ch_2021$geometry, square = FALSE, flat_topped = TRUE)

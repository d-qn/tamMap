# On R studio cloud
#  install.packages(c("remotes", "tidyverse", "sf"))
#  remotes::install_github("d-qn/tamMap") 
#  install.packages("tilemaps")

library(sf)
library(tilemaps)
library(tamMap)
library(tidyverse)

ch_2021 <- st_read(shp_path(2021)[[1]])

#https://kaerosen.github.io/tilemaps/articles/tilemaps.html#creating-many-tile-maps-1

params <- list(prop = c(0, 0.1),
               interpolate = c(0.1, 0.5),
               smoothness = c(0, 10),
               shift = list(c(0,0), c(0,0.5))
)

start_time <- Sys.time()
tms_2021 <- many_maps(ch_2021$geometry, ch_2021$GMDNR,
                      square = FALSE, flat_topped = TRUE,
                      prop = params$prop, interpolate = params$interpolate,
                      smoothness = params$smoothness, shift = params$shift)
Sys.time() - start_time
# 7h...
# Preferred version 
# prop <- c(0, 0.1) # no influence
# interpolate <- 0.5
# smoothness <- 0
# shift <- "0     0, 0      27.89270
#15   31.18504  0.1         0.5          0 0.0, 0.5"

save(tms_2021, params, file = str_c(
  "tms_muni_ch_2021_", 
  unlist(params) %>% str_c(collapse = "_"),
  ".RData"))

plot_many_maps(tms_2021$map, labels = "")

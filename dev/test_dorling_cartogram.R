library(sf)
library(tilemaps)
library(tamMap)
library(tidyverse)
library(cartogram)

ch_2021 <- st_read(shp_path(2021)[[1]])
munip <- loadCommunesCHportraits()


pop <- tibble(
  GMDNR = munip %>% rownames() %>% as.numeric ,
  pop = munip[,1] %>% as.numeric
)

ch_2021 <- left_join(
  ch_2021 %>% select(GMDNR),
  pop
)

ch_cartoc <- cartogram_cont(ch_2021, "pop", itermax = 5)
ch_cartod <- cartogram_dorling(ch_2021, "pop")

plot(ch_cartoc)
plot(ch_cartod)

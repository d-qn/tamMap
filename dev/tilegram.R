require(sf)
require(tidyverse)
library(tamMap)

tmc <- tilemap_ch()

tilegram_ch <- function(
  df = loadCantonsCHportraits()  %>% 
    select(`Habitants en milliers`, UDC) %>% 
    rownames_to_column("Name"),
  size_var = 'Habitants en milliers',
  col_var = 'UDC',
  canton_col = 'Name'
) {
  if(!is.data.frame(df)) stop("df is not a data.frame!")
  if(!size_var %in% colnames(df)) {
    stop(size_var, " is not present as a column in df!")
  }
  if(!col_var %in% colnames(df)) {
    stop(col_var, " is not present as a column in df!")
  }
  
  df <- left_join(
    tilemap_ch(),
    df
  ) %>% 
    rename(pop = {{size_var}}) %>% 
    rename(col = {{col_var}})

  df <- df %>% 
    group_by(canton_weight) %>% 
    mutate(max_size = max(pop)) %>% 
    ungroup() %>% 
    mutate(scalingF = pop / max_size) %>% 
    mutate()
  
  
  df$sideL <- st_area(df) %>% sqrt 

  df <- df %>% 
    mutate(sideT = scalingF * sideL) %>% 
    mutate(dist = sideT - sideL) %>% 
    mutate(dist = if_else(as.numeric(dist) < 0, 
                          sqrt(abs(as.numeric(dist))) * -1, 
                          as.numeric(dist))) %>% 
    mutate(dist = units::set_units(dist, "m" ))
    
    
  st_buffer(df, dist = df$dist) %>% select () %>% plot()
}


#   mutate(id = row_number())
# 
# tmcf <- left_join(
#   tmc %>% st_set_geometry(NULL),
#   tmc %>% 
#     st_coordinates() %>% 
#     as.data.frame() %>% 
#     select(X,Y, L2) %>% 
#     rename(id = L2)
# )
# 
# tmcf %>% 
#   ggplot(aes(x = X, y = Y, group = id)) +
#   geom_polygon() +
#   coord_equal()



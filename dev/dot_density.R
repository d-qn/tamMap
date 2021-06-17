# DOT DENSITY map

# https://github.com/RobWHickman/sf.dotdensity/

# https://www.cultureofinsight.com/post/multivariate-dot-density-maps-in-r-with-sf-ggplot2


#devtools::install_github("RobWHickman/sf.dotdensity")
library(tidyverse)
library(tamMap)
library(sf)
library(sf.dotdensity)
library(profvis)

ch_2021 <- st_read(shp_path(2021)[[1]]) %>% 
  select(GMDNR, GMDNAME, KTNR)

ch_prod <- st_read(shp_path(y = 2019, dirGeo = "CH/productive")) %>% 
  st_union() %>% 
  st_transform(crs = st_crs(ch_2021))
ch_2021p <- st_intersection(ch_2021, ch_prod) %>% 
  select(GMDNR, GMDNAME, KTNR)

co2 <- readxl::read_xlsx("je-f-17.03.03.bt.6440.c.xlsx", skip = 6, n_max = 2172-8) %>% 
  filter(!is.na(`No commune`))

df <- left_join(ch_2021p,
          co2 %>% select(`No commune`, Oui, Non),
          by = c("GMDNR" = "No commune")) %>% 
  st_as_sf() %>% 
  filter(!is.na(Oui) & !is.na(Non))


colours <- c("deepskyblue", "red" )
names(colours) <- c("Oui", "Non")


#how many people should lead to one dot
people_per_dots <- 100



# FAIL if any NA value
#calculate the dot positions for each column
tStart <- Sys.time()
profvis({
  df_dots <- calc_dots(df = df,
                       col_names = c("Oui", "Non"),
                       n_per_dot = people_per_dots)
})

Sys.time() - tStart



df_dots_sf <- df_dots %>% st_as_sf(coords = c("lon","lat"))
st_crs(df_dots_sf) <- st_crs(df)

ddm <- ggplot() +
  #first add the shape as a background
  geom_sf(data = df, fill = NA, colour = "white", size = 0.001) +
  #add the dots
  geom_sf(data = df_dots_sf,
             aes(colour = variable), size = 0.000001, shape = 19,
          alpha = 0.6) +
  #colour based on the scale already defined
  #scale_color_brewer(type = "div", palette = "PiYG") +
  scale_colour_manual(name = "Vote", values = c(tamTheme::tam_dpal[13], 
                      tamTheme::tam_pal[8]))  +
  theme_dotdensity() 

ddm
ggsave(filename = "dotdensity.png", dpi = "retina")



  #title
  ggtitle("Dot Density Map of London in the 2017 General Election",
          subtitle = paste("one dot equals", people_per_dots, "people")) +
  theme_dotdensity() +
  #make the legend shapes bigger so it's possible to see them clearly
  guides(colour = guide_legend(override.aes = list(size = 10)))




### Example from github

london_shapefile <- sf.dotdensity::london_shapefile
london_election_data <- sf.dotdensity::london_election_data

#get the data to plot
#merge a shapefile with the population data
london_sf_data <- merge(london_shapefile, london_election_data, by = "ons_id")

#the columns we want to select and plot
parties <- names(london_sf_data)[4:8]
#set up a colour scale for these if so inclined
colours = c("deepskyblue", "red", "gold", "purple", "green")
names(colours) = parties

#how many people should lead to one dot
people_per_dots <- 1000

#calculate the dot positions for each column
london_dots <- calc_dots(df = london_sf_data,
                         col_names = parties,
                         n_per_dot = people_per_dots)

#plot the results

london_plot <- ggplot() +
  #first add the shape as a background
  geom_sf(data = london_sf_data, fill = "transparent",colour = "white") +
  #add the dots
  geom_point(data = london_dots, aes(lon, lat, colour = variable), size = 0.005) +
  #colour based on the scale already defined
  scale_colour_manual(name = "Party", values = colours) +
  #title
  ggtitle("Dot Density Map of London in the 2017 General Election",
          subtitle = paste("one dot equals", people_per_dots, "people")) +
  theme_dotdensity() +
  #make the legend shapes bigger so it's possible to see them clearly
  guides(colour = guide_legend(override.aes = list(size = 10)))

#plot
london_plot

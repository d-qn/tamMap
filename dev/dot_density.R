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

canton <- st_read(shp_path(2021)[[2]]) %>% 
  select(KTNAME)

# crop lakes from canton
canton <- st_difference(canton, st_read(shp_path(2021)[[4]]) %>% st_union())


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
  df_dots <- calc_dots(df = df,
                       col_names = c("Oui", "Non"),
                       n_per_dot = people_per_dots,
                       ncores = 3) 
Sys.time() - tStart


library(dqnTheme)
library(ggtext)
dqnTheme::update_geom_font_defaults()

df_dots_sf <- df_dots %>% st_as_sf(coords = c("lon","lat"))
st_crs(df_dots_sf) <- st_crs(df)


ddm <- ggplot() +
  #first add the shape as a background
  geom_sf(data = canton, fill = NA, colour = "white", size = 0.03) +
  #add the dots
  geom_sf(data = df_dots_sf,
             aes(colour = variable), size = 0.000000001, shape = ".",
          alpha = 0.85) +
  #colour based on the scale already defined
  #scale_color_brewer(type = "div", palette = "PiYG") +
  scale_colour_manual(name = "Vote", values = c(tamTheme::tam_dpal[13], 
                      tamTheme::tam_pal[8]))  +
  theme_lt(grid = F, axis = F, ticks = F, base_size = 20, plot_title_size = 28, subtitle_size = 21)  +
  labs(title = "Votation sur la loi CO2 - le fossé villes-campagnes",
       subtitle = str_c("<b>1 point = ", people_per_dots, " votes</b> pour le ",
                        '<b style="color:#5ca0f7;">OUI</b> ou ' , 
                        'le <b style="color:#d43d51;">NON</b><br>' , 
                        "<i>La loi sur le CO2 a été rejetée le 13 juin 2021 par  51,6% des votants</i>", collapse = ""),
       caption = "Données: OFS, Swisstopo | @duc_qn | code: ggplot2 & sf") +
  theme(plot.background = element_rect(fill = "black", color = NA),
        panel.grid.major = element_blank(), 
        axis.text.x  = element_blank(),
        axis.text.y  = element_blank(),
        legend.position = "none",
        plot.title = element_textbox_simple(
          margin = margin(b=10),
          colour = "white"
          ),
        plot.subtitle = element_textbox_simple(
          lineheight = 1.8,
          family="IBM Plex Sans", 
          colour = "#e6e6e6"
        )
        ) +
  coord_sf(expand = F)

ddm


ggsave(filename = "dotdensity.png", dpi = 300, width = 13, height = 10)




# deckgl
library(deckgl)

dots_df <- do.call(rbind, st_geometry(df_dots_sf %>% st_transform(crs = 4326) )) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

dots_df <- bind_cols(
  dots_df,
  df_dots_sf %>% st_set_geometry(NULL)       
) %>% 
  mutate(color = ifelse(variable == "Oui", 
                        tamTheme::tam_pal[8],tamTheme::tam_dpal[13]))

properties <- list(
      getPosition = ~lon + lat,
      radiusUnits = "pixels",
      opacity =  0.85,
      stroked = T,
      filled = T,
      radiusMinPixels = 1,
      radiusMaxPixels = 100,
      getRadius = 1,
      radiusScale = 100,
      getFillColor = ~color
)

dd_map <- deckgl(
  zoom = 9, pitch = 0,  
  latitude = 46.5151,
  longitude = 6.6244) %>%
  add_basemap(
    # use_carto_style(theme = "positron")
  ) %>%
  add_scatterplot_layer(
    data = dots_df,
    properties = properties
  ) 
dd_map



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

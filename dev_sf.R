require(sf)
require(dplyr)
require(tamMap)
require(ggplot2)




fBallot <- loadCantonsCHFederalBallot()
attr(fBallot, "ballotName")
cidx <- grep("naturalisation", attr(fBallot, "ballotName"), ignore.case = T)
attr(fBallot, "ballotName")[cidx]
attr(fBallot, "date")[cidx]
# get only naturalisation facilitée ballots
cidx <- match(c("3150", "4110", "5100", "5110"), colnames(fBallot))
attr(fBallot, "ballotName")[cidx]
fBallot[,cidx]


path.cantons <-  dir(system.file("extdata/shp/CH/2016", package="tamMap"), "cantons.shp", full.names = T)
ca.df <- st_read(path.cantons, layer = "cantons") %>%
  select(KANTONSNUM)

# duplicte canton data.frame for each ballot
df <- do.call(rbind, lapply(cidx, function(idx) {
  value <- fBallot[,idx]
  names(value) <- canton_CH[match(names(value), canton_CH[,1]), "order"]
  res <- ca.df
  res$value <- value[match(res$KANTONSNUM, names(value))]
  res$ballot <- attr(fBallot, "ballotName")[idx]
  res$date <- attr(fBallot, "date")[idx]
  res
}))
# plot maps
brks <- seq(from = 0, to = 1, length.out = 11) * 100
df$bins <- cut(df$value, breaks = brks, right = F)
df$ballot <- factor(df$ballot, levels = attr(fBallot, "ballotName")[cidx])
ggplot(df) +
  geom_sf(fill = bins) +
  facet_wrap(~ ballot)

ggplot(df, aes(x = long, y = lat, group = group)) + geom_polygon(size = 0, aes(fill = bins)) +
  theme_minimal() + theme(legend.position = "bottom", panel.grid = element_blank(),
                          axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  facet_wrap(~ ballot) + scale_fill_brewer(palette = "BrBG" , drop = F) +
  coord_quickmap(expand = F)







##' # get canton shapefiles as a data.frame
##' path.ch <- getPathShp('CH')
##' layers <-  ogrListLayers(path.ch)
##' ca <- readOGR(path.ch, layer = 'cantons')
##' ca.df <- formatShp(ca) %>% select(long, lat, group, KANTONSNUM)
##' 

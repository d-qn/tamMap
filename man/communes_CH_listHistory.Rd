% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/communes_ch_listHistory.R
\name{communes_history}
\alias{communes_history}
\alias{communes_list}
\title{Swiss communes list & history}
\usage{
communes_history(start = "2012-01-01", end = Sys.Date())

communes_list(
  file = dir(system.file("extdata", package = "tamMap"), "EtatCommunes.xls", full.names =
    T)
)
}
\arguments{
\item{start, end}{Date objets: the timeframe at which to query the list of communes changes}

\item{file, }{a string the path to the xls file containing the full list of Swiss communes. Probably doesn't need to modified}
}
\value{
a data.frame

a tibble data.frame of all the communes as of April 2018
}
\description{
Helper functions to reconstruct the merging, scission, name changes of Swiss communes (a freaking headache) or more simply listing all the communes that ever existed.
}
\examples{
### List of communes
communesCH_list <- communes_list()

### Communes history
start <- as.Date("2012-01-01")
data <- communes_history()
## Get all the communes "Fusion"  [A] + [B] = [C]
# get the ID of the new communes created 
ginimut <- data[which(data$GINIART == 21 & data$GINIDAT >= start),'GINIMUT']
# get all the communes "radiated" radiée/suprimée
data[which(data$GFINMUT \%in\% ginimut & data$GFINART == 29),]
# Get for each commune created between start & end time, the GBFSNR of the merged communes
result <- lapply(ginimut, function(i) {
  # get the nr and name of the created commune
  nr_name <- data[which(data$GINIART == 21 & data$GINIMUT == i), c('GBFSNR', 'GNAME')]
  # get the nr of the radiated communes
  radiatedN <- data[which(data$GFINART == 29 & data$GFINMUT == i),'GBFSNR']
  if(identical(radiatedN, integer(0))) {
    list(nr = unlist(nr_name[1]), name = unlist(nr_name[2]), radiated = NULL)
  } else {
    list(nr = unlist(nr_name[1]), name = unlist(nr_name[2]), radiated = unlist(radiatedN))
  }
})
names(result) <- ginimut
result
}
\seealso{
data downloaded from \url{https://www.bfs.admin.ch/bfs/fr/home/bases-statistiques/repertoire-officiel-communes-suisse/liste-historisee-communes.html}, explication \url{https://www.bfs.admin.ch/bfs/fr/home/bases-statistiques/repertoire-officiel-communes-suisse/liste-historisee-communes.assetdetail.4062823.html}

GINIART code:
11 Commune politique
12 Territoire non attribué à une commune
13 Partie cantonale de lac
15 District
16 Canton sans districts
17 Territoire non attribué à un district
20 Première saisie commune/district
21 Création commune/district
22 Changement de nom du district
23 Changement de nom de la commune
24 Rattachement à un autre district/canton
26 Modification du territoire de la commune
27 Renumérotation formelle de la commune/du district
29 Radiation commune/district
30 Annulation de la mutation

\href{https://www.agvchapp.bfs.admin.ch/fr/home}{État des communes - État au 01.04.2018 (Excel XLS)}
}

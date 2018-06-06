# tamMap

Suite of convenience functions for geospatial mapping of Swiss data. It integrates geographical data at different levels (municipality, cantons, ...), cities. It also incorporates various muncipality & cantons socio-econmic and voting indicators. 

It relies on bleeding edge R packages, primarily *sf* and *tidyverse*. 

## Installation

It will probably never be on CRAN. To be installed from github:

``` r
# install.packages("devtools")
devtools::install_github("d-qn/taMap")
```
## TODO

### Clean up
* Use swisstopop API to get geospatial data

### Features
* inlet_helpers: use [gglocator](https://stackoverflow.com/questions/9450873/locator-equivalent-in-ggplot2-for-maps?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa) to find the shift. Seems broken though
* barcode plot: to show municpality distribution

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

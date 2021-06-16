library(sf)
library(raster)
library(dplyr)
library(spData)


#load spatial data and subset
canterbury = nz %>%
  filter(Name == "Canterbury")

canterbury_height = nz_height[canterbury, ]
plot(canterbury_height)
plot(canterbury)

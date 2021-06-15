### Working with spData attributes
library(sf)
library(raster)
library(spData)
library(dplyr)
library(tidyverse)
data("us_states")
data("us_states_df")
# View(data("us_states_df"))
class(data(us_states))
?us_states
us_states
View(us_states)
## select NAME Column
us_states_name <- select(us_states, NAME)
View(us_states_name)

## select columns with population
us_pop_all = select(us_states, total_pop_10, total_pop_15)
us_pop_all
us_states$total_pop_10
us_pop_sqb = us_states["total_pop_10"]
View(us_pop_sqb)

## select states in mid-west
usStateData = us_states
usRegion = select(usStateData, REGION)
usMidWest = filter(usRegion, REGION == "Midwest")
plot(usMidWest)


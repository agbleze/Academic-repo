### WORKING WIHT ATTRIBUTE DATA
library(sf)
library(raster)
library(dplyr)
library(stringr)
library(tidyr)
library(spData)

st_sf(data.frame(n= world$name_long), g = world$geom)

## understanding basci properties of vevtor
dim(world)  # number of rows and columns
nrow(world) # number of rows
ncol(world) #number of columns
WORLD_df <- st_drop_geometry(world) # removes the geometry column
class(WORLD_df)

##SUBSET ROWS BY POSITION. returns first 10 rows and all columns
world[1:10,]
## subset columns by position 3rd col to 5th
world[,3:5]
## subset columns based of names
world[,c("continent", "subregion")]

new_pl <- world$area_km2 >20000
summary(new_pl)
new_da <- world[new_pl,]
new_da
small_countries = world[world$area_km2 < 10000,]
small_countries

## using subset method
another_dt <- subset(world, area_km2 > 300000)
another_dt
## select columns by name
wrlddata <- dplyr::select(world, name_long, pop)
wrlddata
## omit some columns
data_omit <- dplyr::select(world, -subregion, -area_km2, -continent)
data_omit
### select n rename columns
rename_dat = dplyr::select(world, pop, Region = continent)
rename_dat

worlddata = world[, c("name_long", "pop")]
worlddata
names(worlddata)[names(worlddata) == "pop"] = "population"

d = data.frame(pop = 1:10, area = 1:10)
d
a = d[, "pop", drop = FALSE]
a
pid = pull(d, pop)
pid

## slice is the row equivalnet of select. slice returns rows
slice(world, 1:10)

## using filter to select row meeting conditions
filt = filter(world, pop > 10)
filt

## piping
wrd = world %>%
  filter(continent == "Africa") %>%
  select(name_long, pop)%>%
  slice(1:5)
wrd

### BASICS OF RASTER DATA

library(raster)
library(spDataLarge)

## DEFINE PATH TO DATA
raster_filepath <- system.file("raster/srtm.tif", package = "spDataLarge")
rasterData = raster(raster_filepath)
rasterData

help("raster-package") ## view methods and classes in the raster package

############ Experiment with various methods in the raster package
plot(rasterData)
setMinMax(rasterData)
persp(rasterData)
image(rasterData)
contour(rasterData)
maxValue(rasterData)
freq(rasterData)
cellStats(rasterData, stat = mean)
point_data <- rasterToPoints(rasterData)
point_data
plot(point_data)
spplot(rasterData)
hist(rasterData)
barplot(rasterData)
density(rasterData)
boxplot(rasterData)
nlayers(rasterData)
filename(rasterData)
isLonLat(rasterData)
origin(rasterData)
nbands(rasterData)
coordinates(rasterData)
writeRaster(rasterData)
canProcessInMemory(rasterData)


####creating raster
rastern = raster(nrows = 6, ncols = 6, res = 0.5,
                 xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
                 vals = 1:36)
plot(rastern)
?raster

## combining rasters with Rasterbrick
multipleRaster = system.file("raster/landsat.tif", package = "spDataLarge")
brickRaster = brick(multipleRaster)
brickRaster
image(brickRaster)
spplot(brickRaster)

#### Using RasterStack to combine multiple raster layers
raster1 = raster(brickRaster, layer = 1)
raster2 = raster(xmn = 200, xmx = 500,
                 ymn = 300, ymx = 700, res = 30)
values(raster2) = sample(seq_len(ncell(raster2)))
crs(raster2) = crs(raster1)
extent(raster2) = extent(raster1)
ncol(raster2) = ncol(raster1)
raster_stack = stack(raster1, raster2)
raster_stack
spplot(raster_stack)

### working with CRS
# view and select crs
crs_data = rgdal::make_EPSG()
View(crs_data)

###ste projection of raster object
## raster object ONLY accept proj def
projection(raster_stack)
projection(raster_stack) = "+proj=utm +zone=12 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
                            +units=m +no_defs" # set CRS
## get the resolution
res(raster_stack)
reproj = projectRaster(raster_stack, crs = 26912) ## project the raster object to diff proj
res(reproj)

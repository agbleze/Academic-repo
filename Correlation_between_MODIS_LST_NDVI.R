##########################################################################
#
# Author  : LINUS AGBLEZE
# Task   : TO ASSESS CORRELATION BETWEEN MODIS LAND SURFACE TEMPERATURE (LST) AND NDVI
# DATE    : 25th March, 2019
# INPUT DATA FORMAT: RASTER (MODIS)
# OUTPUT DATA FORMAT: RASTER
#
##########################################################################
#--------------------------------------
# install packages, execute only ones
#--------------------------------------
install.packages("raster")

#Load Library  
library(raster)
library(sp) 	   # provides classes for spatial data in R
library(raster)	 # provides classes and methods for raster datasets
library(rgdal)	 # interface to the Geospatial Data Abstraction Library to read and write different spatial file formats



# Defining the working directory
setwd('C:/Rassign/CORRELATION/EXTRACT/NDVI')

#Load Raster images
NDVI <- raster("NDVIMOD13A2.tif")
LST <- raster("MODISLST11A2.tif")

#Load shapefile of ROI
ROI <- readOGR(dsn = 'C:/Rassign/CORRELATION', layer = 'Western region')
plot(ROI)

#cropping the NDVI image with ROI,
NDVISub <- crop(NDVI, extent(ROI))
plot(NDVISub)

#Cropping the LST image with ROI
LSTSub <- crop(LST, extent(ROI))
plot(LSTSub)

Pal <- colorRampPalette(c("red" , "blue" , "green"))

#Correlation function
corrNDVI_LST <- corLocal(NDVISub , LSTSub , test = T)
plot(corrNDVI_LST)

# Plot Pearson value correlation and P value
plot(corr$pearson , col = Pal(20) , main = "Pearson Correlation")
plot(corr$p.value , col = c("red" , "blue" , rep("azure3" , 15)) ,main = "p-value")



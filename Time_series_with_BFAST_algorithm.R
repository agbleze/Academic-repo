##########################################################################
###
# Author        : LINUS AGBLEZE
# OBJECTIVE     : USE BFAST TO DETECT CHANGES IN NDVI TIME SERIES
# Input         : NDVI Time Series
# INPUT FORMAT  : CSV
# OUTPUT FORMAT : LINE GRAPH
# DATE          : 25th March, 2019
# 
###
##########################################################################
#--------------------------------------
# install packages, execute only onces
#--------------------------------------

install.packages("bfast", dependencies=TRUE)

# Load library
library(zoo)
library(bfast)
library(tidyverse)

#--------------------------------------
# Set the working directory 
#--------------------------------------
setwd("C:/Rassign") # The working directory path needs to be specified to where your data is located

## read NDVI time series
Modisndvi_data <- read.csv(file = "C:/Rassign/NDVI series tera.csv") 

## Inspect the column heading of the time series data
head(Modisndvi_data)

## Checking variables
str(Modisndvi_data[1,1:6])

## Create a time Series object for five different biomes 
Modisndvi_ts <- ts(Modisndvi_data[2:6], start=c(2007,1), frequency=23)

#Plot NDVI Time series of the different biomes
plot(Modisndvi_ts,type="l", main = "NDVI Time Series for five Biomes",cex.lab = 0.5, cex.axis=0.75 )


#########################################################################################################
#
### Create time Series object for Cropland (Area of Interest) ###
ModisndviCropland_ts <- ts(Modisndvi_data[,2], start=c(2007,1), frequency=23)

##  Additive decomposition
dc = decompose(ModisndviCropland_ts, type = 'additive')

# Fitting bfast model for Cropland (Testing for breaks in Cropland)
fitCropland = bfast01(Modisndvi_ts, test = "OLS-MOSUM", level = 0.05, aggregate = all, bandwidth = 0.15, functional = "max", order = 3, na.action = na.omit)

# Displaying graph of various components of BFAST of Cropland
plot(fitCropland, plot.type = "multiple",
     which = c("response", "trend", "season"), screens = c(1, 2, 3), main = "BFAST OF CROPLAND")


## fit variations for Cropland
bfCropland <- bfast01(ModisndviCropland_ts)

bfCropland3 <- bfast01(ModisndviCropland_ts, test = c("BIC", "OLS-MOSUM", "supLM"), aggregate = any, bandwidth = 0.15) 

## inspect Cropland test decisions
bfCropland$test
bfCropland$breaks

bfCropland3$test
bfCropland3$breaks

############################################################################################################
#
### Create time Series object for Deciduous Broadleaf Forest (Area of Interest) ###
ModisndviDeciduous_ts <- ts(Modisndvi_data[,3], start=c(2007,1), frequency=23)
##  Additive decomposition
dc = decompose(ModisndviDeciduous_ts, type = 'additive')

## Fitting bfast model for Deciduous Broadleaf Forest (Detecting breaks)
fitDeciduous = bfast01(ModisndviDeciduous_ts, test = "OLS-MOSUM", level = 0.05, aggregate = all, bandwidth = 0.15, functional = "max", order = 3, na.action = na.omit)

# Displaying graph of various components of BFAST of Deciduous Broadleaf Forest
plot(fitDeciduous, plot.type = "multiple",
     which = c("response", "trend", "season"), screens = c(1, 2, 3), main = "BFAST of Deciduous Broadleaf Forest")


## fit variations for Deciduous Broadleaf Forest
bfDeciduous <- bfast01(ModisndviDeciduous_ts)

#Estimate breaks in Deciduous Broadleaf Forest 
bfDeciduous3 <- bfast01(ModisndviDeciduous_ts, test = c("BIC", "OLS-MOSUM", "supLM"), aggregate = any, bandwidth = 0.15) 

## inspect Deciduous Broadleaf Forest test decisions
bfDeciduous$test
bfDeciduous$breaks

bfDeciduous3$test
bfDeciduous3$breaks

#######################################################################################################
#
### Create time Series object for Mixed Forest (Area of Interest) ###
ModisndviMixed_ts <- ts(Modisndvi_data[,4], start=c(2007,1), frequency=23)

##  Additive decomposition
dc = decompose(ModisndviMixed_ts, type = 'additive')

## Fitting bfast model for Mixed Forest (To detect breaks)
fitMixedforest = bfast01(ModisndviMixed_ts, test = "OLS-MOSUM", level = 0.05, aggregate = all, bandwidth = 0.15, functional = "max", order = 3, na.action = na.omit)

# Display graph of various components of BFAST of Mixed Forest
plot(fitMixedforest, plot.type = "multiple",
     which = c("response", "trend", "season"), screens = c(1, 2, 3), main = "BFAST OF Mixed Forest")


## fit variations for Mixed Forest
bfMixedforest <- bfast01(ModisndviMixed_ts)

#Estimate breaks in Mixed Forest
bfMixedforest3 <- bfast01(ModisndviMixed_ts, test = c("BIC", "OLS-MOSUM", "supLM"), aggregate = any, bandwidth = 0.15) 

## inspect Mixed Forest test decisions
bfMixedforest$test
bfMixedforest$breaks

bfMixedforest3$test
bfMixedforest3$breaks

#############################################################################################################
#
### Create time Series object for Evergreen Needleleaved Forest (Area of Interest) ###
ModisndviEvergreen_ts <- ts(Modisndvi_data[,5], start=c(2007,1), frequency=23)

##  Additive decomposition
dc = decompose(ModisndviEvergreen_ts, type = 'additive')

# Fitting bfast model for Evergreen Needleleaved Forest (Testing for breaks)
fitEvergreen = bfast01(ModisndviEvergreen_ts, test = "OLS-MOSUM", level = 0.05, aggregate = all, bandwidth = 0.15, functional = "max", order = 3, na.action = na.omit)

# Display graph of various components of BFAST for Evergreen Needleleaved Forest
plot(fitEvergreen, plot.type = "multiple",
     which = c("response", "trend", "season"), screens = c(1, 2, 3), main = "BFAST OF Evergreen Needleleaved Forest")


## fit variations for Evergreen Needleleaved Forest
bfEvergreen <- bfast01(ModisndviEvergreen_ts)
bfEvergreen3 <- bfast01(ModisndviEvergreen_ts, test = c("BIC", "OLS-MOSUM", "supLM"), aggregate = any, bandwidth = 0.15) 

## inspect Evergreen Needleleaved Forest test decisions
bfEvergreen$test
bfEvergreen$breaks

bfEvergreen3$test
bfEvergreen3$breaks

#########################################################################################################
#
### Create time Series object for Urban Builtup (Area of Interest) ###
ModisndviUrban_ts <- ts(Modisndvi_data[,6], start=c(2007,1), frequency=23)

##  Additive decomposition
dc = decompose(ModisndviUrban_ts, type = 'additive')

# Fitting bfast model for Urban Builtup (Testing for breaks)
fitUrban = bfast01(ModisndviUrban_ts, test = "OLS-MOSUM", level = 0.05, aggregate = all, bandwidth = 0.15, functional = "max", order = 3, na.action = na.omit)

# Display various components of BFAST for Urban Builtup
plot(fitUrban, plot.type = "multiple",
     which = c("response", "trend", "season"), screens = c(1, 2, 3), main = "BFAST OF Urban Builtup")


## fit variations for Urban Builtup
bfUrban <- bfast01(ModisndviUrban_ts)
bfUrban3 <- bfast01(ModisndviUrban_ts, test = c("BIC", "OLS-MOSUM", "supLM"), aggregate = any, bandwidth = 0.15) 

## inspect Urban Builtup test decisions
bfUrban$test
bfUrban$breaks

bfUrban3$test
bfUrban3$breaks


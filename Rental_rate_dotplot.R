#What is the trend for rental rates in the Austin metro area?
#load libraries
library(ggplot2)
library(readr)
library(tidyr)

# read data
datald = read_csv("County_Zri_SingleFamilyResidenceRental.csv", col_names = TRUE)
datald%>%
select(RegionName, State, Metro, `2010` = `2010-12`, `2011` = `2011-12`, `2012` = `2012-12`, `2013` = `2013-12`, `2014` = `2014-12`, `2015` = `2015-12`, `2016` = `2016-12`, `2017` = `2017-12`)%>%  
filter(State == 'TX' & Metro == 'Austin')%>%  
gather(`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, key = "YR", value = "ZRI")%>%  
ggplot(mapping = aes(x = YR, y = ZRI, colour = RegionName)) + geom_point() + ggtitle("ZRI for single") + xlab("Year") + ylab("ZRI") + geom_text(aes(label = ZRI, vjust = 1), size = 3) 

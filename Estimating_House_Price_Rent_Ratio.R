## Determining the Price-Rent Ratio for the Austin metropolitan area
##load library
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)

homeprice = read_csv("County_Zhvi_SingleFamilyResidence.csv", col_names = TRUE)
Hmpr_select = select(homeprice, RegionName, State, Metro, `2010` = `2010-12`, `2011` = `2011-12`, `2012` = `2012-12`, `2013` = `2013-12`, `2014` = `2014-12`, `2015` = `2015-12`, `2016` = `2016-12`, `2017` = `2017-12`)
Hmpr_filter = filter(Hmpr_select, State == 'TX' & Metro == 'Austin' & RegionName == 'Travis')
Hmpr_gat = gather(Hmpr_filter, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, key = 'YR', value = 'ZHVI')

## analysze rental data
Rentpr = read_csv("County_Zri_SingleFamilyResidenceRental.csv", col_names = TRUE)
Rentpr_select = select(Rentpr, RegionName, State, Metro, `2010` = `2010-12`, `2011` = `2011-12`, `2012` = `2012-12`, `2013` = `2013-12`, `2014` = `2014-12`, `2015` = `2015-12`, `2016` = `2016-12`, `2017` = `2017-12`)
Rentpr_filter = filter(Rentpr_select, State == 'TX' & Metro == 'Austin' & RegionName == 'Travis')
Rentpr_gat = gather(Rentpr_filter, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, key = 'YR', value = 'ZRI')

##Join both data
JoinData = inner_join(Hmpr_gat, Rentpr_gat, by = "YR")
View(JoinData)
## Calculate Price rent ratio
PrRentRatio = mutate(JoinData, PriceRentRatio = ZHVI / (12 * ZRI))
## plot bar chart
ggplot(data = PrRentRatio, mapping = aes(x = YR, y = PriceRentRatio)) + geom_col() +xlab("Year") + ylab("Price-Rent ratio") + geom_text(aes(label = PriceRentRatio, vjust = -0.5), size = 3) + ggtitle('Price Rent Ratio')

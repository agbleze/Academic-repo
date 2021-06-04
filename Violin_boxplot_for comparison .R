##  Comparing residential home values in Austin to other Texas and U.S. metropolitan areas
# Load library
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

datard = read_csv("County_Zhvi_SingleFamilyResidence.csv", col_names = TRUE)
datard%>%
  select(RegionName, State, Metro, `2010` = `2010-12`, `2011` = `2011-12`, `2012` = `2012-12`, `2013` = `2013-12`, `2014` = `2014-12`, `2015` = `2015-12`, `2016` = `2016-12`, `2017` = `2017-12`)%>%
  filter(State == "TX" & Metro %in% c("Austin", "San Antonio", "Dallas-Fort Wprth", "Houston"))%>%
  gather(`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, key = "YR", value = 'ZHVI')%>%
  group_by(Metro)%>%
  ggplot(mapping = aes(x = Metro, y = ZHVI)) + geom_violin() + geom_boxplot(width = 0.1) + ggtitle("ZHVI for Metro Texas") + xlab("Metro") + ylab("ZHVI")

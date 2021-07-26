###### time series decomposition  ##########
library(ggplot2)
library(forecast)

autoplot(elecsales) + xlab("year") + ylab("GWh") + 
  ggtitle("Annual elec sales: South Australia")

### Moving Average
ma(elecsales, 5)
elecsales

##### plot data with MA
autoplot(elecsales, series = "Data") + 
  autolayer(ma(elecsales, 5), series = "5-MA") +
  xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia") +
  scale_colour_manual(values = c("Data" = "grey50", "5-MA" = "red"),
                      breaks = c("Data", "5-MA"))

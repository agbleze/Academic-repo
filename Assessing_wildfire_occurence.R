## Has the length of the fire season increased over time?

## load library
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
dataload = read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
selData = select(dataload, ORGANIZATI, YR = YEAR_, STATE, ACRES = TOTALACRES, CAUSE, STARTDATED)
filData = filter(selData, ACRES >= 1000)
startTimeData = mutate(filData, DOY = yday(as.Date(STARTDATED, format = '%m/%d/%y%H:%M')))
#group data by year
grpData = group_by(startTimeData, YR)
## make a column for earlest and latest day of fire
sumData = summarize(grpData, dtEarly = min(DOY, na.rm = TRUE), dtLate = max(DOY, na.rm = TRUE))
## Plot graph
ggrplot() + geom_line(mapping = aes(x = YR, Y = dtEarly, color = "B")) +
  geom_line(mapping = aes(x = YR, y = dtLate, color = "R")) + geom_smooth(method = loess, se = TRUE, aes (x = YR, y= dtLate, color = R )) +
  xlab("Year") + ylab("Day of year") + scale_colour_manual(name = "Legend", values = c("R" = "#FF0000", "B" = "#000000"), labels = c("First Fire", "Last Fire"))
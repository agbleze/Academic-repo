###Does the average wildfire size differ by federal organization
#load library
library(readr)
library(lubridate)
library(ggplot2)

readData = read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
readData%>%
  select(ORG = ORGANIZATI, STATE, YR = YEAR_, ACRES = TOTALACRES, CAUSE, STARTDATED)%>%
  filter(ACRES >= 1000)%>%
  group_by(ORG, YR)%>%
  summarize(mean_acres = mean(ACRES))%>%
  ggplot(mapping = aes(x = YR, y = log(mean_acres))) + geom_point() +
  facet_wrap(~ORG) + geom_smooth(method = loess, se = TRUE) +
  ggtitle("ACres burnt by federal government") + xlab("Year") + ylab("log of total acres burned") + theme_dark()
  

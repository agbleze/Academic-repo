#### Estimating normality  ##########

library(readxl)
library(ggplot2)

#install.packages("psych")
#install.packages("pastecs")
library(psych)  ## for summarizing
library(pastecs)  ## for summarizing
# setRepositories(graphics = getOption("menu.graphics"),
#                 ind = NULL, addURLs = character())

## load data
dataset_load <- read_excel("~/Downloads/R FOR DATA ANALYSIS AND SCIENCE/R_Business_Analytic/Golf Stats.xlsx", sheet = "2011")
View(dataset_load)
head(dataset_load[, 1:10])

## histogram with reference normal distribution
ggplot(dataset_load, aes(`Driving Accuracy`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset_load$`Driving Accuracy`, na.rm = T),
                                         sd = sd(dataset_load$`Driving Accuracy`, na.rm = T))) +
  xlab("Driving Accuracy (%)")

## histogram with reference normal distribution for earnings
ggplot(dataset_load, aes(`Earnings`)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset_load$Earnings, na.rm = T),
                                         sd = sd(dataset_load$Earnings, na.rm = T))) +
  scale_x_continuous(labels = scales::dollar)

### Q-Q plot to test for normality
# a seeming straight line shows the data distribution is normal
# curved line shows the data deviates from normality or skewed
qqnorm(dataset_load$`Driving Accuracy`, main = "Normal Q-Q Plot for Driving Accuracy")

# Q-Q plot for earning
qqnorm(dataset_load$Earnings, main = "Normal Q-Q Plot for Earnings")

# assess summary of a variable
describe(dataset_load$`Driving Accuracy`)
# assess multiple var
describe(dataset_load[, c("Driving Accuracy", "Earnings")])
## using stat.desc()
stat.desc(dataset_load[, c("Driving Accuracy", "Earnings")], basic = F, norm = T)

#### shapiro wirk test for normality
shapiro.test(dataset_load$`Driving Accuracy`)
shapiro.test(dataset_load$Earnings)

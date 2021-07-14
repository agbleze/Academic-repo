library(moments)
#install.packages("moments")
library(outliers)
#install.packages("outliers")
testData <- read_xlsx("~/Downloads/R FOR DATA ANALYSIS AND SCIENCE/R_Business_Analytic/Baseball Salaries 2011.xlsx")
testData
View(testData)
library(ggplot2)

library(readxl)
testDatan <- read_excel("~/Downloads/R FOR DATA ANALYSIS AND SCIENCE/R_Business_Analytic/Baseball Salaries 2011.xlsx", 
                                     sheet = "Salaries 2011")
View(testDatan)
mean(testDatan$Salary, na.rm = TRUE)

### function for mode
get_mode <- function(v){
  unique_value <- unique(v)
  unique_value[which.max(tabulate(match(v, unique_value)))]
}
get_mode(testDatan$Salary)

### variance
var(testDatan$Salary)
### standard deviation
sd(testDatan$Salary)
### mean absolute deviation
mad(testDatan$Salary, center = median(testDatan$Salary))

### MEASURE SHAPE OF DISTRIBUTION
## skewness measures symetry of distribution negative = left skewed
## kurtosis measured peakedness of the distribution negative = platykurtic, positive = leptokurtic
skewness(testDatan$Salary, na.rm = TRUE)
kurtosis(testDatan$Salary, na.rm = TRUE)

## outliers are most extreme values outside the observation
# get most extreme right-tail observation
outlier(testDatan$Salary)

# get most extreme left-tail observation
outlier(testDatan$Salary, opposite = TRUE)

## outliers based on z-scores
z_scores <- scores(testDatan$Salary, type = "z")
z_scores
which(abs(z_scores) > 1.96)

## outliers based on values less than or greater than
## whiskers on a boxplor (1.5 x IQR or more below 1st quartile or above 3rd quartile)
which(scores(testDatan$Salary, type = "iqr", lim = 1.5))

## remove outlier
testOutlierrm <- rm.outlier(testDatan$Salary)
View(data.frame(testOutlierrm$Salary))
testOutlierepl <- rm.outlier(testDatan$Salary, fill = TRUE)
View(testOutlierepl)

##histogram
hist(testDatan$Salary)

## histogram with ggplot2
ggplot(testDatan, aes(Salary)) + geom_histogram(colour = "black",
                                                fill = "white")+
  scale_x_log10(labels = scales::dollar) + geom_vline(aes(xintercept = mean(Salary)),
                                                      color = "red", linetype = "dashed")+
  annotate("text", x = mean(testDatan$Salary) * 2, y = 255, 
           label = paste0("Avg: $", round(mean(testDatan$Salary)/1000000, 1), "M"))

### dotplot
ggplot(testDatan, aes(Salary)) + geom_dotplot() + scale_x_continuous(labels = scales::dollar)

### boxplot
boxplot(testDatan$Salary, horizontal = TRUE, log = "x")

### boxplot with ggplot
ggplot(testDatan, aes(x=factor(0), y = Salary)) +
  geom_boxplot() + xlab("") + scale_x_discrete(breaks = NULL) +
  scale_y_log10(labels = scales::dollar)+ coord_flip() +
  geom_jitter(shape = 16, position = position_jitter(0.4), alpha = .3) +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, fill = "blue")

### boxplot for comparison
ggplot(testDatan, aes(x = Position, y = Salary)) +
  geom_boxplot()+
  scale_y_continuous(labels = scales::dollar) +
  coord_flip()
#################################################
catData <- read_xlsx("~/Downloads/R FOR DATA ANALYSIS AND SCIENCE/R_Business_Analytic/Supermarket Transactions.xlsx", sheet = "Data")
head(catData)
head(catData[,c(3:5, 8:9, 14:16)])
## frequency of various columns
df <- table(catData$`Marital Status`, catData$`Gender`)
View(catData)
catData2 <- table(catData$`Marital Status`, catData$Gender,
                  catData$`State or Province`)
NEW <- ftable(catData2)
View(NEW)

########## proportions
prop_catData2 <- prop.table(catData2)
View(prop_catData2)
prop.table(df)
### customer % across location by gender and marital status
ftable(round(prop.table(catData2), 3))

#### marginal
## frequency marginals
#row mrginals total of each marital status across gender
margin.table(catData2, 1)
# column marginals --total of each gender across marital status
margin.table(catData2, 2)
##### Percentage marginals
# row marginals --- row percentages across gender
prop.table(catData2, margin = 1)
# colum marginals --- column % across marital status
prop.table(catData2, margin = 2)

####  barchart
ang = 90
# reoder levels
reorder_size <- function(x){
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}
ggplot(catData, aes(x=reorder_size(`State or Province`))) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  theme(axis.text.x = element_text(angle = ang, hjust = 1))+
  xlab("State or Province") +
  scale_y_continuous(labels = scales::percent, name = "Proportion")

### plot for gender and marital status
ggplot(catData, aes(x = reorder_size(`State or Province`))) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  xlab("State or Province") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(`Marital Status` ~Gender) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

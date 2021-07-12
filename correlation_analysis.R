###### correlation analysis  ##########
library(corrgram) ## visualizing data
library(corrplot) ## visualizing data
library(Hmisc)  ## produce correlation matrices with p-values
library(ppcor)  # assesses partial correlations
library(readxl)
library(gridExtra)
library(grid)

setRepositories(graphics = getOption("menu.graphics"),
                ind = NULL, addURLs = character())

golf <- read_excel("~/Downloads/R FOR DATA ANALYSIS AND SCIENCE/R_Business_Analytic/Golf Stats.xlsx", sheet = "2011")
head(golf[, 1:9])

qplot(x = Events, y = Rounds, data = golf) +
  geom_smooth(method = "lm", se = F) + ggtitle("Strong Positive Association")

p1 <- qplot(x = `Driving Accuracy`, y = `Yards/Drive`, 
            data = golf) + geom_smooth(method = "lm", se = F) +
  ggtitle("Moderate negative assoc")

p2 <- qplot(x=Age, y = `Greens in Regulation`, data = golf) +
  geom_smooth(method = "lm", se = F) + ggtitle("Weak/No assoc")
grid.arrange(p1, p2, ncol = 2)

p1 <- qplot(x = x1, y = y1, data = anscombe)
p2 <- qplot(x = x2, y = y2, data = anscombe)
p3 <- qplot(x = x3, y = y3, data = anscombe)
p4 <- qplot(x = x4, y = y4, data = anscombe)

grid.arrange(p1, p2, p3, p4, ncol = 2,
             top = textGrob("Anscombe's Quartet"))
View(anscombe)

#ploting multiple scatter plots
pairs(golf[, c(3:5)])

########## ploting with corrgram
par(bg = "#fdfdfd")
corrgram(golf[, c(1, 3:10)], lower.panel = panel.shade, upper.panel = panel.pts)

#### plot using corrplot
## calculate correlation matirx
cor_matrix <- cor(golf[, c(1, 3:10)], use = "complete.obs")
plot(cor_matrix)
corrplot.mixed(cor_matrix, lower = "circle", upper = "number", tl.pos = "lt", diag = "u")

#### Pearson correlation using cor
cor(golf$Age, golf$`Yards/Drive`)
cor(golf$Age, golf$`Yards/Drive`, use = "complete.obs")

### pearson correlation for multiple variables
cor(golf[, c(1, 3:10)], use = 'complete.obs')

### using cor.test to calculate pearson correlation, sig.level and confidence interval
cor.test(golf$Age, golf$`Yards/Drive`, use = 'complete.obs')

## using rcorr() for pairwise pearson correlation
# of multiple variables and p-values
## based on Hmisc library
rcorr(as.matrix(golf[, c(1, 3:9)]))

###### partial correlation will control for some variables
## done with ppcor library
# remove NA
golf_complete <- na.omit(golf)
## partial correlation bt x and y and control for z
pcor.test(x = golf_complete$`Yards/Drive`, y = golf_complete$`Greens in Regulation`,
          z = golf_complete$`Driving Accuracy`)
#### second-order partial correlation
## controlling 2 variables
pcor.test(x = golf_complete$`Yards/Drive`, y = golf_complete$`Greens in Regulation`,
          z = golf_complete[, c("Driving Accuracy", "Age")])



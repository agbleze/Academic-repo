########### machine learning  ##############

#########
#lm(formula = as.numeric(Revenue) ~ Sessions, data_select)
###########

library(h2o)
library(rsample)
library(caret)
library(dplyr)
library(AmesHousing)
library(modeldata)
library(magrittr)
library(ggplot2)
library(forecast)
library(gridExtra)
library(glmnet)
## turn-off progress bars
h2o.no_progress()

# launch h2o
h2o.init()

# ames data
ames <- AmesHousing::make_ames()
ames.h2o <- as.h2o(ames)

# attrition data
data("attrition")
churn <- attrition %>%
  mutate_if(is.ordered, factor, ordered = F)
churn.h2o <- as.h2o(churn)

######################## simple random sampling
## use base R
set.seed(123) ## allows randomized splits to be reprodouce
index <- sample(1:nrow(ames), round(nrow(ames) * 0.7))
train_1 <- ames[index, ]
test_1 <- ames[-index, ]

(Train1pt <- ggplot(train_1, aes(x = Sale_Price)) + geom_histogram(fill = "blue") + ggtitle("Raw training set"))

## use caret 
set.seed(123)
index2 <- createDataPartition(ames$Sale_Price, p = 0.7, list = FALSE)
train_2 <- ames[index2, ]
test_2 <- ames[-index2, ]

## rsample package
set.seed(123)
split_1 <- initial_split(ames, prop = 0.7)
train_3 <- training(split_1)
test_3 <- testing(split_1)

## use h2o 
split_2 <- h2o.splitFrame(ames.h2o, ratios = 0.7, seed = 123)
train4 <- split_2[[1]]
test_4 <- split_2[[2]]

ggplot(data = train_2, aes(x = Sale_Price)) + geom_density(trim = T) + geom_density(data = test_2, trim = T, col = "red") 


### response distribution
table(churn$Attrition) %>%
  prop.table()

###############  stratified sampling with rsample package ###################
set.seed(123)
split_strat <- initial_split(churn, prop = 0.7,strata = "Attrition")
train_strat <- training(split_strat)
test_strat <- testing(split_strat)

# consistent response ration bt train and test
table(train_strat$Attrition) %>%
  prop.table()

table(test_strat$Attrition) %>%
  prop.table()


############### feature engineering  #################
## one-hot encding


### response transformation
# normalize with log transformation
train_log_y <- log(train_1$Sale_Price)
test_log_y <- log(test_1$Sale_Price)
dat_train_log_y <- data.frame(train_log_y)

## hsitogram for log transformed training set 
(logTrans <- ggplot(dat_train_log_y, aes(x = train_log_y)) + geom_histogram(fill = "yellow") + ggtitle("Log transformation"))

### Box Cox transformation
lambda <- forecast::BoxCox.lambda(train_1$Sale_Price)
train_bc_y <- forecast::BoxCox(train_1$Sale_Price, lambda)
test_bc_y <- forecast::BoxCox(test_1$Sale_Price, lambda)
datfr_train_bc_y <- data.frame(train_bc_y)
(boxCox <- ggplot(datfr_train_bc_y, aes(x = train_bc_y)) + geom_histogram(fill = "red") + ggtitle("BoxCox transform"))

### raw trainging set, log transform training set and BoxCox transform training set
grid.arrange(Train1pt, logTrans, boxCox, ncol = 2)


######## retransforming transformed value into normal state
### log transfor
y <- log(10)

## re-transformation of log transformed
exp(y)

## Box Cox transform a value
z_bc <- forecast::BoxCox(10, lambda)
# Reverse BoxCox transformation
InvBoxCox(z_bc, lambda)

################### predictor transformation
# identify only predictor variable
features <- setdiff(names(train_1), "Sale_Price")

#preprocess estimation based on training features centering and scaling
pre_process <- preProcess(x = train_1[, features],
                          method = c("center", "scale"))

# apply to both training and testing
train_x <- predict(pre_process, train_1[, features])
test_x <- predict(pre_process, train_1[, features])

### linear regression functions
(lm.lm <- lm(Sale_Price ~ ., data = train_1))
(lm.glm <- glm(Sale_Price ~ ., data = train_1, family = gaussian))
(lm.caret <- train(Sale_Price ~ ., data = train_1, method = "lm"))

####### matrix formulation
# separate features (x) from response (y)
# get feature names
features1 <- setdiff(names(train_1), "Sale_Price")

# create feature and response set
train_x1 <- train_1[, features1]
train_y1 <- train_1$Sale_Price

# Example of matrix formulation
(glmnet.m1 <- glmnet(x = train_x1, y = train_y1))


## create var names and h2o training frame
yname <- "Sale_Price"
xname <- setdiff(names(train_1), yname)
train.h2o_1 <- as.h2o(train_1)

# variable name specification 
(h2o.ml2 <- h2o.glm(x = xname, y = yname, training_frame = train.h2o_1))


### cross validation (cv) of model
# 10 fold cv in h2o
(h2o.cv <- h2o.glm(
  x = xname, y = yname, training_frame = train.h2o_1, nfolds = 10
))

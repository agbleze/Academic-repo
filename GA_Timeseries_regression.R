## GA Regression  #########

library(forecast)
library(tidyverse)
library(readr)
library(lubridate)
library(dplyr)
library(magrittr)
GA_Dataset
View(GA_Dataset)
data_select <- select(GA_Dataset, -c("Day Index"))

year_name <- lubridate::year(data_select$Date)
month_name <- lubridate::month(data_select$Date, label = T)

data_select_sum <- data_select%>%
  mutate(Year = year_name, Month = month_name)%>%
  group_by(Year, Month) %>%
  dplyr::summarise(Total_Users = sum(Users), Tot_session_perUser = sum(`Number of Sessions per User`),
                   Total_Pg_Sessions = sum(`Pages / Session`), Total_Session = sum(Sessions),
                   Total_revenue = sum(Revenue), Total_ECR = sum(`Ecommerce Conversion Rate`),
                   Total_bounce_rate = sum(`Bounce Rate`),
                   Total_avg_sess_duration = sum(`Avg. Session Duration`))
  
data_select_sum_ts <- ts(data_select_sum, start = c(2014, 11), frequency = 12)
autoplot(data_select_sum_ts[, c(3, 4:10)])
Day_num <- monthdays(data_select_sum_ts[,2])

data_calendAdjust_ts <- data_select_sum_ts %>%
  cbind(Avg_Users = data_select_sum_ts[,3] / monthdays(data_select_sum_ts[,3]),
            Avg_sessionPer_user = data_select_sum_ts[,4] / monthdays(data_select_sum_ts[,4]),
        Avg_Pg_session = data_select_sum_ts[,5] / monthdays(data_select_sum_ts[,5]),
        Avg_session = data_select_sum_ts[, 6] / monthdays(data_select_sum_ts[,6]),
        Avg_revenue = data_select_sum_ts[,7] / monthdays(data_select_sum_ts[,7]),
        Avg_ECR = data_select_sum_ts[,8] / monthdays(data_select_sum_ts[,8]),
        Avg_bounce_rate = data_select_sum_ts[,9] / monthdays(data_select_sum_ts[,9]),
        Avg_session_duration = data_select_sum_ts[,10] / monthdays(data_select_sum_ts[,10]))

## scatter plot
#data_calendAdjust_ts%>%
  as.data.frame(data_calendAdjust_ts[, c(11, 12:18)])%>%
    ggpairs()
  
  
  
model_ts <- tslm(Avg_revenue ~ Avg_ECR, data = data_calendAdjust_ts) 
(model_ts_sumr <- summary(model_ts))
(model_ts_chkresiduals <- checkresiduals(model_ts))
(model_ts_cv <- CV(model_ts))
(model_ts_plot <- autoplot(data_calendAdjust_ts[,15], series = "Data") + 
  autolayer(fitted(model_ts), series = "fitted") + 
  ylab("Revenue") + ggtitle("Fitted vs Data --Model_ts"))


model_ts2 <- tslm(Avg_revenue ~ Avg_ECR + Avg_Users + Avg_bounce_rate + Avg_session_duration, data = data_calendAdjust_ts)
(model_ts2_sumr <- summary(model_ts2))
(model_ts2_chkresid <- checkresiduals(model_ts2))
(model_ts2_cv <- CV(model_ts2))
(model_ts2_plot <- autoplot(data_calendAdjust_ts[,15], series = "Data")) +
  autolayer(fitted(model_ts2), series = "Fitted") + 
  ylab("Revenue") + ggtitle("Fitted vs Data  -- Model_ts2")

model_tsall <- tslm(Avg_revenue ~ Avg_ECR + Avg_Users + Avg_bounce_rate + Avg_session_duration + 
                    Avg_sessionPer_user + Avg_Pg_session + Avg_session,  data = data_calendAdjust_ts)
(model_tsall_sumr <- summary(model_tsall))
(model_tsall_chkresid <- checkresiduals(model_tsall))
(model_tsall_cv <- CV(model_tsall))
(model_tsall_plot <- autoplot(data_calendAdjust_ts[,15], series = "Data")) + 
  autolayer(fitted(model_tsall), series = "Fitted") + ylab("Revenue") +
  ggtitle("Fitted vs Data -- Model_tsall")
forecast(model_tsall, h = 10)

############ log transformation
## regression model log + 1 of Avg_revenue on Avg_ECR
(model_tslog_AvgECR <- tslm(log(data_calendAdjust_ts[,15] + 1) ~ (data_calendAdjust_ts[,16]), data = data_calendAdjust_ts))
(model_tslog_AvgECR_chkresid <- checkresiduals(model_tslog))
(model_tslog_AvgECR_CV <- CV(model_tslog_AvgECR))
(model_tslog_AvgECR_sumr <- summary(model_tslog_AvgECR))
qqnorm(residuals(model_tslog)); qqline(residuals(model_tslog))

######## regression model of log + 1 Avg_revenue against all predictors
(model_tsall_log <- tslm(log(data_calendAdjust_ts[,15] + 1) ~ Avg_ECR + Avg_Users + Avg_bounce_rate + Avg_session_duration + 
                      Avg_sessionPer_user + Avg_Pg_session + Avg_session,  data = data_calendAdjust_ts))
(model_tsall_log_chkresid <- checkresiduals(model_tsall_log))
summary(model_tsall_log)
qqnorm(residuals(model_tsall_log)); qqline(residuals(model_tsall_log))
(model_tsall_log_CV <- CV(model_tsall_log))
### residual plots against fitted plot
modelplots_tsall_log <- cbind.data.frame(Fitted_tsall_log = fitted(model_tsall_log),
                               Residuals_tsall_log = residuals(model_tsall_log))
## scatterplot of residuals against fitted for model_tsall_log 
ggplot(data = modelplots_tsall_log, aes(x = Fitted_tsall_log, y = Residuals_tsall_log)) + geom_point() + 
  ggtitle("Residuals and Fitted plot of model_tsall_log")

######## regression model of log + 1 Avg_revenue against all predictors without Avg_users
(model_tsall_log_butAvgusers <- tslm(log(data_calendAdjust_ts[,15] + 1) ~ Avg_ECR + Avg_bounce_rate + Avg_session_duration + 
                           Avg_sessionPer_user + Avg_Pg_session + Avg_session,  data = data_calendAdjust_ts))
(model_tsall_log_butAvgusers_chkresid <- checkresiduals(model_tsall_log))
summary(model_tsall_log_butAvgusers)
qqnorm(residuals(model_tsall_log_butAvgusers)); qqline(residuals(model_tsall_log_butAvgusers))
(model_tsall_log_butAvgusers_CV <- CV(model_tsall_log_butAvgusers))

###

#### regression model of log + 1 Avg_revenue against all predictors without Avg_session
### Model 4
(model_tsall_log_butAvgsession <- tslm(log(data_calendAdjust_ts[,15] + 1) ~ Avg_ECR + Avg_Users + Avg_bounce_rate + Avg_session_duration +
      Avg_sessionPer_user + Avg_Pg_session, data = data_calendAdjust_ts ))
(model_tsall_log_butAvgsession_chkresid <- checkresiduals(model_tsall_log_butAvgsession))
summary(model_tsall_log_butAvgsession)
(model_tsall_log_butAvgsession_CV <- CV(model_tsall_log_butAvgsession))
qqnorm(residuals(model_tsall_log_butAvgsession)); qqline(residuals(model_tsall_log_butAvgsession), col = "red")
## plot scatterplot of residuals and fitted of model_tsall_log_butAvgsession
(modelplot_tsall_log_butAvgsession <- cbind.data.frame(Fitted = fitted(model_tsall_log_butAvgsession),
                                                      Residuals = residuals(model_tsall_log_butAvgsession)) %>%
  ggplot(aes(x = Fitted, y = Residuals)) + geom_point() + ggtitle("Residuals against Fitted of tsall_log_butAvgsession"))

#######scenario forecasting ######
###model 4########
#### Increase 0f 5%, 2.5%, 4%, 1%, 3% in ECR and decrease of 2%, 3%, 1.5%, 1%, 5%  in Bounce rate 
# and no change in other predictors
h = 5
scenario1 <- data.frame(
  Avg_ECR = c(5, 2.5, 4, 1, 3),
  Avg_bounce_rate = c(-2, -3, -1.5, -1, -5),
  Avg_Users = c(0, 0, 0, 0, 0),
  Avg_session_duration = c(0, 0, 0, 0, 0),
  Avg_sessionPer_user = c(0, 0, 0, 0, 0),
  Avg_Pg_session = c(0, 0, 0, 0, 0)
)

### use model_tsall_log_butAvgsession to forecast based on scenario 1
(forevalue <- forecast(model_tsall_log_butAvgsession, newdata = scenario1))

## convert forecast to time series
(forevalue_ts <- ts(forevalue_frame, start = c(2021, 8), frequency = 12))

### backtransform forecast values to get actual values
forevalue_frame <- data.frame(forevalue)%>%
  mutate(backtransf <- (exp(forevalue_frame$Point.Forecast - 1)))

(sce1_logall_butAvgsession <- forecast(model_tsall_log_butAvgsession, newdata = scenario1)) 
autoplot(data_calendAdjust_ts[,15], series = "Acutal Revenue") +
  autolayer(forevalue_ts[,6], series = "Forecast Revenue", PI = T) +
  guides(colour = guide_legend(title = "Scenario forecasting"))

##################  Further task ##################
#### backtransform all models with log(x + 1) to obtain the influence of predictors on avg revenue
### partition data into training and test set and develop regression model for prediction / forecasting
### check accuracy of regression model for prediction


####### App interface for GA timeseries regression #############
## Presentation of various and selecting the best based on model diagnostics
## comparing performance of models
## Scenario forecasting Buttons for user to put values for various predictors and model predicts expected impact on sales
## comparing predictions of models with actual forecast



  

# accuracy(model_tsall_log_butAvgsession)
# accuracy(model_tsall_log)
# accuracy(model_tsall_log_butAvgusers)
########linear regression models plots

## residuals plot against and fitted values
(models_fitted_resid <- cbind( Fitted_model_ts = fitted(model_ts),
       Residuals_model_ts = residuals(model_ts),
       Fitted_model_ts2 = fitted(model_ts2),
       Residuals_model_ts2 = residuals(model_ts2),
       Fitted_model_tsall = fitted(model_tsall),
       Residuals_model_tsall = residuals(model_tsall)
       ))

## scatter plots of residuals and fitted
dt <- as.data.frame(models_fitted_resid)
## some pattern are observable hence homoscedasticity is violated suggesting variance is not constant
## transformation of forecast variable may be needed
ggplot(dt, aes(x = Fitted_model_ts, y = Residuals_model_ts)) + geom_point() + geom_smooth(method = "lm")
ggplot(dt, aes(x = Fitted_model_ts2, y = Residuals_model_ts2)) + geom_point() + geom_smooth(method = "lm")
ggplot(dt, aes(x = Fitted_model_tsall, y = Residuals_model_tsall)) + geom_point() + geom_smooth(method = "lm")

## add residuals to data
tsframe <- as.data.frame(data_calendAdjust_ts)
data_model <- cbind.data.frame(tsframe, dt)
## scatter plots for residuals and predictors for the diff. models
ggplot(data_model, aes(x = Avg_ECR, y = Residuals_model_ts)) + geom_point()

ggplot(data_model, aes(x = Avg_ECR, y = Residuals_model_ts2)) + geom_point()
ggplot(data_model, aes(x = Avg_Users, y = Residuals_model_ts2)) + geom_point()
ggplot(data_model, aes(x = Avg_bounce_rate, y = Residuals_model_ts2)) + geom_point()
ggplot(data_model, aes(x = Avg_session_duration, y = Residuals_model_ts2)) + geom_point()

ggplot(data_model, aes(x = Avg_ECR, y = Residuals_model_tsall)) + geom_point()
ggplot(data_model, aes(x = Avg_Users, y = Residuals_model_tsall)) + geom_point()
ggplot(data_model, aes(x = Avg_bounce_rate, y = Residuals_model_tsall)) + geom_point()
ggplot(data_model, aes(x = Avg_session_duration, y = Residuals_model_tsall)) + geom_point()
ggplot(data_model, aes(x = Avg_sessionPer_user, y = Residuals_model_tsall)) + geom_point()
ggplot(data_model, aes(x = Avg_Pg_session, y = Residuals_model_tsall)) + geom_point()
ggplot(data_model, aes(x = Avg_session, y = Residuals_model_tsall)) + geom_point()


(reg_rev_trend_trend_season <- tslm(Avg_revenue ~ trend + season, data = data_calendAdjust_ts)) # linear reg
(fcast_regr_rev <- forecast(reg_rev_trend_trend_season))
autoplot(fcast_regr_rev) + ylab("Average monthly revenue") +
  ggtitle("Forecast based on linear regression on trend and seasonality")
checkresiduals(fcast_regr_rev)

new <- forecast(reg_rev_trend_trend_season, lambda = 0, h = 10, biasadj = T)
checkresiduals(new)

#### cubic spline reg
(spline_reg <- splinef(data_calendAdjust_ts[,15])) #%>%
  checkresiduals(spline_reg)
  autoplot(spline_reg)
summary(spline_reg)

autoplot(data_calendAdjust_ts[,15]) + geom_smooth(method = "lm")


logtrans <- log(data_calendAdjust_ts[,15] + 1)
splinef(logtrans)%>%
  checkresiduals()



View(data_model)

View(dt)
tsplot <- autoplot(data_calendAdjust_ts[, c(11, 12:18)]) 
View(data_select_sum_ts)
View(data_calendAdjust_ts)


##### non-linear regression

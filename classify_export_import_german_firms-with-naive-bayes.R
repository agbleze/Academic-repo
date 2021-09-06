library(haven)
Germany_2005_full_data_3_6_dta <- read_stata("~/Downloads/R FOR DATA ANALYSIS AND SCIENCE/Germany-2005--full data-3,6.dta.zip")
View(Germany_2005_full_data_3_6_dta)

german_enterprise_data <- as.data.frame(Germany_2005_full_data_3_6_dta)
str(german_enterprise_data)
summary(german_enterprise_data)
summary(german_enterprise_data$s7)
str(german_enterprise_data$s7)
table(factor(german_enterprise_data$s7))
barplot(table(factor(german_enterprise_data$s7)))

market_type <- factor(german_enterprise_data$s7, levels = c(2, 1))
barplot(table(market_type))
### using levels to reassign values to factors


### working with factors
levels(market_type)[1] <- "No_export"
levels(market_type)[2] <- "export"

barplot(table(market_type))




gender <- c("male", "female", "none")
gender <- factor(gender, levels = c("male", "female", "none"), ordered = TRUE)

## converting to factors
nums <- factor(c(1.2, 2.3, 3.4, 4.5))
as.numeric(levels(nums)[nums])

## converting to factors
nums <- factor(c(1.2, 2.3, 3.4, 4.5))
as.numeric(levels(nums)[nums])
table(german_enterprise_data$q61a)

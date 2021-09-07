library(haven)
library(caret) ## implementing with caret
library(h2o) 
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

## german production and 
# Q.61a Has your firm acquired new production technology over the last 36 months?
table(german_enterprise_data$q61a)
barplot(table(german_enterprise_data$q61a))

# Q.60 Has your company undertaken any of the following initiatives over the last 36 months? 
# Upgraded an existing product line Q60a2 
table(german_enterprise_data$q60a2)
table(german_enterprise_data$q60b2) ## level of importance of Q60a2 to firms

# select columns to use for the analysis
select_enterprise_data <- german_enterprise_data %>%
 dplyr::select(s7, q36a, q54a, q54b, q54c, q54d, q54e, q54f, q54g, q54h, q54i, q54j, q54k, q54l, q54m, q54n,
         q54o, q54p, q54q, q54r, q54s, q54t, q56a, q56b, q56c, q60a1, q60a2, q60a3, q60a4, q60a5, q60a6,
         q60a7, q60a8, q61a, q61b, q65a, q71a1, q71a2, q71a3) %>%
  dplyr::mutate(market_type = factor(s7, levels = c("export_market", "No_export_market")),
                business_assoc_member = factor(q36a, levels = c("Yes", "No")),
                finance_access = factor(q54a, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                finance_cost = factor(q54b, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                telecommunications = factor(q54c, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                electricity = factor(q54d, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                transportation = factor(q54e, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                land_access = factor(q54f, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                land_lease = factor(q54g, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                tax_rate = factor(q54h, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                tax_adminstration = factor(q54i, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                custom_trade_regul = factor(q54j, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                busi_lincense_permit = factor(q54k, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                labour_regul = factor(q54l, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                skills_edu_workers = factor(q54m, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                uncertainty_regul_policies = factor(q54n, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                macroeconomic_stability = factor(q54o, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                judiciary_functioning = factor(q54p, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                corruption = factor(q54q, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                streetcrime_theft_disorder = factor(q54r, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                organized_crime = factor(q54s, levels = c("No obstacle", "minor obstacle", "moderate obstacle", "major obstacle", "obstacle"), ordered = TRUE),
                )

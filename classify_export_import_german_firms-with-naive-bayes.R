library(haven)
Germany_2005_full_data_3_6_dta <- read_stata("~/Downloads/R FOR DATA ANALYSIS AND SCIENCE/Germany-2005--full data-3,6.dta.zip")
View(Germany_2005_full_data_3_6_dta)

german_enterprise_data <- as.data.frame(Germany_2005_full_data_3_6_dta)

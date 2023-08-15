

library(knitr)
# rprojroot::find_rstudio_root_file()
##setwd("/Users/martin/Documents/GitHub/ubereat-foodpanda/targets")
# source("R/support.R")

feature_change <- list()

fp <- FoodPanda()
fp$available_dates

feature_change$wax$data$before <- fp$retrieve_data("2023-02-03")
feature_change$wax$data$after <- fp$retrieve_data("2023-04-27")
feature_change$wane$data$before <- feature_change$wax$data$after
feature_change$wane$data$after <- fp$retrieve_data("2023-07-26")

saveRDS(feature_change, file = "local-data/feature_change.Rds")

feature_change <- readRDS("local-data/feature_change.Rds")
# TBC -----

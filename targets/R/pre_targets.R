

library(knitr)
# rprojroot::find_rstudio_root_file()
##setwd("/Users/martin/Documents/GitHub/ubereat-foodpanda/targets")
# source("R/support.R")

feature_change <- list()

fp <- foodDelivery::FoodPanda()
fp$available_dates

feature_change$wax$data$before <- fp$retrieve_data("2023-02-03")
feature_change$wax$data$after <- fp$retrieve_data("2023-04-27")
feature_change$wane$data$before <- feature_change$wax$data$after
feature_change$wane$data$after <- fp$retrieve_data("2023-07-26")


saveRDS(feature_change, file = "local-data/feature_change.Rds")

# popularItems <- {
#   folder_path <- "local-data/menuJson_2023-08-06/"
#   file_list <- list.files(folder_path, full.names = TRUE)
#   file_list |>
#     basename() |>
#     stringr::str_remove_all(
#       "foodpandaMenu_|.json"
#     ) -> allShopCodes
#   popularItems <- vector("list", length(file_list)) |>
#     setNames(allShopCodes)
#   for (.x in seq_along(file_list)) {
#     # .x=1
#     file_path = file_list[[.x]]
#     file_content <- xfun::read_utf8(file_path)
#     popular_items_fromJSON <-
#     tryCatch({
#       parsed_json <- jsonlite::fromJSON(file_content)
#       foodDelivery::get_popular_items_from_menuJson(parsed_json)
#     }, error = function(e) {
#       character(0)
#     })
#     popularItems[[.x]] <-popular_items_fromJSON
#   }
# }


feature_change <- readRDS("local-data/feature_change.Rds")
# TBC -----

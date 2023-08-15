library(targets)
library(econDV2)
library(foodDelivery)
library(knitr)
source("R/support.R")
# options(clustermq.scheduler = "multiprocess")
list(
  #1. import data ----
  ## that have been imported and saved earlier
  tar_target(
    feature_change,
    readRDS("local-data/feature_change.Rds")
  ),
  ##1.1 wax data (受蛋荒影響變嚴重) ----
  ### 前期----
  tar_target(
    wax_data_before,
    feature_change$wax$data$before
  ),
  ### 後期----
  tar_target(
    wax_data_after,
    feature_change$wax$data$after
  ),
  ##1.2 wane data (蛋荒影響變緩) ------
  ### 前期----
  tar_target(
    wane_data_before,
    feature_change$wax$data$after
  ),
  ### 後期----
  wane_data_after %t=% feature_change$wane$data$after,
  #2. feature summary ----
  ##2.1 data/commonShop_data/shopNumbers/commonShop_summary----
  feature_change_wax_analysis %t=% foodDelivery::analyze_feature_changes(
    wax_data_before,
    wax_data_after
  ),
  feature_change_wane_analysis %t=% foodDelivery::analyze_feature_changes(
    wane_data_before,
    wane_data_after
  ),
  ##2.2 dropout/entrance ----
  ###a. wax and wane ----
  wax_dropout_entrance %t=% compute_dropout_entrance(feature_change_wax_analysis$shopNumbers),
  wane_dropout_entrance %t=% compute_dropout_entrance(feature_change_wane_analysis$shopNumbers),
  ###b. summary ----
  summary_dropout_entrance %t=% produce_summary_dropout_entrance(
    wax_dropout_entrance, wane_dropout_entrance
  ),
  #3. Common shop summary----
  wax_featureChanges_in_commonShops %t=% feature_change_wax_analysis[["data"]][["changes"]][["commonShops_changes"]],
  wane_featureChanges_in_commonShops %t=% feature_change_wane_analysis[["data"]][["changes"]][["commonShops_changes"]],
  ##3.1 Feature changes in common shops ----
  # check why wane before/after has so many consistent change
  panel_wane %t=% {
    produce_panel(wane_data_before, wane_data_after)
  },
  panel_wax %t=% produce_panel(wax_data_before, wax_data_after),
  wane_shopfeatureHasChanged %t=% forEachShop_whichFeatureHasChanged(panel_wane),
  wax_shopfeatureHasChanged %t=% forEachShop_whichFeatureHasChanged(panel_wax),
  summarise_wane_feature_change %t=% {
     summarise_number_of_shops_withFeatureChange(wane_shopfeatureHasChanged) -> tb
     summarise_for_each_feature_number_and_prop_shops_change(tb) |>
       dplyr::arrange(dplyr::desc(number))
  },
  summarise_wax_feature_change %t=% {
    summarise_number_of_shops_withFeatureChange(wax_shopfeatureHasChanged) -> tb
    summarise_for_each_feature_number_and_prop_shops_change(tb) |>
      dplyr::arrange(dplyr::desc(number))
  },
  features2keep %t=% {
    c("menu",  "rateNum", "rate", "minDelTime",
      "distance", "minPickTime", "budget", "lon", "lat", "discount",
      "pandaOnly", "shopName", "pickup", "address", "inShopPrice",
      "category", "minFee", "township", "minOrder", "county",
      "shopTag",  "service_fee_total", "service_fee_type",
      "service_fee_setup_value")
  },
  ##3.2 Menu change ----
  ###a. 原始menu json字串 ----
  menu_wane %t=%  extract_menu_before_after( wane_data_before, wane_data_after),
  menu_wax %t=%  extract_menu_before_after( wax_data_before, wax_data_after),
  ###b. 每間店menu四面向的變化狀況
  changeInMenu_wane %t=% summarise_changeInMenu2(
    menu_wane, wane_shopfeatureHasChanged),
  changeInMenu_wax %t=% summarise_changeInMenu2(
    menu_wax, wax_shopfeatureHasChanged),
  ###c. 每間店menu四面向有無變動data frame ----
  summaryMenuChangeInEachShop_wax %t=% summarise_menuChange_eachShop(changeInMenu_wax),
  summaryMenuChangeInEachShop_wane %t=% summarise_menuChange_eachShop(changeInMenu_wane),
  ###d. menu每個面向有多少家變動data.frame ----
  summaryMenuChange_wax %t=% summarise_menuChange_fromAllShops(summaryMenuChangeInEachShop_wax),
  summaryMenuChange_wane %t=% summarise_menuChange_fromAllShops(summaryMenuChangeInEachShop_wane),
  # wax_wane menu 變動比較 ----
  comparison_summaryMenuChange %t=% {
    dplyr::bind_rows(
      summaryMenuChange_wax,
      summaryMenuChange_wane
    ) -> comparison_summary
    as.matrix(comparison_summary) -> comparison_summary
    row.names(comparison_summary) <- c("wax", "wane")
    comparison_summary
  },
  prop_anyChangeInMenu %t=% {
    comparison_summaryMenuChange[,5:6]/comparison_summaryMenuChange[,6] |>
      round(4)
  },
  prop_comparison_summaryMenuChange %t=% {
    .tb <- comparison_summaryMenuChange/comparison_summaryMenuChange[,"anyChange"]
    .tb[,-c(4,6)] |> round(4)
  },
  #4. 熱門菜單消費成本計算----
  popularItems %t=% {
    folder_path <- "local-data/menuJson_2023-08-06/"
    file_list <- list.files(folder_path, full.names = TRUE)
    popularItems <- list()
    for (file_path in file_list) {
      file_content <- xfun::read_utf8(file_path)
      tryCatch({
        parsed_json <- jsonlite::fromJSON(file_content)
        if (!is.null(parsed_json)) {
          popular_items_fromJSON <- foodDelivery::get_popular_items_from_menuJson(parsed_json)
          popularItems <- append(popularItems, list(popular_items_fromJSON))
        }
      }, error = function(e) {
        cat("Error while processing file", file_path, ":", conditionMessage(e), "\n")
      })
    }
    file_names <- list.files(folder_path, full.names = FALSE)
    file_names |>
      basename() |>
      stringr::str_remove_all(
        "foodpandaMenu_|.json"
      ) -> allShopCodes
    popularItems<-setNames(popularItems[1:56532],allShopCodes[1:56532])
  },
  ##4.1 menu cost wax  data  ----
  ### before----
  menu_cost_wax_before %t=% {
    menu_cost_wax_before <- vector("numeric", length=length(menu_wax[["before"]])) |>
      setNames(names(menu_wax$before))

    for (i in seq_along(menu_wax[["before"]])) {
      menu_cost_i <-
        tryCatch(
          {
            popularItems_i = popularItems[[names(menu_wax$before[i])]]
            compute_menu_cost(menu_wax$before[i], popularItems_i)
          },
          error= function(e){
            NA
          }
        )
      menu_cost_wax_before[[i]] <- menu_cost_i
    }
  },
  ### after----
  menu_cost_wax_after %t=% {
    menu_cost_wax_after <- vector("numeric", length=length(menu_wax[["after"]])) |>
      setNames(names(menu_wax$after))

    for (i in seq_along(menu_wax[["after"]])) {
      menu_cost_i <-
        tryCatch(
          {
            popularItems_i = popularItems[[names(menu_wax$after[i])]]
            compute_menu_cost(menu_wax$after[i], popularItems_i)
          },
          error= function(e){
            NA
          }
        )
      menu_cost_wax_after[[i]] <- menu_cost_i
    }
  },
  ##4.1 menu cost wane  data  ----
  ### before----
  menu_cost_wane_before %t=% {
    menu_cost_wane_before <- vector("numeric", length=length(menu_wane[["before"]])) |>
      setNames(names(menu_wane$before))

    for (i in seq_along(menu_wane[["before"]])) {
      menu_cost_i <-
        tryCatch(
          {
            popularItems_i = popularItems[[names(menu_wane$before[i])]]
            compute_menu_cost(menu_wane$before[i], popularItems_i)
          },
          error= function(e){
            NA
          }
        )
      menu_cost_wane_before[[i]] <- menu_cost_i
    }
  },
  ### after----
  menu_cost_wane_after %t=% {
    menu_cost_wane_after <- vector("numeric", length=length(menu_wane[["after"]])) |>
      setNames(names(menu_wane$after))

    for (i in seq_along(menu_wane[["after"]])) {
      menu_cost_i <-
        tryCatch(
          {
            popularItems_i = popularItems[[names(menu_wane$after[i])]]
            compute_menu_cost(menu_wane$after[i], popularItems_i)
          },
          error= function(e){
            NA
          }
        )
      menu_cost_wane_after[[i]] <- menu_cost_i
    }
  }
)





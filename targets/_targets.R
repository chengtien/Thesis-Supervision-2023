library(targets)
library(econDV2)
library(foodDelivery)
library(knitr)
library(dplyr)
source("R/support.R")
# options(clustermq.scheduler = "multiprocess")
list(
  # 1. import data ----
  ## that have been imported and saved earlier
  tar_target(
    feature_change,
    readRDS("local-data/feature_change.Rds")
  ),
  ## 1.1 wax data (受蛋荒影響變嚴重) ----
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
  ## 1.2 wane data (蛋荒影響變緩) ------
  ### 前期----
  tar_target(
    wane_data_before,
    feature_change$wax$data$after
  ),
  ### 後期----
  wane_data_after %t=% feature_change$wane$data$after,
  # 2. feature summary ----
  ## 2.1 data/commonShop_data/shopNumbers/commonShop_summary----
  feature_change_wax_analysis %t=% foodDelivery::analyze_feature_changes(
    wax_data_before,
    wax_data_after
  ),
  feature_change_wane_analysis %t=% foodDelivery::analyze_feature_changes(
    wane_data_before,
    wane_data_after
  ),
  ## 2.2 dropout/entrance ----
  ### a. wax and wane ----
  wax_dropout_entrance %t=% compute_dropout_entrance(feature_change_wax_analysis$shopNumbers),
  wane_dropout_entrance %t=% compute_dropout_entrance(feature_change_wane_analysis$shopNumbers),
  ### b. summary ----
  summary_dropout_entrance %t=% produce_summary_dropout_entrance(
    wax_dropout_entrance, wane_dropout_entrance
  ),
  # 3. Common shop summary----
  wax_featureChanges_in_commonShops %t=% feature_change_wax_analysis[["data"]][["changes"]][["commonShops_changes"]],
  wane_featureChanges_in_commonShops %t=% feature_change_wane_analysis[["data"]][["changes"]][["commonShops_changes"]],
  ## 3.1 Feature changes in common shops ----
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
    c(
      "menu", "rateNum", "rate", "minDelTime",
      "distance", "minPickTime", "budget", "lon", "lat", "discount",
      "pandaOnly", "shopName", "pickup", "address", "inShopPrice",
      "category", "minFee", "township", "minOrder", "county",
      "shopTag", "service_fee_total", "service_fee_type",
      "service_fee_setup_value"
    )
  },
  ## 3.2 Menu change ----
  ### a. 原始menu json字串 ----
  menu_wane %t=% extract_menu_before_after(wane_data_before, wane_data_after),
  menu_wax %t=% extract_menu_before_after(wax_data_before, wax_data_after),
  ### b. 每間店menu四面向的變化狀況
  changeInMenu_wane %t=% summarise_changeInMenu2(
    menu_wane, wane_shopfeatureHasChanged
  ),
  changeInMenu_wax %t=% summarise_changeInMenu2(
    menu_wax, wax_shopfeatureHasChanged
  ),
  ### c. 每間店menu四面向有無變動data frame ----
  summaryMenuChangeInEachShop_wax %t=% summarise_menuChange_eachShop(changeInMenu_wax),
  summaryMenuChangeInEachShop_wane %t=% summarise_menuChange_eachShop(changeInMenu_wane),
  ### d. menu每個面向有多少家變動data.frame ----
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
    comparison_summaryMenuChange[, 5:6] / comparison_summaryMenuChange[, 6] |>
      round(4)
  },
  prop_comparison_summaryMenuChange %t=% {
    .tb <- comparison_summaryMenuChange / comparison_summaryMenuChange[, "anyChange"]
    .tb[, -c(4, 6)] |> round(4)
  },
  # 4. 熱門菜單消費成本計算----
  tar_target(
    popularItemsFile, "data/popularItems.Rds",
    format = "file"
  ),
  popularItems %t=% {
    readRDS(popularItemsFile)
  },
  ## 4.1 menu cost wax  data  ----
  ### before----
  menu_cost_wax_before %t=% compute_all_menuCosts(menu_wax$before, popularItems),
  ### after----
  menu_cost_wax_after %t=% compute_all_menuCosts(menu_wax$after, popularItems),
  ## 4.1 menu cost wane  data  ----
  ### before----
  menu_cost_wane_before %t=% compute_all_menuCosts(menu_wane$before, popularItems),
  ### after----
  menu_cost_wane_after %t=% compute_all_menuCosts(menu_wane$after, popularItems),
  ## 4.2 inflation rate  ----
  ### a.原始資料----
  inflation_rate_wax %t=% calculate_inflation_rate(menu_cost_wax_before, menu_cost_wax_after),
  inflation_rate_wane %t=% calculate_inflation_rate(menu_cost_wane_before, menu_cost_wane_after),
  ### b.移除Nan.Na.Inf的資料----
  filtered_inflation_rate_wax %t=% {
    inflation_rate_wax |> dplyr::filter_all(all_vars(!is.na(.) & !is.nan(.) & !is.infinite(.)))
  },
  filtered_inflation_rate_wane %t=% {
    inflation_rate_wane |> dplyr::filter_all(all_vars(!is.na(.) & !is.nan(.) & !is.infinite(.)))
  },
  ### c.物價有上漲的店家比例以表格方式呈現----
  tar_target(
    InflationRatePercentage, data.frame(
      period = c("wax", "wane"),
      percentage = c(
        compute_percentage(filtered_inflation_rate_wax),
        compute_percentage(filtered_inflation_rate_wane)
      )
    )
  ),
  ## 4.3 店家熱門菜單的平均物價計算  ----
  average_menu_cost_wax_before %t=% compute_all_average_menuCosts(menu_wax$before, popularItems),
  average_menu_cost_wax_after %t=% compute_all_average_menuCosts(menu_wax$after, popularItems),
  average_menu_cost_wane_before %t=% compute_all_average_menuCosts(menu_wane$before, popularItems),
  average_menu_cost_wane_after %t=% compute_all_average_menuCosts(menu_wane$after, popularItems),
  ### a.平均物價上漲率----
  average_inflation_rate_wax %t=% calculate_inflation_rate(average_menu_cost_wax_before, average_menu_cost_wax_after),
  average_inflation_rate_wane %t=% calculate_inflation_rate(average_menu_cost_wane_before, average_menu_cost_wane_after),
  ### b.移除Nan.Na.Inf的資料----
  filtered_average_inflation_rate_wax %t=% {
    average_inflation_rate_wax |> filter_all(all_vars(!is.na(.) & !is.nan(.) & !is.infinite(.)))
  },
  filtered_average_inflation_rate_wane %t=% {
    average_inflation_rate_wane |> filter_all(all_vars(!is.na(.) & !is.nan(.) & !is.infinite(.)))
  },
  ### c.平均物價有上漲的店家佔總店家的比例----
  tar_target(
    AverageInflationRatePercentage, data.frame(
      period = c("wax", "wane"),
      percentage = c(
        compute_percentage(filtered_average_inflation_rate_wax),
        compute_percentage(filtered_average_inflation_rate_wane)
      )
    )
  ),
  ## 4.4 三期均有熱門商品品項----
  ### a. 各店家可追踪品項----
  persistent_popularItems %t=% construct_popularItems_common_across_threePeriods(
    menu_wax = menu_wax,
    menu_wane = menu_wane,
    popularItems = popularItems
  ),
  ### b. 各店家品項數 ----
  persistent_popularItems_count %t=% purrr::map_int(persistent_popularItems, length),
  ### c. 可追踪品項數統計 ----
  table_persistent_popularItems_count %t=% table(persistent_popularItems_count),
  ### d. 以多個面向觀察 ----
  #### mealoffering ----
  tar_target(
    mealOffering_shopCodes,
    readRDS("data/mealOffering_shopCodes.Rds")
  ),
  menu_wax_offerMeal_shopCode %t=% {
    get_mealOffering_shopCodes(wax_data_before)
  },
  #### business_hour ----
  tar_target(
    business_hour_types,
    readRDS("data/business_hour_types.Rds")
  ),

  # 5.有供餐且有穩定6項人氣商品餐廳 -----

  list_datas %t=% append(
    feature_change$wax$data,
    feature_change$wane$data
  ),
  ## 5.1 取得tracking shopCodes----
  ### a. 有供餐----
  tracking_shopCodes_mealOffering %t=%
    getTrackingShopCodes_for_mealOfferingShops(list_datas),
  ### b. 有穩定6項人氣商品----
  tracking_shopCodes_offer_6_popularItems %t=% {
    persistent_popularItems_count |>
      subset(persistent_popularItems_count == 6) |>
      names()
  },
  ### c. 交集----
  tracking_shopCodes_mealOffering_6popularItems %t=%
    intersect(
      tracking_shopCodes_mealOffering,
      tracking_shopCodes_offer_6_popularItems
    ),
  ##5.2 計算上漲率及summarise ----
  cpi_wax %t=% compute_inflationRate_logX_minus_logY(
    menu_cost_wax_before[tracking_shopCodes_mealOffering_6popularItems],
    menu_cost_wax_after[tracking_shopCodes_mealOffering_6popularItems]
  ),
  cpi_wane %t=% compute_inflationRate_logX_minus_logY(
    menu_cost_wax_after[tracking_shopCodes_mealOffering_6popularItems],
    menu_cost_wane_after[tracking_shopCodes_mealOffering_6popularItems]
  ),
  ##5.3 統計降價，不調價，漲價 ----
  summary_cpi_wax %t=% summarise_inflationRate(cpi_wax),
  summary_cpi_wane %t=% summarise_inflationRate(cpi_wane),
  tb_cpi_summary %t=% {
    tb_cpi_summary <- rbind(
      summary_cpi_wax,
      summary_cpi_wane
    )
    list(
      cpi_wax, cpi_wane
    ) |>
      purrr::map(
        mean
      ) |>
      unlist() -> mean_inflationRates

    cbind(
      tb_cpi_summary, mean_inflationRates
    )
  },
  ##5.4 穩定六項熱門商品且賣正餐餐廳 熱門商品價格分布 ----
  ### a.總價格 ----
  summary_price_wax_before %t=% summarise_price(menu_cost_wax_before[tracking_shopCodes_mealOffering_6popularItems]),
  summary_price_wax_after %t=% summarise_price(menu_cost_wax_after[tracking_shopCodes_mealOffering_6popularItems]),
  summary_price_wane_after %t=% summarise_price(menu_cost_wane_after[tracking_shopCodes_mealOffering_6popularItems]),
  tb_price_summary %t=% { rbind(
    summary_price_wax_before,
    summary_price_wax_after,
    summary_price_wane_after)|>
      cbind(mean=c(mean(menu_cost_wax_before[tracking_shopCodes_mealOffering_6popularItems]),
                  mean(menu_cost_wax_after[tracking_shopCodes_mealOffering_6popularItems]),
                  mean(menu_cost_wane_after[tracking_shopCodes_mealOffering_6popularItems])))},
  ### b.平均價格----
  summary_AvgPrice_wax_before %t=% summarise_AvgPrice(average_menu_cost_wax_before[tracking_shopCodes_mealOffering_6popularItems]),
  summary_AvgPrice_wax_after %t=% summarise_AvgPrice(average_menu_cost_wax_after[tracking_shopCodes_mealOffering_6popularItems]),
  summary_AvgPrice_wane_after %t=% summarise_AvgPrice(average_menu_cost_wane_after[tracking_shopCodes_mealOffering_6popularItems]),
  tb_AvgPrice_summary %t=% {rbind(
    summary_AvgPrice_wax_before,
    summary_AvgPrice_wax_after,
    summary_AvgPrice_wane_after)|>cbind(mean=c(mean(average_menu_cost_wax_before[tracking_shopCodes_mealOffering_6popularItems]),
                                               mean(average_menu_cost_wax_after[tracking_shopCodes_mealOffering_6popularItems]),
                                               mean(average_menu_cost_wane_after[tracking_shopCodes_mealOffering_6popularItems])))},
  ##5.4 以城市來作分類 ----
  summary_byGroup_wax_before %t=% summary_byGroup(menu_cost_wax_before,wax_data_before,tracking_shopCodes_mealOffering_6popularItems),
  summary_byGroup_wax_after %t=% summary_byGroup(menu_cost_wax_after,wax_data_after,tracking_shopCodes_mealOffering_6popularItems),
  summary_byGroup_wane_after %t=% summary_byGroup(menu_cost_wane_after,wane_data_after,tracking_shopCodes_mealOffering_6popularItems),

  tb_price_byGroup_summary %t=% data.frame(county=summary_byGroup_wax_before$county,
                                           wax_before_avgPrice=summary_byGroup_wax_before$avg_price,
                                           wax_after_avgPrice=summary_byGroup_wax_after$avg_price,
                                           wane_after_avgPrice=summary_byGroup_wane_after$avg_price)

)

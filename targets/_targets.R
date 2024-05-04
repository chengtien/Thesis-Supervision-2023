library(targets)
library(econDV2)
library(foodDelivery)
library(knitr)
library(dplyr)
library(tarchetypes)
library(tibble)
source("R/ggplot_setup.R")
source("R/support.R")
phase2_available_dates = readRDS("data/phase2_available_dates.Rds")
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
  ## 5.2 計算上漲率及summarise ----
  cpi_wax %t=% compute_inflationRate_logX_minus_logY(
    menu_cost_wax_before[tracking_shopCodes_mealOffering_6popularItems],
    menu_cost_wax_after[tracking_shopCodes_mealOffering_6popularItems]
  ),
  cpi_wane %t=% compute_inflationRate_logX_minus_logY(
    menu_cost_wax_after[tracking_shopCodes_mealOffering_6popularItems],
    menu_cost_wane_after[tracking_shopCodes_mealOffering_6popularItems]
  ),
  ## 5.3 統計降價，不調價，漲價 ----
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
  ## 5.4 穩定六項熱門商品且賣正餐餐廳 熱門商品價格分布 ----
  ### a.總價格 ----
  summary_price_wax_before %t=% summarise_price(menu_cost_wax_before[tracking_shopCodes_mealOffering_6popularItems]),
  summary_price_wax_after %t=% summarise_price(menu_cost_wax_after[tracking_shopCodes_mealOffering_6popularItems]),
  summary_price_wane_after %t=% summarise_price(menu_cost_wane_after[tracking_shopCodes_mealOffering_6popularItems]),
  tb_price_summary %t=% {
    rbind(
      summary_price_wax_before,
      summary_price_wax_after,
      summary_price_wane_after
    ) |>
      cbind(mean = c(
        mean(menu_cost_wax_before[tracking_shopCodes_mealOffering_6popularItems]),
        mean(menu_cost_wax_after[tracking_shopCodes_mealOffering_6popularItems]),
        mean(menu_cost_wane_after[tracking_shopCodes_mealOffering_6popularItems])
      ))
  },
  ### b.平均價格----
  summary_AvgPrice_wax_before %t=% summarise_AvgPrice(average_menu_cost_wax_before[tracking_shopCodes_mealOffering_6popularItems]),
  summary_AvgPrice_wax_after %t=% summarise_AvgPrice(average_menu_cost_wax_after[tracking_shopCodes_mealOffering_6popularItems]),
  summary_AvgPrice_wane_after %t=% summarise_AvgPrice(average_menu_cost_wane_after[tracking_shopCodes_mealOffering_6popularItems]),
  tb_AvgPrice_summary %t=% {
    rbind(
      summary_AvgPrice_wax_before,
      summary_AvgPrice_wax_after,
      summary_AvgPrice_wane_after
    ) |> cbind(mean = c(
      mean(average_menu_cost_wax_before[tracking_shopCodes_mealOffering_6popularItems]),
      mean(average_menu_cost_wax_after[tracking_shopCodes_mealOffering_6popularItems]),
      mean(average_menu_cost_wane_after[tracking_shopCodes_mealOffering_6popularItems])
    ))
  },
  ## 5.5 以城市來作分類之CPI ----
  average_inflation_rate_wax_mealoffering_6 %t=% {
    average_inflation_rate_wax |>
      dplyr::filter(shopCode %in% tracking_shopCodes_mealOffering_6popularItems)
  },
  average_inflation_rate_wane_mealoffering_6 %t=% {
    average_inflation_rate_wane |>
      dplyr::filter(shopCode %in% tracking_shopCodes_mealOffering_6popularItems)
  },
  shopcodeCounty %t=% {
    wax_data_before |>
      select(shopCode, county) |>
      dplyr::filter(shopCode %in% tracking_shopCodes_mealOffering_6popularItems)
  },
  CPI_byGroup_wax %t=% {
    average_inflation_rate_wax_mealoffering_6 |>
      left_join(shopcodeCounty, by = "shopCode") |>
      mutate(county = ifelse(shopCode == "y1ew", "新北市", county))
  },
  CPI_byGroup_wane %t=% {
    average_inflation_rate_wane_mealoffering_6 |>
      left_join(shopcodeCounty, by = "shopCode") |>
      mutate(county = ifelse(shopCode == "y1ew", "新北市", county))
  },
  CPI_summary_byGroup_wax %t=% {
    CPI_byGroup_wax |>
      group_by(county) |>
      summarise(mean_inflationRate = mean(inflation_rate))
  },
  CPI_summary_byGroup_wane %t=% {
    CPI_byGroup_wane |>
      group_by(county) |>
      summarise(mean_inflationRate = mean(inflation_rate))
  },
  ## Rate number analysis ----
  list_focused_dataFrame %t=% purrr::map(
  list(
    wax_data_before,
    wax_data_after,
    wane_data_after
  ),
  function(.x) focus_dataFrame_on(.x, tracking_shopCodes_mealOffering_6popularItems)
  ),
  merged_focused_dataFrame %t=% {
  list_focused_dataFrame |>
    purrr::reduce(
      function(.acc, .x) dplyr::left_join(.acc, .x, by = "shopCode")
    ) -> df
  names(df)[-1] <- c("m02", "m04", "m07")
  df
  },
  rateNum_dataFrame_long %t=% {
  merged_focused_dataFrame |>
    tidyr::pivot_longer(
      cols = -1,
      names_to = "time",
      values_to = "rateNum"
    )
    },
  dRateNum_dataFrame %t=% {
  rateNum_dataFrame_long |>
    dplyr::group_by(shopCode) |>
    dplyr::mutate(
      dRateNum = rateNum - dplyr::lag(rateNum)
    ) |>
    na.omit()},
  # 6. 餐飲分類 ----
  shop_cat_offeringMeal %t=% create_factor_offeringMeals(wax_data_before),
  shop_cat_regional %t=% create_region_nonRegion_category(wax_data_before),
  shop_cat_noRegional %t=% create_factors_for_nonRegionalCategories(shop_cat_regional),
  ## 6.1 區域美食/非區域分類 ----
  wax_data_before_cat %t=% create_region_nonRegion_category(wax_data_before),
  wax_data_after_cat %t=% create_region_nonRegion_category(wax_data_after),
  wane_data_before_cat %t=% create_region_nonRegion_category(wane_data_before),
  wane_data_after_cat %t=% create_region_nonRegion_category(wane_data_after),
  ## 6.2 區域分類統計 ----
  tb_region_wax_before %t=% table_regionCategory(wax_data_before_cat),
  tb_region_wax_after %t=% table_regionCategory(wax_data_after_cat),
  tb_region_wane_before %t=% table_regionCategory(wane_data_before_cat),
  tb_region_wane_after %t=% table_regionCategory(wane_data_before_cat),
  summary_byCatRegion %t=% list(
    tb_region_wax_before,
    tb_region_wax_after,
    tb_region_wane_after),
  summary_wide_table_foodRegions %t=% {
    summary_byCatRegion |> create_wide_table_regionCat() -> tb_count
    summary_byCatRegion |> create_wide_table_regionCat(type="proportionTable") -> tb_prop
    list(
      count = tb_count,
      proportion = tb_prop)},
  ## 7.供餐時段 ----
  tar_target(
    scheduleFile, "data/schedules.Rds", format="file"),
  ##7.1 原始資料 ----
  schedules %t=% readRDS(scheduleFile),
  get_businessHourTypes %t=% construct_get_businessHourTypes(),
  tar_target(
    business_hour_typesFile, "data/business_hour_types.Rds", format="file"
    ),
  ##7.2 各店家七類營業時段 ----
  bhours %t=% readRDS(business_hour_typesFile),
  # jsonlite::fromJSON("local-data/business_hours.json"),
  ###a. data frame格式 ----
  business_wday_time %t=% get_dataFrame_business_day_time(bhours),
  ##7.3 七類營業時段統計 ----
  summary_competition_by_businessHours %t=% {
    summarise_competition_by_businessHours(business_wday_time)
    },
  tar_target(shop0806_path, "local-data/shop0806.Rds", format = 'file'),
  tar_target(taiwan_township_sf_path,"local-data/taiwan_township.Rds", format = 'file'),
  # 參考期資料 ----
  shop0806 %t=% readRDS(shop0806_path),
  taiwan_township_sf %t=% readRDS(taiwan_township_sf_path),
  # 熱門菜單----
  tar_target(popularItemsOnMenus_path, "data/popularItemsOnMenus.Rds",format = "file"),
  popularItemsOnMenus %t=% readRDS(popularItemsOnMenus_path),
  tar_target(popularItemsOnMenus_path2, "data/popularItemsOnMenus2.Rds",format = "file"),
  popularItemsOnMenus2 %t=% readRDS(popularItemsOnMenus_path2),
  # 各期各店家人氣消費成本 ----
  #  count: 參考期的6項人氣有供應幾項
  okMenus_df2 %t=% {
    popularItemsOnMenus2 |>
      purrr::keep(~{is.null(.x$error)}) |>
      purrr::map(~{.x$result}) -> result
    names(result) |>
      purrr::map_dfr(
        ~{
          convert_okMenus2_to_data_frame2(result[[.x]]) -> dfX
          dfX$date <- lubridate::ymd(.x)
          dfX}
        )},
  okMenus_df %t=% {
    popularItemsOnMenus |>
      purrr::keep(~{is.null(.x$error)}) |>
      purrr::map(~{.x$result}) |>
      convert_okMenus_to_data_frame()
    },
  plot_popularItemsOffer %t=% plot_variation_popularItems(okMenus_df),
  okMenus_6count_df %t=% filter_6count_okMenus(okMenus_df),
  plot_popularItemsOffer6_continuity %t=%
    plot_6countPopularItem_continuity(okMenus_6count_df),
  ### 各期用來計算forward inflation rate的店家 -----
  sampled_shops_each_date %t=% get_sampled_shops_each_date(okMenus_6count_df),
  summary_sampleSize_by_dates %t=% get_summary_sampleSize_by_dates(okMenus_df),
  summary_validSampleSize_by_dates %t=% get_summary_validSampleSize_by_dates(sampled_shops_each_date, summary_sampleSize_by_dates),
  okMenus_6count_df_valid %t=% {
    okMenus_6count_df |>
      dplyr::filter(
        nextCount == 6
        )
    },
  # 權重準備 ----
  ## 參考期特徵分配 ----
  shop0806_county %t=% {
    shop0806 |>
      dplyr::select(
        shopCode, county) |>
      dplyr::filter(
        !(county %in% c("",'金門縣','澎湖縣'))
        ) |>
      na.omit() |>
      dplyr::mutate(
        county = factor(county)
        )},
  ## 各期店家特徵分配 ----
  menu_cost %t=% {
    1:nrow(summary_validSampleSize_by_dates) |>
      purrr::map(
        ~{
          currentDate <- summary_validSampleSize_by_dates$date[[.x]]
          nextDate <- summary_validSampleSize_by_dates$leadDate[[.x]]
          cost_data_for_one_period <- construct_cost_data_for_one_period(okMenus_6count_df_valid, currentDate, nextDate, okMenus_df2)
          cost_data_for_one_period
        }
      ) |>
      setNames(summary_validSampleSize_by_dates$date) -> result


    result |>
      purrr::map(
        ~{
          .x |>
            dplyr::left_join(
              shop0806_county,
              by="shopCode"
            ) |>
            dplyr::filter(
              inflation < 25,
              inflation > -25,
              !is.na(inflation)
            )
        })
  },
  ## 各期含權重資料 -----
  menu_cost_weights %t=% {
    menu_cost |>
      purrr::map(
        ~{
          .x$county |>
            table() |>
            prop.table() -> sample_distX

          weightDF <- data.frame(
            county= names(sample_distX),
            weight = as.numeric(pop_distribution/sample_distX)
          )

          .x |>
            dplyr::left_join(
              weightDF,
              by="county"
            )
        }
      )
  },
  ## 各期物價上漲率 -----
  summary_inflation %t=% {
    purrr::map_dfr(
      menu_cost_weights,
      ~{
        .x |>
          na.omit() |>
          dplyr::summarise(
            inflation = weighted.mean(inflation, weight, na.rm=T)
          )
      }
    )-> df
    df$date = names(menu_cost_weights)
    df |>
      dplyr::relocate(date)  |>
      dplyr::mutate(
        days = {
          ds = lubridate::ymd(date)
          as.numeric(
            dplyr::lead(ds)-ds
          )
        },
        inflation_byDay = {
          inflation/days
        },
        inflation_annualise = {
          inflation_byDay*365
        }
      )
  },
  # 其他 ----
  pop_distribution %t=% {
    shop0806_county$county |>
      table() |>
      prop.table()
  },

  extract_menu_and_available_popular_items %t=%
    Construct_extract_menu_and_available_popular_items(popularItems),
  # distribution ----
  ## by city
  dist_by_city %t=% {
    shop0806 %>%
      group_by(city) |>
      summarise(
        n = n()/nrow(shop0806) *100
      ) |>
      ungroup() |>
      arrange(
        desc(n)
      )
  },
  #6. 營業時段 ----

  ##6.1 原始資料 ----



  ##6.2 各店家七類營業時段 ----

  # jsonlite::fromJSON("local-data/business_hours.json"),
  ###a. data frame格式 ----

  ##6.3 七類營業時段統計 ----

  ##6.4 只含有供餐 ----
  MealBusiness_wday_time %t=% {
    get_dataFrame_business_day_time(bhours[mealOffering_shopCodes])
  },
  summary_meal_competition_by_businessHours %t=% {
    summarise_competition_by_businessHours(MealBusiness_wday_time)
  },
  # Thesis All Data----
  tar_target(
    combinedokMenu, "data/combinedokMenu.Rds", format="file"
  ),
  tar_target(
    okMenus_6count_df_combined, "data/okMenus_6count_df_combined.Rds", format="file"
  ),
  tar_target(
    sampled_shops_each_date_combined, "data/sampled_shops_each_date_combined.Rds", format="file"
  ),
  tar_target(
    summary_sampleSize_by_dates_combined, "data/summary_sampleSize_by_dates_combined.Rds", format="file"
  ),
  tar_target(
    summary_validSampleSize_by_dates_combined, "data/summary_validSampleSize_by_dates_combined.Rds", format="file"
  ),
  tar_target(
    okMenus_6count_df_valid_combined, "data/okMenus_6count_df_valid_combined.Rds", format="file"
  ),
  tar_target(
    menu_cost_mealoffering, "data/menu_cost_mealoffering.Rds", format="file"
  ),
  tar_target(
    inflation_Mealoffering, "data/inflation_Mealoffering.Rds", format="file"
  ),
  tar_target(menu_cost_mealoffering8,
             readRDS("data/menu_cost_mealoffering8.Rds"))
  )



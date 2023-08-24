
calculate_inflation_rate <- function(menu_cost_before, menu_cost_after) {
  shopcode<-intersect(names(menu_cost_before),names(menu_cost_after))
  # 從 menu_cost_before 中選擇符合 shopcode 的部分
  selected_menu_cost_before <- menu_cost_before[names(menu_cost_before) %in% shopcode]
  # 從 menu_cost_after 中選擇符合 shopcode 的部分
  selected_menu_cost_after <- menu_cost_after[names(menu_cost_after) %in% shopcode]
  # 計算物價上漲率
  inflation_rate <- log(selected_menu_cost_after) - log(selected_menu_cost_before)
  # 將結果儲存在dataframe中
  result <- data.frame(shopCode = names(inflation_rate), inflation_rate)
  return(result)
}

# 計算結果
inflation_rate_wax<-calculate_inflation_rate(menu_cost_wax_before, menu_cost_wax_after)
inflation_rate_wane<-calculate_inflation_rate(menu_cost_wane_before, menu_cost_wane_after)



#把Nan.Na.Inf的資料拿掉
filtered_inflation_rate_wax <-inflation_rate_wax|>filter_all(all_vars(!is.na(.) & !is.nan(.) & !is.infinite(.)))
filtered_inflation_rate_wane <-inflation_rate_wane|>filter_all(all_vars(!is.na(.) & !is.nan(.) & !is.infinite(.)))


#計算價格有上漲店家佔總店家的比例
compute_percentage<-function(filter_inflation_rate){
  filter_inflation_rate|>dplyr::filter(inflation_rate != 0)|> nrow() -> numberOfShopInflation
  nrow(filter_inflation_rate)->total
  (numberOfShopInflation/total) -> percentage
  return(percentage)
}

compute_percentage(filtered_inflation_rate_wax)
compute_percentage(filtered_inflation_rate_wane)
#以dataframe呈現
InflationRatePercentage<-data.frame(period=c("wax","wane"),
                                    percentage=c(compute_percentage(filtered_inflation_rate_wax),compute_percentage(filtered_inflation_rate_wane)))



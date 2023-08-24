compute_average_menu_cost <- function(menu, PopularItems) {
  menu|> jsonlite::fromJSON()->shop
  shopCode <- names(menu)
  product <- shop$product
  price <- shop$discountedPrice
  names(price) <- product

  mean(price[PopularItems], na.rm=T)
}



compute_all_average_menuCosts <- function(menu_wane_before, popularItems) {
  average_menu_cost_wane_before <- vector("numeric", length=length(menu_wane_before)) |>
    setNames(names(menu_wane_before))

  for (i in seq_along(menu_wane_before)) {
    menu_cost_i <-
      tryCatch(
        {
          popularItems_i = popularItems[[names(menu_wane_before[i])]]
          compute_average_menu_cost(menu_wane_before[[i]], popularItems_i)
        },
        error= function(e){
          NA
        }
      )
    average_menu_cost_wane_before[[i]] <- menu_cost_i
  }
  average_menu_cost_wane_before
}

average_menu_cost_wax_before<-compute_all_average_menuCosts(menu_wax$before, popularItems)
average_menu_cost_wax_after<-compute_all_average_menuCosts(menu_wax$after, popularItems)
average_menu_cost_wane_before<-compute_all_average_menuCosts(menu_wane$before, popularItems)
average_menu_cost_wane_after<-compute_all_average_menuCosts(menu_wane$after, popularItems)


#計算平均物價上漲率
average_inflation_rate_wax<-calculate_inflation_rate(average_menu_cost_wax_before,average_menu_cost_wax_after)
average_inflation_rate_wane<-calculate_inflation_rate(average_menu_cost_wane_before,average_menu_cost_wane_after)

#把Nan.Na.Inf拿掉
filtered_average_inflation_rate_wax <-average_inflation_rate_wax|>filter_all(all_vars(!is.na(.) & !is.nan(.) & !is.infinite(.)))
filtered_average_inflation_rate_wane <-average_inflation_rate_wane|>filter_all(all_vars(!is.na(.) & !is.nan(.) & !is.infinite(.)))
#計算平均價格有上漲店家佔總店家的比例
AverageInflationRatePercentage<-data.frame(period=c("wax","wane"),
                                    percentage=c(compute_percentage(filtered_average_inflation_rate_wax),compute_percentage(filtered_average_inflation_rate_wane)))

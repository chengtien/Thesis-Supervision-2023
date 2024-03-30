purrr::map_dfr(
  menu_cost_weights,
  ~{
    .x |>
      na.omit() |>
      dplyr::summarise(
        inflation =mean(inflation,na.rm=T)
      )
  }
)-> df50
df50$date = names(menu_cost_weights)
df50 |>
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
  )->Ddf0



#更早期資料
phase2_available_dates1 <- fp2$available_dates
extract_intersect_popular_items <- function(.x) {
  dataX <- fp2$retrieve_data(.x)
  menu <- extract_menu(dataX)
  return(intersect_popularItems(menu, popularItems)
  )
}
extract_menu <- function(dataX) {
  menu <- dataX$menu |> foodDelivery::fix_jsonChar()
  names(menu) <- dataX$shopCode
  return(menu)
}

intersect(menu0203[[1]],popularItems[[1]])
modified_data_list3[[1]]->df02032
extract_menu0203<-extract_menu(df02032)
extract_menu0203[[1]]
fromJSON(extract_menu0203[[1]])->json0203
intersect(json0203$product,popularItems[[1]])

#建立熱門菜單
intersect(modified_data_list3[[18]]$shopCode,names(popularItems))->periodShopCode
modified_data_list3[[18]]|>dplyr::filter(shopCode %in% periodShopCode)->existBasePeriod


menu_list4<-list()
extract_menu0710<-extract_menu(existBasePeriod)
fromJSON(extract_menu0203[[shopcode]])->shopMenu
intersect(shopMenu$product,popularItems$shopcode)->menu_list[[i]]

popularItems[names(popularItems) %in% existBasePeriod]->filterPopularItems



menu_list <- list()
existBasePeriod$shopCode->shopcode1
# 遍歷 existBasePeriod 中的每個 shopcode
for (i in 1:length(periodShopCode)) {
  # 提取 menu
  extract_menu0203 <- extract_menu(existBasePeriod)
  fromJSON(extract_menu0203[[i]]) -> shopMenu

  # 與 popularItems 的 shopcode 進行交集
  intersect(shopMenu$product, filterPopulatItems[[i]]) -> menu_list4[[i]]
}

#0309

menu_list5 <- list()


intersect(modified_data_list3[[2]]$shopCode,names(popularItems))->periodShopCode
modified_data_list3[[2]]|>dplyr::filter(shopCode %in% periodShopCode)->existBasePeriod
popularItems[names(popularItems) %in% periodShopCode]->filterPopularItems

extract_menu0210 <- extract_menu(existBasePeriod)
menu_list5<-list()


#有NA版本
for (i in 1:length(periodShopCode)) {
  # 檢查 extract_menu0210 是否為 NULL 或 NA
  if (is.null(extract_menu0701[[i]]) || is.na(extract_menu0701[[i]])) {
    # 如果是，直接將 menu_list5[[i]] 設為 NA
    menu_list5[[i]] <- list(item = NA, cost = NA)
    next  # 繼續下一個循環
  }

  # 提取 menu
  shopMenu <- fromJSON(extract_menu0701[[i]])

  # 與 popularItems 的 shopcode 進行交集
  item <- intersect(shopMenu$product, filterPopularItems[[i]])

  # 在 shopMenu$product 中找到 item 的位置
  item_positions <- match(item, shopMenu$product)

  # 檢查 item_positions 是否包含缺失值
  if (!any(is.na(item_positions)) && all(!is.na(item_positions))) {
    # 根據位置提取對應的 discountedPrice
    discountedPrice <- shopMenu$discount[item_positions]

    # 檢查 discountedPrice 是否包含缺失值
    if (!any(is.na(discountedPrice)) && all(!is.na(discountedPrice))) {
      # 將字符型別轉換為數值型別，然後進行求和
      discountedPrice_numeric <- as.numeric(discountedPrice)

      # 確保 menu_list5 的每個元素都是 list
      menu_list5[[i]] <- list(item = item, cost = sum(discountedPrice_numeric, na.rm = TRUE))
    } else {
      # 如果含有缺失值，則將 cost 設為 NA
      menu_list5[[i]] <- list(item = item, cost = NA)
    }
  } else {
    # 如果 item_positions 含有缺失值，則將 cost 設為 NA
    menu_list5[[i]] <- list(item = item, cost = NA)
  }
}


#0701特殊狀況
intersect(data_list4[[3]]$shopCode,names(popularItems))->periodShopCode

data_list4[[3]]|>dplyr::filter(shopCode %in% periodShopCode)->existBasePeriod

extract_menu1008<-extract_menu(existBasePeriod)
menu_list9<-list()
for (i in 1:length(periodShopCode)) {
  tryCatch(
    {
      # 提取 menu
      shopMenu <- fromJSON(extract_menu1008[[i]])

      # 與 popularItems 的 shopcode 進行交集
      item <- intersect(shopMenu$product, filterPopularItems[[i]])

      # 在 shopMenu$product 中找到 item 的位置
      item_positions <- match(item, shopMenu$product)

      # 檢查 item_positions 是否包含缺失值
      if (!any(is.na(item_positions)) && all(!is.na(item_positions))) {
        # 根據位置提取對應的 discountedPrice
        discountedPrice <- shopMenu$discount[item_positions]

        # 檢查 discountedPrice 是否包含缺失值
        if (!any(is.na(discountedPrice)) && all(!is.na(discountedPrice))) {
          # 將字符型別轉換為數值型別，然後進行求和
          discountedPrice_numeric <- as.numeric(discountedPrice)

          # 確保 menu_list5 的每個元素都是 list
          menu_list9[[i]] <- list(item = item, cost = sum(discountedPrice_numeric, na.rm = TRUE))
        } else {
          # 如果含有缺失值，則將 cost 設為 NA
          menu_list9[[i]] <- list(item = item, cost = NA)
        }
      } else {
        # 如果 item_positions 含有缺失值，則將 cost 設為 NA
        menu_list9[[i]] <- list(item = item, cost = NA)
      }
    },
    error = function(e) {
      # 發生錯誤時將 menu_list5[[i]] 設為 NA
      menu_list9[[i]] <- list(item = NA, cost = NA)
    }
  )
}
names(menu_list9)<-existBasePeriod$shopCode
popularItemsOnMenus4[[2]]<-menu_list9

#更新版
extract_menu1025 <- lapply(extract_menu1025, function(json_text) {
  gsub("'", '"', json_text)
})


for (i in 1:length(periodShopCode)) {
  tryCatch(
    {
      # 提取 menu
      shopMenu <- fromJSON(extract_menu1025[[i]])

      # 與 popularItems 的 shopcode 進行交集
      item <- intersect(shopMenu$product, filterPopularItems[[i]])

      # 在 shopMenu$product 中找到 item 的位置
      item_positions <- match(item, shopMenu$product)

      # 檢查 item_positions 是否包含缺失值
      if (!any(is.na(item_positions)) && all(!is.na(item_positions))) {
        # 根據位置提取對應的 discountedPrice
        discountedPrice <- shopMenu$variations$discountedPrice[item_positions]

        # 檢查 discountedPrice 是否包含缺失值
        if (!any(is.na(discountedPrice)) && all(!is.na(discountedPrice))) {
          # 將字符型別轉換為數值型別，然後進行求和
          discountedPrice_numeric <- as.numeric(discountedPrice)

          # 確保 menu_list5 的每個元素都是 list
          menu_list9[[i]] <- list(item = item, cost = sum(discountedPrice_numeric, na.rm = TRUE))
        } else {
          # 如果含有缺失值，則將 cost 設為 NA
          menu_list9[[i]] <- list(item = item, cost = NA)
        }
      } else {
        # 如果 item_positions 含有缺失值，則將 cost 設為 NA
        menu_list9[[i]] <- list(item = item, cost = NA)
      }
    },
    error = function(e) {
      # 發生錯誤時將 menu_list5[[i]] 設為 NA
      menu_list9[[i]] <- list(item = NA, cost = NA)
    }
  )
}


names(menu_list5)<-existBasePeriod$shopCode




popularItemsOnMenus1<-list()
popularItemsOnMenus1[[1]]<-menu_list6
popularItemsOnMenus1[[17]]<-menu_list5




#我的function
extract_menu_cost <- function(data_list) {
  intersect(data_list$shopCode, names(popularItems)) -> periodShopCode
  data_list |> dplyr::filter(shopCode %in% periodShopCode) -> existBasePeriod
  popularItems[names(popularItems) %in% periodShopCode] -> filterPopularItems

  extract_menu_date <- extract_menu(existBasePeriod)
  menu_list <- list()

  for (i in 1:length(periodShopCode)) {
    if (is.null(extract_menu_date[[i]]) || is.na(extract_menu_date[[i]])) {
      menu_list[[i]] <- list(item = NA, cost = NA)
      next
    }

    shopMenu <- fromJSON(extract_menu_date[[i]])
    item <- intersect(shopMenu$product, filterPopularItems[[i]])
    item_positions <- match(item, shopMenu$product)

    if (!any(is.na(item_positions)) && all(!is.na(item_positions))) {
      discountedPrice <- shopMenu$discount[item_positions]

      if (!any(is.na(discountedPrice)) && all(!is.na(discountedPrice))) {
        discountedPrice_numeric <- as.numeric(discountedPrice)
        menu_list[[i]] <- list(item = item, cost = sum(discountedPrice_numeric, na.rm = TRUE))
      } else {
        menu_list[[i]] <- list(item = item, cost = NA)
      }
    } else {
      menu_list[[i]] <- list(item = item, cost = NA)
    }
  }
  names(menu_list)<-periodShopCode
  return(menu_list)
}


menu_list8<-extract_menu_cost(modified_data_list3[[4]])

popularItemsOnMenus1[[4]]<-menu_list8
popularItemsOnMenus1[[5]]<-extract_menu_cost(modified_data_list3[[5]])
popularItemsOnMenus1[[6]]<-extract_menu_cost(modified_data_list3[[6]])
popularItemsOnMenus1[[7]]<-extract_menu_cost(modified_data_list3[[7]])
popularItemsOnMenus1[[8]]<-extract_menu_cost(modified_data_list3[[8]])
popularItemsOnMenus1[[9]]<-extract_menu_cost(modified_data_list3[[9]])
popularItemsOnMenus1[[9]]<-extract_menu_cost(modified_data_list3[[10]])
popularItemsOnMenus1[[10]]<-extract_menu_cost(modified_data_list3[[11]])
popularItemsOnMenus1[[11]]<-extract_menu_cost(modified_data_list3[[12]])
popularItemsOnMenus1[[12]]<-extract_menu_cost(modified_data_list3[[13]])
popularItemsOnMenus1[[13]]<-extract_menu_cost(modified_data_list3[[14]])
popularItemsOnMenus1[[14]]<-extract_menu_cost(modified_data_list3[[15]])
popularItemsOnMenus1[[15]]<-extract_menu_cost(modified_data_list3[[16]])
popularItemsOnMenus1[[16]]<-extract_menu_cost(modified_data_list3[[17]])
popularItemsOnMenus1[[17]]<-extract_menu_cost(modified_data_list3[[18]])


names(popularItemsOnMenus1)<-date1

saveRDS(popularItemsOnMenus1, file = "data/popularItemsOnMenus1.Rds")


#7到12月的資料
popularItemsOnMenus4<-list()
popularItemsOnMenus4[[1]]<-extract_menu_cost(data_list4[[1]])
popularItemsOnMenus4[[2]]<-extract_menu_cost(data_list4[[2]])




#1025後的資料要這樣做
intersect(data_list4[[2]]$shopCode,names(popularItems))->periodShopCode

data_list4[[2]]|>dplyr::filter(shopCode %in% periodShopCode)->existBasePeriod
popularItems[names(popularItems) %in% periodShopCode]->filterPopularItems

extract_menu1025<-extract_menu(existBasePeriod)
extract_menu1025 <- lapply(extract_menu1025, function(json_text) {
  gsub("'", '"', json_text)
})


menu_list9<-list()



for (i in 1:length(periodShopCode)) {
  tryCatch(
    {
      # 提取 menu
      shopMenu <- fromJSON(extract_menu1025[[i]])

      # 與 popularItems 的 shopcode 進行交集
      item <- intersect(shopMenu$product, filterPopularItems[[i]])

      # 在 shopMenu$product 中找到 item 的位置
      item_positions <- match(item, shopMenu$product)

      # 檢查 item_positions 是否包含缺失值
      if (!any(is.na(item_positions)) && all(!is.na(item_positions))) {
        # 根據位置提取對應的 discountedPrice
        discountedPrice <- shopMenu$discountedPrice[item_positions]

        # 檢查 discountedPrice 是否包含缺失值
        if (!any(is.na(discountedPrice)) && all(!is.na(discountedPrice))) {
          # 將字符型別轉換為數值型別，然後進行求和
          discountedPrice_numeric <- as.numeric(discountedPrice)

          # 確保 menu_list5 的每個元素都是 list
          menu_list9[[i]] <- list(item = item, cost = sum(discountedPrice_numeric, na.rm = TRUE))
        } else {
          # 如果含有缺失值，則將 cost 設為 NA
          menu_list9[[i]] <- list(item = item, cost = NA)
        }
      } else {
        # 如果 item_positions 含有缺失值，則將 cost 設為 NA
        menu_list9[[i]] <- list(item = item, cost = NA)
      }
    },
    error = function(e) {
      # 發生錯誤時將 menu_list5[[i]] 設為 NA
      menu_list9[[i]] <- list(item = NA, cost = NA)
    }
  )
}


names(menu_list9)<-existBasePeriod$shopCode

popularItemsOnMenus4[[2]]<-menu_list9

names(popularItemsOnMenus4)<-date3

saveRDS(popularItemsOnMenus4, "data/popularItemsOnMenus3.Rds")

#把data抓回來



phase2_available_dates1 |>
  purrr::map(
    purrr::safely(extract_intersect_popular_items)
  ) -> popularItemsOnMenus2

names(popularItemsOnMenus) <- phase2_available_dates

saveRDS(popularItemsOnMenus, "data/popularItemsOnMenus.Rds")

popularItemsOnMenus |>
  purrr::keep(~{!is.null(.x$error)}) |> View()

popularItemsOnMenus<-readRDS("data/popularItemsOnMenus.Rds")
popularItemsOnMenus4<-readRDS("data/popularItemsOnMenus3.Rds")
popularItemsOnMenus1<-readRDS("data/popularItemsOnMenus1.Rds")
# popular items cost -----
popularItemsOnMenus4 |>
  purrr::keep(~{is.null(.x$error)}) |>
  purrr::map(~{.x$result}) -> result3

names(result) |>
  purrr::map_dfr(
    ~{
      convert_okMenus2_to_data_frame2_2(result[[.x]]) -> dfX
      dfX$date <- .x
      dfX
    }
  ) -> df_final

.x=1
resultX <- result[[.x]]



#當期有多少項在參考期出現
popularItemsOnMenus1 |>
  purrr::keep(~{is.null(.x$error)}) |>
  purrr::map(~{.x}) -> result3

names(result3) |>
  purrr::map_dfr(
    ~{
      convert_okMenus2_to_data_frame2_2(result3[[.x]]) -> dfX
      dfX$date <- lubridate::ymd(.x)
      dfX
    }
  )->okMenus1

popularItemsOnMenus4 |>
  purrr::keep(~{is.null(.x$error)}) |>
  purrr::map(~{.x}) -> result3

names(result3) |>
  purrr::map_dfr(
    ~{
      convert_okMenus2_to_data_frame2_2(result3[[.x]]) -> dfX
      dfX$date <- lubridate::ymd(.x)
      dfX
    }
  )->okMenus3

convert_okMenus2_to_data_frame2_2 <- function(resultX) {
  names(resultX) |>
    purrr::map_dfr(
      ~{
        shop = resultX[[.x]]
        data.frame(
          shopCode = .x,
          cost = ifelse(length(shop$item)==0,0, resultX[[.x]]$cost),
          count = length(resultX[[.x]]$item)
        )
      }
    ) -> df_result
  df_result
}
okMenus_6count_df3<-filter_6count_okMenus(okMenus3)
sampled_shops_each_date3 <- get_sampled_shops_each_date(okMenus_6count_df3)
summary_sampleSize_by_dates3 <- get_summary_sampleSize_by_dates(okMenus3)
summary_validSampleSize_by_dates3 <- get_summary_validSampleSize_by_dates(sampled_shops_each_date3, summary_sampleSize_by_dates3)

popularItemsOnMenus4 |>
  purrr::keep(~{is.null(.x$error)}) |>
  purrr::map(~{.x}) -> result3

names(result3) |>
  purrr::map_dfr(
      ~{
        convert_okMenus2_to_data_frame2_2(result3[[.x]]) -> dfX
        dfX$date <- lubridate::ymd(.x)
        dfX
      }
    )->okMenus3

convert_okMenus2_to_data_frame2_2 <- function(resultX) {
  names(resultX) |>
    purrr::map_dfr(
      ~{
        shop = resultX[[.x]]
        data.frame(
          shopCode = .x,
          cost = ifelse(length(shop$item)==0,0, resultX[[.x]]$cost),
          count = length(resultX[[.x]]$item)
        )
      }
    ) -> df_result
  df_result
}
okMenus_6count_df3<-filter_6count_okMenus(okMenus3)
sampled_shops_each_date3 <- get_sampled_shops_each_date(okMenus_6count_df3)
summary_sampleSize_by_dates3 <- get_summary_sampleSize_by_dates(okMenus3)
summary_validSampleSize_by_dates3 <- get_summary_validSampleSize_by_dates(sampled_shops_each_date3, summary_sampleSize_by_dates3)


#把三個資料框合併在一起
rbind(okMenus1,okMenus2,okMenus3)->combinedokMenu
okMenus_6count_df_combined<-filter_6count_okMenus(combinedokMenu)
sampled_shops_each_date_combined <- get_sampled_shops_each_date(okMenus_6count_df_combined)
summary_sampleSize_by_dates_combined <- get_summary_sampleSize_by_dates(combinedokMenu)
summary_validSampleSize_by_dates_combined <- get_summary_validSampleSize_by_dates(sampled_shops_each_date_combined, summary_sampleSize_by_dates_combined)

okMenus_6count_df_valid_combined<- {
  okMenus_6count_df_combined |>
    dplyr::filter(
      nextCount == 6
    )
}
#上面的function
get_sampled_shops_each_date <- function(okMenus_6count_df)
{
  okMenus_6count_df |>
    dplyr::filter(nextCount==6) |>
    dplyr::arrange(shopCode)
}

get_summary_sampleSize_by_dates <- function(okMenus_df)
{
  okMenus_df |>
    dplyr::ungroup() |>
    dplyr::group_by(shopCode) |>
    dplyr::mutate(
      leadDate = dplyr::lead(date,1)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      n_total = dplyr::n(),
      leadDate = leadDate[[1]]
    )
}

get_summary_validSampleSize_by_dates <- function(sampled_shops_each_date, summary_sampleSize_by_dates)
  {
  sampled_shops_each_date |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      n_valid = dplyr::n()
    ) |>
    dplyr::left_join(
      summary_sampleSize_by_dates,
      by="date"
    ) |>
    dplyr::mutate(
      prop = round(n_valid/n_total*100,2)
    ) |>
    dplyr::relocate(
      prop, .before=leadDate
    )
}

sampled_shops_each_date_combined |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    n_valid = dplyr::n()
  ) ->sampled_shops_each_date_combined2

summary_sampleSize_by_dates_combined|>dplyr::left_join(
  sampled_shops_each_date_combined2,
    by="date"
  ) |>
  dplyr::mutate(
    prop = round(n_valid/n_total*100,2)
  ) |>
  dplyr::relocate(
    prop, .before=leadDate
  )->summary_sampleSize_by_dates_combined2

summary_validSampleSize_by_dates_combined2<- summary_sampleSize_by_dates_combined |>
  dplyr::left_join(
    sampled_shops_each_date_combined2,
    by="date"
  ) |>
  dplyr::mutate(
    prop = round(n_valid/n_total*100,2)
  ) |>
  dplyr::relocate(
    date, n_valid, n_total, prop, .before = leadDate
  )
# #1
# menu_cost_combined <- {
#   1:nrow(summary_sampleSize_by_dates_combined) |>
#     purrr::map(
#       ~{
#         currentDate <- summary_validSampleSize_by_dates_combined$date[[.x]]
#         nextDate <- summary_validSampleSize_by_dates_combined$leadDate[[.x]]
#         cost_data_for_one_period <- construct_cost_data_for_one_period(okMenus_6count_df_valid_combined, currentDate, nextDate, combinedokMenu)
#         cost_data_for_one_period
#       }
#     ) |>
#     setNames(summary_validSampleSize_by_dates_combined$date) -> result
#
#   result |>
#     purrr::map(
#       ~{
#         .x |>
#           dplyr::left_join(
#             shop0806_county,
#             by="shopCode"
#           ) |>
#           dplyr::filter(
#             inflation < 25,
#             inflation > -25,
#             !is.na(inflation)
#           )
#       })
# }
#
# #2
# menu_cost_combined <- {
#   1:nrow(summary_validSampleSize_by_dates_combined) |>
#     purrr::map(
#       ~{
#         currentDate <- summary_validSampleSize_by_dates_combined$date[[.x]]
#         nextDate <- summary_validSampleSize_by_dates_combined$leadDate[[.x]]
#         cost_data_for_one_period <- construct_cost_data_for_one_period(okMenus_6count_df_valid_combined, currentDate, nextDate, combinedokMenu)
#         cost_data_for_one_period
#       }
#     ) |>
#     setNames(summary_validSampleSize_by_dates_combined$date) -> result
#
#   result |>
#     purrr::map(
#       ~{
#         .x |>
#           dplyr::left_join(
#             shop0806_county,
#             by="shopCode"
#           ) |>
#           dplyr::filter(
#             inflation < 25,
#             inflation > -25,
#             !is.na(inflation)
#           )
#       })
# }


construct_cost_data_for_one_period <- function(okMenus_6count_df_valid, currentDate, nextDate, okMenus_df2) {
  okMenus_6count_df_valid |>
    dplyr::filter(
      date == currentDate
    ) -> df_current

  df_current |>
    dplyr::left_join(
      okMenus_df2 |>
        dplyr::select(shopCode, date, cost),
      by=c("shopCode", "date")
    ) |>
    dplyr::select(shopCode, date, cost) -> df_current2

  okMenus_6count_df_valid |>
    dplyr::filter(
      date == nextDate
    ) -> df_next
  df_next  |>
    dplyr::left_join(
      okMenus_df2 |>
        dplyr::select(shopCode, date, cost),
      by=c("shopCode", "date")
    ) |>
    dplyr::select(shopCode, date, cost) |>
    dplyr::rename(
      "nextCost"="cost",
      "leadDate"="date"
    ) -> df_next2

  df_current2 |>
    dplyr::left_join(
      df_next2,
      by=c("shopCode")
    ) |>
    dplyr::mutate(
      inflation = round(log(nextCost)-log(cost),4)*100
    ) -> df_merge
  df_merge
}

#分段跑結果

menu_cost_combined <- {
  1:nrow(summary_validSampleSize_by_dates_combined) |>
    purrr::map(
      ~{
        currentDate <- summary_validSampleSize_by_dates_combined$date[[.x]]
        nextDate <- summary_validSampleSize_by_dates_combined$leadDate[[.x]]


        okMenus_6count_df_valid_combined |>
          dplyr::filter(
            date == currentDate
          ) -> df_current

        combinedokMenu |>
          dplyr::select(shopCode, date, cost)->menu

        df_current |>
          dplyr::left_join(menu,
            by=c("shopCode", "date")
          ) |>
          dplyr::select(shopCode, date, cost.x)|>rename("cost"="cost.x") -> df_current2


        okMenus_6count_df_valid_combined |>
          dplyr::filter(
            date == nextDate
          ) -> df_next


        df_next |>
          dplyr::left_join(
            combinedokMenu |>
              dplyr::select(shopCode, date, cost),
            by=c("shopCode", "date")
          ) |>
          dplyr::select(shopCode, date, cost.x) |>
          dplyr::rename(
            "nextCost"="cost.x",
            "leadDate"="date"
          ) -> df_next2


        df_current2 |>
          dplyr::left_join(
            df_next2,
            by=c("shopCode")
          ) |>
          dplyr::mutate(
            inflation = round(log(nextCost)-log(cost),4)*100
          ) -> df_merge
        df_merge

      }
    ) |>
    setNames(summary_validSampleSize_by_dates_combined$date) -> result

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
}

#試試第二種方法能不能出現1213的資料
menu_cost_combined2 <- {
  1:nrow(summary_validSampleSize_by_dates_combined2) |>
    purrr::map(
      ~{
        currentDate <- summary_validSampleSize_by_dates_combined2$date[[.x]]
        nextDate <- summary_validSampleSize_by_dates_combined2$leadDate[[.x]]


        okMenus_6count_df_valid_combined |>
          dplyr::filter(
            date == currentDate
          ) -> df_current

        combinedokMenu |>
          dplyr::select(shopCode, date, cost)->menu

        df_current |>
          dplyr::left_join(menu,
                           by=c("shopCode", "date")
          ) |>
          dplyr::select(shopCode, date, cost.x)|>rename("cost"="cost.x") -> df_current2


        okMenus_6count_df_valid_combined |>
          dplyr::filter(
            date == nextDate
          ) -> df_next


        df_next |>
          dplyr::left_join(
            combinedokMenu |>
              dplyr::select(shopCode, date, cost),
            by=c("shopCode", "date")
          ) |>
          dplyr::select(shopCode, date, cost.x) |>
          dplyr::rename(
            "nextCost"="cost.x",
            "leadDate"="date"
          ) -> df_next2


        df_current2 |>
          dplyr::left_join(
            df_next2,
            by=c("shopCode")
          ) |>
          dplyr::mutate(
            inflation = round(log(nextCost)-log(cost),4)*100
          ) -> df_merge
        df_merge

      }
    ) |>
    setNames(summary_validSampleSize_by_dates_combined2$date) -> result

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
}

saveRDS(menu_cost_combined,"data/menu_cost_combined.Rds")
saveRDS(combinedokMenu,"data/combinedokMenu.Rds")
saveRDS(okMenus_6count_df_combined,"data/okMenus_6count_df_combined.Rds")
saveRDS(sampled_shops_each_date_combined,"data/sampled_shops_each_date_combined.Rds")
saveRDS(summary_sampleSize_by_dates_combined,"data/summary_sampleSize_by_dates_combined.Rds")
saveRDS(summary_validSampleSize_by_dates_combined,"data/summary_validSampleSize_by_dates_combined.Rds")
saveRDS(okMenus_6count_df_valid_combined,"data/okMenus_6count_df_valid_combined.Rds")


#各期的平均物價上漲率
inflationALL<- {
  purrr::map_dfr(
    menu_cost_combined,
    ~{
      .x |>
        na.omit() |>
        dplyr::summarise(
          inflation = mean(inflation,na.rm=T)
        )
    }
  )-> df
  df$date = names(menu_cost_combined)
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
}

#0701為什麼valid那麼少
intersect(data_list3[[17]]$shopCode,names(popularItems))->periodShopCode

data_list3[[17]]|>dplyr::filter(shopCode %in% periodShopCode)->existBasePeriod

extract_menu0701<-extract_menu(existBasePeriod)
extract_menu0701 <- lapply(extract_menu0701, function(json_text) {
  gsub("'", '"', json_text)
})

menu_list9<-list()
for (i in 1:length(periodShopCode)) {
  tryCatch(
    {
      # 提取 menu
      shopMenu <- fromJSON(extract_menu0701[[i]])

      # 與 popularItems 的 shopcode 進行交集
      item <- intersect(shopMenu$product, filterPopularItems[[i]])

      # 在 shopMenu$product 中找到 item 的位置
      item_positions <- match(item, shopMenu$product)

      # 檢查 item_positions 是否包含缺失值
      if (!any(is.na(item_positions)) && all(!is.na(item_positions))) {
        # 根據位置提取對應的 discountedPrice
        discountedPrice <- shopMenu$discountedPrice[item_positions]

        # 檢查 discountedPrice 是否包含缺失值
        if (!any(is.na(discountedPrice)) && all(!is.na(discountedPrice))) {
          # 將字符型別轉換為數值型別，然後進行求和
          discountedPrice_numeric <- as.numeric(discountedPrice)

          # 確保 menu_list5 的每個元素都是 list
          menu_list9[[i]] <- list(item = item, cost = sum(discountedPrice_numeric, na.rm = TRUE))

          # 設定名稱為 extract_menu0701 的對應名稱
          names(menu_list9[[i]]) <- names(extract_menu0701[[i]])
        } else {
          # 如果含有缺失值，則將 cost 設為 NA
          menu_list9[[i]] <- list(item = item, cost = NA)
        }
      } else {
        # 如果 item_positions 含有缺失值，則將 cost 設為 NA
        menu_list9[[i]] <- list(item = item, cost = NA)
      }
    },
    error = function(e) {
      # 發生錯誤時將 menu_list5[[i]] 設為 NA
      menu_list9[[i]] <- list(item = NA, cost = NA)
    }
  )
}

#0710熱門商品供應六項的只有151間

#主計處公布外食類cpi
cpi_takeout<-data.frame(month=c("2023-02","2023-03","2023-04","2023-05","2023-06",
                                "2023-07","2023-08","2023-09","2023-10","2023-011",
                                "2023-12"),
                        inflationRate=c(0.49,0.68,0.73,0.4,0.24,0.24,0.11,
                                        0.16,0.14,0.09,0.27))|>mutate(inflation_annualise=inflationRate*12)
cpi_takeout2<-data.frame(date=inflationALL$date,
                         inflation_annualise=c(6.3875,
                                               6.3875,
                                               6.3875,
                                               6.3875,
                                               8.0065,
                                               8.0065,
                                               8.0065,
                                               8.0065,
                                               8.8817,
                                               4.7097,
                                               4.7097,
                                               4.7097,
                                               4.7097,
                                               2.92,
                                               2.92,
                                               2.8258,
                                               2.8258,
                                               2.8258,
                                               1.2952,
                                               1.2952,
                                               1.2952,
                                               1.2952,
                                               1.9467,
                                               1.9467,
                                               1.6484,
                                               1.6484,
                                               1.6484,
                                               1.6484,
                                               1.095,
                                               1.095,
                                               1.095,
                                               3.1790,
                                               3.1790)
                         )
cpi_takeout|>mutate(inflation_annualise=inflationRate*12)->cpi_takeout2




ggplot() +
  geom_line(data = na.omit(inflationALL), aes(x = date, y = inflation_annualise, group = 1, color = "foodpanda店家物價上漲率")) +
  geom_point(data = na.omit(inflationALL), aes(x = date, y = inflation_annualise, color = "foodpanda店家物價上漲率")) +
  geom_line(data = na.omit(cpi_takeout2), aes(x = date, y = inflation_annualise, group = 1, color = "主計處公布外食費CPI上漲率"), linetype = "dashed") +
  geom_point(data = na.omit(cpi_takeout2), aes(x = date, y = inflation_annualise, color = "主計處公布外食費CPI上漲率")) +
  geom_text(data = na.omit(inflationALL), aes(x = date, y = inflation_annualise, label = round(inflation_annualise, 2), color = "foodpanda店家物價上漲率"), vjust = -0.5, size = 3) +
  geom_text(data = na.omit(cpi_takeout2), aes(x = date, y = inflation_annualise, label = round(inflation_annualise, 2), color = "主計處公布外食費CPI上漲率"), vjust = -0.5, size = 3) +
  scale_color_manual(values = c("foodpanda店家物價上漲率" = "blue","主計處公布外食費CPI上漲率"="black")) +
  labs(color = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



inflation_mealoffering<- {
  purrr::map_dfr(
    menu_cost_combined,
    ~{
      .x |>
        na.omit() |>
        dplyr::summarise(
          inflation = mean(inflation,na.rm=T)
        )
    }
  )-> df
  df$date = names(menu_cost_combined)
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
}

#正餐店家
menu_cost_mealoffering <- list()

for (i in seq_along(menu_cost_combined)) {
  menu_cost_mealoffering[[i]] <- menu_cost_combined[[i]] %>%
    filter(shopCode %in% mealOffering_shopCodes)
}
names(menu_cost_mealoffering)<-names(menu_cost_combined)

inflation_Mealoffering<- {
  purrr::map_dfr(
    menu_cost_mealoffering,
    ~{
      .x |>
        na.omit() |>
        dplyr::summarise(
          inflation = mean(inflation,na.rm=T)
        )
    }
  )-> df
  df$date = names(menu_cost_mealoffering)
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
}

saveRDS(menu_cost_mealoffering,"data/menu_cost_mealoffering.Rds")
saveRDS(inflation_Mealoffering,"data/inflation_Mealoffering.Rds")
#主計處與正餐店家
ggplot() +
  geom_line(data = na.omit(cpi_takeout2), aes(x = date, y = inflation_annualise, group = 1, color = "主計處公布外食費CPI上漲率"), linetype = "dashed") +
  geom_point(data = na.omit(cpi_takeout2), aes(x = date, y = inflation_annualise, color = "主計處公布外食費CPI上漲率")) +
  geom_line(data = na.omit(inflation_Mealoffering), aes(x = date, y = inflation_annualise, group = 1, color = "foodpanda正餐店家物價上漲率"), linetype = "longdash") +
  geom_point(data = na.omit(inflation_Mealoffering), aes(x = date, y = inflation_annualise, color = "foodpanda正餐店家物價上漲率")) +
  geom_text(data = na.omit(cpi_takeout2), aes(x = date, y = inflation_annualise, label = round(inflation_annualise, 2), color = "主計處公布外食費CPI上漲率"), vjust = -0.5, size = 3) +
  geom_text(data = na.omit(inflation_Mealoffering), aes(x = date, y = inflation_annualise, label = round(inflation_annualise, 2), color = "foodpanda正餐店家物價上漲率"), vjust = -0.5, size = 3) +
  scale_color_manual(values = c("主計處公布外食費CPI上漲率" = "black", "foodpanda正餐店家物價上漲率"="blue")) +
  labs(color = "")

#三者比較
ggplot() +
  geom_line(data = na.omit(inflationALL), aes(x = date, y = inflation_annualise, group = 1, color = "foodpanda店家物價上漲率")) +
  geom_point(data = na.omit(inflationALL), aes(x = date, y = inflation_annualise, color = "foodpanda店家物價上漲率")) +
  geom_line(data = na.omit(cpi_takeout2), aes(x = date, y = inflation_annualise, group = 1, color = "主計處公布外食費CPI上漲率"), linetype = "dashed") +
  geom_point(data = na.omit(cpi_takeout2), aes(x = date, y = inflation_annualise, color = "主計處公布外食費CPI上漲率")) +
  geom_line(data = na.omit(inflation_Mealoffering), aes(x = date, y = inflation_annualise, group = 1, color = "foodpanda正餐店家物價上漲率"), linetype = "longdash") +
  geom_point(data = na.omit(inflation_Mealoffering), aes(x = date, y = inflation_annualise, color = "foodpanda正餐店家物價上漲率")) +
  geom_text(data = na.omit(cpi_takeout2), aes(x = date, y = inflation_annualise, label = round(inflation_annualise, 2), color = "主計處公布外食費CPI上漲率"), vjust = -0.5, size = 3) +
  geom_text(data = na.omit(inflationALL), aes(x = date, y = inflation_annualise, label = round(inflation_annualise, 2), color = "foodpanda店家物價上漲率"), vjust = -0.5, size = 3) +
  geom_text(data = na.omit(inflation_Mealoffering), aes(x = date, y = inflation_annualise, label = round(inflation_annualise, 2), color = "foodpanda正餐店家物價上漲率"), vjust = -0.5, size = 3) +
  scale_color_manual(values = c("主計處公布外食費CPI上漲率" = "black", "foodpanda店家物價上漲率" = "red","foodpanda正餐店家物價上漲率"="blue")) +
  labs(color = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot() +
  geom_line(data = na.omit(inflationALL), aes(x = date, y = inflation_annualise, group = 1, color = "foodpanda店家物價上漲率")) +
  geom_point(data = na.omit(inflationALL), aes(x = date, y = inflation_annualise, color = "foodpanda店家物價上漲率")) +
  geom_line(data = na.omit(cpi_takeout2), aes(x = date, y = inflation_annualise, group = 1, color = "主計處公布外食費CPI上漲率"), linetype = "dashed") +
  geom_point(data = na.omit(cpi_takeout2), aes(x = date, y = inflation_annualise, color = "主計處公布外食費CPI上漲率")) +
  geom_line(data = na.omit(inflation_Mealoffering), aes(x = date, y = inflation_annualise, group = 1, color = "foodpanda正餐店家物價上漲率"), linetype = "longdash") +
  geom_point(data = na.omit(inflation_Mealoffering), aes(x = date, y = inflation_annualise, color = "foodpanda正餐店家物價上漲率")) +
  geom_text(data = na.omit(cpi_takeout2), aes(x = date, y = inflation_annualise, label = round(inflation_annualise, 2), color = "主計處公布外食費CPI上漲率"), vjust = -0.5, size = 3) +
  geom_text(data = na.omit(inflation_Mealoffering), aes(x = date, y = inflation_annualise, label = round(inflation_annualise, 2), color = "foodpanda正餐店家物價上漲率"), vjust = -0.5, size = 3) +
  scale_color_manual(values = c("主計處公布外食費CPI上漲率" = "black", "foodpanda店家物價上漲率" = "red","foodpanda正餐店家物價上漲率"="blue")) +
  labs(color = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

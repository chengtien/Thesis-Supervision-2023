summarise_menuChange_fromAllShops <- function(summaryMenuChangeInEachShop) {
  # summaryMenuChangeInEachShop = summaryMenuChangeInEachShop_wane
  summaryMenuChangeInEachShop |>
    dplyr::ungroup() |>
    dplyr::summarise(
      dplyr::across(
        .cols = -shopCode,
        .fns = function(.x) sum(.x, na.rm = T)
      )
    ) -> .summarise
  .summarise$n <- nrow(summaryMenuChangeInEachShop)
  .summarise
}
summarise_menuChange_eachShop <- function(changeInMenu_wax) {
  changeInMenu_wax |>
    purrr::map(
      ~{
        .x$result$hasChangeIn
      }
    ) -> wax_hasChangeIn

  seq_along(wax_hasChangeIn) |>
    purrr::map_dfr(
      ~{
        tryCatch(
          {
            resultX <- wax_hasChangeIn[[.x]]
            resultX |> as.list() |>
              list2DF() -> summaryX
            shopCodeX = names(wax_hasChangeIn[.x])
            summaryX$shopCode <- shopCodeX
            summaryX
          },
          error=function(e){
            return()
          }
        )

      }
    ) |>
    dplyr::relocate(shopCode) -> summary_wax_menuChange

  summary_wax_menuChange |> dplyr::rowwise() |>
    dplyr::mutate(
      anyChange = any(dplyr::c_across(
        c("product", "preDiscountPrice", "discountedPrice")), na.rm=T)
    ) -> summary_wax_menuChange

  summary_wax_menuChange

}
compute_dropout_entrance <- function(.shopNumbers) {
  .beforeTotal <- (.shopNumbers$`only before`+.shopNumbers$`Both Dates`)
  dropOutRate =  .shopNumbers$`only before`/ .beforeTotal
  entranceRate = .shopNumbers$`only after`/ .beforeTotal
  list(
    dropout=dropOutRate,
    entranceRate=entranceRate
  )
}
produceArrayWithNames <- function(.array, .name){
  .temp <- .array
  names(.temp) <- .name
  .temp
}
summarise_changeInMenu <- function(panel_wane, wane_data_before,wane_data_after) {
  wane_shopfeatureHasChanged <- forEachShop_whichFeatureHasChanged(panel_wane)

  wane_whichHasChangeMenu <-
    which(wane_shopfeatureHasChanged$menu)

  wane_hasChangedMenu_shopCode <- wane_shopfeatureHasChanged$shopCode[wane_whichHasChangeMenu]

  menu_before <- {
    wane_data_before |>
      dplyr::filter(
        shopCode %in% wane_hasChangedMenu_shopCode
      ) |>
      dplyr::arrange(shopCode) -> .temp
    .temp$menu[wane_whichHasChangeMenu]  |>
      stringr::str_replace_all("\"","**") |>
      stringr::str_replace_all("'",'"') |>
      stringr::str_remove_all("[\\<\\>]+")
  }
  menu_after <- {
    wane_data_after  |>
      dplyr::filter(
        shopCode %in% wane_hasChangedMenu_shopCode
      ) |>
      dplyr::arrange(shopCode) -> .temp
    .temp$menu[wane_whichHasChangeMenu] |>
      stringr::str_replace_all("\"","**") |>
      stringr::str_replace_all("'",'"') |>
      stringr::str_remove_all("[\\<\\>]+")
  }
  changeInMenu <- purrr::map2(
    menu_before, menu_after, purrr::safely(summarise_menuChange)
  ) |>
    setNames(sort(wane_hasChangedMenu_shopCode))

  changeInMenu
}
summarise_changeInMenu2 <- function(.menus, .shopfeatureHasChanged) {


  whichHasChangeMenu <- which(.shopfeatureHasChanged$menu)
  shopCodeHasChangeMenu <- .shopfeatureHasChanged$shopCode[whichHasChangeMenu]

  changeInMenu <- purrr::map(
    shopCodeHasChangeMenu,
    purrr::safely(
      function(.x){
        summarise_menuChange(.menus$before[[.x]], .menus$after[[.x]])}
    )
  ) |>
    setNames(shopCodeHasChangeMenu)

}
getDifferenceCountOnFeature <- function(feature, menuX, menuY, continuedItems) {
  if(is.null(menuX[[feature]])) return(NA)
  .arrayXwithNames <- produceArrayWithNames(menuX[[feature]], menuX[["product"]])
  .arrayYwithNames <- produceArrayWithNames(menuY[[feature]], menuY[["product"]])
  differenceCounts <-
    getHowManyDifferencesInContinuedItems(.arrayXwithNames, .arrayYwithNames, continuedItems)
  differenceCounts
}
#'
#' fix_jsonChar_m <- function(mm) {
#'   mm |> stringr::str_remove_all("\"") |>
#'     stringr::str_replace("'product'",'"product"') |>
#'     stringr::str_replace("'preDiscountPrice'",'"preDiscountPrice"') |>
#'     stringr::str_replace("'discountedPrice'",'"discountedPrice"') |>
#'     stringr::str_replace("'description'",'"description"') |>
#'     stringr::str_replace_all("''",'""') |>
#'     stringr::str_replace_all(
#'       c("\\['"='\\[\\"',
#'         "'\\]"='\\"\\]',
#'         "', '"='", "',
#'         "', \""='", "',
#'         "\", '"='", "')
#'     ) |>
#'     stringr::str_remove_all('\\\\')
#' }
#' #' Fix menu json string error in old data
#' #'
#' #' @param mm a string of menu json
#' #'
#' #' @return a string of correct json
#' #' @export
#' fix_jsonChar <- function(mm){
#'   mm |>
#'     stringr::str_replace_all("\"","'") |>
#'     stringr::str_replace_all("\\\\","\\\\\\\\") |>
#'     stringr::str_replace_all(
#'       "(?<=\\{|\\:[:blank:]\\[|\\,[:blank:]|\\]\\,[blank]|\\]\\,[:blank:])'|'(?=\\:[:blank:]\\[|\\,[:blank:]|\\]\\,[:blank:]|\\]\\})",
#'       "\"")
#' }
extract_menu_before_after <- function(data_before, data_after) {
  menu_after <- data_after$menu |> foodDelivery::fix_jsonChar()
  names(menu_after) <- data_after$shopCode
  menu_before <- data_before$menu |> foodDelivery::fix_jsonChar()
  names(menu_before) <- data_before$shopCode
  list(
    before=menu_before,
    after=menu_after
  )
}

getHowManyDifferencesInContinuedItems <- function(.arrayX, .arrayY, continuedItems)
{
  arrayX=.arrayX[continuedItems]; arrayY=.arrayY[continuedItems]
  sum(arrayX != arrayY[names(arrayX)], na.rm = T)
}
summarise_menuChange <- function(.menuX, .menuY) {
  jsonlite::fromJSON(.menuX, simplifyDataFrame = F) -> menuX
  jsonlite::fromJSON(.menuY, simplifyDataFrame = F) -> menuY
  retiredItems <-
    {
      setdiff(menuX$product, menuY$product)
    }
  newItems <-
    {
      setdiff(menuY$product, menuX$product)
    }
  continuedItems <-
    {
      intersect(menuX$product, menuY$product)
    }
  continuedItemsDifferenceCount <- c(
    preDiscountPrice = getDifferenceCountOnFeature("preDiscountPrice",
                                                   menuX, menuY, continuedItems),
    discountedPrice = getDifferenceCountOnFeature("discountedPrice",
                                                  menuX, menuY, continuedItems),
    description = getDifferenceCountOnFeature("description",
                                              menuX, menuY, continuedItems)
  )

  productChange = c(
    retiredItems_count = length(retiredItems),
    newItems_count = length(newItems),
    continuedItems_count = length(continuedItems)
  )
  hasChangeIn = c(
    product = all(productChange[c("retiredItems_count","newItems_count")]),
    preDiscountPrice = continuedItemsDifferenceCount[["preDiscountPrice"]] !=0,
    discountedPrice = continuedItemsDifferenceCount[["discountedPrice"]] !=0,
    description = ifelse(
      is.na(continuedItemsDifferenceCount[["description"]]), NA,
      continuedItemsDifferenceCount[["description"]] !=0)
  )
  return(
    list(
      productChange = productChange,
      differenceItemCount_on_continuedItems = continuedItemsDifferenceCount,
      hasChangeIn = hasChangeIn
    )
  )
}

produce_summary_dropout_entrance <- function(wax_dropout_entrance, wane_dropout_entrance) {
  {
    rbind(
      wax_dropout_entrance |>
        list2DF(),
      wane_dropout_entrance |>
        list2DF()
    ) -> summary_dropout_entrance
    rownames(summary_dropout_entrance) <- c("wax", "wane")
    summary_dropout_entrance
  }
}
histogram_feature <- function(wax_featureChanges_in_commonShops) {
  wax_featureChanges_in_commonShops[["each_feature_change_has_how_many_shops"]] |>
    sort(decreasing = T) |>
    as.list() |>
    as.data.frame() -> .temp
  .temp |>
    dplyr::select(
      !tidyselect::any_of(c("updateDate", "Unnamed..0", "rateNum", "rate", "distance"))
    )
}
forEachShop_whichFeatureHasChanged <- function(panel_data) {
  panel_data |>
    dplyr::group_by(shopCode) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::everything(),
        .fns= function(.x){
          if(length(.x)==1 || any(is.na(.x))){return(NA)}
          return(.x[[1]]!=.x[[2]])}
      )
    ) |>
    dplyr::ungroup() -> summarise_feature_changes
  summarise_feature_changes
}
produce_panel <- function(wane_data_before, wane_data_after) {
  dplyr::bind_rows(
    wane_data_before,
    wane_data_after
  ) |>
    dplyr::arrange(shopCode)
}
summarise_number_of_shops_withFeatureChange <- function(wane_shopfeatureHasChanged) {
  wane_shopfeatureHasChanged |>
    dplyr::summarise(
      dplyr::across(
        .col = - shopCode,
        .fns = list(
          number=function(.x) sum(.x, na.rm=T),
          proportion = function(.x) mean(.x, na.rm=T)),
        .names = "{.col}:{.fn}"
      ))
}
extract_long_table_based_on <- function(tb_long, summary_statistics="number") {

  renameVector = "value"
  names(renameVector)=summary_statistics

  tb_long |>
    dplyr::filter(
      stringr::str_detect(statistics, paste0(":", summary_statistics))
    ) |>
    dplyr::mutate(
      feature = stringr::str_extract(statistics,"[^:]+")
    ) |>
    dplyr::select(feature, value) |>
    dplyr::rename(
      dplyr::all_of(renameVector)
    )-> tb_long1

  tb_long1
}
summarise_for_each_feature_number_and_prop_shops_change <- function(tb) {
  tb |>
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = "statistics",
                        values_to = "value") -> tb_long

  tb_long |>
    extract_long_table_based_on("number") -> tb_long_number
  tb_long |>
    extract_long_table_based_on("proportion") -> tb_proportion
  dplyr::full_join(
    tb_long_number, tb_proportion, by="feature"
  )
}
produceKable_kable_wax_wane_feature_change <- function(summarise_wax_feature_change,
                                           summarise_wane_feature_change,
                                           features2keep){
  dplyr::bind_rows(
    summarise_wax_feature_change,
    summarise_wane_feature_change
  ) -> .temp
  .temp |> as.matrix() |> t() -> .temp
  colnames(.temp) <- c("wax", "wane")
  .temp[features2keep,]
}

compute_menu_cost2 <- function(menu, PopularItems) {
  menu|> jsonlite::fromJSON()->shop
  shopCode <- names(menu)
  product <- shop$product
  price <- shop$discountedPrice
  names(price) <- product

  sum(
    price[PopularItems], na.rm=T
  )
}

compute_menu_cost <- function(menu, PopularItems) {
  menu|> jsonlite::fromJSON()->shop
  shopCode <- names(menu)
  product <- shop$product
  price <- shop$discountedPrice
  MenuCost<-data.frame(shopCode = shopCode,product = product, price = price)
  MenuCost|>
    dplyr::filter(product %in% PopularItems) |>
    dplyr::summarise(cost = sum(price, na.rm = TRUE)) |>
    dplyr::pull(cost)
}

compute_all_menuCosts <- function(menu_wane_before, popularItems) {
  menu_cost_wane_before <- vector("numeric", length=length(menu_wane_before)) |>
    setNames(names(menu_wane_before))

  for (i in seq_along(menu_wane_before)) {
    menu_cost_i <-
      tryCatch(
        {
          popularItems_i = popularItems[[names(menu_wane_before[i])]]
          compute_menu_cost2(menu_wane_before[[i]], popularItems_i)
        },
        error= function(e){
          NA
        }
      )
    menu_cost_wane_before[[i]] <- menu_cost_i
  }
  menu_cost_wane_before
}
get_popularItems_from_filepath <- function(filepath) {
  tryCatch(
    {
      filepath |>
        xfun::read_utf8() |>
        jsonlite::fromJSON() |>
        foodDelivery::get_popular_items_from_menuJson()
    },
    error=function(e){
      return(character(0))
    }
  )
}

calculate_inflation_rate <- function(menu_cost_before, menu_cost_after) {
  shopcode<-intersect(names(menu_cost_before),names(menu_cost_after))
  selected_menu_cost_before <- menu_cost_before[names(menu_cost_before) %in% shopcode]
  selected_menu_cost_after <- menu_cost_after[names(menu_cost_after) %in% shopcode]
  inflation_rate <- log(selected_menu_cost_after) - log(selected_menu_cost_before)
  result <- data.frame(shopCode = names(inflation_rate), inflation_rate)
  return(result)
}

compute_percentage<-function(filter_inflation_rate){
  filter_inflation_rate|>dplyr::filter(inflation_rate > 0)|> nrow() -> numberOfShopInflation
  nrow(filter_inflation_rate)->total
  (numberOfShopInflation/total) -> percentage
  return(percentage)
}

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
construct_popularItems_common_across_threePeriods <- function(menu_wax, menu_wane, popularItems) {

  names(menu_wax$before) |>
    intersect(
      names(menu_wax$after)
    ) |>
    intersect(
      names(menu_wane$after)
    ) -> allShopCodes

  FindAllPrice <- vector("list", length(allShopCodes)) |>
    setNames(allShopCodes)
  for (i in seq_along(allShopCodes)) {
    shopCodeX <- allShopCodes[[i]]
    item <- popularItems[[shopCodeX]]
    menu_wax_before_parsed <- parseMenu_allowNA(menu_wax$before[[shopCodeX]])
    menu_wax_after_parsed <- parseMenu_allowNA(menu_wax$after[[shopCodeX]])
    menu_wane_after_parsed <- parseMenu_allowNA(menu_wane$after[[shopCodeX]])

    # 檢查item是否出現在對應的三期菜單中
    item |>
      intersect(menu_wax_before_parsed$product) |>
      intersect(menu_wax_after_parsed$product) |>
      intersect(menu_wane_after_parsed$product) -> FindAllPrice[[allShopCodes[[i]]]]
  }
  FindAllPrice
}
parseMenu_allowNA <- function(menuX) {
  if(is.na(menuX)) return(character(0))
  jsonlite::fromJSON(menuX)
}

peristentPopularItems_mealOffering<-function(shopcode_count,count){
  names(persistent_popularItems_count[persistent_popularItems_count==count]) ->shopcode_count
  shopcode_count|>intersect(mealOffering_shopCodes)->shopcode_count_mealOffering
  length(shopcode_count_mealOffering)/length(shopcode_count)
}

shop_business_hour<-function(types){
  names_with <- vector("list",length = length(business_hour_types))|>
    setNames(names(business_hour_types))
  for (i in 1:length(business_hour_types)) {
    names_with[[i]] <- business_hour_types[[i]][grep(types, business_hour_types[[i]])]
  }
  names_with <- names_with[sapply(names_with, length) > 0]
  return(names_with)
}

intersection_counts <- function(shopcode_count) {
  c(
    sum(length(intersect(shopcode_count, names(names_with_breakfast)))),
    sum(length(intersect(shopcode_count, names(names_with_lunch)))),
    sum(length(intersect(shopcode_count, names(names_with_dinner)))),
    sum(length(intersect(shopcode_count, names(names_with_bartime)))),
    sum(length(intersect(shopcode_count, names(names_with_brunch)))),
    sum(length(intersect(shopcode_count, names(names_with_midnight)))),
    sum(length(intersect(shopcode_count, names(names_with_hightea))))
  )}

pie_plot_business_hours<-function(intersection_counts){
  periods <- c("Breakfast", "Lunch", "Dinner", "Bartime", "Brunch", "Midnight", "Hightea")
  data <- data.frame(periods, intersection_counts)
  data <- data[order(data$intersection_counts, decreasing = TRUE), ]
  data$percentage <- (data$intersection_counts / sum(data$intersection_counts)) * 100
  my_colors <- c("#4682A9", "#749BC2", "#91C8E4", "#FFEADD", "#DFD7BF","#F2EAD3", "#F5F5F5")
  ggplot(data, aes(x = "", y = percentage, fill = reorder(periods, -intersection_counts))) +
    geom_bar(stat = "identity", width = 1) +
    scale_fill_manual(values = my_colors) +
    coord_polar(theta = "y") +
    geom_text(aes(label = paste(round(percentage, 1), "%\n", periods)),
              position = position_stack(vjust = 0.5),
              size=3) +
    labs(title = "Percentages by Period",fill="") +
    theme_void()

}
remove_otherpairings_in_drinkShop <- function(category) {
  category |>
    stringr::str_remove_all(
      "'(甜點|咖啡|蛋糕|甜甜圈|豆花|甜點|飲料)'"
    ) |>
    stringr::str_remove_all("\\s") |>
    stringr::str_remove_all(",,")
}
pick_shops_offering_no_meal <- function(category) {
  category |>
    remove_otherpairings_in_drinkShop() |>
    stringr::str_detect("\\[,?\\]")
}
get_mealOffering_shopCodes <- function(wax_data_before) {
  wax_data_before |>
    dplyr::filter(
      !pick_shops_offering_no_meal(category)
    ) |>
    dplyr::pull(shopCode)
}
compute_inflationRate_logX_minus_logY <- function(.before, .after) {

  cpi_wax <- log(.after)-log(.before)

  cpi_wax |>
    is.nan() -> pickNan
  pickFinite <- abs(cpi_wax) != Inf

  cpi_wax[!pickNan & pickFinite] |>
    na.omit() -> cpi_wax_valid
  cpi_wax_valid

}

summarise_inflationRate <- function(cpi_wax_valid) {
  cpi_wax_valid |>
    cut(
      c(-Inf, -1, -1e-7, 1e-7, 1, Inf),
      ordered_result = T
    )  -> fct_cpi
  levels(fct_cpi)[c(2,3,4)] <- c("(-1,0)","0","(0,1]")
  table(fct_cpi)
}

getTrackingShopCodes_for_mealOfferingShops <- function(list_datas) {
  # Meal offering shops that exist for three periods

  purrr::map(
    seq_along(list_datas),
    ~get_mealOffering_shopCodes(list_datas[[.x]])
  ) -> list_mealOfferingShopCodes

  purrr::reduce(
    list_mealOfferingShopCodes,
    intersect
  )
}

summarise_inflationRate2 <- function(cpi_wax_valid) {
  cpi_wax_valid |>
    cut(
      c(-Inf,-0.2,-0.1, -1e-7, 1e-7,0.1, 0.2,Inf),
      ordered_result = T
    )  -> fct_cpi
  levels(fct_cpi)[c(1:7)] <- c("(-Inf,-0.2]","(-0.2,-0.1]","(-0.1,0]","0","(0,0.1]","(0.1,0.2]","(0.2,Inf]")
  table(fct_cpi)
}
summarise_price<-function(period){
  period|>cut(c(0,200,400,600,800,1000,1200,1400,1600,1800,2000,Inf))->fct_price
  levels(fct_price)[5:11]<-c("(800,1000]","(1000,1200]","(1200,1400]","(1400,1600]",
                             "(1600,1800]","(1800,2000]","(2000,Inf]")
  table(fct_price)
}

summarise_AvgPrice<-function(period){
  period|>cut(c(0,50,100,150,200,250,Inf))->fct_price
  table(fct_price)
}

summary_byGroup<-function(menu_cost_wax_before,wax_data_before,mealoffering_6_shopcode){
  data1<-data.frame(shopCode=names(menu_cost_wax_before[mealoffering_6_shopcode]),
                    price=menu_cost_wax_before[mealoffering_6_shopcode])

  wax_data_before|>select(shopCode,county)->shopcodeCounty


  shopcodeCounty %>%
    dplyr::filter(shopCode %in% mealoffering_6_shopcode)->shopcodeCounty
  shopcodeCounty|>left_join(data1,by="shopCode")|>
    mutate(county = ifelse(shopCode == "y1ew", "新北市", county))->shopcodeCounty

  shopcodeCounty %>%
    group_by(county) %>%
    summarise(avg_price = mean(price, na.rm = TRUE))->summary



}


get_dataFrame_business_day_time <- function(bhours) {
  purrr::map_dfr(
    seq_along(bhours),
    ~ {
      shopCode <- names(bhours[.x])
      wday_bhours <- bhours[[.x]]
      tibble::tibble(
        shopCode = shopCode,
        wday_bhours = unlist(wday_bhours)
      )
    }
  ) -> df_bhours
  df_bhours$wday_bhours |>
    stringr::str_split_fixed(":",2) -> mat_wday_bhours
  colnames(mat_wday_bhours) <- c("weekday","business_hour")
  dplyr::bind_cols(
    df_bhours, mat_wday_bhours
  ) -> df_bhours2
  df_bhours2
}
get_mealType_for_wday <- function(list_wdayXmealIntervals_schedules_wdayX) {

  schedules_wdayX = list_wdayXmealIntervals_schedules_wdayX$schedules_wdayX
  wdayXmealIntervals = list_wdayXmealIntervals_schedules_wdayX$wdayXmealIntervals

  seq_along(schedules_wdayX$opening_time) |>
    purrr::map(
      ~{
        create_interval(
          wdayX =
            list_wdayXmealIntervals_schedules_wdayX$wdayX,
          opening_time = schedules_wdayX$opening_time[[.x]],
          closing_time = schedules_wdayX$closing_time[[.x]]
        ) -> scheduleX
        scheduleX |>
          judge_scheduleX_overlaps_wdayXmealIntervals(
            list_wdayXmealIntervals_schedules_wdayX$wdayXmealIntervals
          )
      }
    )
}

judge_scheduleX_overlaps_wdayXmealIntervals <- function(scheduleX, wdayXmealIntervals) {
  seq_along(wdayXmealIntervals$interval) |>
    purrr::map_lgl(
      ~{
        # .x=1
        wdayXmealIntervals$interval[[.x]] |>
          lubridate::int_overlaps(
            scheduleX
          ) -> flagOverlap
        flagOverlap
      }
    ) -> pickOverlap
  wdayXmealIntervals[pickOverlap, ]
}
create_wdays = function(){
  sunday = "2023-08-06"
  saturday = "2023-08-12"

  wdays <- seq(
    from=lubridate::ymd(sunday),
    to=lubridate::ymd(saturday),
    by="1 day"
  )
  wdays
}
construct_get_businessHourTypes <- function() {
  typeIntervals = get_typeIntervals()
  typeIntervals |> names() -> allTypes
  levels(factor(1:7):factor(allTypes)) -> allWdaysTypes
  return(
    function(schedules) {
      w_busTypes = c()
      schedules$weekday |> unique() -> openWdays
      for(.x in openWdays){
        #.x = openWdays[[1]]
        create_wdayXmealIntervals_schedules_wdayX(
          .x,
          typeIntervals, schedules
        )  |>
          get_mealType_for_wday() -> busHours

        busHours |>
          purrr::map(~{.x$type}) |>
          unlist() -> busTypes

        w_busTypes = c(w_busTypes, paste0(.x, ":", busTypes))
      }
      w_busTypes
    }
  )
}

get_typeIntervals <- function(){
  return(
    list(
      breakfast = c("06:00","09:00"),
      lunch = c("11:00","14:00"),
      dinner = c("17:00", "20:00"),
      brunch = c("09:01","10:59"),
      hightea = c("14:01","16:59"),
      bartime= c("20:01","24:00"),
      midnight = c("00:01","05:59"))
  )
}
typeIntervals = list(
  breakfast = c("06:00","09:00"),
  lunch = c("11:00","14:00"),
  dinner = c("17:00", "20:00"),
  brunch = c("09:01","10:59"),
  hightea = c("14:01","16:59"),
  bartime= c("20:01","24:00"),
  midnight = c("00:01","05:59")
)

create_wdayXmealIntervals_schedules_wdayX <- function( .x, typeIntervals, schedules) {

  wdays = create_wdays()

  create_mealIntervalTibble_for_oneDate(
    wdays[[.x]],
    typeIntervals
  ) -> wdayXmealIntervals

  schedules |>
    dplyr::filter(
      weekday ==.x, opening_type == "delivering"
    ) -> schedules_wdayX

  list(
    wdayX = wdays[[.x]],
    wdayXmealIntervals=wdayXmealIntervals,
    schedules_wdayX=schedules_wdayX
  )
}

create_mealIntervalTibble_for_oneDate <- function(.date, typeIntervals) {
  allTypes <- names(typeIntervals)
  purrr::map_dfr(
    seq_along(allTypes),
    ~{      typex = allTypes[[.x]]
    tibble::tibble(
      type=typex,
      interval = create_interval(
        .date, typeIntervals[[typex]][[1]], typeIntervals[[typex]][[2]]
      )
    )})
}
create_interval <- function(wdayX="2023-08-06",
                            opening_time = "17:00",
                            closing_time = "23:00") {
  .interval = alist(start=, end=)
  glue::glue(
    "{wdayX}{opening_time}"
  ) |>
    lubridate::ymd_hm(tz="Asia/Taipei") -> .interval$start
  glue::glue(
    "{wdayX}{closing_time}"
  ) |>
    lubridate::ymd_hm(tz="Asia/Taipei") -> .interval$end

  if(opening_time_is_bigger_than_closing_time(opening_time, closing_time)){
    .interval$end + lubridate::days(1) -> .interval$end
  }
  lubridate::interval(
    .interval$start,
    .interval$end
  ) -> intervalX
  intervalX
}
opening_time_is_bigger_than_closing_time <- function(opening_time, closing_time){
  lubridate::hm(opening_time) |> lubridate::hour() -> open_hour
  lubridate::hm(closing_time) |> lubridate::hour() -> close_hour
  open_hour > close_hour
}
summarise_competition_by_businessHours <- function( business_wday_time) {
  business_wday_time |>
    dplyr::group_by(
      business_hour
    ) |>
    dplyr::summarise(
      n = length(unique(shopCode)),
      prop = n/length(unique(business_wday_time$shopCode)) |>
        round(digits =  4)
    ) |>
    dplyr::arrange(
      dplyr::desc(n)
    ) |>
    dplyr::ungroup()
}

create_wide_table_regionCat <- function(list_tb, type="table") {
  seq_along(list_tb) |>
    purrr::map_dfr(
      ~{
        list_tb[[.x]][[type]] |>
          as.data.frame() -> df
        names(df) <- c("catetory","frequency")
        df$month <- .x
        df
      }
    ) -> df_tbs
  tibble_tbs <- dplyr::as_tibble(df_tbs)
  df_tbs |>
    dplyr::mutate(
      month = factor(month,
                     levels=c(1,2,3),
                     labels=c("Feb","Apr","Jul"))
    ) |>
    tidyr::pivot_wider(
      names_from = "month",
      values_from = "frequency"
    )
}
create_region_nonRegion_category <- function(wax_data_before) {
  wax_data_before$category |>
    purrr::map(
      ~{
        .x |> stringr::str_extract_all("[^'\\[\\],\\s]+") -> cat
        cat[[1]]
      }
    ) -> list_shopCategory
  categories_by_region <- c("台式", "歐美","日式", "中式", "東南亞", "異國", "韓式", "港式")

  shopCategory_regionOnly <- {
    list_shopCategory |>
      purrr::map_chr(
        ~{
          .x |>
            intersect(categories_by_region) |>
            sort() -> cats
          ifelse(length(cats)==0, "無區域", paste0(cats, collapse = "-"))

        }
      )
  }
  shop_nonRegionCategory <- {
    list_shopCategory |>
      purrr::map_chr(
        ~{
          .x |>
            setdiff(categories_by_region) |>
            sort() -> cats
          ifelse(length(cats)==0, "無", paste0(cats, collapse = "-"))
        }
      )
  }

  wax_data_before$cat_region <- factor(shopCategory_regionOnly)
  wax_data_before$cat_nonRegion <- factor(shop_nonRegionCategory)
  wax_data_before |>
    dplyr::select(shopCode, cat_region, cat_nonRegion)
}
table_regionCategory <- function(wax_data_after_cat) {
  wax_data_after_cat$cat_region |>
    table() |> sort(d=T) -> tb
  tb |> prop.table()-> prop.tb
  prop.tb*100
  list(
    table = tb,
    proportionTable = round(
      prop.tb*100,4
    )
  )
}

focus_dataFrame_on = function(wax_data_before, tracking_shopCodes_mealOffering_6popularItems){
  wax_data_before |>
    dplyr::select(
      shopCode, rateNum
    ) |>
    dplyr::filter(
      shopCode %in% tracking_shopCodes_mealOffering_6popularItems
    ) ->
    df
  df
}
summarise_menuChange_fromAllShops <- function(summaryMenuChangeInEachShop) {
  # summaryMenuChangeInEachShop = summaryMenuChangeInEachShop_wane
  summaryMenuChangeInEachShop |>
    dplyr::ungroup() |>
    dplyr::summarise(
      dplyr::across(
        .cols = -shopCode,
        .fns = function(.x) sum(.x, na.rm = T)
      )
    ) -> .summarise
  .summarise$n <- nrow(summaryMenuChangeInEachShop)
  .summarise
}
summarise_menuChange_eachShop <- function(changeInMenu_wax) {
  changeInMenu_wax |>
    purrr::map(
      ~{
        .x$result$hasChangeIn
      }
    ) -> wax_hasChangeIn

  seq_along(wax_hasChangeIn) |>
    purrr::map_dfr(
      ~{
        tryCatch(
          {
            resultX <- wax_hasChangeIn[[.x]]
            resultX |> as.list() |>
              list2DF() -> summaryX
            shopCodeX = names(wax_hasChangeIn[.x])
            summaryX$shopCode <- shopCodeX
            summaryX
          },
          error=function(e){
            return()
          }
        )

      }
    ) |>
    dplyr::relocate(shopCode) -> summary_wax_menuChange

  summary_wax_menuChange |> dplyr::rowwise() |>
    dplyr::mutate(
      anyChange = any(dplyr::c_across(
        c("product", "preDiscountPrice", "discountedPrice")), na.rm=T)
    ) -> summary_wax_menuChange

  summary_wax_menuChange

}
compute_dropout_entrance <- function(.shopNumbers) {
  .beforeTotal <- (.shopNumbers$`only before`+.shopNumbers$`Both Dates`)
  dropOutRate =  .shopNumbers$`only before`/ .beforeTotal
  entranceRate = .shopNumbers$`only after`/ .beforeTotal
  list(
    dropout=dropOutRate,
    entranceRate=entranceRate
  )
}
produceArrayWithNames <- function(.array, .name){
  .temp <- .array
  names(.temp) <- .name
  .temp
}
summarise_changeInMenu <- function(panel_wane, wane_data_before,wane_data_after) {
  wane_shopfeatureHasChanged <- forEachShop_whichFeatureHasChanged(panel_wane)

  wane_whichHasChangeMenu <-
    which(wane_shopfeatureHasChanged$menu)

  wane_hasChangedMenu_shopCode <- wane_shopfeatureHasChanged$shopCode[wane_whichHasChangeMenu]

  menu_before <- {
    wane_data_before |>
      dplyr::filter(
        shopCode %in% wane_hasChangedMenu_shopCode
      ) |>
      dplyr::arrange(shopCode) -> .temp
    .temp$menu[wane_whichHasChangeMenu]  |>
      stringr::str_replace_all("\"","**") |>
      stringr::str_replace_all("'",'"') |>
      stringr::str_remove_all("[\\<\\>]+")
  }
  menu_after <- {
    wane_data_after  |>
      dplyr::filter(
        shopCode %in% wane_hasChangedMenu_shopCode
      ) |>
      dplyr::arrange(shopCode) -> .temp
    .temp$menu[wane_whichHasChangeMenu] |>
      stringr::str_replace_all("\"","**") |>
      stringr::str_replace_all("'",'"') |>
      stringr::str_remove_all("[\\<\\>]+")
  }
  changeInMenu <- purrr::map2(
    menu_before, menu_after, purrr::safely(summarise_menuChange)
  ) |>
    setNames(sort(wane_hasChangedMenu_shopCode))

  changeInMenu
}
summarise_changeInMenu2 <- function(.menus, .shopfeatureHasChanged) {


  whichHasChangeMenu <- which(.shopfeatureHasChanged$menu)
  shopCodeHasChangeMenu <- .shopfeatureHasChanged$shopCode[whichHasChangeMenu]

  changeInMenu <- purrr::map(
    shopCodeHasChangeMenu,
    purrr::safely(
      function(.x){
        summarise_menuChange(.menus$before[[.x]], .menus$after[[.x]])}
    )
  ) |>
    setNames(shopCodeHasChangeMenu)

}
getDifferenceCountOnFeature <- function(feature, menuX, menuY, continuedItems) {
  if(is.null(menuX[[feature]])) return(NA)
  .arrayXwithNames <- produceArrayWithNames(menuX[[feature]], menuX[["product"]])
  .arrayYwithNames <- produceArrayWithNames(menuY[[feature]], menuY[["product"]])
  differenceCounts <-
    getHowManyDifferencesInContinuedItems(.arrayXwithNames, .arrayYwithNames, continuedItems)
  differenceCounts
}
#'
#' fix_jsonChar_m <- function(mm) {
#'   mm |> stringr::str_remove_all("\"") |>
#'     stringr::str_replace("'product'",'"product"') |>
#'     stringr::str_replace("'preDiscountPrice'",'"preDiscountPrice"') |>
#'     stringr::str_replace("'discountedPrice'",'"discountedPrice"') |>
#'     stringr::str_replace("'description'",'"description"') |>
#'     stringr::str_replace_all("''",'""') |>
#'     stringr::str_replace_all(
#'       c("\\['"='\\[\\"',
#'         "'\\]"='\\"\\]',
#'         "', '"='", "',
#'         "', \""='", "',
#'         "\", '"='", "')
#'     ) |>
#'     stringr::str_remove_all('\\\\')
#' }
#' #' Fix menu json string error in old data
#' #'
#' #' @param mm a string of menu json
#' #'
#' #' @return a string of correct json
#' #' @export
#' fix_jsonChar <- function(mm){
#'   mm |>
#'     stringr::str_replace_all("\"","'") |>
#'     stringr::str_replace_all("\\\\","\\\\\\\\") |>
#'     stringr::str_replace_all(
#'       "(?<=\\{|\\:[:blank:]\\[|\\,[:blank:]|\\]\\,[blank]|\\]\\,[:blank:])'|'(?=\\:[:blank:]\\[|\\,[:blank:]|\\]\\,[:blank:]|\\]\\})",
#'       "\"")
#' }
extract_menu_before_after <- function(data_before, data_after) {
  menu_after <- data_after$menu |> foodDelivery::fix_jsonChar()
  names(menu_after) <- data_after$shopCode
  menu_before <- data_before$menu |> foodDelivery::fix_jsonChar()
  names(menu_before) <- data_before$shopCode
  list(
    before=menu_before,
    after=menu_after
  )
}

getHowManyDifferencesInContinuedItems <- function(.arrayX, .arrayY, continuedItems)
{
  arrayX=.arrayX[continuedItems]; arrayY=.arrayY[continuedItems]
  sum(arrayX != arrayY[names(arrayX)], na.rm = T)
}
summarise_menuChange <- function(.menuX, .menuY) {
  jsonlite::fromJSON(.menuX, simplifyDataFrame = F) -> menuX
  jsonlite::fromJSON(.menuY, simplifyDataFrame = F) -> menuY
  retiredItems <-
    {
      setdiff(menuX$product, menuY$product)
    }
  newItems <-
    {
      setdiff(menuY$product, menuX$product)
    }
  continuedItems <-
    {
      intersect(menuX$product, menuY$product)
    }
  continuedItemsDifferenceCount <- c(
    preDiscountPrice = getDifferenceCountOnFeature("preDiscountPrice",
                                                   menuX, menuY, continuedItems),
    discountedPrice = getDifferenceCountOnFeature("discountedPrice",
                                                  menuX, menuY, continuedItems),
    description = getDifferenceCountOnFeature("description",
                                              menuX, menuY, continuedItems)
  )

  productChange = c(
    retiredItems_count = length(retiredItems),
    newItems_count = length(newItems),
    continuedItems_count = length(continuedItems)
  )
  hasChangeIn = c(
    product = all(productChange[c("retiredItems_count","newItems_count")]),
    preDiscountPrice = continuedItemsDifferenceCount[["preDiscountPrice"]] !=0,
    discountedPrice = continuedItemsDifferenceCount[["discountedPrice"]] !=0,
    description = ifelse(
      is.na(continuedItemsDifferenceCount[["description"]]), NA,
      continuedItemsDifferenceCount[["description"]] !=0)
  )
  return(
    list(
      productChange = productChange,
      differenceItemCount_on_continuedItems = continuedItemsDifferenceCount,
      hasChangeIn = hasChangeIn
    )
  )
}

produce_summary_dropout_entrance <- function(wax_dropout_entrance, wane_dropout_entrance) {
  {
    rbind(
      wax_dropout_entrance |>
        list2DF(),
      wane_dropout_entrance |>
        list2DF()
    ) -> summary_dropout_entrance
    rownames(summary_dropout_entrance) <- c("wax", "wane")
    summary_dropout_entrance
  }
}
histogram_feature <- function(wax_featureChanges_in_commonShops) {
  wax_featureChanges_in_commonShops[["each_feature_change_has_how_many_shops"]] |>
    sort(decreasing = T) |>
    as.list() |>
    as.data.frame() -> .temp
  .temp |>
    dplyr::select(
      !tidyselect::any_of(c("updateDate", "Unnamed..0", "rateNum", "rate", "distance"))
    )
}
forEachShop_whichFeatureHasChanged <- function(panel_data) {
  panel_data |>
    dplyr::group_by(shopCode) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::everything(),
        .fns= function(.x){
          if(length(.x)==1 || any(is.na(.x))){return(NA)}
          return(.x[[1]]!=.x[[2]])}
      )
    ) |>
    dplyr::ungroup() -> summarise_feature_changes
  summarise_feature_changes
}
produce_panel <- function(wane_data_before, wane_data_after) {
  dplyr::bind_rows(
    wane_data_before,
    wane_data_after
  ) |>
    dplyr::arrange(shopCode)
}
summarise_number_of_shops_withFeatureChange <- function(wane_shopfeatureHasChanged) {
  wane_shopfeatureHasChanged |>
    dplyr::summarise(
      dplyr::across(
        .col = - shopCode,
        .fns = list(
          number=function(.x) sum(.x, na.rm=T),
          proportion = function(.x) mean(.x, na.rm=T)),
        .names = "{.col}:{.fn}"
      ))
}
extract_long_table_based_on <- function(tb_long, summary_statistics="number") {

  renameVector = "value"
  names(renameVector)=summary_statistics

  tb_long |>
    dplyr::filter(
      stringr::str_detect(statistics, paste0(":", summary_statistics))
    ) |>
    dplyr::mutate(
      feature = stringr::str_extract(statistics,"[^:]+")
    ) |>
    dplyr::select(feature, value) |>
    dplyr::rename(
      dplyr::all_of(renameVector)
    )-> tb_long1

  tb_long1
}
summarise_for_each_feature_number_and_prop_shops_change <- function(tb) {
  tb |>
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = "statistics",
                        values_to = "value") -> tb_long

  tb_long |>
    extract_long_table_based_on("number") -> tb_long_number
  tb_long |>
    extract_long_table_based_on("proportion") -> tb_proportion
  dplyr::full_join(
    tb_long_number, tb_proportion, by="feature"
  )
}
produceKable_kable_wax_wane_feature_change <- function(summarise_wax_feature_change,
                                                       summarise_wane_feature_change,
                                                       features2keep){
  dplyr::bind_rows(
    summarise_wax_feature_change,
    summarise_wane_feature_change
  ) -> .temp
  .temp |> as.matrix() |> t() -> .temp
  colnames(.temp) <- c("wax", "wane")
  .temp[features2keep,]
}

compute_menu_cost2 <- function(menu, PopularItems) {
  menu|> jsonlite::fromJSON()->shop
  shopCode <- names(menu)
  product <- shop$product
  price <- shop$discountedPrice
  names(price) <- product

  sum(
    price[PopularItems], na.rm=T
  )
}

compute_menu_cost <- function(menu, PopularItems) {
  menu|> jsonlite::fromJSON()->shop
  shopCode <- names(menu)
  product <- shop$product
  price <- shop$discountedPrice
  MenuCost<-data.frame(shopCode = shopCode,product = product, price = price)
  MenuCost|>
    dplyr::filter(product %in% PopularItems) |>
    dplyr::summarise(cost = sum(price, na.rm = TRUE)) |>
    dplyr::pull(cost)
}

compute_all_menuCosts <- function(menu_wane_before, popularItems) {
  menu_cost_wane_before <- vector("numeric", length=length(menu_wane_before)) |>
    setNames(names(menu_wane_before))

  for (i in seq_along(menu_wane_before)) {
    menu_cost_i <-
      tryCatch(
        {
          popularItems_i = popularItems[[names(menu_wane_before[i])]]
          compute_menu_cost2(menu_wane_before[[i]], popularItems_i)
        },
        error= function(e){
          NA
        }
      )
    menu_cost_wane_before[[i]] <- menu_cost_i
  }
  menu_cost_wane_before
}
get_popularItems_from_filepath <- function(filepath) {
  tryCatch(
    {
      filepath |>
        xfun::read_utf8() |>
        jsonlite::fromJSON() |>
        foodDelivery::get_popular_items_from_menuJson()
    },
    error=function(e){
      return(character(0))
    }
  )
}

calculate_inflation_rate <- function(menu_cost_before, menu_cost_after) {
  shopcode<-intersect(names(menu_cost_before),names(menu_cost_after))
  selected_menu_cost_before <- menu_cost_before[names(menu_cost_before) %in% shopcode]
  selected_menu_cost_after <- menu_cost_after[names(menu_cost_after) %in% shopcode]
  inflation_rate <- log(selected_menu_cost_after) - log(selected_menu_cost_before)
  result <- data.frame(shopCode = names(inflation_rate), inflation_rate)
  return(result)
}

compute_percentage<-function(filter_inflation_rate){
  filter_inflation_rate|>dplyr::filter(inflation_rate > 0)|> nrow() -> numberOfShopInflation
  nrow(filter_inflation_rate)->total
  (numberOfShopInflation/total) -> percentage
  return(percentage)
}

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
construct_popularItems_common_across_threePeriods <- function(menu_wax, menu_wane, popularItems) {

  names(menu_wax$before) |>
    intersect(
      names(menu_wax$after)
    ) |>
    intersect(
      names(menu_wane$after)
    ) -> allShopCodes

  FindAllPrice <- vector("list", length(allShopCodes)) |>
    setNames(allShopCodes)
  for (i in seq_along(allShopCodes)) {
    shopCodeX <- allShopCodes[[i]]
    item <- popularItems[[shopCodeX]]
    menu_wax_before_parsed <- parseMenu_allowNA(menu_wax$before[[shopCodeX]])
    menu_wax_after_parsed <- parseMenu_allowNA(menu_wax$after[[shopCodeX]])
    menu_wane_after_parsed <- parseMenu_allowNA(menu_wane$after[[shopCodeX]])

    # 檢查item是否出現在對應的三期菜單中
    item |>
      intersect(menu_wax_before_parsed$product) |>
      intersect(menu_wax_after_parsed$product) |>
      intersect(menu_wane_after_parsed$product) -> FindAllPrice[[allShopCodes[[i]]]]
  }
  FindAllPrice
}
parseMenu_allowNA <- function(menuX) {
  if(is.na(menuX)) return(character(0))
  jsonlite::fromJSON(menuX)
}

peristentPopularItems_mealOffering<-function(shopcode_count,count){
  names(persistent_popularItems_count[persistent_popularItems_count==count]) ->shopcode_count
  shopcode_count|>intersect(mealOffering_shopCodes)->shopcode_count_mealOffering
  length(shopcode_count_mealOffering)/length(shopcode_count)
}

shop_business_hour<-function(types){
  names_with <- vector("list",length = length(business_hour_types))|>
    setNames(names(business_hour_types))
  for (i in 1:length(business_hour_types)) {
    names_with[[i]] <- business_hour_types[[i]][grep(types, business_hour_types[[i]])]
  }
  names_with <- names_with[sapply(names_with, length) > 0]
  return(names_with)
}

intersection_counts <- function(shopcode_count) {
  c(
    sum(length(intersect(shopcode_count, names(names_with_breakfast)))),
    sum(length(intersect(shopcode_count, names(names_with_lunch)))),
    sum(length(intersect(shopcode_count, names(names_with_dinner)))),
    sum(length(intersect(shopcode_count, names(names_with_bartime)))),
    sum(length(intersect(shopcode_count, names(names_with_brunch)))),
    sum(length(intersect(shopcode_count, names(names_with_midnight)))),
    sum(length(intersect(shopcode_count, names(names_with_hightea))))
  )}

pie_plot_business_hours<-function(intersection_counts){
  periods <- c("Breakfast", "Lunch", "Dinner", "Bartime", "Brunch", "Midnight", "Hightea")
  data <- data.frame(periods, intersection_counts)
  data <- data[order(data$intersection_counts, decreasing = TRUE), ]
  data$percentage <- (data$intersection_counts / sum(data$intersection_counts)) * 100
  my_colors <- c("#4682A9", "#749BC2", "#91C8E4", "#FFEADD", "#DFD7BF","#F2EAD3", "#F5F5F5")
  ggplot(data, aes(x = "", y = percentage, fill = reorder(periods, -intersection_counts))) +
    geom_bar(stat = "identity", width = 1) +
    scale_fill_manual(values = my_colors) +
    coord_polar(theta = "y") +
    geom_text(aes(label = paste(round(percentage, 1), "%\n", periods)),
              position = position_stack(vjust = 0.5),
              size=3) +
    labs(title = "Percentages by Period",fill="") +
    theme_void()

}
remove_otherpairings_in_drinkShop <- function(category) {
  category |>
    stringr::str_remove_all(
      "'(甜點|咖啡|蛋糕|甜甜圈|豆花|甜點|飲料)'"
    ) |>
    stringr::str_remove_all("\\s") |>
    stringr::str_remove_all(",,")
}
pick_shops_offering_no_meal <- function(category) {
  category |>
    remove_otherpairings_in_drinkShop() |>
    stringr::str_detect("\\[,?\\]")
}
get_mealOffering_shopCodes <- function(wax_data_before) {
  wax_data_before |>
    dplyr::filter(
      !pick_shops_offering_no_meal(category)
    ) |>
    dplyr::pull(shopCode)
}
create_factor_offeringMeals <- function(wane_data_after){
  wane_data_after$category |>
    pick_shops_offering_no_meal() -> lgl_noMeal
  fct_meal <- factor(lgl_noMeal, levels=c(FALSE, TRUE), labels=c("正餐","非正餐"))
  data.frame(
    shopCode = wane_data_after$shopCode,
    mealOffer = fct_meal
  )
}

create_factors_for_nonRegionalCategories <- function(wane_data_after_cat) {
  wane_data_after_cat$cat_nonRegion |>
    stringr::str_detect("飲料") -> lgl_drink
  wane_data_after_cat$cat_nonRegion |>
    stringr::str_detect("甜點|蛋糕|豆花|甜甜圈") -> lgl_snack
  # wane_data_after_cat$cat_nonRegion |>
  #   stringr::str_detect("素食") -> lgl_vegie
  # lgl_vegie |> table()



  fct_snack <- factor(lgl_snack, levels=c(TRUE, FALSE), labels=c("甜點","非甜點"))
  fct_drink <- factor(lgl_drink, levels=c(TRUE, FALSE), labels=c("飲料","非飲料"))
  data.frame(
    shopCode = wane_data_after_cat$shopCode,
    dessertOffer = fct_snack,
    drinkOffer = fct_drink
  )
}


compute_inflationRate_logX_minus_logY <- function(.before, .after) {

  cpi_wax <- log(.after)-log(.before)

  cpi_wax |>
    is.nan() -> pickNan
  pickFinite <- abs(cpi_wax) != Inf

  cpi_wax[!pickNan & pickFinite] |>
    na.omit() -> cpi_wax_valid
  cpi_wax_valid

}

summarise_inflationRate <- function(cpi_wax_valid) {
  cpi_wax_valid |>
    cut(
      c(-Inf, -1, -1e-7, 1e-7, 1, Inf),
      ordered_result = T
    )  -> fct_cpi
  levels(fct_cpi)[c(2,3,4)] <- c("(-1,0)","0","(0,1]")
  table(fct_cpi)
}

getTrackingShopCodes_for_mealOfferingShops <- function(list_datas) {
  # Meal offering shops that exist for three periods

  purrr::map(
    seq_along(list_datas),
    ~get_mealOffering_shopCodes(list_datas[[.x]])
  ) -> list_mealOfferingShopCodes

  purrr::reduce(
    list_mealOfferingShopCodes,
    intersect
  )
}

summarise_inflationRate2 <- function(cpi_wax_valid) {
  cpi_wax_valid |>
    cut(
      c(-Inf,-0.2,-0.1, -1e-7, 1e-7,0.1, 0.2,Inf),
      ordered_result = T
    )  -> fct_cpi
  levels(fct_cpi)[c(1:7)] <- c("(-Inf,-0.2]","(-0.2,-0.1]","(-0.1,0]","0","(0,0.1]","(0.1,0.2]","(0.2,Inf]")
  table(fct_cpi)
}
summarise_price<-function(period){
  period|>cut(c(0,200,400,600,800,1000,1200,1400,1600,1800,2000,Inf))->fct_price
  levels(fct_price)[5:11]<-c("(800,1000]","(1000,1200]","(1200,1400]","(1400,1600]",
                             "(1600,1800]","(1800,2000]","(2000,Inf]")
  table(fct_price)
}

summarise_AvgPrice<-function(period){
  period|>cut(c(0,50,100,150,200,250,Inf))->fct_price
  table(fct_price)
}

summary_byGroup<-function(menu_cost_wax_before,wax_data_before,mealoffering_6_shopcode){
  data1<-data.frame(shopCode=names(menu_cost_wax_before[mealoffering_6_shopcode]),
                    price=menu_cost_wax_before[mealoffering_6_shopcode])

  wax_data_before|>select(shopCode,county)->shopcodeCounty


  shopcodeCounty %>%
    dplyr::filter(shopCode %in% mealoffering_6_shopcode)->shopcodeCounty
  shopcodeCounty|>left_join(data1,by="shopCode")|>
    mutate(county = ifelse(shopCode == "y1ew", "新北市", county))->shopcodeCounty

  shopcodeCounty %>%
    group_by(county) %>%
    summarise(avg_price = mean(price, na.rm = TRUE))->summary



}

# # post shoplist url
# post_shopList_link <- "https://drive.google.com/drive/folders/1-BgSE8paSRibkj0Xw8jT0hrGj3arxyST"
# post_menu_link <- "https://drive.google.com/drive/folders/1-A4MhEKDjHAgfFBZEqcrFH4m0CVQfciW"
#
# fp <- foodDelivery::FoodPanda(
#   shopListFolderUrl = post_shopList_link,
#   shopMenuFolderUrl = post_menu_link
# )
#
# sampled shopCode in each period ----
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
## ----
filter_6count_okMenus <- function(okMenus_df) {
  okMenus_df |>
    dplyr::group_by(shopCode) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      nextCount = dplyr::lead(count)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      count == 6
    ) -> okMenus_6count_df
  #okMenus_6count_df$nextCount[is.na(okMenus_6count_df$nextCount)] <- "NA"
  okMenus_6count_df |>
    dplyr::mutate(
      nextCount = factor(nextCount, exclude = NULL,
                         levels=c(NA, as.character(0:6) ))
    ) -> okMenus_6count_df
  okMenus_6count_df
}
plot_6countPopularItem_continuity <- function(okMenus_6count_df) {
  ggplot(data=okMenus_6count_df |>
           dplyr::filter(date != "2023-10-15"))+
    geom_bar(
      aes(x=date,fill=nextCount),
      position = "fill"
    )+
    scale_fill_brewer(
      type="qual",
      palette = 3,
      na.value="grey",
      #
      # limits = c(as.character(0:6), "NA"),
      breaks = c(NA, as.character(0:6))
    )+
    scale_x_date(
      breaks = okMenus_6count_df$date |> unique()
    )+
    theme_classic()+
    theme(
      axis.text.x = element_text(angle=45, hjust=1)
    )+
    labs(fill="次期供應數",title="本期供應的6項人氣商品\n下期還有供應幾項")
}
plot_variation_popularItems <- function(okMenus_df) {
  ggplot(data=okMenus_df |>
           dplyr::filter(date != "2023-10-25"))+
    geom_bar(
      aes(x=date,fill=factor(count)),
      position = "fill"
    )+
    scale_x_date(
      breaks = okMenus_df$date |> unique()
    )+
    theme_classic()+
    theme(
      axis.text.x = element_text(angle=45, hjust=1)
    )+
    labs(fill="供應數",title="參考期人氣商品在各期供應狀況")
}
convert_okMenus2_to_data_frame2 <- function(resultX) {
  names(resultX) |>
    purrr::map_dfr(
      ~{
        shop = resultX[[.x]]
        data.frame(
          shopCode = .x,
          cost = ifelse(length(shop$availableItems)==0,0, resultX[[.x]]$cost),
          count = length(resultX[[.x]]$availableItems)
        )
      }
    ) -> df_result
  df_result
}
convert_okMenus_to_data_frame = function(okMenus){
  names(okMenus) |>
    purrr::map_dfr(
      ~{
        okMenus[[.x]] |>
          purrr::map_int(length) -> trackablePopularItemCounts
        data.frame(
          shopCode=names(trackablePopularItemCounts),
          count = trackablePopularItemCounts,
          date = .x
        )
      }
    ) -> okMenus_df
  okMenus_df$date |>
    lubridate::ymd() -> okMenus_df$date
  okMenus_df
}
extract_menu <- function(dataX) {
  menu <- dataX$menu |> foodDelivery::fix_jsonChar()
  names(menu) <- dataX$shopCode
  return(menu)
}
intersect_popularItems <- function(menu, popularItems, computeCost =T) {
  names(menu) -> allShopCodes

  FindAllPrice <- vector("list", length(allShopCodes)) |>
    setNames(allShopCodes)
  for (i in seq_along(allShopCodes)) {
    shopCodeX <- allShopCodes[[i]]
    item <- popularItems[[shopCodeX]]
    parse_menu_json(menu[[shopCodeX]]) -> menu_parsed

    # 檢查item是否出現在對應的三期菜單中
    item |>
      intersect(menu_parsed$product) -> availableItems

    if(computeCost
       && !is.null(availableItems)
       && all(!is.na(availableItems))
       && (length(availableItems)!=0)){
      compute_popularItemsCost(menu_parsed,availableItems) -> FindAllPrice[[allShopCodes[[i]]]]$cost
    }


    availableItems -> FindAllPrice[[allShopCodes[[i]]]]$availableItems
  }
  FindAllPrice
}
compute_popularItemsCost <- function(menu_parsed,availableItems){
  menu_parsed$discountedPrice[
    menu_parsed$product %in% availableItems
  ] |> sum()
}
parse_menu_json = function(menuJson){
  tryCatch(
    sp$parseMenu_allowNA(menuJson),
    error = function(e) {
      return(list(
        product = NA
      ))
    }
  )
}
menu_lookfor_items = function(menu_parsed, items){
  item |>
    intersect(menu_parsed$product)
}
Construct_extract_menu_and_available_popular_items <- function(popularItems) {
  return(function(.x) {
    dataX <- fp$retrieve_data(.x)
    menu <- extract_menu(dataX)
    return(list(
      menu = menu,
      popularItemsIntersect = intersect_popularItems(menu, popularItems)
    ))
  })
}
construct_tar_map_for_menu_data = function(phase2_available_dates){
  values <- tibble(
    method_function = rlang::syms(c("extract_menu_and_available_popular_items")),
    data_source = phase2_available_dates
  )
  targets <- tar_map(
    values = values,
    tar_target(menu_data, method_function(data_source))
  )
  return(list(targets))
}
get_menu_data_each_section <- function(phase2_available_dates) {
  menu_data <- vector("list", length(phase2_available_dates)) |>
    setNames(phase2_available_dates)
  for(.x in phase2_available_dates){
    print(.x)
    menu_data[[.x]] <-
      tryCatch(
        extract_menu_and_available_popular_items(.x),
        error=function(e){
          return(NA)
        })
  }
  return(menu_data)
}

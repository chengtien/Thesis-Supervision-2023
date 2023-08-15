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

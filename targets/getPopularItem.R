library(targets)
library(xfun)
library(jsonlite)
library(foodDelivery)
tar_load(menu_wane)
tar_load(menu_wax)


# 資料夾路徑
folder_path <- "local-data/menuJson_2023-08-06/"

# 取得資料夾中的所有檔案
file_list <- list.files(folder_path, full.names = TRUE)

# 創建一個空的列表來存結果
popularItems <- list()

# 迴圈處理每個檔案
for (file_path in file_list) {
  # 使用 xfun::read_utf8() 讀取UTF-8格式檔案
  file_content <- xfun::read_utf8(file_path)

  # 使用 tryCatch 避免解析錯誤中斷整個迴圈
  tryCatch({
    parsed_json <- jsonlite::fromJSON(file_content)
    if (!is.null(parsed_json)) {
      # 假設 foodDelivery::get_popular_items_from_menuJson() 可以處理解析後的 JSON 物件
      popular_items_fromJSON <- foodDelivery::get_popular_items_from_menuJson(parsed_json)

      # 將結果添加到列表中
      popularItems <- append(popularItems, list(popular_items_fromJSON))
    }
  }, error = function(e) {
    cat("Error while processing file", file_path, ":", conditionMessage(e), "\n")
  })
}


#拿到檔案名稱(以知道shopcode)
file_names <- list.files(folder_path, full.names = FALSE)
file_names |>
  basename() |>
  stringr::str_remove_all(
    "foodpandaMenu_|.json"
  ) -> allShopCodes


#element names改為shopCode
popularItems<-setNames(popularItems[1:56532],allShopCodes[1:56532])




#把當中為空的拿掉
popularItems<-popularItems[popularItems!="character(0)"]



#寫一個compute_menu_cost的function

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
#menu_cost_wax_before最終結果
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

#menu_cost_wax_after最終結果
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

#menu_cost_wane_before最終結果
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

#menu_cost_wane_after最終結果
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

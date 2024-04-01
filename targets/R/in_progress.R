# categories

wax_data_before$category |> head()
wax_data_before$category[[1]] |>
  stringr::str_extract_all("[^'\\[\\],\\s]+") -> shopXcategpory

wax_data_before$category |>
  purrr::map(
    ~{
      .x |> stringr::str_extract_all("[^'\\[\\],\\s]+") -> cat
      cat[[1]]
    }
  ) -> list_shopCategory

list_shopCategory |> unlist() |>
  table() |>
  sort(d=T)

categories_by_region <- c("台式", "歐美","日式", "中式", "東南亞", "異國", "韓式", "港式")

other_categories <- c("麵食", "小吃", "飲料","湯品",
                     "便當",  "早餐", "甜點", "炸雞", "火鍋", "咖啡",
                     "健康餐", "咖哩", "/", "三明治",
                     "吐司", "漢堡", "餃子", "蛋糕", "素食", "粥",
                     "牛排", "披薩", "鐵板燒")


# Categories by region ------
## region, non-region ----
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

wax_data_before_cat <- create_region_nonRegion_category(wax_data_before)

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

tb_region_wane_after |>
  knitr::kable(format="markdown") |>
  clipr::write_clip()

tb_region_wax_before$table |>
  as.data.frame() -> df
names(df) <- c("catetory","frequency")
df$month <- "Feb"

list_tb <- list(
  tb_region_wax_before,
  tb_region_wax_after,
  tb_region_wane_after
)

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

list_tb |> create_wide_table_regionCat() |>
  knitr::kable(format="markdown") |>
  clipr::write_clip()
list_tb |> create_wide_table_regionCat(type="proportionTable")|>
  knitr::kable(format="markdown") |>
  clipr::write_clip()

# Rate number analysis


list_focused_dataFrame <- purrr::map(
  list(
    wax_data_before,
    wax_data_after,
    wane_data_after
  ),
  function(.x) focus_dataFrame_on(.x, tracking_shopCodes_mealOffering_6popularItems)
)

merged_focused_dataFrame <- {
  list_focused_dataFrame |>
    purrr::reduce(
      function(.acc, .x) dplyr::left_join(.acc, .x, by = "shopCode") #,
      # .init = list_focused_dataFrame[[1]]
    )
}

rateNum_dataFrame_long <- {
  names(merged_focused_dataFrame)[-1] <- c("m02", "m04", "m07")
  merged_focused_dataFrame |>
    tidyr::pivot_longer(
      cols = -1,
      names_to = "time",
      values_to = "rateNum"
    )
}

dRateNum_dataFrame <- {
  rateNum_dataFrame_long |>
    dplyr::group_by(shopCode) |>
    dplyr::mutate(
      dRateNum = rateNum - dplyr::lag(rateNum)
    ) |>
    na.omit()
}

library(ggplot2)


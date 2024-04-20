library(googledrive)
importCSVrateNumber <- function(drive_csv1) {
  # get drive file inf
  meta1 <- googledrive::drive_get(drive_csv1)
  # get date from meta name
  meta1$name |>
    stringr::str_extract("\\d{4}-\\d{2}-\\d{2}") ->
    createdDate
  googledrive::drive_download(drive_csv1, path = "data1.csv",
                 overwrite = T)
  data1 <- read.csv("data1.csv")

  data1$date <- as.Date(createdDate)

  data1 |>
    dplyr::select(date, shopCode, rateNum)
}

# 3-4
drive_csv1 <- "https://drive.google.com/file/d/15XOsL0MI-FZlSTin9X3aPbiPyrLC8Ri7/view?usp=sharing"
# 3-11
drive_csv2 <- "https://drive.google.com/file/d/10Pg0_D0df916tnwHnlcjomBWcBK_XOF9/view?usp=sharing"
# 3-18
drive_csv3 <- "https://drive.google.com/file/d/19dz_7Tse55JSsVV5skmy6ScmP1n2wPfX/view?usp=sharing"

# import each csv and bind_row
importCSVrateNumber(drive_csv1) |>
  dplyr::bind_rows(importCSVrateNumber(drive_csv2)) |>
  dplyr::bind_rows(importCSVrateNumber(drive_csv3)) -> data


library(tidyverse)
data |>
  arrange(date) |>
  group_by(shopCode) |>
  mutate(
    rateNumIncrease = rateNum - lag(rateNum)
  ) |>
  filter(
    rateNumIncrease < 0
  )

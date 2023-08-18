

parallel::mclapply(
  fileList,
  get_popularItems_from_filepath,
  mc.cores = 4
) -> results

basename(fileList) |>
  stringr::str_remove_all(
    "foodpandaMenu_|.json"
  ) -> names(results)

results |>
  saveRDS("data/popularItems.Rds")
popularItems <-{readRDS("data/popularItems.Rds")}

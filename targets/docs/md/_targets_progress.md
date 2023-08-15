# 2023-08-15

  * I save `popularItems` in a .Rds and put it under `data/popularItems.Rds`.  
    * When generating `popularItems`, I avoid  `popularItems <- append(popularItems, list(popular_items_fromJSON))` since object that updated itself with its showing on both sides of `<-` will significantly slow down your loop when data is large. See the [`#` lines in `R/pre_process.R`](https://github.com/chengtien/Thesis-Supervision-2023/blob/b9b73524a1caf3b7b8b5b957b2255b4e7112fbfe/targets/R/pre_targets.R#L42).   
  * If codes with data frame are slow, try to avoid using data frame, but use R's built-in object types. That is use only atomic vectors (`c()`), lists, and matrices. I wrap up your old cades in [`compute_menu_cost`](https://github.com/chengtien/Thesis-Supervision-2023/blob/b9b73524a1caf3b7b8b5b957b2255b4e7112fbfe/targets/R/support.R#L331-L341), and substitute it with mine [`compute_menu_cost2`](https://github.com/chengtien/Thesis-Supervision-2023/blob/b9b73524a1caf3b7b8b5b957b2255b4e7112fbfe/targets/R/support.R#L319-L329)(see `R/support.R`). Take a look of how not to use data frame and still achieve the same outcome. The speed is at least 10 times faster.  
  * When blocks with similar expressions repeat several times like  
  ```r
  {
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
  }
  ```  
  Try to wrap it up as a function as I did in [`compute_all_menuCosts`](https://github.com/chengtien/Thesis-Supervision-2023/blob/b711f9e5d0e1cbeb1155af91db2ca761f5558cfc/targets/R/support.R#L343-L361). You could select one block and go to **Code > Extract Function** in RStudio to turn one block of them into a function prototype.  
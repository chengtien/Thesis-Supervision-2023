readRDS("data/rateNum.Rds")->rateNum
readRDS("data/menu_cost_mealoffering.Rds")->menu_cost_mealoffering
readRDS("data/category0806.Rds")->category0806
readRDS("data/cat_population.Rds")->cat_population


date1<-c( "2023-02-03","2023-02-10","2023-02-17","2023-02-24","2023-03-04",
          "2023-03-11", "2023-03-18", "2023-03-25" ,"2023-04-27" ,"2023-05-09",
          "2023-05-16", "2023-05-23", "2023-05-31" ,"2023-06-05", "2023-06-14",
          "2023-07-01" ,"2023-07-22" ,"2023-07-26", "2023-08-06" ,"2023-08-13",
          "2023-08-20", "2023-08-28" ,"2023-09-04" ,"2023-09-21" ,"2023-10-04",
          "2023-10-08", "2023-10-15" ,"2023-11-08" ,"2023-11-15", "2023-11-29",
          "2023-12-06")

#各店家評論增加數
rateNum |>
  arrange(date) |>
  group_by(shopCode) |>
  mutate(
    rateNumIncrease = rateNum - lag(rateNum)
  ) ->rateNumIncrement

for (i in seq_along(menu_cost_mealoffering)){
  rateNumIncrement|>dplyr::filter(date==date1[[i]])|>select(shopCode,rateNumIncrease)->rateIncreased
  left_join(menu_cost_mealoffering[[i]],rateIncreased,by="shopCode")->menu_cost_mealoffering[[i]]
}


#計算加權
final_weight<-list()
inflation_final<-data.frame()

for (i in seq_along(menu_cost_mealoffering)){
  total_shops <- nrow(menu_cost_mealoffering[[i]])

  menu_cost_mealoffering[[i]]|>
    group_by(county,  cat_region, business_hour) |>
    summarise(
      weight=weight[[1]],
      cat_inflation = weighted.mean(inflation, rateNumIncrease,na.rm = T),
      numOfShop= n(),
      .groups = "keep"
    ) |>mutate(
      sample_percentage = numOfShop/ total_shops
    )-> final_weight[[i]]
  left_join(final_weight[[i]],cat_population,by=c("county","cat_region","business_hour"))->final_weight[[i]]

}


#母體調整
for (i in seq_along(final_weight)){
  final_weight[[i]]|>mutate(finalWeight=popu_prop/sample_percentage)->final_weight[[i]]
}



#最終每期加權上漲率
final_inflation<-data.frame()
for (i in seq_along(final_weight)) {
  data.frame(date=names(final_weight[i]),
             inflation=weighted.mean(final_weight[[i]]$cat_inflation,final_weight[[i]]$popu_prop))->d
  rbind(final_inflation,d)->final_inflation

}
final_inflation$date<-as.Date(final_inflation$date)
left_join(final_inflation,date_days,by="date")->final_inflation
final_inflation|>mutate(inflation_byDay=inflation/days,
                        inflation_annualise=inflation_byDay*365)->final_inflation






#商家的三因子加權
category0806$county:category0806$cat_region:category0806$business_hour|>
  table() |>
  prop.table()|>as.data.frame() ->jointPop_CountyCatRegionBusinessHour
jointPop_CountyCatRegionBusinessHour|>rename(county_catRegion_businessHour=Var1,popu_prop=Freq)->jointPop_CountyCatRegionBusinessHour

menu_cost_mealoffering7<-menu_cost_mealoffering

for (i in seq_along(menu_cost_mealoffering7)) {
  d <- menu_cost_mealoffering7[[i]]$county:menu_cost_mealoffering7[[i]]$cat_region:menu_cost_mealoffering7[[i]]$business_hour |>
    table() |>
    prop.table() |>
    as.data.frame()
  d|>rename(county_catRegion_businessHour= Var1, sample_prop = Freq)->d


  jointPop_sample_CountyCatRegionBusinessHour <- left_join(jointPop_CountyCatRegionBusinessHour, d, by = "county_catRegion_businessHour")
  jointPop_sample_CountyCatRegionBusinessHour|>mutate(weight = popu_prop/sample_prop) |>
    separate(county_catRegion_businessHour, into = c("county","cat_region", "business_hour"), sep = ":", remove = FALSE)-> jointPop_sample_CountyCatRegionBusinessHour


  ccw <- jointPop_sample_CountyCatRegionBusinessHour |>
    select(county,cat_region, business_hour, weight)

  menu_cost_mealoffering7[[i]] <- left_join(menu_cost_mealoffering7[[i]], ccw, by = c("county","cat_region", "business_hour"))
}


summary_inflation_weight7 <- {
  purrr::map_dfr(
    menu_cost_mealoffering7,
    ~{
      .x |>
        na.omit() |>
        dplyr::summarise(
          inflation = weighted.mean(inflation, weight, na.rm=T)
        )
    }
  )-> df
  df$date = names(menu_cost_mealoffering7)
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


#比較兩者之間
ggplot() +
  geom_line(data = na.omit(summary_inflation_weight7), aes(x = date, y = inflation_annualise, group = 1, linetype = "經商家加權CPI上漲率"), color = "black", linewidth = 1) +
  geom_line(data = na.omit(final_inflation), aes(x = date, y = inflation_annualise, group = 1, linetype = "經消費者加權CPI上漲率"), color = "black", linewidth = 1) +
  scale_color_manual(values = c("black","black")) +
  scale_linetype_manual(values = c("經商家加權CPI上漲率" = "solid", "經消費者加權CPI上漲率" = "twodash")) +
  labs(color = "", x = "日期", y = "年化CPI上漲率") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=7),
        axis.title.x = element_text(color = "black", size = 10, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(color = "black", size = 10, face = "bold", margin = margin(r = 10)),
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(labels = percent_format(), limits = c(-0.04, 0.14), breaks = seq(-0.04, 0.14, by = 0.01)) +
  scale_x_date(breaks = date1, date_labels = "%Y年%m月%d日")

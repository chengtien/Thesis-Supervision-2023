#1.消費者加權方式的簡單平均
final_weight<-list()
inflation_final<-data.frame()

for (i in seq_along(menu_cost_mealoffering8)){
  menu_cost_mealoffering8[[i]]|>
    group_by(county,  cat_region, business_hour) |>
    summarise(
      weight=weight[[1]],
      cat_inflation = mean(inflation,na.rm = T),
      .groups = "keep"
    ) -> final_weight[[i]]
  left_join(final_weight[[i]],cat_population,by=c("county","cat_region","business_hour"))->final_weight[[i]]

}
names(final_weight)<-date1



final_inflation_<-data.frame()
for (i in seq_along(final_weight)) {
  # 计算加权平均通胀
  inflation <- final_weight[[i]] %>%
    ungroup()|>
    na.omit()|>
    summarise(
      inflation = weighted.mean(cat_inflation, popu_prop,na.rm = TRUE)
    )

  # 假设每个final_weight的元素名称是其对应的日期
  date_name <- names(final_weight[i])  # 获取日期名称

  # 添加日期信息
  inflation$date <- date_name

  # 将结果合并到最终数据框
  final_inflation_ <- rbind(final_inflation_, inflation)
}



final_inflation_$date<-as.Date(final_inflation_$date)
left_join(final_inflation_,date_days,by="date")->final_inflation_
final_inflation_|>mutate(inflation_byDay=inflation/days,
                             inflation_annualise=inflation_byDay*365)->final_inflation_






#2.經加權平均
final_weight<-list()
inflation_final<-data.frame()

for (i in seq_along(menu_cost_mealoffering8)){
  total_shops <- nrow(menu_cost_mealoffering8[[i]])

  menu_cost_mealoffering8[[i]]|>
    na.omit()|>
    group_by(county,  cat_region, business_hour) |>
    summarise(
      weight=weight[[1]],
      cat_inflation = weighted.mean(inflation,rateNumIncrease,na.rm = T),
      numOfShop= n(),
      .groups = "keep"
    ) -> final_weight[[i]]
  left_join(final_weight[[i]],cat_population,by=c("county","cat_region","business_hour"))->final_weight[[i]]

}



for (i in seq_along(final_weight)) {
  # 计算加权平均通胀
  inflation <- final_weight[[i]] %>%
    summarise(
      inflation = weighted.mean(cat_inflation, popu_prop,na.rm = TRUE),
      .groups = "drop"
    )

  # 假设每个final_weight的元素名称是其对应的日期
  date_name <- names(final_weight[i])  # 获取日期名称

  # 添加日期信息
  inflation$date <- date_name

  # 将结果合并到最终数据框
  final_inflation <- rbind(final_inflation, inflation)
}

final_inflation<-data.frame()
for (i in seq_along(final_weight)) {
  data.frame(date=names(final_weight[i]),
             inflation=weighted.mean(final_weight[[i]]$cat_inflation,final_weight[[i]]$popu_prop))->d
  rbind(final_inflation_mean,d)->final_inflation_mean

}
final_inflation_mean$date<-as.Date(final_inflation_mean$date)
left_join(final_inflation_mean,date_days,by="date")->final_inflation_mean
final_inflation_mean|>mutate(inflation_byDay=inflation/days,
                             inflation_annualise=inflation_byDay*365)->final_inflation_mean





#2.
final_weight2<-list()
inflation_final2<-data.frame()

for (i in seq_along(menu_cost_mealoffering8)){
  menu_cost_mealoffering8[[i]]|>
    group_by(county,  cat_region, business_hour) |>
    summarise(
      weight=weight[[1]],
      cat_inflation = weighted.mean(inflation,rateNumIncrease,na.rm = T),
      .groups = "keep"
    ) -> final_weight2[[i]]
  left_join(final_weight2[[i]],cat_population,by=c("county","cat_region","business_hour"))->final_weight2[[i]]

}
names(final_weight2)<-date1



final_inflation<-data.frame()
for (i in seq_along(final_weight)) {
  # 计算加权平均通胀
  inflation <- final_weight2[[i]] %>%
    ungroup()|>
    na.omit()|>
    summarise(
      inflation = weighted.mean(cat_inflation, popu_prop,na.rm = TRUE)
    )

  # 假设每个final_weight的元素名称是其对应的日期
  date_name <- names(final_weight2[i])  # 获取日期名称

  # 添加日期信息
  inflation$date <- date_name

  # 将结果合并到最终数据框
  final_inflation <- rbind(final_inflation, inflation)
}



ggplot() +
  geom_line(data = na.omit(final_inflation_), aes(x = date, y = inflation_annualise, group = 1, linetype = "未經評論增加數加權CPI上漲率"), color = "black", linewidth = 1) +
  geom_line(data = na.omit(final_inflation), aes(x = date, y = inflation_annualise, group = 1, linetype = "經評論增加數加權CPI上漲率"), color = "black", linewidth = 1) +
  scale_color_manual(values = c("black","black")) +
  scale_linetype_manual(values = c("經評論增加數加權CPI上漲率" = "solid", "未經評論增加數加權CPI上漲率" = "twodash")) +
  labs(color = "", x = "日期", y = "年化CPI上漲率") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 10, face = "bold", margin = margin(r = 10)),
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=12)) +
  scale_y_continuous(labels = percent_format(), limits = c(-0.02, 0.14), breaks = seq(-0.02, 0.14, by = 0.01)) +
  scale_x_date(breaks = date1, date_labels = "%Y年%m月%d日")


#3.各縣市的加權CPI
inflation_final2<-data.frame()

for (i in seq_along(final_weight)) {
  # 计算加权平均通胀
  inflation <- final_weight[[i]] %>%
    group_by(county)|>
    summarise(
      inflation = weighted.mean(cat_inflation, popu_prop, na.rm = TRUE),
      # 防止分组输出
    )

  # 假设每个final_weight的元素名称是其对应的日期
  date_name <- names(final_weight[i])  # 获取日期名称

  # 添加日期信息
  inflation$date <- date_name

  # 将结果合并到最终数据框
  inflation_final2 <- rbind(inflation_final2, inflation)
}

inflation_final2$date<-as.Date(inflation_final2$date)
left_join(inflation_final2,date_days,by="date")->inflation_final2
inflation_final2|>mutate(inflation_byDay=inflation/days,
                             inflation_annualise=inflation_byDay*365)->inflation_final2



#4.區域類別CPI
inflation_final3<-data.frame()

for (i in seq_along(final_weight)) {
  # 计算加权平均通胀
  inflation <- final_weight[[i]] %>%
    na.omit()|>
    group_by(cat_region)|>
    summarise(
      inflation = weighted.mean(cat_inflation, popu_prop, na.rm = TRUE)
    )

  # 假设每个final_weight的元素名称是其对应的日期
  date_name <- names(final_weight[i])  # 获取日期名称

  # 添加日期信息
  inflation$date <- date_name

  # 将结果合并到最终数据框
  inflation_final3 <- rbind(inflation_final3, inflation)
}

inflation_final3$date<-as.Date(inflation_final3$date)
left_join(inflation_final3,date_days,by="date")->inflation_final3
inflation_final3|>mutate(inflation_byDay=inflation/days, inflation_annualise=inflation_byDay*365)->inflation_final3


#5.營業時段CPI
inflation_final4<-data.frame()

for (i in seq_along(final_weight)) {
  # 计算加权平均通胀
  inflation <- final_weight[[i]] %>%
    na.omit()|>
    group_by(business_hour)|>
    summarise(
      inflation = weighted.mean(cat_inflation, popu_prop, na.rm = TRUE),

    )

  # 假设每个final_weight的元素名称是其对应的日期
  date_name <- names(final_weight[i])  # 获取日期名称

  # 添加日期信息
  inflation$date <- date_name

  # 将结果合并到最终数据框
  inflation_final4 <- rbind(inflation_final4, inflation)
}

inflation_final4$date<-as.Date(inflation_final4$date)
left_join(inflation_final4,date_days,by="date")->inflation_final4
inflation_final4|>mutate(inflation_byDay=inflation/days, inflation_annualise=inflation_byDay*365)->inflation_final4




#part A  How large should the time span be? -----------------------------

#1:install the read the package
library(forecast)
library(ggplot2)
library(lubridate)
library(readr)
library(dplyr)
library(fixest)
library(readxl)
library(tidyr)
library(tibble)
library(StuteTest) 
#2
setwd("C:/Users/42517/Desktop/master2/case studies/a2/crude oil price/fredgraph2")
oil_weekly <- read.csv("2020.1.3-2024.1.5 weekly.csv")

# 3. category，define pre / transition / post -----------------------------

oil_weekly$observation_date <- as.Date(oil_weekly$observation_date)
oil_weekly$period <- ifelse(
  oil_weekly$observation_date <= as.Date("2022-02-18"),
  "pre",
  ifelse(
    oil_weekly$observation_date == as.Date("2022-02-25"),
    "transition",
    "post"
  )
)


# 4. create the pre and post data -----------------------------

pre <- subset(oil_weekly, period == "pre")
post <- subset(oil_weekly, period == "post")
#search any NA in dataset
if (anyNA(pre$DCOILWTICO)) {
  stop("the NA ")
}

# create time series
y <- ts(pre$DCOILWTICO, frequency = 1)


# draw the crude oil diagram to see the trend or not
plot(
  pre$observation_date,
  y,
  type = "l",
  xlab = "Week",
  ylab = "USD per barrel",
  main = "Pre-war WTI Crude Oil Price"
)

# 5. autosearch ARIMA function --------------------------------------
fit_method <- function(z, method) {
  
  # Random walk as benchmark
  if (method == "random_walk") {
    
    return(
      Arima(
        z,
        order = c(0, 1, 0),
        include.drift = FALSE
      )
    )
    
  }
  
  # auto ARIMA search
  auto.arima(
    z,
    seasonal = FALSE,
    ic = "aicc",
    test = "kpss",
    max.p = 5,
    max.q = 5,
    max.order = 10,
    stepwise = (method == "fast"),
    approximation = FALSE
  )
}


# 6. Reserve the last eight pre-battle weeks for a preliminary comparison --------------------------

h_valid <- 8L

train <- head(y, -h_valid)
test <- tail(y, h_valid)

# quick search
fit_fast <- fit_method(train, "fast")

# full-fit search
fit_thorough <- fit_method(train, "thorough")

# Random walk benchmark
fit_rw <- fit_method(train, "random_walk")


summary(fit_fast)
summary(fit_thorough)
summary(fit_rw)

models <- list(
  fast = fit_fast,
  thorough = fit_thorough,
  random_walk = fit_rw
)

model_summary <- data.frame(
  Model = names(models),
  ARIMA = sapply(models, function(x) {
    paste0("ARIMA(", paste(arimaorder(x), collapse = ","), ")")
  }),
  AIC = sapply(models, AIC),
  AICc = sapply(models, function(x) x$aicc),
  BIC = sapply(models, BIC)
)

print(model_summary)


# 7. cheack residuals ---------------------------------------------

checkresiduals(fit_fast)

checkresiduals(fit_thorough)

checkresiduals(fit_rw)

residual_summary <- data.frame(
  Model = names(models),
  
  Ljung_Box_p = sapply(models, function(x) {
    
    res <- residuals(x)
    
    lag_use <- min(10, floor(length(res) / 5))
    
    Box.test(
      res,
      lag = lag_use,
      type = "Ljung-Box",
      fitdf = length(coef(x))
    )$p.value
    
  })
)

residual_summary$Result <- ifelse(
  residual_summary$Ljung_Box_p > 0.05,
  "Pass",
  "Potential autocorrelation"
)

print(residual_summary)


# 8. compare prediction accuracy using test data-------------------------------------------

accuracy(
  forecast(fit_fast, h = h_valid),
  test
)

accuracy(
  forecast(fit_thorough, h = h_valid),
  test
)

accuracy(
  forecast(fit_rw, h = h_valid),
  test
)



# 9. Rolling-origin validation -------------------------------

origins <- length(y) - c(32L, 24L, 16L, 8L)

methods <- c(
  "fast",
  "thorough",
  "random_walk"
)

cv_rows <- list()

for (method in methods) {
  
  for (origin in origins) {
    
    history <- y[seq_len(origin)]
    actual <- y[
      seq.int(
        origin + 1L,
        origin + h_valid
      )
    ]
    
    # build the model
    model <- fit_method(
      history,
      method
    )
    
    # predict the next eight weeks
    predicted <- as.numeric(
      forecast(
        model,
        h = h_valid
      )$mean
    )
    
    # Save the verification results of this round
    cv_rows[[length(cv_rows) + 1L]] <- data.frame(
      
      method = method,
      
      origin = as.character(
        pre$observation_date[origin]
      ),
      
      RMSE = sqrt(
        mean(
          (actual - predicted)^2
        )
      ),
      
      MAE = mean(
        abs(
          actual - predicted
        )
      )
    )
  }
}


cv_results <- do.call(
  rbind,
  cv_rows
)

print(cv_results)


# Calculate the average RMSE/MAE for each method
cv_summary <- aggregate(
  cbind(RMSE, MAE) ~ method,
  data = cv_results,
  FUN = mean
)

# The RMSE is sorted in ascending order
cv_summary <- cv_summary[
  order(cv_summary$RMSE),
]

print(cv_summary)


# 10. Select the final method based on rolling validation -------------------------------

selected_method <- as.character(
  cv_summary$method[1]
)

print(selected_method)
final_fit <- fit_method(
  y,
  selected_method
)



# 11. build post-war counterfactual forecast --------------------

# forecast step 1:
# 2022-02-25

# transition week
# forecast step 2:
# 2022-03-04 is the first whole post-war week
# predict H + 1 week，
#and delete first week

# Here, let's first observe the first 26 complete post-war weeks
H <-52L

stopifnot(
  H <= nrow(post)
)


# Predict for one more week because the first week is a transition
fc <- forecast(
  final_fit,
  h = H + 1L,
  level = 95
)


# delete transition week 
keep <- seq.int(
  2L,
  H + 1L
)


# 12. build actual vs counterfactual table ------------------------

weekly_forecast <- data.frame(
  
  date =
    post$observation_date[
      seq_len(H)
    ],
  
  actual =
    post$DCOILWTICO[
      seq_len(H)
    ],
  
  predicted =
    as.numeric(
      fc$mean
    )[keep],
  
  lower95 =
    as.numeric(
      fc$lower[, 1]
    )[keep],
  
  upper95 =
    as.numeric(
      fc$upper[, 1]
    )[keep]
  
)



# 13. Calculate the gap between the actual price and the predicted price without war -----------------

weekly_forecast$gap <-
  weekly_forecast$actual -
  weekly_forecast$predicted


print(
  weekly_forecast
)


# Average price deviation
mean_gap <- mean(
  weekly_forecast$gap,
  na.rm = TRUE
)

print(
  mean_gap
)




# 14. draw actual vs counterfactual -----------------------------

plot(
  weekly_forecast$date,
  weekly_forecast$actual,
  type = "l",
  
  xlab = "Week",
  ylab = "USD per barrel",
  
  main =
    "Actual vs Counterfactual WTI Price",
  
  ylim = range(
    c(
      weekly_forecast$actual,
      weekly_forecast$lower95,
      weekly_forecast$upper95
    ),
    na.rm = TRUE
  )
)


# Counterfactual forecast
lines(
  weekly_forecast$date,
  weekly_forecast$predicted,
  lty = 1
)


# 95% prediction interval
lines(
  weekly_forecast$date,
  weekly_forecast$lower95,
  lty = 2
)

lines(
  weekly_forecast$date,
  weekly_forecast$upper95,
  lty = 2
)


legend(
  "topleft",
  
  c(
    "Actual",
    "Counterfactual forecast",
    "95% prediction interval"
  ),
  
  lty = c(
    1,
    1,
    2
  ),
  
  bty = "n"
)
 

# 15. find actual <= predicted -----------------------------

returned <- weekly_forecast$gap <= 0

return_week <- which(returned)[1]

return_week

weekly_forecast$date[return_week]

# 15.2 strict way：
# continous three weeks actual <= predicted

return_week_3 <- NA_integer_

for (i in 1:(length(returned) - 2)) {
  
 
  if (
    all(!is.na(returned[i:(i + 2)])) &&
    all(returned[i:(i + 2)])
  ) {
    
    return_week_3 <- i
    break
  }
}


return_week_3


 write.csv(
   weekly_forecast,
   "oil_weekly_forecast.csv",
   row.names = FALSE
 )

 write.csv(
   cv_results,
 "oil_weekly_validation.csv",
  row.names = FALSE)
 #part A finish
 
 #part B seperate hagh and low exposure group-----------------------------

 # 1. Import dataset
 setwd("C:/Users/42517/Desktop/master2/case studies/a2")

  oil <- read_excel(
   "use_pet_capita.xlsx",
   sheet = "Barrels per capita",
   skip = 2
 )
 affinity <- read_csv("Affinity - State - Daily.csv")
 
 # 2. Create date variable
 affinity <- affinity %>%
   mutate(
     date = make_date(year, month, day),
     spend_nondurables = as.numeric(
       na_if(spend_nondurables, ".")
     )
   )
 
 # Calculate the per capita consumption of petroleum products in each state before the war
 oil_exposure <- oil %>%
   select(State, `2019`, `2020`, `2021`) %>%
   filter(State %in% state.abb) %>%
   mutate(
     exposure = rowMeans(
       across(c(`2019`, `2020`, `2021`)),
       na.rm = FALSE
     )
   ) %>%
   filter(!is.na(exposure)) %>%
   arrange(exposure) %>%
   mutate(
     rank = row_number(),
     high = case_when(
       rank <= 18 ~ 0L,        
       rank > n() - 18 ~ 1L,
       TRUE ~ NA_integer_
     )
   ) %>%
   filter(!is.na(high))
 
 # 将州缩写转换为 FIPS,为了对应消费变化表（affinity）
 state_lookup <- tibble(
   State = state.abb,
   statefips = c(
     1, 2, 4, 5, 6, 8, 9, 10, 12, 13,
     15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
     25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
     35, 36, 37, 38, 39, 40, 41, 42, 44, 45,
     46, 47, 48, 49, 50, 51, 53, 54, 55, 56
   )
 )
 
 oil_exposure <- oil_exposure %>%
   left_join(
     state_lookup,
     by = "State"
   )
 
 # 查看分组
 oil_exposure %>%
   select(State, exposure, high) %>%
   arrange(desc(exposure))

 
 #part c did dataset prepareation-----------------------------
 
  affinity <- affinity %>%
   mutate(
     date = make_date(year, month, day)
   )
 
 
  # 战争前27周(pre war 27)
 pre_weeks <- seq(
   as.Date("2021-08-22"),
   as.Date("2022-02-20"),
   by = "week"
 )
 
 # 战争后27周(post war 27)
 post_weeks <- seq(
   as.Date("2022-03-06"),
   as.Date("2022-09-04"),
   by = "week"
 )
 
 # 合并消费数据和州暴露度
 did_data <- affinity %>%
   inner_join(
     oil_exposure %>%
       select(statefips, State, exposure, high),
     by = "statefips"
   )


 #and finalyly!!!!get the did data ----------------------------- 
 # Part C: DID Dataset Preparation
 
 # Step 1: Create the week_end variable
 did_data <- did_data %>%
   mutate(
     week_end = date + days(7 - wday(date, week_start = 1))
   )
 
 
 # Step 2: Select variables for DID analysis
 did_data2 <- did_data %>%
   select(
     date,
     week_end,
     State,
     statefips,
     high,
     spend_nondurables
   )
 
 
 # Step 3: Define the post-war period
 # Keep only observations within the study period
 did_data2 <- did_data2 %>%
   filter(week_end %in% c(pre_weeks, post_weeks))
  did_data2 <- did_data2 %>%
   mutate(
     post = if_else(
       week_end %in% post_weeks,
       1L,
       0L
     )
   )
 
 
 # Step 4: Check the number of observations in each group
 did_data2 %>%
   group_by(high, post) %>%
   summarise(
     observations = n(),
     missing = sum(is.na(spend_nondurables)),
     valid = sum(!is.na(spend_nondurables)),
     .groups = "drop"
   )
 
 
 # Step 5: Check whether daily observations are consecutive
 did_data2 %>%
   arrange(State, date) %>%
   group_by(State) %>%
   mutate(
     date_gap = as.numeric(date - lag(date))
   ) %>%
   filter(date_gap > 1) %>%
   select(State, date, date_gap)
 
 
 # Step 6: Aggregate daily observations into weekly averages
 did_weekly <- did_data2 %>%
   group_by(State, statefips, high, week_end, post) %>%
   summarise(
     n_obs = sum(!is.na(spend_nondurables)),
     
     spend_nondurables_mean = mean(
       spend_nondurables,
       na.rm = TRUE
     ),
     
     .groups = "drop"
   )
 
 
 # Step 7: Check whether each state has 81 weeks of data
 week_check <- did_weekly %>%
   group_by(State) %>%
   summarise(
     n_weeks = n_distinct(week_end),
     .groups = "drop"
   )
 
 week_check

 #Parallel Trends Test ----------------------------- 
 oil_exposure_cont <- oil %>%
   select(State, `2019`, `2020`, `2021`) %>%
   filter(State %in% state.abb) %>%
   mutate(
     exposure = rowMeans(
       across(c(`2019`, `2020`, `2021`)),
       na.rm = FALSE
     )
   ) %>%
   filter(!is.na(exposure)) %>%
   left_join(state_lookup, by = "State") %>%
   mutate(
     exposure_std = as.numeric(scale(exposure))
   )
 
 # Examine the correspondence between the original exposure and the standardized exposure in each state
 exposure_table <- oil_exposure_cont %>%
   arrange(exposure) %>%
   transmute(
     State,
     exposure_original = exposure,
     exposure_standardized = round(exposure_std, 3)
   )
 
 print(exposure_table, n = Inf)
 
 
 # Step 2: Create Continuous DID Dataset
 #This section converts daily consumption data into panel data for states× weeks 
 # and adds continuous oil exposure for each state.
 did_cont <- affinity %>%
   mutate(
     date = make_date(year, month, day),
     week_end = date + days(7 - wday(date, week_start = 1))
   ) %>%
   inner_join(
     oil_exposure_cont %>%
       select(statefips, exposure, exposure_std),
     by = "statefips"
   ) %>%
   filter(week_end %in% c(pre_weeks, post_weeks)) %>%
   mutate(
     post = as.integer(week_end %in% post_weeks)
   ) %>%
   group_by(statefips, week_end, post, exposure_std) %>%
   summarise(
     spend_nondurables_mean = mean(
       spend_nondurables,
       na.rm = TRUE
     ),
     .groups = "drop"
   ) %>%
   filter(is.finite(spend_nondurables_mean))
 
 
 # Step 3: Keep 27 Pre-war Weeks + 27 Post-war Weeks
 pre_27 <- did_cont %>%
   filter(post == 0) %>%
   distinct(week_end) %>%
   arrange(desc(week_end)) %>%
   slice_head(n = 27) %>%
   pull(week_end)
 
 did_cont_27 <- did_cont %>%
   filter(post == 1 | week_end %in% pre_27)
 
 # 3.4 Descriptive Statistics and Spending Patterns ---------------------
 
 # 1. 准备描述性分析数据 -----------------------------------------------
 
 desc_panel <- did_cont_27 %>%
   mutate(
     week_end = as.Date(week_end),
     spending_pct = 100 * spend_nondurables_mean
   )
 
 # 每个州只保留一条暴露度记录，并限定为当前分析样本中的州
 desc_exposure <- oil_exposure_cont %>%
   semi_join(
     desc_panel %>% distinct(statefips),
     by = "statefips"
   ) %>%
   select(State, statefips, exposure, exposure_std) %>%
   distinct()
 
 # 检查州—周记录是否唯一
 stopifnot(
   !anyDuplicated(desc_panel[c("statefips", "week_end")]),
   !anyDuplicated(desc_exposure$statefips)
 )
 
 
 # 2. 检查样本规模与完整性 ---------------------------------------------
 
 state_coverage <- desc_panel %>%
   group_by(statefips) %>%
   summarise(
     Pre_weeks = n_distinct(week_end[post == 0]),
     Post_weeks = n_distinct(week_end[post == 1]),
     Total_weeks = n_distinct(week_end),
     .groups = "drop"
   ) %>%
   left_join(
     desc_exposure %>% select(statefips, State),
     by = "statefips"
   ) %>%
   select(State, everything())
 
 sample_summary <- desc_panel %>%
   summarise(
     States = n_distinct(statefips),
     Weeks = n_distinct(week_end),
     Observations = n(),
     Pre_observations = sum(post == 0),
     Post_observations = sum(post == 1)
   ) %>%
   mutate(
     Complete_54_week_panel = all(
       state_coverage$Pre_weeks == length(pre_weeks) &
         state_coverage$Post_weeks == length(post_weeks)
     )
   )
 
 print(sample_summary, width = Inf)
 print(state_coverage, n = Inf)
 
 # 若有州未覆盖完整窗口，单独列出
 incomplete_states <- state_coverage %>%
   filter(
     Pre_weeks != length(pre_weeks) |
       Post_weeks != length(post_weeks)
   )
 
 print(incomplete_states)
 
 
 # 3. 描述性统计表 -----------------------------------------------------
 
 # 统一计算统计量
 describe_variable <- function(x, variable, unit, level) {
   
   x <- x[is.finite(x)]
   
   tibble(
     Variable = variable,
     Unit = unit,
     Level = level,
     N = length(x),
     Mean = if (length(x) > 0) mean(x) else NA_real_,
     SD = if (length(x) > 1) sd(x) else NA_real_,
     Min = if (length(x) > 0) min(x) else NA_real_,
     Median = if (length(x) > 0) median(x) else NA_real_,
     Max = if (length(x) > 0) max(x) else NA_real_
   )
 }
 
 descriptive_table <- bind_rows(
   
   describe_variable(
     desc_panel$spending_pct,
     "Non-durable spending: full sample",
     "% change from baseline",
     "State-week"
   ),
   
   describe_variable(
     desc_panel$spending_pct[desc_panel$post == 0],
     "Non-durable spending: pre-war",
     "% change from baseline",
     "State-week"
   ),
   
   describe_variable(
     desc_panel$spending_pct[desc_panel$post == 1],
     "Non-durable spending: post-war",
     "% change from baseline",
     "State-week"
   ),
   
   describe_variable(
     desc_exposure$exposure,
     "Petroleum exposure: 2019-2021 average",
     "Barrels per capita per year",
     "State"
   ),
   
   describe_variable(
     desc_exposure$exposure_std,
     "Standardized petroleum exposure",
     "Standard deviations",
     "State"
   )
 )
 
 descriptive_table_display <- descriptive_table %>%
   mutate(
     across(
       c(Mean, SD, Min, Median, Max),
       ~ round(.x, 3)
     )
   )
 
 print(descriptive_table_display, width = Inf)
 
 
 # 4. 战前石油暴露度分布图 ---------------------------------------------
 
 p_exposure_distribution <- ggplot(
   desc_exposure,
   aes(x = exposure)
 ) +
   geom_histogram(
     bins = 12,
     fill = "#4477AA",
     color = "white"
   ) +
   geom_vline(
     xintercept = mean(desc_exposure$exposure),
     linetype = "dashed",
     color = "#BB4444",
     linewidth = 0.8
   ) +
   labs(
     title = "Distribution of Pre-war Petroleum Exposure",
     subtitle = "Average petroleum consumption per capita, 2019-2021",
     x = "Petroleum consumption (barrels per capita per year)",
     y = "Number of states",
     caption = paste(
       "Each state appears once.",
       "The dashed line indicates the sample mean."
     )
   ) +
   theme_minimal(base_size = 12) +
   theme(
     panel.grid.minor = element_blank(),
     plot.title = element_text(face = "bold")
   )
 
 print(p_exposure_distribution)
 
 
 # 5. 各州暴露度排序图 -------------------------------------------------
 
 p_exposure_states <- ggplot(
   desc_exposure,
   aes(
     x = exposure,
     y = reorder(State, exposure)
   )
 ) +
   geom_point(
     color = "#4477AA",
     size = 2
   ) +
   labs(
     title = "Pre-war Petroleum Exposure by State",
     subtitle = "Average petroleum consumption per capita, 2019-2021",
     x = "Petroleum consumption (barrels per capita per year)",
     y = NULL
   ) +
   theme_minimal(base_size = 11) +
   theme(
     panel.grid.minor = element_blank(),
     panel.grid.major.y = element_blank(),
     axis.text.y = element_text(size = 8),
     plot.title = element_text(face = "bold")
   )
 
 print(p_exposure_states)
 
 
 # 6. 计算每周各州等权平均支出 -----------------------------------------
 
 weekly_spending_summary <- desc_panel %>%
   group_by(week_end, post) %>%
   summarise(
     Mean_spending = mean(spending_pct),
     States_available = n_distinct(statefips),
     .groups = "drop"
   ) %>%
   arrange(week_end)
 
 # 检查每周参与平均的州数量是否一致
 print(
   weekly_spending_summary %>%
     summarise(
       Minimum_states = min(States_available),
       Maximum_states = max(States_available)
     )
 )
 
 
 # 7. 消费支出走势图 ---------------------------------------------------
 
 p_spending_trend <- ggplot() +
   
   # 标记被排除的过渡周：2022年2月21日至27日
   annotate(
     "rect",
     xmin = as.Date("2022-02-21"),
     xmax = as.Date("2022-02-28"),
     ymin = -Inf,
     ymax = Inf,
     fill = "grey70",
     alpha = 0.3
   ) +
   
   # 每个州的支出走势
   # 按post分段，避免跨越被排除的过渡周连接
   geom_line(
     data = desc_panel,
     aes(
       x = week_end,
       y = spending_pct,
       group = interaction(statefips, post)
     ),
     color = "grey65",
     linewidth = 0.35,
     alpha = 0.45
   ) +
   
   # 各州等权平均
   geom_line(
     data = weekly_spending_summary,
     aes(
       x = week_end,
       y = Mean_spending,
       group = post
     ),
     color = "#225588",
     linewidth = 1.2
   ) +
   
   # 战争发生日期
   geom_vline(
     xintercept = as.numeric(as.Date("2022-02-24")),
     linetype = "dashed",
     color = "#BB4444",
     linewidth = 0.7
   ) +
   
   scale_x_date(
     date_breaks = "2 months",
     date_labels = "%b %Y"
   ) +
   
   labs(
     title = "Nominal Non-durable Consumer Spending",
     subtitle = "27 pre-war weeks and 27 post-war weeks",
     x = "Week ending",
     y = "Spending change from baseline (%)",
     caption = paste(
       "Grey lines: individual states; blue line: equal-weight state mean.",
       "\nRed dashed line: 24 February 2022; shaded area: excluded transition week.",
       "\nBaseline: 6 January-2 February 2020. This is not week-on-week growth."
     )
   ) +
   
   theme_minimal(base_size = 12) +
   theme(
     panel.grid.minor = element_blank(),
     plot.title = element_text(face = "bold"),
     plot.caption = element_text(hjust = 0, size = 9)
   )
 
 print(p_spending_trend)
 
 

 
 
 # Continuous DID:Pre-trend stute Test -----------------------------
 
 # 1. 提取连续DID的战前27周
 pre_cont <- did_cont_27 %>%
   filter(post == 0) %>%
   mutate(week_end = as.Date(week_end)) %>%
   select(
     statefips, week_end,
     exposure_std, spend_nondurables_mean
   )
 
 # 检查周数、重复记录和有效值
 stopifnot(
   n_distinct(pre_cont$week_end) == 27L,
   !anyDuplicated(pre_cont[c("statefips", "week_end")]),
   all(is.finite(pre_cont$exposure_std)),
   all(is.finite(pre_cont$spend_nondurables_mean))
 )
 
 # 检查各州有效周数
 pre_cont %>%
   count(statefips, name = "n_weeks") %>%
   print(n = Inf)
 
 # 2. 使用有完整27周的州
 pre_cont <- pre_cont %>%
   group_by(statefips) %>%
   filter(n() == 27L) %>%
   ungroup()
 # 3. 最后战前周为参考周
 ref_week <- max(pre_cont$week_end)
 
 reference_data <- pre_cont %>%
   filter(week_end == ref_week) %>%
   transmute(
     statefips,
     spend_ref = spend_nondurables_mean
   )
 
 # 4. 计算各战前周相对于参考周的指标变化
 pre_cont <- pre_cont %>%
   left_join(reference_data, by = "statefips") %>%
   mutate(
     pre_change = spend_nondurables_mean - spend_ref,
     weeks_before = as.integer(
       as.numeric(week_end - ref_week) / 7
     )
   ) %>%
   # 参考周差值机械为0，剔除
   filter(weeks_before < 0)
 
 
 # pre_cont 来自前面第4步：
 # 已包含 pre_change、exposure_std、statefips、weeks_before
 # 已剔除参考周，并保留战前27周完整的州
 
 stute_data <- pre_cont %>%
   mutate(
     # 将26个待检验周编号为1至26
     time_id = dense_rank(week_end)
   ) %>%
   arrange(statefips, time_id) %>%
   as.data.frame()
 
 # 检查有效值、重复记录及平衡面板
 stopifnot(
   all(is.finite(stute_data$pre_change)),
   all(is.finite(stute_data$exposure_std)),
   !anyDuplicated(stute_data[c("statefips", "time_id")])
 )
 
 panel_check <- stute_data %>%
   count(statefips, name = "n_periods")
 
 stopifnot(all(panel_check$n_periods == 26L))
 
 # Stute nonparametric pretest
 set.seed(2026)
 
 stute_pre <- StuteTest::stute_test(
   df = stute_data,
   Y = "pre_change",
   D = "exposure_std",
   group = "statefips",
   time = "time_id",
   order = 0,
   brep = 1999
 )
 
 # 各周结果及联合检验
 print(stute_pre)
 
 # 单独查看联合检验
 stute_pre$joint

 # Supplement: Wald pre-trend regression and rank check -----------------
 
 # 1. 提取战前数据，并保持与 Stute 相同的州样本
 # 从 did_cont_27 提取，可保留已被 pre_cont 剔除的参考周
 pre_wald <- did_cont_27 %>%
   filter(post == 0) %>%
   semi_join(
     pre_cont %>% distinct(statefips),
     by = "statefips"
   ) %>%
   mutate(
     week_end = as.Date(week_end),
     
     # 最后战前周为 -1，之前依次为 -2、-3……
     event_week = as.integer(
       as.numeric(week_end - ref_week) / 7
     ) - 1L
   )
 
 # 检查州—周是否重复，以及每州是否有完整27周
 stopifnot(
   !anyDuplicated(pre_wald[c("statefips", "week_end")]),
   all((pre_wald %>% count(statefips))$n == 27L)
 )
 
 # 2. 估计战前暴露度交互回归
 m_pre_wald <- fixest::feols(
   spend_nondurables_mean ~
     i(event_week, exposure_std, ref = -1) |
     statefips + week_end,
   data = pre_wald
 )
 
 # 3. 计算按州聚类的协方差矩阵
 # 关闭自动修正，以检查未经修正的矩阵
 V_cluster <- vcov(
   m_pre_wald,
   vcov = ~statefips,
   vcov_fix = FALSE
 )
 
 # 4. 选出需要联合检验的战前交互系数
 idx <- grep(
   "^event_week::",
   names(coef(m_pre_wald))
 )
 
 stopifnot(length(idx) > 0L)
 
 V_test <- V_cluster[idx, idx, drop = FALSE]
 
 # 5. 比较限制数量与矩阵的数值秩
 q <- nrow(V_test)
 rank_test <- as.integer(Matrix::rankMatrix(V_test))
 G <- n_distinct(pre_wald$statefips)
 
 rank_result <- tibble(
   States = G,
   Restrictions = q,
   Covariance_rank = rank_test,
   Full_rank = (rank_test == q)
 )
 
 print(rank_result)
 
 # 6. 只有满秩时，才继续常规联合 Wald 检验
 if (rank_test == q) {
   
   wald_pre_result <- fixest::wald(
     m_pre_wald,
     keep = "^event_week::",
     vcov = V_cluster
   )
   
 } else {
   
   message(
     "协方差矩阵秩不足，跳过常规联合 Wald 检验。"
   )
   
 }
 # 联合 Wald 检验：所有战前暴露度交互系数是否同时为零
 wald_pre_result <- fixest::wald(
   m_pre_wald,
   keep = "^event_week::",
   vcov = V_cluster
 )
 
 # 显示检验结果
 print(wald_pre_result)
 
 
 
 
 # 1. 使用Stute检验中的全部州
 state_lines <- stute_data %>%
   transmute(
     statefips,
     week_end = as.Date(week_end),
     spending_change_pp = 100 * pre_change
   )
 
 # 2. 补回参考周：各州差值按定义为0
 reference_rows <- stute_data %>%
   distinct(statefips) %>%
   mutate(
     week_end = as.Date(ref_week),
     spending_change_pp = 0
   )
 
 state_lines <- bind_rows(
   state_lines,
   reference_rows
 ) %>%
   arrange(statefips, week_end)
 
 # 显示实际绘制的州数量
 n_states <- n_distinct(state_lines$statefips)
 message("Number of states plotted: ", n_states)
 
 # 3. 每周所有州的等权平均
 mean_line <- state_lines %>%
   group_by(week_end) %>%
   summarise(
     mu_t = mean(spending_change_pp),
     .groups = "drop"
   )
 
 # 4. 每个州一条线，黑色虚线为全体州平均
 p_all_states <- ggplot() +
   geom_hline(
     yintercept = 0,
     color = "grey85",
     linewidth = 0.4
   ) +
   geom_line(
     data = state_lines,
     aes(
       x = week_end,
       y = spending_change_pp,
       group = statefips,
       color = factor(statefips)
     ),
     linewidth = 0.5,
     alpha = 0.55,
     show.legend = FALSE
   ) +
   geom_line(
     data = mean_line,
     aes(
       x = week_end,
       y = mu_t,
       linetype = "All-state mean"
     ),
     color = "black",
     linewidth = 1.2
   ) +
   scale_linetype_manual(
     name = NULL,
     values = c("All-state mean" = "dashed")
   ) +
   scale_x_date(
     date_breaks = "1 month",
     date_labels = "%b %Y"
   ) +
   labs(
     title = paste0(
       "Pre-war Spending Changes: ",
       n_states, " States"
     ),
     subtitle = paste(
       "Reference week:",
       format(as.Date(ref_week), "%d %b %Y")
     ),
     x = "Date",
     y = "Spending change (percentage points)",
     caption = paste(
       "Each colored line represents one state.",
       "The black dashed line is the equal-weighted mean."
     )
   ) +
   theme_minimal(base_size = 12) +
   theme(
     legend.position = "bottom",
     panel.grid.minor = element_blank()
   )
 
 print(p_all_states)
 
 # Post-war Stute Test and Spending Changes Plot ------------------------
 
 # 1. Keep post-war observations for the states in the pre-war test
 post_cont <- did_cont_27 %>%
   filter(post == 1) %>%
   semi_join(
     stute_data %>% distinct(statefips),
     by = "statefips"
   ) %>%
   mutate(week_end = as.Date(week_end)) %>%
   select(
     statefips, week_end,
     exposure_std, spend_nondurables_mean
   )
 
 # Check that the same states have all 27 post-war weeks
 stopifnot(
   !anyDuplicated(post_cont[c("statefips", "week_end")]),
   all(is.finite(post_cont$spend_nondurables_mean)),
   all(is.finite(post_cont$exposure_std)),
   setequal(
     unique(post_cont$statefips),
     unique(stute_data$statefips)
   ),
   all((post_cont %>% count(statefips))$n == 27L),
   setequal(unique(post_cont$week_end), post_weeks)
 )
 
 
 # 2. Compare each post-war week with the SAME pre-war reference week
 # reference_data and ref_week already exist in your pre-war code
 post_cont <- post_cont %>%
   left_join(reference_data, by = "statefips") %>%
   mutate(
     post_change = spend_nondurables_mean - spend_ref,
     time_id = dense_rank(week_end)
   )
 
 stopifnot(all(is.finite(post_cont$post_change)))
 
 stute_post_data <- post_cont %>%
   arrange(statefips, time_id) %>%
   as.data.frame()
 
 
 # 3. Run the post-war Stute test
 # Same settings as your pre-war test
 set.seed(2026)
 
 stute_post <- StuteTest::stute_test(
   df = stute_post_data,
   Y = "post_change",
   D = "exposure_std",
   group = "statefips",
   time = "time_id",
   order = 0,
   brep = 1999
 )
 
 print(stute_post)
 
 # Joint test across all 27 post-war weeks
 stute_post$joint
 
 
 # 4. Prepare state-level lines in percentage points
 post_state_lines <- post_cont %>%
   transmute(
     statefips,
     week_end,
     spending_change_pp = 100 * post_change
   )
 
 # Equal-weighted mean across states in each week
 post_mean_line <- post_state_lines %>%
   group_by(week_end) %>%
   summarise(
     mu_t = mean(spending_change_pp),
     .groups = "drop"
   )
 
 
 # 5. Plot: same style as your pre-war figure
 p_post_states <- ggplot() +
   geom_hline(
     yintercept = 0,
     color = "grey65",
     linetype = "dotted",
     linewidth = 0.5
   ) +
   geom_line(
     data = post_state_lines,
     aes(
       x = week_end,
       y = spending_change_pp,
       group = statefips,
       color = factor(statefips)
     ),
     linewidth = 0.5,
     alpha = 0.55,
     show.legend = FALSE
   ) +
   geom_line(
     data = post_mean_line,
     aes(
       x = week_end,
       y = mu_t,
       linetype = "All-state mean"
     ),
     color = "black",
     linewidth = 1.2
   ) +
   scale_linetype_manual(
     name = NULL,
     values = c("All-state mean" = "dashed")
   ) +
   scale_x_date(
     date_breaks = "1 month",
     date_labels = "%b %Y"
   ) +
   labs(
     title = paste0(
       "Post-war Spending Changes: ",
       n_distinct(post_state_lines$statefips),
       " States"
     ),
     subtitle = paste(
       "Reference week:",
       format(as.Date(ref_week), "%d %b %Y")
     ),
     x = "Date",
     y = "Spending change (percentage points)",
     caption = paste(
       "Changes relative to the pre-war reference week.",
       "\nEach colored line represents one state.",
       "The black dashed line is the equal-weighted mean."
     )
   ) +
   theme_minimal(base_size = 12) +
   theme(
     legend.position = "bottom",
     panel.grid.minor = element_blank()
   )
 
 print(p_post_states)
 
 
 

 # Part D: Baseline DID Regression --------------------------------------------
 # Step 0: Keep 27 pre-war weeks and 27 post-war weeks
 pre_27 <- did_weekly %>%
   filter(post == 0) %>%
   distinct(week_end) %>%
   arrange(desc(week_end)) %>%
   slice_head(n = 27) %>%
   pull(week_end)
 
 # Create balanced time-window dataset
 did_weekly_27 <- did_weekly %>%
   filter(post == 1 | week_end %in% pre_27)
 
 
 # Step 1: Check the DID dataset
 did_weekly_27 %>%
   group_by(high, post) %>%
   summarise(
     observations = n(),
     n_weeks = n_distinct(week_end),
     mean_spending = mean(
       spend_nondurables_mean,
       na.rm = TRUE
     ),
     .groups = "drop"
   )
 
 
 # Step 2: Estimate the baseline DID model
 model_did_27 <- feols(
   spend_nondurables_mean ~ high:post |
     statefips + week_end,
   data = did_weekly_27,
   cluster = ~statefips
 )
 
 summary(model_did_27)

 
 
 
## Part e: robutness --------------------------------------------
 # # PADD Gasoline Prices + Continuous DID


 # 27 Pre-war weeks + 27 Post-war weeks

 

 # STEP 1: Read EIA Gasoline Price Data
 
 file <- "PET_PRI_GND_A_EPM0_PTE_DPGAL_W.xls"
 
 # Gasoline data are in sheet "Data 2"
 # First 3 rows contain headers
 gas_raw <- read_excel(
   file,
   sheet = "Data 2",
   skip = 3,
   col_names = FALSE,
   .name_repair = "minimal"
 )
 
 # Check imported dataset
 dim(gas_raw)
 head(gas_raw)
 
 # STEP 2: Select Seven PADD Regions
 
 # Excel columns:
 # A = Date
 # C = New England (PADD 1A)
 # D = Central Atlantic (PADD 1B)
 # E = Lower Atlantic (PADD 1C)
 # F = Midwest (PADD 2)
 # G = Gulf Coast (PADD 3)
 # H = Rocky Mountain (PADD 4)
 # I = West Coast (PADD 5)
 
 gas_padd <- gas_raw %>%
   select(
     date = 1,
     `1A` = 3,
     `1B` = 4,
     `1C` = 5,
     `2` = 6,
     `3` = 7,
     `4` = 8,
     `5` = 9
   )
 
 # STEP 3: Convert Dates and Prices
 
 # Handle Date, Excel numeric dates, and text dates
 convert_eia_date <- function(x) {
   
   if (inherits(x, "Date") ||
       inherits(x, "POSIXt")) {
     return(as.Date(x))
   }
   
   x <- trimws(as.character(x))
   
   result <- suppressWarnings(as.Date(
     x,
     tryFormats = c(
       "%Y-%m-%d",
       "%b %d, %Y",
       "%B %d, %Y",
       "%m/%d/%Y",
       "%m/%d/%y"
     )
   ))
   
   numeric_x <- suppressWarnings(as.numeric(x))
   
   idx <- is.na(result) &
     !is.na(numeric_x) &
     numeric_x > 30000 &
     numeric_x < 60000
   
   result[idx] <- as.Date(
     numeric_x[idx],
     origin = "1899-12-30"
   )
   
   result
 }
 
 gas_padd <- gas_padd %>%
   mutate(
     date = convert_eia_date(date)
   ) %>%
   pivot_longer(
     cols = -date,
     names_to = "padd",
     values_to = "gas_price"
   ) %>%
   mutate(
     gas_price = as.numeric(gas_price),
     
     # EIA reports weekly observations dated Monday
     # Match them to the Sunday of the same week
     week_end = ceiling_date(
       date,
       unit = "week",
       week_start = 1
     ) - days(1)
   ) %>%
   filter(
     !is.na(date),
     is.finite(gas_price)
   )
 

 # STEP 4: Restrict Research Period
 
 # 27 weeks before the war
 # 2021-08-22 to 2022-02-20
 
 pre_27 <- seq(
   as.Date("2021-08-22"),
   as.Date("2022-02-20"),
   by = "week"
 )
 
 # 27 weeks after the war
 # 2022-03-06 to 2022-09-04
 
 post_27 <- seq(
   as.Date("2022-03-06"),
   as.Date("2022-09-04"),
   by = "week"
 )
 
 target_weeks <- c(pre_27, post_27)
 
 gas_padd <- gas_padd %>%
   filter(
     week_end %in% target_weeks
   ) %>%
   arrange(padd, week_end)
 
 # Prevent duplicate PADD-week combinations
 stopifnot(
   !anyDuplicated(gas_padd[c("padd", "week_end")])
 )
 

 # STEP 5: Check Gasoline Data Completeness
 
 gas_check <- gas_padd %>%
   group_by(padd) %>%
   summarise(
     n_weeks = n_distinct(week_end),
     first_week = min(week_end),
     last_week = max(week_end),
     .groups = "drop"
   )
 
 print(gas_check)
 
 # Each of seven PADD regions must have 54 weeks
 expected_panel <- expand_grid(
   padd = c("1A", "1B", "1C", "2", "3", "4", "5"),
   week_end = target_weeks
 )
 
 missing_panel <- expected_panel %>%
   anti_join(
     gas_padd,
     by = c("padd", "week_end")
   )
 
 print(missing_panel)
 

 # STEP 6: Match States to PADD Regions
 
 state_padd <- tibble(
   State = state.abb
 ) %>%
   mutate(
     padd = case_when(
       
       # PADD 1A - New England
       State %in% c(
         "CT", "ME", "MA", "NH", "RI", "VT"
       ) ~ "1A",
       
       # PADD 1B - Central Atlantic
       State %in% c(
         "DE", "MD", "NJ", "NY", "PA"
       ) ~ "1B",
       
       # PADD 1C - Lower Atlantic
       State %in% c(
         "FL", "GA", "NC", "SC", "VA", "WV"
       ) ~ "1C",
       
       # PADD 2 - Midwest
       State %in% c(
         "IL", "IN", "IA", "KS", "KY",
         "MI", "MN", "MO", "NE", "ND",
         "OH", "OK", "SD", "TN", "WI"
       ) ~ "2",
       
       # PADD 3 - Gulf Coast
       State %in% c(
         "AL", "AR", "LA", "MS", "NM", "TX"
       ) ~ "3",
       
       # PADD 4 - Rocky Mountain
       State %in% c(
         "CO", "ID", "MT", "UT", "WY"
       ) ~ "4",
       
       # PADD 5 - West Coast
       State %in% c(
         "AK", "AZ", "CA", "HI", "NV",
         "OR", "WA"
       ) ~ "5",
       
       TRUE ~ NA_character_
     )
   ) %>%
   left_join(
     state_lookup %>%
       distinct(State, statefips),
     by = "State"
   )
 
 # Check state mapping
 stopifnot(
   nrow(state_padd) == 50,
   !anyNA(state_padd$padd),
   !anyNA(state_padd$statefips),
   !anyDuplicated(state_padd$statefips)
 )
 
 state_padd %>%
   count(padd)
 

 # STEP 7: Create State-Week Gasoline Dataset
 
 # States within the same PADD share the same
 # regional gasoline price in each week.
 
 gas_state_weekly <- state_padd %>%
   inner_join(
     gas_padd,
     by = "padd",
     relationship = "many-to-many"
   ) %>%
   select(
     statefips,
     State,
     padd,
     week_end,
     gas_price
   ) %>%
   arrange(
     statefips,
     week_end
   )

 
 # Check duplicated state-week observations
 stopifnot(
   !anyDuplicated(
     gas_state_weekly[c("statefips", "week_end")]
   )
 )
 
 

 # STEP 8: Merge Gasoline Prices with DID Dataset
 
 # did_cont_27 was created in your previous code
 
 did_gas <- did_cont_27 %>%
   filter(
     week_end %in% target_weeks
   ) %>%
   left_join(
     gas_state_weekly %>%
       select(
         statefips,
         week_end,
         padd,
         gas_price
       ),
     by = c("statefips", "week_end"),
     relationship = "many-to-one"
   )
 
 # Check data integrity
 did_gas %>%
   summarise(
     n_states = n_distinct(statefips),
     n_weeks = n_distinct(week_end),
     observations = n(),
     missing_gas = sum(is.na(gas_price))
   ) %>%
   print()
 
 stopifnot(
   !anyDuplicated(
     did_gas[c("statefips", "week_end")]
   ),
   !anyNA(did_gas$gas_price)
 )
 
 # Additional time-window check
 did_gas %>%
   group_by(post) %>%
   summarise(
     states = n_distinct(statefips),
     weeks = n_distinct(week_end),
     observations = n(),
     .groups = "drop"
   ) %>%
   print()
 

 # STEP 9: Baseline Continuous DID
 # Without Gasoline Price Control
 
 model_baseline <- feols(
   spend_nondurables_mean ~
     exposure_std:post |
     statefips + week_end,
   data = did_gas,
   cluster = ~statefips
 )
 
 summary(model_baseline)
 
 
 # STEP 10: DID with PADD Gasoline Price Control
 
 model_gas <- feols(
   spend_nondurables_mean ~
     exposure_std:post + gas_price |
     statefips + week_end,
   data = did_gas,
   cluster = ~statefips
 )
 
 summary(model_gas)
 

 # STEP 11: Compare DID Results
 
 etable(
   model_did_27,
   model_baseline,
   model_gas,
   digits = 4,
   headers = c("true baseline",
     "Baseline DID",
     "DID + Gas Price"
   )
 )
 

 # STEP 12: Clean Regression Results
 
 did_result_gas <- tibble(
   Variable = names(coef(model_gas)),
   Coefficient = as.numeric(coef(model_gas)),
   Std_Error = as.numeric(se(model_gas)),
   P_value = as.numeric(pvalue(model_gas)),
   CI_Lower = confint(model_gas)[, 1],
   CI_Upper = confint(model_gas)[, 2]
 )
 
 print(did_result_gas)
 
 # STEP 12B: Continuous DID Using 2019 Exposure -------------------
 
 # 1. 提取2019年人均石油消费量
 oil_2019 <- oil %>%
   transmute(
     State,
     exposure_2019 = as.numeric(`2019`)
   ) %>%
   filter(
     State %in% state.abb,
     is.finite(exposure_2019)
   ) %>%
   left_join(state_lookup, by = "State")
 
 # 2. 加入已有连续DID面板，保留共同样本
 did_gas_2019 <- did_gas %>%
   inner_join(
     oil_2019 %>%
       select(statefips, exposure_2019),
     by = "statefips"
   )
 
 # 确保没有因更换暴露度而改变样本
 stopifnot(
   nrow(did_gas_2019) == nrow(did_gas),
   !anyDuplicated(did_gas_2019[c("statefips", "week_end")])
 )
 
 # 3. 使用与原模型相同的标准差作为计量单位
 # 两种暴露度都按“增加2019–2021暴露度的1个标准差”解释
 baseline_sd <- sd(oil_exposure_cont$exposure)
 
 stopifnot(
   is.finite(baseline_sd),
   baseline_sd > 0
 )
 
 did_gas_2019 <- did_gas_2019 %>%
   mutate(
     exposure_2019_scaled =
       (exposure_2019 - mean(oil_2019$exposure_2019)) /
       baseline_sd
   )
 
 # 4. 2019年暴露度：不加入汽油价格
 model_2019 <- feols(
   spend_nondurables_mean ~ exposure_2019_scaled:post |
     statefips + week_end,
   data = did_gas_2019,
   cluster = ~statefips
 )
 
 summary(model_2019)
 
 # 5. 2019年暴露度：加入已有的汽油价格控制
 model_2019_gas <- feols(
   spend_nondurables_mean ~
     exposure_2019_scaled:post + gas_price |
     statefips + week_end,
   data = did_gas_2019,
   cluster = ~statefips
 )
 
 summary(model_2019_gas)
 
 # 6. 比较原定义和2019年定义
 etable(model_did_27,
   model_baseline,
   model_2019,
   model_gas,
   model_2019_gas,
   digits = 4,
   headers = c(
     "basic did 2019-2021",
     "continuous 2019–2021",
     "2019 only",
     "2019–2021 + Gas",
     "2019 only + Gas"
   ),
   dict = c(
     "exposure_std:post" = "Exposure × Post",
     "exposure_2019_scaled:post" = "Exposure × Post"
   )
 )
 
 # 7. 整理2019年模型的结果
 did_result_2019 <- tibble(
   Variable = names(coef(model_2019)),
   Coefficient = as.numeric(coef(model_2019)),
   Std_Error = as.numeric(se(model_2019)),
   P_value = as.numeric(pvalue(model_2019)),
   CI_Lower = confint(model_2019)[, 1],
   CI_Upper = confint(model_2019)[, 2]
 )
 
 print(did_result_2019)
 
 
 # STEP 13: Export Processed Data
 
 write.csv(
   gas_padd,
   "gas_padd_27pre_27post.csv",
   row.names = FALSE
 )
 
 write.csv(
   did_gas,
   "did_with_gas_price.csv",
   row.names = FALSE
 )
 
 
#the diagram
 state_change <- did_cont_27 %>%
   filter(is.finite(spend_nondurables_mean)) %>%
   group_by(statefips, post) %>%
   summarise(
     avg_spending = mean(spend_nondurables_mean),
     n_weeks = n_distinct(week_end),
     .groups = "drop"
   ) %>%
   pivot_wider(
     names_from = post,
     values_from = c(avg_spending, n_weeks),
     names_prefix = "period_"
   ) %>%
   # 先用战前、战后都有完整27周的州作图
   filter(
     n_weeks_period_0 == 27,
     n_weeks_period_1 == 27
   ) %>%
   mutate(
     spending_change =
       avg_spending_period_1 - avg_spending_period_0
   ) %>%
   left_join(
     oil_exposure_cont %>%
       select(statefips, State, exposure, exposure_std),
     by = "statefips"
   )
 
 print(state_change)
 ggplot(
   state_change,
   aes(x = exposure, y = spending_change)
 ) +
   geom_hline(
     yintercept = 0,
     linetype = "dotted",
     color = "grey50"
   ) +
   geom_point(size = 2.5, alpha = 0.8) +
   
   # 蓝色虚线：线性关系
   geom_smooth(
     method = "lm",
     se = FALSE,
     color = "blue",
     linetype = "dashed"
   ) +
   
   labs(
     title = "Oil Exposure and Pre–Post Spending Changes",
     x = "Pre-war petroleum consumption (barrels per capita per year)",
     y = "Post-war mean spending minus pre-war mean spending"
   ) +
   theme_minimal()



 

 
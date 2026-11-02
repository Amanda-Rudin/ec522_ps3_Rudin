library(tidyverse)
library(fpp3)
retail_raw <- read_csv("data/retail_sales_raw.csv")
head(retail_raw)
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)
head(retail_raw)
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")
head(retail_raw_long)
head(retail_raw)
retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)
head(retail_ts)
has_gaps(retail_ts)
head(canadian_gas)
key_vars(canadian_gas)
index_var(canadian_gas)
can_gas<- canadian_gas |>
  select(Month, Volume)
has_gaps(can_gas)
head(can_gas)
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))
head(can_gas_recent)
can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))
can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))
can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)
#We sum the monthly values because if we took their mean. we would still be in a monthly unit or scale of the data. If we sum it, we are now in the quarterly units of gas. The series still appears to be seasonal - the variation within quarters doesn't exist anymore, but variation between quarters does. There looks to be a repeating seasonal cycle.
retail_ts |> autoplot(sales)
retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()
#The NSA series has a spike right at the end of the year, followed by a drop right at the beginning of the year. The series then reaches a middle ground for most of the rest of the year (while it is still bumpy, but not as large as the spikes). The SA series does not seem to have these spikes at the beginning and end of the year. 
#Overall, the series follow the same overall trends such as recessions and booms. This can be seen with both series dipping around 2009, and increases around 2021 ish.
can_gas |> 
  autoplot(Volume) +
  labs(
    title = "Canadian Gas Production",
    subtitle = "Monthly data from 1960 to 2004",
    x = NULL, y = "Millions of cubic metres", color = NULL
  ) +
  theme_minimal()
retail_ts |>
  autoplot(sales) +
  scale_y_continuous(
    labels = scales::dollar_format(scale = 1e-6, suffix = "M")
  )+
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()
#I added scale_y_continuous to make the Y axis in dollar format. I also added a suffix of M to show that the values are in the millions. Both of these make the Y axis more readable.
#The size of the swings changes over time, but doesn't necesarily always grow as the lvel grows. From 1970 to 1990, the swings grow, but they get smaller again post 1990.
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))
can_gas |> 
  autoplot(log_volume) +
  labs(
    title = "Canadian Gas Production",
    subtitle = "Monthly data",
    x = NULL, y = "Log of Millions of cubic metres", color = NULL
  ) +
  theme_minimal()
#The logged series swings are smaller than the original series, making the series smoother. Also, the logged series looks like steep growth, with a slowing of growth towards the end. The original series looks like steep growth, then slower growth, then steep again.
#I think logs are not a good idea for can_gas. On the logged graph. the swings still grow and shrink in size (being very large for the first few years, large again from 1980-1990 ish, and smaller post-2000). Due to this, I'm assuming seasonality isn't proportional to the level - so logging the series is not a perfect fix.
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))
retail_nsa |>
  autoplot(sales) +
  scale_y_continuous(
    labels = scales::dollar_format(scale = 1e-6, suffix = "M")
  )+
  labs(
    title = "U.S. Retail and Food Services Sales",
    x = NULL, y = "Sales (Millions of dollars)"
    ) +
  theme_minimal()
retail_nsa |>
  autoplot(log_sales) +
  labs(
    title = "U.S. Retail and Food Services Sales (NSA, log scale)",
    x = NULL, y = "log(Sales)"
  ) +
  theme_minimal()
#The case for logging this series is stronger than it was for can_gas. The non-logged series' swings grow over time, where the swings in the logged series stay the same size over time.
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )
can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Canadian Gas Production Growth Direct",
    x = NULL, y = "% change from prev month"
  ) +
  theme_minimal()
can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))
can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Gap Btwn Direct and Log-Diff Growth Rates",
    x = NULL, y = "Absolute difference in % points"
  )+
  theme_minimal()
#I think the log-diff approx is fairly good in this case, as the largest abs diff in % points is only just over 3 percentage points.
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()
#The month-over month growth rate of the SA series is much smaller than the NSA series. The SA series also shows times of greater growth and negative growth, while these trends are much harder to see in the NSA series. To connect back, the NSA series shows the spikes and dips within each year, while the SA series only shows actual growth trends, not seasonality. The NSA series is much noisier, because seasonal trends still exist.
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()
retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()
#In the NSA series, there is a repeating shape between each line/seasonal period. In the SA series, the lines all have their own shapes with no clear repeating trends.
can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Canadian Gas Production: Seasonal Plot (log scale)",
    x = NULL, y = "log(volume)"
  ) +
  theme_minimal()
#The seasonal pattern I am seeing is a higher level of production in Jan-March, a dip from March-Sept, and then a growth until Dec. This matches my intuition about natural gas production, as gas is used for heating in the cooler months.
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()
#The seasonal factor is the highest in December 1990s, with sales being about 20% above trend (factor of about 1.2 in Dec early 1990s). The size of the seasonal swings appears to shrink over time, with Dec being about 1.1. 
sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()
#My series matches the official adjustment quite closely, being around 0.5% off. The adjustments differ the most during covid times (2020-2023 ish). During covid times, the difference is around 1.5%.
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)
can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)
#The average monthly growth rate is about 0.4% per month (0.003973 x 100). The most significant season effect is June (season 6), with the absolute value of the |t-stat| being 4.777.
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()
#The fitted line tracks the actual series moderately okay. It does well from 1964-1970,  and 1981-2000. It visibly misses from 1960-1964 (over predicts), 1970-1980 (under predicts), and 2000 on (over predicts).
can_gas_fit |> gg_tsresiduals()
can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)
#These residuals, according to the ACF, do not look like white noise.There is a visible trend left in these residuals. Each lag is far above the blue bands, and decays very slowly. White noise would be random, uncorrelated spikes. 
#At the 5% level, we reject the H0 of the Ljung Box test. The lb_pvalue is 0, so we reject Ho at the 5% level.
gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

#The first forecast evaluated is 1980 Jan, and the last is 2005 feb. 
#SNAIVE - MAE is 0.563, RMSE is 0.702. TSLM - MAE is 4.01, RMSE is 4.36. The SNAIVE model forecasts more accurately, with the two metrics agreeing.
gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE)) |>
  select(.model, MAE, RMSE) |>
  knitr::kable(digits = 3)
errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)
#DM stat - -31.278, p-val - <2.2e-16 (essentially 0). From the DM test, the H0 is that both models forecast equally accurately. At the 5% level, we reject H0. Since the DM test stat is negative, we can say that the SNAIVE (first model) has smaller errors than the second (TSLM).







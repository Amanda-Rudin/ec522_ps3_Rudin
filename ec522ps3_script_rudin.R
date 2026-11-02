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






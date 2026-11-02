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
    title = "Canadian Gas Consumption",
    subtitle = "Monthly data from 1960 to 2004",
    x = NULL, y = "Millions of cubic metres", color = NULL
  ) +
  theme_minimal()




library(tidyverse)
# 2.2 a
retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

# 2.2 b
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

# 2.3 
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

# 2.4
library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)

has_gaps(retail_ts)

# 2.5
head(canadian_gas)

key_vars(canadian_gas)
index_var(canadian_gas)

# 2.5 b
can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

# 2.6
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

# 2.6 b
can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

# 2.6 c
can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

# 2.6 d
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

# We sum the monthly values because Q1 is Jan, Feb, Mar combined. 
# It is like reporting revenue for a company, you dont just average it you sum it. Same goes for oil production, Q1 consists of 4 months' production
# Yes the quarterly series is still seasonal, it seems like Q2 is consistently less than Q1, could be because of weather, etc.

# 2.7 a

retail_ts |> autoplot(sales)

# 2.7 b

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

# The not seasonally adjusted line is much more volatile, meaning its peaks are much higher and its troughs are much lower than the Seasonal Adjusted (SA)
# For example, the NSA has a large drop every January. The NSA also has large spikes around March every year
# The SA has much smaller spikes here and there, but in general is much more smoothed out and steady
# Yes both follow the same path over time. It looks like the SA is just an average of the NSA, and a more smoothed out way of displaying the sales over time
# The only major drop the SA has is in COVID

# 2.7 c
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Gas Production",
    x = "Date", y = "Volume"
  ) +
  theme_minimal()

# 2.7 d
can_gas |>
  autoplot(Volume) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(
    title = "Canadian Gas Production",
    x = "Date", y = "Volume"
  ) +
  theme_minimal()

# I added geom_smooth which just a fits a statistical model to the data and draws the fitted line on the plot. It gives us a more smoothed out interpretation of the increase in volume production

# 3.1 a
# It looks like the swings get less prominent, with the biggest swings occuring between the 1980s and 1990s. In fact after 1990 the swings are less significant

# 3.1 b

# add a column with the log of the volume
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

# plot the log version
can_gas |>
  autoplot(log_volume) +
  geom_smooth(method = "loess") +
  labs(
    title = "Log of Canadian Gas Production",
    x = "Date", y = "log(Volume)"
  ) +
  theme_minimal()

# Yes think that logs are a good idea for can_gas because even though the log doesn't fully eliminate the seasonal swings in volume, it does smooth out the volume as the time increases, specifically after 1990. It is important to note that if we were to show this graph to a non-expert it would be much harder to interpret, especially considering that the pattern is effectively the same

# 3.1 c

# add a column with the log of the volume
retail_ts <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

# plot the log version
retail_ts |>
  autoplot(log_sales) +
  geom_smooth(method = "loess") +
  labs(
    title = "Log of RSAFSNA Sales",
    x = "Date", y = "log(RSAFSNA Sales)"
  ) +
  theme_minimal()

# I think that logging this series was weaker than it was for can_gas because it doesnt smooth out the spikes as overall sales go up like can_gas did with volume

# 3.2
# add a column with the log of the volume
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

# plot
can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Month-over-Month % Growth",
    x = "Date", y = "% Growth"
  ) +
  theme_minimal()

#
can_gas <- can_gas |>
  mutate(
    growth_gap = abs(growth_direct - growth_logdiff)
  )

can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Absolute Difference Between Growth & Log Growth",
    x = "Date", y = "Difference"
  ) +
  theme_minimal()

# the log difference approximation is good because the difference between it and the direct growth shrinks over time

# 3.3

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

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
retail_ts_na <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

# plot the log version
retail_ts_na |>
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

# The NSA growth rate is much noisier, with most of that being the seasonal pattern described in 2.7b showing up again.
# It makes sense that the Seasonal Adjusted is less noisy as the seasonal aspect is removed. Its month-over-month growth reflects underlying changes in spending, so it stays close to a small positive number most months.

# 4.1 a
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# 4.1 b
retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# The difference is now clear: The seasonal adjusted version just shows the underlying growth rather than the seasonal spikes usually experienced around December. In the NSA, the seasons are relatively following the same pattern but are very up-and-down. The SA is very steady and smooth, like a more straight line.

can_gas |>
  gg_season(log_volume)+
  labs(
    title = "Seasonal Pattern in Log Gas Volume",
    y = "Log Volume"
  ) +
  theme_minimal()

# Because gas is used for heating there is a consistent dip in production around the summer months (June-August) and then an increased demand in the winter months usually peaking around December and January

# 4.2 
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

# 4.3
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

# From the graphs it looks like December has the highest seasonal factor. This is the tall spike at the end of each year. In the early 1990s the factor was about 1.2, meaning December sales were ~20% above the trend. By the 2020s it had fallen to about 1.1, or ~10% above trend.

#4.3 b
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


# It looks like the default X-13 matches up fairly closely with the census bureau's adjustment except for in 2020 which is when COVID occurred. This makes sense because the census bureau probably adjusted for many other factors in 2020 because it was so irregular

# 4.4 a
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

# The estimated average monthly growth rate of gas production is 0.004
# The largest seasonal effect occurs in June with a t stat of -4.777. This makes sense because it is one of the first summer months and will most likely incur the biggest change

# 4.4 b
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

# It does very well between 1980 and 2000 and it visibly misses between 1960 and 1980.

# 4.4 c
can_gas_fit |> gg_tsresiduals()


can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

# These residuals are not white noise, as there is a clear trend of residuals that follows our previous analaysis. There is also a left skewed histogram showing that most residuals around at or very close to 0.
# The Ljung Box stat is 7774 with a p value of 0. So at the 5% level, we reject the null hypothesis that residuals are white noise.







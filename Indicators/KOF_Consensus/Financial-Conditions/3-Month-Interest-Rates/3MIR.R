# ************************************************************************
# 3-month interest rates ----
# ************************************************************************
# Feel free to copy, adapt, and use this code for your own purposes.
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(ggtext)

start_date <- "2005-01-01"
chrecdp <- read_csv(file = "Recession-Dates/Recession-Dates_CEPR_EA_Monthly_Midpoint.csv")

## Access the data ----

# ch.kof.consensus.q_qn_3minterest_[3m,12m].[count,max,mean,median,mean,stdev]

### 3-months ahead ----
kof_cons_3minterest_3m.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_3minterest_3m.mean")
kof_cons_3minterest_3m.mean <- ts(
  data = kof_cons_3minterest_3m.mean$ch.kof.consensus.q_qn_3minterest_3m.mean,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_3minterest_3m.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_3minterest_3m.min")
kof_cons_3minterest_3m.min <- ts(
  data = kof_cons_3minterest_3m.min$ch.kof.consensus.q_qn_3minterest_3m.min,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_3minterest_3m.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_3minterest_3m.max")
kof_cons_3minterest_3m.max <- ts(
  data = kof_cons_3minterest_3m.max$ch.kof.consensus.q_qn_3minterest_3m.max,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_3minterest_3m.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_3minterest_3m.stdev")
kof_cons_3minterest_3m.stdev <- ts(
  data = kof_cons_3minterest_3m.stdev$ch.kof.consensus.q_qn_3minterest_3m.stdev,
  start = c(2001, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(
  kof_cons_3minterest_3m.max, kof_cons_3minterest_3m.mean, kof_cons_3minterest_3m.min, kof_cons_3minterest_3m.stdev
  )) |> 
  mutate(time = date(time)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_3minterest_3m.min, ymax = kof_cons_3minterest_3m.max), fill = "#374e8e", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_3minterest_3m.mean - kof_cons_3minterest_3m.stdev, ymax = kof_cons_3minterest_3m.mean + kof_cons_3minterest_3m.stdev), fill = "#374e8e", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_3minterest_3m.mean), color = "#374e8e", linewidth = 1) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-1, 4)) +
  theme_bw() +
  labs(
    title = "KOF consensus 3-month interest rate forecast",
    subtitle = "3 months ahead",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/Financial-Conditions/3-Month-Interest-Rates/Fig_KOF-Consensus-Month-Interest-Rates_03m.png", width = 8, height = 4)
graphics.off()

### 12 months ahead ----
kof_cons_3minterest_12m.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_3minterest_12m.mean")
kof_cons_3minterest_12m.mean <- ts(
  data = kof_cons_3minterest_12m.mean$ch.kof.consensus.q_qn_3minterest_12m.mean,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_3minterest_12m.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_3minterest_12m.min")
kof_cons_3minterest_12m.min <- ts(
  data = kof_cons_3minterest_12m.min$ch.kof.consensus.q_qn_3minterest_12m.min,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_3minterest_12m.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_3minterest_12m.max")
kof_cons_3minterest_12m.max <- ts(
  data = kof_cons_3minterest_12m.max$ch.kof.consensus.q_qn_3minterest_12m.max,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_3minterest_12m.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_3minterest_12m.stdev")
kof_cons_3minterest_12m.stdev <- ts(
  data = kof_cons_3minterest_12m.stdev$ch.kof.consensus.q_qn_3minterest_12m.stdev,
  start = c(2001, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(
  kof_cons_3minterest_12m.max, kof_cons_3minterest_12m.mean, kof_cons_3minterest_12m.min, kof_cons_3minterest_12m.stdev
  )) |> 
  mutate(time = date(time)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_3minterest_12m.min, ymax = kof_cons_3minterest_12m.max), fill = "#374e8e", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_3minterest_12m.mean - kof_cons_3minterest_12m.stdev, ymax = kof_cons_3minterest_12m.mean + kof_cons_3minterest_12m.stdev), fill = "#374e8e", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_3minterest_12m.mean), color = "#374e8e", linewidth = 1) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-1, 4)) +
  theme_bw() +
  labs(
    title = "KOF consensus 3-month interest rate forecast",
    subtitle = "12 months ahead",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/Financial-Conditions/3-Month-Interest-Rates/Fig_KOF-Consensus-Month-Interest-Rates_12m.png", width = 8, height = 4)
graphics.off()

# END
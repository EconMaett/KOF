# ************************************************************************
# EUR-CHF ----
# ************************************************************************
# Feel free to copy, adapt, and use this code for your own purposes at
# your own risk.
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(ggtext)

start_date <- "2010-01-01"

## Access the data ----
# ch.kof.consensus.q_qn_eurchf_[3m,12m].[count,max,mean,median,mean,stdev]

### 3-months ahead ----
kof_cons_eurchf_3m.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_eurchf_3m.mean")
kof_cons_eurchf_3m.mean <- ts(
  data = kof_cons_eurchf_3m.mean$ch.kof.consensus.q_qn_eurchf_3m.mean,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_eurchf_3m.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_eurchf_3m.min")
kof_cons_eurchf_3m.min <- ts(
  data = kof_cons_eurchf_3m.min$ch.kof.consensus.q_qn_eurchf_3m.min,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_eurchf_3m.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_eurchf_3m.max")
kof_cons_eurchf_3m.max <- ts(
  data = kof_cons_eurchf_3m.max$ch.kof.consensus.q_qn_eurchf_3m.max,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_eurchf_3m.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_eurchf_3m.stdev")
kof_cons_eurchf_3m.stdev <- ts(
  data = kof_cons_eurchf_3m.stdev$ch.kof.consensus.q_qn_eurchf_3m.stdev,
  start = c(2001, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(
  kof_cons_eurchf_3m.max, kof_cons_eurchf_3m.mean, kof_cons_eurchf_3m.min, kof_cons_eurchf_3m.stdev
  )) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_eurchf_3m.min, ymax = kof_cons_eurchf_3m.max), fill = "lightgray", alpha = 0.5) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_eurchf_3m.mean - kof_cons_eurchf_3m.stdev, ymax = kof_cons_eurchf_3m.mean + kof_cons_eurchf_3m.stdev), fill = "darkgrey", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_eurchf_3m.mean), color = "black", linewidth = 1) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0.5, 2)) +
  theme_bw() +
  labs(
    title = "KOF consensus EUR/CHF forecast - 3 months ahead",
    subtitle = "",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/ExchangeRates/EURCHF/KOF_ConsEurCHF_3m.png", width = 8, height = 4)
graphics.off()

### 12 months ahead ----
kof_cons_eurchf_12m.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_eurchf_12m.mean")
kof_cons_eurchf_12m.mean <- ts(
  data = kof_cons_eurchf_12m.mean$ch.kof.consensus.q_qn_eurchf_12m.mean,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_eurchf_12m.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_eurchf_12m.min")
kof_cons_eurchf_12m.min <- ts(
  data = kof_cons_eurchf_12m.min$ch.kof.consensus.q_qn_eurchf_12m.min,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_eurchf_12m.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_eurchf_12m.max")
kof_cons_eurchf_12m.max <- ts(
  data = kof_cons_eurchf_12m.max$ch.kof.consensus.q_qn_eurchf_12m.max,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_eurchf_12m.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_eurchf_12m.stdev")
kof_cons_eurchf_12m.stdev <- ts(
  data = kof_cons_eurchf_12m.stdev$ch.kof.consensus.q_qn_eurchf_12m.stdev,
  start = c(2001, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(
  kof_cons_eurchf_12m.max, kof_cons_eurchf_12m.mean, kof_cons_eurchf_12m.min, kof_cons_eurchf_12m.stdev
  )) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_eurchf_12m.min, ymax = kof_cons_eurchf_12m.max), fill = "lightgray", alpha = 0.5) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_eurchf_12m.mean - kof_cons_eurchf_12m.stdev, ymax = kof_cons_eurchf_12m.mean + kof_cons_eurchf_12m.stdev), fill = "darkgrey", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_eurchf_12m.mean), color = "black", linewidth = 1) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0.5, 2)) +
  theme_bw() +
  labs(
    title = "KOF consensus EUR/CHF forecast - 12 months ahead",
    subtitle = "",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/ExchangeRates/EURCHF/KOF_ConsEurCHF_12m.png", width = 8, height = 4)
graphics.off()

# END
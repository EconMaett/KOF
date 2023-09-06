# ************************************************************************
# Unemployment ----
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
chrecdp <- read_csv(file = "Recession-Dates/Recession-Dates_NBER_US_Daily_Midpoint.csv")

## Access the data ----
# ch.kof.consensus.q_qn_unemp_[cy,ny,5y].[count,max,mean,median,mean,stdev]
# ch.kof.consensus.q_qn_unemp_prob_[cy,ny,5y]_[1-17].[count,max,mean,median,mean,stdev]

### Current year ----
kof_cons_unemp_cy.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_cy.mean")
kof_cons_unemp_cy.mean <- ts(
  data = kof_cons_unemp_cy.mean$ch.kof.consensus.q_qn_unemp_cy.mean,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_unemp_cy.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_cy.min")
kof_cons_unemp_cy.min <- ts(
  data = kof_cons_unemp_cy.min$ch.kof.consensus.q_qn_unemp_cy.min,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_unemp_cy.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_cy.max")
kof_cons_unemp_cy.max <- ts(
  data = kof_cons_unemp_cy.max$ch.kof.consensus.q_qn_unemp_cy.max,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_unemp_cy.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_cy.stdev")
kof_cons_unemp_cy.stdev <- ts(
  data = kof_cons_unemp_cy.stdev$ch.kof.consensus.q_qn_unemp_cy.stdev,
  start = c(2001, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_unemp_cy.max, kof_cons_unemp_cy.mean, kof_cons_unemp_cy.min, kof_cons_unemp_cy.stdev)) |> 
  mutate(time = date(time)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_unemp_cy.min, ymax = kof_cons_unemp_cy.max), fill = "#df7c18", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_unemp_cy.mean - kof_cons_unemp_cy.stdev, ymax = kof_cons_unemp_cy.mean + kof_cons_unemp_cy.stdev), fill = "#df7c18", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_unemp_cy.mean), color = "#df7c18", linewidth = 1) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1)) +
  theme_bw() +
  labs(
    title = "KOF consensus real unemployment rate forecast",
    subtitle = "Current year",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/Unemployment/Fig_KOF_Consensus-Unemplyoment_0y.png", width = 8, height = 4)
graphics.off()

### Next year ----
kof_cons_unemp_ny.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_ny.mean")
kof_cons_unemp_ny.mean <- ts(
  data = kof_cons_unemp_ny.mean$ch.kof.consensus.q_qn_unemp_ny.mean,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_unemp_ny.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_ny.min")
kof_cons_unemp_ny.min <- ts(
  data = kof_cons_unemp_ny.min$ch.kof.consensus.q_qn_unemp_ny.min,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_unemp_ny.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_ny.max")
kof_cons_unemp_ny.max <- ts(
  data = kof_cons_unemp_ny.max$ch.kof.consensus.q_qn_unemp_ny.max,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_unemp_ny.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_ny.stdev")
kof_cons_unemp_ny.stdev <- ts(
  data = kof_cons_unemp_ny.stdev$ch.kof.consensus.q_qn_unemp_ny.stdev,
  start = c(2001, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_unemp_ny.max, kof_cons_unemp_ny.mean, kof_cons_unemp_ny.min, kof_cons_unemp_ny.stdev)) |> 
  mutate(time = date(time)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_unemp_ny.min, ymax = kof_cons_unemp_ny.max), fill = "#df7c18", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_unemp_ny.mean - kof_cons_unemp_ny.stdev, ymax = kof_cons_unemp_ny.mean + kof_cons_unemp_ny.stdev), fill = "#df7c18", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_unemp_ny.mean), color = "#df7c18", linewidth = 1) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1)) +
  theme_bw() +
  labs(
    title = "KOF consensus real unemployment rate forecast",
    subtitle = "Next year",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/Unemployment/Fig_KOF_Consensus-Unemplyoment_1y.png", width = 8, height = 4)
graphics.off()

### Five years ----
kof_cons_unemp_5y.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_5y.mean")
kof_cons_unemp_5y.mean <- ts(
  data = kof_cons_unemp_5y.mean$ch.kof.consensus.q_qn_unemp_5y.mean,
  start = c(2015, 2),
  frequency = 4
)

kof_cons_unemp_5y.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_5y.min")
kof_cons_unemp_5y.min <- ts(
  data = kof_cons_unemp_5y.min$ch.kof.consensus.q_qn_unemp_5y.min,
  start = c(2015, 2),
  frequency = 4
)

kof_cons_unemp_5y.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_5y.max")
kof_cons_unemp_5y.max <- ts(
  data = kof_cons_unemp_5y.max$ch.kof.consensus.q_qn_unemp_5y.max,
  start = c(2015, 2),
  frequency = 4
)

kof_cons_unemp_5y.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_5y.stdev")
kof_cons_unemp_5y.stdev <- ts(
  data = kof_cons_unemp_5y.stdev$ch.kof.consensus.q_qn_unemp_5y.stdev,
  start = c(2015, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_unemp_5y.max, kof_cons_unemp_5y.mean, kof_cons_unemp_5y.min, kof_cons_unemp_5y.stdev)) |> 
  mutate(time = date(time)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_unemp_5y.min, ymax = kof_cons_unemp_5y.max), fill = "#df7c18", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_unemp_5y.mean - kof_cons_unemp_5y.stdev, ymax = kof_cons_unemp_5y.mean + kof_cons_unemp_5y.stdev), fill = "#df7c18", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_unemp_5y.mean), color = "#df7c18", linewidth = 1) +
  scale_x_date(limits = c(date("2015-01-01"), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1)) +
  theme_bw() +
  labs(
    title = "KOF consensus real unemployment rate forecast",
    subtitle = "5 years ahead",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/Unemployment/Fig_KOF_Consensus-Unemplyoment_5y.png", width = 8, height = 4)
graphics.off()

# END
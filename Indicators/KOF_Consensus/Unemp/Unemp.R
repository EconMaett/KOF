# ************************************************************************
# Unemployment ----
#
# Feel free to copy, adapt, and use this code for your own purposes at
# your own risk.
#
# Matthias Spichiger, 2023 (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(ggtext)

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
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_unemp_cy.min, ymax = kof_cons_unemp_cy.max), fill = "lightgray", alpha = 0.5) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_unemp_cy.mean - kof_cons_unemp_cy.stdev, ymax = kof_cons_unemp_cy.mean + kof_cons_unemp_cy.stdev), fill = "darkgray", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_unemp_cy.mean), color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  scale_y_continuous(limits = c(0, 6), breaks = c(0, 1, 2, 3, 4, 5, 6)) +
  theme_bw() +
  labs(
    title = "KOF consensus real unemployment rate forecast - current year",
    subtitle = "",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/Unemp/KOF_ConsUnemp_cy.png", width = 8, height = 4)
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
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_unemp_ny.min, ymax = kof_cons_unemp_ny.max), fill = "lightgray", alpha = 0.5) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_unemp_ny.mean - kof_cons_unemp_ny.stdev, ymax = kof_cons_unemp_ny.mean + kof_cons_unemp_ny.stdev), fill = "darkgray", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_unemp_ny.mean), color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  scale_y_continuous(limits = c(0, 6), breaks = c(0, 1, 2, 3, 4, 5, 6)) +
  theme_bw() +
  labs(
    title = "KOF consensus real unemployment rate forecast - next year",
    subtitle = "",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/Unemp/KOF_ConsUnemp_ny.png", width = 8, height = 4)
graphics.off()

### Five years ----
kof_cons_unemp_5y.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_5y.mean")
kof_cons_unemp_5y.mean <- ts(
  data = kof_cons_unemp_5y.mean$ch.kof.consensus.q_qn_unemp_5y.mean,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_unemp_5y.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_5y.min")
kof_cons_unemp_5y.min <- ts(
  data = kof_cons_unemp_5y.min$ch.kof.consensus.q_qn_unemp_5y.min,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_unemp_5y.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_5y.max")
kof_cons_unemp_5y.max <- ts(
  data = kof_cons_unemp_5y.max$ch.kof.consensus.q_qn_unemp_5y.max,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_unemp_5y.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_unemp_5y.stdev")
kof_cons_unemp_5y.stdev <- ts(
  data = kof_cons_unemp_5y.stdev$ch.kof.consensus.q_qn_unemp_5y.stdev,
  start = c(2001, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_unemp_5y.max, kof_cons_unemp_5y.mean, kof_cons_unemp_5y.min, kof_cons_unemp_5y.stdev)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_unemp_5y.min, ymax = kof_cons_unemp_5y.max), fill = "lightgray", alpha = 0.5) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_unemp_5y.mean - kof_cons_unemp_5y.stdev, ymax = kof_cons_unemp_5y.mean + kof_cons_unemp_5y.stdev), fill = "darkgray", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_unemp_5y.mean), color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  scale_y_continuous(limits = c(0, 6), breaks = c(0, 1, 2, 3, 4, 5, 6)) +
  theme_bw() +
  labs(
    title = "KOF consensus real unemployment rate forecast - 5 yeary",
    subtitle = "",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/Unemp/KOF_ConsUnemp_5y.png", width = 8, height = 4)
graphics.off()

# END
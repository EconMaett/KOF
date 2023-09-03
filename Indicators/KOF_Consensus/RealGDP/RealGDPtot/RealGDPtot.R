# ************************************************************************
# Real GDP ----
# ************************************************************************
# Feel free to copy, adapt, and use this code for your own purposes.
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(ggtext)

start_date <- "2017-01-01"

## Access the data ----
# ch.kof.consensus.q_qn_realgdp_[cy,ny,5y].[count,max,mean,median,mean,stdev]

# ch.kof.consensus.q_qn_realgdp_prob_[cy,ny,5y]_[1-17].[count,max,mean,median,mean,stdev]

### Current year ----
kof_cons_gdp_cy.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_realgdp_cy.mean")
kof_cons_gdp_cy.mean <- ts(
  data = kof_cons_gdp_cy.mean$ch.kof.consensus.q_qn_realgdp_cy.mean,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_gdp_cy.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_realgdp_cy.min")
kof_cons_gdp_cy.min <- ts(
  data = kof_cons_gdp_cy.min$ch.kof.consensus.q_qn_realgdp_cy.min,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_gdp_cy.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_realgdp_cy.max")
kof_cons_gdp_cy.max <- ts(
  data = kof_cons_gdp_cy.max$ch.kof.consensus.q_qn_realgdp_cy.max,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_gdp_cy.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_realgdp_cy.stdev")
kof_cons_gdp_cy.stdev <- ts(
  data = kof_cons_gdp_cy.stdev$ch.kof.consensus.q_qn_realgdp_cy.stdev,
  start = c(2001, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_gdp_cy.max, kof_cons_gdp_cy.mean, kof_cons_gdp_cy.min, kof_cons_gdp_cy.stdev)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_gdp_cy.min, ymax = kof_cons_gdp_cy.max), fill = "lightgray", alpha = 0.5) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_gdp_cy.mean - kof_cons_gdp_cy.stdev, ymax = kof_cons_gdp_cy.mean + kof_cons_gdp_cy.stdev), fill = "darkgray", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_gdp_cy.mean), color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-6, 6)) +
  theme_bw() +
  labs(
    title = "KOF consensus real GDP growth forecast - current year",
    subtitle = "",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/RealGDP/RealGDPtot/KOF_ConsRealGDPtot_cy.png", width = 8, height = 4)
graphics.off()

### Next year ----
kof_cons_gdp_ny.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_realgdp_ny.mean")
kof_cons_gdp_ny.mean <- ts(
  data = kof_cons_gdp_ny.mean$ch.kof.consensus.q_qn_realgdp_ny.mean,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_gdp_ny.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_realgdp_ny.min")
kof_cons_gdp_ny.min <- ts(
  data = kof_cons_gdp_ny.min$ch.kof.consensus.q_qn_realgdp_ny.min,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_gdp_ny.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_realgdp_ny.max")
kof_cons_gdp_ny.max <- ts(
  data = kof_cons_gdp_ny.max$ch.kof.consensus.q_qn_realgdp_ny.max,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_gdp_ny.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_realgdp_ny.stdev")
kof_cons_gdp_ny.stdev <- ts(
  data = kof_cons_gdp_ny.stdev$ch.kof.consensus.q_qn_realgdp_ny.stdev,
  start = c(2001, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_gdp_ny.max, kof_cons_gdp_ny.mean, kof_cons_gdp_ny.min, kof_cons_gdp_ny.stdev)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_gdp_ny.min, ymax = kof_cons_gdp_ny.max), fill = "lightgray", alpha = 0.5) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_gdp_ny.mean - kof_cons_gdp_ny.stdev, ymax = kof_cons_gdp_ny.mean + kof_cons_gdp_ny.stdev), fill = "darkgray", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_gdp_ny.mean), color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-6, 6)) +
  theme_bw() +
  labs(
    title = "KOF consensus real GDP growth forecast - next year",
    subtitle = "",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/RealGDP/RealGDPtot/KOF_ConsRealGDPtot_ny.png", width = 8, height = 4)
graphics.off()

### Five years ----
kof_cons_gdp_5y.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_realgdp_5y.mean")
kof_cons_gdp_5y.mean <- ts(
  data = kof_cons_gdp_5y.mean$ch.kof.consensus.q_qn_realgdp_5y.mean,
  start = c(2015, 2),
  frequency = 4
)

kof_cons_gdp_5y.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_realgdp_5y.min")
kof_cons_gdp_5y.min <- ts(
  data = kof_cons_gdp_5y.min$ch.kof.consensus.q_qn_realgdp_5y.min,
  start = c(2015, 2),
  frequency = 4
)

kof_cons_gdp_5y.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_realgdp_5y.max")
kof_cons_gdp_5y.max <- ts(
  data = kof_cons_gdp_5y.max$ch.kof.consensus.q_qn_realgdp_5y.max,
  start = c(2015, 2),
  frequency = 4
)

kof_cons_gdp_5y.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_realgdp_5y.stdev")
kof_cons_gdp_5y.stdev <- ts(
  data = kof_cons_gdp_5y.stdev$ch.kof.consensus.q_qn_realgdp_5y.stdev,
  start = c(2015, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_gdp_5y.max, kof_cons_gdp_5y.mean, kof_cons_gdp_5y.min, kof_cons_gdp_5y.stdev)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_gdp_5y.min, ymax = kof_cons_gdp_5y.max), fill = "lightgray", alpha = 0.5) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_gdp_5y.mean - kof_cons_gdp_5y.stdev, ymax = kof_cons_gdp_5y.mean + kof_cons_gdp_5y.stdev), fill = "darkgray", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_gdp_5y.mean), color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-6, 6)) +
  theme_bw() +
  labs(
    title = "KOF consensus real GDP growth forecast - 5 years",
    subtitle = "",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/RealGDP/RealGDPtot/KOF_ConsRealGDPtot_5y.png", width = 8, height = 4)
graphics.off()

# END
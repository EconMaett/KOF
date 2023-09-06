# ************************************************************************
# Investment ----
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
# ch.kof.consensus.q_qn_invest_cy.[count,max,mean,median,mean,stdev]

### Current year ----
kof_cons_invest_cy.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_invest_cy.mean")
kof_cons_invest_cy.mean <- ts(
  data = kof_cons_invest_cy.mean$ch.kof.consensus.q_qn_invest_cy.mean,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_invest_cy.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_invest_cy.min")
kof_cons_invest_cy.min <- ts(
  data = kof_cons_invest_cy.min$ch.kof.consensus.q_qn_invest_cy.min,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_invest_cy.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_invest_cy.max")
kof_cons_invest_cy.max <- ts(
  data = kof_cons_invest_cy.max$ch.kof.consensus.q_qn_invest_cy.max,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_invest_cy.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_invest_cy.stdev")
kof_cons_invest_cy.stdev <- ts(
  data = kof_cons_invest_cy.stdev$ch.kof.consensus.q_qn_invest_cy.stdev,
  start = c(2001, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_invest_cy.max, kof_cons_invest_cy.mean, kof_cons_invest_cy.min, kof_cons_invest_cy.stdev)) |> 
  mutate(time = date(time)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_invest_cy.min, ymax = kof_cons_invest_cy.max), fill = "#ae49a2", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_invest_cy.mean - kof_cons_invest_cy.stdev, ymax = kof_cons_invest_cy.mean + kof_cons_invest_cy.stdev), fill = "#ae49a2", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_invest_cy.mean), color = "#ae49a2", linewidth = 1) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-15, 15), breaks = seq(-15, 15, 5)) +
  theme_bw() +
  labs(
    title = "KOF consensus investment forecast",
    subtitle = "Current year",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/Real-GDP/Investment/Fig_KOF_Consensus-Investment_cy.png", width = 8, height = 4)
graphics.off()


### Next year ----
kof_cons_invest_ny.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_invest_ny.mean")
kof_cons_invest_ny.mean <- ts(
  data = kof_cons_invest_ny.mean$ch.kof.consensus.q_qn_invest_ny.mean,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_invest_ny.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_invest_ny.min")
kof_cons_invest_ny.min <- ts(
  data = kof_cons_invest_ny.min$ch.kof.consensus.q_qn_invest_ny.min,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_invest_ny.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_invest_ny.max")
kof_cons_invest_ny.max <- ts(
  data = kof_cons_invest_ny.max$ch.kof.consensus.q_qn_invest_ny.max,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_invest_ny.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_invest_ny.stdev")
kof_cons_invest_ny.stdev <- ts(
  data = kof_cons_invest_ny.stdev$ch.kof.consensus.q_qn_invest_ny.stdev,
  start = c(2001, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_invest_ny.max, kof_cons_invest_ny.mean, kof_cons_invest_ny.min, kof_cons_invest_ny.stdev)) |> 
  mutate(time = date(time)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_invest_ny.min, ymax = kof_cons_invest_ny.max), fill = "#ae49a2", alpha = 0.5) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_invest_ny.mean - kof_cons_invest_ny.stdev, ymax = kof_cons_invest_ny.mean + kof_cons_invest_ny.stdev), fill = "#ae49a2", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_invest_ny.mean), color = "#ae49a2", linewidth = 1) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-15, 15), breaks = seq(-15, 15, 5)) +
  theme_bw() +
  labs(
    title = "KOF consensus investment forecast",
    subtitle = "Next year",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/Real-GDP/Investment/Fig_KOF_Consensus-Investment_ny.png", width = 8, height = 4)
graphics.off()

# END
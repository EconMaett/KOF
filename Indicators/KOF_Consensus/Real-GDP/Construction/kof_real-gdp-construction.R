# ************************************************************************
# Construction investment ----
# Feel free to copy, adapt, and use this code for your own purposes.
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(ggtext)

start_date <- "2017-01-01"
chrecdp <- read_csv(file = "Recession-Dates/Recession-Dates_CEPR_EA_Monthly_Midpoint.csv")

## Access the data ----

# ch.kof.consensus.q_qn_investbau_[cy,ny].[count,max,mean,median,mean,stdev]

### Current year ----
kof_cons_investbau_cy.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investbau_cy.mean")
kof_cons_investbau_cy.mean <- ts(
  data = kof_cons_investbau_cy.mean$ch.kof.consensus.q_qn_investbau_cy.mean,
  start = c(2016, 5),
  frequency = 4
)

kof_cons_investbau_cy.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investbau_cy.min")
kof_cons_investbau_cy.min <- ts(
  data = kof_cons_investbau_cy.min$ch.kof.consensus.q_qn_investbau_cy.min,
  start = c(2016, 5),
  frequency = 4
)

kof_cons_investbau_cy.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investbau_cy.max")
kof_cons_investbau_cy.max <- ts(
  data = kof_cons_investbau_cy.max$ch.kof.consensus.q_qn_investbau_cy.max,
  start = c(2016, 5),
  frequency = 4
)

kof_cons_investbau_cy.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investbau_cy.stdev")
kof_cons_investbau_cy.stdev <- ts(
  data = kof_cons_investbau_cy.stdev$ch.kof.consensus.q_qn_investbau_cy.stdev,
  start = c(2016, 5),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_investbau_cy.max, kof_cons_investbau_cy.mean, kof_cons_investbau_cy.min, kof_cons_investbau_cy.stdev)) |> 
  mutate(time = date(time)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_investbau_cy.min, ymax = kof_cons_investbau_cy.max), fill = "#374e8e", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_investbau_cy.mean - kof_cons_investbau_cy.stdev, ymax = kof_cons_investbau_cy.mean + kof_cons_investbau_cy.stdev), fill = "#374e8e", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_investbau_cy.mean), color = "#374e8e", linewidth = 1) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-6, 6)) +
  theme_bw() +
  labs(
    title = "KOF consensus real construction investment forecast",
    subtitle = "Current year",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/Real-GDP/Construction/Fig_KOF_Construction_cy.png", width = 8, height = 4)
graphics.off()


### Next year ----
kof_cons_investbau_ny.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investbau_ny.mean")
kof_cons_investbau_ny.mean <- ts(
  data = kof_cons_investbau_ny.mean$ch.kof.consensus.q_qn_investbau_ny.mean,
  start = c(2016, 5),
  frequency = 4
)

kof_cons_investbau_ny.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investbau_ny.min")
kof_cons_investbau_ny.min <- ts(
  data = kof_cons_investbau_ny.min$ch.kof.consensus.q_qn_investbau_ny.min,
  start = c(2016, 5),
  frequency = 4
)

kof_cons_investbau_ny.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investbau_ny.max")
kof_cons_investbau_ny.max <- ts(
  data = kof_cons_investbau_ny.max$ch.kof.consensus.q_qn_investbau_ny.max,
  start = c(2016, 5),
  frequency = 4
)

kof_cons_investbau_ny.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investbau_ny.stdev")
kof_cons_investbau_ny.stdev <- ts(
  data = kof_cons_investbau_ny.stdev$ch.kof.consensus.q_qn_investbau_ny.stdev,
  start = c(2016, 5),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_investbau_ny.max, kof_cons_investbau_ny.mean, kof_cons_investbau_ny.min, kof_cons_investbau_ny.stdev)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  mutate(time = date(time)) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_investbau_ny.min, ymax = kof_cons_investbau_ny.max), fill = "#374e8e", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_investbau_ny.mean - kof_cons_investbau_ny.stdev, ymax = kof_cons_investbau_ny.mean + kof_cons_investbau_ny.stdev), fill = "#374e8e", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_investbau_ny.mean), color = "#374e8e", linewidth = 1) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-6, 6)) +
  theme_bw() +
  labs(
    title = "KOF consensus real construction investment forecast",
    subtitle = "Next year",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/Real-GDP/Construction/Fig_KOF_Construction_ny.png", width = 8, height = 4)
graphics.off()

# END
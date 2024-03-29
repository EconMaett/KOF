# ************************************************************************
# Exports ----
# ************************************************************************
# Feel free to copy, adapt, and use this code for your own purposes.
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(ggtext)

start_date <- "2005-01-01"
chrecdp <- read_csv(file = "Recession-Dates/Recession-Dates_CEPR_EA_Monthly_Midpoint.csv")

## Access the data ----
# ch.kof.consensus.q_qn_exports_[cy,ny].[count,max,mean,median,mean,stdev]

### Current year ----
kof_cons_exports_cy.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_exports_cy.mean")
kof_cons_exports_cy.mean <- ts(
  data = kof_cons_exports_cy.mean$ch.kof.consensus.q_qn_exports_cy.mean,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_exports_cy.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_exports_cy.min")
kof_cons_exports_cy.min <- ts(
  data = kof_cons_exports_cy.min$ch.kof.consensus.q_qn_exports_cy.min,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_exports_cy.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_exports_cy.max")
kof_cons_exports_cy.max <- ts(
  data = kof_cons_exports_cy.max$ch.kof.consensus.q_qn_exports_cy.max,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_exports_cy.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_exports_cy.stdev")
kof_cons_exports_cy.stdev <- ts(
  data = kof_cons_exports_cy.stdev$ch.kof.consensus.q_qn_exports_cy.stdev,
  start = c(2001, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_exports_cy.max, kof_cons_exports_cy.mean, kof_cons_exports_cy.min, kof_cons_exports_cy.stdev)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  mutate(time = date(time)) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_exports_cy.min, ymax = kof_cons_exports_cy.max), fill = "#ac004f", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_exports_cy.mean - kof_cons_exports_cy.stdev, ymax = kof_cons_exports_cy.mean + kof_cons_exports_cy.stdev), fill = "#ac004f", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_exports_cy.mean), color = "#ac004f", linewidth = 1) +
  scale_x_date(expand = c(0, 0), limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(-15, 15), breaks = seq(-15, 15, 5)) +
  theme_bw() +
  labs(
    title = "KOF consensus exports forecast",
    subtitle = "Current year",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/Real-GDP/Exports/Fig_KOF_Consensus-Exports_cy.png", width = 8, height = 4)
graphics.off()


### Next year ----
kof_cons_exports_ny.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_exports_ny.mean")
kof_cons_exports_ny.mean <- ts(
  data = kof_cons_exports_ny.mean$ch.kof.consensus.q_qn_exports_ny.mean,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_exports_ny.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_exports_ny.min")
kof_cons_exports_ny.min <- ts(
  data = kof_cons_exports_ny.min$ch.kof.consensus.q_qn_exports_ny.min,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_exports_ny.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_exports_ny.max")
kof_cons_exports_ny.max <- ts(
  data = kof_cons_exports_ny.max$ch.kof.consensus.q_qn_exports_ny.max,
  start = c(2001, 2),
  frequency = 4
)

kof_cons_exports_ny.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_exports_ny.stdev")
kof_cons_exports_ny.stdev <- ts(
  data = kof_cons_exports_ny.stdev$ch.kof.consensus.q_qn_exports_ny.stdev,
  start = c(2001, 2),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_exports_ny.max, kof_cons_exports_ny.mean, kof_cons_exports_ny.min, kof_cons_exports_ny.stdev)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  mutate(time = date(time)) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_exports_ny.min, ymax = kof_cons_exports_ny.max), fill = "#ac004f", alpha = 0.3) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_exports_ny.mean - kof_cons_exports_ny.stdev, ymax = kof_cons_exports_ny.mean + kof_cons_exports_ny.stdev), fill = "#ac004f", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_exports_ny.mean), color = "#ac004f", linewidth = 1) +
  scale_x_date(expand = c(0, 0), limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(-15, 15), breaks = seq(-15, 15, 5)) +
  theme_bw() +
  labs(
    title = "KOF consensus exports forecast",
    subtitle = "Next year",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/Real-GDP/Exports/Fig_KOF_Consensus-Exports_ny.png", width = 8, height = 4)
graphics.off()

# END
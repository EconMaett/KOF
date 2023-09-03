# ************************************************************************
# Equipment ----
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
# ch.kof.consensus.q_qn_investequ_[cy,ny].[count,max,mean,median,mean,stdev]

### Current year ----
kof_cons_equip_cy.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investequ_cy.mean")
kof_cons_equip_cy.mean <- ts(
  data = kof_cons_equip_cy.mean$ch.kof.consensus.q_qn_investequ_cy.mean,
  start = c(2016, 5),
  frequency = 4
)

kof_cons_equip_cy.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investequ_cy.min")
kof_cons_equip_cy.min <- ts(
  data = kof_cons_equip_cy.min$ch.kof.consensus.q_qn_investequ_cy.min,
  start = c(2016, 5),
  frequency = 4
)

kof_cons_equip_cy.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investequ_cy.max")
kof_cons_equip_cy.max <- ts(
  data = kof_cons_equip_cy.max$ch.kof.consensus.q_qn_investequ_cy.max,
  start = c(2016, 5),
  frequency = 4
)

kof_cons_equip_cy.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investequ_cy.stdev")
kof_cons_equip_cy.stdev <- ts(
  data = kof_cons_equip_cy.stdev$ch.kof.consensus.q_qn_investequ_cy.stdev,
  start = c(2016, 5),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_equip_cy.max, kof_cons_equip_cy.mean, kof_cons_equip_cy.min, kof_cons_equip_cy.stdev)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_equip_cy.min, ymax = kof_cons_equip_cy.max), fill = "lightgray", alpha = 0.5) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_equip_cy.mean - kof_cons_equip_cy.stdev, ymax = kof_cons_equip_cy.mean + kof_cons_equip_cy.stdev), fill = "darkgray", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_equip_cy.mean), color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-15, 15)) +
  theme_bw() +
  labs(
    title = "KOF consensus equipment investment forecast - current year",
    subtitle = "",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/RealGDP/Equipment/KOF_ConsEquip_cy.png", width = 8, height = 4)
graphics.off()


### Next year ----
kof_cons_equip_ny.mean <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investequ_ny.mean")
kof_cons_equip_ny.mean <- ts(
  data = kof_cons_equip_ny.mean$ch.kof.consensus.q_qn_investequ_ny.mean,
  start = c(2016, 5),
  frequency = 4
)

kof_cons_equip_ny.min <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investequ_ny.min")
kof_cons_equip_ny.min <- ts(
  data = kof_cons_equip_ny.min$ch.kof.consensus.q_qn_investequ_ny.min,
  start = c(2016, 5),
  frequency = 4
)

kof_cons_equip_ny.max <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investequ_ny.max")
kof_cons_equip_ny.max <- ts(
  data = kof_cons_equip_ny.max$ch.kof.consensus.q_qn_investequ_ny.max,
  start = c(2016, 5),
  frequency = 4
)

kof_cons_equip_ny.stdev <- get_time_series(ts_keys = "ch.kof.consensus.q_qn_investequ_ny.stdev")
kof_cons_equip_ny.stdev <- ts(
  data = kof_cons_equip_ny.stdev$ch.kof.consensus.q_qn_investequ_ny.stdev,
  start = c(2016, 5),
  frequency = 4
)

#### Plot the data ----
ts_df(ts_c(kof_cons_equip_ny.max, kof_cons_equip_ny.mean, kof_cons_equip_ny.min, kof_cons_equip_ny.stdev)) |> 
  pivot_wider(names_from = id, values_from = value) |> 
  ggplot() +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_equip_ny.min, ymax = kof_cons_equip_ny.max), fill = "lightgray", alpha = 0.5) +
  geom_ribbon(mapping = aes(x = time, ymin = kof_cons_equip_ny.mean - kof_cons_equip_ny.stdev, ymax = kof_cons_equip_ny.mean + kof_cons_equip_ny.stdev), fill = "darkgray", alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = kof_cons_equip_ny.mean), color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-15, 15)) +
  theme_bw() +
  labs(
    title = "KOF consensus equipment investment forecast - next year",
    subtitle = "",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Consensus/RealGDP/Equipment/KOF_ConsEquip_ny.png", width = 8, height = 4)
graphics.off()

# END
# ************************************************************************
# KOF Employment Indicator ----
# URL: https://kof.ethz.ch/en/forecasts-and-indicators/indicators/kof-employment-indicator.html
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

list_keys_in_collection(collectionname = "ogd_ch.kof.ie")
# Assessment (Urteil): ch.kof.ie.retro.ch_total.ass.d11
# Expectation (Erwartung): ch.kof.ie.retro.ch_total.exp.d11
# Overall (Insgesamt): ch.kof.ie.retro.ch_total.ind.d11

## Access the data ----

### Assessment ----
kof_empl_ass <- get_time_series(ts_keys = "ch.kof.ie.retro.ch_total.ass.d11")
kof_empl_ass <- ts(
  data = kof_empl_ass$ch.kof.ie.retro.ch_total.ass.d11,
  start = c(1992, 3),
  frequency = 4
)

### Expectation ----
kof_empl_exp <- get_time_series(ts_keys = "ch.kof.ie.retro.ch_total.exp.d11")
kof_empl_exp <- ts(
  data = kof_empl_exp$ch.kof.ie.retro.ch_total.exp.d11,
  start = c(1992, 3),
  frequency = 4
)

### Overall ----
kof_empl_ind <- get_time_series(ts_keys = "ch.kof.ie.retro.ch_total.ind.d11")
kof_empl_ind <- ts(
  data = kof_empl_ind$ch.kof.ie.retro.ch_total.ind.d11,
  start = c(1992, 3),
  frequency = 4
)

## Plot the data ----

ts_df(ts_c(kof_empl_ass, kof_empl_exp, kof_empl_ind)) |> 
  ggplot(mapping = aes(x = time, y = value, color = id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  scale_color_manual(values = c("#ae49a2", "#006d64", "#478c5b")) +
  labs(
    title = "KOF Employment Indicators",
    subtitle = "<span style = 'color: #ae49a2;'>Assessment</span>, <span style = 'color: #006d64;'>expectations</span>, <span style = 'color: #478c5b;'>overall</span>.",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

ggsave(filename = "Indicators/KOF_Employment/KOF_Employment.png", width = 8, height = 4)
graphics.off()

# END
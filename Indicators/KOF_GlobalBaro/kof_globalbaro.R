# ************************************************************************
# KOF Global Barometers ----
# URL: https://kof.ethz.ch/en/forecasts-and-indicators/indicators/kof-globalbaro.html
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

start_date <- "2018-01-01"

## Access the data ----

list_keys_in_collection(collectionname = "ogd_ch.kof.globalbaro")
# "ch.kof.globalbaro.coincident" "ch.kof.globalbaro.leading"

### Leading barometer ----
kof_globalbaro_lead <- get_time_series(ts_keys = "ch.kof.globalbaro.leading")
kof_globalbaro_lead <- ts(
  data = kof_globalbaro_lead$ch.kof.globalbaro.leading, 
  start = c(1991, 7), 
  frequency = 12
  )

### Coincident barometer ----
kof_globalbaro_coi <- get_time_series(ts_keys = "ch.kof.globalbaro.coincident")
kof_globalbaro_coi <- ts(
  data = kof_globalbaro_coi$ch.kof.globalbaro.coincident,
  start = c(1991, 7),
  frequency = 12
)

## Plot the data ----
ts_df(ts_c(kof_globalbaro_lead, kof_globalbaro_coi)) |> 
  ggplot(mapping = aes(x = time, y = value, color = id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 100, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(50, 150)) +
  scale_color_manual(values = c("#1B9E77", "#374e8e")) +
  labs(
    title = "Global economic barometers",
    subtitle = "<span style = 'color: #1B9E77;'>Coincident</span> and <span style = 'color: #374e8e;'>leading indicator</span>.",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

ggsave(filename = "Indicators/KOF_GlobalBaro/KOF_Globalbaro.png", width = 8, height = 4)
graphics.off()

# END
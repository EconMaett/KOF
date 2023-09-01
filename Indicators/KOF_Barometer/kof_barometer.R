# ************************************************************************
# KOF Economic Barometer ----
# ************************************************************************
# URL: https://kof.ethz.ch/prognosen-indikatoren/indikatoren/kof-konjunkturbarometer.html
# Feel free to copy, adapt, and use this code for your own purposes at
# your own risk.
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)

start_date <- "2018-01-01"

## Access the data ----
list_keys_in_collection(collectionname = "ogd_ch.kof.barometer")
# "ch.kof.barometer"

kof_baro <- get_time_series(ts_keys = "ch.kof.barometer")
kof_baro <- ts(
  data = kof_baro$ch.kof.barometer,
  start = c(1991, 1),
  frequency = 12
)

## Plot the data ----
ts_df(kof_baro) |> 
  ggplot(mapping = aes(x = time, y = value)) +
  geom_line(linewidth = 1, color = "#1B9E77") +
  geom_hline(yintercept = 100, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(50, 150), breaks = seq(50, 150, 25)) +
  labs(
    title = "KOF Economic Barometer",
    subtitle = "Automatic combination of 200 out of a total of 500 economic time series",
    caption = "Graph created by @econmaett with data from KOF",
    x = "", y = ""
  ) +
  theme_bw()

ggsave(filename = "Indicators/KOF_Barometer/KOFBarometer.png", width = 8, height = 4)
graphics.off()

# END
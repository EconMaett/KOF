# ************************************************************************
# KOF Global Barometers ----
# URL: https://kof.ethz.ch/en/forecasts-and-indicators/indicators/kof-globalbaro.html
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
  ggplot() +
  geom_hline(yintercept = 100, color = "darkgrey", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(expand = c(0, 0), limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(50, 150)) +
  scale_color_manual(
    values = c("#374e8e", "#1B9E77"),
    breaks = c("kof_globalbaro_coi", "kof_globalbaro_lead")) +
  labs(
    title = "Global economic barometers",
    subtitle = "<span style = 'color: #374e8e;'>Coincident indicator</span> and <span style = 'color: #1B9E77;'>leading indicator</span>.",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

ggsave(filename = "Indicators/KOF_Global-Barometer/Fig_KOF_Global-Barometer.png", width = 8, height = 4)
graphics.off()

# END
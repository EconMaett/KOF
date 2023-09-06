# ************************************************************************
# KOF Economic Sentiment Indicator (ESI) ----
# ************************************************************************
# URL: https://kof.ethz.ch/prognosen-indikatoren/indikatoren/kof-economic-sentiment-indicator.html
# Feel free to copy, adapt, and use this code for your own purposes.
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(eurostat) # To access the ESI for the EU
library(ggtext)

start_date <- "2007-01-01"
chrecdp <- read_csv(file = "Recession-Dates/Recession-Dates_NBER_US_Daily_Midpoint.csv")


## Access the data ----

### Access the EU ESI ----
eu_esi <- get_eurostat(id = "teibs010", time_format = "date", select_time = "M", 
                       sinceTimePeriod = "1990-01-01")

eu_esi <- eu_esi |> 
  filter(geo == "EU27_2020") |> 
  select(time, values) |> 
  rename(eu_esi = values)

eu_esi <- ts_ts(eu_esi)

### Access the KOF ESI ----
list_keys_in_collection(collectionname = "ogd_ch.kof.esi")
# ch.kof.esi.index, ch.kof.esi.index.v2018

kof_esi <- get_time_series(ts_keys = "ch.kof.esi.index")

kof_esi <- ts(
  data = kof_esi$ch.kof.esi.index,
  start = c(2007, 4),
  frequency = 12
)

## Plot the data ----
ts_df(ts_c(eu_esi, kof_esi)) |> 
  ggplot() +
  geom_hline(yintercept = 100, color = "darkgrey", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(50, 125), breaks = seq(50, 125, 25)) +
  scale_color_manual(
    values = c("#1B9E77", "#374e8e"),
    breaks = c("eu_esi", "kof_esi")
    ) +
  labs(
    title = "Economic Sentiment Indicators (ESI)",
    subtitle = "<span style = 'color: #374e8e;'>KOF ESI</span> and <span style = 'color: #1B9E77;'>Eurostat ESI</span>",
    caption = "Graph created by @econmaett with data from KOF",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

ggsave(filename = "Indicators/KOF_Economic-Sentiment-Indicator/Fig_KOF_Economic-Sentiment-Indicator.png", width = 8, height = 4)
graphics.off()

# END
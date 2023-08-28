# ************************************************************************
# KOF Economic Sentiment Indicator (ESI) ----
# ************************************************************************
# URL: https://kof.ethz.ch/prognosen-indikatoren/indikatoren/kof-economic-sentiment-indicator.html
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
library(eurostat) # To access the ESI for the EU
library(ggtext)

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
  ggplot(mapping = aes(x = time, y = value, color = id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 100, color = "black", linetype = "dashed", show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  scale_color_manual(values = c("#1B9E77", "#374e8e")) +
  labs(
    title = "Economic Sentiment Indicators (ESI)",
    subtitle = "<span style = 'color: #1B9E77;'>Eurostat</span>, <span style = 'color: #374e8e;'>KOF</span>",
    caption = "Graph created by @econmaett with data from KOF",
    x = "", y = ""
  ) +
  ylim(0, 150) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

ggsave(filename = "Indicators/KOF_ESI/KOF_ESI.png", width = 8, height = 4)
graphics.off()

# END
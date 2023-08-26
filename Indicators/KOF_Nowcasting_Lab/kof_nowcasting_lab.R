# ************************************************************************
# KOF Nowcasting Lab ----
# URL: https://kof.ethz.ch/en/forecasts-and-indicators/forecasts/nowcastinglab.html
# Project website: https://nowcastinglab.org/map
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(ggtext)

## Access the data ----
kof_ncst_url <- "https://nowcastinglab.org/api/output/download?indicator=en&model=all"

kof_ncst_data <- readr::read_csv(file = kof_ncst_url)

## Plot the data ----

kof_ncst_data |> 
  mutate(
    kofcast_datestamp = dmy(kofcast_datestamp)
  ) |> 
  rename(time = kofcast_datestamp, period = target_period, id = country) |> 
  filter(period == "Q3 2023") |> 
  select(time, id, value) |> 
  filter(id %in% c("ch", "de", "ea", "es", "fr", "gb", "it", "us")) |> 
  ggplot(mapping = aes(x = time, y = value, color = id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_y_continuous(limits = c(-0.5, 1)) +
  scale_color_manual(values = c("#ac004f", "#e3b13e", "#7e7e8f", "#df7c18", "#8aabfd", "#ce4631", "#374e8e", "#383751")) +
  labs(
    title = "Current GDP Growth Forecast Q3 2023",
    subtitle = "<span style = 'color: #ac004f;'>CH</span>, <span style = 'color: #e3b13e;'>DE</span>, <span style = 'color: #7e7e8f;'>EA</span>,<span style = 'color: #df7c18;'>ES</span>, <span style = 'color: #8aabfd;'>FR</span>, <span style = 'color: #ce4631;'>GB</span>, <span style = 'color: #374e8e;'>IT</span>, <span style = 'color: #383751;'>US</span>.",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

ggsave(filename = "Indicators/KOF_Nowcasting_Lab/kof_nowcasting_lab.png", width = 8, height = 4)
graphics.off()


# END
# ************************************************************************
# KOF Employment Indicator ----
# URL: https://kof.ethz.ch/en/forecasts-and-indicators/indicators/kof-employment-indicator.html
# Feel free to copy, adapt, and use this code for your own purposes.
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(ggtext)

start_date <- "2005-01-01"
chrecdp <- read_csv(file = "Recession-Dates/Recession-Dates_CEPR_EA_Monthly_Midpoint.csv")


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
  ggplot() +  
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(expand = c(0, 0), limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(-25, 20), breaks = seq(-25, 20, 5)) +
  scale_color_manual(
    values = c("#006d64", "#478c5b", "#ae49a2"),
    breaks = c("kof_empl_exp", "kof_empl_ass", "kof_empl_ind")
    ) +
  labs(
    title = "KOF Employment Indicators",
    subtitle = "<span style = 'color: #006d64;'>Expectations</span>, <span style = 'color: #478c5b;'>Assessment</span>, <span style = 'color: #ae49a2;'>Overall</span>.",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

ggsave(filename = "Indicators/KOF_Employment/Fig_KOF_Employment.png", width = 8, height = 4)
graphics.off()

# END
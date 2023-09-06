# ************************************************************************
# KOF Swiss job tracker ----
# URL: https://kof.ethz.ch/en/forecasts-and-indicators/indicators/swiss-job-tracker.html
# Project website: https://swissjobtracker.ch/#/
# Feel free to copy, adapt, and use this code for your own purposes.
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(ggtext)

start_date <- "2018-01-01"
chrecdp <- read_csv(file = "Recession-Dates/Recession-Dates_NBER_US_Daily_Midpoint.csv")

# ch.kof.jobtracker  
list_keys_in_collection(collectionname = "ch.kof.jobtracker")

# ch.kof.jobtracker.canton.[canton].clean.idx
# ch.kof.jobtracker.isco.[0-9].clean.idx
# ch.kof.jobtracker.noga.[ab,c-r,st,u].clean.idx
# ch.kof.jobtracker.total.total.clean.idx

# ch.kof.jobtracker.canton.[canton].sum_clean.idx
# ch.kof.jobtracker.isco.[0-9].sum_clean.idx
# ch.kof.jobtracker.noga.[ab,c-r,st,u].sum_clean.idx
# ch.kof.jobtracker.total.total.sum_clean.idx

## Total ----
kof_jobtracker_total <- get_time_series(ts_keys = "ch.kof.jobtracker.total.total.clean.idx")
kof_jobtracker_total <- ts(
  data = kof_jobtracker_total$ch.kof.jobtracker.total.total.clean.idx,
  start = c(2018, 01, 05),
  frequency = 52.1775
)

### Plot the data ----
ts_df(ts_c(kof_jobtracker_total)) |> 
  mutate(time = date(time)) |> 
  ggplot() +
  geom_hline(yintercept = 100, color = "darkgrey", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value), color = "#374e8e", linewidth = 1) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 30)) +
  theme_bw() +
  labs(
    title = "Weekly index of online job postings",
    subtitle = "Jan 2020 = 100",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  )

ggsave(filename = "Indicators/KOF_Jobtracker/Fig_KOF_Jobtracker-Total.png", width = 8, height = 4)
graphics.off()


## By canton ----
kof_jobtracker.zh <- get_time_series(ts_keys = "ch.kof.jobtracker.canton.zh.clean.idx")
kof_jobtracker.zh <- ts(
  data = kof_jobtracker.zh$ch.kof.jobtracker.canton.zh.clean.idx,
  start = c(2018, 01, 05),
  frequency = 52.1775
)

kof_jobtracker.ag <- get_time_series(ts_keys = "ch.kof.jobtracker.canton.ag.clean.idx")
kof_jobtracker.ag <- ts(
  data = kof_jobtracker.ag$ch.kof.jobtracker.canton.ag.clean.idx,
  start = c(2018, 01, 05),
  frequency = 52.1775
)

kof_jobtracker.be <- get_time_series(ts_keys = "ch.kof.jobtracker.canton.be.clean.idx")
kof_jobtracker.be <- ts(
  data = kof_jobtracker.be$ch.kof.jobtracker.canton.be.clean.idx,
  start = c(2018, 01, 05),
  frequency = 52.1775
)

kof_jobtracker.vd <- get_time_series(ts_keys = "ch.kof.jobtracker.canton.vd.clean.idx")
kof_jobtracker.vd <- ts(
  data = kof_jobtracker.vd$ch.kof.jobtracker.canton.vd.clean.idx,
  start = c(2018, 01, 05),
  frequency = 52.1775
)

kof_jobtracker.bs <- get_time_series(ts_keys = "ch.kof.jobtracker.canton.bs.clean.idx")
kof_jobtracker.bs <- ts(
  data = kof_jobtracker.bs$ch.kof.jobtracker.canton.bs.clean.idx,
  start = c(2018, 01, 05),
  frequency = 52.1775
)

kof_jobtracker.ge <- get_time_series(ts_keys = "ch.kof.jobtracker.canton.ge.clean.idx")
kof_jobtracker.ge <- ts(
  data = kof_jobtracker.ge$ch.kof.jobtracker.canton.ge.clean.idx,
  start = c(2018, 01, 05),
  frequency = 52.1775
)

kof_jobtracker.zg <- get_time_series(ts_keys = "ch.kof.jobtracker.canton.zg.clean.idx")
kof_jobtracker.zg <- ts(
  data = kof_jobtracker.zg$ch.kof.jobtracker.canton.zg.clean.idx,
  start = c(2018, 01, 05),
  frequency = 52.1775
)

### Plot the data ----
ts_df(ts_c(
  kof_jobtracker.ag, kof_jobtracker.be, kof_jobtracker.bs, kof_jobtracker.ge, 
  kof_jobtracker.vd, kof_jobtracker.zg, kof_jobtracker.zh, kof_jobtracker_total
  )) |> 
  mutate(time = date(time)) |> 
  ggplot() +
  geom_hline(yintercept = 100, color = "black", linetype = "dashed", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_color_manual(values = c("#374e8e", "#ac004f", "#383751", "#df7c18", "#478c5b", "#a07bde", "#8aabfd", "#7e7e8f")) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 30)) +
  theme_bw() +
  labs(
    title = "Weekly index of online job postings by canton",
    subtitle = "Jan 2020 = 100. <span style = 'color: #374e8e;'>AG</span>, <span style = 'color: #ac004f;'>BE</span>, <span style = 'color: #383751;'>BS</span>, <span style = 'color: #df7c18;'>GE</span>, <span style = 'color: #478c5b;'>VD</span>, <span style = 'color: #a07bde;'>ZG</span>, <span style = 'color: #8aabfd;'>ZH</span>, <span style = 'color: #7e7e8f;'>Total</span>.",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  ) +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

ggsave(filename = "Indicators/KOF_Jobtracker/Fig_KOF_Jobtracker-Canton.png", width = 8, height = 4)
graphics.off()

# END
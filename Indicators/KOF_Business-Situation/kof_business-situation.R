# ************************************************************************
# KOF Business Situation Indicator ----
# URL: https://kof.ethz.ch/en/forecasts-and-indicators/indicators/kof-business-situation-indicator.html
# Feel free to copy, adapt, and use this code for your own purposes.
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(ggtext)

start_date <- "2009-06-01"
chrecdp <- read_csv(file = "Recession-Dates/Recession-Dates_CEPR_EA_Monthly_Midpoint.csv")

## Access the data ----

# bs_indicator: KOF Geschäftslage Indikator und Subbranchen
list_keys_in_collection(collectionname = "bs_indicator")

# Wirtschaftsbereiche:
# - Industrie: (INU): ch.kof.inu.ng08.fx.q_ql_ass_bs.balance.d11
# - Detailhandel: (DHU) ch.kof.dhu.ng08.fx.q_ql_ass_bs.balance.d11
# - Baugewerbe: (BAU) ch.kof.bau.ng08.fx.q_ql_ass_bs.balance.d11
# - Projektierung: 
# - Finanz- und Versicherungsdienstleistungen: (FVU) ch.kof.fvu.ng08.fx.q_ql_ass_bs.balance.d11
# - übrige Dienstleistungsbranchen:

# - Dienstleistungsunernehmen: (DLU) ch.kof.dlu.ng08.fx.q_ql_ass_bs.balance.d11
# - Grosshandelsunternehmen: (GHU) ch.kof.ghu.ng08.fx.q_ql_ass_bs.balance.d11

# - AIU ch.kof.aiu.ng08.fx.q_ql_ass_bs.balance.d11
# - GGU ch.kof.ggu.ng08.fx.q_ql_ass_bs.balance.d11

## Access the data ----

# Total ----
kof_bs_total <- get_time_series(ts_keys = "ch.kof.bts_total.ng08.fx.q_ql_ass_bs.balance.d11")
kof_bs_total <- ts(
  data = kof_bs_total$ch.kof.bts_total.ng08.fx.q_ql_ass_bs.balance.d11,
  start = c(2009, 4),
  frequency = 12
)

## Plot the data ----
ts_df(kof_bs_total) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_line(mapping = aes(x = time, y = value), linewidth = 1, color = "#374e8e") +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  scale_x_date(expand = c(0, 0), limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(-30, 70), breaks = seq(-30, 70, 20)) +
  labs(
    title = "KOF business situation",
    subtitle = "Total index",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  ) +
  theme_bw()

ggsave(filename = "Indicators/KOF_Business-Situation/Fig_KOF-Business-Situation-Total.png", width = 8, height = 4)
graphics.off()

# Industrieunternehmen (INU) ----
kof_bs_inu <- get_time_series(ts_keys = "ch.kof.inu.ng08.fx.q_ql_ass_bs.balance.d11")
kof_bs_inu <- ts(
  data = kof_bs_inu$ch.kof.inu.ng08.fx.q_ql_ass_bs.balance.d11,
  start = c(2004, 1),
  frequency = 12
)

# Detailhandelsunternehmen (DHU) ----
kof_bs_dhu <- get_time_series(ts_keys = "ch.kof.dhu.ng08.fx.q_ql_ass_bs.balance.d11")
kof_bs_dhu <- ts(
  data = kof_bs_dhu$ch.kof.dhu.ng08.fx.q_ql_ass_bs.balance.d11,
  start = c(1973, 2),
  frequency = 12
)

# Bauunternehmen (BAU) ----
kof_bs_bau <- get_time_series(ts_keys = "ch.kof.bau.ng08.fx.q_ql_ass_bs.balance.d11")
kof_bs_bau <- ts(
  data = kof_bs_bau$ch.kof.bau.ng08.fx.q_ql_ass_bs.balance.d11,
  start = c(1994, 10),
  frequency = 12
)

# Finanz- und Versicherungsunternehmen (FVU) ----
kof_bs_fvu <- get_time_series(ts_keys = "ch.kof.fvu.ng08.fx.q_ql_ass_bs.balance.d11")
kof_bs_fvu <- ts(
  data = kof_bs_fvu$ch.kof.fvu.ng08.fx.q_ql_ass_bs.balance.d11,
  start = c(2010, 7),
  frequency = 12
)

# Dienstleistungsunternehmen (DLU) ----
kof_bs_dlu <- get_time_series(ts_keys = "ch.kof.dlu.ng08.fx.q_ql_ass_bs.balance.d11")
kof_bs_dlu <- ts(
  data = kof_bs_dlu$ch.kof.dlu.ng08.fx.q_ql_ass_bs.balance.d11,
  start = c(2006, 4),
  frequency = 4
)

# Grosshandelsunternehmen (GHU) ----
kof_bs_ghu <- get_time_series(ts_keys = "ch.kof.ghu.ng08.fx.q_ql_ass_bs.balance.d11")
kof_bs_ghu <- ts(
  data = kof_bs_ghu$ch.kof.ghu.ng08.fx.q_ql_ass_bs.balance.d11,
  start = c(2007, 3),
  frequency = 4
)

## Plot the sub-indeces ----
ts_df(
  ts_span(
    ts_c(
      kof_bs_bau, kof_bs_dhu, kof_bs_dlu, kof_bs_fvu, kof_bs_ghu, kof_bs_inu, kof_bs_total
      ), start = "2010-01-01", end = today()
    )
  ) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(expand = c(0, 0), limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(-30, 70), breaks = seq(-30, 70, 20)) +
  scale_color_manual(values = c("#a07bde", "#478c5b", "#ac004f", "#ae49a2", "#a07bde", "#8aabfd", "#df7c18")) +
  labs(
    title = "KOF business conditions",
    subtitle = "<span style = 'color: #a07bde;'>Construction</span>, <span style = 'color: #478c5b;'>retail sales</span>, <span style = 'color: #ac004f;'>services</span>, <span style = 'color: #ae49a2;'>finance</span>, <span style = 'color: #a07bde;'>wholesale</span>, <span style = 'color: #8aabfd;'>industry</span>, <span style = 'color: #374e8e;'>total</span>.",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

ggsave(filename = "Indicators/KOF_Business-Situation/Fig_KOF_Business-Situation-Sub-Indexes.png", width = 8, height = 4)
graphics.off()

# END
# ************************************************************************
# KOF-Baublatt-Ausblick ----
# ************************************************************************
# URL: https://kof.ethz.ch/prognosen-indikatoren/indikatoren/kof-baublatt-ausblick.html
# Feel free to copy, adapt, and use this code for your own purposes.
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(ggtext)

start_date <- "2005-01-01"
chrecdp <- read_csv(file = "Recession-Dates/Recession-Dates_CEPR_EA_Monthly_Midpoint.csv")


## Access the data ----

list_available_collections()
# ds_kof_baublatt_ausblick_fitted_qtr: KOF-Baublatt-Ausblick fitted values
# ds_kof_baublatt_ausblick_nowcast_qtr: KOF-Baublatt-Ausblick Realtime
# ds_kof_baublatt_ausblick_qtr: KOF-Baublatt-Ausblick



### Nominal construction investment ----
list_keys_in_collection(collectionname = "ds_kof_baublatt_ausblick_qtr")
# ch.kof.baublatt.ausblick.nom.chf_pct, ch.seco.gdp_exp.nom_q_p5111c_pct.kof_baublatt_ausblick

kof_nom_const_inv <- get_time_series(ts_keys = "ch.seco.gdp_exp.nom_q_p5111c_pct.kof_baublatt_ausblick")
kof_nom_const_inv <- ts(
  data = kof_nom_const_inv$ch.seco.gdp_exp.nom_q_p5111c_pct.kof_baublatt_ausblick, 
  start = c(1981, 1), 
  frequency = 4
  )

### KOF-Baublatt-Ausblick based on building permits -----
kof_baubl_ausbl <- get_time_series(ts_keys = "ch.kof.baublatt.ausblick.nom.chf_pct")
kof_baubl_ausbl <- ts(
  data = kof_baubl_ausbl$ch.kof.baublatt.ausblick.nom.chf_pct, 
  start = c(2023, 1), 
  frequency = 4
  )


# Plot the data ----
ts_df(ts_c(kof_nom_const_inv, kof_baubl_ausbl)) |> 
  ggplot() +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(expand = c(0, 0), limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(-15, 20), breaks = c(-15, -10, -5, 0, 5, 10, 15, 20)) +
  scale_color_manual(values = c("#1B9E77", "#374e8e")) +
  labs(
    title = "<span style = 'color: #1B9E77;'>KOF-Baublatt-Index</span> and <span style = 'color: #374e8e;'>nominal construction investment (in %)</span>.",
    subtitle = "Baublatt-Index based on building permits.",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.title = element_markdown(), legend.position = "none")

ggsave(filename = "Indicators/KOF_Baublatt/Fig_Nominal-Construction-Investment-KOF-Baublatt-Index.png", width = 8, height = 4)
graphics.off()

# ************************************************************************
## Methodology ----
# URL: https://kof.ethz.ch/en/forecasts-and-indicators/indicators/kof-baublatt-outlook/methodology.html
# ************************************************************************

### Fitted values vs construction investment ----

list_keys_in_collection(collectionname = "ds_kof_baublatt_ausblick_fitted_qtr")
# ch.kof.baublatt.ausblick.fitted_values.nom.chf_pct

kof_baubl_fitted <- get_time_series(ts_keys = "ch.kof.baublatt.ausblick.fitted_values.nom.chf_pct")
kof_baubl_fitted <- ts(
  data = kof_baubl_fitted$ch.kof.baublatt.ausblick.fitted_values.nom.chf_pct, 
  start = c(2001, 1), 
  frequency = 4
  )

### Plot the data ----
ts_df(ts_c(kof_nom_const_inv, kof_baubl_fitted)) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(expand = c(0, 0), limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(-15, 20), breaks = c(-15, -10, -5, 0, 5, 10, 15, 20)) +
  scale_color_manual(values = c("#1B9E77", "#374e8e")) +
  labs(
    title = "<span style = 'color: #1B9E77;'>Fitted values</span> and <span style = 'color: #374e8e;'>nominal construction investment (in %)</span>.",
    subtitle = "Baublatt-Index based on building permits.",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.title = element_markdown(), legend.position = "none")

ggsave(filename = "Indicators/KOF_Baublatt/Fig_Nominal-Construction-Investment-Fitted-Values.png", width = 8, height = 4)
graphics.off()


### Real-time comparison ----
list_keys_in_collection(collectionname = "ds_kof_baublatt_ausblick_nowcast_qtr")
# ch.kof.baublatt.ausblick.nowcast.nom.chf_pct, ch.seco.gdp_exp.nom_q_p5111c_pct.first_release

kof_baubl_ncst <- get_time_series(ts_keys = "ch.kof.baublatt.ausblick.nowcast.nom.chf_pct")
kof_baubl_ncst <- ts(
  data = kof_baubl_ncst$ch.kof.baublatt.ausblick.nowcast.nom.chf_pct, 
  start = c(2004, 4), 
  frequency = 4
  )

### Plot the data ----
ts_df(ts_c(kof_nom_const_inv, kof_baubl_ncst)) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(expand = c(0, 0), limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(-15, 20), breaks = c(-15, -10, -5, 0, 5, 10, 15, 20)) +
  scale_color_manual(values = c("#1B9E77", "#374e8e")) +
  labs(
    title = "<span style = 'color: #1B9E77;'>Nowcast of KOF-Baublatt-Index</span> and <span style = 'color: #374e8e;'>nominal construction investment (in %)</span>.",
    subtitle = "Baublatt-Index based on building permits.",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.title = element_markdown(), legend.position = "none")

ggsave(filename = "Indicators/KOF_Baublatt/Fig_Nominal-Construction-Investment-Fidded-Values-Nowcast.png", width = 8, height = 4)
graphics.off()

# END
# ************************************************************************
# GDP++ ----
# ************************************************************************
# URL: https://kof.ethz.ch/en/forecasts-and-indicators/indicators/gdpplusplus.html
# GDP++ is a new measure for U.S. GDP growth resulting from data reconciliation that
# is shown to undergo smaller revisions than a simple average of GDE and GDI as published
# by the BEA (Aruoba et al. (2016)).
# GDP++ puts more weight on the expenditure side (Jacobs, Sarferaz, Sturm and van Norden (2020)).
# Feel free to copy, adapt, and use this code for your own purposes at
# your own risk.
#
# Matthias Spichiger, 2023 (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(readxl)
library(fredr)
library(ggtext)


## Access the data ----

### Access GDP++ from KOF ----
GDPpp_url <- "https://ethz.ch/content/dam/ethz/special-interest/dual/kof-dam/documents/Webseite_Dokumente/GDPplusplus_data.xls"
download.file(url = GDPpp_url, destfile = "GDP++/GDPpp.xls", method = "curl")

GDPpp  <- readxl::read_excel(path = "GDP++/GDPpp.xls") |> 
  rename(value = `GDP++`) |> 
  mutate(
    date = str_replace_all(date, pattern = c(" Q1" = "-01-01", " Q2" = "-04-01", " Q3" = "-07-01", " Q4" = "-10-01"))
  ) |> 
  mutate(
    date = date(date),
    series_id = "GDPpp"
    ) |> 
  select(date, series_id, value)


### Access GDE and GDI from FRED ----
params <- list(
  series_id = c("GDPC1", "GDI"),
  units = c("pca", "pca")
)
df <- purrr::pmap_dfr(.l = params, .f = ~fredr(series_id = .x, units = .y)) |> 
  select(date, series_id, value)


## Plot the data ----
rbind(df, GDPpp) |> 
  pivot_wider(names_from = series_id, values_from = value) |> 
  mutate(GDPavg = (GDPC1 + GDI) / 2) |> 
  pivot_longer(cols = GDPC1:GDPavg, names_to = "series_id", values_to = "value") |> 
  ggplot(mapping = aes(x = date, y = value, color = series_id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", show.legend = NULL) +
  scale_x_date(limits = c(date("2003-01-01"), date("2017-01-01")), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 2)) +
  scale_color_manual(
    values = c("#374e8e", "#ac004f", "#1B9E77", "#e3b13e"), 
    breaks = c("GDPC1", "GDI", "GDPpp", "GDPavg")
    ) +
  theme_bw() +
  labs(
    title = "Real GDP Growth",
    subtitle = "SAAR: <span style = 'color: #374e8e;'>GDE</span>, <span style = 'color: #ac004f;'>GDI</span>, <span style = 'color: #1B9E77;'>GDP++</span> and <span style = 'color: #e3b13e;'>Mean(GDE, GDI)</span>",
    caption = "Graph created by @econmaett with data from FRED.",
    x = "", y = ""
  ) +
  theme(plot.subtitle = element_markdown(), legend.position = "none")
  
ggsave(filename = "GDP++/GDPMeasures.png", width = 8, height = 4)
graphics.off()

# END
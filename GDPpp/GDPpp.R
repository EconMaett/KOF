# ************************************************************************
# GDP++ ----
# ************************************************************************
# URL: https://kof.ethz.ch/en/forecasts-and-indicators/indicators/gdpplusplus.html
# Feel free to copy, adapt, and use this code for your own purposes.
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(fredr)
library(ggtext)

usrecdp <- read_csv(file = "Recession-Dates/Recession-Dates_NBER_US_Daily_Midpoint.csv")

## Access the data ----

### Access GDP++ from KOF ----
GDPpp_url <- "https://ethz.ch/content/dam/ethz/special-interest/dual/kof-dam/documents/Webseite_Dokumente/GDPplusplus_data.xls"
download.file(url = GDPpp_url, destfile = "GDPpp/GDPpp.xls", method = "curl")

GDPpp  <- readxl::read_excel(path = "GDPpp/GDPpp.xls") |> 
  rename(value = `GDP++`) |> 
  mutate(
    date = str_replace_all(date, pattern = c(" Q1" = "-01-01", " Q2" = "-04-01", " Q3" = "-07-01", " Q4" = "-10-01"))
  ) |> 
  mutate(
    date = date(date),
    series_id = "GDPpp"
    ) |> 
  select(date, series_id, value)


### Access GDP+ from Philadelphia Fed ----
GDPp_url <- "https://www.philadelphiafed.org/-/media/frbp/assets/data-visualizations/gdpplus.xlsx"
download.file(url = GDPp_url, destfile = "GDPpp/GDPp.xlsx", method = "curl")
GDPp <- readxl::read_excel(path = "GDPpp/GDPp.xlsx") |> 
  mutate(
    date = date(paste0(OBS_YEAR, "-", 3*OBS_QUARTER-2, "-01"))
  ) |> 
  rename(GDE = GRGDP_DATA, GDI = GRGDI_DATA, GDPp = GDPPLUS_DATA) |> 
  select(date, GDE, GDI, GDPp) |> 
  mutate(GDPavg = (GDE + GDI) / 2) |> 
  pivot_longer(cols = GDE:GDPavg, names_to = "series_id", values_to = "value")


### Plot the data ----
rbind(GDPp, GDPpp) |> 
  pivot_wider(names_from = series_id, values_from = value) |> 
  mutate(
    Min = pmin(GDE, GDI, na.rm = TRUE),
    Max = pmax(GDE, GDI, na.rm = TRUE)
  ) |>
  ggplot() +
  geom_ribbon(mapping = aes(x = date, ymin = Min, ymax = Max), fill = "#374e8e", alpha = 0.3) +
  geom_line(mapping = aes(x = date, y = GDPavg), color = "#374e8e", linewidth = 1) +
  geom_line(mapping = aes(x = date, y = GDPp), color = "#006d64", linewidth = 1) +
  geom_line(mapping = aes(x = date, y = GDPpp), color = "#ac004f", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", show.legend = NULL) +
  geom_rect(data = usrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  scale_x_date(limits = c(date("2003-01-01"), date("2017-01-01")), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 5)) +
  theme_bw() +
  labs(
    title = "Real GDP Growth Mesures", 
    subtitle = "<span style = 'color: #374e8e;'>Average of GDE and GDI (with Min/Max)</span>, <span style = 'color: #006d64;'>GDP+</span> and <span style = 'color: #ac004f;'>GDP++</span>", 
    caption = "Graph created by @econmaett with data from KOF and the Philadelphia Federal Reserve Bank.",
    x = "", y = ""
  ) +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

ggsave(filename = "GDPpp/GDPMeasures.png", width = 8, height = 4)
graphics.off()

# END
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

usrecdp <- read_csv(file = "GDPpp/US_NBER_Recession_Dates.csv")

## Access the data ----

### Access GDP++ from KOF ----
# GDP++ is a new measure for U.S. GDP growth resulting from data reconciliation that
# is shown to undergo smaller revisions than a simple average of GDE and GDI as published
# by the BEA (Aruoba et al. (2016)).
# GDP++ puts more weight on the expenditure side (Jacobs, Sarferaz, Sturm and van Norden (2020)).
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
# URL: https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/gdpplus
# GDPplus is the Federal Reserve Bank of Philadelphia's measure of the quarter-over-quarter 
# rate of growth of real output in continuously compounded annualized percentage points. 
# It improves on the Bureau of Economic Analysis's expenditure-side and income-side measures.

# The expenditure-side measure (real GDP or real GDE) is more commonly used by economic analysts, 
# whereas the income-side measure, sometimes called real gross domestic income (real GDI), 
# is little used, but each contains useful information. 

# In particular, as proposed in Aruoba, Diebold, Nalewaik, Schorfheide, and Song (ADNSS), 
# one can view both real GDP and real GDI as noisy indicators of true, underlying, 
# latent real gross domestic product, which can then be estimated using optimal filtering methods. 

# Here we implement the ADNSS statistical framework. We call the optimal estimate GDPplus. 
# (We construct GDPplus via the Kalman smoother.)

# On this web page, we provide GDPplus data. 
# We will update the page whenever we construct a new GDPplus vintage, 
# whether due to the BEA's release of an additional observation or a revision of previously 
# available observations for the real GDP and real GDI indicators. 
# Note that our annualized growth rates use the formula for continuous compounding and are 
# expressed in percentage points. 
# We estimate the ADNSS two-equation model with the identifying restriction on the variance matrix 
# for the shocks. 
# We re-estimate the model for every new vintage using the method of maximum likelihood.
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
  geom_ribbon(mapping = aes(x = date, ymin = Min, ymax = Max), fill = "#383751", alpha = 0.2) +
  geom_line(mapping = aes(x = date, y = GDPavg), color = "black", linewidth = 1) +
  geom_line(mapping = aes(x = date, y = GDPp), color = "#006d64", linewidth = 1) +
  geom_line(mapping = aes(x = date, y = GDPpp), color = "#374e8e", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", show.legend = NULL) +
  geom_rect(data = usrecdp, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), fill = "grey", alpha = 0.2) +
  scale_x_date(limits = c(date("2003-01-01"), date("2017-01-01")), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 5)) +
  theme_bw() +
  labs(
    title = "Real GDP Growth Mesures", 
    subtitle = "<span style = 'color: black;'>Average of GDE and GDI (with Min/Max)</span>, <span style = 'color: #006d64;'>GDP+</span> and <span style = 'color: #374e8e;'>GDP++</span>", 
    caption = "Graph created by @econmaett with data from KOF and the Philadelphia Federal Reserve Bank.",
    x = "", y = ""
  ) +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

ggsave(filename = "GDPpp/GDPMeasures.png", width = 8, height = 4)
graphics.off()

# END
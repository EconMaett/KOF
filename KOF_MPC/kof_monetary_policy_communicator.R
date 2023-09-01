# ************************************************************************
# KOF Monetary Policy Communicator ----
# URL: https://kof.ethz.ch/en/forecasts-and-indicators/indicators/kof-monetary-policy-communicator.html
# Feel free to copy, adapt, and use this code for your own purposes at
# your own risk.
#
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(ggtext)
library(readxl)
library(eurostat)


## Access the data ----

### KOF Monetary Policy Communicator ----
kof_mpc_url <- "https://datenservice.kof.ethz.ch/api/v1/public/ts?keys=ch.kof.mpc&mime=xlsx&df=YYYY-MM-DD"

download.file(url = kof_mpc_url, destfile = "KOF_MPC/KOF_MPC.xlsx", method = "curl")
kof_mpc <- readxl::read_excel(path = "KOF_MPC/KOF_MPC.xlsx")


### ECB Main Refinancing Rate ----

eurostat::search_eurostat(pattern = "marginal lending facility")

ecb_mrr <- get_eurostat(id = "teibs010", time_format = "date", select_time = "M", 
                       sinceTimePeriod = "1990-01-01")

## Plot the data ----

kof_mpc |> 
  mutate(date = ymd(date)) |> 
  rename(time = date, value = ch.kof.mpc) |> 
  ggplot(mapping = aes(x = time, y = value)) +
  geom_line(linewidth = 1, color = "black") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    title = "KOF monetary policy communicator", 
    subtitle = "",
    caption = "Graph created by @econmaett with data from KOF.",
    x = "", y = ""
  ) +
  theme_bw()


# END
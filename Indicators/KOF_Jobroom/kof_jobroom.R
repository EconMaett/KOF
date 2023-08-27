# ************************************************************************
# KOF Jobroom ----
# URL: https://www.job-room.ch/home/job-seeker
#
# Feel free to copy, adapt, and use this code for your own purposes at
# your own risk.
#
# Matthias Spichiger, 2023 (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(xts)
library(tsbox)
library(kofdata)
library(ggtext)

## Access the data ----

list_keys_in_collection(collectionname = "ch.seco.jobroom")
# available by cantons or total (tot)

# ch.seco.jobroom.candidates.immediate.[canton]
# ch.seco.jobroom.candidates.tot.[canton]
# ch.seco.jobroom.vacancies.[canton]
# ch.seco.jobroom.candidates_rate
# ch.seco.jobroom.predicted_unemprate

#### Total
seco.jobroom.cand.imm.tot <- get_time_series(ts_keys = "ch.seco.jobroom.candidates.immediate.tot")

seco.jobroom.cand.imm.tot <- ts(
  start = c(2019)
)

plot(seco.jobroom.cand.imm.tot)

# END
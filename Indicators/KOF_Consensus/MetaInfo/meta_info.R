# ************************************************************************
# KOF Consensus forecast - meta information -----
# Feel free to copy, adapt, and use this code for your own purposes at
# your own risk.
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# *************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(ggtext)

## Access the data ----

# kof_consensus_forecast        KOF Consensus Forecast
# kof_consensus_forecast_count  KOF Consensus Forecast Count
# kof_consensus_forecast_dates  KOF Consensus Forecast Dates
# kof_consensus_forecast_max    KOF Consensus Forecast Maximum
# kof_consensus_forecast_mean   KOF Consensus Forecast Mean
# kof_consensus_forecast_median KOF Consensus Forecast Median
# kof_consensus_forecast_min    KOF Consensus Forecast Minimum
# kof_consensus_forecast_prob   KOF Consensus Forecast Probabilites
# kof_consensus_forecast_stdev  KOF Consensus Forecast Standard Deviation

list_keys_in_collection(collectionname = "kof_consensus_forecast")


## Meta information ----

### KOF media release day ----
# ch.kof.consensus.media_release_day
kof_cons_release_date <- get_time_series(ts_keys = "ch.kof.consensus.media_release_day")
kof_cons_release_date <- ts(
  data = kof_cons_release_date$ch.kof.consensus.media_release_day,
  start = c(2001, 2),
  frequency = 4
)


### KOF questionnaire ----
# ch.kof.consensus.questionnaire_end_day    
kof_cons_quest_end <- get_time_series(ts_keys = "ch.kof.consensus.questionnaire_end_day")
kof_cons_quest_end <- ts(
  data = kof_cons_quest_end$ch.kof.consensus.questionnaire_end_day,
  start = c(2001, 2),
  frequency = 4
)


# ch.kof.consensus.questionnaire_start_day
kof_cons_quest_start <- get_time_series(ts_keys = "ch.kof.consensus.questionnaire_start_day")
kof_cons_quest_start <- ts(
  data = kof_cons_quest_start$ch.kof.consensus.questionnaire_start_day,
  start = c(2001, 2),
  frequency = 4
)

# END
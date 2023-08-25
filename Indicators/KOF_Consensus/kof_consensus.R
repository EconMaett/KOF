# ************************************************************************
# KOF Consensus forecast -----
#
# Feel free to copy, adapt, and use this code for your own purposes at
# your own risk.
#
# Matthias Spichiger, 2023 (matthias.spichiger@bluewin.ch)
# ************************************************************************

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

### KOF questionnaire ----
# ch.kof.consensus.questionnaire_end_day     
# ch.kof.consensus.questionnaire_start_day

# END
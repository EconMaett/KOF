# ************************************************************************
# KOF Tourism Forecasts ----
# URL: https://kof.ethz.ch/en/forecasts-and-indicators/forecasts/kof-forecasts-of-tourism-in-switzerland.html
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

### Total ----
# ch.kof.trsm.fcst.year   KOF Tourism Forecast Total Year
list_keys_in_collection(collectionname = "ch.kof.trsm.fcst.year")
# ch.kof.trsm.fcst.ty.l.all.alpin.lg ch.kof.trsm.fcst.ty.l.all.ch.lg
# ch.kof.trsm.fcst.ty.l.all.city.lg  ch.kof.trsm.fcst.ty.l.all.rest.lg 
# ch.kof.trsm.fcst.ty.l.che.ch.lg    ch.kof.trsm.fcst.ty.l.chn.ch.lg 
# ch.kof.trsm.fcst.ty.l.deu.ch.lg    ch.kof.trsm.fcst.ty.l.for.ch.lg 
# ch.kof.trsm.fcst.ty.l.fra.ch.lg    ch.kof.trsm.fcst.ty.l.ita.ch.lg   
# ch.kof.trsm.fcst.ty.l.rest.ch.lg   ch.kof.trsm.fcst.ty.l.usa.ch.lg  
kof_trsm_fcst.ty.l.all.ch.lg <- get_time_series(ts_keys = "ch.kof.trsm.fcst.ty.l.all.ch.lg")
kof_trsm_fcst.ty.l.all.ch.lg <- ts(
  data = kof_trsm_fcst.ty.l.all.ch.lg$ch.kof.trsm.fcst.ty.l.all.ch.lg,
  start = c(2006),
  frequency = 1
)

plot(kof_trsm_fcst.ty.l.all.ch.lg)

### Summer season ----

# ch.kof.trsm.fcst.summer KOF Tourism Forecast Summer Season

list_keys_in_collection(collectionname = "ch.kof.trsm.fcst.summer")
# ch.kof.trsm.fcst.ss.l.all.alpin.lg ch.kof.trsm.fcst.ss.l.all.ch.lg   
# ch.kof.trsm.fcst.ss.l.all.city.lg  ch.kof.trsm.fcst.ss.l.all.rest.lg 
# ch.kof.trsm.fcst.ss.l.che.ch.lg    ch.kof.trsm.fcst.ss.l.chn.ch.lg   
# ch.kof.trsm.fcst.ss.l.deu.ch.lg    ch.kof.trsm.fcst.ss.l.for.ch.lg   
# ch.kof.trsm.fcst.ss.l.fra.ch.lg    ch.kof.trsm.fcst.ss.l.ita.ch.lg   
# ch.kof.trsm.fcst.ss.l.rest.ch.lg   ch.kof.trsm.fcst.ss.l.usa.ch.lg


### Winter season ----
# ch.kof.trsm.fcst.winter KOF Tourism Forecast Winter Season

list_keys_in_collection(collectionname = "ch.kof.trsm.fcst.winter")
# ch.kof.trsm.fcst.ws.l.all.alpin.lg ch.kof.trsm.fcst.ws.l.all.ch.lg   
# ch.kof.trsm.fcst.ws.l.all.city.lg  ch.kof.trsm.fcst.ws.l.all.rest.lg 
# ch.kof.trsm.fcst.ws.l.che.ch.lg    ch.kof.trsm.fcst.ws.l.chn.ch.lg   
# ch.kof.trsm.fcst.ws.l.deu.ch.lg    ch.kof.trsm.fcst.ws.l.for.ch.lg   
# ch.kof.trsm.fcst.ws.l.fra.ch.lg    ch.kof.trsm.fcst.ws.l.ita.ch.lg   
# ch.kof.trsm.fcst.ws.l.rest.ch.lg   ch.kof.trsm.fcst.ws.l.usa.ch.lg


# END
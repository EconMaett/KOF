# KOF Employment Indicator ----

# URL: https://kof.ethz.ch/prognosen-indikatoren/indikatoren/kof-beschaeftigungsindikator.html

# KOF Beschaeftigungsindikator 
list_keys_in_collection(collectionname = "ogd_ch.kof.ie")
# Assessment (Urteil): ch.kof.ie.retro.ch_total.ass.d11
# Expectation (Erwartung): ch.kof.ie.retro.ch_total.exp.d11
# Index (Insgesamt): ch.kof.ie.retro.ch_total.ind.d11

kof_empl_ass <- get_time_series(ts_keys = "ch.kof.ie.retro.ch_total.ass.d11")
kof_empl_ass.ts <- ts(
  data = kof_empl_ass$ch.kof.ie.retro.ch_total.ass.d11,
  start = c(1992, 3),
  frequency = 4
)

kof_empl_exp <- get_time_series(ts_keys = "ch.kof.ie.retro.ch_total.exp.d11")
kof_empl_exp.ts <- ts(
  data = kof_empl_exp$ch.kof.ie.retro.ch_total.exp.d11,
  start = c(1992, 3),
  frequency = 4
)

kof_empl_ind <- get_time_series(ts_keys = "ch.kof.ie.retro.ch_total.ind.d11")
kof_empl_ind.ts <- ts(
  data = kof_empl_ind$ch.kof.ie.retro.ch_total.ind.d11,
  start = c(1992, 3),
  frequency = 4
)

plot(kof_empl_ind.ts, col = "black")
lines(kof_empl_exp.ts, col = "darkblue")
lines(kof_empl_ass.ts, col = "darkgreen")
abline(h = 0, col = "darkgrey")
graphics.off()
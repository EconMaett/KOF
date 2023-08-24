# KOF Economic Barometer ----
# URL: https://kof.ethz.ch/prognosen-indikatoren/indikatoren/kof-konjunkturbarometer.html

list_keys_in_collection(collectionname = "ogd_ch.kof.barometer")
# "ch.kof.barometer"

kof_baro <- get_time_series(ts_keys = "ch.kof.barometer")
kof_baro.ts <- ts(
  data = kof_baro$ch.kof.barometer,
  start = c(1991, 1),
  frequency = 12
)

plot(kof_baro.ts, col = "black")
abline(h = 100, col = "darkgrey")
graphics.off()

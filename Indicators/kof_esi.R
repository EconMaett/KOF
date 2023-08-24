# KOF Economic Sentiment Indicator (ESI) ----
# URL: https://kof.ethz.ch/prognosen-indikatoren/indikatoren/kof-economic-sentiment-indicator.html

list_keys_in_collection(collectionname = "ogd_ch.kof.esi")
#  "ch.kof.esi.index"       "ch.kof.esi.index.v2018"

kof_esi <- get_time_series(ts_keys = "ch.kof.esi.index")
kof_esi.ts <- ts(
  data = kof_esi$ch.kof.esi.index,
  start = c(2007, 4),
  frequency = 12
)

plot(kof_esi.ts, col = "black")
abline(h = 100, col = "darkgrey")
graphics.off()

# END
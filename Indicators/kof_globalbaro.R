# KOF Global Barometer ----


list_keys_in_collection(collectionname = "ogd_ch.kof.globalbaro")
# "ch.kof.globalbaro.coincident" "ch.kof.globalbaro.leading"

kof_globalbaro_lead <- get_time_series(ts_keys = "ch.kof.globalbaro.leading")
kof_globalbaro_lead.ts <- ts(
  data = kof_globalbaro_lead$ch.kof.globalbaro.leading, 
  start = c(1991, 7), 
  frequency = 12
  )

kof_globalbaro_coi <- get_time_series(ts_keys = "ch.kof.globalbaro.coincident")
kof_globalbaro_coi.ts <- ts(
  data = kof_globalbaro_coi$ch.kof.globalbaro.coincident,
  start = c(1991, 7),
  frequency = 12
)

plot(kof_globalbaro_lead.ts, col = "darkblue")
lines(kof_globalbaro_coi.ts, col = "darkgreen")
abline(h = 100, color = "darkgrey")
graphics.off()
# END
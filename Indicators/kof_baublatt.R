# KOF-Baublatt-Ausblick ----

# URL: https://kof.ethz.ch/prognosen-indikatoren/indikatoren/kof-baublatt-ausblick.html

# 16  ds_kof_baublatt_ausblick_fitted_qtr: KOF-Baublatt-Ausblick fitted values
# 17 ds_kof_baublatt_ausblick_nowcast_qtr: KOF-Baublatt-Ausblick Realtime
# 18         ds_kof_baublatt_ausblick_qtr: KOF-Baublatt-Ausblick

list_keys_in_collection(collectionname = "ds_kof_baublatt_ausblick_qtr")
# ch.kof.baublatt.ausblick.nom.chf_pct, ch.seco.gdp_exp.nom_q_p5111c_pct.kof_baublatt_ausblick

kof_baubl_hist <- get_time_series(ts_keys = "ch.seco.gdp_exp.nom_q_p5111c_pct.kof_baublatt_ausblick")
kof_baubl_hist.ts <- ts(
  data = kof_baubl_hist$ch.seco.gdp_exp.nom_q_p5111c_pct.kof_baublatt_ausblick, 
  start = c(1981, 1), 
  frequency = 4
  )

kof_baubl_ausbl <- get_time_series(ts_keys = "ch.kof.baublatt.ausblick.nom.chf_pct")
kof_baubl_ausbl.ts <- ts(
  data = kof_baubl_ausbl$ch.kof.baublatt.ausblick.nom.chf_pct, 
  start = c(2023, 1), 
  frequency = 4
  )

plot(kof_baubl_hist.ts, col = "black") # Bauinvestitionen
lines(kof_baubl_ausbl.ts, col = "darkgreen") # KOF-Baublatt Ausblick
abline(h = 0, col = "darkgrey")
graphics.off()

## Methodik ----
# URL: https://kof.ethz.ch/prognosen-indikatoren/indikatoren/kof-baublatt-ausblick/methodik.html

### Fitted vs actual values

list_keys_in_collection(collectionname = "ds_kof_baublatt_ausblick_fitted_qtr")
# ch.kof.baublatt.ausblick.fitted_values.nom.chf_pct
kof_baubl_fitted <- get_time_series(ts_keys = "ch.kof.baublatt.ausblick.fitted_values.nom.chf_pct")
kof_baubl_fitted.ts <- ts(
  data = kof_baubl_fitted$ch.kof.baublatt.ausblick.fitted_values.nom.chf_pct, 
  start = c(2001, 1), 
  frequency = 4
  )

plot(kof_baubl_hist.ts, col = "black") # Bauinvestitionen
lines(kof_baubl_fitted.ts, col = "darkgreen") # Fitted values
abline(h = 0, col = "darkgrey")
graphics.off()


### Bauinvesitionen vs kurzfristige Prognose ----
list_keys_in_collection(collectionname = "ds_kof_baublatt_ausblick_nowcast_qtr")
# ch.kof.baublatt.ausblick.nowcast.nom.chf_pct, ch.seco.gdp_exp.nom_q_p5111c_pct.first_release
kof_baubl_ncst <- get_time_series(ts_keys = "ch.kof.baublatt.ausblick.nowcast.nom.chf_pct")
kof_baubl_ncst.ts <- ts(
  data = kof_baubl_ncst$ch.kof.baublatt.ausblick.nowcast.nom.chf_pct, 
  start = c(2004, 4), 
  frequency = 4
  )

plot(kof_baubl_hist.ts, col = "black") # Bauinvesitionen
lines(kof_baubl_ncst.ts, col = "darkgreen") # Nowcast
abline(h = 0, col = "darkgrey")
graphics.off()
# END
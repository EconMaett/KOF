# KOF Geschaeftslage Indikator ----
# URL: https://kof.ethz.ch/prognosen-indikatoren/indikatoren/kof-geschaeftslageindikator.html

# bs_indicator: KOF Geschäftslage Indikator und Subbranchen
list_keys_in_collection(collectionname = "bs_indicator")

# Wirtschaftsbereiche:
# - Industrie: (INU): ch.kof.inu.ng08.fx.q_ql_ass_bs.balance.d11
# - Detailhandel: (DHU) ch.kof.dhu.ng08.fx.q_ql_ass_bs.balance.d11
# - Baugewerbe: (BAU) ch.kof.bau.ng08.fx.q_ql_ass_bs.balance.d11
# - Projektierung: 
# - Finanz- und Versicherungsdienstleistungen: (FVU) ch.kof.fvu.ng08.fx.q_ql_ass_bs.balance.d11
# - übrige Dienstleistungsbranchen:

# - Dienstleistungsunernehmen: (DLU) ch.kof.dlu.ng08.fx.q_ql_ass_bs.balance.d11
# - Grosshandelsunternehmen: (GHU) ch.kof.ghu.ng08.fx.q_ql_ass_bs.balance.d11

# - AIU ch.kof.aiu.ng08.fx.q_ql_ass_bs.balance.d11
# - GGU ch.kof.ggu.ng08.fx.q_ql_ass_bs.balance.d11

# Industrieunternehmen (INU) ----
kof_bs_inu <- get_time_series(ts_keys = "ch.kof.inu.ng08.fx.q_ql_ass_bs.balance.d11")
kof_bs_inu.ts <- ts(
  data = kof_bs_inu$ch.kof.inu.ng08.fx.q_ql_ass_bs.balance.d11,
  start = c(2004, 1),
  frequency = 12
)

plot(kof_bs_inu.ts, col = "black")
abline(h = 0, col = "darkgrey")
graphics.off()

# Detailhandelsunternehmen (DHU) ----
kof_bs_dhu <- get_time_series(ts_keys = "ch.kof.dhu.ng08.fx.q_ql_ass_bs.balance.d11")
kof_bs_dhu.ts <- ts(
  data = kof_bs_dhu$ch.kof.dhu.ng08.fx.q_ql_ass_bs.balance.d11,
  start = c(1973, 2),
  frequency = 12
)

plot(kof_bs_dhu.ts, col = "black")
abline(h = 0, col = "darkgrey")
graphics.off()


# Bauunternehmen (BAU) ----
kof_bs_bau <- get_time_series(ts_keys = "ch.kof.bau.ng08.fx.q_ql_ass_bs.balance.d11")
kof_bs_bau.ts <- ts(
  data = kof_bs_bau$ch.kof.bau.ng08.fx.q_ql_ass_bs.balance.d11,
  start = c(1994, 10),
  frequency = 12
)

plot(kof_bs_bau.ts, col = "black")
abline(h = 0, col = "darkgrey")
graphics.off()

# Finanz- und Versicherungsunternehmen (FVU) ----
kof_ds_fvu <- get_time_series(ts_keys = "ch.kof.fvu.ng08.fx.q_ql_ass_bs.balance.d11")
kof_ds_fvu.ts <- ts(
  data = kof_ds_fvu$ch.kof.fvu.ng08.fx.q_ql_ass_bs.balance.d11,
  start = c(2010, 7),
  frequency = 12
)

plot(kof_ds_fvu.ts, col = "black")
abline(h = 0, col = "darkgrey")
graphics.off()

# Dienstleistungsunternehmen (DLU) ----
kof_ds_dlu <- get_time_series(ts_keys = "ch.kof.dlu.ng08.fx.q_ql_ass_bs.balance.d11")
kof_ds_dlu.ts <- ts(
  data = kof_ds_dlu$ch.kof.dlu.ng08.fx.q_ql_ass_bs.balance.d11,
  start = c(2006, 4),
  frequency = 4
)

plot(kof_ds_dlu.ts)
abline(h = 0, col = "darkgrey")
graphics.off()

# Grosshandelsunternehmen (GHU) ----
kof_ds_ghu <- get_time_series(ts_keys = "ch.kof.ghu.ng08.fx.q_ql_ass_bs.balance.d11")
kof_ds_ghu.ts <- ts(
  data = kof_ds_ghu$ch.kof.ghu.ng08.fx.q_ql_ass_bs.balance.d11,
  start = c(2007, 3),
  frequency = 4
)

plot(kof_ds_ghu.ts, col = "black")
abline(h = 0, col = "darkgrey")
graphics.off()

# Total ----
kof_ds_total <- get_time_series(ts_keys = "ch.kof.bts_total.ng08.fx.q_ql_ass_bs.balance.d11")
kof_ds_total.ts <- ts(
  data = kof_ds_total$ch.kof.bts_total.ng08.fx.q_ql_ass_bs.balance.d11,
  start = c(2009, 4),
  frequency = 4
)

plot(kof_ds_total.ts, col = "black")
abline(h = 0, col = "darkgrey")
graphics.off()


# END
# Computes pseudo real-time forecast statistics for various Global Baro versions (robustness tests).
# The pseudo real-time vintages for each Global Baro version are stored in the RData-files with the prefix "baros_".
# Make sure all files (scripts and data) are in the working directory.

library(data.table)
library(purrr)
source("utils.R")

# Function definitions
stat_table <- function(horizons, stat_func, relative = function(x) x, decreasing = F) {
  from <- dates[1]
  to <- dates[length(dates)]

  vals <- lapply(horizons, function(h) {
    vals <- sapply(forecasts, function(x) {
      stat_func(ts_win(x[[paste0("H", h)]], start = from, end = to), stats::lag(gdp_ref_eval, h))
    })
    dt <- do.call(data.table, as.list(round(vals, 2)))
  })
  dt <- cbind(h = horizons, data.table(rbindlist(vals), keep.rownames = T))
  names(dt) <- c("Horizon", versions)
  dt
}

stat_table_dm <- function(horizons, ref, alt) {
  from <- dates[1]
  to <- dates[length(dates)]

  vals <- lapply(horizons, function(h) {
    hchar <- paste0("H", h)
    err1 <- ts_win(forecasts[[ref]][[hchar]], start = from, end = to) - stats::lag(gdp_ref_eval, h)
    lag_max <- floor(4 * (length(err1) / 100)^0.25)
    vals <- map2_dbl(forecasts, names(forecasts), function(x, ver) {
      err2 <- ts_win(x[[hchar]], start = from, end = to) - stats::lag(gdp_ref_eval, h)
      if (ver == ref) NA else forecast::dm.test(err1, err2, h = lag_max + 1, alternative = alt)$p.value
    })
    dt <- do.call(data.table, as.list(round(vals, 2)))
  })
  dt <- cbind(h = horizons, data.table(rbindlist(vals), keep.rownames = T))
  names(dt) <- c("Horizon", versions)
  dt
}

baro_fc <- function(baros, ref_win = 120, horizons = -5:12) {
  fch <- lapply(horizons, function(h) {
    pred <- sapply(baros, function(baro) {
      gdp_ref <- baro$gdp_ref
      factor <- baro$factor

      gdp_ref <- ts_win(gdp_ref, start = add_months(end_date(gdp_ref), -ref_win + 1))
      factor_is <- stats::lag(factor, -h)
      from <- max(start_date(gdp_ref), start_date(factor_is))
      to <- min(end_date(gdp_ref), end_date(factor_is))
      factor_is <- ts_win(factor_is, start = from, end = to)
      gdp_ref <- ts_win(gdp_ref, start = from, end = to)

      lm_res <- lm(gdp_ref ~ factor_is)

      baro_pred <- factor * lm_res$coefficients[-1] + lm_res$coefficients[1]
      as.vector(tail(baro_pred, 1))
    })
    make_ts(pred, start = as.Date(names(baros)[1]), frequency = 12)
  })
  names(fch) <- paste0("H", horizons)
  fch
}

# All Global Baro versions
versions <- c(
  coin = "Coincident Baseline",
  lead = "Leading Baseline",
  coin_pca = "Coincident Principal Component",
  lead_pca = "Leading Principal Component",
  coin_nocrisis = "Coincident No Crisis",
  lead_nocrisis = "Leading No Crisis",
  coin_novarsel = "Coincident No Variable Selection",
  lead_novarsel = "Leading No Variable Selection",
  coin_noreg = "Coincident No Regional Aggregation",
  lead_noreg = "Leading No Regional Aggregation"
)

# The evaluation period
dates <- as.character(seq(ymd_date(2002, 1), ymd_date(2018, 12), "month"))

# Compute forecasts with horizons -5 to 12 months of all versions
horizons <- -5:12
forecasts <- lapply(names(versions), function(version) {
  cat(version, "\n")
  # Load pseudo real-time vintages
  load(paste0("baros_", version, ".RData"), envir = environment())
  baro_fc(baros[dates], 120, horizons)
})
names(forecasts) <- names(versions)
save(forecasts, file = "forecasts.RData")

# The forecasts are evaluated against the newest GDP reference series in the evaluation period.
load("baros_coin.RData")
gdp_ref_eval <- baros$`2018-12-01`$gdp_ref

# Compute stat tables with baseline versions (coin/lead) as reference for DM test.
stats <- list(
  "Correlation" = stat_table(horizons, ts_cor, decreasing = T),
  "RMSE" = stat_table(horizons, rmse),
  "pValue Coincident" = stat_table_dm(horizons, "coin", "less"),
  "pValue Leading" = stat_table_dm(horizons, "lead", "less")
)
save(stats, file = "robustness_stats.RData")

# Write out as Excel
library(openxlsx)
wb <- createWorkbook()
for (name in names(stats)) {
  addWorksheet(wb, name)
  writeData(wb, sheet = name, x = stats[[name]])
}
saveWorkbook(wb, "robustness_stats.xlsx", overwrite = TRUE)

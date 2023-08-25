#' Returns Date object given year, month and day. Month and day are set to 1 by default.
#'
#' @param year integer
#' @param day integer
#' @param month integer
#' @export
ymd_date <- function(year, month = 1, day = 1) {
  as.Date(paste0(year, "-", month, "-", day))
}

#' Turns date into numeric to be used in ts-functions
#' @param date date
#' @export
date_to_time <- function(date) {
  data.table::year(date) + (data.table::month(date) - 1) / 12
}

#' Turns numeric used in ts-functions to date
#' @param time numeric
#' @export
time_to_date <- function(time) {
  time <- round(time, 3)
  ymd_date(floor(time), round((time - floor(time)) / (1 / 12)) + 1)
}

#' Returns numeric of start date
#' @param ts series
#' @export
start_time <- function(ts) {
  time(ts)[1]
}

#' Returns numeric of end date
#' @param ts series
#' @export
end_time <- function(ts) {
  time(ts)[length(ts)]
}

#' Returns start date of series
#' @param ts series
#' @export
start_date <- function(ts) {
  time_to_date(start_time(ts))
}

#' Returns end date of series
#' @param ts series
#' @export
end_date <- function(ts) {
  time_to_date(end_time(ts))
}

#' Returns frequency date
#'
#' @param date date
#' @param frequency frequency
#' @export
frequency_date <- function(date, frequency) {
  ymd_date(data.table::year(date), (ceiling(data.table::month(date) / (12 / frequency)) - 1) * (12 / frequency) + 1)
}

#' Turns monthly series into quarterly
#' @param x monthly series
to_quarterly_ts <- function(x) {
  dates <- zoo::as.Date(time(x))
  is_quarter <- month(dates) %in% c(1, 4, 7, 10)
  ts(x[is_quarter], start = date_to_time(dates[is_quarter][1]), frequency = 4)
}

#' Turns quarterly series into monthly
#' @param trim whether to trim NA-months of last quarter
#' @param x monthly series
to_monthly_ts <- function(x, trim = T) {
  vals <- unlist(lapply(as.vector(x), function(val) c(val, NA, NA)))
  x <- ts(vals, start = start_time(x), frequency = 12)
  if (trim) {
    x <- zoo::na.trim(x)
  }
  x
}

#' Difference between two dates in months
#' @param start_date start date
#' @param end_date end date
#' @export
diff_months <- function(start_date, end_date) {
  (year(end_date) - year(start_date)) * 12 + month(end_date) - month(start_date)
}

#' Add months to date
#' @param date date
#' @param months integer
#' @export
add_months <- function(date, months) {
  m <- data.table::month(date) - 1 + months
  new_month <- m %% 12 + 1
  as.Date(paste0(data.table::year(date) + m %/% 12, "-", new_month, "-", data.table::mday(date)))
}

#' Transforms data to mean zero and stdv 1.
#' @param x data
#' @export
stdize <- function(x) {
  (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
}

#' Window of series with dates
#'
#' @param x series
#' @param start start date
#' @param end end date
#' @param extend extend with NA
#' @export
ts_win <- function(x, start = NULL, end = NULL, extend = F) {
  window(x, start = if (is.null(start)) NULL else date_to_time(start), end = if (is.null(end)) NULL else date_to_time(end), extend = extend)
}

#' Makes ts from data and start/end dates
#'
#' @param x data
#' @param start date
#' @param end date
#' @param frequency integer
#' @export
make_ts <- function(x, start, end, frequency) {
  if (missing(start)) {
    ts(x, end = date_to_time(end), frequency = frequency)
  } else if (missing(end)) {
    ts(x, start = date_to_time(start), frequency = frequency)
  } else if (missing(frequency)) {
    ts(x, start = date_to_time(start), end = date_to_time(end))
  } else {
    ts(x, start = date_to_time(start), end = date_to_time(end), frequency = frequency)
  }
}

#' Computes correlation of 2 time series over common time period
#'
#' @param a
#' @param b
#' @export
ts_cor <- function(a, b) {
  freq <- frequency(a)
  if (freq != frequency(b)) {
    return(NULL)
  }

  from <- max(start_date(a), start_date(b))
  to <- min(end_date(a), end_date(b))

  cor(ts_win(a, from, to), ts_win(b, from, to))
}

#' RMSE
#'
#' @param a
#' @param b
#' @export
rmse <- function(a, b) {
  sqrt(mean((a - b)^2))
}

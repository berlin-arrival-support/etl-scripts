library(data.table)
library(ggplot2)
library(magrittr)
library(ggdist)


# Functions --------------------------------------------------------------------

#' @title Logistic Curve
#'
#' @param x (`numeric()`) A value/vector to evaluate the function at.
#' @param x0 (`numeric(1)`) Midpoint of the sigmoid
#' @param L (`numeric(1)`) Maximum value
#' @param k (`numeric(1)`) Logistic growth rate
#'
#' @return A vector of length `length(x)` of function values.
logistic_curve <- function(x, x0, L, k) {
  return(L / (1 + exp(- k * (x - x0))))
}


#' @title Get Estimate
#' @description
#' For a given start time and initial value, estimate the
#' logistic decay over a specified time frame.
#'
#' @param ts (`POSIXct(1)`)
#' @param init (`numeric(1)`)
#' @param max_mins (`numeric(1)`)
#' @param half_decay (`numeric(1)`)
#' @param k (`numeric(1)`) Logistic (de) growth rate
#'
#' @return A `data.table`
get_estimate <- function(ts, init, max_mins = 210, half_decay = 23, k = 0.01) {
  checkmate::assert_posixct(ts, len = 1)
  checkmate::assert_number(init)
  checkmate::assert_int(max_mins)
  checkmate::assert_number(half_decay)
  checkmate::assert_number(k)

  span <- 1:(max_mins)

  est <- logistic_curve(
    x = span,
    x0 = length(span) - half_decay,
    L = init,
    k = k
  )
  est <- rev(est)

  return(data.table(
    ts = (60 * span) + ts,
    est = est
  ))

}

#' @title Wrapper around get_estimate
#'
#' @return A `data.table`
get_estimate_dt <- function(data,
                            var_time,
                            var_pax,
                            k = 0.009,
                            max_mins = 300,
                            half_decay = 40) {

  ls_estimates <- mapply(
    get_estimate,
    data[[var_time]],
    data[[var_pax]],
    k = k,
    max_mins = max_mins,
    half_decay = half_decay,
    SIMPLIFY = FALSE
  )

  dt_range <- data.table(
    ts = seq(
      from = min(data[[var_time]]),
      to = max(data[[var_time]]) + max_mins * 60,
      by = "mins"
    ),
    est = 0
  )

  for (dt in ls_estimates)
    dt_range[dt, on = "ts", est := est + i.est]


  dt_range[, `:=`(
      day = as.Date(ts),
      time = as.POSIXct(strftime(ts, format = "%H:%M"), format = "%H:%M")
    )
  ]

  return(dt_range)
}


# Processing -------------------------------------------------------------------


DAYS <- 10

data <- fread("arrival-times-db/data/cleaned_db_data.R")
data[, ts_actual := fifelse(is.na(ts_actual), ts_planned, ts_actual)]

data_range <- get_estimate_dt(
  data,
  var_time = "ts_actual",
  var_pax = "pax_info",
  k = 0.05,
  max_mins = 450,
  half_decay = 100
)

data_range[est == 0, est := NA]

# Plotting ---------------------------------------------------------------------

data_range %>%
  ggplot() +
  aes(x = time, y = est) +
  geom_point(aes(group = day), alpha = 1/10) +
  theme_bw()

ggplot(data_range[day >= "2022-04-23", ]) +
  aes(x = time, y = est) +
  stat_lineribbon(
    .width = c(.5, .6, .7),
    size = 0,
    na.rm = TRUE) +
  theme_bw() +
  scale_fill_brewer()


data_range[day >= "2022-04-28",] %>%
  ggplot(aes(x = time, y = est)) +
  stat_lineribbon(
    aes(fill = stat(.width)),
    .width = ppoints(200),
    size = 0
    ) +
  scale_fill_distiller() +
  theme_bw() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours")



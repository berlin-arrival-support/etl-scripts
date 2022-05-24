# ETL Script: Arrival Data from Google Sheet -> DB

# Libraries
library(googlesheets4)
library(data.table)

# Functions --------------------------------------------------------------------

read_gs_dt <- function(sheet) {
  dt <- googlesheets4::read_sheet(
    ss = Sys.getenv("GS_SHEET_INFO_CLONE"),
    sheet = sheet,
    col_names = c(
      "date", "time", "type", "station", "id", "pax_info", "platform",
      "description", "time_new"
    ),
    col_types = "c",
    skip = 1
  )
  setDT(dt)
  return(dt)
}


# Processing -------------------------------------------------------------------

# 0. Connections, constants


DAY_SECONDS <- 60*60*24

gs4_deauth()
gs4_auth(path = Sys.getenv("GS_TOKEN"))

# 1. Reading data

# 1.a Arrival data from Google SheetQ
dta_arrivals_list <- lapply(c("arrival_times", "arrival_times_backup"), read_gs_dt)

dta_arrivals <- rbindlist(dta_arrivals_list)
dta_arrivals <- na.omit(dta_arrivals, cols = c("station", "id", "time"))
dta_arrivals <- unique(dta_arrivals, by = c("station", "id", "date", "time"))
dta_arrivals_bk <- copy(dta_arrivals)

dta_arrivals[, ts_planned := as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M")]
dta_arrivals[is.na(ts_planned),
    ts_planned := as.POSIXct(paste(date, time), format = "%d/%m/%Y %H:%M")
]

dta_arrivals[, diff := difftime(
    as.POSIXct(time_new, format = "%H:%M"),
    as.POSIXct(time, format = "%H:%M"),
    units = "secs"
)]

dta_arrivals[is.na(diff), diff := 0]

dta_arrivals[, ts_new := ifelse(
    diff >= -600,
    ts_planned + diff,
    ts_planned + diff + DAY_SECONDS
)]


dta_arrivals[, ts_new := as.POSIXct(ts_new, origin = "1970-01-01", tzone = "CET")]
dta_arrivals <- dta_arrivals[, c("station", "id", "pax_info", "platform", "description", "ts_planned", "ts_new")]
dta_arrivals <- dta_arrivals[, !c("status", "date", "time", "time_new", "diff")]

dta_arrivals[, hour := format(ts_planned, format = "%H:%M")]

recode_trains <- data.table(
    id = c(
      "RE1", "IC2432", "RE 56/ IC 2432", "IC1921",
      "IC1922", "IC1923", "IC1919", "IC1920", "IC1918",
      "EC246", "IC 248", "EC248", "EC40", "EC56", "EC378", "EC 60456/ NJ 456"),
    train_id_new = c(
      "RE 1", "IC 2432", "IC 2432", "IC 1921",
      "IC 1922", "IC 1923", "IC 1919", "IC 1920", "IC 1918",
      "EC 246", "EC 248", "EC 248", "EC 40", "EC 56", "EC 378",
      "NJ 456 / EC 60456")
)

dta_arrivals[recode_trains, on = "id", id := i.train_id_new]
dta_arrivals[, pax_info := as.numeric(gsub(
    ".*?([0-9]+).*",
    "\\1",
    pax_info
))]

dta_arrivals[, delay := difftime(ts_new, ts_planned, units = "mins")]
setnames(dta_arrivals, "ts_new", "ts_actual")
# dta_arrivals[, ts_new := NULL]

dta_arrivals <- dta_arrivals[, .(id, ts_planned, ts_actual, station, delay, hour, pax_info, platform, description)]
dta_arrivals <- unique(dta_arrivals, on = c("id", "ts_planned"))


# Write to remote data storage

db_data <- dta_arrivals[
  station == "Hbf (Train)"
    & !is.na(pax_info)
    & !grepl("RE", id)
    & id != 'ICE 1100'
  ]
db_data$days <- as.POSIXct(round(db_data$ts_planned, "days"))


db_data[, `:=`(
  hour = NULL,
  week = strftime(ts_planned, format = "%W"),
  week_day = strftime(ts_planned, format = "%w")
)]

export <- copy(db_data)

export[is.na(ts_actual), `:=`(ts_actual = ts_planned, delay = 0)]

export[, `:=`(
  ts_planned = as.character(ts_planned),
  ts_actual = as.character(ts_actual),
  days = as.character(format(days, format = "%Y-%m-%d")),
  delay = as.numeric(delay)
)]


fwrite(export, "arrival-times-db/data/cleaned_db_data.R")

write_sheet(
  data = export,
  ss = Sys.getenv("GS_SHEET_ARRIVAL_OUT"),
  sheet = "arrivals_clean"
)


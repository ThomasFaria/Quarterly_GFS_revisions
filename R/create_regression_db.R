library(data.table)
library(arrow)
library(stringr)
library(lubridate)
source("R/functions.R")

data <- arrow::read_parquet("data/RevisionDB.parquet")
data <- preprocess_revision_db(data)

DatasetRaw <- arrow::read_csv_arrow("data/RealTimeDatabase.csv")
setDT(DatasetRaw)
DatasetRaw <- preprocess_raw_db(DatasetRaw)

FinalValues <- arrow::read_parquet("data/FinalValues.parquet")
setDT(FinalValues)
FinalValues <- preprocess_final_values_db(FinalValues)

GRateDB <- arrow::read_parquet("data/GRateDB.parquet")
GRateDB <- preprocess_growth_rate_db(GRateDB)
DateRange <- seq(as.Date("2006-07-01"), as.Date("2019-10-01"), by = "quarter")
VintageList <- unique(GRateDB[, ECB_vintage])
date_to_vintage <- setNames(as.list(VintageList[1:length(as.list(DateRange))]), DateRange)
Countries <- unique(data$Country_code)
Variables <- unique(data$Variable_code)

get_past_revisions <- function(data, Country, Variable, Obs_date, date_to_vintage, lag) {
  compared_vintages <- c(
    date_to_vintage[[as.character(Obs_date %m-% months(3 * lag))]],
    date_to_vintage[[as.character(Obs_date)]]
  )

  past_revision <- diff(data[(Country_code %in% Country) &
    (Variable_code %in% Variable) &
    (Date == Obs_date %m-% months(3 * lag)) &
    (ECB_vintage %in% compared_vintages)][, Value])

  return(past_revision)
}

RegressionDB <- data.table()
for (country in Countries) {
  for (variable in Variables) {
    cat(country, variable, "\n")
    for (obs_date in as.character(DateRange)) {
      obs_date <- as.Date(obs_date)
      FinalRevision <- data[(Country_code %in% country) & (Variable_code %in% variable) &
        (Date %in% obs_date) & (Revision_nb == 1) &
        (Type_revision == "Final")][, Value]

      FirstAnnounGr <- GRateDB[(Country_code %in% country) & (Variable_code %in% variable) &
        (Date %in% obs_date) & (ECB_vintage %in% date_to_vintage[[as.character(obs_date)]])][, Value]

      past_revisions <- unlist(sapply(1:5, get_past_revisions,
        data = GRateDB,
        Country = country,
        Variable = variable,
        Obs_date = obs_date,
        date_to_vintage = date_to_vintage
      ))
      length(past_revisions) <- 5

      revisions_macro <- data[(Country_code %in% country) &
        (Variable_code %in% c("YEN", "ITN", "EXN", "GCN", "WGS", "PCN")) &
        (Date %in% obs_date) & (Revision_nb == 1) &
        (Type_revision == "Final")][, .(Variable_code, Value)][
        ,
        Variable_code := factor(Variable_code, levels = c("YEN", "ITN", "EXN", "GCN", "WGS", "PCN"))
      ][
        order(Variable_code)
      ][, Value]
      one_line <- data.table(
        Date = obs_date,
        Country_code = country,
        Variable_code = variable,
        Final_revision = FinalRevision,
        First_announcement = FirstAnnounGr,
        Rev_lag1 = past_revisions[1],
        Rev_lag2 = past_revisions[2],
        Rev_lag3 = past_revisions[3],
        Rev_lag4 = past_revisions[4],
        Rev_lag5 = past_revisions[5],
        Rev_YEN = revisions_macro[1],
        Rev_ITN = revisions_macro[2],
        Rev_EXN = revisions_macro[3],
        Rev_GCN = revisions_macro[4],
        Rev_WGS = revisions_macro[5],
        Rev_PCN = revisions_macro[6]
      )

      RegressionDB <- rbindlist(list(RegressionDB, one_line))
    }
  }
}

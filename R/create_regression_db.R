library(data.table)
library(arrow)
library(stringr)
library(lubridate)
source("R/functions.R")

aws.s3::get_bucket("tfaria", region = "")
data <- aws.s3::s3read_using(
  FUN = arrow::read_parquet,
  object = "public/RevisionDB.parquet",
  bucket = "tfaria",
  opts = list("region" = "")
)
RevisionDB <- preprocess_revision_db(data)

data <- aws.s3::s3read_using(
  FUN = arrow::read_csv_arrow,
  object = "public/RealTimeDatabase.csv",
  bucket = "tfaria",
  opts = list("region" = "")
)
setDT(data)
DatasetRaw <- preprocess_raw_db(data)

data <- aws.s3::s3read_using(
  FUN = arrow::read_parquet,
  object = "public/FinalValues.parquet",
  bucket = "tfaria",
  opts = list("region" = "")
)
setDT(data)
FinalValues <- preprocess_final_values_db(data)

data <- aws.s3::s3read_using(
  FUN = arrow::read_parquet,
  object = "public/GRateDB.parquet",
  bucket = "tfaria",
  opts = list("region" = "")
)
GRateDB <- preprocess_growth_rate_db(data)

DateRange <- seq(as.Date("2006-07-01"), as.Date("2019-10-01"), by = "quarter")
VintageList <- unique(GRateDB[, ECB_vintage])
date_to_vintage <- setNames(as.list(VintageList[1:length(as.list(DateRange))]), DateRange)
Countries <- unique(RevisionDB$Country_code)
Variables <- unique(RevisionDB$Variable_code)

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
  RevisionDB_cropped <- RevisionDB[(Country_code %in% country)]
  for (variable in Variables) {
    cat(country, variable, "\n")
    GRateDB_cropped <- GRateDB[(Country_code %in% country) & (Variable_code %in% variable)]
    for (obs_date in as.character(DateRange)) {
      obs_date <- as.Date(obs_date)

      FinalRevision <- RevisionDB_cropped[(Country_code %in% country) & (Variable_code %in% variable) &
        (Date %in% obs_date) & (Revision_nb == 1) &
        (Type_revision == "Final")][, Value]

      FirstAnnounGr <- GRateDB_cropped[(Country_code %in% country) & (Variable_code %in% variable) &
        (Date %in% obs_date) & (ECB_vintage %in% date_to_vintage[[as.character(obs_date)]])][, Value]

      past_revisions <- unlist(sapply(1:5, get_past_revisions,
        data = GRateDB_cropped,
        Country = country,
        Variable = variable,
        Obs_date = obs_date,
        date_to_vintage = date_to_vintage
      ))
      length(past_revisions) <- 5

      revisions_macro <- RevisionDB_cropped[(Country_code %in% country) &
        (Variable_code %in% c("YEN", "ITN", "EXN", "GCN", "WGS", "PCN")) &
        (Date %in% obs_date) & (Revision_nb == 1) &
        (Type_revision == "Final")][, .(Variable_code, Value)][
        ,
        Variable_code := factor(Variable_code, levels = c("YEN", "ITN", "EXN", "GCN", "WGS", "PCN"))
      ][
        order(Variable_code)
      ][, Value]

      new_line <- data.table(
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

      RegressionDB <- rbindlist(list(RegressionDB, new_line))
    }
  }
}

aws.s3::s3write_using(
  RegressionDB,
  FUN = arrow::write_parquet,
  object = "public/RegressionDB.parquet",
  bucket = "tfaria",
  opts = list("region" = "")
)

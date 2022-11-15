library(arrow)
library(dplyr)
library(xts)
library(stringr)
library(data.table)
source("R/functions.R")

data <- arrow::read_parquet("data/GRateDB.parquet")


VintageList <- unique(data[, ECB_vintage])
DateRange <- seq(as.Date("2006-07-01"), as.Date("2019-10-01"), by = "quarter")
Countries <- unique(data$Country_code)
Variables <- unique(data$Variable_code)

vintage_to_date <- setNames(as.list(DateRange), VintageList[1:length(as.list(DateRange))])

RevisionDB <- data.table()

for (vintage in names(vintage_to_date)) {
  i <- match(vintage, VintageList)

  for (country in Countries) {
    cat(vintage, "-", country, "\n")

    for (variable in Variables) {
      revs <- calcs_revisions(data, vintage, VintageList, country, variable)

      if (is.na(revs[[1]])) {
        revs[1:length(revs)] <- NA
      }

      # Add final revisions
      final_rev <- data.table(Vintage_base = names(revs), Value = revs)[ , Type_revision := "Final",
                              ][ , Vintage_comp := tail(names(revs),1),
                              ][ , Date := vintage_to_date[[VintageList[i]]],
                              ][ , Country_code := country,
                              ][ , Variable_code := variable,
                              ][ , Revision_nb := 1:length(revs),
                              ]
      
      # Add intermediate revisions
      interm_rev <- data.table(Vintage_comp = names(diff(revs)), Value = diff(revs))[ , Type_revision := "Intermediate",
                              ][ , Vintage_base := names(revs)[1:(length(revs)-1)],
                              ][ , Date := vintage_to_date[[VintageList[i]]],
                              ][ , Country_code := country,
                              ][ , Variable_code := variable,
                              ][ , Revision_nb := 1:(length(revs)-1),
                              ]

      RevisionDB <- rbindlist(list(RevisionDB, final_rev, interm_rev), use.names=TRUE)
    }
  }
}

arrow::write_parquet(x = RevisionDB, "data/RevisionDB.parquet")

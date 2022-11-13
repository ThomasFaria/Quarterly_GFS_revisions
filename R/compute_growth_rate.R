library(arrow)
library(dplyr)
library(xts)
library(tidyr)
library(stringr)
library(data.table)
source("R/functions.R")
#data <- arrow::read_csv_arrow("https://minio.lab.sspcloud.fr/tfaria/public/RealTimeDatabase.csv")
data <- arrow::read_csv_arrow("~/Documents/2021_4A_ENS_CESURE/ECB/qgfs_revisions/data/RealTimeDatabase.csv")
setDT(data)

VintageList <- unique(data[,ECB_vintage])
Final_vintages <- VintageList[startsWith(VintageList, "A")]
GRateDB <- data.table()

reference_years <- sapply(Final_vintages, get_reference_year, simplify = FALSE, USE.NAMES = TRUE)
for (final_vintage in c(Final_vintages)) {
  data[(ECB_vintage %in% final_vintage) & (Date %between% reference_years[[final_vintage]]), IsFinalValue := T]
} 

for (vintage in VintageList) {
  cat(vintage, "\n")

  xts_data <- data[ECB_vintage %in% vintage, c("Date", "Variable_code", "Country_code", "Value")] %>%
    dcast(Date ~ paste0(CountryCode, "_", VariableCode), value.var = "Value") %>%
    as.xts.data.table()

  xts_growth_rate <- (log(xts_data / stats::lag(xts_data, 4)) * 100)["1999-01-01/"] %>%
    as.data.table() %>%
    melt(measure.vars = patterns("(.)_(.)"), value.name = "Value")
  xts_growth_rate[, c("Country_code", "Variable_code") := tstrsplit(variable, "_", fixed = TRUE)
                  ][, "ECB_vintage" := vintage
                  ][, variable := NULL
                  ]
  GRateDB <- rbindlist(list(GRateDB, xts_growth_rate))
}

setnames(GRateDB, "index", "Date")
for (final_vintage in c(Final_vintages)) {
  GRateDB[(ECB_vintage %in% final_vintage) & (Date %between% reference_years[[final_vintage]]), IsFinalValue := T]
} 

FinalValues <- rbindlist(list(GRateDB[(IsFinalValue)][, Measure := "GRate"],
                              data[(IsFinalValue)][, Measure := "Raw"][, c("Date", "Variable_code", "Country_code", "Value", "Measure")]))

arrow::write_parquet(x = GRateDB, "data/GRateDB.parquet")

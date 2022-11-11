library(arrow)
library(dplyr)
library(xts)
library(tidyr)
library(stringr)
library(data.table)

data <- arrow::read_csv_arrow("https://minio.lab.sspcloud.fr/tfaria/public/RealTimeDatabase.csv")%>%
  as.data.table()

#data <- arrow::read_csv_arrow("~/Documents/2021_4A_ENS_CESURE/ECB/qgfs_revisions/RealTimeDatabase.csv")%>%
#as.data.table()

VintageList <- c(outer(c("W", "G", "S", "A"), str_pad(7:20, 2, pad = "0"), FUN=paste0))
GRateDB <- data.table()
vintage <- "W07"

for (vintage in VintageList) {
  cat(vintage, "\n")
  
  xts_data <- data[ECBVintage %in% vintage, c("Date", "VariableCode", "CountryCode", "Value")]%>%
    dcast(Date ~ paste0(CountryCode, "_", VariableCode), value.var = "Value")%>%
    as.xts.data.table()
  
  xts_growth_rate <- (log(xts_data / stats::lag(xts_data, 4)) * 100)["1999-01-01/"]%>% 
    as.data.table() %>%
    melt(measure.vars=patterns("(.)_(.)"), value.name = "Value")
  xts_growth_rate[, c("Country", "Variable") := tstrsplit(variable, "_", fixed=TRUE)
  ][, "ECB_vintage" := vintage
  ][,variable := NULL
  ]
  GRateDB <- rbindlist(list(GRateDB, xts_growth_rate))
  

}

setnames(GRateDB, "index", "Date")
arrow::write_csv_arrow(x = GRateDB, "data/GRateDB.csv")

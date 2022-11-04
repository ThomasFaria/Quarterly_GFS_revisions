library(arrow)
library(dplyr)
library(xts)
library(tidyr)
library(stringr)

data <- arrow::read_csv_arrow("https://minio.lab.sspcloud.fr/tfaria/public/RealTimeDatabase.csv")

data <- data%>%
  as_tibble()

VintageList <- c(outer(c("W", "G", "S", "A"), str_pad(7:20, 2, pad = "0"), FUN=paste0))

GRateDB <- tibble(Index = as.Date(NA),
                  Country = character(),
                  Variable = character(),
                  ECBVintage = character(),
                  Value = numeric()
                  )

for (vintage in VintageList) {
  cat(vintage, "\n")
  ts_cropped <- data %>% filter(ECBVintage %in% vintage)%>%
    select(Date, VariableCode, CountryCode, Value)%>%
    pivot_wider(names_from =  c(CountryCode, VariableCode), values_from = Value)%>%
    arrange(Date)
  
  ts_cropped <- xts(as.data.frame(ts_cropped[,-1]), order.by=as.Date(ts_cropped[,1]%>%pull()))
  
  ts_cropped_grate <- (log(ts_cropped / stats::lag(ts_cropped, 4)) * 100)["2000-01-01/"]%>% 
    fortify.zoo() %>%
    as_tibble() %>%
    pivot_longer(
      cols = !Index,
      names_to = c("Country", "Variable"),
      names_pattern = "(.*)_(.*)",
      values_to = "Value"
    )%>%
    mutate(ECBVintage = vintage)
  
  GRateDB <- bind_rows(GRateDB, ts_cropped_grate)

}

GRateDB <- GRateDB%>%
  rename(Date = Index)
arrow::write_csv_arrow(x = GRateDB, "data/GRateDB.csv")

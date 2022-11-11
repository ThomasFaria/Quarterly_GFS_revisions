library(arrow)
library(dplyr)
library(xts)
library(tidyr)
library(stringr)
library(data.table)

data <- arrow::read_parquet("data/GRateDB.parquet")

calcs_revisions <- function(dt, vintage, VintageList, country, variable){
  
  case <- substr(vintage, 1, 1)
  
  i <- match(vintage, VintageList)
  
  switch(case,
         G={
           revs <- dt[ (Country == country) &
                         (Variable == variable) &
                         (ECB_vintage %in% VintageList[i:(i+2)]) & 
                         (Date == as.POSIXlt.Date(vintage_to_date[[VintageList[i]]])) , Value]
           
           if (length(revs)!=length(i:(i+2))) {
             # Removing intermediate revisions when final revision doesn't exist
             revs <- setNames(rep(NA,length(i:(i+2))), VintageList[i:(i+2)])
           }else{
             # Compute the revisions by substracting the last value
             revs <- setNames(revs - tail(revs, 1), VintageList[i:(i+2)])
           }
           
         },
         
         W={
           
           revs <- dt[ (Country == country) &
                         (Variable == variable) &
                         (ECB_vintage %in% VintageList[i:(i+3)]) & 
                         (Date == as.POSIXlt.Date(vintage_to_date[[VintageList[i]]])) , Value]
           
           if (length(revs)!=length(i:(i+3))) {
             # Removing intermediate revisions when final revision doesn't exist
             revs <- setNames(rep(NA,length(i:(i+3))), VintageList[i:(i+3)])
           }else{
             # Compute the revisions by substracting the last value
             revs <- setNames(revs - tail(revs, 1), VintageList[i:(i+3)])
           }
           
         },
         
         A={
           revs <- dt[ (Country == country) &
                         (Variable == variable) &
                         (ECB_vintage %in% VintageList[i:(i+4)]) & 
                         (Date == as.POSIXlt.Date(vintage_to_date[[VintageList[i]]])) , Value]
           
           if (length(revs)!=length(i:(i+4))) {
             # Removing intermediate revisions when final revision doesn't exist
             revs <- setNames(rep(NA,length(i:(i+4))), VintageList[i:(i+4)])
           }else{
             # Compute the revisions by substracting the last value
             revs <- setNames(revs - tail(revs, 1), VintageList[i:(i+4)])
           }
           
         },
         
         S={
           revs <- dt[ (Country == country) &
                         (Variable == variable) &
                         (ECB_vintage %in% VintageList[i:(i+5)]) & 
                         (Date == as.POSIXlt.Date(vintage_to_date[[VintageList[i]]])) , Value]
           
           if (length(revs)!=length(i:(i+5))) {
             # Removing intermediate revisions when final revision doesn't exist
             revs <- setNames(rep(NA,length(i:(i+5))), VintageList[i:(i+5)])
           }else{
             # Compute the revisions by substracting the last value
             revs <- setNames(revs - tail(revs, 1), VintageList[i:(i+5)])
           }
           
         },
         
         stop("Unknown vintage abbreviation. It should start by G, W, A or S")
  )
  return(revs)
}

VintageList <- c(outer(c("W", "G", "S", "A"), str_pad(7:20, 2, pad = "0"), FUN=paste0))
DateRange <- seq(as.Date("2006-07-01"), as.Date("2019-10-01"), by = "quarter")
Countries <- unique(data$Country)
Variables <- unique(data$Variable)

vintage_to_date <- setNames(as.list(DateRange), VintageList[1:length(as.list(DateRange))])

RevisionDB <- data.table()

for (vintage in names(vintage_to_date)) {
  i <- match(vintage, VintageList)
  
  for (country in Countries) {
    cat(vintage,"-", country, "\n")
    
    for (variable in Variables) {
      
      revs <- calcs_revisions(data, vintage, VintageList, country, variable)

      if (is.na(revs[[1]])) {
        revs[1:length(revs)] <- NA
      }
      
      # Add final revisions
      final_rev <- data.table(VintageBase = names(revs), Value= revs)[ , TypeRevision := "Final",
                              ][ , VintageComp := tail(names(revs),1),
                              ][ , Date := vintage_to_date[[VintageList[i]]],
                              ][ , Country := country,
                              ][ , Variable := variable,
                              ][ , RevisionNb := 1:length(revs),
                              ]
      
      # Add intermediate revisions
      interm_rev <- data.table(VintageComp = names(diff(revs)), Value= diff(revs))[ , TypeRevision := "Intermediate",
                              ][ , VintageBase := names(revs)[1:(length(revs)-1)],
                              ][ , Date := vintage_to_date[[VintageList[i]]],
                              ][ , Country := country,
                              ][ , Variable := variable,
                              ][ , RevisionNb := 1:(length(revs)-1),
                              ]

      RevisionDB <- rbindlist(list(RevisionDB, final_rev, interm_rev), use.names=TRUE)
    }
  }
}

arrow::write_parquet(x = RevisionDB, "data/RevisionDB.parquet")

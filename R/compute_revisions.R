library(arrow)
library(dplyr)
library(xts)
library(tidyr)
library(stringr)

data <- arrow::read_csv_arrow("data/GRateDB.csv")
calcs_revisions <- function(data, vintage, VintageList, country, variable){
  
  case <- substr(vintage, 1, 1)
  
  i <- match(vintage, VintageList)
  
  switch(case,
         G={
           revs <- data%>%filter((Country == country) &
                                   (Variable == variable) &
                                   (ECBVintage %in% VintageList[i:(i+2)]) & 
                                   (Index == vintage_to_date[[VintageList[i]]]))%>%
             pull(Value)
           
           if (length(revs)==0) {
             revs <- setNames(rep(NA,length(i:(i+2))), VintageList[i:(i+2)])
           }else{
             revs <- setNames(revs - tail(revs, 1), VintageList[i:(i+2)])
           }
           
         },
         
         W={
           
           revs <- data%>%filter((Country == country) &
                                   (Variable == variable) &
                                   (ECBVintage %in% VintageList[i:(i+3)]) & 
                                   (Index == vintage_to_date[[VintageList[i]]]))%>%
             pull(Value)
           
           if (length(revs)==0) {
             revs <- setNames(rep(NA,length(i:(i+3))), VintageList[i:(i+3)])
           }else{
             revs <- setNames(revs - tail(revs, 1), VintageList[i:(i+3)])
           }
           
         },
         
         A={
           revs <- data%>%filter((Country == country) &
                                   (Variable == variable) &
                                   (ECBVintage %in% VintageList[i:(i+4)]) & 
                                   (Index == vintage_to_date[[VintageList[i]]]))%>%
             pull(Value)
           
           if (length(revs)==0) {
             revs <- setNames(rep(NA,length(i:(i+4))), VintageList[i:(i+4)])
           }else{
             revs <- setNames(revs - tail(revs, 1), VintageList[i:(i+4)])
           }
           
         },
         
         S={
           revs <- data%>%filter((Country == country) &
                                   (Variable == variable) &
                                   (ECBVintage %in% VintageList[i:(i+5)]) & 
                                   (Index == vintage_to_date[[VintageList[i]]]))%>%
             pull(Value)
           
           if (length(revs)==0) {
             revs <- setNames(rep(NA,length(i:(i+5))), VintageList[i:(i+5)])
           }else{
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

RevisionDB <- tibble(Date = as.Date(NA),
                     VintageBase = character(),
                     VintageComp = character(),
                     Country = character(),
                     Variable = character(),
                     TypeRevision = character(),
                     RevisionNb = numeric(),
                     Value = numeric()
)

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
      RevisionDB <- bind_rows(RevisionDB, 
                              revs%>%
                                tibble::enframe(name = "VintageBase", value = "Value")%>%
                                mutate(TypeRevision = "Final",
                                       VintageComp = tail(names(revs),1),
                                       Date = vintage_to_date[[VintageList[i]]],
                                       Country = country, 
                                       Variable = variable,
                                       RevisionNb = 1:length(revs)
                                )
                              )
      
      # Add intermediate revisions
      RevisionDB <- bind_rows(RevisionDB, 
                              diff(revs)%>%
                                tibble::enframe(name = "VintageComp", value = "Value")%>%
                                mutate(TypeRevision = "Intermediate",
                                       VintageBase = names(revs)[1:(length(revs)-1)],
                                       Date = vintage_to_date[[VintageList[i]]],
                                       Country = country, 
                                       Variable = variable,
                                       RevisionNb = 1:(length(revs)-1)
                                )
                              )
      
    }
  }
  
}

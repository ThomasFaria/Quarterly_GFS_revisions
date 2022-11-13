get_reference_year <- function(vintage){
  past_year <- 2000 + as.double(substr(vintage, start = 2, stop = 3)) - 1
  if (past_year == 2006) {
    reference_year <- c(paste0(past_year, "-07-01"), paste0(past_year, "-12-31"))
  }else{
    reference_year <- c(paste0(past_year, "-01-01"), paste0(past_year, "-12-31"))
  }
  return(reference_year)
}

calcs_revisions <- function(dt, vintage, VintageList, country, variable) {
  case <- substr(vintage, 1, 1)
  
  i <- match(vintage, VintageList)
  
  switch(case,
         G = {
           revs <- dt[(Country == country) &
                        (Variable == variable) &
                        (ECB_vintage %in% VintageList[i:(i + 2)]) &
                        (Date == as.POSIXlt.Date(vintage_to_date[[VintageList[i]]])), Value]
           
           if (length(revs) != length(i:(i + 2))) {
             # Removing intermediate revisions when final revision doesn't exist
             revs <- setNames(rep(NA, length(i:(i + 2))), VintageList[i:(i + 2)])
           } else {
             # Compute the revisions by substracting the last value
             revs <- setNames(revs - tail(revs, 1), VintageList[i:(i + 2)])
           }
         },
         W = {
           revs <- dt[(Country == country) &
                        (Variable == variable) &
                        (ECB_vintage %in% VintageList[i:(i + 3)]) &
                        (Date == as.POSIXlt.Date(vintage_to_date[[VintageList[i]]])), Value]
           
           if (length(revs) != length(i:(i + 3))) {
             # Removing intermediate revisions when final revision doesn't exist
             revs <- setNames(rep(NA, length(i:(i + 3))), VintageList[i:(i + 3)])
           } else {
             # Compute the revisions by substracting the last value
             revs <- setNames(revs - tail(revs, 1), VintageList[i:(i + 3)])
           }
         },
         A = {
           revs <- dt[(Country == country) &
                        (Variable == variable) &
                        (ECB_vintage %in% VintageList[i:(i + 4)]) &
                        (Date == as.POSIXlt.Date(vintage_to_date[[VintageList[i]]])), Value]
           
           if (length(revs) != length(i:(i + 4))) {
             # Removing intermediate revisions when final revision doesn't exist
             revs <- setNames(rep(NA, length(i:(i + 4))), VintageList[i:(i + 4)])
           } else {
             # Compute the revisions by substracting the last value
             revs <- setNames(revs - tail(revs, 1), VintageList[i:(i + 4)])
           }
         },
         S = {
           revs <- dt[(Country == country) &
                        (Variable == variable) &
                        (ECB_vintage %in% VintageList[i:(i + 5)]) &
                        (Date == as.POSIXlt.Date(vintage_to_date[[VintageList[i]]])), Value]
           
           if (length(revs) != length(i:(i + 5))) {
             # Removing intermediate revisions when final revision doesn't exist
             revs <- setNames(rep(NA, length(i:(i + 5))), VintageList[i:(i + 5)])
           } else {
             # Compute the revisions by substracting the last value
             revs <- setNames(revs - tail(revs, 1), VintageList[i:(i + 5)])
           }
         },
         stop("Unknown vintage abbreviation. It should start by G, W, A or S")
  )
  return(revs)
}

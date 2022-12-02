get_reference_year <- function(vintage) {
  past_year <- 2000 + as.double(substr(vintage, start = 2, stop = 3)) - 1
  if (past_year == 2006) {
    reference_year <- c(paste0(past_year, "-07-01"), paste0(past_year, "-12-31"))
  } else {
    reference_year <- c(paste0(past_year, "-01-01"), paste0(past_year, "-12-31"))
  }
  return(reference_year)
}

calcs_revisions <- function(dt, vintage, VintageList, country, variable) {
  case <- substr(vintage, 1, 1)

  i <- match(vintage, VintageList)

  switch(case,
    G = {
      revs <- dt[(Country_code == country) &
        (Variable_code == variable) &
        (ECB_vintage %in% VintageList[i:(i + 2)]) &
        (Date == vintage_to_date[[VintageList[i]]]), Value, ECB_vintage]
      
      revs <- revs[data.table(ECB_vintage = VintageList[i:(i + 2)]), on = .(ECB_vintage)]
      final_value <- revs[(ECB_vintage == tail(VintageList[i:(i + 2)], 1)), Value]
      
      if (is.na(final_value)) {
        # Removing intermediate revisions when final revision doesn't exist
        revs <- setNames(rep(NA, length(i:(i + 2))), VintageList[i:(i + 2)])
      } else {
        # Compute the revisions by substracting the last value
        revs <- setNames(final_value - revs[,Value], VintageList[i:(i + 2)])
      }
    },
    W = {
      revs <- dt[(Country_code == country) &
        (Variable_code == variable) &
        (ECB_vintage %in% VintageList[i:(i + 3)]) &
        (Date == vintage_to_date[[VintageList[i]]]), Value, ECB_vintage]
      
      revs <- revs[data.table(ECB_vintage = VintageList[i:(i + 3)]), on = .(ECB_vintage)]
      final_value <- revs[(ECB_vintage == tail(VintageList[i:(i + 3)], 1)), Value]
      
      if (is.na(final_value)) {
        # Removing intermediate revisions when final revision doesn't exist
        revs <- setNames(rep(NA, length(i:(i + 3))), VintageList[i:(i + 3)])
      } else {
        # Compute the revisions by substracting the last value
        revs <- setNames(final_value - revs[,Value], VintageList[i:(i + 3)])
      }
    },
    A = {
      revs <- dt[(Country_code == country) &
        (Variable_code == variable) &
        (ECB_vintage %in% VintageList[i:(i + 4)]) &
        (Date == vintage_to_date[[VintageList[i]]]), Value, ECB_vintage]
      
        revs <- revs[data.table(ECB_vintage = VintageList[i:(i + 4)]), on = .(ECB_vintage)]
        final_value <- revs[(ECB_vintage == tail(VintageList[i:(i + 4)], 1)), Value]
        
        if (is.na(final_value)) {
          # Removing intermediate revisions when final revision doesn't exist
          revs <- setNames(rep(NA, length(i:(i + 4))), VintageList[i:(i + 4)])
        } else {
          # Compute the revisions by substracting the last value
          revs <- setNames(final_value - revs[,Value], VintageList[i:(i + 4)])
        }
    },
    S = {
      revs <- dt[(Country_code == country) &
        (Variable_code == variable) &
        (ECB_vintage %in% VintageList[i:(i + 5)]) &
        (Date == vintage_to_date[[VintageList[i]]]), Value, ECB_vintage]

      revs <- revs[data.table(ECB_vintage = VintageList[i:(i + 5)]), on = .(ECB_vintage)]
      final_value <- revs[(ECB_vintage == tail(VintageList[i:(i + 5)], 1)), Value]
      
      if (is.na(final_value)) {
        # Removing intermediate revisions when final revision doesn't exist
        revs <- setNames(rep(NA, length(i:(i + 5))), VintageList[i:(i + 5)])
      } else {
        # Compute the revisions by substracting the last value
        revs <- setNames(final_value - revs[,Value], VintageList[i:(i + 5)])
      }
    },
    stop("Unknown vintage abbreviation. It should start by G, W, A or S")
  )
  return(revs)
}

get_past_revisions <- function(data, country, variable, obs_date, date_to_vintage, lag) {
  compared_vintages <- c(
    date_to_vintage[[as.character(obs_date %m-% months(3 * lag))]],
    date_to_vintage[[as.character(obs_date)]]
  )
  
  past_revision <- diff(data[(Country_code %in% country) &
                               (Variable_code %in% variable) &
                               (Date == obs_date %m-% months(3 * lag)) &
                               (ECB_vintage %in% compared_vintages)][, Value])
  
  return(past_revision)
}

preprocess_revision_db <- function(data) {
  data[
    , c("ObsQ", "ObsY", "Measure", "Revision_nb") := list(as.integer(substr(quarters(Date), 2, 2)), year(Date), "GRate", as.double(Revision_nb))
  ][, RevisionPlace := .(fcase(
    ObsQ == 1, Revision_nb,
    ObsQ == 2, Revision_nb + 1,
    ObsQ == 3, Revision_nb + 2,
    ObsQ == 4, Revision_nb + 3
  ))][, ReleaseDate := .(fcase(
    startsWith(Vintage_base, "W"), paste0(stringr::str_replace(Vintage_base, "W", "20"), "-01-01"),
    startsWith(Vintage_base, "G"), paste0(stringr::str_replace(Vintage_base, "G", "20"), "-04-01"),
    startsWith(Vintage_base, "S"), paste0(stringr::str_replace(Vintage_base, "S", "20"), "-07-01"),
    startsWith(Vintage_base, "A"), paste0(stringr::str_replace(Vintage_base, "A", "20"), "-10-01")
  ))][, Group := .(fcase(
    Variable_code %in% c("TOR", "DTX", "TIN", "SCT"), "Revenue",
    Variable_code %in% c("TOE", "THN", "PUR", "COE", "GIN"), "Expenditure",
    Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS"), "Macro",
    Variable_code %in% c("KTR", "OCR", "OCE", "OKE", "INP"), "Others"
  ))][
    ,
    Group2 := .(fcase(
      Variable_code %in% c("TOR", "DTX", "TIN", "SCT", "OCR", "KTR"), "Revenue",
      Variable_code %in% c("TOE", "THN", "PUR", "INP", "COE", "OCE", "GIN", "OKE"), "Expenditure",
      Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS"), "Macro"
    ))
  ][
    Variable_code %in% c("KTR", "OCR", "OCE", "OKE", "INP"),
    ToShade := "TRUE"
  ][
    ,
    Variable_long := .(fcase(
      Variable_code %in% c("TOR"), "Total revenue",
      Variable_code %in% c("TOR"), "Total revenue",
      Variable_code %in% c("DTX"), "Direct taxes",
      Variable_code %in% c("TIN"), "Indirect taxes",
      Variable_code %in% c("SCT"), "Social contributions",
      Variable_code %in% c("TOE"), "Total expenditure",
      Variable_code %in% c("THN"), "Social transfers",
      Variable_code %in% c("PUR"), "Purchases",
      Variable_code %in% c("COE"), "Gov. compensation",
      Variable_code %in% c("GIN"), "Gov. investment",
      Variable_code %in% c("YEN"), "GDP",
      Variable_code %in% c("PCN"), "Private consumption",
      Variable_code %in% c("ITN"), "Total investment",
      Variable_code %in% c("EXN"), "Exports",
      Variable_code %in% c("GCN"), "Gov. consumption",
      Variable_code %in% c("WGS"), "Wages and salaries",
      Variable_code %in% c("OCR"), "Other current revenue",
      Variable_code %in% c("KTR"), "Capital revenue",
      Variable_code %in% c("INP"), "Interest payments",
      Variable_code %in% c("OCE"), "Other current expenditure",
      Variable_code %in% c("OKE"), "Other capital expenditure"
    ))
  ]
  return(data)
}

preprocess_raw_db <- function(data) {
  data[
    , c("ObsQ", "ObsY") := list(as.integer(substr(quarters(Date), 2, 2)), year(Date))
  ][, ReleaseDate := .(fcase(
    startsWith(ECB_vintage, "W"), paste0(stringr::str_replace(ECB_vintage, "W", "20"), "-01-01"),
    startsWith(ECB_vintage, "G"), paste0(stringr::str_replace(ECB_vintage, "G", "20"), "-04-01"),
    startsWith(ECB_vintage, "S"), paste0(stringr::str_replace(ECB_vintage, "S", "20"), "-07-01"),
    startsWith(ECB_vintage, "A"), paste0(stringr::str_replace(ECB_vintage, "A", "20"), "-10-01")
  ))][, Group := .(fcase(
    Variable_code %in% c("TOR", "DTX", "TIN", "SCT"), "Revenue",
    Variable_code %in% c("TOE", "THN", "PUR", "COE", "GIN"), "Expenditure",
    Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS"), "Macro",
    Variable_code %in% c("KTR", "OCR", "OCE", "OKE", "INP"), "Others"
  ))][
    ,
    Group2 := .(fcase(
      Variable_code %in% c("TOR", "DTX", "TIN", "SCT", "OCR", "KTR"), "Revenue",
      Variable_code %in% c("TOE", "THN", "PUR", "INP", "COE", "OCE", "GIN", "OKE"), "Expenditure",
      Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS"), "Macro"
    ))
  ][
    Variable_code %in% c("KTR", "OCR", "OCE", "OKE", "INP"),
    ToShade := "TRUE"
  ][
    ,
    Variable_long := .(fcase(
      Variable_code %in% c("TOR"), "Total revenue",
      Variable_code %in% c("TOR"), "Total revenue",
      Variable_code %in% c("DTX"), "Direct taxes",
      Variable_code %in% c("TIN"), "Indirect taxes",
      Variable_code %in% c("SCT"), "Social contributions",
      Variable_code %in% c("TOE"), "Total expenditure",
      Variable_code %in% c("THN"), "Social transfers",
      Variable_code %in% c("PUR"), "Purchases",
      Variable_code %in% c("COE"), "Gov. compensation",
      Variable_code %in% c("GIN"), "Gov. investment",
      Variable_code %in% c("YEN"), "GDP",
      Variable_code %in% c("PCN"), "Private consumption",
      Variable_code %in% c("ITN"), "Total investment",
      Variable_code %in% c("EXN"), "Exports",
      Variable_code %in% c("GCN"), "Gov. consumption",
      Variable_code %in% c("WGS"), "Wages and salaries",
      Variable_code %in% c("OCR"), "Other current revenue",
      Variable_code %in% c("KTR"), "Capital revenue",
      Variable_code %in% c("INP"), "Interest payments",
      Variable_code %in% c("OCE"), "Other current expenditure",
      Variable_code %in% c("OKE"), "Other capital expenditure"
    ))
  ]
  return(data)
}

preprocess_final_values_db <- function(data) {
  data[
    ,
    Variable_long := .(fcase(
      Variable_code %in% c("TOR"), "Total revenue",
      Variable_code %in% c("TOR"), "Total revenue",
      Variable_code %in% c("DTX"), "Direct taxes",
      Variable_code %in% c("TIN"), "Indirect taxes",
      Variable_code %in% c("SCT"), "Social contributions",
      Variable_code %in% c("TOE"), "Total expenditure",
      Variable_code %in% c("THN"), "Social transfers",
      Variable_code %in% c("PUR"), "Purchases",
      Variable_code %in% c("COE"), "Gov. compensation",
      Variable_code %in% c("GIN"), "Gov. investment",
      Variable_code %in% c("YEN"), "GDP",
      Variable_code %in% c("PCN"), "Private consumption",
      Variable_code %in% c("ITN"), "Total investment",
      Variable_code %in% c("EXN"), "Exports",
      Variable_code %in% c("GCN"), "Gov. consumption",
      Variable_code %in% c("WGS"), "Wages and salaries",
      Variable_code %in% c("OCR"), "Other current revenue",
      Variable_code %in% c("KTR"), "Capital revenue",
      Variable_code %in% c("INP"), "Interest payments",
      Variable_code %in% c("OCE"), "Other current expenditure",
      Variable_code %in% c("OKE"), "Other capital expenditure"
    ))
  ]
  return(data)
}

preprocess_growth_rate_db <- function(data) {
  data[, Group := .(fcase(
    Variable_code %in% c("TOR", "DTX", "TIN", "SCT"), "Revenue",
    Variable_code %in% c("TOE", "THN", "PUR", "COE", "GIN"), "Expenditure",
    Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS"), "Macro",
    Variable_code %in% c("KTR", "OCR", "OCE", "OKE", "INP"), "Others"
  ))][
    ,
    Group2 := .(fcase(
      Variable_code %in% c("TOR", "DTX", "TIN", "SCT", "OCR", "KTR"), "Revenue",
      Variable_code %in% c("TOE", "THN", "PUR", "INP", "COE", "OCE", "GIN", "OKE"), "Expenditure",
      Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS"), "Macro"
    ))
  ][
    Variable_code %in% c("KTR", "OCR", "OCE", "OKE", "INP"),
    ToShade := "TRUE"
  ][
    ,
    Variable_long := .(fcase(
      Variable_code %in% c("TOR"), "Total revenue",
      Variable_code %in% c("TOR"), "Total revenue",
      Variable_code %in% c("DTX"), "Direct taxes",
      Variable_code %in% c("TIN"), "Indirect taxes",
      Variable_code %in% c("SCT"), "Social contributions",
      Variable_code %in% c("TOE"), "Total expenditure",
      Variable_code %in% c("THN"), "Social transfers",
      Variable_code %in% c("PUR"), "Purchases",
      Variable_code %in% c("COE"), "Gov. compensation",
      Variable_code %in% c("GIN"), "Gov. investment",
      Variable_code %in% c("YEN"), "GDP",
      Variable_code %in% c("PCN"), "Private consumption",
      Variable_code %in% c("ITN"), "Total investment",
      Variable_code %in% c("EXN"), "Exports",
      Variable_code %in% c("GCN"), "Gov. consumption",
      Variable_code %in% c("WGS"), "Wages and salaries",
      Variable_code %in% c("OCR"), "Other current revenue",
      Variable_code %in% c("KTR"), "Capital revenue",
      Variable_code %in% c("INP"), "Interest payments",
      Variable_code %in% c("OCE"), "Other current expenditure",
      Variable_code %in% c("OKE"), "Other capital expenditure"
    ))
  ]
  return(data)
}

preprocess_regression_db <- function(data) {
  data <- data[(Country_code %in% c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"))][
    ,
    Group := .(fcase(
      Variable_code %in% c("TOR", "DTX", "TIN", "SCT"), "Revenue",
      Variable_code %in% c("TOE", "THN", "PUR", "COE", "GIN"), "Expenditure",
      Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS"), "Macro",
      Variable_code %in% c("KTR", "OCR", "OCE", "OKE", "INP"), "Others"
    ))
  ][
    (Date > as.Date("2014-03-01")),
    ESA2010 := 1
  ][
    , ObsQ := as.integer(substr(quarters(Date), 2, 2))
  ] |>
    dcast(... ~ paste0("ObsQ_", ObsQ), fun = length) |>
    dcast(Country_code + ... ~ paste0("Country_", Country_code), fun = length)

  data <- data[
    (ObsQ_1 + ObsQ_2 + ObsQ_3 == 0),
    ObsQ_4 := 1
  ][
    (ObsQ_1 + ObsQ_2 + ObsQ_3 == 1),
    ObsQ_4 := 0
  ] |>
    na.omit(cols = c("Rev_lag1", "Rev_lag2", "Rev_lag3", "Rev_lag4", "Rev_lag5", "Rev_ITN", "Rev_EXN", "Rev_GCN", "Rev_YEN", "Rev_PCN", "Rev_WGS"))

  return(data)
}

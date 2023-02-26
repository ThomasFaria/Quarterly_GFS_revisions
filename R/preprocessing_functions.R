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
  ))][, Group3 := .(fcase(
    Group %in% c("Revenue", "Expenditure"), "Fiscal",
    Group %in% c("Macro"), "Macro",
    !(Group %in% c("Macro", "Revenue", "Expenditure")), "Others"
  ))][, IsREA := .(fcase(
    Country_code %in% c("EL", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"), 1,
    !(Country_code %in% c("EL", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT")), 0
  ))][, IsEA := .(fcase(
    Country_code %in% c("EA"), 1,
    !(Country_code %in% c("EA")), 0
  ))]
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
    ,
    ToShade := .(fcase(
      Variable_code %in% c("KTR", "OCR", "OCE", "OKE", "INP"), "TRUE",
      Variable_code %in% c("TOR", "DTX", "TIN", "SCT", "OCR", "KTR",
                           "TOE", "THN", "PUR", "INP", "COE", "OCE", "GIN", "OKE",
                           "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"), "FALSE"
    ))
  ][
    ,
    Variable_long := .(fcase(
      Variable_code == "WGS", "Comp. of employees",
      Variable_code != "WGS", Variable_long
    ))
  ]
  return(data)
}

preprocess_regression_db <- function(data) {
  data <- data[(Country_code %in% c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"))][
    ,
    ESA2010 := .(fcase(
      Date > as.Date("2014-03-01"), 1,
      Date <= as.Date("2014-03-01"), 0
    ))
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
  ][
    (Group != "Others")
  ] |>
    na.omit(cols = c("Rev_lag1", "Rev_lag2", "Rev_lag3", "Rev_lag4", "Rev_lag5", "Rev_ITN", "Rev_EXN", "Rev_GCN", "Rev_YEN", "Rev_PCN", "Rev_WGS"))

  return(data)
}

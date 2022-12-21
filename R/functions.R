library(styler)

get_reference_year <- function(vintage) {
  past_year <- 2000 + as.double(substr(vintage, start = 2, stop = 3)) - 1
  if (past_year == 2006) {
    reference_year <- c(paste0(past_year, "-07-01"), paste0(past_year, "-12-31"))
  } else {
    reference_year <- c(paste0(past_year, "-01-01"), paste0(past_year, "-12-31"))
  }
  return(reference_year)
}

calcs_revisions <- function(dt, index_vintage, VintageList, date, country, variable) {
  case <- substr(VintageList[[index_vintage]], 1, 1)

  switch(case,
    G = {
      revs <- dt[(Country_code == country) &
        (Variable_code == variable) &
        (ECB_vintage %in% VintageList[index_vintage:(index_vintage + 2)]) &
        (Date == date), Value, ECB_vintage]

      revs <- revs[data.table(ECB_vintage = VintageList[index_vintage:(index_vintage + 2)]), on = .(ECB_vintage)]
      final_value <- revs[(ECB_vintage == tail(VintageList[index_vintage:(index_vintage + 2)], 1)), Value]

      if (is.na(final_value)) {
        # Removing intermediate revisions when final revision doesn't exist
        revs <- setNames(rep(NA, length(index_vintage:(index_vintage + 2))), VintageList[index_vintage:(index_vintage + 2)])
      } else {
        # Compute the revisions by substracting the last value
        revs <- setNames(final_value - revs[, Value], VintageList[index_vintage:(index_vintage + 2)])
      }
    },
    W = {
      revs <- dt[(Country_code == country) &
        (Variable_code == variable) &
        (ECB_vintage %in% VintageList[index_vintage:(index_vintage + 3)]) &
        (Date == date), Value, ECB_vintage]

      revs <- revs[data.table(ECB_vintage = VintageList[index_vintage:(index_vintage + 3)]), on = .(ECB_vintage)]
      final_value <- revs[(ECB_vintage == tail(VintageList[index_vintage:(index_vintage + 3)], 1)), Value]

      if (is.na(final_value)) {
        # Removing intermediate revisions when final revision doesn't exist
        revs <- setNames(rep(NA, length(index_vintage:(index_vintage + 3))), VintageList[index_vintage:(index_vintage + 3)])
      } else {
        # Compute the revisions by substracting the last value
        revs <- setNames(final_value - revs[, Value], VintageList[index_vintage:(index_vintage + 3)])
      }
    },
    A = {
      revs <- dt[(Country_code == country) &
        (Variable_code == variable) &
        (ECB_vintage %in% VintageList[index_vintage:(index_vintage + 4)]) &
        (Date == date), Value, ECB_vintage]

      revs <- revs[data.table(ECB_vintage = VintageList[index_vintage:(index_vintage + 4)]), on = .(ECB_vintage)]
      final_value <- revs[(ECB_vintage == tail(VintageList[index_vintage:(index_vintage + 4)], 1)), Value]

      if (is.na(final_value)) {
        # Removing intermediate revisions when final revision doesn't exist
        revs <- setNames(rep(NA, length(index_vintage:(index_vintage + 4))), VintageList[index_vintage:(index_vintage + 4)])
      } else {
        # Compute the revisions by substracting the last value
        revs <- setNames(final_value - revs[, Value], VintageList[index_vintage:(index_vintage + 4)])
      }
    },
    S = {
      revs <- dt[(Country_code == country) &
        (Variable_code == variable) &
        (ECB_vintage %in% VintageList[index_vintage:(index_vintage + 5)]) &
        (Date == date), Value, ECB_vintage]

      revs <- revs[data.table(ECB_vintage = VintageList[index_vintage:(index_vintage + 5)]), on = .(ECB_vintage)]
      final_value <- revs[(ECB_vintage == tail(VintageList[index_vintage:(index_vintage + 5)], 1)), Value]

      if (is.na(final_value)) {
        # Removing intermediate revisions when final revision doesn't exist
        revs <- setNames(rep(NA, length(index_vintage:(index_vintage + 5))), VintageList[index_vintage:(index_vintage + 5)])
      } else {
        # Compute the revisions by substracting the last value
        revs <- setNames(final_value - revs[, Value], VintageList[index_vintage:(index_vintage + 5)])
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
  ][, Group3 := .(fcase(
    Group %in% c("Revenue", "Expenditure"), "Fiscal",
    Group %in% c("Macro"), "Macro",
    !(Group %in% c("Macro", "Revenue", "Expenditure")), "Others"
  ))][, IsREA := .(fcase(
    Country_code %in% c("GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"), 1,
    !(Country_code %in% c("GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT")), 0
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
    ,
    Variable_long := .(fcase(
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
      Variable_code %in% c("WGS"), "Wages and salaries"
    ))
  ][
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

get_list_models <- function(countries) {
  Regressors <- c(
    paste0("Country_", countries, collapse = "+"),
    paste0("ObsQ_", 1:4, collapse = "+"),
    "ESA2010",
    "First_announcement",
    paste0("Rev_lag", 1:5)
  )

  List_models <- unlist(lapply(
    1:length(Regressors),
    function(n) {
      combn(Regressors, n, FUN = function(row) {
        paste0("Final_revision ~ ", paste0(row, collapse = "+"))
      })
    }
  ))

  return(List_models)
}

get_intermediate_models <- function(old_models, interm_model_number) {
  model_str <- stringr::str_replace_all(
    old_models, c("\\+Rev_lag1" = "", "\\+Rev_lag2" = "", "\\+Rev_lag3" = "", "\\+Rev_lag4" = "", "\\+Rev_lag5" = "")
  ) |>
    stringr::str_replace_all(
      c("Rev_lag1" = "0", "Rev_lag2" = "0", "Rev_lag3" = "0", "Rev_lag4" = "0", "Rev_lag5" = "0")
    )

  if (interm_model_number == 1) {
    return(model_str)
  }

  model_str <- stringr::str_replace_all(
    model_str, c("\\+First_announcement" = "")
  ) |>
    stringr::str_replace_all(
      c("First_announcement" = "0")
    )

  if (interm_model_number == 2) {
    return(model_str)
  }

  model_str <- stringr::str_replace_all(
    model_str, c("\\+ESA2010" = "")
  ) |>
    stringr::str_replace_all(
      c("ESA2010" = "0")
    )
  if (interm_model_number == 3) {
    return(model_str)
  }

  model_str <- stringr::str_replace_all(
    model_str, c("\\+ObsQ_1" = "", "\\+ObsQ_2" = "", "\\+ObsQ_3" = "", "\\+ObsQ_4" = "")
  ) |>
    stringr::str_replace_all(
      c("ObsQ_1" = "0")
    )

  if (interm_model_number == 4) {
    return(model_str)
  }

  model_str <- stringr::str_replace_all(
    model_str, c("\\+Country_DE" = "", "\\+Country_ES" = "", "\\+Country_FR" = "", "\\+Country_IT" = "", "\\+Country_NL" = "", "\\+Country_BE" = "", "\\+Country_AT" = "", "\\+Country_FI" = "", "\\+Country_PT" = "", "\\+Country_REA" = "")
  ) |>
    stringr::str_replace_all(
      c("Country_DE" = "0")
    )
  return(model_str)
}

simulate_models <- function(data, variable, list_models, include_naive, all_models) {
  sample <- data[(Variable_code == variable)]

  if (!all_models) {
    list_models <- list_models[variable]
  }

  tryCatch(
    expr = {
      # We estimate a model without constant
      model_summary <- rbindlist(lapply(list_models, function(model) {
        estimated_model <- lm(paste(model, "-1"), data = sample)
        summary <- broom::glance(estimated_model)
        summary$Model_specification <- model
        summary$N <- nobs(estimated_model)
        summary$Variable <- variable
        summary$Variable_long <- unique(sample$Variable_long)
        summary$RMSE <- sqrt(c(crossprod(estimated_model$residuals)) / length(estimated_model$residuals))
        summary$Group <- unique(sample$Group)
        return(summary)
      }), use.names = TRUE)

      if (include_naive) {
        estimated_model <- lm("Final_revision ~ 0", data = sample)
        naive_summary <- data.table(
          AIC = AIC(estimated_model),
          BIC = BIC(estimated_model),
          N = nobs(estimated_model),
          Model_specification = "Final_revision ~ 0",
          Variable = variable,
          Variable_long = unique(sample$Variable_long),
          p.value = NA,
          RMSE = c(sqrt(crossprod(estimated_model$residuals) / length(estimated_model$residuals))),
          Group = unique(sample$Group)
        )
        model_summary <- rbindlist(list(model_summary, naive_summary), use.names = TRUE, fill = TRUE)
      }

      return(model_summary)
    },
    error = function(e) {
      # When there is no regressor we have to add the constant
      estimated_model <- lm(list_models, data = sample)
      model_summary <- data.table(
        AIC = AIC(estimated_model),
        BIC = BIC(estimated_model),
        N = nobs(estimated_model),
        Model_specification = list_models,
        Variable = variable,
        Variable_long = unique(sample$Variable_long),
        p.value = NA,
        RMSE = c(sqrt(crossprod(estimated_model$residuals) / length(estimated_model$residuals))),
        Group = unique(sample$Group)
      )
      return(model_summary)
    }
  )
}

get_best_model <- function(models, variable, criterion) {
  best_model <- models[[variable]][
    ,
    c("Variable", "Variable_long", "Group", "Model_specification", "RMSE", "AIC", "BIC", "p.value", "N")
  ][
    ,
    Criterion := criterion
  ] |>
    setorderv(criterion) |>
    head(1)
  return(best_model)
}

run_regression <- function(data, list_models, variables) {
  # Run complete model
  model_results <- sapply(variables, simulate_models, data = data, list_models = list_models, include_naive = TRUE, simplify = FALSE, all_models = TRUE)
  top_models <- rbindlist(lapply(c("AIC", "BIC"), function(criterion) {
    rbindlist(lapply(variables, get_best_model, models = model_results, criterion = criterion))
  }))

  # Run intermediate model
  for (interm_model in 1:5) {
    for (criterion in c("AIC", "BIC")) {
      new_models <- get_intermediate_models(top_models[Criterion == criterion, Model_specification], interm_model)
      names(new_models) <- variables
      results_interm_models <- rbindlist(sapply(variables, simulate_models, data = data, list_models = new_models, include_naive = FALSE, simplify = FALSE, all_models = FALSE), fill = TRUE)
      top_models[Criterion == criterion, paste0("RMSE_interm_", interm_model) := results_interm_models$RMSE]
    }
  }
  # Return only best models according to AIC and BIC
  return(top_models)
}

get_regression_table <- function(data, criterion, variables) {
  table <- data[Criterion == criterion][,
    c("Compl/Naive", "Intrm1/Naive", "Intrm2/Naive", "Intrm3/Naive", "Intrm4/Naive") := lapply(.SD, function(rmse) round(rmse / get("RMSE_interm_5"), 2)),
    .SDcols = c("RMSE", "RMSE_interm_1", "RMSE_interm_2", "RMSE_interm_3", "RMSE_interm_4")
  ][
    ,
    Variable := factor(Variable, levels = variables)
  ][
    ,
    c("F-value", "Expl. variable") := list(round(p.value, 2), Model_specification)
  ][
    ,
    .(Variable_long, `Expl. variable`, N, `F-value`, `Compl/Naive`, `Intrm1/Naive`, `Intrm2/Naive`, `Intrm3/Naive`, `Intrm4/Naive`)
  ][, ][
    ,
    `Expl. variable` := stringr::str_replace_all(get("Expl. variable"), c(
      "Final_revision ~ " = "",
      "First_announcement" = "x1",
      "ev_lag" = "_",
      "ObsQ_1\\+ObsQ_2\\+ObsQ_3\\+ObsQ_4" = "Q",
      "Country_DE\\+Country_ES\\+Country_FR\\+Country_IT\\+Country_NL\\+Country_BE\\+Country_AT\\+Country_FI\\+Country_PT\\+Country_REA" = "Country"
    ))
  ]
  
  table <- rename_to_latex(table)
  return(table)
}

produce_regressions <- function(data, variables, criterion) {
  RegressionDB <- preprocess_regression_db(data)
  List_models <- get_list_models(c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"))
  results <- run_regression(RegressionDB, List_models, variables)
  return(results)
}

compute_growth_rate <- function(data) {
  GRateDB <- data.table()
  vintage_list <- unique(data[, ECB_vintage])
  for (vintage in vintage_list) {
    xts_data <- data[ECB_vintage %in% vintage, c("Date", "Variable_code", "Country_code", "Value")] |>
      dcast(Date ~ paste0(Country_code, "_", Variable_code), value.var = "Value") |>
      as.xts.data.table()

    xts_growth_rate <- (log(xts_data / stats::lag(xts_data, 4)) * 100)["1999-01-01/"] |>
      as.data.table() |>
      melt(measure.vars = patterns("(.)_(.)"), value.name = "Value")
    xts_growth_rate[, c("Country_code", "Variable_code") := tstrsplit(variable, "_", fixed = TRUE)][, "ECB_vintage" := vintage][, variable := NULL]
    GRateDB <- rbindlist(list(GRateDB, xts_growth_rate))
  }
  setnames(GRateDB, "index", "Date")

  return(GRateDB)
}

get_final_values <- function(raw_data, growth_rate_data) {
  vintage_list <- unique(raw_data[, ECB_vintage])
  Final_vintages <- vintage_list[startsWith(vintage_list, "A")]
  reference_years <- sapply(Final_vintages, get_reference_year, simplify = FALSE, USE.NAMES = TRUE)

  for (final_vintage in c(Final_vintages)) {
    growth_rate_data[(ECB_vintage %in% final_vintage) & (Date %between% reference_years[[final_vintage]]), Is_final_value := T]
    raw_data[(ECB_vintage %in% final_vintage) & (Date %between% reference_years[[final_vintage]]), Is_final_value := T]
  }

  FinalValues <- rbindlist(list(
    growth_rate_data[(Is_final_value)][, Measure := "GRate"],
    raw_data[(Is_final_value)][, Measure := "Raw"][, c("Date", "Value", "Country_code", "Variable_code", "ECB_vintage", "Is_final_value", "Measure")]
  ))

  return(FinalValues)
}

get_RTDB <- function(file) {
  return(data.table(arrow::read_csv_arrow(file)))
}

create_regression_db <- function(raw_data, revision_data, gr_data, final_values) {
  DatasetRaw <- preprocess_raw_db(raw_data)
  RevisionDB <- preprocess_revision_db(revision_data)
  FinalValues <- preprocess_final_values_db(final_values)
  GRateDB <- preprocess_growth_rate_db(gr_data)
  DateRange <- seq(as.Date("2006-07-01"), as.Date("2019-10-01"), by = "quarter")

  VintageList <- unique(GRateDB[, ECB_vintage])
  date_to_vintage <- setNames(as.list(VintageList[1:length(as.list(DateRange))]), DateRange)
  Countries <- unique(RevisionDB$Country_code)
  Variables <- unique(RevisionDB$Variable_code)

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
          country = country,
          variable = variable,
          obs_date = obs_date,
          date_to_vintage = date_to_vintage
        ))

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

  return(RegressionDB)
}

compute_revisions <- function(data) {
  VintageList <- unique(data[, ECB_vintage])
  Countries <- unique(data$Country_code)
  Variables <- unique(data$Variable_code)
  DateRange <- seq(as.Date("2006-07-01"), as.Date("2019-10-01"), by = "quarter")

  vintage_to_date <- setNames(as.list(DateRange), VintageList[1:length(as.list(DateRange))])

  RevisionDB <- data.table()
  for (vintage in names(vintage_to_date)) {
    index_vintage <- match(vintage, VintageList)
    date <- vintage_to_date[[VintageList[index_vintage]]]

    for (country in Countries) {
      cat(vintage, "-", country, "\n")

      for (variable in Variables) {
        revs <- calcs_revisions(data, index_vintage, VintageList, date, country, variable)

        if (is.na(revs[[1]])) {
          revs[1:length(revs)] <- NA
        }

        # Add final revisions
        final_rev <- data.table(Vintage_base = names(revs), Value = revs)[ , Type_revision := "Final",
        ][ , Vintage_comp := tail(names(revs),1),
        ][ , Date := date,
        ][ , Country_code := country,
        ][ , Variable_code := variable,
        ][ , Revision_nb := 1:length(revs),
        ]
        
        # Add intermediate revisions
        interm_rev <- data.table(Vintage_comp = names(diff(revs)), Value = diff(revs) * -1)[ , Type_revision := "Intermediate",
        ][ , Vintage_base := names(revs)[1:(length(revs)-1)],
        ][ , Date := date,
        ][ , Country_code := country,
        ][ , Variable_code := variable,
        ][ , Revision_nb := 1:(length(revs)-1),
        ]
        
        RevisionDB <- rbindlist(list(RevisionDB, final_rev, interm_rev), use.names = TRUE)
      }
    }
  }

  return(RevisionDB)
}

rename_to_latex <- function(table) {
  
  table[, `Expl. variable` := stringr::str_replace_all(`Expl. variable`, 
                                                       c("ESA2010" = "\\\\mathds{1}_{\\\\left[t\\\\geq2014Q2\\\\right]}",
                                                         "Country" = "\\\\sum\\\\limits_{m=1}^{10}C_{m}",
                                                         "\\bQ\\b" = "\\\\sum\\\\limits_{j=1}^{4}Q_{t}^{j}",
                                                         "x1" = "x_{t,m}^{1}",
                                                         "R_1\\+R_2\\+R_3\\+R_4" = "\\\\sum\\\\limits_{i=1}^{4}R_{t-i,m}",
                                                         "R_2\\+R_3\\+R_4\\+R_5" = "\\\\sum\\\\limits_{i=2}^{5}R_{t-i,m}",
                                                         "R_1" = "R_{t-1,m}",
                                                         "R_2" = "R_{t-2,m}",
                                                         "R_3" = "R_{t-3,m}",
                                                         "R_4" = "R_{t-4,m}",
                                                         "R_5" = "R_{t-5,m}",
                                                         "^0" = "")
  )
  ][,
    `Expl. variable` := paste0("$", `Expl. variable`, "$")
  ]
  
  return(table)
  
}

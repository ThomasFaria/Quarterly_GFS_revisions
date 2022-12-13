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
        revs <- setNames(final_value - revs[, Value], VintageList[i:(i + 2)])
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
        revs <- setNames(final_value - revs[, Value], VintageList[i:(i + 3)])
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
        revs <- setNames(final_value - revs[, Value], VintageList[i:(i + 4)])
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
        revs <- setNames(final_value - revs[, Value], VintageList[i:(i + 5)])
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

get_regression_table <- function(data, criterion) {
  table <- data[Criterion == criterion][,
    c("Compl/Naive", "Intrm1/Naive", "Intrm2/Naive", "Intrm3/Naive", "Intrm4/Naive") := lapply(.SD, function(rmse) round(rmse / get("RMSE_interm_5"), 2)),
    .SDcols = c("RMSE", "RMSE_interm_1", "RMSE_interm_2", "RMSE_interm_3", "RMSE_interm_4")
  ][
    ,
    Variable := factor(Variable, levels = Variables)
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
  return(table)
}

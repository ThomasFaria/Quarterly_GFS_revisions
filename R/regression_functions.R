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

produce_regressions <- function(data, variables, criterion) {
  RegressionDB <- preprocess_regression_db(data)
  List_models <- get_list_models(c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"))
  results <- run_regression(RegressionDB, List_models, variables)
  return(results)
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

rename_to_latex <- function(table) {
  table[, `Expl. variable` := stringr::str_replace_all(
    `Expl. variable`,
    c(
      "ESA2010" = "\\\\mathds{1}_{\\\\left[t\\\\geq2014Q2\\\\right]}",
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
      "^0" = ""
    )
  )][
    ,
    `Expl. variable` := paste0("$", `Expl. variable`, "$")
  ]

  return(table)
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

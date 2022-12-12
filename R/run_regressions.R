library(data.table)
source("R/functions.R")

aws.s3::get_bucket("tfaria", region = "")
data <- aws.s3::s3read_using(
  FUN = arrow::read_parquet,
  object = "public/RegressionDB.parquet",
  bucket = "tfaria",
  opts = list("region" = "")
)
data <- arrow::read_parquet("data/RegressionDB.parquet")
RegressionDB <- preprocess_regression_db(data)

##### Lists #####
Var_Revenue <- c("TOR", "DTX", "SCT", "TIN")
Var_Expenditure <- c("TOE", "THN", "PUR", "COE", "GIN")
Var_Macro <- c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS")

Variables <- c(Var_Revenue, Var_Macro, Var_Expenditure)
unique(RegressionDB$Variable_code)
unique(RegressionDB$Country_code)

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
        p.value = NA,
        RMSE = c(sqrt(crossprod(estimated_model$residuals) / length(estimated_model$residuals))),
        Group = unique(sample$Group)
      )
      return(model_summary)
    }
  )
}

# Faire des fonctions pour simplifier
# finir le renommage des variables
##### MODEL 8  #####
Regressors <- c(
  paste0("Country_", c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"), collapse = "+"),
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

model_results <- sapply(Variables, simulate_models, data = RegressionDB, list_models = List_models, include_naive = TRUE, simplify = FALSE, all_models = TRUE)
top_models <- rbindlist(lapply(c("AIC", "BIC"), function(criterion) {
  rbindlist(lapply(Variables, get_best_model, models = model_results, criterion = criterion))
}))

top_models[Criterion == criterion, Model_specification]

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

for (interm_model in 1:5) {
  new_models <- get_intermediate_models(top_models[, Model_specification], interm_model)
  names(new_models) <- rep(Variables, 2)
  results_interm_models <- rbindlist(sapply(rep(Variables, 2), simulate_models, data = RegressionDB, list_models = new_models, include_naive = FALSE, simplify = FALSE, all_models = FALSE), fill = TRUE)
  top_models[, paste0("RMSE_interm_", interm_model) := results_interm_models$RMSE]
}


dplyr::mutate(
  Group = factor(Group, levels = c("Revenue", "Expenditure", "Macro"))
) |>
  dplyr::mutate_if(is.numeric, round, digits = 2) |>
  dplyr::arrange(Group) |>
  dplyr::mutate(Group = as.character(Group)) |>
  dplyr::select("Variable", "Group", "N", "Model_specification", "p.value", "RMSE8", "RMSE6", "RMSE5", "RMSE4", "RMSE3")


# model_resultss[, Criterion := rep(c("AIC", "BIC"), each=length(Variables))]

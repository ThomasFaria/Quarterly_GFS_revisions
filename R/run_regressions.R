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

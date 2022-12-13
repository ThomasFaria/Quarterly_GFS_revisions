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

Var_Revenue <- c("TOR", "DTX", "TIN", "SCT")
Var_Expenditure <- c("TOE", "THN", "PUR", "COE", "GIN")
Var_Macro <- c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS")
Variables <- c(Var_Revenue, Var_Expenditure, Var_Macro)

List_models <- get_list_models(c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"))
results <- run_regression(RegressionDB, List_models, Variables)
get_regression_table(results, "BIC")[]

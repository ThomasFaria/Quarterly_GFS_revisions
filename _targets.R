library(targets)

tar_option_set(
  packages = c("data.table"),
  format = "parquet",
  memory = "transient", 
  garbage_collection = TRUE
)
# Run the R scripts in the R/ folder with your custom functions:
tar_source("R/functions.R")

list(
  tar_target(
    name = file,
    command = "data/RealTimeDatabase.csv",
    format = "file"
  ),  
  tar_target(
    name = RTDB,
    command = get_RTDB(file),
  ),
  tar_target(
    name = GRateDB,
    command = compute_growth_rate(RTDB),
  ),
  tar_target(
    name = Final_values,
    command = get_final_values(RTDB, GRateDB),
  ),
  tar_target(
    name = data,
    command = arrow::read_parquet("data/RegressionDB.parquet"),
  ),
  tar_target(
    name = Regressions,
    command = produce_regressions(data, c(c("TOR", "DTX", "TIN", "SCT"), c("TOE", "THN", "PUR", "COE", "GIN"), c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS")))
  ),
  tar_target(
    name = table_AIC,
    command = get_regression_table(Regressions, "AIC", c(c("TOR", "DTX", "TIN", "SCT"), c("TOE", "THN", "PUR", "COE", "GIN"), c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS")))
  )
)

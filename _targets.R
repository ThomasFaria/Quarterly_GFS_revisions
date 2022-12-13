library(targets)

tar_option_set(
  packages = c("data.table"),
  format = "rds"
)
# Run the R scripts in the R/ folder with your custom functions:
tar_source("R/functions.R")

list(
  tar_target(
    name = data,
    command = arrow::read_parquet("data/RegressionDB.parquet")
  ),
  tar_target(
    name = Regressions,
    command = produce_regressions(data, c(c("TOR", "DTX", "TIN", "SCT"), c("TOE", "THN", "PUR", "COE", "GIN"), c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS")))
  )
)

tar_make()
tar_visnetwork(targets_only = T)

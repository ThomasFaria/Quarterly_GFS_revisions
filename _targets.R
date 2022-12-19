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
    name = input,
    command = "data/RealTimeDatabase.csv",
    format = "file"
  ),  
  tar_target(
    name = RTDB,
    command = data.table(arrow::read_csv_arrow(input)),
    format = "file"
  ),
  tar_target(
    name = GRateDB,
    command = compute_growth_rate(RTDB),
    format = "file"
  ),
  tar_target(
    name = Final_values,
    command = get_final_values(RTDB, GRateDB),
    format = "file"
  ),
  tar_target(
    name = data,
    command = arrow::read_parquet("data/RegressionDB.parquet"),
    format = "file"
  ),
  tar_target(
    name = Regressions,
    command = produce_regressions(data, c(c("TOR", "DTX", "TIN", "SCT"), c("TOE", "THN", "PUR", "COE", "GIN"), c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS")))
  )
)

# tar_make()
# tar_visnetwork(targets_only = T)

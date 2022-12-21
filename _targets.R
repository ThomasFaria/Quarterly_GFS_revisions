library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("data.table", "lubridate", "rmarkdown", "ggplot2"),
  format = "parquet",
  memory = "transient", 
  garbage_collection = TRUE
)
# Run the R scripts in the R/ folder with your custom functions:
tar_source("R/functions.R")
tar_source("R/plot_functions.R")

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
    name = RevisionDB,
    command = compute_revisions(GRateDB),
  ),
  tar_target(
    name = RegressionDB,
    command = create_regression_db(RTDB, RevisionDB, GRateDB, Final_values),
  ),
  tar_target(
    name = Regressions,
    command = produce_regressions(RegressionDB, c(c("TOR", "DTX", "TIN", "SCT"), c("TOE", "THN", "PUR", "COE", "GIN"), c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS")))
  ),
  tar_target(
    name = table_AIC,
    command = get_regression_table(Regressions, "AIC", c(c("TOR", "DTX", "TIN", "SCT"), c("TOE", "THN", "PUR", "COE", "GIN"), c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS")))
  ),
  tar_target(
    name = plot_total_revision,
    command = Plot_TOTAL_REVISION(RevisionDB, Final_values, c("NL"), c("COE"), "GRate"),
    format = "rds"
  ),
  tar_target(
    name = plot_revisions_across_variables,
    command = Plot_all_revisions(Data_all_revisions(RevisionDB, 
                                                    c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"), 
                                                    "Final", 1,"GRate")),
    format = "rds"
  ),
  tar_target(
    name = plot_revisions_across_countries,
    command = Plot_revisions_across_countries(Data_revisions_across_countries(RevisionDB, 
                                                                              c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
                                                                              c("TOR", "TOE", "YEN"),
                                                                              "Final", 1, "GRate")),
    format = "rds"
  ),
  tar_target(
    name = plot_revisions_across_time,
    command = Plot_revisions_across_time(Data_revisions_across_time(RevisionDB, 
                                                                    c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"), 
                                                                    c("YEN", "TOE", "TOR"), 
                                                                    "Final", 1, "GRate")),
    format = "rds"
  ),
  tar_target(
    name = plot_statistics_full_sample,
    command = Plot_STATISTICS(RevisionDB, Final_values, 
                              c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"), 
                              c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"), 
                              "Final", "GRate", c(1), 
                              as.Date("2020-01-01"), as.Date("2006-01-01"), scales_final_rev),
    format = "rds"
  ),
  tar_target(
    name = plot_statistics_pre2014,
    command = Plot_STATISTICS(RevisionDB, Final_values, 
                              c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"), 
                              c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"), 
                              "Final", "GRate", c(1), 
                              as.Date("2013-12-31"), as.Date("2006-01-01"), scales_final_rev),
    format = "rds"
  ),
  tar_target(
    name = plot_statistics_post2014,
    command = Plot_STATISTICS(RevisionDB, Final_values, 
                              c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"), 
                              c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"), 
                              "Final", "GRate", c(1), 
                              as.Date("2020-01-01"), as.Date("2013-12-31"), scales_final_rev),
    format = "rds"
  ),
  tar_target(
    name = plot_statistics_interm_pre2014,
    command = Plot_STATISTICS(RevisionDB, Final_values, 
                              c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"), 
                              c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"), 
                              "Intermediate", "GRate", c(1), 
                              as.Date("2013-12-31"), as.Date("2006-01-01"), scales_interm_rev),
    format = "rds"
  ),
  tar_target(
    name = plot_statistics_interm_post2014,
    command = Plot_STATISTICS(RevisionDB, Final_values, 
                              c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"), 
                              c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"), 
                              "Intermediate", "GRate", c(1), 
                              as.Date("2020-01-01"), as.Date("2013-12-31"), scales_interm_rev),
    format = "rds"
  ),
  tar_target(
    name = plot_single_rev_example,
    command = Plot_SingleVSTotal(Data_SingleVSTotal(RevisionDB, c("FR", "AT", "FI"), "SCT", "GRate", "Final",
                                                    as.Date("2016-07-02"), as.Date("2016-06-30"))),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_paths,
    command = Plot_PATHS(Data_PATHS(RevisionDB,
                                    c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT"), 
                                    c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"), 
                                    "Final", "GRate", as.Date("2020-01-01"), as.Date("2006-01-01"))),
    format = "rds"
  )
)

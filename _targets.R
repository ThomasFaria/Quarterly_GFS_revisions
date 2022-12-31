library(targets)

tar_option_set(
  packages = c("tarchetypes", "data.table", "xts", "lubridate", "rmarkdown", "ggplot2"),
  format = "parquet",
  memory = "transient",
  garbage_collection = TRUE
)

tar_source(files = "R")
options(timeout = 5*60)

list(
  tarchetypes::tar_download(
    name = file,
    urls = c("https://minio.lab.sspcloud.fr/tfaria/public/RealTimeDatabase.csv"),
    paths = "data/RealTimeDatabase.csv"
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
    command = create_regression_db(RevisionDB, GRateDB),
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
    command = Plot_all_revisions(Data_all_revisions(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revisions_across_countries,
    command = Plot_revisions_across_countries(Data_revisions_across_countries(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      c("TOR", "TOE", "YEN"),
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revisions_across_time,
    command = Plot_revisions_across_time(Data_revisions_across_time(
      RevisionDB,
      c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"),
      c("YEN", "TOE", "TOR"),
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_statistics_full_sample,
    command = Plot_STATISTICS(
      RevisionDB, Final_values,
      c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"),
      c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"),
      "Final", "GRate", c(1),
      as.Date("2020-01-01"), as.Date("2006-01-01"), scales_final_rev
    ),
    format = "rds"
  ),
  tar_target(
    name = plot_statistics_pre2014,
    command = Plot_STATISTICS(
      RevisionDB, Final_values,
      c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"),
      c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"),
      "Final", "GRate", c(1),
      as.Date("2014-03-01"), as.Date("2006-01-01"), scales_final_rev
    ),
    format = "rds"
  ),
  tar_target(
    name = plot_statistics_post2014,
    command = Plot_STATISTICS(
      RevisionDB, Final_values,
      c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"),
      c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"),
      "Final", "GRate", c(1),
      as.Date("2020-01-01"), as.Date("2014-03-01"), scales_final_rev
    ),
    format = "rds"
  ),
  tar_target(
    name = plot_statistics_interm_pre2014,
    command = Plot_STATISTICS(
      RevisionDB, Final_values,
      c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"),
      c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"),
      "Intermediate", "GRate", 1:5,
      as.Date("2014-03-01"), as.Date("2006-01-01"), scales_interm_rev
    ),
    format = "rds"
  ),
  tar_target(
    name = plot_statistics_interm_post2014,
    command = Plot_STATISTICS(
      RevisionDB, Final_values,
      c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"),
      c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"),
      "Intermediate", "GRate", 1:5,
      as.Date("2020-01-01"), as.Date("2014-03-01"), scales_interm_rev
    ),
    format = "rds"
  ),
  tar_target(
    name = plot_single_rev_example,
    command = Plot_SingleVSTotal(Data_SingleVSTotal(
      RevisionDB, c("FR", "AT", "FI"), "SCT", "GRate", "Final",
      as.Date("2016-07-02"), as.Date("2016-06-30")
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_paths,
    command = Plot_PATHS(Data_PATHS(
      RevisionDB,
      c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT"),
      c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"),
      "Final", "GRate", as.Date("2020-01-01"), as.Date("2006-01-01")
    )),
    format = "rds"
  ),
  # APPENDIX
  tar_target(
    name = plot_revision_TOR,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "TOR",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_DTX,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "DTX",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_TIN,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "TIN",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_SCT,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "SCT",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_OCR,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "OCR",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_KTR,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "KTR",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_TOE,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "TOE",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_THN,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "THN",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_PUR,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "PUR",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_INP,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "INP",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_COE,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "COE",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_OCE,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "OCE",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_GIN,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "GIN",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_OKE,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "OKE",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_YEN,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "YEN",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_PCN,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "PCN",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_ITN,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "ITN",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_EXN,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "EXN",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_GCN,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "GCN",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_WGS,
    command = Plot_revisions_per_variable(Data_revisions_per_variable(
      RevisionDB,
      c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "REA", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
      "WGS",
      "Final", 1, "GRate"
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_share_GDP,
    command = Plot_Share_GDP(Data_Share_GDP(RTDB, 
                                            c("DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"), 
                                            c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS", "KTR", "OCR", "OCE", "OKE", "INP"),
                                            "S21")
    ),
    format = "rds"
  ),
  tar_target(
    name = plot_mean_sd,
    command = Plot_Mean_SD(Data_Mean_SD(GRateDB, 
                                        c("DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT"),
                                        c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS", "KTR", "OCR", "OCE", "OKE", "INP"),
                                        "S21", "GRate", as.Date("2019-12-31"), as.Date("2006-01-01")), scales_Mean_SD),
    format = "rds"
  ),
  tar_target(
    name = plot_share_TOE,
    command = Plot_Ranking(Data_Ranking(RTDB, "TOE", "S21", 2019)),
    format = "rds"
  ),
  tar_target(
    name = table_BIC,
    command = get_regression_table(Regressions, "BIC", c(c("TOR", "DTX", "TIN", "SCT"), c("TOE", "THN", "PUR", "COE", "GIN"), c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS")))
  ),
  tar_target(
    name = plot_revision_paths_pre2014,
    command = Plot_PATHS(Data_PATHS(
      RevisionDB,
      c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT"),
      c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"),
      "Final", "GRate", as.Date("2014-03-01"), as.Date("2006-01-01")
    )),
    format = "rds"
  ),
  tar_target(
    name = plot_revision_paths_post2014,
    command = Plot_PATHS(Data_PATHS(
      RevisionDB,
      c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT"),
      c("TOR", "DTX", "TIN", "SCT", "TOE", "THN", "PUR", "COE", "GIN", "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"),
      "Final", "GRate", as.Date("2020-01-01"), as.Date("2014-03-01")
    )),
    format = "rds"
  )
)

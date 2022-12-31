get_RTDB <- function(file) {
  data <- data.table(arrow::read_csv_arrow(file))
  return(preprocess_raw_db(data))
}

compute_growth_rate <- function(data) {
  GRateDB <- data.table()
  vintage_list <- unique(data[, ECB_vintage])
  for (vintage in vintage_list) {
    xts_data <- data[ECB_vintage %in% vintage, c("Date", "Variable_code", "Variable_long", "Country_code", "Value")] |>
      dcast(Date ~ paste0(Country_code, "_", Variable_code), value.var = "Value") |>
      as.xts.data.table()

    xts_growth_rate <- (log(xts_data / stats::lag(xts_data, 4)) * 100)["1999-01-01/"] |>
      as.data.table() |>
      melt(measure.vars = patterns("(.)_(.)"), value.name = "Value")
    xts_growth_rate[, c("Country_code", "Variable_code") := tstrsplit(variable, "_", fixed = TRUE)][, "ECB_vintage" := vintage][, variable := NULL]
    GRateDB <- rbindlist(list(GRateDB, xts_growth_rate))
  }
  setnames(GRateDB, "index", "Date")
  
  join_table <- data[(ECB_vintage == vintage_list[1]) & (Country_code == "DE") & (Date == "1999-01-01"), 
                     .(Variable_code, Variable_long, Group, Group2, ToShade)]
  
  GRateDB <- merge(GRateDB, join_table, by = "Variable_code")
  
  return(GRateDB)
}

get_reference_year <- function(vintage) {
  past_year <- 2000 + as.double(substr(vintage, start = 2, stop = 3)) - 1
  if (past_year == 2006) {
    reference_year <- c(paste0(past_year, "-07-01"), paste0(past_year, "-12-31"))
  } else {
    reference_year <- c(paste0(past_year, "-01-01"), paste0(past_year, "-12-31"))
  }
  return(reference_year)
}

get_final_values <- function(raw_data, growth_rate_data) {
  vintage_list <- unique(raw_data[, ECB_vintage])
  Final_vintages <- vintage_list[startsWith(vintage_list, "A")]
  reference_years <- sapply(Final_vintages, get_reference_year, simplify = FALSE, USE.NAMES = TRUE)

  for (final_vintage in c(Final_vintages)) {
    growth_rate_data[(ECB_vintage %in% final_vintage) & (Date %between% reference_years[[final_vintage]]), Is_final_value := T]
    raw_data[(ECB_vintage %in% final_vintage) & (Date %between% reference_years[[final_vintage]]), Is_final_value := T]
  }

  FinalValues <- rbindlist(list(
    growth_rate_data[(Is_final_value)][, Measure := "GRate"],
    raw_data[(Is_final_value)][, Measure := "Raw"][, c("Date", "Value", "Country_code", "Variable_code", "Variable_long", "ECB_vintage", "Group", "Group2", "ToShade", "Is_final_value", "Measure")]
  ), fill = TRUE)

  return(FinalValues)
}

calcs_revisions <- function(dt, index_vintage, VintageList, date, country, variable) {
  case <- substr(VintageList[[index_vintage]], 1, 1)

  switch(case,
    G = {
      revs <- dt[(Country_code == country) &
        (Variable_code == variable) &
        (ECB_vintage %in% VintageList[index_vintage:(index_vintage + 2)]) &
        (Date == date), Value, ECB_vintage]

      revs <- revs[data.table(ECB_vintage = VintageList[index_vintage:(index_vintage + 2)]), on = .(ECB_vintage)]
      final_value <- revs[(ECB_vintage == tail(VintageList[index_vintage:(index_vintage + 2)], 1)), Value]

      if (is.na(final_value)) {
        # Removing intermediate revisions when final revision doesn't exist
        revs <- setNames(rep(NA, length(index_vintage:(index_vintage + 2))), VintageList[index_vintage:(index_vintage + 2)])
      } else {
        # Compute the revisions by substracting the last value
        revs <- setNames(final_value - revs[, Value], VintageList[index_vintage:(index_vintage + 2)])
      }
    },
    W = {
      revs <- dt[(Country_code == country) &
        (Variable_code == variable) &
        (ECB_vintage %in% VintageList[index_vintage:(index_vintage + 3)]) &
        (Date == date), Value, ECB_vintage]

      revs <- revs[data.table(ECB_vintage = VintageList[index_vintage:(index_vintage + 3)]), on = .(ECB_vintage)]
      final_value <- revs[(ECB_vintage == tail(VintageList[index_vintage:(index_vintage + 3)], 1)), Value]

      if (is.na(final_value)) {
        # Removing intermediate revisions when final revision doesn't exist
        revs <- setNames(rep(NA, length(index_vintage:(index_vintage + 3))), VintageList[index_vintage:(index_vintage + 3)])
      } else {
        # Compute the revisions by substracting the last value
        revs <- setNames(final_value - revs[, Value], VintageList[index_vintage:(index_vintage + 3)])
      }
    },
    A = {
      revs <- dt[(Country_code == country) &
        (Variable_code == variable) &
        (ECB_vintage %in% VintageList[index_vintage:(index_vintage + 4)]) &
        (Date == date), Value, ECB_vintage]

      revs <- revs[data.table(ECB_vintage = VintageList[index_vintage:(index_vintage + 4)]), on = .(ECB_vintage)]
      final_value <- revs[(ECB_vintage == tail(VintageList[index_vintage:(index_vintage + 4)], 1)), Value]

      if (is.na(final_value)) {
        # Removing intermediate revisions when final revision doesn't exist
        revs <- setNames(rep(NA, length(index_vintage:(index_vintage + 4))), VintageList[index_vintage:(index_vintage + 4)])
      } else {
        # Compute the revisions by substracting the last value
        revs <- setNames(final_value - revs[, Value], VintageList[index_vintage:(index_vintage + 4)])
      }
    },
    S = {
      revs <- dt[(Country_code == country) &
        (Variable_code == variable) &
        (ECB_vintage %in% VintageList[index_vintage:(index_vintage + 5)]) &
        (Date == date), Value, ECB_vintage]

      revs <- revs[data.table(ECB_vintage = VintageList[index_vintage:(index_vintage + 5)]), on = .(ECB_vintage)]
      final_value <- revs[(ECB_vintage == tail(VintageList[index_vintage:(index_vintage + 5)], 1)), Value]

      if (is.na(final_value)) {
        # Removing intermediate revisions when final revision doesn't exist
        revs <- setNames(rep(NA, length(index_vintage:(index_vintage + 5))), VintageList[index_vintage:(index_vintage + 5)])
      } else {
        # Compute the revisions by substracting the last value
        revs <- setNames(final_value - revs[, Value], VintageList[index_vintage:(index_vintage + 5)])
      }
    },
    stop("Unknown vintage abbreviation. It should start by G, W, A or S")
  )
  return(revs)
}

compute_revisions <- function(data) {
  VintageList <- unique(data[, ECB_vintage])
  Countries <- unique(data$Country_code)
  Variables <- unique(data$Variable_code)
  DateRange <- seq(as.Date("2006-07-01"), as.Date("2019-10-01"), by = "quarter")

  vintage_to_date <- setNames(as.list(DateRange), VintageList[1:length(as.list(DateRange))])

  RevisionDB <- data.table()
  for (vintage in names(vintage_to_date)) {
    index_vintage <- match(vintage, VintageList)
    date <- vintage_to_date[[VintageList[index_vintage]]]

    for (country in Countries) {

      for (variable in Variables) {
        revs <- calcs_revisions(data, index_vintage, VintageList, date, country, variable)

        if (is.na(revs[[1]])) {
          revs[1:length(revs)] <- NA
        }

        # Add final revisions
        final_rev <- data.table(Vintage_base = names(revs), Value = revs)[, Type_revision := "Final", ][, Vintage_comp := tail(names(revs), 1), ][, Date := date, ][, Country_code := country, ][, Variable_code := variable, ][, Revision_nb := 1:length(revs), ]

        # Add intermediate revisions
        interm_rev <- data.table(Vintage_comp = names(diff(revs)), Value = diff(revs) * -1)[, Type_revision := "Intermediate", ][, Vintage_base := names(revs)[1:(length(revs) - 1)], ][, Date := date, ][, Country_code := country, ][, Variable_code := variable, ][, Revision_nb := 1:(length(revs) - 1), ]

        RevisionDB <- rbindlist(list(RevisionDB, final_rev, interm_rev), use.names = TRUE)
      }
    }
  }

  join_table <- data[(ECB_vintage == names(vintage_to_date)[1]) & (Country_code == "DE") & (Date == "1999-01-01"), 
                        .(Variable_code, Variable_long, Group, Group2, ToShade)]
  
  RevisionDB <- merge(RevisionDB, join_table, by = "Variable_code")
  
  return(preprocess_revision_db(RevisionDB))
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

create_regression_db <- function(revision_data, gr_data) {
  DateRange <- seq(as.Date("2006-07-01"), as.Date("2019-10-01"), by = "quarter")

  VintageList <- unique(gr_data[, ECB_vintage])
  date_to_vintage <- setNames(as.list(VintageList[1:length(as.list(DateRange))]), DateRange)
  Countries <- unique(revision_data$Country_code)
  Variables <- unique(revision_data$Variable_code)

  RegressionDB <- data.table()
  for (country in Countries) {
    revision_data_cropped <- revision_data[(Country_code %in% country)]
    for (variable in Variables) {
      gr_data_cropped <- gr_data[(Country_code %in% country) & (Variable_code %in% variable)]
      for (obs_date in as.character(DateRange)) {
        obs_date <- as.Date(obs_date)

        FinalRevision <- revision_data_cropped[(Country_code %in% country) & (Variable_code %in% variable) &
          (Date %in% obs_date) & (Revision_nb == 1) &
          (Type_revision == "Final")][, Value]

        FirstAnnounGr <- gr_data_cropped[(Country_code %in% country) & (Variable_code %in% variable) &
          (Date %in% obs_date) & (ECB_vintage %in% date_to_vintage[[as.character(obs_date)]])][, Value]

        past_revisions <- unlist(sapply(1:5, get_past_revisions,
          data = gr_data_cropped,
          country = country,
          variable = variable,
          obs_date = obs_date,
          date_to_vintage = date_to_vintage
        ))

        revisions_macro <- revision_data_cropped[(Country_code %in% country) &
          (Variable_code %in% c("YEN", "ITN", "EXN", "GCN", "WGS", "PCN")) &
          (Date %in% obs_date) & (Revision_nb == 1) &
          (Type_revision == "Final")][, .(Variable_code, Value)][
          ,
          Variable_code := factor(Variable_code, levels = c("YEN", "ITN", "EXN", "GCN", "WGS", "PCN"))
        ][
          order(Variable_code)
        ][, Value]

        new_line <- data.table(
          Date = obs_date,
          Country_code = country,
          Variable_code = variable,
          Final_revision = FinalRevision,
          First_announcement = FirstAnnounGr,
          Rev_lag1 = past_revisions[1],
          Rev_lag2 = past_revisions[2],
          Rev_lag3 = past_revisions[3],
          Rev_lag4 = past_revisions[4],
          Rev_lag5 = past_revisions[5],
          Rev_YEN = revisions_macro[1],
          Rev_ITN = revisions_macro[2],
          Rev_EXN = revisions_macro[3],
          Rev_GCN = revisions_macro[4],
          Rev_WGS = revisions_macro[5],
          Rev_PCN = revisions_macro[6]
        )

        RegressionDB <- rbindlist(list(RegressionDB, new_line))
      }
    }
  }
  
  join_table <- gr_data[(ECB_vintage == VintageList[1]) & (Country_code == "DE") & (Date == "1999-01-01"), 
                     .(Variable_code, Variable_long, Group, Group2, ToShade)]
  
  RegressionDB <- merge(RegressionDB, join_table, by = "Variable_code")
  
  return(preprocess_regression_db(RegressionDB))
}

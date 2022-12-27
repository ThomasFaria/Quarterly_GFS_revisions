Data_TOTAL_REV <- function(data_rev, data_final, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber) {
  sample <- rbindlist(list(
    data_rev[(Country_code %in% Countries) &
      (Variable_code %in% Items) &
      (Type_revision %in% TypeOfRevision) &
      (Revision_nb %in% RevisionNumber) &
      (Measure == MeasureUsed)][, Type := "Revision"][, .(Date, Value, Type)],
    data_final[(Country_code %in% Countries) &
      (Variable_code %in% Items) &
      (Measure == MeasureUsed)][, Type := "Data"][, .(Date, Value, Type)]
  ))

  return(sample)
}

Plot_TOTAL_REV <- function(sample, Legend) {
  plot <- ggplot() +
    ggtitle("") +
    # makes the bar and format
    geom_bar(data = sample[Type == "Revision"], aes(x = Date, y = Value, fill = "#003299"), stat = "identity") +
    geom_line(data = sample[Type == "Data"], aes(x = Date, y = Value, color = "black")) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    # set general theme
    theme_ECB() +
    theme(plot.margin = unit(c(0.15, 0.3, 0.15, 0.15), "cm")) +

    # set colors and name of data
    scale_fill_manual("", values = "#003299", labels = "Final revision") +
    scale_color_manual("", values = "black", labels = "Realised data") +
    {
      if (!Legend) {
        theme(legend.position = "none")
      }
    }
  return(plot)
}

Data_TOTAL_REV_DECOMP <- function(data_rev, data_final, Countries, Items, TypeOfRevision, MeasureUsed) {
  sample <- rbindlist(list(
    data_rev[(Country_code %in% Countries) &
      (Variable_code %in% Items) &
      (Type_revision %in% TypeOfRevision) &
      (Measure == MeasureUsed)][, c("Type", "Revision_nb") := list("Revision", as.character(Revision_nb))][, .(Date, Value, Revision_nb, Type)],
    data_final[(Country_code %in% Countries) &
      (Variable_code %in% Items) &
      (Measure == MeasureUsed)][, c("Type", "Revision_nb") := list("Data", NA)][, .(Date, Value, Revision_nb, Type)]
  ))

  return(sample)
}

Plot_TOTAL_REV_DECOMP <- function(sample, Legend) {
  ###### Defining factors
  sample[, Revision_nb := factor(Revision_nb, levels = paste0(1:5))]

  SumData <- sample[(Type == "Revision"), Sum := sum(Value), by = Date][
    ,
    FinalRev := "Final revision"
  ]

  ##### Plotting
  plot <- ggplot() +
    ggtitle("") +

    # makes the bar and format
    geom_bar(data = sample[(Type == "Revision")], aes(x = Date, y = Value, fill = Revision_nb), stat = "identity") +
    geom_point(data = SumData, aes(x = Date, y = Sum, color = FinalRev), shape = "-", size = 6, stroke = 7) +
    geom_line(data = sample[(Type == "Data")], aes(x = Date, y = Value, linetype = Type)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    # set general theme
    theme_ECB() +
    theme(
      legend.title = element_text(size = 10),
      legend.box = "vertical",
      legend.margin = margin(),
      legend.direction = "horizontal"
    ) +
    # set colors and name of data
    scale_fill_manual("", values = ECB_col, labels = c("1st", "2nd", "3rd", "4th", "5th")) +
    scale_color_manual("", values = "black", labels = c("")) +
    scale_linetype_manual("", values = "solid", labels = c("")) +
    guides(
      fill = guide_legend(title = "Interm. revision:", nrow = 2, byrow = T),
      color = guide_legend(title = "Final revision:"),
      linetype = guide_legend(title = "Realised data:")
    ) +
    {
      if (!Legend) {
        theme(legend.position = "none")
      }
    }
  return(plot)
}

Plot_TOTAL_REVISION <- function(data_rev, data_final, Countries, Items, MeasureUsed) {
  sample_rev <- Data_TOTAL_REV(data_rev, data_final, Countries, Items, "Final", MeasureUsed, 1)
  sample_rev_decomp <- Data_TOTAL_REV_DECOMP(data_rev, data_final, Countries, Items, "Intermediate", MeasureUsed)
  Plot1 <- Plot_TOTAL_REV(sample_rev, F)
  Plot2 <- Plot_TOTAL_REV_DECOMP(sample_rev_decomp, F)

  legend1 <- cowplot::get_legend(
    Plot_TOTAL_REV(sample_rev, T) + theme(legend.box.margin = margin(0, 0, 0, 0))
  )

  legend2 <- cowplot::get_legend(
    Plot_TOTAL_REV_DECOMP(sample_rev_decomp, T) + theme(legend.box.margin = margin(0, 0, 0, 0))
  )

  legends <- cowplot::plot_grid(legend1,
    legend2,
    ncol = 2, vjust = -0.8
  )

  prow <- cowplot::plot_grid(Plot1 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
    Plot2 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
    ncol = 2, vjust = -0.8
  )

  plot <- cowplot::plot_grid(legends + theme(plot.margin = unit(c(1.4, 0, -0.1, 0), "cm")),
    prow + theme(plot.margin = unit(c(1.2, 0.2, 0, 0), "cm")),
    ncol = 1, rel_heights = c(0.1, 1)
  )

  return(plot)
}

Data_all_revisions <- function(data, Countries, TypeOfRevision, RevisionNb, MeasureUsed) {
  sample <- data[
    (Country_code %in% Countries) &
      (Type_revision %in% TypeOfRevision) &
      (Revision_nb == RevisionNb) &
      (Measure == MeasureUsed)
  ][
    ,
    Country_code := factor(Country_code, levels = c(setdiff(Countries, "EA"), "EA"))
  ][
    ,
    .(Date, Variable_long, Variable_code, Country_code, Group, Value)
  ]
  return(sample)
}

Subplot_all_revision <- function(sample, variable, x_lab, grey_bg, Legend) {
  temp <- sample[(Variable_code %in% variable),
    .(N = sum(!is.na(Value))),
    by = Variable_long
  ]

  NewTitle <- paste0(temp$Variable_long, "\n(", temp$N, ")")
  names(NewTitle) <- temp$Variable_long

  plot <- ggplot(data = sample[(Variable_code %in% variable)]) +
    ggtitle("") +
    {
      if (grey_bg) {
        geom_rect(aes(fill = Group),
          xmin = -Inf, xmax = Inf,
          ymin = -Inf, ymax = Inf, alpha = 0.3, fill = rgb(230, 230, 230, maxColorValue = 255)
        )
      }
    } +
    geom_point(aes(y = Date, x = Value, color = Country_code, alpha = Country_code), size = 1, shape = 16) +
    theme_ECB() +
    scale_color_manual(values = c(rep(ECB_col[1], 19), ECB_col[2])) +
    scale_alpha_manual(values = c(seq(19, 1, -1) / 19, 1)) +
    scale_x_continuous(limits = c(-650, 550), breaks = seq(-600, 500, 300)) +
    ylab(NewTitle) +
    {
      if (!Legend) {
        theme(legend.position = "none")
      }
    } +
    {
      if (!x_lab) {
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(color = rgb(217, 217, 217, maxColorValue = 255))
        )
      }
    } +
    theme(
      # axis.title.y = element_text(size = 10, face = "bold"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.spacing = unit(0, "cm"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = NA),
      panel.ontop = TRUE
    )

  return(plot)
}

Plot_all_revisions <- function(sample) {
  Plot1 <- Subplot_all_revision(sample, "TOR", F, F, F)
  Plot2 <- Subplot_all_revision(sample, "DTX", F, F, F)
  Plot3 <- Subplot_all_revision(sample, "TIN", F, F, F)
  Plot4 <- Subplot_all_revision(sample, "SCT", F, F, F)
  Plot5 <- Subplot_all_revision(sample, "OCR", F, T, F)
  Plot6 <- Subplot_all_revision(sample, "KTR", F, T, F)
  Plot7 <- Subplot_all_revision(sample, "TOE", F, F, F)
  Plot8 <- Subplot_all_revision(sample, "THN", F, F, F)
  Plot9 <- Subplot_all_revision(sample, "PUR", F, F, F)
  Plot10 <- Subplot_all_revision(sample, "INP", F, T, F)
  Plot11 <- Subplot_all_revision(sample, "COE", F, F, F)
  Plot12 <- Subplot_all_revision(sample, "OCE", F, T, F)
  Plot13 <- Subplot_all_revision(sample, "GIN", F, F, F)
  Plot14 <- Subplot_all_revision(sample, "OKE", F, T, F)
  Plot15 <- Subplot_all_revision(sample, "YEN", F, F, F)
  Plot16 <- Subplot_all_revision(sample, "PCN", F, F, F)
  Plot17 <- Subplot_all_revision(sample, "ITN", F, F, F)
  Plot18 <- Subplot_all_revision(sample, "EXN", F, F, F)
  Plot19 <- Subplot_all_revision(sample, "GCN", F, F, F)
  Plot20 <- Subplot_all_revision(sample, "WGS", T, F, F)

  legend <- cowplot::get_legend(
    Subplot_all_revision(sample, "WGS", T, F, T) + theme(legend.box.margin = margin(-15, 0, 0, 0))
  )

  prow <- cowplot::plot_grid(Plot1 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot2 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot3 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot4 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot5 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot6 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot7 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot8 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot9 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot10 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot11 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot12 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot13 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot14 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot15 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot16 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot17 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot18 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot19 + theme(plot.margin = unit(c(-0.74, 0, 0.24, 0), "cm")),
    Plot20 + theme(plot.margin = unit(c(-0.74, 0, 0, 0), "cm")),
    align = "v", ncol = 1, vjust = -0.8
  )

  plot <- cowplot::plot_grid(legend,
    prow + theme(plot.margin = unit(c(-0.3, 0.1, 0.1, 0.1), "cm")),
    ncol = 1, rel_heights = c(0.1, 1)
  )

  return(plot)
}

Data_revisions_across_countries <- function(data, Countries, Items, TypeOfRevision, RevisionNb, MeasureUsed) {
  sample <- data[
    (Country_code %in% Countries) &
      (Variable_code %in% Items) &
      (Type_revision %in% TypeOfRevision) &
      (Revision_nb == RevisionNb) &
      (Measure == MeasureUsed)
  ][
    ,
    Country_code := factor(Country_code, levels = Countries)
  ][
    ,
    .(Date, Variable_long, Variable_code, Country_code, Group, IsREA, Value)
  ]


  temp <- sample[, .(N = sum(!is.na(Value))), by = Country_code][
    ,
    Country_code := factor(Country_code, levels = Countries)
  ][order(Country_code)][
    ,
    N := paste0("NULL[(~", N, ")]")
  ]

  sample <- merge(sample, temp, by = "Country_code")
  sample[, Variable_long := factor(Variable_long, levels = c("Total revenue", "Total expenditure", "GDP"))]

  return(sample)
}

Plot_revisions_across_countries <- function(sample) {
  plot <- ggplot() +
    ggtitle("") +
    geom_rect(
      data = subset(sample, IsREA == 1), aes(fill = as.factor(IsREA)), xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = Inf, alpha = 0.3
    ) +
    geom_point(data = sample, aes(x = Date, y = Value, color = Variable_long), size = 1, shape = 16, alpha = 0.75) +
    coord_flip() +
    facet_wrap(. ~ Country_code + N, ncol = 1, strip.position = "left", labeller = label_parsed) +
    theme_ECB() +
    scale_color_manual(values = ECB_col) +
    scale_fill_manual(values = rgb(230, 230, 230, maxColorValue = 255), guide = "none") +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.spacing = unit(0, "cm"),
      strip.placement = "outside",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = NA),
      panel.ontop = TRUE
    )

  return(plot)
}

Data_revisions_across_time <- function(data, Countries, Items, TypeOfRevision, RevisionNb, MeasureUsed) {
  sample <- data[
    (Country_code %in% Countries) &
      (Variable_code %in% Items) &
      (Type_revision %in% TypeOfRevision) &
      (Revision_nb == RevisionNb) &
      (Measure == MeasureUsed)
  ][
    ,
    Country_code := factor(Country_code, levels = Countries)
  ][
    ,
    .(Date, ObsY, Variable_long, Variable_code, Group, Value)
  ]

  sample[, Variable_long := factor(Variable_long, levels = c("Total revenue", "Total expenditure", "GDP"))]
  return(sample)
}

Plot_revisions_across_time <- function(sample) {
  Bound <- sample[, .(
    q5 = quantile(Value, c(.05), na.rm = TRUE),
    q95 = quantile(Value, c(.95), na.rm = TRUE)
  ),
  by = .(Variable_long, ObsY)
  ][, ObsY := as.Date(paste0(ObsY, "-07-01"))]

  plot <- ggplot() +
    ggtitle("") +
    geom_ribbon(data = Bound, aes(xmin = q5, xmax = q95, y = ObsY, fill = Variable_long), alpha = 0.3) +
    geom_point(data = sample, aes(x = Value, y = Date, color = Variable_long), size = 1, shape = 16, alpha = 0.75) +
    theme_ECB() +
    scale_color_manual(values = ECB_col) +
    scale_fill_manual(values = ECB_col, guide = "none") +
    scale_y_date(date_breaks = "2 years", date_labels = "%Y") +
    theme(
      panel.spacing = unit(0, "cm"),
      strip.placement = "outside",
      panel.background = element_rect(fill = NA),
      panel.ontop = TRUE
    )

  return(plot)
}

Data_STATISTICS <- function(sample, Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate) {
  Final_values <- Finalvalues[
    (Country_code %in% Countries) &
      (Variable_code %in% Items) &
      (Measure == MeasureUsed) &
      (Date %between% c(LowDate, UpDate)),
    .(SDF = sd(Value, na.rm = TRUE)),
    by = Variable_long
  ]

  sample <- sample[
    (Country_code %in% Countries) &
      (Variable_code %in% Items) &
      (Measure == MeasureUsed) &
      (Revision_nb %in% RevisionNumber) &
      (Type_revision %in% TypeOfRevision) &
      (Date %between% c(LowDate, UpDate)),
    .(
      SD = sd(Value, na.rm = TRUE),
      MR = mean(Value, na.rm = TRUE),
      N = as.double(sum(!is.na(Value))),
      MAR = mean(abs(Value), na.rm = TRUE),
      RMSR = sqrt(mean(Value^2, na.rm = TRUE)),
      MIN = min(Value, na.rm = TRUE),
      MAX = max(Value, na.rm = TRUE)
    ),
    by = .(Variable_long, Group)
  ] |>
    merge.data.table(Final_values)

  sample[, N2S := SD / SDF]
  sample <- melt(sample, id.vars = c("Variable_long", "Group"), value.name = "Value", variable.name = "Statistic")
  return(sample[!(Statistic %in% c("SD", "SDF"))])
}

SubPlot_STATISTICS <- function(sample, Statistics, Legend, Ylabs, scales_y) {
  sample <- sample[, c("Variable_long", "Group", "Statistic") := list(
    factor(Variable_long, levels = c(
      "Total revenue", "Direct taxes", "Indirect taxes", "Social contributions", "Other current revenue", "Capital revenue",
      "Total expenditure", "Social transfers", "Purchases", "Interest payments", "Gov. compensation", "Other current expenditure", "Gov. investment", "Other capital expenditure",
      "GDP", "Private consumption", "Total investment", "Exports", "Gov. consumption", "Wages and salaries"
    )),
    factor(Group, levels = c("Revenue", "Expenditure", "Macro")),
    factor(Statistic, levels = c("N", "MR", "MIN", "MAX", "MAR", "RMSR", "N2S"))
  )][, Variable_long := forcats::fct_rev(Variable_long)]

  temp <- sample[(Statistic == "N")][order(Variable_long)]

  NewTitle <- paste0(temp$Variable_long, "\n(", temp$Value, ")")
  names(NewTitle) <- temp$Variable_long

  sample <- sample[(Statistic != "N")][, Variable_long := factor(dplyr::recode(Variable_long, !!!NewTitle), levels = NewTitle)]

  plot <- ggplot(data = subset(sample, Statistic == Statistics)) +
    ggtitle(Statistics) +

    # makes the bar and format
    geom_bar(aes(x = Variable_long, y = Value, fill = Group), stat = "identity", position = "dodge", width = 0.8) +
    geom_hline(yintercept = 0) +
    coord_flip() +

    # Add labels
    geom_text(aes(x = Variable_long, y = Value, hjust = ifelse(Value > 0, -0.2, 1.15), label = round(Value, 2)), size = 3) +

    # set general theme
    theme_ECB() +
    theme(
      plot.title = element_text(size = 9, face = "plain", colour = "black"),
      strip.text.y = element_text(hjust = 0)
    ) +
    {
      if (rlang::is_empty(scales_y)) {
        scale_y_continuous(expand = c(1, 0))
      }
    } +
    {
      if (!rlang::is_empty(scales_y$Scale)) {
        scales_y$Scale[[Statistics]]
      }
    } +
    {
      if (!rlang::is_empty(scales_y$Expand)) {
        scales_y$Expand[[Statistics]]
      }
    } +
    {
      if (!Legend) {
        theme(legend.position = "none")
      }
    } +
    {
      if (!Ylabs) {
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
      }
    } +
    # set colors and name of data
    scale_fill_manual("", values = ECB_col)

  return(plot)
}

Plot_STATISTICS <- function(sample, Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate, scales_y) {
  data <- Data_STATISTICS(sample, Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate)
  Plot1 <- SubPlot_STATISTICS(data, "MR", F, T, scales_y)
  Plot2 <- SubPlot_STATISTICS(data, "MIN", F, F, scales_y)
  Plot3 <- SubPlot_STATISTICS(data, "MAX", F, F, scales_y)
  Plot4 <- SubPlot_STATISTICS(data, "MAR", F, F, scales_y)
  Plot5 <- SubPlot_STATISTICS(data, "RMSR", F, F, scales_y)
  Plot6 <- SubPlot_STATISTICS(data, "N2S", F, F, scales_y)


  legend <- cowplot::get_legend(
    # create some space to the left of the legend
    SubPlot_STATISTICS(Data_STATISTICS(sample, Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate), "MR", T, F, scales_y)
    + theme(legend.box.margin = margin(-15, 0, 0, 0))
  )

  prow <- cowplot::plot_grid(Plot1 + theme(plot.margin = unit(c(0, -2.5, 0, 0.2), "cm")),
    Plot2 + theme(plot.margin = unit(c(0, -2, 0, 2.5), "cm")),
    Plot3 + theme(plot.margin = unit(c(0, -1.5, 0, 2), "cm")),
    Plot4 + theme(plot.margin = unit(c(0, -1, 0, 1.5), "cm")),
    Plot5 + theme(plot.margin = unit(c(0, -0.5, 0, 1), "cm")),
    Plot6 + theme(plot.margin = unit(c(0, 0, 0, 0.5), "cm")),
    align = "h", ncol = 6, vjust = -0.8
  )

  plot <- cowplot::plot_grid(legend,
    prow + theme(plot.margin = unit(c(-0.5, 0, 0, 0), "cm")),
    ncol = 1, rel_heights = c(0.1, 1)
  )

  return(plot)
}

Data_SingleVSTotal <- function(data, Countries, Items, MeasureUsed, TypeOfRevision, UpDate, LowDate) {
  sample <- data[(Country_code %in% Countries) &
    (Variable_code %in% Items) &
    (Measure == MeasureUsed) &
    (Date %between% c(LowDate, UpDate))][
    RevisionPlace == 3, RevisionPlace_renamed := "January 2017"
  ][
    RevisionPlace == 4, RevisionPlace_renamed := "April 2017"
  ][
    RevisionPlace == 5, RevisionPlace_renamed := "July 2017"
  ][
    RevisionPlace == 6, RevisionPlace_renamed := "October 2017"
  ][
    , .(RevisionPlace_renamed, Country_code, Type_revision, Value)
  ]

  sample <- rbindlist(list(
    sample[Type_revision %in% TypeOfRevision][, Type := "Plot"],
    sample[(RevisionPlace_renamed == "January 2017") & (Type_revision == "Final") | (Type_revision == "Intermediate"),
      .(Value = mean(abs(Value), na.rm = TRUE)),
      by = .(Type_revision, Country_code)
    ][
      Type_revision == "Final", Type_revision := "MAR (Final)"
    ][
      Type_revision == "Intermediate", Type_revision := "MAR (Interm.)"
    ][
      , c("Type", "RevisionPlace_renamed") := list("Table", "PLACEHOLDER")
    ]
  ), use.names = TRUE)

  sample[, c("Country_code", "RevisionPlace_renamed") := list(factor(Country_code, levels = c("AT", "FR", "FI")), factor(RevisionPlace_renamed, levels = c("January 2017", "April 2017", "July 2017", "October 2017")))]

  return(sample)
}

Plot_SingleVSTotal <- function(data) {
  table <- data[(Type == "Table")][, Value := round(Value, 2)] |>
    dcast(Type_revision ~ Country_code, value.var = "Value")

  sample <- data[(Type == "Plot")][, .(RevisionPlace_renamed, Country_code, Value)][
    , Date := RevisionPlace_renamed
  ]


  ##### Creating the table #####
  cols <- matrix("black", nrow(table), ncol(table))
  cols[1:2, 2] <- rep(ECB_col[1], 2)
  cols[1:2, 3] <- rep(ECB_col[2], 2)
  cols[1:2, 4] <- rep(ECB_col[3], 2)


  theme_table <- gridExtra::ttheme_minimal(
    base_size = 8,
    core = list(
      fg_params = list(col = cols, fontface = matrix(c(rep(2, 2), rep(1, 6)), 2, 4)),
      bg_params = list(col = NA)
    ),
    rowhead = list(bg_params = list(col = NA)),
    colhead = list(bg_params = list(col = NA))
  )

  Table2plot <- gridExtra::tableGrob(table, rows = NULL, theme = theme_table)

  Table2plot <- gtable::gtable_add_grob(Table2plot,
    grobs = grid::grid.segments( # line across the bottom
      x0 = unit(0, "npc"),
      y0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(0, "npc"),
      gp = grid::gpar(lwd = 1.0)
    ),
    t = 1, b = 1, l = 1, r = ncol(Table2plot)
  )
  ##### Creating labels#####
  data_starts <- sample[(Date == "January 2017")][, Value := round(Value, 2)]


  ##### Plotting #####
  plot <- ggplot(data = sample, aes(x = Date, y = Value, color = Country_code, group = Country_code)) +
    ggtitle("") +
    geom_line(stat = "summary", fun = sum, linewidth = 1.25) +
    stat_summary(fun = sum, geom = "line") +
    annotate("text", x = 0.85, y = data_starts[Country_code == "AT", Value], label = as.character(data_starts[Country_code == "AT", Value]), color = ECB_col[1]) +
    annotate("text", x = 0.85, y = data_starts[Country_code == "FR", Value], label = as.character(data_starts[Country_code == "FR", Value]), color = ECB_col[2]) +
    annotate("text", x = 0.85, y = data_starts[Country_code == "FI", Value], label = as.character(data_starts[Country_code == "FI", Value]), color = ECB_col[3]) +
    theme_ECB() +
    scale_color_manual(values = ECB_col)

  plot <- plot + annotation_custom(Table2plot,
    xmin = 3, ymin = -1.1,
    xmax = 4, ymax = -1.0
  )
  return(plot)
}

Data_PATHS <- function(data, Countries, Items, TypeOfRevision, MeasureUsed, UpDate, LowDate) {
  sample <- data[
    (Country_code %in% Countries) &
      (Variable_code %in% Items) &
      (Type_revision %in% TypeOfRevision) &
      (Date %between% c(LowDate, UpDate)) &
      (Measure == MeasureUsed),
    .(MAR = mean(abs(Value), na.rm = TRUE)),
    by = .(Variable_code, RevisionPlace, ObsQ, Group)
  ][,
    .(RevisionPlace = RevisionPlace, MAR = MAR, Benchmark = MAR / max(MAR)),
    by = .(Variable_code, ObsQ, Group)
  ][, NotSelected := .(fcase(
    Variable_code %in% c("COE", "DTX", "PUR", "SCT", "YEN"), Variable_code,
    !(Variable_code %in% c("COE", "DTX", "PUR", "SCT", "YEN")), "Others"
  ))][, RevisionPlace := .(fcase(
    RevisionPlace == 1, "Jul (T)",
    RevisionPlace == 2, "Oct (T)",
    RevisionPlace == 3, "Jan (T+1)",
    RevisionPlace == 4, "Apr (T+1)",
    RevisionPlace == 5, "Jul (T+1)",
    RevisionPlace == 6, "Oct (T+1)"
  ))]

  return(sample)
}

Subplot_PATHS <- function(sample, Typevalue, Legend, Q) {
  if (Typevalue == "Raw") {
    colnames(sample) <- c("Item", "ObsQ", "Group", "Revision", "value", "Normalised", "NotSelected")
  } else {
    colnames(sample) <- c("Item", "ObsQ", "Group", "Revision", "Raw", "value", "NotSelected")
  }

  sample <- sample[(ObsQ == Q)][
    ,
    Revision := factor(Revision, levels = c("Jul (T)", "Oct (T)", "Jan (T+1)", "Apr (T+1)", "Jul (T+1)", "Oct (T+1)"))
  ][
    ,
    Group := factor(Group, levels = c("Revenue", "Expenditure", "Macro", "Others"))
  ]

  plot <- ggplot(data = sample, aes(x = Revision, y = value, color = Group, group = Item)) +
    {
      if (Typevalue == "Raw") {
        ggtitle(paste0("Q", Q, " (T)"))
      }
    } +
    {
      if (Typevalue == "Normalised") {
        ggtitle("")
      }
    } +

    # makes the bar and format
    geom_line(stat = "summary", fun = sum) +
    stat_summary(fun = sum, geom = "line") +
    theme_ECB() +
    theme(
      axis.text.x = element_text(size = 6),
      plot.title = element_text(size = 10),
    ) +
    {
      if (Q == 1 | Q == 3) {
        theme(axis.title.y = element_text(size = 10, angle = 90, color = rgb(83, 83, 83, maxColorValue = 255)))
      }
    } +
    {
      if (Typevalue == "Normalised") {
        if (Q == 1 | Q == 3) {
          scale_y_continuous(Typevalue)
        } else {
          scale_y_continuous("")
        }
      }
    } +
    {
      if (Typevalue == "Raw") {
        if (Q == 1 | Q == 3) {
          ylab(Typevalue)
        }
      }
    } +
    {
      if (Typevalue == "Raw") {
        theme(axis.text.x = element_blank())
      }
    } +
    {
      if (!Legend) {
        theme(legend.position = "none")
      }
    } +
    {
      if (Legend) {
        guides(color = guide_legend(nrow = 1))
      }
    } +

    # set colors and name of data

    scale_color_manual("", values = ECB_col)

  # plot <- last_plot() + aes(group=rev(Item))

  return(plot)
}

Plot_PATHS <- function(sample) {
  plotQ1B <- Subplot_PATHS(sample, "Normalised", F, 1)
  plotQ1A <- Subplot_PATHS(sample, "Raw", F, 1)

  plotQ2B <- Subplot_PATHS(sample, "Normalised", F, 2)
  plotQ2A <- Subplot_PATHS(sample, "Raw", F, 2)

  plotQ3B <- Subplot_PATHS(sample, "Normalised", F, 3)
  plotQ3A <- Subplot_PATHS(sample, "Raw", F, 3)

  plotQ4B <- Subplot_PATHS(sample, "Normalised", F, 4)
  plotQ4A <- Subplot_PATHS(sample, "Raw", F, 4)


  prow <- cowplot::plot_grid(plotQ1A + theme(plot.margin = unit(c(0, 0, 0.05, 0), "cm")), plotQ2A + theme(plot.margin = unit(c(0, 0.2, 0.05, -0.2), "cm")),
    plotQ1B + theme(plot.margin = unit(c(-0.15, 0, 0, 0), "cm")), plotQ2B + theme(plot.margin = unit(c(-0.15, 0.2, 0, -0.2), "cm")),
    plotQ3A + theme(plot.margin = unit(c(0, 0, 0.05, 0), "cm")), plotQ4A + theme(plot.margin = unit(c(0, 0.2, 0.05, -0.2), "cm")),
    plotQ3B + theme(plot.margin = unit(c(-0.15, 0, 0, 0), "cm")), plotQ4B + theme(plot.margin = unit(c(-0.15, 0.2, 0, -0.2), "cm")),
    align = "v", ncol = 2, vjust = -0.8
  )

  prow

  legend <- cowplot::get_legend(
    # create some space to the left of the legend
    Subplot_PATHS(sample, "Normalised", T, 1) + theme(legend.box.margin = margin(-18, 0, 0, 0))
  )

  plot <- cowplot::plot_grid(legend,
    prow + theme(plot.margin = unit(c(-0.75, 0, 0, 0), "cm")),
    ncol = 1, rel_heights = c(.1, 1)
  )
  return(plot)
}

theme_ECB <- function() {
  dark_grey <- rgb(83, 83, 83, maxColorValue = 255)
  light_grey <- rgb(217, 217, 217, maxColorValue = 255)
  ECB_blue <- "#003299"
  theme_minimal() %+replace%
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      axis.ticks = element_line(color = dark_grey),
      axis.ticks.length = unit(5, "pt"),
      legend.position = "top",
      strip.text.x = element_text(size = 10),
      strip.text.y = element_text(size = 10),
      legend.text = element_text(size = 10),
      plot.caption = element_text(hjust = 0, size = 8, colour = ECB_blue),
      plot.subtitle = element_text(size = 10, colour = ECB_blue),
      plot.title = element_text(size = 14, face = "bold", colour = ECB_blue),
      panel.background = element_rect(colour = light_grey),
      axis.line = element_line(color = dark_grey),
      legend.margin = margin(t = -0.1, b = -0.1, unit = "cm"),
      legend.key.size = unit(0.2, "cm"),
      legend.key.width = unit(0.4, "cm")
    )
}

scales_final_rev <- list(
  "Scale" =
    list(
      "MR" = ggplot2::scale_y_continuous(breaks = seq(-2, 2, 2)),
      "MIN" = ggplot2::scale_y_continuous(limits = c(NA, 0), breaks = seq(-300, 0, 100)),
      "MAX" = ggplot2::scale_y_continuous(limits = c(0, NA), breaks = seq(0, 149, 50)),
      "MAR" = ggplot2::scale_y_continuous(limits = c(0, NA), breaks = seq(0, 8, 3)),
      "RMSR" = ggplot2::scale_y_continuous(limits = c(0, NA), breaks = seq(0, 20, 8)),
      "N2S" = ggplot2::scale_y_continuous(limits = c(0, NA), breaks = seq(0, 2, 0.5))
    ),
  "Expand" =
    list(
      "MR" = ggplot2::expand_limits(y = c(-3, 2.5)),
      "MIN" = ggplot2::expand_limits(y = -250),
      "MAX" = ggplot2::expand_limits(y = 149),
      "MAR" = ggplot2::expand_limits(y = 9),
      "RMSR" = ggplot2::expand_limits(y = 20),
      "N2S" = ggplot2::expand_limits(y = 1.2)
    )
)

scales_interm_rev <- list(
  "Scale" =
    list(
      "MR" = ggplot2::scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)),
      "MIN" = ggplot2::scale_y_continuous(limits = c(NA, 0), breaks = seq(-300, 0, 150)),
      "MAX" = ggplot2::scale_y_continuous(limits = c(0, NA), breaks = seq(0, 150, 75)),
      "MAR" = ggplot2::scale_y_continuous(limits = c(0, NA), breaks = seq(0, 4, 2)),
      "RMSR" = ggplot2::scale_y_continuous(limits = c(0, NA), breaks = seq(0, 10, 5)),
      "N2S" = ggplot2::scale_y_continuous(limits = c(0, NA), breaks = seq(0, 0.6, 0.3))
    ),
  "Expand" =
    list(
      "MR" = ggplot2::expand_limits(y = c(-0.9, 0.7)),
      "MIN" = ggplot2::expand_limits(y = -320),
      "MAX" = ggplot2::expand_limits(y = 170),
      "MAR" = ggplot2::expand_limits(y = 4.7),
      "RMSR" = ggplot2::expand_limits(y = 13.5),
      "N2S" = ggplot2::expand_limits(y = 0.9)
    )
)

## Customize ECB palette
rgb2hex <- function(x) rgb(x[1], x[2], x[3], maxColorValue = 255)
ECB_col <- sapply(list(
  c(0, 50, 153),
  c(255, 180, 0),
  c(255, 75, 0),
  c(101, 184, 0),
  c(0, 177, 234),
  c(0, 120, 22),
  c(129, 57, 198),
  c(92, 92, 92),
  c(152, 161, 208),
  c(253, 221, 167),
  c(246, 177, 131),
  c(206, 225, 175),
  c(215, 238, 248),
  c(141, 184, 141),
  c(174, 151, 199),
  c(169, 169, 169),
  c(217, 217, 217)
), rgb2hex)

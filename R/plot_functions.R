Data_TOTAL_REV <- function(data_rev, data_final, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber) {
  data_rev <- preprocess_revision_db(data_rev)
  data_final <- preprocess_final_values_db(data_final)

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
  data_rev <- preprocess_revision_db(data_rev)
  data_final <- preprocess_final_values_db(data_final)

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
  data <- preprocess_revision_db(data)

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
  data <- preprocess_revision_db(data)
  
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
  ][order(Country_code)]
  
  NewTitle <- paste0(temp$Country_code, "\n (", temp$N, ")")
  names(NewTitle) <- temp$Country_code
  
  sample[, Country_code := dplyr::recode(Country_code, !!!NewTitle)][, c("Country_code", "Variable_long") := list(
    factor(Country_code, levels = NewTitle),
    factor(Variable_long, levels = c("Total revenue", "Total expenditure", "GDP"))
  )]
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
    facet_wrap(. ~ Country_code, ncol = 1, strip.position = "left") +
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
  data <- preprocess_revision_db(data)
  
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
  sample <- preprocess_revision_db(sample)
  Finalvalues <- preprocess_final_values_db(Finalvalues)
  
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
    factor(Variable_long, levels = c("Total revenue", "Direct taxes", "Indirect taxes", "Social contributions", "Other current revenue", "Capital revenue",
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
    theme(plot.title = element_text(size = 9, face = "plain", colour = "black")) +
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

scales_Big9 <- list(
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

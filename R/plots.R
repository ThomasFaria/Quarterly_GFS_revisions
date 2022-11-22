source("R/utils/theme_ECB.R")
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(data.table)
library(rlang)
library(forcats)
library(cowplot)
library(gridExtra)
library(gtable)
library(grid)

# 1) comprendre comment gérer les bases de données pour le projet et pour les plots
# 2) rendre le code plus beau
# 3) faire du data.table et le rendre plus rapide
data <- arrow::read_parquet("data/RevisionDB.parquet")

data <- data %>%
  mutate(
    ObsQ = as.integer(substr(quarters(Date), 2, 2)),
    ObsY = year(Date),
    Measure = "GRate",
    Revision_nb = as.double(Revision_nb),
    ReleaseDate = as.Date(case_when(
      startsWith(Vintage_base, "W") ~ paste0(str_replace(Vintage_base, "W", "20"), "-01-01"),
      startsWith(Vintage_base, "G") ~ paste0(str_replace(Vintage_base, "G", "20"), "-04-01"),
      startsWith(Vintage_base, "S") ~ paste0(str_replace(Vintage_base, "S", "20"), "-07-01"),
      startsWith(Vintage_base, "A") ~ paste0(str_replace(Vintage_base, "A", "20"), "-10-01")
    )),
    IsNAN = is.na(Value),
    Group = case_when(
      Variable_code %in% c("TOR", "DTX", "TIN", "SCT") ~ "Revenue",
      Variable_code %in% c("TOE", "THN", "PUR", "COE", "GIN") ~ "Expenditure",
      Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS") ~ "Macro",
      TRUE ~ "Others"
    ),
    Group2 = case_when(
      Variable_code %in% c("TOR", "DTX", "TIN", "SCT", "OCR", "KTR") ~ "Revenue",
      Variable_code %in% c("TOE", "THN", "PUR", "INP", "COE", "OCE", "GIN", "OKE") ~ "Expenditure",
      Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS") ~ "Macro",
      TRUE ~ "Others"
    ),
    ToShade = case_when(
      Variable_code %in% c("KTR", "OCR", "OCE", "OKE", "INP") ~ "TRUE",
      TRUE ~ "FALSE"
    ),
    RevisionPlace = case_when(
      ObsQ == 1 ~ Revision_nb,
      ObsQ == 2 ~ Revision_nb + 1,
      ObsQ == 3 ~ Revision_nb + 2,
      ObsQ == 4 ~ Revision_nb + 3,
    ),
    Variable_long = case_when(
      Variable_code %in% c("TOR") ~ "Total revenue",
      Variable_code %in% c("DTX") ~ "Direct taxes",
      Variable_code %in% c("TIN") ~ "Indirect taxes",
      Variable_code %in% c("SCT") ~ "Social contributions",
      Variable_code %in% c("TOE") ~ "Total expenditure",
      Variable_code %in% c("THN") ~ "Social transfers",
      Variable_code %in% c("PUR") ~ "Purchases",
      Variable_code %in% c("COE") ~ "Gov. compensation",
      Variable_code %in% c("GIN") ~ "Gov. investment",
      Variable_code %in% c("YEN") ~ "GDP",
      Variable_code %in% c("PCN") ~ "Private consumption",
      Variable_code %in% c("ITN") ~ "Total investment",
      Variable_code %in% c("EXN") ~ "Exports",
      Variable_code %in% c("GCN") ~ "Gov. consumption",
      Variable_code %in% c("WGS") ~ "Wages and salaries",
      Variable_code %in% c("OCR") ~ "Other current revenue",
      Variable_code %in% c("KTR") ~ "Capital revenue",
      Variable_code %in% c("INP") ~ "Interest payments",
      Variable_code %in% c("OCE") ~ "Other current expenditure",
      Variable_code %in% c("OKE") ~ "Other capital expenditure",
      TRUE ~ "Others"
    )
  ) %>%
  filter_all(all_vars(!is.infinite(.)))

DatasetRaw <- arrow::read_csv_arrow("data/RealTimeDatabase.csv")
setDT(DatasetRaw)
DatasetRaw <- DatasetRaw %>%
  mutate(
    ObsQ = as.integer(substr(quarters(Date), 2, 2)),
    ObsY = year(Date),
    ReleaseDate = as.Date(ifelse(startsWith(ECB_vintage, "W"), paste0(str_replace(ECB_vintage, "W", "20"), "-01-01"),
      ifelse(startsWith(ECB_vintage, "G"), paste0(str_replace(ECB_vintage, "G", "20"), "-04-01"),
        ifelse(startsWith(ECB_vintage, "S"), paste0(str_replace(ECB_vintage, "S", "20"), "-07-01"),
          paste0(str_replace(ECB_vintage, "A", "20"), "-07-01")
        )
      )
    )),
    Group = case_when(
      Variable_code %in% c("TOR", "DTX", "TIN", "SCT") ~ "Revenue",
      Variable_code %in% c("TOE", "THN", "PUR", "COE", "GIN") ~ "Expenditure",
      Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS") ~ "Macro",
      TRUE ~ "Others"
    ),
    Group2 = case_when(
      Variable_code %in% c("TOR", "DTX", "TIN", "SCT", "OCR", "KTR") ~ "Revenue",
      Variable_code %in% c("TOE", "THN", "PUR", "INP", "COE", "OCE", "GIN", "OKE") ~ "Expenditure",
      Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS") ~ "Macro",
      TRUE ~ "Others"
    ),
    ToShade = case_when(
      Variable_code %in% c("KTR", "OCR", "OCE", "OKE", "INP") ~ "TRUE",
      TRUE ~ "FALSE"
    ),
    Variable_long = case_when(
      Variable_code %in% c("TOR") ~ "Total revenue",
      Variable_code %in% c("DTX") ~ "Direct taxes",
      Variable_code %in% c("TIN") ~ "Indirect taxes",
      Variable_code %in% c("SCT") ~ "Social contributions",
      Variable_code %in% c("TOE") ~ "Total expenditure",
      Variable_code %in% c("THN") ~ "Social transfers",
      Variable_code %in% c("PUR") ~ "Purchases",
      Variable_code %in% c("COE") ~ "Gov. compensation",
      Variable_code %in% c("GIN") ~ "Gov. investment",
      Variable_code %in% c("YEN") ~ "GDP",
      Variable_code %in% c("PCN") ~ "Private consumption",
      Variable_code %in% c("ITN") ~ "Total investment",
      Variable_code %in% c("EXN") ~ "Exports",
      Variable_code %in% c("GCN") ~ "Gov. consumption",
      Variable_code %in% c("WGS") ~ "Wages and salaries",
      Variable_code %in% c("OCR") ~ "Other current revenue",
      Variable_code %in% c("KTR") ~ "Capital revenue",
      Variable_code %in% c("INP") ~ "Interest payments",
      Variable_code %in% c("OCE") ~ "Other current expenditure",
      Variable_code %in% c("OKE") ~ "Other capital expenditure",
      TRUE ~ "Others"
    )
  ) %>%
  filter_all(all_vars(!is.infinite(.)))

FinalValues <- arrow::read_parquet("data/FinalValues.parquet")
FinalValues <- FinalValues %>%
  mutate(Variable_long = case_when(
    Variable_code %in% c("TOR") ~ "Total revenue",
    Variable_code %in% c("DTX") ~ "Direct taxes",
    Variable_code %in% c("TIN") ~ "Indirect taxes",
    Variable_code %in% c("SCT") ~ "Social contributions",
    Variable_code %in% c("TOE") ~ "Total expenditure",
    Variable_code %in% c("THN") ~ "Social transfers",
    Variable_code %in% c("PUR") ~ "Purchases",
    Variable_code %in% c("COE") ~ "Gov. compensation",
    Variable_code %in% c("GIN") ~ "Gov. investment",
    Variable_code %in% c("YEN") ~ "GDP",
    Variable_code %in% c("PCN") ~ "Private consumption",
    Variable_code %in% c("ITN") ~ "Total investment",
    Variable_code %in% c("EXN") ~ "Exports",
    Variable_code %in% c("GCN") ~ "Gov. consumption",
    Variable_code %in% c("WGS") ~ "Wages and salaries",
    Variable_code %in% c("OCR") ~ "Other current revenue",
    Variable_code %in% c("KTR") ~ "Capital revenue",
    Variable_code %in% c("INP") ~ "Interest payments",
    Variable_code %in% c("OCE") ~ "Other current expenditure",
    Variable_code %in% c("OKE") ~ "Other capital expenditure",
    TRUE ~ "Others"
  ))

# Lists #####
VintageList <- c(outer(c("W", "G", "S", "A"), str_pad(7:20, 2, pad = "0"), FUN = paste0))
Ctry_ALL <- c(
  "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT",
  "REA", "EA",
  "PL", "SE", "DK", "RO", "CZ", "BG", "HU", "HR"
)

Ctry_EA19 <- c("EA", "DE", "FR", "IT", "ES", "NL", "BE", "AT", "FI", "PT", "GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT")
Ctry_Agg <- c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT")

Var_Revenue <- c("TOR", "DTX", "TIN", "SCT")
Var_Expenditure <- c("TOE", "THN", "PUR", "COE", "GIN")
Var_Macro <- c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS")
LevelItem <- c(
  "TOR", "DTX", "TIN", "SCT", "OCR", "KTR",
  "TOE", "THN", "PUR", "INP", "COE", "OCE", "GIN", "OKE",
  "YEN", "PCN", "ITN", "EXN", "GCN", "WGS"
)
LevelItem2 <- c(
  "Total revenue", "Direct taxes", "Indirect taxes", "Social contributions", "Other current revenue", "Capital revenue",
  "Total expenditure", "Social transfers", "Purchases", "Interest payments", "Gov. compensation", "Other current expenditure", "Gov. investment", "Other capital expenditure",
  "GDP", "Private consumption", "Total investment", "Exports", "Gov. consumption", "Wages and salaries"
)

# STATISTICS ITEM #####

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
      N = sum(!is.na(Value)),
      MAR = mean(abs(Value), na.rm = TRUE),
      RMSR = sqrt(mean(Value^2, na.rm = TRUE)),
      MIN = min(Value, na.rm = TRUE),
      MAX = max(Value, na.rm = TRUE)
    ),
    by = .(Variable_long, Group)
  ] %>%
    merge.data.table(Final_values)

  sample[, N2S := SD / SDF]
  sample <- melt(sample, id.vars = c("Variable_long", "Group"), value.name = "Value", variable.name = "Statistic")
  return(sample[!(Statistic %in% c("SD", "SDF"))])
}
SubPlot_STATISTICS <- function(sample, Statistics, Legend, Ylabs, scales_y) {
  sample <- sample %>%
    mutate(
      Variable_long = factor(Variable_long, levels = LevelItem2),
      Variable_long = forcats::fct_rev(Variable_long),
      Group = factor(Group, levels = c(
        "Revenue",
        "Expenditure",
        "Macro"
      )),
      Statistic = factor(Statistic, levels = c(
        "N",
        "MR",
        "MIN",
        "MAX",
        "MAR",
        "RMSR",
        "N2S"
      ))
    )

  temp <- sample %>%
    subset(Statistic == "N") %>%
    arrange(Variable_long)


  NewTitle <- paste0(temp$Variable_long, "\n(", temp$Value, ")")
  names(NewTitle) <- temp$Variable_long
  sample <- sample %>%
    subset(Statistic != "N") %>%
    mutate(
      Variable_long = recode(Variable_long, !!!NewTitle),
      Variable_long = factor(Variable_long, levels = NewTitle)
    )

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
  Plot1 <- SubPlot_STATISTICS(Data_STATISTICS(sample, Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate), "MR", F, T, scales_y)
  Plot2 <- SubPlot_STATISTICS(Data_STATISTICS(sample, Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate), "MIN", F, F, scales_y)
  Plot3 <- SubPlot_STATISTICS(Data_STATISTICS(sample, Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate), "MAX", F, F, scales_y)
  Plot4 <- SubPlot_STATISTICS(Data_STATISTICS(sample, Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate), "MAR", F, F, scales_y)
  Plot5 <- SubPlot_STATISTICS(Data_STATISTICS(sample, Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate), "RMSR", F, F, scales_y)
  Plot6 <- SubPlot_STATISTICS(Data_STATISTICS(sample, Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate), "N2S", F, F, scales_y)


  legend <- cowplot::get_legend(
    # create some space to the left of the legend
    SubPlot_STATISTICS(Data_STATISTICS(sample, Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate), "MR", T, F, scales_y)
    + theme(legend.box.margin = margin(-15, 0, 0, 0))
  )

  prow <- plot_grid(Plot1 + theme(plot.margin = unit(c(0, -2.5, 0, 0.2), "cm")),
    Plot2 + theme(plot.margin = unit(c(0, -2, 0, 2.5), "cm")),
    Plot3 + theme(plot.margin = unit(c(0, -1.5, 0, 2), "cm")),
    Plot4 + theme(plot.margin = unit(c(0, -1, 0, 1.5), "cm")),
    Plot5 + theme(plot.margin = unit(c(0, -0.5, 0, 1), "cm")),
    Plot6 + theme(plot.margin = unit(c(0, 0, 0, 0.5), "cm")),
    align = "h", ncol = 6, vjust = -0.8
  )



  plot <- plot_grid(legend,
    prow + theme(plot.margin = unit(c(-0.5, 0, 0, 0), "cm")),
    ncol = 1, rel_heights = c(0.1, 1)
  )

  return(plot)
}

# Big 9
scales_Big9 <- list(
  "Scale" =
    list(
      "MR" = scale_y_continuous(breaks = seq(-2, 2, 2)),
      "MIN" = scale_y_continuous(limits = c(NA, 0), breaks = seq(-300, 0, 100)),
      "MAX" = scale_y_continuous(limits = c(0, NA), breaks = seq(0, 149, 50)),
      "MAR" = scale_y_continuous(limits = c(0, NA), breaks = seq(0, 8, 3)),
      "RMSR" = scale_y_continuous(limits = c(0, NA), breaks = seq(0, 20, 8)),
      "N2S" = scale_y_continuous(limits = c(0, NA), breaks = seq(0, 2, 0.5))
    ),
  "Expand" =
    list(
      "MR" = expand_limits(y = c(-3, 2.5)),
      "MIN" = expand_limits(y = -250),
      "MAX" = expand_limits(y = 149),
      "MAR" = expand_limits(y = 9),
      "RMSR" = expand_limits(y = 20),
      "N2S" = expand_limits(y = 1.2)
    )
)
Plot_STATISTICS(data, FinalValues, c(Ctry_Agg, "REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Final", "GRate", c(1), as.Date("2020-01-01"), as.Date("2006-01-01"), scales_Big9)
Plot_STATISTICS(data, FinalValues, c(Ctry_Agg, "REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Final", "GRate", c(1), as.Date("2013-12-31"), as.Date("2006-01-01"), scales_Big9)
Plot_STATISTICS(data, FinalValues, c(Ctry_Agg, "REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Final", "GRate", c(1), as.Date("2020-01-01"), as.Date("2013-12-31"), scales_Big9)

scales_Big9_Inter <- list(
  "Scale" =
    list(
      "MR" = scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)),
      "MIN" = scale_y_continuous(limits = c(NA, 0), breaks = seq(-300, 0, 150)),
      "MAX" = scale_y_continuous(limits = c(0, NA), breaks = seq(0, 150, 75)),
      "MAR" = scale_y_continuous(limits = c(0, NA), breaks = seq(0, 4, 2)),
      "RMSR" = scale_y_continuous(limits = c(0, NA), breaks = seq(0, 10, 5)),
      "N2S" = scale_y_continuous(limits = c(0, NA), breaks = seq(0, 0.6, 0.3))
    ),
  "Expand" =
    list(
      "MR" = expand_limits(y = c(-0.9, 0.7)),
      "MIN" = expand_limits(y = -320),
      "MAX" = expand_limits(y = 170),
      "MAR" = expand_limits(y = 4.7),
      "RMSR" = expand_limits(y = 13.5),
      "N2S" = expand_limits(y = 0.9)
    )
)

Plot_STATISTICS(data, FinalValues, c(Ctry_Agg, "REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Intermediate", "GRate", 1:5, as.Date("2013-12-31"), as.Date("2006-01-01"), scales_Big9_Inter)
Plot_STATISTICS(data, FinalValues, c(Ctry_Agg, "REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Intermediate", "GRate", 1:5, as.Date("2020-01-01"), as.Date("2013-12-31"), scales_Big9_Inter)

# Total_Revision #####
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
    geom_bar(data = subset(sample, Type == "Revision"), aes(x = Date, y = Value, fill = "#003299"), stat = "identity") +
    geom_line(data = subset(sample, Type == "Data"), aes(x = Date, y = Value, color = "black")) +
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
      (Measure == MeasureUsed)][, Type := "Data"][, .(Date, Value, Revision_nb, Type)]
  ))

  return(sample)
}
Plot_TOTAL_REV_DECOMP <- function(sample, Legend) {
  ###### Defining factors

  sample$Revision_nb <- factor(sample$Revision_nb, levels = c(
    "1",
    "2",
    "3",
    "4",
    "5"
  ))

  SumData <- sample %>%
    subset(Type == "Revision") %>%
    group_by(Date) %>%
    summarise(
      Sum = sum(Value)
    ) %>%
    mutate(FinalRev = "Final revision")
  ##### Plotting
  plot <- ggplot() +
    ggtitle("") +

    # makes the bar and format
    geom_bar(data = subset(sample, Type == "Revision"), aes(x = Date, y = Value, fill = Revision_nb), stat = "identity") +
    geom_point(data = SumData, aes(x = Date, y = Sum, color = FinalRev), shape = "-", size = 6, stroke = 7) +
    geom_line(data = subset(sample, Type == "Data"), aes(x = Date, y = Value, linetype = Type)) +
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

  legend1 <- get_legend(
    Plot_TOTAL_REV(sample_rev, T) + theme(legend.box.margin = margin(0, 0, 0, 0))
  )

  legend2 <- get_legend(
    Plot_TOTAL_REV_DECOMP(sample_rev_decomp, T) + theme(legend.box.margin = margin(0, 0, 0, 0))
  )

  legends <- plot_grid(legend1,
    legend2,
    ncol = 2, vjust = -0.8
  )

  prow <- plot_grid(Plot1 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
    Plot2 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
    ncol = 2, vjust = -0.8
  )

  plot <- plot_grid(legends + theme(plot.margin = unit(c(1.4, 0, -0.1, 0), "cm")),
    prow + theme(plot.margin = unit(c(1.2, 0.2, 0, 0), "cm")),
    ncol = 1, rel_heights = c(0.1, 1)
  )

  return(plot)
}
Plot_TOTAL_REVISION(data, FinalValues, c("NL"), c("COE"), "GRate")

# Total VS Intermediate ####
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

  return(sample)
}
Plot_SingleVSTotal <- function(data) {
  table <- le[(Type == "Table")][, Value := round(Value, 2)] %>%
    dcast(Type_revision ~ Country_code, value.var = "Value")

  sample <- le[(Type == "Plot")][, .(RevisionPlace_renamed, Country_code, Value)][
    , Date := RevisionPlace_renamed
  ]

  #   Ctry = factor(Ctry, levels = c("AT", "FR", "FI")
  #  Date = factor(Date, levels = c("January 2017", "April 2017", "July 2017", "October 2017")

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

  Table2plot <- tableGrob(table, rows = NULL, theme = theme_table)

  Table2plot <- gtable::gtable_add_grob(Table2plot,
    grobs = grid::grid.segments( # line across the bottom
      x0 = unit(0, "npc"),
      y0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(0, "npc"),
      gp = gpar(lwd = 1.0)
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
    xmin = 3, ymin = 1.1,
    xmax = 4, ymax = 1.0
  )
  return(plot)
}
Plot_SingleVSTotal(Data_SingleVSTotal(data, c("FR", "AT", "FI"), c("SCT"), c("GRate"), c("Final"), as.Date("2016-07-02"), as.Date("2016-06-30")))

# PATHS #####
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
Plot_PATHS1 <- function(sample, Typevalue, Legend, Q) {
  if (Typevalue == "Raw") {
    colnames(sample) <- c("Item", "ObsQ", "Group", "Revision", "value", "Normalised", "NotSelected")
  } else {
    colnames(sample) <- c("Item", "ObsQ", "Group", "Revision", "Raw", "value", "NotSelected")
  }

  sample <- sample %>%
    subset(ObsQ == Q) %>%
    mutate(
      Revision = factor(Revision, level = c("Jul (T)", "Oct (T)", "Jan (T+1)", "Apr (T+1)", "Jul (T+1)", "Oct (T+1)")),
      Group = factor(Group, levels = c(
        "Revenue",
        "Expenditure",
        "Macro",
        "Others"
      ))
    )

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
  plotQ1B <- Plot_PATHS1(sample, "Normalised", F, 1)
  plotQ1A <- Plot_PATHS1(sample, "Raw", F, 1)

  plotQ2B <- Plot_PATHS1(sample, "Normalised", F, 2)
  plotQ2A <- Plot_PATHS1(sample, "Raw", F, 2)

  plotQ3B <- Plot_PATHS1(sample, "Normalised", F, 3)
  plotQ3A <- Plot_PATHS1(sample, "Raw", F, 3)

  plotQ4B <- Plot_PATHS1(sample, "Normalised", F, 4)
  plotQ4A <- Plot_PATHS1(sample, "Raw", F, 4)


  prow <- plot_grid(plotQ1A + theme(plot.margin = unit(c(0, 0, 0.05, 0), "cm")), plotQ2A + theme(plot.margin = unit(c(0, 0.2, 0.05, -0.2), "cm")),
    plotQ1B + theme(plot.margin = unit(c(-0.15, 0, 0, 0), "cm")), plotQ2B + theme(plot.margin = unit(c(-0.15, 0.2, 0, -0.2), "cm")),
    plotQ3A + theme(plot.margin = unit(c(0, 0, 0.05, 0), "cm")), plotQ4A + theme(plot.margin = unit(c(0, 0.2, 0.05, -0.2), "cm")),
    plotQ3B + theme(plot.margin = unit(c(-0.15, 0, 0, 0), "cm")), plotQ4B + theme(plot.margin = unit(c(-0.15, 0.2, 0, -0.2), "cm")),
    align = "v", ncol = 2, vjust = -0.8
  )

  prow

  legend <- get_legend(
    # create some space to the left of the legend
    Plot_PATHS1(sample, "Normalised", T, 1) + theme(legend.box.margin = margin(-18, 0, 0, 0))
  )

  plot <- plot_grid(legend,
    prow + theme(plot.margin = unit(c(-0.75, 0, 0, 0), "cm")),
    ncol = 1, rel_heights = c(.1, 1)
  )
  return(plot)
}
Plot_PATHS(Data_PATHS(data, Ctry_Agg, c(Var_Expenditure, Var_Revenue, Var_Macro), "Final", "GRate", as.Date("2020-01-01"), as.Date("2006-01-01")))
Plot_PATHS(Data_PATHS(data, Ctry_Agg, c(Var_Expenditure, Var_Revenue, Var_Macro), "Final", "GRate", as.Date("2014-03-01"), as.Date("2006-01-01")))
Plot_PATHS(Data_PATHS(data, Ctry_Agg, c(Var_Expenditure, Var_Revenue, Var_Macro), "Final", "GRate", as.Date("2020-01-01"), as.Date("2014-03-01")))

# Shares GDP ####
Data_Share_GDP <- function(data, Countries, Items, Vintages) {
  sample <- data[
    (Variable_code %in% Items) &
      (Country_code %in% Countries) &
      (ECB_vintage %in% Vintages),
    .(Value = mean(Value, na.rm = TRUE)),
    by = .(Variable_long, Group2, ToShade)
  ][
    , Share := Value / max(Value) * 100
  ]
  return(sample)
}
Plot_Share_GDP <- function(sample) {
  sample <- sample %>%
    mutate(
      Variable_long = factor(Variable_long, levels = LevelItem2),
      Group2 = factor(Group2, levels = c(
        "Revenue",
        "Expenditure",
        "Macro",
        "Others"
      ))
    )


  plot <- ggplot(data = sample) +
    ggtitle("") +

    # makes the bar and format
    geom_bar(aes(x = Variable_long, y = Share / 100, fill = Group2, alpha = ToShade), stat = "identity", position = "dodge", width = 0.8) +

    # Add labels
    geom_text(aes(x = Variable_long, y = Share / 100, vjust = ifelse(Share > 0, -0.2, 1.15), label = round(Share, 2)), size = 3, color = rgb(83, 83, 83, maxColorValue = 255)) +

    # set general theme
    theme_ECB() +
    theme(axis.text.x = element_text(size = 10, angle = 90)) +
    scale_y_continuous(labels = scales::percent) +
    expand_limits(y = 1.02) +
    # set colors and name of data
    scale_fill_manual("", values = c(ECB_col)) +
    scale_alpha_manual("", values = c(1, 0.4), guide = "none")

  return(plot)
}
Plot_Share_GDP(Data_Share_GDP(DatasetRaw, setdiff(Ctry_EA19, "EA"), c(Var_Revenue, Var_Macro, Var_Expenditure, "KTR", "OCR", "OCE", "OKE", "INP"), "S21"))

# Mean and volatility ####
Data_Mean_SD <- function(data, Countries, Items, Vintages, MeasureUsed, UpDate, LowDate) {
  sample <- data[
    (Variable_code %in% Items) &
      (Country_code %in% Countries) &
      (ECB_vintage %in% Vintages) &
      (Date %between% c(LowDate, UpDate)),
    .(
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE)
    ),
    by = .(Variable_long, Group2, ToShade)
  ] %>%
    melt(id.vars = c("Variable_long", "Group2", "ToShade"), value.name = "Value", variable.name = "Statistic")
  return(sample)
}
SubPlot_Mean_SD <- function(sample, Statistics, Legend, Ylabs, scales_y) {
  sample <- sample %>%
    mutate(
      Item2 = factor(Variable_long, levels = LevelItem2),
      Item2 = fct_rev(Variable_long),
      Group = factor(Group2, levels = c(
        "Revenue",
        "Expenditure",
        "Macro",
        "Others"
      )),
      Statistic = factor(Statistic, levels = c(
        "Mean",
        "SD",
        "Share"
      ))
    )


  plot <- ggplot(data = subset(sample, Statistic == Statistics)) +
    ggtitle(ifelse(Statistics == "SD", "Standard deviation", Statistics)) +

    # makes the bar and format
    geom_bar(aes(x = Variable_long, y = Value, fill = Group2, alpha = ToShade), stat = "identity", position = "dodge", width = 0.8) +
    geom_hline(yintercept = 0) +
    coord_flip() +

    # Add labels
    {
      if (Statistics == "Share") {
        geom_text(aes(x = Variable_long, y = Value, hjust = ifelse(Value > 0, -0.2, 1.15), label = round(Value * 100, 2)), size = 3)
      }
    } +
    {
      if (Statistics != "Share") {
        geom_text(aes(x = Variable_long, y = Value, hjust = ifelse(Value > 0, -0.2, 1.15), label = round(Value, 2)), size = 3)
      }
    } +

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
    scale_fill_manual("", values = ECB_col) +
    scale_alpha_manual("", values = c(1, 0.4), guide = "none")

  return(plot)
}
Plot_Mean_SD <- function(sample, Countries, Items, Vintages, MeasureUsed, UpDate, LowDate, scales_y) {
  Plot1 <- SubPlot_Mean_SD(Data_Mean_SD(sample, Countries, Items, Vintages, MeasureUsed, UpDate, LowDate), "Mean", F, T, scales_y)
  Plot2 <- SubPlot_Mean_SD(Data_Mean_SD(sample, Countries, Items, Vintages, MeasureUsed, UpDate, LowDate), "SD", F, F, scales_y)

  legend <- get_legend(
    # create some space to the left of the legend
    SubPlot_Mean_SD(Data_Mean_SD(sample, Countries, Items, Vintages, MeasureUsed, UpDate, LowDate), "Mean", T, F, scales_y)
    + theme(legend.box.margin = margin(-15, 0, 0, 0))
  )

  prow <- plot_grid(Plot1 + theme(plot.margin = unit(c(0, -1.7, 0, 0.1), "cm")),
    Plot2 + theme(plot.margin = unit(c(0, 0.2, 0, 1.8), "cm")),
    align = "h", ncol = 2, vjust = -0.8
  )


  plot <- plot_grid(legend,
    prow + theme(plot.margin = unit(c(-0.5, 0, 0, 0.2), "cm")),
    ncol = 1, rel_heights = c(0.1, 1)
  )

  return(plot)
}

scales_ <- list(
  "Scale" =
    list(
      "Mean" = scale_y_continuous(limits = c(NA, NA), breaks = seq(0, 6, 3)),
      "SD" = scale_y_continuous(limits = c(0, NA), breaks = seq(0, 90, 45))
    ),
  "Expand" =
    list(
      "Mean" = expand_limits(y = c(-1, 7.2)),
      "SD" = expand_limits(y = 120)
    )
)

Plot_Mean_SD(DatasetRaw, setdiff(Ctry_EA19, "EA"), c(Var_Revenue, Var_Macro, Var_Expenditure, "KTR", "OCR", "OCE", "OKE", "INP"), "S21", "GRate", as.Date("2020-01-01"), as.Date("2006-01-01"), scales_)

# Ranking TOE ####
Data_Ranking <- function(data, Items, Vintages, ObsYear) {
  sample <- data[
    (Variable_code %in% Items) &
      (ECB_vintage %in% Vintages) &
      (year(Date) %in% ObsYear),
    .(Value = sum(Value, na.rm = TRUE)),
    by = .(Country_code)
  ][
    , Share := Value / max(Value)
  ]
  return(sample)
}
Plot_Ranking <- function(sample) {
  sample <- sample %>%
    subset(!(Country_code %in% c("EA", "I8"))) %>%
    mutate(
      Country_code = factor(Country_code, levels = c(
        "DE", "FR", "IT", "ES", "NL", "BE",
        "AT", "FI", "PT", "REA", "GR", "IE", "SK",
        "LU", "SI", "LT", "LV", "EE", "CY", "MT"
      )),
      Group = case_when(
        Country_code %in% c("GR", "IE", "SK", "LU", "SI", "LT", "LV", "EE", "CY", "MT") ~ "Others",
        Country_code == "REA" ~ "Rest of EA",
        TRUE ~ "Big 9"
      )
    )

  plot <- ggplot(data = sample) +
    ggtitle("") +

    # makes the bar and format
    geom_bar(aes(x = Country_code, y = Share, fill = Group), stat = "identity", position = "dodge", width = 0.8) +

    # Add labels
    geom_text(aes(x = Country_code, y = Share, vjust = ifelse(Share > 0, -0.2, 1.15), label = round(Share * 100, 2)), size = 3, color = rgb(83, 83, 83, maxColorValue = 255)) +

    # set general theme
    theme_ECB() +
    scale_y_continuous(labels = scales::percent) +
    expand_limits(y = 0.3) +
    # set colors and name of data
    scale_fill_manual("", values = c(ECB_col[1], ECB_col[9], ECB_col[3]))

  return(plot)
}
Plot_Ranking(Data_Ranking(DatasetRaw, "TOE", "S21", 2019))

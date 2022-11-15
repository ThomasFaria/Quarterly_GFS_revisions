source('R/utils/theme_ECB.R')
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(data.table)
library(rlang)
library(forcats)
library(cowplot)

library(ggplot2)
library(readr)
library(reshape2)
library(tidyverse)
library(forcats)
library(broom)
library(stringr)
library(cowplot)
library(tikzDevice)
library(lubridate)
library(facetscales)

# 1) comprendre comment gérer les bases de données pour le projet et pour les plots
# 2) rendre le code plus beau
# 3) faire du data.table et le rendre plus rapide
data <- arrow::read_parquet("data/RevisionDB.parquet")

data <-  data %>%
  mutate(ObsQ=  as.integer(substr(quarters(Date), 2, 2) ),
         ObsY= year(Date),
         Measure= "GRate",
         Revision_nb = as.double(Revision_nb),
         ReleaseDate = as.Date(case_when(
           startsWith(Vintage_base, "W") ~ paste0(str_replace(Vintage_base, "W", "20"),"-01-01"),
           startsWith(Vintage_base, "G") ~ paste0(str_replace(Vintage_base, "G", "20"),"-04-01"),
           startsWith(Vintage_base, "S") ~ paste0(str_replace(Vintage_base, "S", "20"),"-07-01"),
           startsWith(Vintage_base, "A") ~ paste0(str_replace(Vintage_base, "A", "20"),"-10-01")
         )),
         IsNAN = is.na(Value),
         Group = case_when(Variable_code %in% c("TOR", "DTX", "TIN", "SCT") ~ "Revenue",
                           Variable_code %in% c("TOE","THN","PUR", "COE", "GIN") ~ "Expenditure",
                           Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS") ~ "Macro",
                           TRUE ~ "Others"
         ),
         Group2 = case_when(Variable_code %in% c("TOR", "DTX", "TIN", "SCT", "OCR", "KTR") ~ "Revenue",
                            Variable_code %in% c("TOE","THN","PUR","INP", "COE","OCE", "GIN", "OKE") ~ "Expenditure",
                            Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS") ~ "Macro",
                            TRUE ~ "Others"
         ),
         ToShade = case_when(Variable_code %in% c("KTR", "OCR", "OCE", "OKE", "INP") ~ "TRUE",
                             TRUE ~ "FALSE"
         ),
         RevisionPlace = case_when(ObsQ == 1 ~ Revision_nb,
                                   ObsQ == 2 ~ Revision_nb + 1,
                                   ObsQ == 3 ~ Revision_nb + 2,
                                   ObsQ == 4 ~ Revision_nb + 3,
         ),
         Variable_long = case_when(Variable_code %in% c("TOR") ~ "Total revenue",
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
  )%>% 
  filter_all(all_vars(!is.infinite(.)))

DatasetRaw <- arrow::read_csv_arrow("data/RealTimeDatabase.csv")
DatasetRaw <- DatasetRaw%>%
  mutate(ObsQ=  as.integer(substr(quarters(Date), 2, 2) ),
         ObsY= year(Date),
         ReleaseDate = as.Date(ifelse(startsWith(ECB_vintage, "W") , paste0(str_replace(ECB_vintage, "W", "20"),"-01-01"), 
                                      ifelse(startsWith(ECB_vintage, "G") , paste0(str_replace(ECB_vintage, "G", "20"),"-04-01"),
                                             ifelse(startsWith(ECB_vintage, "S"), paste0(str_replace(ECB_vintage, "S", "20"),"-07-01"),
                                                    paste0(str_replace(ECB_vintage, "A", "20"),"-07-01"))))),
         Group = case_when(Variable_code %in% c("TOR", "DTX", "TIN", "SCT") ~ "Revenue",
                           Variable_code %in% c("TOE","THN","PUR", "COE", "GIN") ~ "Expenditure",
                           Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS") ~ "Macro",
                           TRUE ~ "Others"
         ),
         Group2 = case_when(Variable_code %in% c("TOR", "DTX", "TIN", "SCT", "OCR", "KTR") ~ "Revenue",
                            Variable_code %in% c("TOE","THN","PUR","INP", "COE","OCE", "GIN", "OKE") ~ "Expenditure",
                            Variable_code %in% c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS") ~ "Macro",
                            TRUE ~ "Others"
         ),
         ToShade = case_when(Variable_code %in% c("KTR", "OCR", "OCE", "OKE","INP") ~ "TRUE",
                             TRUE ~ "FALSE"
         ),
         Variable_long = case_when(Variable_code %in% c("TOR") ~ "Total revenue",
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
  )%>% 
  filter_all(all_vars(!is.infinite(.)))

FinalValues <- arrow::read_parquet("data/FinalValues.parquet")
FinalValues <- FinalValues%>%
  mutate(Variable_long = case_when(Variable_code %in% c("TOR") ~ "Total revenue",
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
VintageList <- c(outer(c("W", "G", "S", "A"), str_pad(7:20, 2, pad = "0"), FUN=paste0))
Ctry_ALL <- c("DE", "FR", "IT","ES", "NL", "BE", "AT","FI", "PT",  "GR","IE",  "SK", "LU","SI","LT", "LV","EE", "CY","MT",
              "REA","EA",
              "PL", "SE", "DK","RO","CZ","BG", "HU", "HR")

Ctry_EA19 <- c("EA", "DE", "FR", "IT","ES", "NL", "BE", "AT","FI", "PT",  "GR","IE",  "SK", "LU","SI","LT", "LV","EE", "CY","MT")
Ctry_Agg <- c("DE", "ES", "FR", "IT", "NL", "BE","AT","FI", "PT")

Var_Revenue <- c("TOR", "DTX", "TIN", "SCT")
Var_Expenditure <- c("TOE","THN","PUR", "COE", "GIN")
Var_Macro <- c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS")
LevelItem <- c("TOR", "DTX", "TIN", "SCT", "OCR", "KTR",
               "TOE","THN","PUR","INP", "COE","OCE", "GIN", "OKE",
               "YEN", "PCN", "ITN", "EXN", "GCN", "WGS")
LevelItem2 <- c("Total revenue","Direct taxes","Indirect taxes","Social contributions","Other current revenue","Capital revenue",
                "Total expenditure", "Social transfers", "Purchases","Interest payments","Gov. compensation","Other current expenditure","Gov. investment","Other capital expenditure",
                "GDP","Private consumption","Total investment","Exports","Gov. consumption", "Wages and salaries")


# STATISTICS ITEM #####
Data_STATISTICS(data, FinalValues, c(Ctry_Agg, "REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Final", "GRate", c(1), as.Date("2020-01-01"), as.Date("2006-01-01"))
SubPlot_STATISTICS(Data_STATISTICS(data, FinalValues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate), "MR", F, T, scales_EA)
Plot_STATISTICS(data, FinalValues, "EA", c(Var_Revenue, Var_Macro, Var_Expenditure), "Final", "GRate", c(1), as.Date("2020-01-01"), as.Date("2006-01-01"), scales_EA)

Data_STATISTICS <- function(sample, Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate){
  
  Final_values <- Finalvalues[(Country_code %in% Countries) & 
                (Variable_code %in% Items) & 
                (Measure == MeasureUsed) & 
                (Date %between% c(LowDate, UpDate)), 
              .(SDF = sd(Value, na.rm = TRUE)), 
              by = Variable_long]
  
  sample <- sample[(Country_code %in% Countries) & 
           (Variable_code %in% Items) & 
           (Measure == MeasureUsed) & 
           (Revision_nb %in% RevisionNumber) & 
           (Type_revision %in% TypeOfRevision) & 
           (Date %between% c(LowDate, UpDate)), 
         .(SD = sd(Value, na.rm = TRUE),
           MR = mean(Value, na.rm = TRUE),
           N = sum(!is.na(Value)),
           MAR = mean(abs(Value), na.rm = TRUE),
           RMSR = sqrt(mean(Value^2, na.rm = TRUE)),
           MIN = min(Value, na.rm = TRUE),
           MAX = max(Value, na.rm = TRUE)
           ),
         by = .(Variable_long, Group)] %>%
    merge.data.table(Final_values)
  
  sample[, N2S := SD/SDF]
  sample <- melt(sample, id.vars = c("Variable_long", "Group"), value.name = "Value", variable.name = "Statistic")
  return(sample[!(Statistic %in% c("SD", "SDF"))])
}
SubPlot_STATISTICS <- function(sample, Statistics, Legend, Ylabs, scales_y){
  
  
  sample <- sample %>%
    mutate(Variable_long = factor(Variable_long, levels = LevelItem2),
           Variable_long = forcats::fct_rev(Variable_long),
           Group = factor(Group, levels = c("Revenue",
                                            "Expenditure",
                                            "Macro")),
           Statistic = factor(Statistic ,levels = c("N",
                                                    "MR",
                                                    "MIN",
                                                    "MAX",
                                                    "MAR",
                                                    "RMSR",
                                                    "N2S")))
  
  temp <- sample %>% 
    subset(Statistic == "N")%>% 
    arrange(Variable_long)
  
  
  NewTitle <- paste0(temp$Variable_long, "\n(", temp$Value, ")")
  names(NewTitle) <- temp$Variable_long  
  sample <- sample %>%
    subset(Statistic != "N") %>%
    mutate(Variable_long = recode(Variable_long, !!!NewTitle), 
           Variable_long = factor(Variable_long, levels = NewTitle))
  
  plot <- ggplot(data=subset(sample, Statistic == Statistics)) +
    ggtitle(Statistics) +
    
    #makes the bar and format 
    geom_bar(aes(x=Variable_long ,y= Value, fill=Group),stat="identity",position="dodge",width=0.8)+
    geom_hline(yintercept = 0) +
    coord_flip()+
    
    #Add labels
    geom_text(aes(x=Variable_long,y=Value, hjust =  ifelse(Value > 0, -0.2, 1.15), label = round(Value,2)), size = 3)+
    
    #set general theme
    theme_ECB()+
    theme(plot.title=element_text(size= 9, face = "plain", colour = "black"))+
    
    {if (rlang::is_empty(scales_y))
      scale_y_continuous(expand=c(1,0))
    }+
    
    {if (!rlang::is_empty(scales_y$Scale))
      scales_y$Scale[[Statistics]]
    }+
    
    {if (!rlang::is_empty(scales_y$Expand))
      scales_y$Expand[[Statistics]]
    }+
    
    {if (!Legend)
      theme(legend.position="none")
    }+
    {if (!Ylabs)
      theme(axis.text.y=element_blank(),
            axis.ticks.y = element_blank()
      )
    }+
    #set colors and name of data
    scale_fill_manual("",values=ECB_col) 
  
  return(plot)
}
Plot_STATISTICS <- function(sample,Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate, scales_y){
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
  
  prow <- plot_grid(Plot1 + theme(plot.margin = unit(c(0,-2.5  ,0,0.2), "cm")) ,
                    Plot2 + theme(plot.margin = unit(c(0,-2,0,2.5), "cm")),
                    Plot3 + theme(plot.margin = unit(c(0,-1.5,0,2), "cm")),
                    Plot4 + theme(plot.margin = unit(c(0,-1,0,1.5), "cm")),
                    Plot5 + theme(plot.margin = unit(c(0,-0.5,0,1), "cm")),
                    Plot6 + theme(plot.margin = unit(c(0,0,0,0.5), "cm")),
                    align = "h", ncol = 6, vjust = -0.8)
  
  
  
  plot <- plot_grid(legend,
                    prow + theme(plot.margin = unit(c(-0.5, 0, 0, 0), "cm")), 
                    ncol = 1, rel_heights = c(0.1, 1))
  
  return(plot)
}


# Big 9
scales_Big9 <- list("Scale" = 
                      list(
                        "MR" = scale_y_continuous(breaks = seq(-2, 2, 2)),
                        "MIN" = scale_y_continuous(limits = c(NA,0),breaks = seq(-300, 0, 100)),
                        "MAX" = scale_y_continuous(limits = c(0,NA),breaks = seq(0, 149, 50)),
                        "MAR" = scale_y_continuous(limits = c(0,NA),breaks = seq(0, 8, 3)),
                        "RMSR" = scale_y_continuous(limits = c(0,NA),breaks = seq(0, 20, 8)),
                        "N2S" = scale_y_continuous(limits = c(0,NA),breaks = seq(0, 2, 0.5))
                      ),
                    "Expand" = 
                      list(
                        "MR" = expand_limits(y = c(-3,2.5)),
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

scales_Big9_Inter <- list("Scale" = 
                            list(
                              "MR" = scale_y_continuous(breaks = seq(-0.5, 0.5, 0.5)),
                              "MIN" = scale_y_continuous(limits = c(NA,0),breaks = seq(-300, 0, 150)),
                              "MAX" = scale_y_continuous(limits = c(0,NA),breaks = seq(0, 150, 75)),
                              "MAR" = scale_y_continuous(limits = c(0,NA),breaks = seq(0, 4, 2)),
                              "RMSR" = scale_y_continuous(limits = c(0,NA),breaks = seq(0, 10, 5)),
                              "N2S" = scale_y_continuous(limits = c(0,NA),breaks = seq(0, 0.6, 0.3))
                            ),
                          "Expand" = 
                            list(
                              "MR" = expand_limits(y = c(-0.9,0.7)),
                              "MIN" = expand_limits(y = -320),
                              "MAX" = expand_limits(y = 170),
                              "MAR" = expand_limits(y = 4.7),
                              "RMSR" = expand_limits(y = 13.5),
                              "N2S" = expand_limits(y = 0.9)
                            )
)

Plot_STATISTICS(data, FinalValues, c(Ctry_Agg, "REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Intermediate", "GRate", 1:5, as.Date("2013-12-31"), as.Date("2006-01-01"), scales_Big9_Inter)
Plot_STATISTICS(data, FinalValues, c(Ctry_Agg, "REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Intermediate", "GRate", 1:5, as.Date("2020-01-01"), as.Date("2013-12-31"), scales_Big9_Inter)

# Total_Rev #####
Data_TOTAL_REV <- function(sampleRev,sampleData, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber){
  sampleRev <- subset(sampleRev, Ctry %in% Countries & 
                        Item %in% Items & 
                        TypeRevision == TypeOfRevision & 
                        Measure == MeasureUsed & 
                        Revision_nb == RevisionNumber) %>% 
    mutate(Value = Value * -1, Type = "Revision") %>% 
    select(ObsDate, Value, Type)
  
  sample <- subset(sampleData, Ctry %in% Countries & 
                     Item %in% Items & 
                     Measure == MeasureUsed) %>% 
    mutate(Type = "Data") %>% 
    select(ObsDate, Value, Type)%>% 
    full_join(sampleRev)
  
  return(sample)
}
Plot_TOTAL_REV <- function(sample, Legend){
  
  plot <- ggplot() +
    ggtitle('') +
    #makes the bar and format 
    geom_bar(data=subset(sample,Type == "Revision"), aes(x=ObsDate,y=Value, fill = "#003299"),stat="identity") +
    geom_line(data=subset(sample,Type == "Data"), aes(x=ObsDate,y=Value, color = "black")) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
    #set general theme
    theme_ECB()+
    theme(plot.margin = unit(c(0.15,0.3,0.15, 0.15), "cm"))+
    
    #set colors and name of data
    scale_fill_manual("",values="#003299", labels = "Final revision") +
    scale_color_manual("",values="black", labels = "Realised data") +
    {if (!Legend)
      theme(legend.position="none")
    }
  return(plot)
}

tikz(file = paste0(Desti,'Total_Rev','.tex'), width = 6.299, height = 2.75)
Plot_TOTAL_REV(Data_TOTAL_REV(Dataset,FinalValues, c("NL"), c("COE"), "Final", "GRate", 1), T)
dev.off()

# Total_Rev_Decomposition #####
Data_TOTAL_REV_DECOMP <- function(sampleRev,sampleData, Countries, Items, TypeOfRevision, MeasureUsed){
  sampleRev <- subset(sampleRev, Ctry %in% Countries & 
                        Item %in% Items & 
                        TypeRevision == TypeOfRevision & 
                        Measure == MeasureUsed) %>% 
    mutate(Value = Value * -1, Revision_nb = as.character(Revision_nb), Type = "Revision") %>%  
    select(ObsDate, Value, Revision_nb, Type)
  
  sample <- subset(sampleData, Ctry %in% Countries & 
                     Item %in% Items & 
                     Measure == MeasureUsed) %>% 
    mutate(Type = "Data") %>% 
    select(ObsDate, Value, Type)%>% 
    full_join(sampleRev)
  return(sample)
}
Plot_TOTAL_REV_DECOMP <- function(sample, Legend){
  
  ###### Defining factors
  
  sample$Revision_nb <- factor(sample$Revision_nb ,levels = c("1",
                                                            "2",
                                                            "3",
                                                            "4",
                                                            "5"))
  
  SumData <-sample %>%
    subset(Type == "Revision")%>%
    group_by(ObsDate)%>%
    summarise(
      Sum = sum(Value)
    )%>%
    mutate(FinalRev = "Final revision")
  ##### Plotting
  plot <- ggplot() +
    ggtitle('') +
    
    #makes the bar and format 
    geom_bar(data=subset(sample,Type == "Revision"), aes(x=ObsDate,y=Value, fill = Revision_nb),stat="identity")+
    geom_point(data = SumData, aes(x=ObsDate,y=Sum, color = FinalRev), shape  = "-",  size = 2, stroke = 7)+
    geom_line(data=subset(sample,Type == "Data"), aes(x=ObsDate,y=Value,linetype = Type)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
    #set general theme
    theme_ECB()+
    theme(legend.title = element_text(size = 10),
          legend.box="vertical", 
          legend.margin=margin(),
          legend.direction = "horizontal")+
    #set colors and name of data
    scale_fill_manual("",values=ECB_col, labels = c("1st","2nd","3rd","4th","5th"))+
    scale_color_manual("",values="black", labels = c("")) +
    scale_linetype_manual("",values="solid", labels = c("")) +
    guides(fill     = guide_legend(title="Interm. revision:", nrow = 2, byrow = T),
           color    = guide_legend(title="Final revision:"),
           linetype = guide_legend(title="Realised data:"))+
    
    {if (!Legend)
      theme(legend.position="none")
    }
  return(plot)
}

tikz(file = paste0(Desti,'Total_Rev_Decomposition','.tex'), width = 6.299, height = 2.75)
Plot_TOTAL_REV_DECOMP(Data_TOTAL_REV_DECOMP(Dataset,FinalValues, c("NL"), c("COE"), "Single", "GRate"), T)
dev.off()

# Total_Revision #####
Plot_TOTAL_REVISION <- function(sampleRevi,sampleDataa){
  Plot1 <- Plot_TOTAL_REV(Data_TOTAL_REV(sampleRevi,sampleDataa, c("NL"), c("COE"), "Final", "GRate", 1), F)
  Plot2 <- Plot_TOTAL_REV_DECOMP(Data_TOTAL_REV_DECOMP(sampleRevi,sampleDataa, c("NL"), c("COE"), "Single", "GRate"), F)
  
  legend1 <- get_legend(
    Plot_TOTAL_REV(Data_TOTAL_REV(sampleRevi,sampleDataa, c("NL"), c("COE"), "Final", "GRate", 1), T)+ theme(legend.box.margin = margin(0, 0, 0, 0))
  ) 
  
  legend2 <- get_legend(
    Plot_TOTAL_REV_DECOMP(Data_TOTAL_REV_DECOMP(sampleRevi,sampleDataa, c("NL"), c("COE"), "Single", "GRate"), T) + theme(legend.box.margin = margin(0, 0, 0, 0))
  )
  
  legends <- plot_grid(legend1,
                       legend2,
                       align = "v", ncol = 2, vjust = -0.8)
  
  prow <- plot_grid(Plot1 + theme(plot.margin = unit(c(0,0,0,0), "cm")) ,
                    Plot2 + theme(plot.margin = unit(c(0,0,0,0), "cm")),
                    align = "v", ncol = 2, vjust = -0.8)
  
  plot <- plot_grid(legends + theme(plot.margin = unit(c(1.4, 0, -0.1, 0), "cm")),
                    prow + theme(plot.margin = unit(c(1.2, 0.2, 0, 0), "cm")), 
                    ncol = 1, rel_heights = c(0.1, 1))
  
  return(plot)
}
tikz(file = paste0(Desti,'Total_Revision','.tex'), width = 6.299, height = 3.5)
Plot_TOTAL_REVISION(Dataset, FinalValues)
dev.off()

# Single_Rev_Example_FR #####
DATA_Single_Rev_Example <- function(sample, Countries, Items, TypeOfRevision, MeasureUsed,UpDate, LowDate){
  sample <- subset(sample, Ctry %in% Countries & 
                     Item %in% Items & 
                     TypeRevision == TypeOfRevision & 
                     Measure == MeasureUsed &
                     ObsDate < UpDate &
                     ObsDate > LowDate) %>% 
    mutate(Value = Value * -1, ObsQ = paste0("2018 Q",as.character(ObsQ)), RevisionPlace = ifelse(RevisionPlace== 1 , "Jul 2018", 
                                                                                                  ifelse(RevisionPlace== 2 , "Oct 2018",
                                                                                                         ifelse(RevisionPlace== 3 , "Jan 2019",
                                                                                                                ifelse(RevisionPlace== 4 , "Apr 2019",
                                                                                                                       ifelse(RevisionPlace== 5 , "Jul 2019", "Oct 2019"))))))   %>% 
    select(RevisionPlace, Value, ObsQ)
  
  return(sample)
}
Plot_Single_Rev_Example <- function(sample){
  sample$RevisionPlace <- factor(sample$RevisionPlace ,levels = c( "Jul 2018","Oct 2018","Jan 2019", 
                                                                   "Apr 2019", "Jul 2019", "Oct 2019"))
  
  ##### Plotting
  plot <- ggplot(data=sample, aes(x=RevisionPlace,y=Value, color = ObsQ, group =ObsQ)) +
    ggtitle('') +
    
    geom_line(stat='summary', fun=sum, size = 1.25)+
    stat_summary(fun=sum, geom="line")+
    #set general theme
    theme_ECB()+
    
    #set colors and name of data
    scale_color_manual(values=ECB_col) 
  return(plot)
}

tikz(file = paste0(Desti,'Single_Rev_Example_FR','.tex'), width = 6.299, height = 2.75)
Plot_Single_Rev_Example(DATA_Single_Rev_Example(Dataset, c("FR"), c("INC"), "Final", "PotGDP",as.Date("2019-01-01"), as.Date("2018-01-01")))
dev.off()

# 2-Dimension charts #####
Data_2D_CHART <- function(sample, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber){
  sample <- subset(sample, Ctry %in% Countries & 
                     Item %in% Items & 
                     TypeRevision == TypeOfRevision & 
                     Measure == MeasureUsed & 
                     Revision_nb == RevisionNumber)
  sample <- sample %>% group_by(Item2, Ctry, Group)%>% 
    summarise(
      Value = mean(abs(Value), na.rm = T),
      .groups = 'keep'
    )
  return(sample)
}
Plot_2D_CHART <- function(sample, Label,vLine){
  
  ###### Defining factors
  sample <- sample %>%
    mutate(Item2 = factor(Item2, levels = LevelItem2))
  
  sample$Item2 <- fct_rev(sample$Item2)
  
  sample$Group <- factor(sample$Group ,levels = c("Revenue",
                                                  "Expenditure",
                                                  "Macro"))
  
  sample$Ctry <- factor(sample$Ctry ,levels = c("Overall", Ctry_ALL))
  
  ##### Plotting
  
  plot <- ggplot(data=sample, aes(x=Ctry ,y=Item2 , color = Group)) +
    ggtitle('') +
    
    #makes the bar and format 
    geom_point(aes(size=Value), shape = 16)+
    {if (Label)
      geom_text(aes(label=round(Value,2)), vjust=0, size = 2, nudge_y = 0.35, show.legend = F)
      
    }+
    geom_vline(xintercept = vLine[1], color = rgb(83, 83, 83, maxColorValue = 255))+
    geom_vline(xintercept = vLine[2], color = rgb(83, 83, 83, maxColorValue = 255))+
    #set general theme
    theme_ECB()+
    
    #set colors and name of data
    scale_color_manual("",values=ECB_col) +
    scale_size(guide = 'none')
  
  
  return(plot)
}

# Big 6 +EA
tikz(file = paste0(Desti,'2D_GR_Final_BIG6','.tex'), width = 6.299, height = 4.75)
Plot_2D_CHART(Data_2D_CHART(Dataset, c(Ctry_Agg, "EA","REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Final","GRate", 1), T,c(0,0))
dev.off()

# EA19 
tikz(file = paste0(Desti,'2D_GR_Final_EA19','.tex'),width = 6.299, height = 4.75)
Plot_2D_CHART(Data_2D_CHART(Dataset, setdiff(Ctry_EA19, "EA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Final","GRate", 1), F,c(0,0))
dev.off()

# Big 9 + EA + EAG
tikz(file = paste0(Desti,'2D_GR_Final_BIG6','.tex'), width = 6.299, height = 4.75)
x <- subset(Dataset, Ctry %in% c(Ctry_Agg, "REA") & 
              Item %in% c(Var_Revenue, Var_Macro, Var_Expenditure) & 
              TypeRevision == "Final" & 
              Measure == "GRate" & 
              Revision_nb == 1)%>% 
  group_by(Item2, Group)%>% 
  summarise(
    Value = mean(abs(Value), na.rm = T),
  )%>% 
  mutate(Ctry = "Overall") %>% 
  rbind(Data_2D_CHART(Dataset, c(Ctry_Agg, "EA","REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Final","GRate", 1))
Plot_2D_CHART(x, T, c(1.5,11.5))
dev.off()

# Big 9 + EA + EAG LEVEL
tikz(file = paste0(Desti,'2D_LVL_Final_BIG6','.tex'), width = 6.299, height = 4.75)
x <- subset(Dataset, Ctry %in% c(Ctry_Agg, "REA") & 
              Item %in% c(Var_Revenue, "GCN", Var_Expenditure) & 
              TypeRevision == "Final" & 
              Measure == "PotGDP" & 
              Revision_nb == 1)%>% 
  group_by(Item2, Group)%>% 
  summarise(
    Value = mean(abs(Value), na.rm = T),
  )%>% 
  mutate(Ctry = "Overall") %>% 
  rbind(Data_2D_CHART(Dataset, c(Ctry_Agg, "EA","REA"), c(Var_Revenue, "GCN", Var_Expenditure), "Final","PotGDP", 1))
Plot_2D_CHART(x, T, c(1.5,11.5))
dev.off()

# Budg_Bal_QRT #####
Data_Budg_Bal_QRT <- function(sample, Countries, Items, TypeOfRevision, MeasureUsed, Revisionplace){
  sample <- subset(sample, Ctry %in% Countries & 
                     Item %in% Items & 
                     TypeRevision == TypeOfRevision & 
                     Measure == MeasureUsed & 
                     RevisionPlace == Revisionplace)
  
  sampleCtry <- sample %>% 
    group_by(Ctry, ObsQ)%>% 
    summarise(
      Value = mean(Value * -1, na.rm = T),
    )
  
  sample <-  sample %>% 
    subset(Ctry %in% setdiff(Countries, "EA")) %>%
    group_by(ObsQ)%>% 
    summarise(
      Value = mean(Value * -1, na.rm = T),
    )%>%
    mutate(Ctry = "Overall")%>%
    full_join(sampleCtry)%>%
    mutate(ObsQ = paste0("Q", ObsQ))
  
  return(sample)
}
Plot_Budg_Bal_QRT1 <- function(sample, Legend){
  ###### Defining factors
  factors <- sample%>%
    group_by(Ctry) %>%
    summarise(
      SUM = sum(Value)
    ) %>%
    arrange(SUM)
  
  sample$Ctry <- factor(sample$Ctry ,levels = factors$Ctry)
  
  sample$Ctry <- fct_rev(sample$Ctry)
  
  sample_point <- sample%>%
    group_by(Ctry) %>%
    select(Ctry, Value)%>%
    summarize_all(list(sum = sum))
  
  
  ##### Plotting
  plot <- ggplot(data=sample) +
    ggtitle('') +
    
    #makes the bar and format 
    geom_bar(aes(x=Ctry,y=Value, fill = ObsQ),stat="identity",width=0.8)+
    geom_point(data = sample_point, aes(x=Ctry,y=sum),shape  = 16, color = 'black')+
    geom_hline(yintercept = 0) +
    coord_flip()+
    
    #set general theme
    theme_ECB()+
    
    #set colors and name of data
    {if (!Legend)
      theme(legend.position="none")
    }+
    {if (Legend)
      guides(fill=guide_legend(title="Quarter :"))
    }+
    scale_fill_manual("",values=ECB_col)
  
  
  return(plot)
}
Plot_Budg_Bal_QRT2 <- function(agg){
  
  plot <- ggplot(data=agg) +
    ggtitle('') +
    
    #makes the bar and format 
    geom_bar(aes(x=Ctry,y=Value, fill = ObsQ),stat="identity",width=0.8)+
    geom_point(aes(x=Ctry,y=sum(Value)),shape  = 16, color = 'black')+
    geom_hline(yintercept = 0) +
    coord_flip()+
    
    #set general theme
    theme_ECB()+
    
    #set colors and name of data
    scale_fill_manual("",values=ECB_col)+
    theme(legend.position="none")
  
  
  return(plot)
}
Plot_Budg_Bal_QRT <- function(sample){
  Plot1 <- Plot_Budg_Bal_QRT1(subset(sample , Ctry != "Overall"), F)
  Plot2 <- Plot_Budg_Bal_QRT2(subset(sample , Ctry == "Overall"))
  
  
  legend <- get_legend(
    # create some space to the left of the legend
    Plot_Budg_Bal_QRT1(subset(sample , Ctry != "Overall"), T) + theme(legend.box.margin = margin(0, 0, 0, 0))
  )
  
  prow <- plot_grid(Plot1 + theme(plot.margin = unit(c(0, 0.2, -4.5, 0.1), "cm")) ,
                    Plot2 + theme(plot.margin = unit(c(4.25, 0.2, 0.25, 0.1), "cm")),
                    align = "v", ncol = 1, vjust = -0.8)
  
  
  
  plot <- plot_grid(legend,
                    prow + theme(plot.margin = unit(c(-0.7, 0.2, 0, 0), "cm")), 
                    ncol = 1, rel_heights = c(0.1, 1))
  
  return(plot)
}

tikz(file = paste0(Desti,'Budg_Bal_Qrt','.tex'), width = 6.299, height = 5)
Plot_Budg_Bal_QRT(Data_Budg_Bal_QRT(Dataset, c(Ctry_Agg, "EA","REA"), c("SED"), "Final", "PotGDP", 4))
dev.off()

# Budg_Bal_CATEG #####
Data_Budg_Bal_CATEG1 <- function(sample, Countries, Items, TypeOfRevision, MeasureUsed, Revisionplace){
  sample <- subset(sample, Ctry %in% Countries & 
                     Item %in% Items & 
                     TypeRevision == TypeOfRevision & 
                     Measure == MeasureUsed & 
                     RevisionPlace == Revisionplace)%>% 
    group_by(Ctry, Item2)%>% 
    summarise(
      Value = mean(Value * -1, na.rm = T),
    )
  
  return(sample)
}
Data_Budg_Bal_CATEG2 <- function(sample, Countries, Items, TypeOfRevision, MeasureUsed, Revisionplace){
  sample <- subset(sample, Ctry %in% Countries & 
                     Item %in% Items & 
                     TypeRevision == TypeOfRevision & 
                     Measure == MeasureUsed & 
                     RevisionPlace == Revisionplace) %>%
    group_by(Item2, ObsQ, Group) %>% 
    summarise(
      MR = mean(Value * -1, na.rm = T),
    ) %>% 
    group_by(Item2, Group) %>% 
    summarise(
      Value = sum(MR, na.rm = T),
    ) %>% 
    mutate(Item2 = ifelse(Group== "Others", "Budget Balance", Item2),
           Group = ifelse(Group== "Others", "Budget Balance", Group))
  
  return(sample)
}
Plot_Budg_Bal_CATEG1 <- function(sample){
  
  factors <- sample%>%
    subset(Item2 == "Others")%>%
    arrange(Value)
  
  sample <- sample%>%
    mutate(Item2 = factor(Item2, levels = c("Total revenue",
                                            "Total expenditure",
                                            "Others")),
           Ctry = factor(Ctry ,levels = factors$Ctry),
           Ctry = fct_rev(Ctry)
    )
  ##### Plotting
  plot <- ggplot(data=sample) +
    ggtitle('') +
    
    #makes the bar and format 
    geom_bar(aes(x=Ctry,y=Value, fill = Item2),stat="identity", position = 'dodge', width=0.8)+
    geom_hline(yintercept = 0) +
    coord_flip()+
    
    #set general theme
    theme_ECB()+
    
    #set colors and name of data
    scale_fill_manual("",values=ECB_col)+
    #guides(fill=guide_legend(reverse = TRUE))
    theme(legend.position="none")
  
  return(plot)
}
Plot_Budg_Bal_CATEG2 <- function(sample, Legend){
  
  ###### Defining factors
  sample$Group <- factor(sample$Group ,levels = c("Revenue",
                                                  "Expenditure",
                                                  "Budget Balance"))
  
  sample$Item2 <- factor(sample$Item2 ,levels = c(LevelItem2, "Budget Balance"))
  sample$Item2 <- fct_rev(sample$Item2)
  
  ##### Plotting
  plot <- ggplot(data=sample) +
    ggtitle('') +
    
    #makes the bar and format 
    geom_bar(aes(x=Item2,y=Value, fill = Group),stat="identity", position = 'dodge', width=0.8)+
    geom_hline(yintercept = 0) +
    coord_flip()+
    
    #set general theme
    theme_ECB()+
    
    #set colors and name of data
    scale_fill_manual("",values=ECB_col)+
    {if (!Legend)
      theme(legend.position="none")
    }
  
  return(plot)
}
Plot_Budg_Bal_CATEG <- function(sample1, sample2){
  Plot1 <- Plot_Budg_Bal_CATEG1(sample1)
  Plot2 <- Plot_Budg_Bal_CATEG2(sample2, F)
  
  
  legend <- get_legend(
    # create some space to the left of the legend
    Plot_Budg_Bal_CATEG2(sample2, T) + theme(legend.box.margin = margin(-15, 0, 0, 0))
  )
  
  prow <- plot_grid(Plot1 + theme(plot.margin = unit(c(-0.1, 0.2, -0.4, 0.1), "cm")) ,
                    Plot2 + theme(plot.margin = unit(c(0, 0.2, 0.25, 0.1), "cm")),
                    align = "v", ncol = 1, vjust = -0.8)
  
  
  
  plot <- plot_grid(legend,
                    prow + theme(plot.margin = unit(c(-1, 0, 0, 0), "cm")), 
                    ncol = 1, rel_heights = c(0.1, 1))
  
  return(plot)
}

tikz(file = paste0(Desti,'Budg_Bal_Categ','.tex'), width = 6.299, height = 5.75)
Plot_Budg_Bal_CATEG(Data_Budg_Bal_CATEG1(Dataset, c(Ctry_Agg, "EA","REA"), c("SED", "TOR", "TOE"), "Final", "PotGDP", 4),
                    Data_Budg_Bal_CATEG2(Dataset, c(Ctry_Agg, "REA"), c("SED", setdiff(Var_Expenditure, "TOE"), setdiff(Var_Revenue, "TOR")), "Final", "PotGDP", 4))
dev.off()

# PATHS #####
Data_PATHS <- function(sample, Countries, Items, TypeOfRevision, MeasureUsed, UpDate, LowDate){
  sample <- subset(sample, Ctry %in% Countries & 
                     Item  %in% Items & 
                     TypeRevision == TypeOfRevision & 
                     Measure ==MeasureUsed &
                     ObsDate < UpDate &
                     ObsDate > LowDate) %>% 
    group_by(Item, RevisionPlace, ObsQ, Group)%>% 
    summarise(
      MAR = mean(abs(Value), na.rm = T)
    ) %>% 
    group_by(Item, ObsQ, Group)%>% 
    summarise(
      RevisionPlace = RevisionPlace,
      MAR = MAR,
      Benchmark = MAR/max(MAR)
    )%>% 
    mutate(NotSelected = ifelse(!(Item %in% c("COE", "DTX", "PUR", "SCT", "YEN")) , "Others", Item),
           RevisionPlace = ifelse(RevisionPlace== 1 , "Jul (T)", 
                                  ifelse(RevisionPlace== 2 , "Oct (T)",
                                         ifelse(RevisionPlace== 3 , "Jan (T+1)",
                                                ifelse(RevisionPlace== 4 , "Apr (T+1)",
                                                       ifelse(RevisionPlace== 5 , "Jul (T+1)", "Oct (T+1)"))))))
  
  return(sample)
}
Plot_PATHS1 <- function(sample, Typevalue, Legend,Q){
  
  if (Typevalue == "Raw"){
    colnames(sample) <- c('Item',"ObsQ","Group","Revision","value", "Normalised", "NotSelected")
  }else{
    colnames(sample) <- c('Item',"ObsQ","Group", "Revision","Raw", "value", "NotSelected")
  }
  
  sample <- sample %>%
    subset(ObsQ == Q) %>%
    mutate(Revision = factor(Revision, level = c("Jul (T)","Oct (T)","Jan (T+1)", "Apr (T+1)","Jul (T+1)", "Oct (T+1)")),
           Group = factor(Group, levels = c("Revenue",
                                            "Expenditure",
                                            "Macro",
                                            "Others"))) 
  
  plot <- ggplot(data=sample, aes(x=Revision ,y= value, color=Group, group = Item)) +
    
    {if (Typevalue == "Raw")
      ggtitle(paste0("Q", Q, " (T)"))}+
    
    {if (Typevalue == "Normalised")
      ggtitle('') }+
    
    #makes the bar and format 
    geom_line(stat='summary', fun=sum)+
    stat_summary(fun=sum, geom="line")+
    theme_ECB()+
    theme(axis.text.x = element_text(size = 6),
          plot.title=element_text(size=10),
    )+
    
    {if (Q == 1| Q ==3)
      theme(axis.title.y=element_text(size = 10, angle = 90, color = rgb(83, 83, 83, maxColorValue = 255)))}+
    
    {if (Typevalue == "Normalised")
      
      if(Q == 1| Q ==3) {
        scale_y_continuous(Typevalue,labels=latex_percent) }
      else{
        scale_y_continuous("",labels=latex_percent) }
    }+  
    
    {if (Typevalue == "Raw")
      if(Q == 1| Q ==3) {
        ylab(Typevalue)}
    }+  
    
    {if (Typevalue == "Raw")
      theme(axis.text.x=element_blank())
    }+
    {if (!Legend)
      theme(legend.position="none")
    }+
    
    {if (Legend)
      guides(color = guide_legend(nrow = 1))
    }+
    
    #set colors and name of data
    
    scale_color_manual("",values=ECB_col)
  
  #plot <- last_plot() + aes(group=rev(Item))
  
  return(plot)
}
Plot_PATHS <- function(sample){
  
  
  plotQ1B <- Plot_PATHS1(sample, "Normalised", F,1)
  plotQ1A <- Plot_PATHS1(sample, "Raw", F,1)
  
  plotQ2B <- Plot_PATHS1(sample, "Normalised", F,2)
  plotQ2A <- Plot_PATHS1(sample, "Raw", F,2)
  
  plotQ3B <- Plot_PATHS1(sample, "Normalised", F,3)
  plotQ3A <- Plot_PATHS1(sample, "Raw", F,3)
  
  plotQ4B <- Plot_PATHS1(sample, "Normalised", F,4)
  plotQ4A <- Plot_PATHS1(sample, "Raw", F,4)
  
  
  prow <- plot_grid(plotQ1A + theme(plot.margin = unit(c(0, 0, 0.05, 0), "cm")) ,plotQ2A + theme(plot.margin = unit(c(0, 0.2, 0.05, -0.2), "cm")),
                    plotQ1B + theme(plot.margin = unit(c(-0.15, 0, 0, 0), "cm")) ,plotQ2B + theme(plot.margin = unit(c(-0.15, 0.2, 0, -0.2), "cm")),
                    plotQ3A + theme(plot.margin = unit(c(0, 0, 0.05, 0), "cm")) ,plotQ4A + theme(plot.margin = unit(c(0, 0.2, 0.05,-0.2), "cm")),
                    plotQ3B + theme(plot.margin = unit(c(-0.15, 0, 0, 0), "cm")) ,plotQ4B + theme(plot.margin = unit(c(-0.15, 0.2, 0, -0.2), "cm")),
                    align = "v", ncol = 2, vjust = -0.8)
  
  prow
  
  legend <- get_legend(
    # create some space to the left of the legend
    Plot_PATHS1(sample, "Normalised", T,1) + theme(legend.box.margin = margin(-18, 0, 0, 0))
  )
  
  plot <- plot_grid(legend,
                    prow + theme(plot.margin = unit(c(-0.75, 0, 0, 0), "cm")), 
                    ncol = 1, rel_heights = c(.1, 1))
  return(plot)
  
}

tikz(file = paste0(Desti,'Single_Rev_Path','.tex'), width = 6.299, height = 5.75)
Plot_PATHS(Data_PATHS(Dataset, Ctry_Agg, c(Var_Expenditure,Var_Revenue, Var_Macro), "Final", "GRate", as.Date("2020-01-01"), as.Date("2006-01-01")))
dev.off()

tikz(file = paste0(Desti,'Single_Rev_Path_1Part','.tex'), width = 6.299, height = 5.75)
Plot_PATHS(Data_PATHS(Dataset, Ctry_Agg, c(Var_Expenditure,Var_Revenue, Var_Macro), "Final", "GRate", as.Date("2014-03-01"), as.Date("2006-01-01")))
dev.off()

tikz(file = paste0(Desti,'Single_Rev_Path_2Part','.tex'), width = 6.299, height = 5.75)
Plot_PATHS(Data_PATHS(Dataset, Ctry_Agg, c(Var_Expenditure,Var_Revenue, Var_Macro), "Final", "GRate", as.Date("2020-01-01"), as.Date("2014-03-01")))
dev.off()

sample <- subset(Dataset, Ctry %in% Ctry_EA19 & 
                   Item  %in% setdiff(c(Var_Expenditure,Var_Revenue), c("TOE", "TOR")) & 
                   TypeRevision == "Final" & 
                   Measure == "PotGDP" & 
                   ObsQ == 3)

x <- sample %>% group_by(Ctry, RevisionPlace)%>% 
  filter_all(all_vars(!is.infinite(.))) %>%
  summarise(
    MAR = mean(abs(Value), na.rm = T)
  ) %>% 
  group_by(Ctry)%>% 
  summarise(
    RevisionPlace = RevisionPlace,
    MAR = MAR,
    Benchmark = MAR/max(MAR)
  )


# Total_Rev #####

Data_TYPE_REV <- function(sample,sampleFinal, Countries, Items, Vintages, MeasureUsed, UpDate, LowDate, aType){
  sample <- subset(sample, Ctry %in% Countries & 
                     Item %in% Items & 
                     Vintage %in% Vintages & 
                     Measure %in% MeasureUsed & 
                     ObsDate < UpDate &
                     ObsDate > LowDate) %>% 
    mutate(Type = aType, Vintage = ifelse(startsWith(Vintage, "W") , str_replace(Vintage, "W", "Jan 20"), 
                                          ifelse(startsWith(Vintage, "G") , str_replace(Vintage, "G", "Apr 20"),
                                                 ifelse(startsWith(Vintage, "S"), str_replace(Vintage, "S", "Jul 20"),
                                                        str_replace(Vintage, "A", "Dec 20")))))%>% 
    select(ObsDate, Vintage, Value, Type, Measure)
  
  return(sample)
}
Plot_TYPE_REV1 <- function(sample,aType, Legend){
  scaleFactor <- max(select(subset(sample,Type == aType & Measure != "GRate"), Value)) / max(select(subset(sample,Type == aType & Measure == "GRate"), Value))
  
  plot <- ggplot() +
    ggtitle('') +
    #makes the bar and format 
    geom_line(data=subset(sample,Type == aType & Measure != "GRate"), aes(x=ObsDate,y=Value, color = Vintage)) +
    geom_line(data=subset(sample,Type == aType & Measure == "GRate"), aes(x=ObsDate,y=Value*scaleFactor, color = Vintage), linetype = "dashed") +
    scale_y_continuous(sec.axis=sec_axis(~ ./scaleFactor)) +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y")+
    #set general theme
    theme_ECB()+
    theme(plot.margin = unit(c(0.15,0.3,0.15, 0.15), "cm"))+
    
    #set colors and name of data
    scale_color_manual("",values=ECB_col) +
    {if (!Legend)
      theme(legend.position="none")
    }
  return(plot)
}
Plot_TYPE_REV <- function(sample){
  Plot1 <-   Plot_TYPE_REV1(sample,"Type1", T )
  Plot2 <-   Plot_TYPE_REV1(sample,"Type2", T )
  Plot3 <-   Plot_TYPE_REV1(sample,"Type3", T )
  
  plot <- plot_grid(Plot1 + theme(plot.margin = unit(c(0,0,0,0.2), "cm")) ,
                    Plot2 + theme(plot.margin = unit(c(0,0,0,0), "cm")),
                    Plot3 + theme(plot.margin = unit(c(0,0.6,0,0), "cm")),
                    align = "h", ncol = 3, vjust = -0.8)
  return(plot)
}

sample <- full_join(full_join(Data_TYPE_REV(DatasetRaw,FinalValues, c("FR"), c("STM"), c("W18", "G18"),c("MainDb","GRate"), as.Date("2018-01-01"), as.Date("2015-11-01"), "Type1"),
                              Data_TYPE_REV(DatasetRaw,FinalValues, c("NL"), c("TIN"), c("S18", "G18"),c("MainDb","GRate"), as.Date("2018-04-01"), as.Date("2014-11-01"), "Type2")),
                    Data_TYPE_REV(DatasetRaw,FinalValues, c("ES"), c("SIN"), c("W19", "G19"),c("MainDb","GRate"), as.Date("2019-01-01"), as.Date("2016-11-01"), "Type3"))

tikz(file = paste0(Desti,'Type_Rev','.tex'), width = 6.299, height = 2.75)
Plot_TYPE_REV(sample)
dev.off()

# Total VS Intermediate ####
Data_SingleVSTotal <- function(sample, Countries, Items, MeasureUsed,TypeOfRevision, UpDate, LowDate){
  sample <- subset(Dataset, Ctry %in% Countries & 
                     Item %in% Items & 
                     Measure %in% MeasureUsed & 
                     ObsDate < UpDate &
                     ObsDate > LowDate)  %>% 
    mutate(Value = Value * -1,
           RevisionPlace = case_when(RevisionPlace == 3 ~ "January 2017",
                                     RevisionPlace == 4 ~ "April 2017",
                                     RevisionPlace == 5 ~ "July 2017",
                                     RevisionPlace == 6 ~ "October 2017") 
    )%>%
    select(RevisionPlace,Ctry, TypeRevision, Value)
  
  DataPlot <- subset(sample, TypeRevision %in% TypeOfRevision)%>%
    mutate(Type = "Plot")
  
  sample <- subset(sample, RevisionPlace == "January 2017" & TypeRevision == "Final" | TypeRevision == "Single")%>%
    group_by(TypeRevision, Ctry)%>% 
    summarise(
      Value = mean(abs(Value), na.rm = T),
    )%>%
    mutate(TypeRevision = case_when(TypeRevision == "Final" ~ "MAR (Final)",
                                    TypeRevision == "Single" ~ "MAR (Interm.)"),
           Type = "Table",
           RevisionPlace = "PLACEHOLDER")%>% 
    full_join(DataPlot)
  
  return(sample)
}
Plot_SingleVSTotal <- function(sample){
  
  table <- sample %>%
    subset(Type == "Table")%>%
    mutate(Value = round(Value,2))%>%
    spread( Ctry, Value) %>%
    select(TypeRevision, AT, FR, FI)%>%
    rename(" " = TypeRevision)
  
  
  sample <- sample %>%
    subset(Type == "Plot")%>%
    select(RevisionPlace, Ctry, Value)%>%
    rename(Date = RevisionPlace)%>%
    mutate(Ctry = factor(Ctry, levels = c("AT", "FR", "FI")),
           Date = factor(Date, levels = c("January 2017", "April 2017", "July 2017", "October 2017"))
    )
  
  ##### Creating the table #####
  
  cols <- matrix("black", nrow(table), ncol(table))
  cols[1:2,2] <- rep(ECB_col[1], 2)
  cols[1:2,3] <- rep(ECB_col[2], 2)
  cols[1:2,4] <- rep(ECB_col[3], 2)
  
  
  theme_table <- ttheme_minimal(base_size = 8,
                                core=list(fg_params = list(col = cols, fontface = matrix(c(rep(2,2), rep(1,6)), 2,4)),
                                          bg_params = list(col=NA)),
                                rowhead=list(bg_params = list(col=NA)),
                                colhead=list(bg_params = list(col=NA)))
  
  Table2plot <- tableGrob(table,rows = NULL, theme=theme_table)
  
  Table2plot <- gtable_add_grob(Table2plot,
                                grobs = segmentsGrob( # line across the bottom
                                  x0 = unit(0,"npc"),
                                  y0 = unit(0,"npc"),
                                  x1 = unit(1,"npc"),
                                  y1 = unit(0,"npc"),
                                  gp = gpar(lwd = 1.0)),
                                t = 1, b = 1, l = 1, r = ncol(Table2plot))
  #####Creating labels#####
  DATA_SET_STARTS <- sample %>% 
    group_by(Ctry) %>% 
    top_n(-1, Date) %>% 
    pull(Value)%>%
    round(2)
  
  
  data_starts <- sample %>% 
    filter(Date == "January 2017")
  
  data_starts$Value  <- round(data_starts$Value,2)
  
  
  ##### Plotting #####
  plot <- ggplot(data=sample, aes(x=Date,y=Value, color = Ctry, group =Ctry)) +
    ggtitle('') +
    
    geom_line(stat='summary', fun=sum, size = 1.25)+
    stat_summary(fun=sum, geom="line")+
    annotate("text", x = 0.85, y = data_starts$Value[data_starts$Ctry == "AT"], label = as.character(data_starts$Value[data_starts$Ctry == "AT"]), color = ECB_col[1])+
    annotate("text", x = 0.85, y = data_starts$Value[data_starts$Ctry == "FR"], label = as.character(data_starts$Value[data_starts$Ctry == "FR"]), color = ECB_col[2])+
    annotate("text", x = 0.85, y = data_starts$Value[data_starts$Ctry == "FI"], label = as.character(data_starts$Value[data_starts$Ctry == "FI"]), color = ECB_col[3])+
    
    theme_ECB()+
    
    scale_color_manual(values=ECB_col) 
  
  
  plot <-  plot + annotation_custom(Table2plot,
                                    xmin = 3, ymin = -1.1,
                                    xmax = 4, ymax = -1.0)
  return(plot)
}

tikz(file = paste0(Desti,'SingleVSTotal','.tex'), width = 6.299, height = 2.75)
Plot_SingleVSTotal(Data_SingleVSTotal(Dataset, c("FR", "AT", "FI"), c("SCT"), c("GRate"),c("Final"), as.Date("2016-09-02"), as.Date("2016-08-31")))
dev.off()


# Volatility ####
Data_Volatility <- function(sample, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate){
  
  sample <- subset(sample, Ctry %in% Countries & 
                     Item %in% Items & 
                     TypeRevision == TypeOfRevision & 
                     Measure == MeasureUsed & 
                     Revision_nb %in% RevisionNumber &
                     ObsDate < UpDate &
                     ObsDate > LowDate) %>%
    group_by(Item, Group, ObsY)%>%
    summarise(
      SD = sd(Value*-1, na.rm = T))
  # %>%
  #   mutate(ObsY = as.Date(paste0(ObsY, "-01-01")))
  
  return(sample)
}
sample <- Data_Volatility(Dataset, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate)
SubPlot_STATISTICS <- function(sample, Statistics, Legend, Ylabs, scales_y){
  
  
  sample <- sample %>%
    mutate(Item = factor(Item, levels = LevelItem),
           Item = fct_rev(Item),
           Group = factor(Group, levels = c("Revenue",
                                            "Expenditure",
                                            "Macro"))
    )
  
  
  
  
  plot <- ggplot(data=sample) +
    ggtitle('') +
    
    #makes the bar and format 
    geom_bar(aes(x=ObsY ,y= SD, fill=Group),stat="identity",width=0.8)+
    #coord_flip()+
    facet_grid(rows = vars(Item))+
    
    #Add labels
    geom_text(aes(x=ObsY,y=SD, vjust =  -0.5, label = round(SD,2)), size = 3)+
    expand_limits(y = 40)+
    #set general theme
    theme_ECB()+
    #set colors and name of data
    scale_fill_manual("",values=ECB_col) 
  
  return(plot)
}

# Ranking TOE ####
Data_Ranking <- function(sample, Items, Vintages, MeasureUsed, ObsYear){
  
  sample <- subset(sample,
                   Item %in% Items & 
                     Vintage == Vintages & 
                     Measure == MeasureUsed & 
                     ObsY == ObsYear) %>%
    group_by(Ctry)%>%
    summarise(
      Value = sum(Value)
    )%>% 
    mutate("EA" = max(Value), 
           "Share" = Value/EA)%>% 
    select(Ctry, Share)%>% 
    rename("Value" = "Share")
  
  
  return(sample)
}
Plot_Ranking <- function(sample){
  
  
  sample <- sample %>%
    subset(!(Ctry %in% c("EA", "I8")))%>%
    mutate(Ctry = factor(Ctry, levels = c("DE", "FR", "IT", "ES","NL", "BE",
                                          "AT","FI", "PT","REA", "GR", "IE","SK",
                                          "LU","SI","LT","LV", "EE", "CY", "MT")),
           Group = case_when(Ctry %in% c("GR","IE", "SK", "LU","SI","LT", "LV","EE", "CY","MT") ~ "Others",
                             Ctry == "REA" ~ "Rest of EA",
                             TRUE ~ "Big 9")
    )
  
  plot <- ggplot(data=sample) +
    ggtitle('') +
    
    #makes the bar and format 
    geom_bar(aes(x=Ctry ,y= Value, fill=Group),stat="identity",position="dodge",width=0.8)+
    
    #Add labels
    geom_text(aes(x=Ctry,y=Value, vjust =  ifelse(Value > 0, -0.2, 1.15), label = round(Value*100,2)), size = 3, color = rgb(83, 83, 83, maxColorValue = 255))+
    
    #set general theme
    theme_ECB()+
    scale_y_continuous(labels=latex_percent)+
    expand_limits(y = 0.3)+
    #set colors and name of data
    scale_fill_manual("",values=c(ECB_col[1],ECB_col[9],ECB_col[3]))
  
  return(plot)
}

tikz(file = paste0(Desti,'TOE_Ranking','.tex'), width = 6.299, height = 2.75)
Plot_Ranking(Data_Ranking(DatasetRaw,"TOE", "S21", "MainDb", 2019))
dev.off()

# Shares GDP ####
Data_Share_GDP <- function(sample,Countries, Items, Vintages, MeasureUsed){
  
  sample <- subset(sample,
                   Ctry %in% Countries &
                     Item %in% Items & 
                     Vintage == Vintages & 
                     Measure == MeasureUsed) %>%
    group_by(Item2, Group2, ToShade)%>%
    summarise(
      Value = mean(Value, rm.na = T)
    ) %>%
    ungroup()%>%
    mutate("YEN" = max(Value), "Share" = Value/YEN *100) %>%
    select(Item2, Group2, ToShade, Share)%>%
    rename("Group"= "Group2", "Value" = "Share")
  
  return(sample)
}
Plot_Share_GDP <- function(sample){
  
  
  sample <- sample %>%
    mutate(Item2 = factor(Item2, levels = LevelItem2),
           Group = factor(Group, levels = c("Revenue",
                                            "Expenditure",
                                            "Macro",
                                            "Others")))
  
  
  plot <- ggplot(data=sample) +
    ggtitle('') +
    
    #makes the bar and format 
    geom_bar(aes(x=Item2 ,y= Value/100, fill=Group, alpha = ToShade),stat="identity",position="dodge",width=0.8)+
    
    #Add labels
    geom_text(aes(x=Item2,y=Value/100, vjust =  ifelse(Value > 0, -0.2, 1.15), label = round(Value,2)), size = 3, color = rgb(83, 83, 83, maxColorValue = 255))+
    
    #set general theme
    theme_ECB()+
    theme(axis.text.x =element_text(size = 10, angle = 90))+
    scale_y_continuous(labels=latex_percent)+
    expand_limits(y = 1.02)+
    #set colors and name of data
    scale_fill_manual("",values=c(ECB_col))+
    scale_alpha_manual("", values = c(1, 0.4), guide = 'none')
  
  return(plot)
}

tikz(file = paste0(Desti,'Average_Share_GDP','.tex'), width = 6.299, height = 3.75)
Plot_Share_GDP(Data_Share_GDP(DatasetRaw,setdiff(Ctry_EA19, "EA"), c(Var_Revenue, Var_Macro, Var_Expenditure, "KTR","OCR", "OCE", "OKE", "INP"), "S21", "MainDb"))
dev.off()

# Mean and volatility ####
Data_Mean_SD <- function(sample,Countries, Items, Vintages, MeasureUsed, UpDate, LowDate){
  
  sample <- subset(sample,
                   Ctry %in% Countries &
                     Item %in% Items & 
                     Vintage == Vintages & 
                     Measure == MeasureUsed &
                     ObsDate < UpDate &
                     ObsDate > LowDate) %>%
    group_by(Item2,Group2, ToShade)%>%
    summarise(
      Mean = mean(Value, na.rm = T),
      SD = sd(Value, na.rm = T)
    ) %>%
    rename("Group"= "Group2") %>%
    melt(id.vars = c("Item2", "Group", "ToShade"), value.name = "Value", variable.name = "Statistic")%>%
    
    return(sample)
}
SubPlot_Mean_SD <- function(sample, Statistics, Legend, Ylabs, scales_y){
  
  
  sample <- sample %>%
    mutate(Item2 = factor(Item2, levels = LevelItem2),
           Item2 = fct_rev(Item2),
           Group = factor(Group, levels = c("Revenue",
                                            "Expenditure",
                                            "Macro",
                                            "Others")),
           Statistic = factor(Statistic ,levels = c("Mean",
                                                    "SD",
                                                    "Share")))
  
  
  plot <- ggplot(data=subset(sample, Statistic == Statistics)) +
    ggtitle(ifelse(Statistics== "SD", "Standard deviation", Statistics)) +
    
    #makes the bar and format 
    geom_bar(aes(x=Item2 ,y= Value, fill=Group, alpha=ToShade),stat="identity",position="dodge",width=0.8)+
    geom_hline(yintercept = 0) +
    coord_flip()+
    
    #Add labels
    {if (Statistics == "Share")
      geom_text(aes(x=Item2,y=Value, hjust =  ifelse(Value > 0, -0.2, 1.15), label = round(Value*100,2)), size = 3)
    }+
    
    {if (Statistics != "Share")
      geom_text(aes(x=Item2,y=Value, hjust =  ifelse(Value > 0, -0.2, 1.15), label = round(Value,2)), size = 3)
    }+
    
    #set general theme
    theme_ECB()+
    theme(plot.title=element_text(size= 9, face = "plain", colour = "black"))+
    
    {if (is_empty(scales_y))
      scale_y_continuous(expand=c(1,0))
    }+
    
    {if (!is_empty(scales_y$Scale))
      scales_y$Scale[[Statistics]]
    }+
    
    {if (!is_empty(scales_y$Expand))
      scales_y$Expand[[Statistics]]
    }+
    
    {if (!Legend)
      theme(legend.position="none")
    }+
    {if (!Ylabs)
      theme(axis.text.y=element_blank(),
            axis.ticks.y = element_blank()
      )
    }+
    #set colors and name of data
    scale_fill_manual("",values=ECB_col) +
    scale_alpha_manual("", values = c(1, 0.4), guide = 'none')
  
  return(plot)
}
Plot_Mean_SD <- function(sample,Countries, Items, Vintages, MeasureUsed, UpDate, LowDate, scales_y){
  Plot1 <- SubPlot_Mean_SD(Data_Mean_SD(sample,Countries, Items, Vintages, MeasureUsed, UpDate, LowDate), "Mean", F, T, scales_y)
  Plot2 <- SubPlot_Mean_SD(Data_Mean_SD(sample,Countries, Items, Vintages, MeasureUsed, UpDate, LowDate), "SD", F, F, scales_y)
  
  legend <- get_legend(
    # create some space to the left of the legend
    SubPlot_Mean_SD(Data_Mean_SD(sample,Countries, Items, Vintages, MeasureUsed, UpDate, LowDate), "Mean", T, F, scales_y)
    + theme(legend.box.margin = margin(-15, 0, 0, 0))
  )
  
  prow <- plot_grid(Plot1 + theme(plot.margin = unit(c(0,-1.7,0,0.1), "cm")) ,
                    Plot2 + theme(plot.margin = unit(c(0,0.2,0,1.8), "cm")),
                    align = "h", ncol = 2, vjust = -0.8)
  
  
  plot <- plot_grid(legend,
                    prow + theme(plot.margin = unit(c(-0.5, 0, 0, 0.2), "cm")), 
                    ncol = 1, rel_heights = c(0.1, 1))
  
  return(plot)
}

scales_ <- list("Scale" = 
                  list(
                    "Mean" = scale_y_continuous(limits = c(NA,NA), breaks = seq(0, 6, 3)),
                    "SD" = scale_y_continuous(limits = c(0,NA), breaks = seq(0, 90, 45))
                  ),
                "Expand" = 
                  list(
                    "Mean" = expand_limits(y = c(-1,7.2)),
                    "SD" = expand_limits(y = 120)
                  )
)

tikz(file = paste0(Desti,'Mean_Volatility_Items','.tex'), width = 6.299, height = 4.75 )
Plot_Mean_SD(DatasetRaw,setdiff(Ctry_EA19, "EA"), c(Var_Revenue, Var_Macro, Var_Expenditure, "KTR","OCR", "OCE", "OKE", "INP"), "S21", "GRate", as.Date("2020-01-01"), as.Date("2006-01-01"), scales_)
dev.off()

tikz(file = paste0(Desti,'Mean_Volatility_Items_1Part','.tex'), width = 6.299, height = 4.75 )
Plot_Mean_SD(DatasetRaw,setdiff(Ctry_EA19, "EA"), c(Var_Revenue, Var_Macro, Var_Expenditure, "KTR","OCR", "OCE", "OKE", "INP"), "S21", "GRate", as.Date("2014-03-01"), as.Date("2006-01-01"), scales_)
dev.off()

tikz(file = paste0(Desti,'Mean_Volatility_Items_2Part','.tex'), width = 6.299, height = 4.75 )
Plot_Mean_SD(DatasetRaw,setdiff(Ctry_EA19, "EA"), c(Var_Revenue, Var_Macro, Var_Expenditure, "KTR","OCR", "OCE", "OKE", "INP"), "S21", "GRate",  as.Date("2020-01-01"), as.Date("2014-03-01"), scales_)
dev.off()


#########
Data_STATISTICS <- function(sample,Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate){
  Finalvalues <- subset(Finalvalues, Ctry %in% Countries & 
                          Item %in% Items & 
                          Measure == MeasureUsed &
                          ObsDate < UpDate &
                          ObsDate > LowDate) %>% 
    group_by(Item2)%>% 
    summarise(
      SDF = sd(Value, na.rm = T)
    )
  
  
  sample <- subset(sample, Ctry %in% Countries & 
                     Item %in% Items & 
                     TypeRevision == TypeOfRevision & 
                     Measure == MeasureUsed & 
                     Revision_nb %in% RevisionNumber &
                     ObsDate < UpDate &
                     ObsDate > LowDate) %>%
    group_by(Item2, Group)%>%
    summarise(
      N = sum(!is.na(Value)),
      MR = mean(Value*-1, na.rm = T),
      MAR = mean(abs(Value), na.rm = T),
      RMSR = sqrt(mean(Value^2, na.rm = T)),
      MIN = min(Value*-1, na.rm = T),
      MAX = max(Value*-1, na.rm = T),
      SD = sd(Value*-1, na.rm = T))%>%
    merge(Finalvalues)%>%
    mutate(N2S = SD/SDF)%>%
    melt(id.vars = c("Item2", "Group"), value.name = "Value", variable.name = "Statistic")
  #subset(!(Statistic %in% c("SD", "SDF")))
  
  return(sample)
}
CheckFunc <- function(sample,Finalvalues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber, UpDate, LowDate){
  sample <- Data_STATISTICS(sample,FinalValues, Countries, Items, TypeOfRevision, MeasureUsed, RevisionNumber,UpDate, LowDate)%>%
    subset(Statistic %in% c("SD", "SDF"))%>%
    mutate(Item2 = factor(Item2, levels = LevelItem2),
           #Item2 = fct_rev(Item2),
           Group = factor(Group, levels = c("Macro",
                                            "Expenditure",
                                            "Revenue",
                                            "Others")),
           Group = fct_rev(Group)
    )
  
  plot <- ggplot(data=sample) +
    ggtitle('') +
    geom_bar(aes(x=Statistic ,y= Value, fill=Group),stat="identity",position="dodge",width=0.8)+
    facet_grid(cols = vars(Item2))+
    geom_hline(yintercept = 0) +
    geom_text(aes(x=Statistic,y=Value, vjust = -0.2, label = round(Value,2)), size = 3)+
    theme_ECB()+
    scale_fill_manual("",values=ECB_col)
  
  return(plot)
}
CheckFunc(Dataset,FinalValues, c(Ctry_Agg, "REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Final", "GRate", c(1), as.Date("2020-01-01"), as.Date("2006-01-01"))
CheckFunc(Dataset,FinalValues, c(Ctry_Agg, "REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Final", "GRate", c(1), as.Date("2014-03-01"), as.Date("2006-01-01"))
CheckFunc(Dataset,FinalValues, c(Ctry_Agg, "REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Final", "GRate", c(1), as.Date("2020-01-01"), as.Date("2014-03-01"))
#######
# Presentation ####
Desti <- "/Users/thomas/Documents/Cloud/2021_4A_ENS_CESURE/ECB/qgfs_revisions/docu/Presentation/figures/"
Plot_TOTAL_REVISION_leg <- function(sampleRevi,sampleDataa){
  Plot1 <- Plot_TOTAL_REV(Data_TOTAL_REV(sampleRevi,sampleDataa, c("NL"), c("COE"), "Final", "GRate", 1), F)
  Plot2 <- Plot_TOTAL_REV_DECOMP(Data_TOTAL_REV_DECOMP(sampleRevi,sampleDataa, c("NL"), c("COE"), "Single", "GRate"), F)
  
  legend1 <- get_legend(
    Plot_TOTAL_REV(Data_TOTAL_REV(sampleRevi,sampleDataa, c("NL"), c("COE"), "Final", "GRate", 1), T)+ theme(legend.box.margin = margin(0, 0, 0, 0))
  ) 
  
  legend2 <- get_legend(
    Plot_TOTAL_REV_DECOMP(Data_TOTAL_REV_DECOMP(sampleRevi,sampleDataa, c("NL"), c("COE"), "Single", "GRate"), T) + theme(legend.box.margin = margin(0, 0, 0, 0))
  )
  
  legends <- plot_grid(legend1,
                       legend2,
                       align = "v", ncol = 2, vjust = -0.8)
  return(legends)
}
Plot_TOTAL_REVISION_noleg <- function(sampleRevi,sampleDataa){
  Plot1 <- Plot_TOTAL_REV(Data_TOTAL_REV(sampleRevi,sampleDataa, c("NL"), c("COE"), "Final", "GRate", 1), F)
  Plot2 <- Plot_TOTAL_REV_DECOMP(Data_TOTAL_REV_DECOMP(sampleRevi,sampleDataa, c("NL"), c("COE"), "Single", "GRate"), F)
  
  legend1 <- get_legend(
    Plot_TOTAL_REV(Data_TOTAL_REV(sampleRevi,sampleDataa, c("NL"), c("COE"), "Final", "GRate", 1), T)+ theme(legend.box.margin = margin(0, 0, 0, 0))
  ) 
  
  legend2 <- get_legend(
    Plot_TOTAL_REV_DECOMP(Data_TOTAL_REV_DECOMP(sampleRevi,sampleDataa, c("NL"), c("COE"), "Single", "GRate"), T) + theme(legend.box.margin = margin(0, 0, 0, 0))
  )
  
  legends <- plot_grid(legend1,
                       legend2,
                       align = "v", ncol = 2, vjust = -0.8)
  
  prow <- plot_grid(Plot1 + theme(plot.margin = unit(c(0,0,0,0), "cm")) ,
                    Plot2 + theme(plot.margin = unit(c(0,0,0,0), "cm")),
                    align = "v", ncol = 2, vjust = -0.8)
  
  plot <- plot_grid(legends + theme(plot.margin = unit(c(1.4, 0, -0.1, 0), "cm")),
                    prow + theme(plot.margin = unit(c(1.2, 0.2, 0, 0), "cm")), 
                    ncol = 1, rel_heights = c(0.1, 1))
  
  return(prow)
}
Plot_PATHS <- function(sample){
  
  
  plotQ1B <- Plot_PATHS1(sample, "Normalised", F,1)
  plotQ1A <- Plot_PATHS1(sample, "Raw", F,1)
  
  plotQ2B <- Plot_PATHS1(sample, "Normalised", F,2)
  plotQ2A <- Plot_PATHS1(sample, "Raw", F,2)
  
  plotQ3B <- Plot_PATHS1(sample, "Normalised", F,3)
  plotQ3A <- Plot_PATHS1(sample, "Raw", F,3)
  
  plotQ4B <- Plot_PATHS1(sample, "Normalised", F,4)
  plotQ4A <- Plot_PATHS1(sample, "Raw", F,4)
  
  
  prow <- plot_grid(plotQ1A + theme(plot.margin = unit(c(0, 0, 0.05, 0), "cm")) ,plotQ2A + theme(plot.margin = unit(c(0, 0.2, 0.05, -0.2), "cm")),
                    plotQ1B + theme(plot.margin = unit(c(-0.15, 0, 0, 0), "cm")) ,plotQ2B + theme(plot.margin = unit(c(-0.15, 0.2, 0, -0.2), "cm")),
                    plotQ3A + theme(plot.margin = unit(c(0, 0, 0.05, 0), "cm")) ,plotQ4A + theme(plot.margin = unit(c(0, 0.2, 0.05,-0.2), "cm")),
                    plotQ3B + theme(plot.margin = unit(c(-0.15, 0, 0, 0), "cm")) ,plotQ4B + theme(plot.margin = unit(c(-0.15, 0.2, 0, -0.2), "cm")),
                    align = "v", ncol = 2, vjust = -0.8)
  
  prow
  
  legend <- get_legend(
    # create some space to the left of the legend
    Plot_PATHS1(sample, "Normalised", T,1) + theme(legend.box.margin = margin(-18, 0, 0, 0))
  )
  
  plot <- plot_grid(legend,
                    prow + theme(plot.margin = unit(c(-0.25, 0, 0, 0), "cm")), 
                    ncol = 1, rel_heights = c(.1, 1))
  return(plot)
  
}


tikz(file = paste0(Desti,'Example_NL_legend','.tex'), width = 5, height = 2.5)
Plot_TOTAL_REVISION_leg(Dataset, FinalValues)
dev.off()

tikz(file = paste0(Desti,'Example_NL','.tex'), width = 4.5, height = 2)
Plot_TOTAL_REVISION_noleg(Dataset, FinalValues)
dev.off()

tikz(file = paste0(Desti,'Statistics_GR_Final_BIG6','.tex'), width = 5, height = 3)
Plot_STATISTICS(Dataset,FinalValues, c(Ctry_Agg, "REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Final", "GRate", c(1), as.Date("2020-01-01"), as.Date("2006-01-01"), scales_Big9)
dev.off()

tikz(file = paste0(Desti,'Statistics_GR_Final_BIG6_2Part','.tex'), width = 5, height = 3)
Plot_STATISTICS(Dataset,FinalValues, c(Ctry_Agg, "REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Final", "GRate", c(1), as.Date("2020-01-01"), as.Date("2014-03-01"), scales_Big9)
dev.off()

tikz(file = paste0(Desti,'Statistics_GR_Inter_BIG6_2Part','.tex'),width = 5, height = 3)
Plot_STATISTICS(Dataset,FinalValues, c(Ctry_Agg, "REA"), c(Var_Revenue, Var_Macro, Var_Expenditure), "Single", "GRate", 1:5, as.Date("2020-01-01"), as.Date("2014-03-01"), scales_Big9_Inter)
dev.off()

tikz(file = paste0(Desti,'Single_Rev_Path','.tex'), width = 4.7, height = 3.4)
Plot_PATHS(Data_PATHS(Dataset, Ctry_Agg, c(Var_Expenditure,Var_Revenue, Var_Macro), "Final", "GRate", as.Date("2020-01-01"), as.Date("2006-01-01")))
dev.off()

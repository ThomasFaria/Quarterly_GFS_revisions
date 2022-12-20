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
  
  SumData <- sample[(Type == "Revision"),  Sum := sum(Value), by = Date][,
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

## Customize ECB palette
temp_col <- rbind(
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
)

ECB_col <- rgb(temp_col[, 1], temp_col[, 2], temp_col[, 3], maxColorValue = 255)

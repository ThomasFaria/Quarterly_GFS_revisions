## Customize R-theme

library(ggplot2)
## Customize theme
## Need to add conditions
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

## Import statements 
library(dplyr)
library(ggplot2)

## Read in SOC data cleaned 
soc_data <- read_csv(file = "cleaned_data.csv")



## home price by region 
FSLPR_REGION <- ggplot(data = soc_data, aes(y = FSLPR, 
                                            x = as.factor(DIV), 
                                            color = as.factor(DIV))) + 
  geom_boxplot() +
  labs(y = "Sale Price of Home",
       x = "Region of Home",
       color = "Region of Home") +
  scale_color_manual(values = c(1:9), labels = c("New England",
                                                 "Middle Atlantic",
                                                 "East North Central",
                                                 "West North Central",
                                                 "South Atlantic",
                                                 "East South Central",
                                                 "West South Central",
                                                 "Mountain",
                                                 "Pacific"))

#FSLPR_REGION

## Create graph of importance scores 

library(extrafont)

loadfonts(device="win")
fonts()
impr.scr <- read.table(file = "451ImportanceScores.txt", header = TRUE, sep ="")
impr.scr$Score <- as.numeric(impr.scr$Score)
scores.graph <- ggpubr::ggdotchart(impr.scr, x = "Variable", y = "Score",
                                   color = "Type",
                                   palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
                                   main = "Guide Importance Scores",
                                   sorting = "descending",                    # Sort value in descending order
                                   add = "segments",                             # Add segments from y = 0 to dots
                                   rotate = TRUE,                                # Rotate vertically
                                   # Order by groups
                                   dot.size = 6,                                 # Large dot size
                                   label = round(impr.scr$Score),
                                   font.label = list(color = "white", size = 7,
                                                     vjust = 0.5),               # Adjust label parameters
                                   ggtheme = ggplot2::theme_bw()                         # ggplot2 theme
)

ggplot2::ggsave(
  file = "scores_graph.png",
  plot = scores.graph,
  scale = 1,
  width = 18,
  height = 24,
  units = "in",
  dpi = 300,
  limitsize = TRUE,
)
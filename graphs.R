## Import statements 
library(dplyr)
library(ggplot2)
library(readr)
library(knitr)

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

## Heating fuel as a proxy for LOTV
ggplot(data = soc_data, aes(y = LOTV, 
                            x = as.factor(FUEL), 
                            color = as.factor(FUEL))) + 
  geom_boxplot() +
  labs(y = "Lot Value",
       x = "Heating Fuel Type",
       color = "Heating Fuel Type") +
  scale_color_manual(values = c(1:6), labels = c("01 = Electricity",
"02 = Natural gas",
"03 = Bottled or liquefied 
petroleum gas (including propane)", 
"04 = Oil  
(including heating oil or kerosene)",
"05 = Other or no heat",
"00 = Not reported"))

## Heating system as proxy for overall size of the house 

ggplot(data = soc_data, aes(y = FSLPR, 
                            x = as.factor(HEAT), 
                            color = as.factor(HEAT))) + 
  geom_boxplot() +
  labs(y = "Area of Home in Square Feet",
       x = "Heating System Used",
       color = "Heating Fuel Type") +
  scale_color_manual(values = c(1:5), labels = c("1 = Heat pump (air or ground source)",
                                                 "2 = Forced air furnace (without heat pump)",
                                                 "3 = Hot water or steam system",
                                                 "4 = Other or no heat",
                                                 "0 = Not reported"))
## DECK VS Sale Price boxplots

ggplot(data = soc_data, aes(x = as.factor(DECK), y = FSLPR)) +
  geom_boxplot()


## LOT AREA vs SALE PRICE with DIV as confounding feature
my_color <- c(ggsci::pal_npg("nrc")(9), ggsci::pal_lancet("lanonc")(9))
ggplot(data = soc_data, aes(x = AREA, y = FSLPR, color = as.factor(DIV))) +
  geom_point() + labs(y = "Sale Price of Home",
                      x = "Lot Size",
                      color = "Region of Home") +
  scale_color_manual(values = my_color, labels = c("New England",
                                                 "Middle Atlantic",
                                                 "East North Central",
                                                 "West North Central",
                                                 "South Atlantic",
                                                 "East South Central",
                                                 "West South Central",
                                                 "Mountain",
                                                 "Pacific"))
## Looking at averages
soc_data %>% group_by(DIV) %>% 
  summarise(avg_price = mean(FSLPR),
            avg_area = mean(AREA)) %>% 
  ggplot(aes(x = avg_area, y = avg_price, color = as.factor(DIV), label = c("New England",
                                                                            "Middle Atlantic",
                                                                            "East North Central",
                                                                            "West North Central",
                                                                            "South Atlantic",
                                                                            "East South Central",
                                                                            "West South Central",
                                                                            "Mountain",
                                                                            "Pacific"))) + 
  geom_point() + labs(y = "Average Sale Price of Home",
                      x = "Average Lot Size",
                      color = "Region of Home") +
  scale_color_manual(values = my_color, labels = c("New England",
                                                   "Middle Atlantic",
                                                   "East North Central",
                                                   "West North Central",
                                                   "South Atlantic",
                                                   "East South Central",
                                                   "West South Central",
                                                   "Mountain",
                                                   "Pacific")) +
  ggrepel::geom_text_repel()

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
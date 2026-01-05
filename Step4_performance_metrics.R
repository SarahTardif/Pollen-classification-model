## script to calculate model performance mertrics and display them on a graph

#load packages
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(forcats)

#load data
matrixRF<-read.csv("./inputs_outputs/ConfusionMatrixRF_species_class.csv", sep=",", h=T)
matrixRFgenus<-read.csv("./inputs_outputs/ConfusionMatrixRF_genus_class.csv", sep=",", h=T)

matrixRF$Modele<-"RF"
matrixRFgenus$Modele<-"RF"

# F1-scores for each taxon
F1RF<-select(matrixRF,Prediction,F1,Modele)
F1RFgenus<-select(matrixRFgenus,Prediction,F1,Modele)

# mean F1-scores
mean(F1RF$F1)
mean(F1RFgenus$F1)

# cleaning data
matrixRF$species<-gsub("Class: ","",matrixRF$Prediction)
matrixRFgenus$species<-gsub("Class: ","",matrixRFgenus$Prediction)

# creation of the figure 
# graph for species-level model - accuracy horizontal bars and F1 scores to the right of each bar
graphsp<- ggplot(matrixRF, aes(y = forcats::fct_relevel(reorder(species, desc(species)), "OTHER", after = 0))) +
  geom_bar(aes(x = Balanced.Accuracy, fill = "Correct"), stat = "identity",alpha=0.5) +
  geom_bar(aes(x = -(1 - Balanced.Accuracy), fill = "Misclassified"),alpha=0.5, 
            stat = "identity", position = position_nudge(x = 1)) +
  scale_fill_manual(values = c("Correct" = "#332288", "Misclassified" = "#CC6677")) +
  geom_text(aes(x = 1.05, label = formatC(F1, format = "f", digits = 2)), 
            position = position_nudge(x = 0), 
            hjust = 0.2, vjust = 0.5, color = "black") +
  annotate("text", x = 1.05, y = max(as.numeric(as.factor(matrixRF$species))) + 0.8, 
         label = "F1 Score", hjust = 0, vjust = 0.1, fontface = "bold")+
  labs(x = "Accurracy",
       y = "Taxon") +
  theme_minimal()+
  scale_y_discrete(expand = expansion(mult = c(0, 0.025)))+ 
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(margin = ggplot2::margin(r = 5))) 

# graph for genus-level model - accuracy horizontal bars and F1 scores to the right of each bar
graphgenus<- ggplot(matrixRFgenus, aes(y = forcats::fct_relevel(reorder(species, desc(species)), "OTHER", after = 0))) +
  geom_bar(aes(x = Balanced.Accuracy, fill = "Correct"), stat = "identity",alpha=0.5) +
  geom_bar(aes(x = -(1 - Balanced.Accuracy), fill = "Misclassified"),alpha=0.5, 
            stat = "identity", position = position_nudge(x = 1)) +
  scale_fill_manual(values = c("Correct" = "#332288", "Misclassified" = "#CC6677")) +
  geom_text(aes(x = 1.05, label = formatC(F1, format = "f", digits = 2)), 
            position = position_nudge(x = 0), 
            hjust = 0.2, vjust = 0.5, color = "black") +
  annotate("text", x = 1.05, y = max(as.numeric(as.factor(matrixRFgenus$species)))+0.4, 
         label = "F1 Score", hjust = 0, vjust = 0.1, fontface = "bold")+
  labs(x = "Accurracy",
       y = "Taxon") +
  theme_minimal()+
  scale_y_discrete(expand = expansion(mult = c(0, 0.025)))+ 
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(margin = ggplot2::margin(r = 5))) 

graphsp <- graphsp + xlim(-0.3, 1.3)
graphgenus <- graphgenus + xlim(-0.3, 1.3)
graph<-grid.arrange(graphsp, graphgenus, ncol = 2)
ggsave("./inputs_outputs/perf_metrics_graph.png", graph, width = 10, height = 15, dpi = 300)


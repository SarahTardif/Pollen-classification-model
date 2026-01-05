## Script to create a heatmap for each confusion matrix 

# load packages
library(ggplot2)
library(tidyr)
library(dplyr)

# load data
rawmatrixRF<-read.csv("./inputs_outputs/ConfusionMatrixRF_species_V2.csv", sep=",", h=T)
#rawmatrixRF<-read.csv("./inputs_outputs/ConfusionMatrixRF_genus.csv", sep=",", h=T) # if considering genus-level model
rawmatrixRF_long <- pivot_longer(rawmatrixRF, cols = -Prediction, names_to = "Taxon", values_to = "Nombre")

# convert matrix in % 
matrix <- rawmatrixRF[, -1]
rawmatrixRF_percent <- apply(matrix, 2, function(x) {
  x <- x / sum(x) * 100
  f <- floor(x)
  f[order(x - f, decreasing = TRUE)[1:(100 - sum(f))]] <- f[order(x - f, decreasing = TRUE)[1:(100 - sum(f))]] + 1
  f
})
rawmatrixRF_percent <- cbind(Prediction = rawmatrixRF$Prediction, rawmatrixRF_percent)
rawmatrixRF_percent <-as.data.frame(rawmatrixRF_percent)
rawmatrixRF_percent_long <- pivot_longer(rawmatrixRF_percent, cols = -Prediction, names_to = "Taxon", values_to = "Nombre")
rawmatrixRF_percent_long$Nombre <- as.numeric(rawmatrixRF_percent_long$Nombre)

# categorize
rawmatrixRF_percent_long$Category <- cut(
  rawmatrixRF_percent_long$Nombre,
  breaks = c(-Inf, 1, 10, 50, 75, 100),
  labels = c("0-1", "1–10", "10–50", "50–75", "75–100"),
  include.lowest = TRUE
)

# heatmap
matgraph<-ggplot(data = rawmatrixRF_percent_long, aes(Prediction, Taxon, fill = Category)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(Nombre < 1, "", round(Nombre))), color = "white",size=4) +
  scale_fill_manual(
    values = c(
  "0-1"   = "lightgrey",   
  "1–10"  = "lightblue", 
  "10–50" = "firebrick",   
  "50–75" = "gold",       
  "75–100"= "darkgreen"  
    )) +
  labs(x = "Actual", y = "Predicted", size=20) +
  theme_minimal() +
  theme(
   axis.text.x = element_text(angle = 45, hjust = 1,size=14),
   axis.text.y = element_text(size=14),
   axis.title.x = element_text(size = 18, face = "bold"),  
   axis.title.y = element_text(size = 18, face = "bold"),   
   legend.title = element_text(size = 16, face = "bold"),   
   legend.text  = element_text(size = 14)
  ) 
matgraph
ggsave("./inputs_outputs/heatmap_species.png", matgraph, width = 25, height = 20, dpi = 300)
#ggsave("./inputs_outputs/heatmap_genus.png", matgraph, width = 25, height = 20, dpi = 300) # if considering genus-level model
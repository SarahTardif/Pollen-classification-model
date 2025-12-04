
matrixNN<-read.csv("ConfusionMatrixNN_class_species.csv", sep=",", h=T)
matrixRF<-read.csv("./inputs_outputs/ConfusionMatrixRF_species_V2_class.csv", sep=",", h=T)
matrixRFgenus<-read.csv("./inputs_outputs/ConfusionMatrixRF_genus_V2_class.csv", sep=",", h=T)
matrixXB<-read.csv("ConfusionMatrixXB_class_species.csv", sep=",", h=T)

matrixNN$Modele<-"NN"
matrixRF$Modele<-"RF"
matrixRFgenus$Modele<-"RF"
matrixXB$Modele<-"XB"
all_data <- rbind(matrixNN,matrixRF)#,matrixXB)

library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(forcats)

# comparer les F1-scores
F1NN<-select(matrixNN,Prediction,F1,Modele)
F1RF<-select(matrixRF,Prediction,F1,Modele)
F1RFgenus<-select(matrixRFgenus,Prediction,F1,Modele)
F1XB<-select(matrixXB,Prediction,F1,Modele)

dataF1<- rbind(F1NN,F1RF)#,F1XB)

#graphique avec boxplot pour F1 scores chaque modele (tout taxa mélangés)
ggplot(data = F1RF, aes(x = Modele, y = F1, fill = Modele)) +
    geom_boxplot() +
    labs(x = "Model", y = "Mean of F1-score of all taxa") +
    scale_fill_grey(start = 0, end = 0.6) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

mean(F1RF$F1)
mean(F1RFgenus$F1)

## F1 score pmodele RF pour chaque taxon
# Calculer les barres d'erreur (erreur standard)
matrixRF$se <- qt(0.975, 4-1) * sqrt(matrixRF$F1 * (1 - matrixRF$F1) / 4)
# Créer le graphique
matrixRF$species<-gsub("Class: ","",matrixRF$Prediction)
matrixRFgenus$species<-gsub("Class: ","",matrixRFgenus$Prediction)
matrixRF<-subset(matrixRF,species!="Debris")
ggplot(matrixRF, aes(x = species, y = F1)) +
  geom_col(width = 0.5) +  # Utiliser geom_col() pour les barres
  labs(title = "F1 Score par Taxon avec modele RF",
       x = "Taxon",
       y = "F1 Score") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Incliner les étiquettes de l'axe x

# Trier les données par ordre alphabétique du nom d'espèce
matrixRF <- matrixRF %>%
  mutate(color_group = case_when(
    F1 <= 0.5 ~ "0–0.5",
    F1 <= 0.75 ~ "0.5–0.75",
    TRUE ~ "0.75–1.0"
  )) 

# Visualisation avec réordonnancement par nom d'espèce et couleurs en fonction du F1score de l'espece
ggplot(matrixRF, aes(x = reorder(species, species), y = F1, fill = color_group)) +
  geom_col(width = 0.5) +  
  labs(title = "F1 Score par Taxon avec modèle RF",
       x = "Taxon",
       y = "F1 Score") +  
  scale_fill_manual(values = c("0–0.5" = "darkred", 
                               "0.5–0.75" = "gold", 
                               "0.75–1.0" = "darkgreen")) +
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 01, size =12),
    axis.text.y = element_text(size =14),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=14),
    legend.title = element_blank()
    )

# création graphique avec accuracy diagramme en barres horizontales

graphsp<- ggplot(matrixRF, aes(y = forcats::fct_relevel(reorder(species, desc(species)), "OTHER", after = 0))) +
  geom_bar(aes(x = Balanced.Accuracy, fill = "Correct"), stat = "identity",alpha=0.5) +
  geom_bar(aes(x = -(1 - Balanced.Accuracy), fill = "Misclassified"),alpha=0.5, 
            stat = "identity", position = position_nudge(x = 1)) +
  # Couleurs personnalisées
  scale_fill_manual(values = c("Correct" = "#332288", "Misclassified" = "#CC6677")) +
  # Ajouter le F1 score à droite de chaque barre
  geom_text(aes(x = 1.05, label = formatC(F1, format = "f", digits = 2)), 
            position = position_nudge(x = 0), 
            hjust = 0.2, vjust = 0.5, color = "black") +
  annotate("text", x = 1.05, y = max(as.numeric(as.factor(matrixRF$species))) + 0.8, 
         label = "F1 Score", hjust = 0, vjust = 0.1, fontface = "bold")+
  labs(x = "Accurracy",
       y = "Taxon") +
  theme_minimal()+
  # Supprimer les lignes au milieu des barres et forcer la grille entre elles
  scale_y_discrete(expand = expansion(mult = c(0, 0.025)))+  # Ajoute un espace entre les barres
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(margin = ggplot2::margin(r = 5))) 

graphgenus<- ggplot(matrixRFgenus, aes(y = forcats::fct_relevel(reorder(species, desc(species)), "OTHER", after = 0))) +
  geom_bar(aes(x = Balanced.Accuracy, fill = "Correct"), stat = "identity",alpha=0.5) +
  geom_bar(aes(x = -(1 - Balanced.Accuracy), fill = "Misclassified"),alpha=0.5, 
            stat = "identity", position = position_nudge(x = 1)) +
  # Couleurs personnalisées
  scale_fill_manual(values = c("Correct" = "#332288", "Misclassified" = "#CC6677")) +
  # Ajouter le F1 score à droite de chaque barre
  geom_text(aes(x = 1.05, label = formatC(F1, format = "f", digits = 2)), 
            position = position_nudge(x = 0), 
            hjust = 0.2, vjust = 0.5, color = "black") +
  annotate("text", x = 1.05, y = max(as.numeric(as.factor(matrixRFgenus$species)))+0.4, 
         label = "F1 Score", hjust = 0, vjust = 0.1, fontface = "bold")+
  labs(x = "Accurracy",
       y = "Taxon") +
  theme_minimal()+
  # Supprimer les lignes au milieu des barres et forcer la grille entre elles
  scale_y_discrete(expand = expansion(mult = c(0, 0.025)))+  # Ajoute un espace entre les barres
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(margin = ggplot2::margin(r = 5))) 

graphsp <- graphsp + xlim(-0.3, 1.3)
graphgenus <- graphgenus + xlim(-0.3, 1.3)
graph<-grid.arrange(graphsp, graphgenus, ncol = 2)
ggsave("./inputs_outputs/graph_V2.png", graph, width = 10, height = 15, dpi = 300)

#heatmap
rawmatrixRF<-read.csv("./inputs_outputs/ConfusionMatrixRF_genus_V2.csv", sep=",", h=T)
rawmatrixRF_long <- pivot_longer(rawmatrixRF, cols = -Prediction, names_to = "Taxon", values_to = "Nombre")

# matrice en % 
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
#matrix <- rawmatrixRF[-1,-1]
#matrix_percentage_by_row <- sweep(matrix, 1, rowSums(rawmatrixRF), FUN = "/") * 100
# Ajouter une colonne pour les catégories
rawmatrixRF_percent_long$Category <- cut(
  rawmatrixRF_percent_long$Nombre,
  breaks = c(-Inf, 1, 10, 50, 75, 100),
  labels = c("0-1", "1–10", "10–50", "50–75", "75–100"),
  include.lowest = TRUE
)

#créer la heatmap
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
ggsave("matgraph_species_V2.png", matgraph, width = 25, height = 20, dpi = 300)

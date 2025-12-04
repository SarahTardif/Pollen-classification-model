#### Contribution des variables #### 
library(randomForest)
library(datasets)
library(caret)
library(pdp)
library(performanceEstimation)
# Get variable importance measures
model<-readRDS("./inputs_outputs/modelRF_genus_V2_20250731.rds")
modrf<-model$finalModel
imp = varImpPlot(modrf)
# Normalisation en pourcentage
imp <- as.data.frame(imp)
imp$MeanDecreaseAccuracy_Normalized <- (imp$MeanDecreaseAccuracy - min(imp$MeanDecreaseAccuracy)) / 
                                        (max(imp$MeanDecreaseAccuracy) - min(imp$MeanDecreaseAccuracy)) * 100

imp$MeanDecreaseGini_Normalized <- (imp$MeanDecreaseGini - min(imp$MeanDecreaseGini)) / 
                                    (max(imp$MeanDecreaseGini) - min(imp$MeanDecreaseGini)) * 100
# Création du premier boxplot pour MeanDecreaseAccuracy
imp$var <- rownames(imp)
imp$var <- factor(imp$var, levels = imp$var[order(imp$MeanDecreaseAccuracy_Normalized, decreasing = TRUE)])

ggplot(imp, aes(x =var, y = MeanDecreaseAccuracy_Normalized, fill = var)) +
  geom_point(size=4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=30),
    axis.text.y = element_text(size=30, angle=90),
    legend.position = "none")
# Création du deuxième boxplot pour MeanDecreaseGini
imp$var <- factor(imp$var, levels = imp$var[order(imp$MeanDecreaseGini_Normalized, decreasing = TRUE)])


ggplot(imp, aes(x =var, y = MeanDecreaseGini_Normalized, fill = var)) +
  geom_point(size=4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=20), 
    axis.text.y = element_text(size=20,angle=90),
    axis.title.y = element_text(size=20,angle=90,vjust=2),
    legend.position = "none")+
  labs(y = "Mean decrease Gini (%)  ge", x = "")



### détails noeud/architecture modele ###
library(randomForestExplainer)
explain_forest(modrf)

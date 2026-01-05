# script to retrieve the contribution of each variable to the model's performance

#load packages
library(randomForest)
library(datasets)
library(caret)
library(pdp)
library(performanceEstimation)

# Get variable importance measures
model<-readRDS("./inputs_outputs/modelRF_species.rds")
# model<-readRDS("./inputs_outputs/modelRF_genus.rds") # if considering genus-level model

modrf <- model$finalModel
imp <- varImpPlot(modrf)
imp <- as.data.frame(imp)
imp$var <- rownames(imp)

# Normalisation in %
imp$MeanDecreaseAccuracy_Normalized <-
  (imp$MeanDecreaseAccuracy - min(imp$MeanDecreaseAccuracy)) /
  (max(imp$MeanDecreaseAccuracy) - min(imp$MeanDecreaseAccuracy)) * 100

imp$MeanDecreaseGini_Normalized <-
  (imp$MeanDecreaseGini - min(imp$MeanDecreaseGini)) /
  (max(imp$MeanDecreaseGini) - min(imp$MeanDecreaseGini)) * 100

# reorder factor for plotting
imp$var <- factor(
  imp$var,
  levels = imp$var[order(imp$MeanDecreaseGini_Normalized, decreasing = TRUE)]
)

# plot
ggplot(imp, aes(x =var, y = MeanDecreaseGini_Normalized, fill = var)) +
  geom_point(size=4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=20), 
    axis.text.y = element_text(size=20,angle=90),
    axis.title.y = element_text(size=20,angle=90,vjust=2),
    legend.position = "none")+
  labs(y = "Mean decrease Gini (%)  ge", x = "")


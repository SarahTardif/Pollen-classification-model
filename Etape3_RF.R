## script pour créer et tester le modèle de classification Random Forest

library(caret)
library(doParallel)
library(tibble)
library(randomForest)
library(dplyr)
#library(ranger)
library(e1071)

## charger les données 
trainset<-read.csv('./inputs_outputs/trainset_species_V2.csv',h=T) ## jeux de données pour entraîner le modèle
testset<-read.csv('./inputs_outputs/testset_genus_V2.csv', h=T,sep=",") ## jeux de données pour tester le modèle
trainset$taxon<-trainset$Class
testset$taxon<-testset$Class
trainset<-dplyr::select(trainset, -Genus, -Family, -Cytometry_Name, -Class)
testset<-dplyr::select(testset, -Family, -Cytometry_Name, -Class)
trainset$taxon<-as.factor(trainset$taxon)
testset$taxon<-as.factor(testset$taxon)
testset$Genus<-as.factor(testset$Genus)
testset$Class<-NULL


## Preparer les paramètres de random forest
CV<- trainControl(method = 'cv',
                  number=10,
                  savePredictions = TRUE)

rfGrid<-expand.grid(mtry=5)


## entrainement du modèle
rf<-train(species~. , data=trainset,
          method='rf',  ## rf = random forest
          trControl=CV,
          verbose=TRUE,
          tuneGrid=rfGrid,
          importance=TRUE,
          na.action=na.exclude)

## enregistrer le modèle
saveRDS(rf, "modelRF_species_balanced_20250106.rds")

## Etapes suivantes à faire sur mon ordi et pas sur calcul quebec
rf<-readRDS("./inputs_outputs/modelRF_genus_V2_20250731.rds")
## tester le modèle créé
predicted_class_test<-predict(rf, testset)
# avec probabilités de classification dans chaque espèce
predicted_class_test_prob<-predict(rf, testset,type="prob")
species_max <- apply(predicted_class_test_prob, 1, function(row) names(predicted_class_test_prob)[which.max(row)])
value_max <- apply(predicted_class_test_prob, 1, function(row) max(row))
predict_species_maxprob <- data.frame(species = species_max, prob = value_max)
testset_prob<-aggregate(prob ~ species, data = predict_species_maxprob, FUN = mean)
write.csv(testset_prob,"./inputs_outputs/testset_prob_genusV2.csv",row.names=F)

cmRF<-confusionMatrix(predicted_class_test, testset$taxon)


## matrice de confusion vers un dataframe pour l'enregistrer
matrixcm<-as.matrix(cmRF)
dataframe_data=as.data.frame(matrixcm)

dataframe_data <- tibble::rownames_to_column(dataframe_data, "Prediction")
write.csv(dataframe_data, "ConfusionMatrixRF_species_V2.csv", row.names = F)

## statistiques par classe, pour enregistrement
mat<-as.matrix(cmRF$byClass)
mat2<-round(mat, 4) ## garder seulement 4 décimales
dataframe_data=as.data.frame(mat2)

dataframe_data <- tibble::rownames_to_column(dataframe_data, "Prediction")
write.csv(dataframe_data, "ConfusionMatrixRF_species_V2_class.csv", row.names = F)

## des infos sur random forest
## https://afit-r.github.io/random_forests

## des infos pour optimiser le modèle
## https://rpubs.com/phamdinhkhanh/389752

## il  est aussi possible d'utiliser le paquet RandomForest, exemples ici :
## http://mehdikhaneboubi.free.fr/random_forest_r.html et là
## https://www.listendata.com/2014/11/random-forest-with-r.html

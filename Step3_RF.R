## Script to create and test the Random Forest classification model

##### Training portion to be performed on a high-capacity server — we used the Compute Canada server #####

#load packages
library(caret)
library(doParallel)
library(tibble)
library(randomForest)
library(dplyr)
#library(ranger)
library(e1071)

## load data for species-level model
trainset<-read.csv('./inputs_outputs/trainset_species.csv',h=T) ## trainset
testset<-read.csv('./inputs_outputs/testset_species.csv', h=T,sep=",") ## testset

## load data for genus-level model
#trainset<-read.csv('./inputs_outputs/trainset_genus.csv',h=T) ## trainset
#testset<-read.csv('./inputs_outputs/testset_genus.csv', h=T,sep=",") ## testset

trainset$taxon<-trainset$Class
testset$taxon<-testset$Class
trainset<-dplyr::select(trainset, -Genus, -Family, -Cytometry_Name, -Class)
testset<-dplyr::select(testset, -Family, -Cytometry_Name, -Class)
trainset$taxon<-as.factor(trainset$taxon)
testset$taxon<-as.factor(testset$taxon)
testset$Genus<-as.factor(testset$Genus)
testset$Class<-NULL


## Set up random forest parameters
CV<- trainControl(method = 'cv',
                  number=10,
                  savePredictions = TRUE)

rfGrid<-expand.grid(mtry=5)


## model training
rf<-train(species~. , data=trainset,
          method='rf',  ## rf = random forest
          trControl=CV,
          verbose=TRUE,
          tuneGrid=rfGrid,
          importance=TRUE,
          na.action=na.exclude)

## save the model
saveRDS(rf, "./inputs_outputs/modelRF_species.rds")
#saveRDS(rf, "./inputs_outputs/modelRF_genus.rds") # if considering genus-level model

##### Next steps can be done on your own computer and not on high-capacity server #####

rf<-readRDS("./inputs_outputs/modelRF_species.rds")
#rf<-readRDS("./inputs_outputs/modelRF_genus.rds") # if considering genus-level model

## test the trained model
predicted_class_test<-predict(rf, testset)
# with classification probabilities in each taxon
predicted_class_test_prob<-predict(rf, testset,type="prob")
species_max <- apply(predicted_class_test_prob, 1, function(row) names(predicted_class_test_prob)[which.max(row)])
value_max <- apply(predicted_class_test_prob, 1, function(row) max(row))
predict_species_maxprob <- data.frame(species = species_max, prob = value_max)
testset_prob<-aggregate(prob ~ species, data = predict_species_maxprob, FUN = mean)
write.csv(testset_prob,"./inputs_outputs/testset_prob_species.csv",row.names=F)
#write.csv(testset_prob,"./inputs_outputs/testset_prob_genus.csv",row.names=F) # if considering genus-level model

cmRF<-confusionMatrix(predicted_class_test, testset$taxon)


## confusion matrix to a dataframe for saving
matrixcm<-as.matrix(cmRF)
dataframe_data=as.data.frame(matrixcm)

dataframe_data <- tibble::rownames_to_column(dataframe_data, "Prediction")
write.csv(dataframe_data, "./inputs_outputs/ConfusionMatrixRF_species.csv", row.names = F)
#write.csv(dataframe_data, "./inputs_outputs/ConfusionMatrixRF_genus.csv", row.names = F) # if considering genus-level model

## statistics by class(=taxon), for saving
mat<-as.matrix(cmRF$byClass)
mat2<-round(mat, 4) ## keep only 4 decimal places
dataframe_data=as.data.frame(mat2)

dataframe_data <- tibble::rownames_to_column(dataframe_data, "Prediction")
write.csv(dataframe_data, "./inputs_outputs/ConfusionMatrixRF_species_class.csv", row.names = F)
#write.csv(dataframe_data, "./inputs_outputs/ConfusionMatrixRF_genus_class.csv", row.names = F) # if considering genus-level model


## Script to prepare training data for the model at genus level

# load packages
library(dplyr)

## load reference pollen data
data<-read.csv("./inputs_outputs/reference_pollen_all.csv", h=T)
names(data)[1]<-"./inputs_outputs/Cytometry_Name_pollens"

## load table with correspondance between sample names and species, genus and family 
names<-read.csv("./inputs_outputs/Collection_Reference_Pollens.csv", sep= ";",h=T)
data2<-left_join(data, names, by="Cytometry_Name_pollens")
data2$Cytometry_Name_debris<- NULL
# rearrange and clean data
training<-dplyr::select(data2, -Cytometry_Name_pollens, -Time, -SampleID)
training$species<-as.factor(paste(training$Genus, training$Species, sep = "_"))
training$Class<-as.factor(training$Genus)
training$Family<-as.factor(training$Family)
training$Cytometry_Name<-as.factor(training$Cytometry_Name)
training$Species<-NULL
training$Genus<-NULL
training$Cytometry_Name<-NULL
training$Family<-NULL
training$species<-NULL
str(training)

## load data for debris
deb<-read.csv("./inputs_outputs/reference_debris_all.csv", h=T)
deb1 <- deb[sample(nrow(deb),100000),] ## keep only 100000 débris
deb2<-dplyr::select(deb1, -Cytometry_Name, -Time, -SampleID)
deb2$Class<-as.factor("OTHER")
str(deb2)
## combine pollen and debris
training2<-rbind(training, deb2)

## cleaning, delete no value lines (inf, NA)
completerecords <- na.omit(training2) 
completerecords2 <-  completerecords %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.))) # checking only numeric columns:

## If there are fewer observations in "completerecords2" than in "training", investigate why
## Possible issue with reference sample names and incorrect linking with names
## There should not be any NA or inf normally

## random placement of lines
datamod <- completerecords2[sample(nrow(completerecords2)),]

####### data balancing #######

# load package for SMOTE function:
library(smotefamily) 
nb_cible <- 10000 # target number
nb_actuel_classe <- table(datamod$Class) # number of pollen grains per class
training_genus <- data.frame() # initialize the new dataset
# Loop to oversample or undersample each class to reach the target number:
for (class_name in names(nb_actuel_classe)) {
  class_data <- datamod[datamod$Class == class_name, ]  
  current_count <- as.numeric(nrow(class_data))
  numeric_data <- class_data[, sapply(class_data, is.numeric), drop = FALSE]
  if (current_count < nb_cible) {
    # oversampling with SMOTE
    perc.over <- abs(100 * (nb_cible - current_count) / current_count)
    synthetic_data <- SMOTE(numeric_data, as.numeric(class_data$Class), K = 5, dup_size = perc.over / 100)
    synthetic_data <- data.frame(synthetic_data$data, Class = class_data$Class)
    synthetic_data$Class <- class_name
    synthetic_data$class<-NULL
    numeric_data$Class<-class_data$Class
    combined_data <- rbind(numeric_data, synthetic_data)
    if (as.numeric(nrow(combined_data)) > nb_cible){
      combined_data <- combined_data[sample(1:nrow(combined_data), nb_cible), ] 
    }
    training_genus <- rbind(training_genus, combined_data)
  } else {
    # undersampling 
    sampled_data <- class_data[sample(1:nrow(class_data), nb_cible), ]
    sampled_data <- sampled_data[,!(names(sampled_data) %in% c("species"))]
    training_genus <- rbind(training_genus, sampled_data)
  }
}
# verification of the number in the final table
table(training_genus$Class)

## model training and testing datasets
index     <- 1:nrow(training_genus)
testindex <- sample(index, trunc(length(index)*30/100))
testset   <- training_genus[testindex,]
trainset  <- training_genus[-testindex,]

## save data for training and testing !!!
write.csv(training_genus, './inputs_outputs/trainingdata_genus.csv', row.names = F) ## complete dataset
write.csv(trainset, './inputs_outputs/trainset_genus.csv', row.names = F) ## training dataset
write.csv(testset, './inputs_outputs/testset_genus.csv', row.names = F) ## testing dataset



library(dplyr)
## charger les données de pollen de référence - vérifier qu'on est dans bon repertoire (le github)
data<-read.csv("references_pollens_all_V2.csv", h=T)
names(data)[1]<-"Cytometry_Name_pollens"
## charger le tableau qui contient le nom des échantillons et 
names<-read.csv("Collection_Reference_Pollens.csv", sep= ";",h=T)

data2<-left_join(data, names, by="Cytometry_Name_pollens")
data2$Cytometry_Name_debris<- NULL
  
training<-dplyr::select(data2, -Cytometry_Name_pollens, -Time, -SampleID)
#training$Class<-as.factor("Pollens")
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

## ajouter les débris
deb<-read.csv("references_debris_all_V2.csv", h=T)
deb1 <- deb[sample(nrow(deb),100000),] ## garder seulement 100000 débris
deb2<-dplyr::select(deb1, -Cytometry_Name, -Time, -SampleID)
deb2$Class<-as.factor("OTHER")
str(deb2)
## combine training et deb2
training2<-rbind(training, deb2)

## nettoyage, pour supprimer les lignes sans valeurs (inf, NA)
completerecords <- na.omit(training2) 
completerecords2 <-  completerecords %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.))) # checking only numeric columns:

## S'il y a moins d'obs. dans completerecords2 que dans training, regarder pourquoi ! 
## possible problème dans le nom d'échantillons de référence et mauvaise liaison avec names
## il ne devrait pas y avoir de NA ou de inf normalement

## Etape à ne pas skipper: enlever Juglans spp et Salix spp - pour modele V1
#completerecords2 <- completerecords2[completerecords2$species != "Juglans_spp" & completerecords2$species != "Salix_spp" & completerecords2$species != "Acer_freemanii" & completerecords2$species != "Fraxinus_nigra"& completerecords2$species != "Fagus_grandifolia"& completerecords2$species != "Salix_gracilistyla"& completerecords2$species != "Taxus_x media", ]

# Réinitialiser les niveaux de la colonne Class
completerecords2$species <- droplevels(completerecords2$species)
completerecords2$Class <- droplevels(completerecords2$Class)
str(completerecords2)


## placement aléatoire des lignes (ID)
datamod <- completerecords2[sample(nrow(completerecords2)),]



####### rééquilibrage des données #######
library(smotefamily)  # Pour SMOTE
# Paramètre cible
nb_cible <- 10000
# Étape 1 : Calculer le nombre de pollen par classe
nb_actuel_classe <- table(datamod$Class)
# Étape 2 : Initialiser le nouveau jeu de données
training_genus <- data.frame()
# Étape 3 : Boucle pour sur-échantillonner ou sous-échantillonner chaque classe
for (class_name in names(nb_actuel_classe)) {
  class_data <- datamod[datamod$Class == class_name, ]  # Données pour la classe en cours
  current_count <- as.numeric(nrow(class_data))
  # Sélectionner uniquement les colonnes numériques (en excluant la colonne de la classe)
  numeric_data <- class_data[, sapply(class_data, is.numeric), drop = FALSE]
  if (current_count < nb_cible) {
    # Sur-échantillonnage avec SMOTE
    perc.over <- abs(100 * (nb_cible - current_count) / current_count)
    # Appliquer SMOTE
    synthetic_data <- SMOTE(numeric_data, as.numeric(class_data$Class), K = 5, dup_size = perc.over / 100)
    # Reconstituer le jeu de données avec les classes
    synthetic_data <- data.frame(synthetic_data$data, Class = class_data$Class)
    synthetic_data$Class <- class_name
    synthetic_data$class<-NULL
    numeric_data$Class<-class_data$Class
    # Combiner les données synthétiques et originales sans dépasser nb_cible
    combined_data <- rbind(numeric_data, synthetic_data)
    if (as.numeric(nrow(combined_data)) > nb_cible){
      combined_data <- combined_data[sample(1:nrow(combined_data), nb_cible), ]  # Limiter à nb_cible
    }
    training_genus <- rbind(training_genus, combined_data)
  } else {
    # Sous-échantillonnage
    sampled_data <- class_data[sample(1:nrow(class_data), nb_cible), ]
    sampled_data <- sampled_data[,!(names(sampled_data) %in% c("species"))]
    training_genus <- rbind(training_genus, sampled_data)
  }
}
# Vérification des résultats
table(training_genus$Class)




## nettoyage (garde seulement le tableau datamod dans les fichiers à droite)
rm(list=setdiff(ls(), "datamod"))

## occurrence de chaque espèce
library(ggplot2)
resumé<-as.data.frame(table(datamod$Class))
ggplot(resumé, aes(x= Var1,y=Freq))+
     geom_col()+
     theme_minimal()+
     theme(axis.text.x = element_text(angle = 45, hjust = 1))+
     geom_hline(yintercept = 1000, color = "grey") +
     geom_hline(yintercept = 2000, color = "grey") +
     geom_hline(yintercept = 5000, color = "grey") +
     geom_hline(yintercept = 10000, color = "grey")


## table d'entraînement et de test du modèle
index     <- 1:nrow(training_genus)
testindex <- sample(index, trunc(length(index)*30/100))
testset   <- training_genus[testindex,]
trainset  <- training_genus[-testindex,]

## sauvegarder les données de train et test !!!
write.csv(training_genus, 'trainingdata_genus_V2.csv', row.names = F) ## jeux de données complet
write.csv(trainset, 'trainset_genus_V2.csv', row.names = F) ## jeux de données pour entraîner le modèle
write.csv(testset, 'testset_genus_V2.csv', row.names = F) ## jeux de données pour tester le modèle


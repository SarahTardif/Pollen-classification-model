## Script to convert cytometer FCS files into CSV files
## adapted from the script by Thomas Ashhurst
## https://github.com/sydneycytometry/CSV-to-FCS/blob/master/FCS-to-CSV%20v2.0.R

#####
## Some packages require the installation of BiocManager; install them as follows:
#install.packages("BiocManager")
#BiocManager::install("flowCore") 
#BiocManager::install("Biobase")


# Load packages
library('flowCore')
library('Biobase')
library('data.table')

# Check the working directory and change it if necessary with setwd()
getwd() ## check
PrimaryDirectory <- getwd()
PrimaryDirectory ## re check

## Retrieves the names of .fcs files in the working directory
FileNames <- list.files(path=PrimaryDirectory, pattern = ".fcs") # list
as.matrix(FileNames) # matrix

## read data from files into a dataframe
DataList=list()

for (File in FileNames) {
  fcsfile <- read.FCS(File, transformation = FALSE,truncate_max_range = FALSE)
  tempdata <- exprs(fcsfile)
  colnames(tempdata)<-fcsfile@parameters@data$desc 
  tempdata <- tempdata[1:nrow(tempdata),1:ncol(tempdata)]
  File <- gsub(".fcs", "", File)
  DataList[[File]] <- tempdata
}
rm(tempdata)
rm(fcsfile)
AllSampleNames <- names(DataList)

## Data check
head(DataList)


##### END USER INPUT #####

# to create a subfolder for CSV files, named in the format "Output_FCS-to-CSV %Y-%m-%d-%H:%M:%S"
x <- Sys.time()
x <- gsub(":", "-", x)
x <- gsub(" ", "_", x)

newdir <- paste0("Output_FCS-to-CSV", "_", x) # can be replaced by newdir<-“FolderName”

setwd(PrimaryDirectory)
dir.create(paste0(newdir), showWarnings = FALSE) #  creates a subfolder named newdir
setwd(newdir)

# To extract the files into an existing folder, use: setwd("/an/existing/folder/")
# in any case, be careful not to overwrite existing files
for(i in c(1:length(AllSampleNames))){
  data_subset <- DataList[i][[1]]
  data_subset <- as.data.frame(data_subset)
  colnames(data_subset) = gsub("-", "_", colnames(data_subset)) ## replace - with _ in column names
  a <- names(DataList)[i]
  data_subset$SampleID <- seq.int(nrow(data_subset)) ## add the sample name as ID
  write.csv(data_subset, paste0(a, ".csv"), row.names = FALSE)
}


## To combine all new .csv files into one:
library(dplyr)
library(here)
library(readr)
library(purrr)
library(fs)

rm(list=ls())

## Creates a vector of file names, with the full path
dir_list <- list.files(here("./Output_FCS-to-CSV %Y-%m-%d-%H:%M:%S"), # output directory created before - check correct name
                       pattern = "*.csv", full.names = TRUE) 

## Name the vector with only the file name, without the extension
names(dir_list) <- path_ext_remove(list.files(here("./Output_FCS-to-CSV %Y-%m-%d-%H:%M:%S"), # output directory created before - check correct name
                                              pattern = "*.csv"))

files_df <- map_dfr(dir_list, read_csv, .id = "Cytometry_Name") ## Combines all CSV files into one, adds a Cytometry_name column with the sample name.


write.csv(files_df, "./inputs_outputs/reference_pollen_all.csv", row.names = F)



####  Repeat all of the above steps with the debris  ####

rm(list=ls())


# Check the working directory and change it if necessary with setwd()
getwd() ## check
PrimaryDirectory <- getwd()
PrimaryDirectory ## re check

## Retrieves the names of .fcs files in the working directory
FileNames <- list.files(path=PrimaryDirectory, pattern = ".fcs")     # list
as.matrix(FileNames) # matrix

## read data from files into a dataframe
DataList=list() 

for (File in FileNames) { 
  fcsfile <- read.FCS(File, transformation = FALSE,truncate_max_range = FALSE)
  tempdata <- exprs(fcsfile)
  colnames(tempdata)<-fcsfile@parameters@data$desc 
  tempdata <- tempdata[1:nrow(tempdata),1:ncol(tempdata)]
  File <- gsub(".fcs", "", File)
  DataList[[File]] <- tempdata
}

rm(tempdata)
rm(fcsfile)
AllSampleNames <- names(DataList)

## Data check
head(DataList)


##### END USER INPUT #####
# to create a subfolder for CSV files, named in the format "Output_FCS-to-CSV %Y-%m-%d-%H:%M:%S"
x <- Sys.time()
x <- gsub(":", "-", x)
x <- gsub(" ", "_", x)

newdir <- paste0("Output_FCS-to-CSV", "_", x) # can be replaced by newdir<-“FolderName”

setwd(PrimaryDirectory)
dir.create(paste0(newdir), showWarnings = FALSE) #  creates a subfolder named newdir
setwd(newdir)

# To extract the files into an existing folder, use: setwd("/an/existing/folder/")
# in any case, be careful not to overwrite existing files
for(i in c(1:length(AllSampleNames))){
  data_subset <- DataList[i][[1]]
  data_subset <- as.data.frame(data_subset)
  colnames(data_subset) = gsub("-", "_", colnames(data_subset)) ## replace - with _ in column names
  a <- names(DataList)[i]
  data_subset$SampleID <- seq.int(nrow(data_subset)) ## add the sample name as ID
  write.csv(data_subset, paste0(a, ".csv"), row.names = FALSE)
}


## To combine all new .csv files into one:
library(dplyr)
library(here)
library(readr)
library(purrr)
library(fs)

rm(list=ls())

## Creates a vector of file names, with the full path
dir_list <- list.files(here("./Output_FCS-to-CSV %Y-%m-%d-%H:%M:%S"), # output directory created before - check correct name
                       pattern = "*.csv", full.names = TRUE)

## Name the vector with only the file name, without the extension
names(dir_list) <- path_ext_remove(list.files(here("./Output_FCS-to-CSV %Y-%m-%d-%H:%M:%S"), # output directory created before - check correct name
                                              pattern = "*.csv"))

files_df <- map_dfr(dir_list, read_csv, .id = "Cytometry_Name") ## Combines all CSV files into one, adds a Cytometry_name column with the sample name.


write.csv(files_df, "./inputs_outputs/reference_debris_all.csv", row.names = F)

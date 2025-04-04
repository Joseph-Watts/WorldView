library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(here)
library(haven)
library(labelled)
library(sjlabelled)

### Read in files
spssdata <- readRDS("WVS_Dataset/WVS_Cross-National_Wave_7_rds_v6_0.rds")
C.data <- readRDS("WVS_Dataset/WVS7_Country.rds")
I.data <- readRDS("WVS_Dataset/WVS7_Individual.rds")
CB_var_info <- read_xlsx("WVS_Dataset/Codebook manual coded index.xlsx")


### Main SPSS Dataset can't be handled directly, so create a data dictionary.
dict <- labelled::generate_dictionary(spssdata)

### dict$label is a list of the labels with attr info, so extract just the label name.
labels <- vector("character",length=nrow(dict))
for (x in 1:nrow(dict)){
  labels[x] <- dict$label[[x]]
}

### Create a data frame mapping the data dictionary variables with the extracted labels. 
mapped.vars.labels <- as.data.frame(cbind(dict$variable,labels))
colnames(mapped.vars.labels) <- c("Col_ID", "Col_Label")

### Using the mapping to add a new column to the Codebook to give a longer ID with meaningful name, eg "Q1 - meaning of Q1".
CB_var_info <- inner_join(CB_var_info,mapped.vars.labels)
CB_var_info$ColLab <- paste0(CB_var_info$Col_ID,"-",CB_var_info$Col_Label)

# Save new codebook updated with labels
write_xlsx(CB_var_info, "WVS_Dataset/WVS7_Codebook_updated_labels.xlsx")



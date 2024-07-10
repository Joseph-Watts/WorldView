library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(here)
library(haven)
library(labelled)
library(sjlabelled)

spssdata <- readRDS("WVS_Dataset/WVS_Cross-National_Wave_7_rds_v6_0.rds")
C.data <- readRDS("WVS_Dataset/WVS7_Country.rds")
I.data <- readRDS("WVS_Dataset/WVS7_Individual.rds")
CB_var_info <- read_xlsx("WVS_Dataset/Codebook manual coded index.xlsx")



dict <- labelled::generate_dictionary(spssdata)

labels <- vector("character",length=nrow(dict))
for (x in 1:nrow(dict)){
  labels[x] <- dict$label[[x]]
}

mapped.vars.labels <- as.data.frame(cbind(dict$variable,labels))
colnames(mapped.vars.labels) <- c("Col_ID", "Col_Label")

CB_var_info <- inner_join(CB_var_info,mapped.vars.labels)
CB_var_info$ColLab <- paste0(CB_var_info$Col_ID,"-",CB_var_info$Col_Label)

# Save new codebook updated with labels
write_xlsx(CB_var_info, "WVS_Dataset/WVS7_Codebook_updated_labels.xlsx")
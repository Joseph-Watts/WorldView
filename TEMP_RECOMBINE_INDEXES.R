library(tidyverse)
library(readxl)
library(writexl)

d <- read_rds("WVS_Dataset/WVS_Cross-National_Wave_7_rds_v6_0.rds")

d1 <- read_xlsx("WVS_Dataset/Codebook manual coded index.xlsx")

d2 <- read_xlsx(("WVS_Dataset/WVS7_Full_Processed_Codebook.xlsx"))

intersect(colnames(d1), colnames(d2))

d12 <- merge(d1, d2, all = T)

colnames(d)

Rs <- paste0(d12$Col_ID, "R")[
  paste0(d12$Col_ID, "R") %in% colnames(d)
  ]

colnames(d12)

cols_to_NA <- c("Variable_Text_ChatGPT",                     
                "Variable_Display_Type",                      
                "Variable_Display_Logical",                   
                "Display_to_Users",                           
                "In_Final_Individual_Dataset",                
                "Dropped_or_Not_Displayed_Reason",            
                "Manual_Display_Type",                        
                "Processed_Data_Type",                       
                "Short_Label",                                
                "WVS_Variable_Title",                         
                "Question_Text",                              
                "Additional_Text",                            
                "Original_Response_Options_Including_Missing",
                "Processed_Response_Values",                  
                "Processing_Notes",
                "Missing_Data_Handling",
                "ColLab")


for(i in 1:length(Rs)){
  
  Rorig <- gsub("R$", "", Rs[i])
  
  d12 <- rbind(d12, d12[d12$Col_ID == Rorig, ])

  d12[nrow(d12), "Col_ID"] <- Rs[i]
  
  d12[nrow(d12), cols_to_NA] <- NA

}

write_xlsx(d12, "WVS_Dataset/WVS7_Variable_Index.xlsx")


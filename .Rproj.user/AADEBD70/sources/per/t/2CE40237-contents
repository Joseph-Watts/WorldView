#' To do:
#' Consider including some of the country level additional data (e.g. HDI etc)

library(tidyverse)
library(haven)
library(shiny)
library(readxl)

#' -----------------------------------------------------------------------------
#' Step 1: Setting up the data
#' -----------------------------------------------------------------------------

#' Reading in the WVS Wave 7 Data
#' Note: This is not in a standard R format, the data is haven formatted which
#' creates some issues for other functions
d <- read_rds("WVS_Dataset/WVS_Cross-National_Wave_7_rds_v6_0.rds")

#' Manually coded table with variables wanted and their classification
d_vars_coded <- read_xlsx("WVS_Dataset/Codebook manual coded index.xlsx")

#' These are manually coded classification of how the variable should be
#' presented in the app
table(d_vars_coded$Variable_Display_Type)

#' Selecting variables wanted and converting to a data.frame
d2 <- as.data.frame(d[, d_vars_coded$Col_ID])

#' Converting all ordinal haven format variables to standard ordered factors
for(i in 1:nrow(d_vars_coded)){
  
  #' Variable ID
  i_ID <- d_vars_coded$Col_ID[i]
  
  #' This variable does not need processing further
  if(i_ID == "B_COUNTRY_ALPHA"){next}
  
  #' raw data for column i
  i_d <- d2[ , i_ID]
  
  #' If the variable is to be treated as as an ordered factor, then...
  if(d_vars_coded$Variable_Display_Type[i] %in% 
     c("factor_ordered", "factor")){
    
    #' Treating variable as numeric
    i_d_numeric <- as.numeric(i_d)
    
    #' Replacing codes for missing data with NA
    i_d_numeric <- ifelse(i_d_numeric < 0, NA, i_d_numeric)
    
    #' Names of different variables
    i_d_values <- attr(i_d, "labels")
    
    #' Index matching values to their names
    i_match_idx <- match(i_d_numeric, i_d_values)
    
    #' Replacing values with their names
    i_d_updated <- ifelse(is.na(i_match_idx),
                          NA,
                          names(i_d_values)[i_match_idx])
    
    #' Ordered level of factor (taking the order from WVS survey)
    #' NOTE: assuming that there are no 0 codes? Need to check this later
    i_d_values_noNA <- names(i_d_values)[i_d_values >= 0]
    
    #' Dropping duplicate factor labels
    i_d_values_noNA <- i_d_values_noNA[!duplicated(i_d_values_noNA)]
    
    #' Is the factor ordered
    i_factor_ordered <- d_vars_coded$Variable_Display_Type[i] == "factor_ordered"
    
    #' Converting variable to factor
    i_d_updated <- factor(i_d_updated,
                          levels = i_d_values_noNA,
                          ordered = i_factor_ordered)
    
    #' Updating variable
    d2[ , i_ID] <- i_d_updated
    
  }else if(d_vars_coded$Variable_Display_Type[i] == "integer"){
    
    #' Treating variable as numeric
    i_d_integer <- as.integer(i_d)
    
    #' Replacing codes for missing data with NA
    i_d_integer <- ifelse(i_d_integer < 0, NA, i_d_integer)
    
    #' Updating variable
    d2[ , i_ID] <- i_d_integer
    
  }else{
    
    stop(paste0(d_vars_coded$Variable_Display_Type[i], 
    " in d_vars_coded$Variable_Display_Type[",
    i,
    "]. Code not currently set up to handle this input.")
    )
    
  }

}


#' Quick tidy up of data
#' 
#' Dropping levels of factor that do not occur
table(d2$B_COUNTRY) 
d2$B_COUNTRY <- factor(d2$B_COUNTRY)

table(d2$B_COUNTRY_ALPHA) 
d2$B_COUNTRY_ALPHA <- factor(d2$B_COUNTRY_ALPHA)

#' Writing out the dataset
write_rds(d2, 
          "WVS_Dataset/WVS7_Individual.rds",
          compress = "gz")



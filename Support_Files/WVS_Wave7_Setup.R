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


#=========================================================================================================
#' Country Level Summary Data

#' Notes:
#' 
#' 1. Need to think about how to present information on whether ordered variables 
#' are reverse coded. Many variables are a little unintuitive as the larger
#' quantity has a smaller value.
#' 
#'  2. It would be nice to incorporate other datasets here

library(tidyverse)
library(plyr)
library(readxl)
library(writexl)
library(tm)

#' Reading in individual level data
d <- read_rds("WVS_Dataset/WVS7_Individual.rds")

#' Reading in variable information
d_vars_coded <- read_xlsx("WVS_Dataset/Codebook manual coded index.xlsx")
#d_vars_coded <- read_xlsx("WVS_Dataset/WVS7_Codebook_updated_labels.xlsx")

#' Converting character to logical
d_vars_coded$Variable_Display_Logical <- as.logical(d_vars_coded$Variable_Display_Logical)

#' Function for generating summary stats for a variable
sum_fun <- function(data, v, min_n = 10){
  
  # Extract column
  d_col <- data[, v]
  
  #' Class of data in column
  d_class <- paste0(class(d_col), collapse = " ")
  
  #; Number of observations in d_col
  n_notNA <- sum(!is.na(d_col))
  
  # If integer, get mean
  if(d_class == "integer"){
    
    #' If there are fewer than min_n observations return NA
    if(n_notNA < min_n){
      
      d_sum <- NA
      
      names(d_sum) <- v
      
      return(d_sum)
      
    }else{
      
      d_sum <- mean(d_col, na.rm = T)
      
      names(d_sum) <- v
      
      return(d_sum)
      
    }
    
    # If ordered factor, treat as numeric and get mean  
  }else if(d_class == "ordered factor"){
    
    #' If there are fewer than min_n observations return NA
    if(n_notNA < min_n){
      
      d_sum <- NA
      
      names(d_sum) <- v
      
      return(d_sum)
      
    }else{
      
      d_sum <- mean(as.numeric(d_col), na.rm = T)
      
      names(d_sum) <- v
      
      return(d_sum)
      
    }
    
    # Is unordered factor, get proportions of each level
  }else if(d_class == "factor"){
    
    #' Return proportion of each 
    d_levels <- levels(d_col)
    
    d_tbl <- table(d_col) %>% 
      as.matrix() / n_notNA
    
    d_sum <- d_tbl[ , 1]
    
    new_names <- removePunctuation(names(d_sum))
    new_names <- gsub(" ", "_", new_names)
    
    names(d_sum) <- paste(v, new_names, sep = ".")
    
    if(n_notNA < min_n){
      
      d_sum[1:length(d_sum)] <- NA
      
    }
    
    return(d_sum)
    
    #' If the column is of a different format stop with an error  
  }else{
    
    stop("Unsupported class")
    
  }
  
} 

#' Columns that sum_fun should be applied to
cols_to_sum_fun <- d_vars_coded$Col_ID[d_vars_coded$Variable_Display_Logical]
#cols_to_sum_fun <- d_vars_coded$ColLab[d_vars_coded$Variable_Display_Logical]

#' Function for applying sum_fun to each of the desired columns in a dataframe,
#' subset by country
country_sum <- function(data, 
                        country,
                        cols = cols_to_sum_fun){
  
  #' Country wanted
  i_d <- data[data$B_COUNTRY == country, ]
  
  #' Applying sum_fun to columns
  i_d_sum <- lapply(cols_to_sum_fun, 
                    sum_fun, 
                    data = i_d)
  
  #' Getting the output in a df
  i_d_sum <- lapply(i_d_sum, rbind)
  
  i_d_sum <- do.call(cbind, i_d_sum) %>%
    as.data.frame()
  
  #' Adding in country column
  i_d_sum$B_COUNTRY <- country
  i_d_sum <- relocate(i_d_sum, B_COUNTRY, .before = Q1)
  
  i_d_sum$B_COUNTRY_ALPHA <- unique(i_d$B_COUNTRY_ALPHA)
  i_d_sum <- relocate(i_d_sum, B_COUNTRY_ALPHA, .before = Q1)
  
  return(i_d_sum)
  
}

#' List of all countries to apply country_sum to 
countries <- unique(d$B_COUNTRY)

#' Applying country_sum function to all countries
country_sum_output <- lapply(countries, country_sum, data = d)

#' Converting the output from a list of data.frames to a data.frame
country_sum_output <- ldply(country_sum_output , data.frame)

#' Saving out the country level summary data
# write_xlsx(country_sum_output, "WVS_Dataset/WVS7_Country.xlsx")
# write_rds(country_sum_output, "WVS_Dataset/WVS7_Country.rds")


#============================================================================================================
#' Country Level Summary Data

#' Notes:
#' 
#' 1. Need to think about how to present information on whether ordered variables 
#' are reverse coded. Many variables are a little unintuitive as the larger
#' quantity has a smaller value.
#' 
#'  2. It would be nice to incorporate other datasets here

library(tidyverse)
library(plyr)
library(readxl)
library(writexl)
library(tm)

#' Reading in individual level data
d <- read_rds("WVS_Dataset/WVS7_Individual.rds")

#' Reading in variable information
d_vars_coded <- read_xlsx("WVS_Dataset/Codebook manual coded index.xlsx")
#d_vars_coded <- read_xlsx("WVS_Dataset/WVS7_Codebook_updated_labels.xlsx")

#' Converting character to logical
d_vars_coded$Variable_Display_Logical <- as.logical(d_vars_coded$Variable_Display_Logical)

#' Function for generating summary stats for a variable
sum_fun <- function(data, v, min_n = 10){
  
  # Extract column
  d_col <- data[, v]
  
  #' Class of data in column
  d_class <- paste0(class(d_col), collapse = " ")
  
  #; Number of observations in d_col
  n_notNA <- sum(!is.na(d_col))
  
  # If integer, get mean
  if(d_class == "integer"){
    
    #' If there are fewer than min_n observations return NA
    if(n_notNA < min_n){
      
      d_sum <- NA
      
      names(d_sum) <- v
      
      return(d_sum)
      
    }else{
          
      d_sum <- mean(d_col, na.rm = T)
      
      names(d_sum) <- v
      
      return(d_sum)
    
    }
  
  # If ordered factor, treat as numeric and get mean  
  }else if(d_class == "ordered factor"){
    
    #' If there are fewer than min_n observations return NA
    if(n_notNA < min_n){
      
      d_sum <- NA
      
      names(d_sum) <- v
      
      return(d_sum)
      
    }else{
      
      d_sum <- mean(as.numeric(d_col), na.rm = T)
      
      names(d_sum) <- v
      
      return(d_sum)
    
    }
  
  # Is unordered factor, get proportions of each level
  }else if(d_class == "factor"){
    
    #' Return proportion of each 
    d_levels <- levels(d_col)

    d_tbl <- table(d_col) %>% 
      as.matrix() / n_notNA
    
    d_sum <- d_tbl[ , 1]
    
    new_names <- removePunctuation(names(d_sum))
    new_names <- gsub(" ", "_", new_names)
  
    names(d_sum) <- paste(v, new_names, sep = ".")
    
    if(n_notNA < min_n){
      
      d_sum[1:length(d_sum)] <- NA
      
    }
    
    return(d_sum)
    
  #' If the column is of a different format stop with an error  
  }else{
    
    stop("Unsupported class")
    
  }
    
} 

#' Columns that sum_fun should be applied to
cols_to_sum_fun <- d_vars_coded$Col_ID[d_vars_coded$Variable_Display_Logical]
#cols_to_sum_fun <- d_vars_coded$ColLab[d_vars_coded$Variable_Display_Logical]

#' Function for applying sum_fun to each of the desired columns in a dataframe,
#' subset by country
country_sum <- function(data, 
                        country,
                        cols = cols_to_sum_fun){
  
  #' Country wanted
  i_d <- data[data$B_COUNTRY == country, ]
  
  #' Applying sum_fun to columns
  i_d_sum <- lapply(cols_to_sum_fun, 
                    sum_fun, 
                    data = i_d)
  
  #' Getting the output in a df
  i_d_sum <- lapply(i_d_sum, rbind)
  
  i_d_sum <- do.call(cbind, i_d_sum) %>%
    as.data.frame()
  
  #' Adding in country column
  i_d_sum$B_COUNTRY <- country
  i_d_sum <- relocate(i_d_sum, B_COUNTRY, .before = Q1)
  
  i_d_sum$B_COUNTRY_ALPHA <- unique(i_d$B_COUNTRY_ALPHA)
  i_d_sum <- relocate(i_d_sum, B_COUNTRY_ALPHA, .before = Q1)
  
  return(i_d_sum)
  
}

#' List of all countries to apply country_sum to 
countries <- unique(d$B_COUNTRY)

#' Applying country_sum function to all countries
country_sum_output <- lapply(countries, country_sum, data = d)

#' Converting the output from a list of data.frames to a data.frame
country_sum_output <- ldply(country_sum_output , data.frame)

#' Saving out the country level summary data
# write_xlsx(country_sum_output, "WVS_Dataset/WVS7_Country.xlsx")
# write_rds(country_sum_output, "WVS_Dataset/WVS7_Country.rds")

#=================================================================================================================================
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

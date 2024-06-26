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

#' Reading in individual level data
d <- read_rds("WVS_Dataset/WVS7_Individual.rds")

#' Reading in variable information
d_vars_coded <- read_xlsx("WVS_Dataset/Codebook manual coded index.xlsx")

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

    #' If there are fewer than min_n observations return NA
    if(n_notNA < min_n){
      
      #' Retain each level as a separate item in the output
      #' This ensures outputs remains the same length across inputs
      d_sum <- rep(NA, times = length(d_levels))
      
      names(d_sum) <- paste(v, names(d_sum), sep = ": ")
      
      return(d_sum)
      
    }else{

      d_tbl <- table(d_col) %>% 
        as.matrix() / n_notNA
      
      d_sum <- d_tbl[ , 1]
      
      names(d_sum) <- paste(v, names(d_sum), sep = ": ")
      
      return(d_sum)
    
    }
    
  #' If the column is of a different format stop with an error  
  }else{
    
    stop("Unsupported class")
    
  }
    
} 

#' Columns that sum_fun should be applied to
cols_to_sum_fun <- d_vars_coded$Col_ID[d_vars_coded$Variable_Display_Logical]

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
write_rds(country_sum_output, 
          "WVS_Dataset/WVS7_Country.rds",
          compress = "gz")





#' To do:
#' Consider including some of the country level additional data (e.g. HDI etc)

#' Notes:
#' - Not currently set up to process the Recoded Variables (e.g. Q95R)
#' - Havnt checked variables beyond those currently marked for display
#' - When changing how a variable is processed in this script, it also need to be 
#' updated in the "WVS7_Variable_Index.xlsx" file to reflect this (otherwise 
#' codebook) wont be updated
#' 
#' - To come back and reflect on the variable processing here. It seems 
#' that categorical information should often be retained. 
#' - Much of this recoding should not be don't the way it is (e.g. Q273 makes no
#' sense to convert to numerical)

library(tidyverse)
library(readxl)
library(writexl)
library(haven)

#' -----------------------------------------------------------------------------
#' Step 1: Setting up the data
#' -----------------------------------------------------------------------------

#' Reading in the WVS Wave 7 Data
#' Note: This is not in a standard R format, the data is haven formatted which
#' creates some issues for other functions
d <- read_rds("data/WVS_Cross-National_Wave_7_rds_v6_0.rds")

#' Reading in variable information
wvs7_var_index <- readxl::read_xlsx("data/WVS7_Variable_Index.xlsx")

#' Converting character to logical
wvs7_var_index$Variable_Display_Logical <- as.logical(wvs7_var_index$Variable_Display_Logical)

#' Columns that will be displayed
cols_display <- wvs7_var_index$Col_ID[wvs7_var_index$Variable_Display_Logical]

#' Ignored questions (given the number of factors they have or any other condition)
#' These will be dropped, even if included in the Variable_Display_Logical
ignored_questions <- c("Q223", # political parties for each country - almost 1000 different factors
                       "Q266", # birth place - basically all countries ~ 200 factors
                       "Q267", # birth place - basically all countries ~ 200 factors
                       "Q268", # birth place - basically all countries ~ 200 factors
                       "Q272", # language groupings - # different factors
                       "Q290") # ethnic groupings - # different factors

cols_display <- cols_display[!cols_display %in% ignored_questions]

# Filtering to those variables being displayed
wvs7_var_index_displayed <- wvs7_var_index[
  wvs7_var_index$Col_ID %in% cols_display,  
]

cols_id <-  c("B_COUNTRY", 
              "B_COUNTRY_ALPHA", 
              "S007")

cols_include <- c(cols_id, 
                  cols_display)


#' These are manually coded classification of how the variable should be
#' presented in the app
table(wvs7_var_index$Variable_Display_Type)

#' Selecting variables wanted and converting to a data.frame
indiv <- as.data.frame(d[ , cols_include])



#' Converting all ordinal haven format variables to standard ordered factors
for(i in 1:length(c("B_COUNTRY", cols_display))){
  
  #' Variable ID
  i_ID <- c("B_COUNTRY", cols_display)[i]
  
  i_display_type <- wvs7_var_index$Variable_Display_Type[
    wvs7_var_index$Col_ID == i_ID]
  
  #' raw data for column i
  i_d <- indiv[ , i_ID]
  
  #' If the variable is to be treated as an ordered factor or factor, then...
  if(i_display_type %in% 
     c("factor_ordered", "factor")){
    
    #' Treating variable as numeric
    i_d_numeric <- as.numeric(zap_labels(i_d))

    #' Replacing codes for missing data with NA
    i_d_numeric <- ifelse(i_d_numeric < 0, NA, i_d_numeric)
    
    #' Names of different levels of factor
    i_d_values <- attr(i_d, "labels")
    
    #' Index matching values to their names
    i_match_idx <- match(i_d_numeric, i_d_values)
    
    #' Replacing values with their names
    i_d_updated <- ifelse(is.na(i_match_idx),
                          NA,
                          names(i_d_values)[i_match_idx])
    
    #' Ordered level of factor (taking the order from WVS survey)
    i_d_values_noNA <- names(i_d_values)[i_d_values > 0]
    
    #' Dropping duplicate factor labels
    i_d_values_noNA <- i_d_values_noNA[!duplicated(i_d_values_noNA)]
    
    #' Is the factor ordered
    i_factor_ordered <- i_display_type == "factor_ordered"
    
    #' Converting variable to factor
    i_d_updated <- factor(i_d_updated,
                          levels = i_d_values_noNA,
                          ordered = i_factor_ordered)
    
    #' Updating variable
    indiv[ , i_ID] <- i_d_updated
    
  }else if(i_display_type == "integer") {
    #' Treating variable as numeric
    i_d_integer <- as.integer(zap_labels(i_d))
    
    #' Replacing codes for missing data with NA
    i_d_integer <- ifelse(i_d_integer < 0, NA, i_d_integer)
    
    #' Updating variable
    indiv[, i_ID] <- i_d_integer
    
  } else{
    stop(
      paste0(
        i_display_type,
        " in wvs7_var_index$Variable_Display_Type[",
        i,
        "]. Code not currently set up to handle this input."
      )
    )
  }
}

# Recoding columns with ordered factors from Yes No to No Yes order
factor_cols_display <- wvs7_var_index_displayed$Col_ID[
  wvs7_var_index_displayed$Variable_Display_Type == "factor"
  ]

indiv <- indiv %>%
  mutate(
    across(
      all_of(factor_cols_display),
      ~ {
        x <- as.factor(.x)
        if (identical(levels(x), c("Yes", "No"))) {
          factor(x, levels = c("No", "Yes"), ordered = is.ordered(x))
        } else {
          x
        }
      }
    )
  )



#' -------------------------------------

#' Writing out the dataset
readr::write_rds(indiv, 
          "data/WVS7_Individual.rds",
          compress = "gz")




#=================================================================================================================================
#' Processed variable codebook and markdown documentation
#'
#' This section reads the full processed codebook generated for WorldView and
#' writes a clean Markdown codebook containing only the variables displayed to
#' users in the app. It is safe to rerun whenever the setup pipeline is rerun.
#=================================================================================================================================

md_clean <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("\\|", "\\\\|", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

md_lines <- c(
  "# WorldView WVS Wave 7 Codebook",
  "",
  "This codebook describes the processed variables displayed in 
  the WorldView Shiny app. Original negative missing and 
  non-response codes are simplified to `NA`. The values displayed here
  are the processed values used by this app",
  "",
  "The origional WVS codebook and data documentation for the Wave 7 
  can be [found here](https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp).",
  "",
  "Please note that the WorldView app displays only a small subset of
  variable from the WVS for teaching purposes.",
  ""
)

for (section_i in unique(wvs7_var_index_displayed$Section)) {
  
  section_df <- wvs7_var_index_displayed[
    wvs7_var_index_displayed$Section == section_i, 
    ]
  
  md_lines <- c(md_lines, paste0("## ", md_clean(section_i)), "")

  for (i in seq_len(nrow(section_df))) {
    
    row_i <- section_df[i, ]
    
    md_lines <- c(
      md_lines,
      paste0("### ", row_i$Col_ID, ": ", md_clean(row_i$Short_Label)),
      paste0("**Question:** ", md_clean(row_i$Question_Text)),
      ""
    )

    md_lines <- c(
      md_lines,
      paste0("**Variable class:** ", md_clean(row_i$Variable_Display_Type)),
      ""
    )

    if(row_i$Variable_Display_Type == "factor_ordered"){
      
      values <- levels(indiv[ ,row_i$Col_ID])
      
      md_lines <- c(md_lines, 
                    "**Values**",
                    paste0("- ", 1:length(values), " = ", values),
                    ""
      )

    }else if(row_i$Variable_Display_Type == "factor"){
      
      values <- levels(indiv[ ,row_i$Col_ID])
      
      md_lines <- c(md_lines, 
                    "**Values**",
                    paste0("- ", values),
                    ""
                    )
      
    }else if(row_i$Variable_Display_Type == "integer"){
      
      md_lines <- c(md_lines, 
                    "**Values**",
                    paste0("Integer (min = ",
                           min(indiv[ ,row_i$Col_ID], na.rm = T),
                           " max = ",
                           max(indiv[ ,row_i$Col_ID], na.rm = T),
                           ")"
                           ),
                    ""
      )

    }else{
      
      stop("Error: Variable_Display_Type not recognised")
      
    }
  }
}


readr::write_lines(md_lines, "www/codebook.md")

#=================================================================================================================================
#=================================================================================================================================
#=================================================================================================================================
#' Country Level Summary Data

#' Notes:
#'  - It would be nice to incorporate other datasets here

#' Reading in individual level data
#indiv <- read_rds("data/WVS7_Individual.rds")
#' 
#' 
#' #' Function for generating summary stats for a variable
#' sum_fun <- function(data, # df 
#'                     v, # column name
#'                     min_n = 10 # minimum number of entries in a country to average
#'                     ){
#'   
#'   # Extract column
#'   d_col <- data[, v]
#'   
#'   #' Class of data in column
#'   d_class <- paste0(class(d_col), collapse = " ")
#'   
#'   #; Number of observations in d_col
#'   n_notNA <- sum(!is.na(d_col))
#'   
#'   # If integer or numeric, get mean
#'   if(d_class == "integer" | d_class == "numeric"){
#'     
#'     #' If there are fewer than min_n observations return NA
#'     if(n_notNA < min_n) {
#'       d_sum <- NA
#'       
#'       names(d_sum) <- v
#'       
#'       return(d_sum)
#'       
#'     } else{
#'       d_sum <- mean(d_col, na.rm = T)
#'       
#'       names(d_sum) <- v
#'       
#'       return(d_sum)
#'     }
#'     
#'     # If ordered factor, treat as numeric and get mean  
#'   }else if(d_class == "ordered factor"){
#'     
#'     #' If there are fewer than min_n observations return NA
#'     if(n_notNA < min_n) {
#'       d_sum <- NA
#'       
#'       names(d_sum) <- v
#'       
#'       return(d_sum)
#'       
#'     } else{
#'       d_sum <- mean(as.numeric(d_col), na.rm = T)
#'       
#'       names(d_sum) <- v
#'       
#'       return(d_sum)
#'     }
#'     
#'     # Is unordered factor, get proportions of each level
#'   }else if (d_class == "factor") {
#'     #' Return proportion of each
#'     d_levels <- levels(d_col)
#'     
#'     d_tbl <- table(d_col) %>%
#'       as.matrix() / n_notNA
#'     
#'     d_sum <- d_tbl[, 1]
#'     
#'     new_names <- tm::removePunctuation(names(d_sum))
#'     new_names <- gsub(" ", "_", new_names)
#'     
#'     names(d_sum) <- paste(v, new_names, sep = ".")
#'     
#'     if (n_notNA < min_n) {
#'       d_sum[1:length(d_sum)] <- NA
#'     }
#'     return(d_sum)
#'     
#'     #' If the column is of a different format stop with an error
#'   } else{
#'     stop("Unsupported class")
#'   }
#' }
#' 
#' 
#' #' Function for applying sum_fun to each of the desired columns in a dataframe,
#' #' subset by country
#' country_sum <- function(data, country, cols = cols_display) {
#'   #' Country wanted
#'   i_d <- data[data$B_COUNTRY == country, ]
#'   
#'   #' Applying sum_fun to columns
#'   i_d_sum <- lapply(cols_display, sum_fun, data = i_d)
#'   
#'   #' Getting the output in a df
#'   i_d_sum <- lapply(i_d_sum, rbind)
#'   
#'   i_d_sum <- do.call(cbind, i_d_sum) %>%
#'     as.data.frame()
#'   
#'   #' Adding in country column
#'   i_d_sum$B_COUNTRY <- country
#'   i_d_sum <- dplyr::relocate(i_d_sum, B_COUNTRY, .before = colnames(i_d_sum)[1])
#'   
#'   i_d_sum$B_COUNTRY_ALPHA <- unique(i_d$B_COUNTRY_ALPHA)
#'   i_d_sum <- dplyr::relocate(i_d_sum, B_COUNTRY_ALPHA, .before = colnames(i_d_sum)[1])
#'   
#'   return(i_d_sum)
#'   
#' }
#' 
#' #' List of all countries to apply country_sum to
#' countries <- unique(indiv$B_COUNTRY)
#' 
#' #' Applying country_sum function to all countries
#' country_sum_output <- lapply(countries, country_sum, data = indiv)
#' 
#' #' Converting the output from a list of data.frames to a data.frame
#' country_sum_output <- plyr::ldply(country_sum_output , data.frame)
#' 
#' #' Saving out the country level summary data
#' readr::write_rds(country_sum_output, "data/WVS7_Country.rds")


#=================================================================================================================================
#=================================================================================================================================
#=================================================================================================================================

#' Picker List

orig_UNSD_data <- readxl::read_excel("data/UNSD — Methodology.xlsx")

# load intermediaries
UNSD_countries_list <- orig_UNSD_data[,c(4,6,8,12)]
WVS7_part_countries <- indiv_data[, c("B_COUNTRY", "B_COUNTRY_ALPHA")]
WVS7_part_countries <- WVS7_part_countries[!duplicated(WVS7_part_countries$B_COUNTRY), ]

# countries and questions simple lists
WVS7_countries_list <- levels(orig_country_data$B_COUNTRY)
WVS7_iso_list <- levels(orig_country_data$B_COUNTRY_ALPHA)

# picker lists
WVS7_part_countries <- WVS7_part_countries %>%
  dplyr::left_join(
    UNSD_countries_list %>%
      dplyr::select(`ISO-alpha3 Code`, `Region Name`),
    by = c("B_COUNTRY_ALPHA" = "ISO-alpha3 Code")
  ) %>%
  dplyr::mutate(`Region Name` = coalesce(`Region Name`, 
                                         "Not defined"))


picker_country_list <- WVS7_part_countries %>%
  dplyr::arrange('Region Name', 'B_COUNTRY') %>%
  dplyr::group_by('Region Name') %>%
  dplyr::summarise(
    Countries = list(stats::setNames(B_COUNTRY_ALPHA, 
                                     B_COUNTRY)),
    .groups = "drop") %>%
  tibble::deframe()

write_rds(picker_country_list, "data/picker_country_list.rds")


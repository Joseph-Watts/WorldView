#' To do:
#' Consider including some of the country level additional data (e.g. HDI etc)

#' -----------------------------------------------------------------------------
#' Step 1: Setting up the data
#' -----------------------------------------------------------------------------

#' Reading in the WVS Wave 7 Data
#' Note: This is not in a standard R format, the data is haven formatted which
#' creates some issues for other functions
d <- read_rds("WVS_Dataset/WVS_Cross-National_Wave_7_rds_v6_0.rds")

#' Manually coded table with variables wanted and their classification
d_vars_coded <- readxl::read_xlsx("WVS_Dataset/Codebook manual coded index.xlsx")


#' Reading in variable information
d_vars_coded <- readxl::read_xlsx("WVS_Dataset/Codebook manual coded index.xlsx")
#' Converting character to logical
d_vars_coded$Variable_Display_Logical <- as.logical(d_vars_coded$Variable_Display_Logical)

#' Columns that will be displayed
cols_display <- d_vars_coded$Col_ID[d_vars_coded$Variable_Display_Logical]

#' These are manually coded classification of how the variable should be
#' presented in the app
table(d_vars_coded$Variable_Display_Type)

#' Selecting variables wanted and converting to a data.frame
indiv <- as.data.frame(d[, d_vars_coded$Col_ID])

#' Converting all ordinal haven format variables to standard ordered factors
for(i in 1:nrow(d_vars_coded)){
  
  #' Variable ID
  i_ID <- d_vars_coded$Col_ID[i]
  
  #' This variable does not need processing further
  if(i_ID == "B_COUNTRY_ALPHA"){next}
  
  #' raw data for column i
  i_d <- indiv[ , i_ID]
  
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
    indiv[ , i_ID] <- i_d_updated
    
  }else if(d_vars_coded$Variable_Display_Type[i] == "integer") {
    #' Treating variable as numeric
    i_d_integer <- as.integer(i_d)
    
    #' Replacing codes for missing data with NA
    i_d_integer <- ifelse(i_d_integer < 0, NA, i_d_integer)
    
    #' Updating variable
    indiv[, i_ID] <- i_d_integer
    
  } else{
    stop(
      paste0(
        d_vars_coded$Variable_Display_Type[i],
        " in d_vars_coded$Variable_Display_Type[",
        i,
        "]. Code not currently set up to handle this input."
      )
    )
  }
}

#' -------------------------------------
#' This sections is being shifted from data wrangling to here

# Ignored questions (given the number of factors they have or any other condition)
ignored_questions <- c("Q223", # political parties for each country - almost 1000 different factors
                       "Q266", # birth place - basically all countries ~ 200 factors
                       "Q267", # birth place - basically all countries ~ 200 factors
                       "Q268", # birth place - basically all countries ~ 200 factors
                       "Q272", # language groupings - # different factors
                       "Q290") # ethnic groupings - # different factors


# transform into ordinal
indiv_ordinal <- as.data.frame(lapply(indiv, function(col) {
  if (is.ordered(col)) {
    as.numeric(col)
  } else {
    col
  }
}))

# transformation of non-ordinal data into numerical
indiv_ordinal <- indiv_ordinal[, lubridate::setdiff(names(indiv_ordinal), 
                                                    ignored_questions)] %>%
  dplyr::mutate(
    Q56 = dplyr::case_when(
      Q56 == "Better off" ~ 1,
      Q56 == "Worse off" ~ -1,
      Q56 == "Or about the same" ~ 0,
      TRUE ~ NA_real_  # Keep NA as is
    )
  ) %>% ######################################  ######################################
dplyr::mutate(
  Q57 = dplyr::case_when(
    Q57 == "Most people can be trusted" ~ 1,
    Q57 == "Need to be very careful" ~ 0,
    TRUE ~ NA_real_
  )
) %>% ######################################  ######################################
dplyr::mutate(Q91 = dplyr::case_when(
  Q91 == "France" ~ 2,
  Q91 == "China" ~ 1,
  Q91 == "India" ~ 0,
  TRUE ~ NA_real_
)) %>% ######################################  ######################################
dplyr::mutate(Q92 = dplyr::case_when(
  Q92 == "Washington DC" ~ 2,
  Q92 == "London" ~ 1,
  Q92 == "Geneva" ~ 0,
  TRUE ~ NA_real_
)) %>% ######################################  ######################################
dplyr::mutate(
  Q93 = dplyr::case_when(
    Q93 == "Climate change" ~ 2,
    Q93 == "Human rights" ~ 1,
    Q93 == "Destruction of historic monuments" ~ 0,
    TRUE ~ NA_real_
  )
) %>% ######################################  ######################################
dplyr::mutate(dplyr::across(
  c(Q94, Q95, Q96, Q97, Q98, Q99, Q100, Q101, Q102, Q103, Q104, Q105),
  ~ dplyr::case_when(
    . == "Active member" ~ 1,
    . == "Inactive member" ~ -1,
    . == "Don't belong" | . == "Not a member" ~ 0,
    TRUE ~ NA_real_
  )
)) %>% ######################################  ######################################
dplyr::mutate(
  Q111 = dplyr::case_when(
    Q111 == "Protecting environment" ~ 2,
    Q111 == "Economy growth and creating jobs" ~ 1,
    Q111 == "Other answer" ~ 0,
    TRUE ~ NA_real_
  )
) %>% ######################################  ######################################
dplyr::mutate(dplyr::across(
  c(Q139, Q140, Q141, Q144, Q145, Q151, Q165, Q166, Q167, Q168, Q269, Q285),
  ~ dplyr::case_when(. == "Yes" ~ 1, . == "No" ~ 0, TRUE ~ NA_real_)
)) %>% ######################################  ######################################
dplyr::mutate(dplyr::across(
  c(Q149, Q150),
  ~ dplyr::case_when(. == "Freedom" ~ 1,
                     . == "Equality" | . == "Security" ~ 0, 
                     TRUE ~ NA_real_)
)) %>% ######################################  ######################################
dplyr::mutate(dplyr::across(
  c(Q152, Q153),
  ~ dplyr::case_when(
    . == "A high level of economic growth" ~ 3,
    . == "Making sure this country has strong defence forces" ~ 2,
    . == "Seeing that people have more say about how  are done at their jobs and in their communities" ~ 1,
    . == "Trying to make our cities and countryside more beautiful" ~ 0,
    TRUE ~ NA_real_
  )
)) %>% ######################################  ######################################
dplyr::mutate(dplyr::across(
  c(Q154, Q155),
  ~ dplyr::case_when(
    . == "Maintaining order in the nation" ~ 3,
    . == "Giving people more say in important government decisions" ~ 2,
    . == "Fighting rising prices" ~ 1,
    . == "Protecting freedom of speech" ~ 0,
    TRUE ~ NA_real_
  )
)) %>% ######################################  ######################################
dplyr::mutate(dplyr::across(
  c(Q156, Q157),
  ~ dplyr::case_when(
    . == "A stable economy" ~ 3,
    . == "Progress toward a less impersonal and more humane society" ~ 2,
    . == "Progress toward a society in which Ideas count more than money" ~ 1,
    . == "The fight against crime" ~ 0,
    TRUE ~ NA_real_
  )
)) %>% ######################################  ######################################
dplyr::mutate(
  Q173 = dplyr::case_when(
    Q173 == "A religious person" ~ 1,
    Q173 == "Not a religious person" ~ 0,
    Q173 == "An atheist" ~ -1,
    TRUE ~ NA_real_
  )
) %>% ######################################  ######################################
dplyr::mutate(
  Q174 = dplyr::case_when(
    Q174 == "Follow religious norms and ceremonies" ~ 1,
    Q174 == "Do good to other people" ~ 0,
    TRUE ~ NA_real_
  )
) %>% ######################################  ######################################
dplyr::mutate(
  Q175 = dplyr::case_when(
    Q175 == "Make sense of life after death" ~ 1,
    Q175 == "Make sense of life in this world" ~ 0,
    TRUE ~ NA_real_
  )
) %>% ######################################  ######################################
dplyr::mutate(
  Q260 = dplyr::case_when(
    Q260 == "Male" ~ 1,
    Q260 == "Female" ~ 0,
    TRUE ~ NA_real_
  )) %>% ######################################  ######################################
dplyr::mutate(
  Q263 = dplyr::case_when(
    Q263 == "I am born in this country" ~ 1,
    Q263 == "I am an immigrant to this country (born outside this country)" ~ 0,
    TRUE ~ NA_real_
  )
) %>% ######################################  ######################################
dplyr::mutate(dplyr::across(
  c(Q264, Q265),
  ~ dplyr::case_when(. == "Immigrant" ~ 1, . == "Not an immigrant" ~ 0, TRUE ~ NA_real_)
)) %>% ######################################  ######################################
dplyr::mutate(
  Q271 = dplyr::case_when(
    Q271 == "Yes, both own parent(s) and parent(s) in law" ~ 3,
    Q271 == "Yes, own parent(s)" ~ 2,
    Q271 == "Yes, parent(s) in law" ~ 1,
    Q271 == "No" ~ 0,
    TRUE ~ NA_real_
  )
) %>% ######################################  ######################################
dplyr::mutate(
  Q273 = dplyr::case_when(
    Q273 == "Married" ~ 5,
    Q273 == "Living together as married" ~ 4,
    Q273 == "Divorced" ~ 3,
    Q273 == "Separated" ~ 2,
    Q273 == "Widowed" ~ 1,
    Q273 == "Single" ~ 0,
    TRUE ~ NA_real_
  )
) %>% ######################################  ######################################
dplyr::mutate(dplyr::across(
  c(Q279, Q280),
  ~ dplyr::case_when(
    . == "Full time (30 hours a week or more)" ~ 9,
    . == "Part time (less than 30 hours a week)" ~ 8,
    . == "Self employed" ~ 7,
    . == "Military Service (EVS)" ~ 6,
    . == "Homemaker not otherwise employed" ~ 5,
    . == "Student" ~ 4,
    . == "Retired/pensioned" ~ 3,
    . == "Disabled (EVS)" ~ 2,
    . == "Unemployed" ~ 1,
    . == "Other" ~ 0,
    TRUE ~ NA_real_
  )
)) %>% ######################################  ######################################
dplyr::mutate(dplyr::across(
  c(Q281, Q282, Q283),
  ~ dplyr::case_when(
    . == "Higher administrative (for example: banker, executive in big business, high government official, union official)" ~ 11,
    . == "Professional and technical (for example: doctor, teacher, engineer, artist, accountant, nurse)" ~ 10,
    . == "Skilled worker (for example: foreman, motor mechanic, printer, seamstress, tool and die maker, electrician)" ~ 9,
    . == "Clerical (for example: secretary, clerk, office manager, civil servant, bookkeeper" ~ 8,
    . == "Sales (for example: sales manager, shop owner, shop assistant, insurance agent, buyer)" ~ 7,
    . == "Service (for example: restaurant owner, police officer, waitress, barber, caretaker)" ~ 6,
    . == "Semi-skilled worker (for example: bricklayer, bus driver, cannery worker, carpenter, sheet metal worker, baker)" ~ 5,
    . == "Farm owner, farm manager" ~ 4,
    . == "Farm worker (for example: farm labourer, tractor driver)" ~ 3,
    . == "Unskilled worker (for example: labourer, porter, unskilled factory worker, cleaner)" ~ 2,
    . == "Never had a job" ~ 1,
    . == "JP,KG,TJ: Other" ~ 0,
    TRUE ~ NA_real_
  )
)) %>% ######################################  ######################################
dplyr::mutate(
  Q284 = dplyr::case_when(
    Q284 == "Private business or industry" ~ 2,
    Q284 == "Private non-profit organization" ~ 1,
    Q284 == "Government or public institution" ~ 0,
    TRUE ~ NA_real_
  )
) %>% ######################################  ######################################
dplyr::mutate(
  Q289 = dplyr::case_when(
    Q289 == "Do not belong to a denomination" ~ 9,
    Q289 == "Catholic (Roman/Greek/etc)" ~ 8,
    Q289 == "Protestant" ~ 7,
    Q289 == "Orthodox (Russian/Greek/etc.)" ~ 6,
    Q289 == "Jew" ~ 5,
    Q289 == "Muslim" ~ 4,
    Q289 == "Hindu" ~ 3,
    Q289 == "Buddhist" ~ 2,
    Q289 == "Other Christian (Jehova withness...)" ~ 1,
    Q289 == "Other" ~ 0,
    TRUE ~ NA_real_
  )
) %>% ######################################  ######################################
dplyr::mutate(
  E1_LITERACY = dplyr::case_when(
    E1_LITERACY == "Literate" ~ 1,
    E1_LITERACY == "Illiterate" ~ 0,
    TRUE ~ NA_real_
  )
) %>% ######################################  ######################################
dplyr::mutate(
  F_INTPRIVACY = dplyr::case_when(
    F_INTPRIVACY == "There were other people around who could follow the interview" ~ 1,
    F_INTPRIVACY == "There were no other people around who could follow the interview" ~ 0,
    TRUE ~ NA_real_
  )
) %>% ######################################  ######################################
dplyr::mutate(
  H_SETTLEMENT = dplyr::case_when(
    H_SETTLEMENT == "Capital city" ~ 4,
    H_SETTLEMENT == "Regional center" ~ 3,
    H_SETTLEMENT == "District center" ~ 2,
    H_SETTLEMENT == "Another city, town (not a regional or district center)" ~ 1,
    H_SETTLEMENT == "Village" ~ 0,
    TRUE ~ NA_real_
  )
) %>% ######################################  ######################################
dplyr::mutate(H_URBRURAL = dplyr::case_when(
  H_URBRURAL == "Urban" ~ 1, H_URBRURAL == "Rural" ~ 0, 
  TRUE ~ NA_real_))

# # picker lists
# WVS7_part_countries <- WVS7_part_countries %>%
#   dplyr::left_join(
#     UNSD_countries_list %>% 
#       dplyr::select(`ISO-alpha3 Code`, `Region Name`),
#     by = c("B_COUNTRY_ALPHA" = "ISO-alpha3 Code")
#   ) %>%
#   dplyr::mutate(`Region Name` = coalesce(`Region Name`, "Not defined"))
# 
# 
# picker_country_list <- WVS7_part_countries %>% 
#   dplyr::arrange('Region Name', 'B_COUNTRY') %>%
#   dplyr::group_by('Region Name') %>%
#   dplyr::summarise(
#     Countries = list(stats::setNames(B_COUNTRY_ALPHA, B_COUNTRY)), 
#     .groups = "drop") %>%
#   tibble::deframe()


#' -------------------------------------
#' Quick tidy up of data
#' 
#' Dropping levels of factor that do not occur
table(indiv_ordinal$B_COUNTRY) 
indiv_ordinal$B_COUNTRY <- factor(indiv_ordinal$B_COUNTRY)

table(indiv_ordinal$B_COUNTRY_ALPHA) 
indiv$B_COUNTRY_ALPHA <- factor(indiv$B_COUNTRY_ALPHA)

#' Dropping variables that will not be displayed
#' Columns that sum_fun should be applied to
indiv_ordinal <- indiv_ordinal[ , c("B_COUNTRY", 
                                    "B_COUNTRY_ALPHA", 
                                    "S007", 
                                    cols_display)]


#' Writing out the dataset
readr::write_rds(indiv_ordinal, 
          "WVS_Dataset/WVS7_Individual.rds",
          compress = "gz")
#=================================================================================================================================
#=================================================================================================================================
#=================================================================================================================================
#' Country Level Summary Data

#' Notes:
#' 
#' 1. Need to think about how to present information on whether ordered variables 
#' are reverse coded. Many variables are a little unintuitive as the larger
#' quantity has a smaller value.
#' 
#'  2. It would be nice to incorporate other datasets here

#' Reading in individual level data
d <- read_rds("WVS_Dataset/WVS7_Individual.rds")



#' Function for generating summary stats for a variable
sum_fun <- function(data, v, min_n = 10){
  
  # Extract column
  d_col <- data[, v]
  
  #' Class of data in column
  d_class <- paste0(class(d_col), collapse = " ")
  
  #; Number of observations in d_col
  n_notNA <- sum(!is.na(d_col))
  
  # If integer, get mean
  if(d_class == "integer" | d_class == "numeric"){
    
    #' If there are fewer than min_n observations return NA
    if(n_notNA < min_n) {
      d_sum <- NA
      
      names(d_sum) <- v
      
      return(d_sum)
      
    } else{
      d_sum <- mean(d_col, na.rm = T)
      
      names(d_sum) <- v
      
      return(d_sum)
    }
    
    # If ordered factor, treat as numeric and get mean  
  }else if(d_class == "ordered factor"){
    
    #' If there are fewer than min_n observations return NA
    if(n_notNA < min_n) {
      d_sum <- NA
      
      names(d_sum) <- v
      
      return(d_sum)
      
    } else{
      d_sum <- mean(as.numeric(d_col), na.rm = T)
      
      names(d_sum) <- v
      
      return(d_sum)
    }
    
    # Is unordered factor, get proportions of each level
  }else if (d_class == "factor") {
    #' Return proportion of each
    d_levels <- levels(d_col)
    
    d_tbl <- table(d_col) %>%
      as.matrix() / n_notNA
    
    d_sum <- d_tbl[, 1]
    
    new_names <- tm::removePunctuation(names(d_sum))
    new_names <- gsub(" ", "_", new_names)
    
    names(d_sum) <- paste(v, new_names, sep = ".")
    
    if (n_notNA < min_n) {
      d_sum[1:length(d_sum)] <- NA
    }
    return(d_sum)
    
    #' If the column is of a different format stop with an error
  } else{
    stop("Unsupported class")
  }
}


#' Function for applying sum_fun to each of the desired columns in a dataframe,
#' subset by country
country_sum <- function(data, country, cols = cols_display) {
  #' Country wanted
  i_d <- data[data$B_COUNTRY == country, ]
  
  #' Applying sum_fun to columns
  i_d_sum <- lapply(cols_display, sum_fun, data = i_d)
  
  #' Getting the output in a df
  i_d_sum <- lapply(i_d_sum, rbind)
  
  i_d_sum <- do.call(cbind, i_d_sum) %>%
    as.data.frame()
  
  #' Adding in country column
  i_d_sum$B_COUNTRY <- country
  i_d_sum <- dplyr::relocate(i_d_sum, B_COUNTRY, .before = colnames(i_d_sum)[1])
  
  i_d_sum$B_COUNTRY_ALPHA <- unique(i_d$B_COUNTRY_ALPHA)
  i_d_sum <- dplyr::relocate(i_d_sum, B_COUNTRY_ALPHA, .before = colnames(i_d_sum)[1])
  
  return(i_d_sum)
  
}

#' List of all countries to apply country_sum to
countries <- unique(d$B_COUNTRY)

#' Applying country_sum function to all countries
country_sum_output <- lapply(countries, country_sum, data = d)

#' Converting the output from a list of data.frames to a data.frame
country_sum_output <- plyr::ldply(country_sum_output , data.frame)

#' Saving out the country level summary data
writexl::write_xlsx(country_sum_output, "WVS_Dataset/WVS7_Country.xlsx")
readr::write_rds(country_sum_output, "WVS_Dataset/WVS7_Country.rds")
#=================================================================================================================================
#=================================================================================================================================
#=================================================================================================================================
### Read in files
spssdata <- readRDS("WVS_Dataset/WVS_Cross-National_Wave_7_rds_v6_0.rds")
C.data <- readRDS("WVS_Dataset/WVS7_Country.rds")
I.data <- readRDS("WVS_Dataset/WVS7_Individual.rds")
CB_var_info <- readxl::read_xlsx("WVS_Dataset/Codebook manual coded index.xlsx")


### Main SPSS Dataset can't be handled directly, so create a data dictionary.
dict <- labelled::generate_dictionary(spssdata)

### dict$label is a list of the labels with attr info, so extract just the label name.
labels <- vector("character", length = nrow(dict))
for (x in 1:nrow(dict)) {
  labels[x] <- dict$label[[x]]
}

### Create a data frame mapping the data dictionary variables with the extracted labels.
mapped.vars.labels <- as.data.frame(cbind(dict$variable, labels))
colnames(mapped.vars.labels) <- c("Col_ID", "Col_Label")

### Using the mapping to add a new column to the Codebook to give a longer ID with meaningful name, eg "Q1 - meaning of Q1".
CB_var_info <- dplyr::inner_join(CB_var_info, mapped.vars.labels)
CB_var_info$ColLab <- paste0(CB_var_info$Col_ID, "-", CB_var_info$Col_Label)

# Save new codebook updated with labels
writexl::write_xlsx(CB_var_info,
                    "WVS_Dataset/WVS7_Codebook_updated_labels.xlsx")

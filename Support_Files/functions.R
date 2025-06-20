library(tidyverse)
library(gtsummary)
library(readxl)



#####################
# SUPPORT FUNCTIONS #
#####################

#' ##### MOMENTARILY DISABLED
#' within_country_compare <- function(data, v1, v2, Country){
#' 
#'   comp_d <- data[data$B_COUNTRY == Country, c(v1, v2)] %>% na.omit()
#' 
#'   v1_class <- paste0(class(comp_d[,v1]), collapse = "")
#'   v2_class <- paste0(class(comp_d[,v2]), collapse = "")
#'   v_classes <- c(v1_class, v2_class)
#'   
#'   #' If both variables are factors (whether ordered or not)
#'   if(sum(v_classes == "integer") == 0){
#' 
#'     #' Heat map
#'     v_plot <- ggplot(comp_d, 
#'                 aes(x=.data[[v1]],
#'                     y=.data[[v2]])) + 
#'       geom_bin_2d() +
#'       theme_minimal()
#' 
#'     # v_table <- tbl_summary(comp_d)
#'     
#'     #' If both factors are ordered perform a kendall cor.test
#'     if(sum(v_classes == "orderedfactor") == 2){
#'       
#'       comp_d_int <- comp_d
#'       comp_d_int[,v1] <- as.integer(comp_d_int[,v1])
#'       comp_d_int[,v2] <- as.integer(comp_d_int[,v2])
#'       
#'       v_stats <- cor.test(comp_d_int[,v1], 
#'                           comp_d_int[,v2],
#'                           method = "kendall")
#'       
#'     #' Otherwise, perform a chisq.test
#'     }else{
#'       
#'       v_stats <- chisq.test(comp_d[,v1], comp_d[,v2])
#'     
#'     }
#' 
#'   # If only one variables is an integer
#'   }else if(sum(v_classes == "integer") == 1){
#' 
#'     #' First, make sure that v2 is treated as the integer no matter the order
#'     #' that it is put in
#'     if(v1_class == "integer"){
#'       v1_orig <- v1
#'       v1 <- v2
#'       v2 <- v1_orig
#'     }
#'     
#'     #' Violin plot
#'     v_plot <- ggplot(comp_d, 
#'                 aes(x = .data[[v1]],
#'                     y = .data[[v2]],
#'                     fill = .data[[v1]])) + 
#'       geom_violin(trim = FALSE) +
#'       geom_jitter(shape = 16, 
#'                   position = position_jitter(0.1),
#'                   alpha = 0.2) +
#'       scale_fill_brewer(palette = "Pastel2") +
#'       theme_minimal()
#' 
#'     # v_table <- tbl_summary(comp_d)
#'     
#'     if("factor" %in% v_classes){
#'       
#'       v_stats <- kruskal.test(as.formula(paste(v1, "~", v2)),
#'           data = comp_d)
#'       
#'     }else if("orderedfactor" %in% v_classes){
#'       
#'       comp_d_int <- comp_d
#'       comp_d_int[,v1] <- as.integer(comp_d_int[,v1])
#' 
#'       v_stats <- cor.test(comp_d_int[,v1], 
#'                           comp_d_int[,v2],
#'                           method = "kendall")
#'       
#'     }
#'     
#'   #' If both variables are integers
#'   } else if(sum(v_classes == "integer") == 2){
#'     
#'     #' Scatter plot with jitter
#'     v_plot <- ggplot(comp_d, 
#'                 aes(x = .data[[v2]],
#'                     y = .data[[v1]])) + 
#'       geom_point() +
#'       geom_jitter(shape = 16, 
#'                   position = position_jitter(0.4),
#'                   alpha = 0.4) +
#'       geom_smooth(method=lm) +
#'       theme_minimal()
#'     
#'     # v_table <- tbl_summary(comp_d)
#'     
#'     comp_d_int <- comp_d
#'     comp_d_int[,v1] <- as.integer(comp_d_int[,v1])
#'     comp_d_int[,v2] <- as.integer(comp_d_int[,v2])
#'     
#'     v_stats <- cor.test(comp_d_int[,v1], 
#'                         comp_d_int[,v2],
#'                         method = "kendall")
#'     
#'   }
#'   
#'   v_table <- tbl_summary(comp_d)
#'   
#'   return(list("plot" = v_plot, 
#'               "table" = v_table, 
#'               "stats" = v_stats))
#' 
# }

#' if(testing){
#'   
#'   #' Individual level data
#'   d_ind <- read_rds("WVS_Dataset/WVS7_Individual.rds")
#'   
#'   # Testing two ordered factors
#'   test_output <- within_country_compare(data = d_ind, 
#'                                         v1 = "Q1",
#'                                         v2 = "Q2",
#'                                         Country = "New Zealand")
#'   test_output$plot
#'   test_output$table
#'   test_output$stats
#'   
#'   #' Testing combining a factor and integer
#'   test_output <- within_country_compare(data = d_ind, 
#'                                         v1 = "Q46",
#'                                         v2 = "Q48",
#'                                         Country = "New Zealand")
#'   test_output$plot
#'   test_output$table
#'   test_output$stats
#'   
#'   #' Testing two integers
#'   test_output <- within_country_compare(data = d_ind, 
#'                                         v1 = "Q48",
#'                                         v2 = "Q176",
#'                                         Country = "New Zealand")
#'   test_output$plot
#'   test_output$table
#'   test_output$stats
#'   
#' }
#####

#####
# Identify factor variables and print the number of levels for each
factor_info <- sapply(indiv_ordinal, function(x) {
  if (is.factor(x)) {
    return(length(levels(x)))
  } else {
    return(NA)  # NA for non-factor variables
  }
})

# # Filter and print only factor variables
# factor_info <- factor_info[!is.na(factor_info)]
# print(factor_info)
# 
# length(factor_info)
#####

#####
# Print all factors for each factor variable
print_factor_levels <- function(data) {
  # Loop through columns of the data frame
  factor_columns <- names(data)[sapply(data, is.factor)]  # Select only factor columns
  
  # Iterate through factor columns and check the number of levels
  for (col_name in factor_columns) {
    column <- data[[col_name]]
    
    if (length(levels(column)) < 15) {  # Check for fewer than 15 levels
      cat(sprintf("Variable '%s' has %d levels:\n", col_name, length(levels(column))))
      print(levels(column))
      cat("\n")  # Add a blank line for readability
    }
  }
}

# print_factor_levels(orig_indiv_data)
# print_factor_levels(indiv_ordinal)
######


#####
# Sample data and keep the missing ratio for each variable
sample_with_missing_ratio <- function(data, sample_size) {
  # Get the total number of rows in the dataset
  total_rows <- nrow(data)
  
  # Calculate the sampled dataset
  sampled_data <- data %>%
    # For each column, sample missing and non-missing rows proportionally
    reframe(across(everything(), ~ {
      missing_indices <- which(is.na(.))
      non_missing_indices <- which(!is.na(.))
      
      # Number of missing and non-missing rows to sample
      num_missing <- round(sample_size * length(missing_indices) / total_rows)
      num_non_missing <- round(sample_size * length(non_missing_indices) / total_rows)
      
      # Sample indices for missing and non-missing values
      sampled_missing <- sample(missing_indices, size = num_missing, replace = FALSE)
      sampled_non_missing <- sample(non_missing_indices, size = num_non_missing, replace = FALSE)
      
      # Combine the sampled values
      combined_indices <- sort(c(sampled_missing, sampled_non_missing))
      .[combined_indices] # Return the sampled values
    }))
  
  return(sampled_data)
}

# sampled_data <- sample_with_missing_ratio(orig_indiv_data, sample_size = 2500)
# vis_miss(sampled_data)
#####


#####
# create a list of all Qs grouped by their category, but the value passed is their Q#
picker_Qs_list <- function(grouped_list) {
  lapply(grouped_list, function(group) {
    setNames(
      # Values passed to input (e.g., "Q1", "Q2")
      sub("^(Q[0-9]+).*", "\\1", group),
      # Labels shown to user
      group
    )
  })
}
#####


#####
# Function to get grouped questions
get_groupedQs_I <- function() {
  var_info <- orig_codebook_data
  sections <- as.list(unique(var_info$Section))
  sections_ord <- factor(var_info$Section, ordered = TRUE, levels = sections)
  testDD <- data.frame(group = sections_ord,
                       qvar = var_info$ColLab)
  choicesgrpQ <- split(testDD$qvar, testDD$group, lex.order = FALSE)
  choicesgrpQ <- choicesgrpQ[-1] # remove IDs and sequencing
  choicesgrpQ <- head(choicesgrpQ, -1) # remove interviewer obs
  choicesgrpQ
}
#####


#####
# Question ID mapping function
get_question_id <- function(label) {
  # Access the codebook data directly
  var_info <- orig_codebook_data
  var_info$Col_ID[var_info$ColLab == label]
}
#####


#####
# Function to generate Kendall's data
prepare_analysis_data <- function(raw_data, var1_label, var2_label, countries, sample_size, var_info) {
  # Get question IDs
  var1_id <- get_question_id(var1_label, var_info)
  var2_id <- get_question_id(var2_label, var_info)
  
  # Prepare data
  data <- raw_data
  if (!is.null(countries)) {
    data <- data %>% 
      filter(B_COUNTRY_ALPHA %in% countries)
  }
  
  # Sample data
  if (nrow(data) > sample_size) {
    data <- data %>% sample_n(sample_size)
  }
  
  data %>%
    select(var1 = !!var1_id, var2 = !!var2_id, country = B_COUNTRY) %>%
    mutate(
      var1 = convert_to_numeric(var1),
      var2 = convert_to_numeric(var2)
    ) %>%
    na.omit()
}
#####
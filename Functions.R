library(tidyverse)
library(gtsummary)
library(readxl)

within_country_compare <- function(data, v1, v2, Country){

  comp_d <- data[data$B_COUNTRY == Country, c(v1, v2)] %>% na.omit()

  v1_class <- paste0(class(comp_d[,v1]), collapse = "")
  v2_class <- paste0(class(comp_d[,v2]), collapse = "")
  v_classes <- c(v1_class, v2_class)
  
  #' If both variables are factors (whether ordered or not)
  if(sum(v_classes == "integer") == 0){

    #' Heat map
    v_plot <- ggplot(comp_d, 
                aes(x=.data[[v1]],
                    y=.data[[v2]])) + 
      geom_bin_2d() +
      theme_minimal()

    v_table <- tbl_summary(comp_d)
    
    #' If both factors are ordered perform a kendall cor.test
    if(sum(v_classes == "orderedfactor") == 2){
      
      comp_d_int <- comp_d
      comp_d_int[,v1] <- as.integer(comp_d_int[,v1])
      comp_d_int[,v2] <- as.integer(comp_d_int[,v2])
      
      v_stats <- cor.test(comp_d_int[,v1], 
                          comp_d_int[,v2],
                          method = "kendall")
      
    #' Otherwise, perform a chisq.test
    }else{
      
      v_stats <- chisq.test(comp_d[,v1], comp_d[,v2])
    
    }

  # If only one variables is an integer
  }else if(sum(v_classes == "integer") == 1){

    #' First, make sure that v2 is treated as the integer no matter the order
    #' that it is put in
    if(v1_class == "integer"){
      v1_orig <- v1
      v1 <- v2
      v2 <- v1_orig
    }
    
    #' Violin plot
    v_plot <- ggplot(comp_d, 
                aes(x = .data[[v1]],
                    y = .data[[v2]],
                    fill = .data[[v1]])) + 
      geom_violin(trim = FALSE) +
      geom_jitter(shape = 16, 
                  position = position_jitter(0.1),
                  alpha = 0.2) +
      scale_fill_brewer(palette = "Pastel2") +
      theme_minimal()

    v_table <- tbl_summary(comp_d)
    
    if("factor" %in% v_classes){
      
      v_stats <- kruskal.test(as.formula(paste(v1, "~", v2)),
          data = comp_d)
      
    }else if("orderedfactor" %in% v_classes){
      
      comp_d_int <- comp_d
      comp_d_int[,v1] <- as.integer(comp_d_int[,v1])

      v_stats <- cor.test(comp_d_int[,v1], 
                          comp_d_int[,v2],
                          method = "kendall")
      
    }
    
  #' If both variables are integers
  } else if(sum(v_classes == "integer") == 2){
    
    #' Scatter plot with jitter
    v_plot <- ggplot(comp_d, 
                aes(x = .data[[v2]],
                    y = .data[[v1]])) + 
      geom_point() +
      geom_jitter(shape = 16, 
                  position = position_jitter(0.4),
                  alpha = 0.4) +
      geom_smooth(method=lm) +
      theme_minimal()
    
    v_table <- tbl_summary(comp_d)
    
    comp_d_int <- comp_d
    comp_d_int[,v1] <- as.integer(comp_d_int[,v1])
    comp_d_int[,v2] <- as.integer(comp_d_int[,v2])
    
    v_stats <- cor.test(comp_d_int[,v1], 
                        comp_d_int[,v2],
                        method = "kendall")
    
  }
  
  return(list("plot" = v_plot, 
         "table" = v_table, 
         "stats" = v_stats))

}

if(testing){
  
  #' Individual level data
  d_ind <- read_rds("WVS_Dataset/WVS7_Individual.rds")
  
  # Testing two ordered factors
  test_output <- within_country_compare(data = d_ind, 
                                        v1 = "Q1",
                                        v2 = "Q2",
                                        Country = "New Zealand")
  test_output$plot
  test_output$table
  test_output$stats
  
  #' Testing combining a factor and integer
  test_output <- within_country_compare(data = d_ind, 
                                        v1 = "Q46",
                                        v2 = "Q48",
                                        Country = "New Zealand")
  test_output$plot
  test_output$table
  test_output$stats
  
  #' Testing two integers
  test_output <- within_country_compare(data = d_ind, 
                                        v1 = "Q48",
                                        v2 = "Q176",
                                        Country = "New Zealand")
  test_output$plot
  test_output$table
  test_output$stats
  
}


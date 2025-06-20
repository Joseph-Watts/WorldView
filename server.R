#' Work in progress
#' 
#' Code based on: https://shiny.posit.co/r/gallery/life-sciences/biodiversity-national-parks/
#' WVS data source: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp



#### server logic ####
################
# SERVER LOGIC #
################

shinyServer(
  
  function(input, output, session) {
    session$onSessionEnded(function() {
      stopApp()
    })
    
    ########################
    # Read in data files
    ########################
    
    # WVS7_Individual.rds
    get_I_data <- reactive({
      d <- orig_indiv_data
      d
    })
    
    # WVS7_Country.rds
    get_C_data <- reactive({
      d <- orig_country_data
      d
    })
    
    # Codebook - with updated ColLab (concatenating Col_Id with label)
    get_var_info <- reactive({
      d <- orig_codebook_data
      d$Variable_Display_Logical <- as.logical(d$Variable_Display_Logical)
      d
    })
    
    # orig_indiv_data modified to have full question as name of column
    get_I_longID <- reactive({
      d.I <- get_I_data()
      d.var_info <- get_var_info()
      
      for (i in 4:293) { # from Q1 to Q290
        names(d.I)[i] <- d.var_info$ColLab[i]
      }
      d.I
    })
    
    # Extract Country names in Individual dataset 
    get_countries <- reactive({
      d.I <- get_I_data()
      d.country_name <- unique(d.I$B_COUNTRY)
      d.country_name
    })
    
    # Extract Questions in Individual dataset
    get_questions_I <- reactive({
      d <- get_var_info()
      d.Qs <- d$ColLab[d$Variable_Display_Logical]
      d.Qs
    })
    
    # Create Question text List and their 'ID'
    get_questions_List <- reactive({
      d <- orig_codebook_data[, c(1, 2, 10)]
      d <- split(d, d$Section)
      c <- lapply(d, function(group) {
        setNames(group$Col_ID, group$ColLab)
        })
      c
    }) # TODO get the list ordered by 'Col_ID' not by 'Section'
    
    # Fetch just sections
    get_sectionsOrd <- reactive({
      var_info <- get_var_info()
      sections <- as.list(unique(var_info$Section))
      sections_ord <- unique(factor(var_info$Section, ordered = TRUE, levels = sections))
      sections_ord <- sections_ord[-1]
      sections_ord
    })
    
    
    ########################
    # PDF & CODEBOOK VIEW
    ########################
    
    # Master Survery Questionnaire PDF
    output$surveyview <- renderUI({
      tags$iframe(style = "height:100vh; width:100%; scrolling=yes",
                  src = "F00011012-WVS_WAVE_7_MASTER_QUESTIONNAIRE_2017-2021_ENGLISH.pdf")
    })
    
    # Codebook PDF
    output$codebookview <- renderUI({
      tags$iframe(style = "height:100vh; width:100%; scrolling=yes",
                  src = "F00011055-WVS7_Codebook_Variables_report_V6.0.pdf")
    })
    
    
    ########################
    # DataTables
    ########################
    
    # Data table - Individual responses
    output$Table_indiv <- DT::renderDataTable({
      DT::datatable(data = get_I_longID(), options = list(scrollX = TRUE))
    })
    
    # Data table - Country aggregate responses
    output$Table_country <- DT::renderDataTable({
      DT::datatable(data = get_C_data(), options = list(scrollX = TRUE))
    })
    
    
    ########################
    # Individual Stats
    ########################
    
    # Reactive control for selecting country 
    output$individualStats_selectCountry <- renderUI({
      pickerInput(
        inputId = "indivStats_country",
        label = "Select Country",
        choices = picker_country_list,
        multiple = FALSE,
        selected = "NZL",
        options = list(
          `live-search` = TRUE,
          `size` = 20
        )
      )
    })
    
    # Reactive control for selecting question
    output$individualStats_selectQuestion <- renderUI({
      pickerInput(
        inputId = "indivStats_question",
        label = "Select Question",
        choices = picker_Qs_list(grouped_questions),
        selected = grouped_questions[[1]][1],
        width = "100%",
        options = list(
          `live-search` = TRUE,
          `size` = 20
        )
      )
    })

    # Give total count of valid observations for selected question
    output$individualStats_totalObs <- renderTable({
      orig_indiv_data |>
        filter(B_COUNTRY_ALPHA == input$indivStats_country) |>
        summarise(
          'Valid observations' = sum(!is.na(.data[["Q1"]])),
          'Missing observations (NA)' = sum(is.na(.data[["Q1"]])),
          'Total observations' = n()
        )
    })
    
    # Show stats for selected question factor levels
    output$statsSelectedQuestion <- renderTable({
      orig_indiv_data |>
        filter(B_COUNTRY_ALPHA == input$indivStats_country) |>
        count(.data[[input$indivStats_question]], name = "Obs per factor") |>
        mutate(
          `%` = round(100 * `Obs per factor` / sum(`Obs per factor`), 2)
        )
    })
    
    sum_stats_filtering <- reactive({
      orig_indiv_data %>%
        filter(B_COUNTRY_ALPHA == input$indivStats_country) %>%
        select('COUNTRY NAME' = B_COUNTRY, 'COUNTRY CODE' = B_COUNTRY_ALPHA, all_of(input$indivStats_question))
    })
    
    output$datatable_filtered_country <- DT::renderDataTable({
      DT::datatable(data = sum_stats_filtering(), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    
    
    ########################
    # Country Stats
    ########################
    
    # Reactive control for selecting country
    output$countryStats_selectCountry <- renderUI({
      pickerInput(
        inputId = "ctryStats_country",
        label = "Select Country",
        choices = picker_country_list,
        multiple = FALSE,
        selected = "NZL",
        options = list(
          `live-search` = TRUE,
          `size` = 20
        )
      )
    })
    
    # Reactive control for selecting question
    output$countryStats_selectQuestion <- renderUI({
      pickerInput(
        inputId = "ctryStats_question",
        label = "Select Question",
        choices = picker_Qs_list(get_groupedQs_I()),
        selected = get_groupedQs_I()[[1]][1],
        width = "100%",
        options = list(
          `live-search` = TRUE,
          `size` = 20
        )
      )
    })
    
    output$response_by_region <- renderTable({
      # Step 1: Join region info
      df <- orig_indiv_data |>
        left_join(
          UNSD_countries_list |> select(iso_alpha3 = `ISO-alpha3 Code`, region = `Region Name`),
          by = c("B_COUNTRY_ALPHA" = "iso_alpha3")
        ) |>
        mutate(region = ifelse(is.na(region), "Not classified", region)) |>
        filter(!is.na(.data[[input$indivStats_question]]))
      
      # Step 2: Calculate count and percent
      region_summary <- df |>
        group_by(region, response = .data[[input$indivStats_question]]) |>
        summarise(n = n(), .groups = "drop") |>
        group_by(region) |>
        mutate(percent = round(100 * n / sum(n), 1)) |>
        ungroup()
      
      # Step 3: Combine percent + count into label
      region_summary_long <- region_summary |>
        mutate(label = paste0(n, "(", percent, "%)")) |>
        select(response, region, label)
      
      # Step 4: Set desired region order, with "Not classified" last
      desired_order <- c("Africa", "Americas", "Asia", "Europe", "Oceania", "Not classified")
      region_summary_long <- region_summary_long |>
        mutate(region = factor(region, levels = desired_order))
      
      # Step 5: Pivot wider
      wide_table <- region_summary_long |>
        pivot_wider(
          names_from = region,
          values_from = label,
          values_fill = "-"
        ) |>
        arrange(factor(response, levels = unique(df[[input$indivStats_question]])))
      
      wide_table
    })
    
    
    output$top_countries_by_response <- renderTable({
      # Let's assume we're tracking % answering "Very important"
      target_answer <- "Very important"
      
      orig_indiv_data |>
        filter(.data[[input$indivStats_question]] %in% target_answer) |>
        group_by(B_COUNTRY_ALPHA) |>
        summarise(
          Selected = n(),
          Total = sum(!is.na(orig_indiv_data[[input$indivStats_question]] &
                               orig_indiv_data$B_COUNTRY_ALPHA == B_COUNTRY_ALPHA)),
          Percent = round(100 * Selected / Total, 1),
          .groups = "drop"
        ) |>
        arrange(desc(Percent)) |>
        slice_head(n = 5)
    })
    
    
    
    ########################
    # Within country
    ########################
    
    # Reactive control for selected required country
    output$wc_country_sel <- renderUI({
      selectInput(
        inputId = "wc_c_select",
        label = "Select Country:",
        choices = get_countries(),
        selected = "New Zealand"
      )
    })
    
    # Give total count of observations for selected country
    output$c_total_obs <- renderTable({
      d <- get_I_longID() |>
        filter(B_COUNTRY == input$wc_c_select) |>
        summarise(Observations = n())
      d
    })
    
    # Reactive control for selecting question A
    output$wc_qA <- renderUI({
      selectInput(
        inputId = "wc_sel_qA",
        label = "Select Question A",
        choices = get_groupedQs_I(),
        selected = get_groupedQs_I()[[1]][1],
        # selected = get_groupedQs_I()[[1]][6],
        width = "100%"
      )
    })
    
    # Reactive control for selecting question B
    output$wc_qB <- renderUI({
      selectInput(
        inputId = "wc_sel_qB",
        label = "Select Question B",
        choices = get_groupedQs_I(),
        selected = get_groupedQs_I()[[1]][2],
        # selected = get_groupedQs_I()[[5]][1],
        width = "100%"
      )
    })
    
    # get the data for the chosen country and questions 
    get_country_data <- reactive({
      d <- get_I_longID() |>
        filter(B_COUNTRY == input$wc_c_select) |>
        select(input$wc_sel_qA, input$wc_sel_qB)
      d
    })
    
    # Show count stats for Question A factor levels
    output$stats_wc_qA <- renderTable({
      get_country_data() |>
        group_by(.data[[input$wc_sel_qA]]) |>
        summarise('Count' = n()) |>
        mutate('%' = round(100 * Count / sum(Count), 2))
    })
    
    # Show count stats for Question B factor levels
    output$stats_wc_qB <- renderTable({
      get_country_data() |>
        group_by(.data[[input$wc_sel_qB]]) |>
        summarise('Count' = n()) |>
        mutate('%' = round(100 * Count / sum(Count), 2))
    })
    
    # Finds the max scale for both 'count' charts
    wc_max_count <- reactive({
      data <- get_country_data()
      varA_counts <- table(data[[input$wc_sel_qA]])
      varB_counts <- table(data[[input$wc_sel_qB]])
      max(c(varA_counts, varB_counts), na.rm = TRUE)
    })
    
    # Plot Question A factor level counts
    output$plot_wc_qA_levels <- renderPlot({ # TODO adjust font-size of x and y labels to different browser window sizes
      plot_data <- get_country_data()
      plot_data[[input$wc_sel_qA]] <- addNA(plot_data[[input$wc_sel_qA]])
      levels(plot_data[[input$wc_sel_qA]])[is.na(levels(plot_data[[input$wc_sel_qA]]))] <- "NA"
      
      ggplot(plot_data,
             aes(x = .data[[input$wc_sel_qA]], fill = .data[[input$wc_sel_qA]])) +
        geom_bar() +
        scale_y_continuous(limits = c(0, wc_max_count())) +
        scale_fill_viridis(discrete = TRUE, option = "D") +
        ggtitle("Factor levels by frequency") +
        labs(x = input$wc_sel_qA, y = "Count") +
        theme_minimal() +
        theme(legend.position = "none")
    })
    
    # Plot Question A factor level proportions
    output$plot_wc_qA_prop <- renderPlot({
      ggplot(get_country_data(),
             aes(x = '', fill = .data[[input$wc_sel_qA]])) +
        geom_bar(position = "fill") +
        ggtitle("Factor level proportions") +
        labs(x = input$wc_sel_qA, y = "Proportion") +
        theme(legend.title = element_blank())
    })
    
    # Plot Question B factor level counts
    output$plot_wc_qB_levels <- renderPlot({
      plot_data <- get_country_data()
      plot_data[[input$wc_sel_qB]] <- addNA(plot_data[[input$wc_sel_qB]])
      levels(plot_data[[input$wc_sel_qB]])[is.na(levels(plot_data[[input$wc_sel_qB]]))] <- "NA"
      
      ggplot(plot_data,
             aes(x = .data[[input$wc_sel_qB]], fill = .data[[input$wc_sel_qB]])) +
        geom_bar() +
        scale_y_continuous(limits = c(0, wc_max_count())) +
        scale_fill_viridis(discrete = TRUE, option = "D") +
        ggtitle("Factor levels by frequency") +
        labs(x = input$wc_sel_qB, y = "Count") +
        theme_minimal() +
        theme(legend.position = "none")
    })
    
    # Plot Question B factor level proportions
    output$plot_wc_qB_prop <- renderPlot({
      ggplot(get_country_data(),
             aes(x = '', fill = .data[[input$wc_sel_qB]])) +
        geom_bar(position = "fill") +
        ggtitle("Factor level proportions") +
        labs(x = input$wc_sel_qB, y = "Proportion") +
        theme(legend.title = element_blank())
    })
    
    
    # Show comparison stats for each question grouped by the other
    output$stats_wc_qAqB <- renderTable({
      get_country_data() |>
        group_by(.data[[input$wc_sel_qA]], .data[[input$wc_sel_qB]]) |>
        summarise(n = n())
    }) 
    
    output$stats_wc_qBqA <- renderTable({
      get_country_data() |>
        group_by(.data[[input$wc_sel_qB]], .data[[input$wc_sel_qA]]) |>
        summarise(n = n())
    }) 
    
    within_country_compare <- reactive({
      comp_d <- get_country_data() |> na.omit()
      
      v1 <- input$wc_sel_qA
      v2 <- input$wc_sel_qB
      
      v1_class <- paste0(class(comp_d[,v1]), collapse = "")
      v2_class <- paste0(class(comp_d[,v2]), collapse = "")
      v_classes <- c(v1_class, v2_class)
      
      #' If both variables are factors (whether ordered or not)
      if(sum(v_classes == "integer") == 0){
        
        #' Heat map
        v_plot <- ggplot(comp_d,
                         aes(x = .data[[v1]],
                             y = .data[[v2]])) +
          geom_bin_2d() +
          scale_fill_viridis(option = "D") +
          theme_minimal()
        
        
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
          geom_violin(trim = FALSE,
                      alpha = 0.4) +
          geom_jitter(shape = 16,
                      position = position_jitter(0.15),
                      alpha = 0.3) +
          geom_boxplot(width = 0.1,
                       alpha = 0.7) +
          scale_fill_viridis(discrete = TRUE, option = "D") +
          theme_minimal() +
          theme(legend.position = "none")
        
        
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
                      position = position_jitter(0.15),
                      alpha = 0.3) +
          geom_boxplot(width = 0.1,
                       alpha = 0.7) +
          scale_fill_viridis(discrete = TRUE, option = "D") +
          geom_smooth(method = lm) +
          theme_minimal() +
          theme(legend.title = element_blank())
        
        
        comp_d_int <- comp_d
        comp_d_int[,v1] <- as.integer(comp_d_int[,v1])
        comp_d_int[,v2] <- as.integer(comp_d_int[,v2])
        
        v_stats <- cor.test(comp_d_int[,v1],
                            comp_d_int[,v2],
                            method = "kendall")
      }
      
      v_table <- tbl_summary(comp_d)
      # sum_tbl <- tbl_summary(comp_d)
      # v_table <- sum_tbl$table_body %>%
      #   select(variable, label, stat_0) %>%
      #   pivot_wider(names_from = variable, values_from = stat_0)
      
      
      return(list("plot" = v_plot, 
                  "table" = v_table, 
                  "stats" = v_stats))
      
    })
    
    ############################ TEST OUTPUT SECTION ##########################

    output$test_output_plot <- renderPlot({
      test_output <- within_country_compare()
      test_output$plot
    })

    output$test_output_table <- renderTable({
      test_output <- within_country_compare()
      test_output$table
    })

    output$test_output_stats <- renderPrint({
      test_output <- within_country_compare()
      print(test_output$stats)
    })

    ###########################################################################
 
    
    ########################
    # Between Countries
    ########################
    
    # Reactive control for selecting BC question
    output$bc_question <- renderUI({
      selectInput(
        inputId = "bc_sel_q",
        label = "Select Question",
        choices = get_groupedQs_I(),
        selected = get_groupedQs_I()[[1]][1],
        width = "100%"
      )
    })
    
    # Reactive control for selecting Country A
    output$bc_countryA <- renderUI({
      selectInput(
        inputId = "bc_sel_cA",
        label = "Select Country:",
        choices = get_countries(),
        selected = "New Zealand"
      )
    })
    
    
    # Reactive control for selecting Country B
    output$bc_countryB <- renderUI({
      selectInput(
        inputId = "bc_sel_cB",
        label = "Select Country:",
        choices = get_countries(),
        selected = "Argentina"
      )
    })
    
    # Get question data for Country A 
    get_countryA_data <- reactive({
      d <- get_I_longID() |>
        filter(B_COUNTRY == input$bc_sel_cA) |>
        select(input$bc_sel_q)
      d
    }) 
    
    # Give total count of observations for selected country
    output$cA_total_obs <- renderTable({
      d <- get_countryA_data() |>
        summarise(Observations = n())
      d
    })
    
    # Get question data for Country A 
    get_countryB_data <- reactive({
      d <- get_I_longID() |>
        filter(B_COUNTRY == input$bc_sel_cB) |>
        select(input$bc_sel_q)
      d
    })
    
    # Give total count of observations for selected country
    output$cB_total_obs <- renderTable({
      d <- get_countryB_data() |>
        summarise(Observations = n())
      d
    })
    
    # Show count stats for Question Country A factor levels
    output$stats_bc_cA <- renderTable({
      get_countryA_data() |>
        group_by(.data[[input$bc_sel_q]]) |>
        summarise('Count' = n()) |>
        mutate('%' = round(100 * Count / sum(Count), 2))
    }) 
    
    # Show count stats for Question Country B factor levels
    output$stats_bc_cB <- renderTable({
      get_countryB_data() |>
        group_by(.data[[input$bc_sel_q]]) |>
        summarise('Count' = n()) |>
        mutate('%' = round(100 * Count / sum(Count), 2))
    }) 
    
    # Finds the max scale for both 'count' charts
    bc_max_count <- reactive({
      var <- input$bc_sel_q
      countsA <- table(get_countryA_data()[[var]])
      countsB <- table(get_countryB_data()[[var]])
      max(c(countsA, countsB), na.rm = TRUE)
    })
    
    # Plot Question factor level counts for Country A
    output$plot_bc_qcA_levels <- renderPlot({
      plot_data <- get_countryA_data()
      plot_data[[input$bc_sel_q]] <- addNA(plot_data[[input$bc_sel_q]])
      levels(plot_data[[input$bc_sel_q]])[is.na(levels(plot_data[[input$bc_sel_q]]))] <- "NA"
      
      
      ggplot(plot_data,
             aes(x = .data[[input$bc_sel_q]], fill = .data[[input$bc_sel_q]])) +
        geom_bar() +
        scale_y_continuous(limits = c(0, bc_max_count())) +
        scale_fill_viridis(discrete = TRUE, option = "D") +
        ggtitle(paste0(input$bc_sel_cA,": Question factor levels by frequency")) +
        labs(x = input$bc_sel_q, y = "Count") +
        theme_minimal() +
        theme(legend.position = "none")
    })
    
    # Plot Question factor level proportions Country A
    output$plot_bc_qA_prop <- renderPlot({
      ggplot(get_countryA_data(),
             aes(x = '', fill = .data[[input$bc_sel_q]])) +
        geom_bar(position = "fill") +
        scale_fill_viridis(discrete = TRUE, option = "D") +
        ggtitle("Factor level proportions") +
        labs(x = input$bc_sel_q, y = "Proportion") +
        theme_minimal() +
        theme(legend.title = element_blank())
    })
    
    # Plot Question factor level counts for Country B
    output$plot_bc_qcB_levels <- renderPlot({
      plot_data <- get_countryB_data()
      plot_data[[input$bc_sel_q]] <- addNA(plot_data[[input$bc_sel_q]])
      levels(plot_data[[input$bc_sel_q]])[is.na(levels(plot_data[[input$bc_sel_q]]))] <- "NA"
      
      
      ggplot(plot_data,
             aes(x = .data[[input$bc_sel_q]], fill = .data[[input$bc_sel_q]])) +
        geom_bar() +
        scale_y_continuous(limits = c(0, bc_max_count())) +
        scale_fill_viridis(discrete = TRUE, option = "D") +
        ggtitle(paste0(input$bc_sel_cB,": Question factor levels by frequency")) +
        labs(x = input$bc_sel_q, y = "Count") +
        theme_minimal() +
        theme(legend.position = "none")
    })
    
    # Plot Question factor level proportions Country B
    output$plot_bc_qB_prop <- renderPlot({
      ggplot(get_countryB_data(),
             aes(x = '', fill = .data[[input$bc_sel_q]])) +
        geom_bar(position = "fill") +
        scale_fill_viridis(discrete = TRUE, option = "D") +
        ggtitle("Factor level proportions") +
        labs(x = input$bc_sel_q, y = "Proportion") +
        theme_minimal() +
        theme(legend.title = element_blank())
    })
    
    #' ---
    #' Global
    #' 
    #' This section looks at the global relationship between two variables. 
    #' 
    #' The country level dataset is very simple in that it provides country level 
    #' mean or proportions. Variation within countries has not been accounted for.
    #' 
    #' This is probably ok for the current simple purposes of the app. 
    #' More sophistication could be added in later.
    #' 

    # Reactive to get the filtered questions for varA
    filtered_choices_A <- reactive({
      # Get the list of all questions
      all_questions <- get_groupedQs_I()
      
      # Exclude the question selected in varB
      selected_B <- input$gbl_sel_varB
      all_questions[!all_questions %in% selected_B]
    })
    
    # Reactive to get the filtered questions for varB
    filtered_choices_B <- reactive({
      # Get the list of all questions
      all_questions <- get_groupedQs_I()
      
      # Exclude the question selected in varA
      selected_A <- input$gbl_sel_varA
      all_questions[!all_questions %in% selected_A]
    })
    
    
    # Reactive control for selecting first variable
    output$global_varA <- renderUI({
      selectInput(
        inputId = "gbl_sel_varA",
        label = "Select Question A",
        choices = filtered_choices_A(),
        selected = get_groupedQs_I()[[1]][1]
      )
    })
    
    # Reactive control for selecting second variable
    output$global_varB <- renderUI({
      selectInput(
        inputId = "gbl_sel_varB",
        label = "Select Question B",
        choices = filtered_choices_B(),
        selected = get_groupedQs_I()[[1]][2]
      )
    })
    
    gblvar1 <- reactive({
      var_info <- get_var_info()
      gblvar1_lookup <- var_info[which(var_info$ColLab == input$gbl_sel_varA), ]
      gv1input <- gblvar1_lookup$Col_ID
    })
    
    # gblvar2 <- reactive({
    #   gblvar2_lookup <- var_info[which(var_info$ColLab == input$Selected_Global_Var_2),]
    #   gv2input <- gblvar2_lookup$Col_ID
    # })
    
    gblvar2 <- reactive({
      var_info <- get_var_info()
      gblvar2_lookup <- var_info[which(var_info$ColLab == input$gbl_sel_varB), ]
      gv2input <- gblvar2_lookup$Col_ID
    })
    
    
    #' This reactions sections doesn't seem necessary.
    #' Something will need to change here to determine the correct plot type
    #' based on the 
    d_global <- reactive({
      d_country <- get_C_data()
      d_country[, c(gblvar1(), gblvar2())]
    })
    
    #' Here it would be good to add:
    #' 1. Global plot for variable 1
    #' 2. Global plot for variable 2
    #' 3. Summary information for each variable
    #' 4. Basic statistical test of the relationship between the variables
    
    #' This will need adapted to handle the different possible formats of the
    #' selected variables. Currently variables can be:
    #' Factor (responses at the country level have been split into different rows)
    #' Ordinal Factor (treated as integer and then mean)
    #' Integer (mean)
    
    output$global_p1 <- renderPlot({
      ggplot(d_global(), aes(x = .data[[gblvar1()]], y = .data[[gblvar2()]])) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        theme_minimal()
    })
    
    
    # Helper function to get question ID from label
    get_question_id <- function(label) {
      var_info <- get_var_info()
      var_info$Col_ID[var_info$ColLab == label]
    }
    
    ########################
    # Bar chart
    ########################
    
    # Bar plot server logic
    output$bar_plot <- renderPlotly({
      input$bar_update
      
      isolate({
        req(input$bar_question, input$bar_countries)
        
        # Get the question ID from the display name
        # var_info <- get_var_info()
        # q_id <- var_info$Col_ID[var_info$ColLab == input$bar_question]
        q_id <- get_question_id(input$bar_question)
        
        # Prepare data
        plot_data <- orig_indiv_data %>%
          filter(B_COUNTRY_ALPHA %in% input$bar_countries) %>%
          select(country = B_COUNTRY, response = !!q_id) %>%
          mutate(response = as.factor(response)) %>%
          count(country, response) %>%
          group_by(country) %>%
          mutate(percent = n / sum(n) * 100)
        
        # Create plot based on selected type
        if (input$bar_type == "Percentage") {
          p <- ggplot(plot_data, aes(x = response, y = percent, fill = country)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            labs(y = "Percentage (%)", title = paste("Distribution of", input$bar_question))
        } else {
          p <- ggplot(plot_data, aes(x = response, y = n, fill = country)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            labs(y = "Count", title = paste("Distribution of", input$bar_question))
        }
        
        p <- p +
          labs(x = "Response", fill = "Country") +
          scale_fill_viridis_d() +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(p) %>% 
          layout(legend = list(orientation = "h", y = -0.2))
      })
    })
    
    
    ########################
    # Scatterplot
    ########################
    
    output$scatter_plot <- renderPlotly({
      input$scatter_update
      
      isolate({
        req(input$scatter_x, input$scatter_y)
        
        # Get question IDs
        var_info <- get_var_info()
        x_id <- var_info$Col_ID[var_info$ColLab == input$scatter_x]
        y_id <- var_info$Col_ID[var_info$ColLab == input$scatter_y]
        
        # Prepare data
        plot_data <- orig_indiv_data
        if (!is.null(input$scatter_countries)) {
          plot_data <- plot_data %>% 
            filter(B_COUNTRY_ALPHA %in% input$scatter_countries)
        }
        
        # Sample data for performance
        if (nrow(plot_data) > input$scatter_sample) {
          plot_data <- plot_data %>% sample_n(input$scatter_sample)
        }
        
        plot_data <- plot_data %>%
          select(x = !!x_id, y = !!y_id, country = B_COUNTRY_ALPHA)
        
        # Create plot
        p <- ggplot(plot_data, aes(x = x, y = y, color = country)) +
          geom_point(alpha = 0.6) +
          geom_smooth(method = "lm", se = FALSE) +
          labs(title = paste(input$scatter_x, "vs", input$scatter_y),
               x = input$scatter_x, y = input$scatter_y) +
          theme_minimal()
        
        ggplotly(p)
      })
    })
    
    
    ########################
    # Corrplot
    ########################
    
    output$corr_plot <- renderPlot({
      input$corr_update
      
      isolate({
        req(input$corr_questions, length(input$corr_questions) > 1)
        
        # Get question IDs
        var_info <- get_var_info()
        q_ids <- sapply(input$corr_questions, function(q) {
          var_info$Col_ID[var_info$ColLab == q]
        })
        
        # Prepare data
        plot_data <- orig_indiv_data
        if (!is.null(input$corr_countries)) {
          plot_data <- plot_data %>% 
            filter(B_COUNTRY_ALPHA %in% input$corr_countries)
        }
        
        plot_data <- plot_data %>%
          select(all_of(q_ids)) %>%
          mutate(across(everything(), as.numeric))  # Convert to numeric for correlation
        
        # Compute correlation matrix
        cor_matrix <- cor(plot_data, 
                          use = "pairwise.complete.obs",
                          method = tolower(input$corr_method))
        
        # Create plot
        corrplot(cor_matrix, method = "color", type = "upper", 
                 tl.col = "black", tl.srt = 45, 
                 addCoef.col = "black", number.cex = 0.7,
                 title = "Correlation Matrix")
      })
    })
    
    ########################
    # Heatmap
    ########################
    
    # Reactive for map data
    map_data <- reactive({
      req(input$map_question)
      
      # Get question ID
      var_info <- get_var_info()
      q_id <- var_info$Col_ID[var_info$ColLab == input$map_question]
      
      # Calculate country-level values
      country_values <- orig_indiv_data %>%
        group_by(B_COUNTRY_ALPHA) %>%
        summarise(
          mean_val = mean(as.numeric(!!sym(q_id)), na.rm = TRUE),
          median_val = median(as.numeric(!!sym(q_id)), na.rm = TRUE),
          mode_val = as.numeric(names(sort(table(!!sym(q_id)), decreasing = TRUE)[1]))
        ) %>%
        rename(iso = B_COUNTRY_ALPHA)
      
      # Merge with world map
      world_available %>%
        left_join(country_values, by = c("iso_a3" = "iso"))
    })
    
    # Render the map
    output$map <- renderLeaflet({
      input$map_update
      
      isolate({
        data <- map_data()
        metric <- input$map_metric
        
        # Select appropriate column based on metric
        values <- switch(metric,
                         "Mean" = data$mean_val,
                         "Median" = data$median_val,
                         "Mode" = data$mode_val)
        
        # Create color palette
        pal <- colorNumeric("viridis", domain = values, na.color = "grey")
        
        # Create labels
        labels <- sprintf(
          "<strong>%s</strong><br/>%s: %.2f",
          data$name, metric, values
        ) %>% lapply(htmltools::HTML)
        
        leaflet(data,
                options = leafletOptions(
                  zoomControl = FALSE,
                  minZoom = 2,
                  maxZoom = 5,
                  dragging = TRUE)) %>%
          addProviderTiles("CartoDB.Positron") %>%
          setView(0, 20, zoom = 2) %>%
          addPolygons(
            layerId = ~iso_a3,
            label = labels,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"),
            weight = 1,
            color = "black",
            fillColor = ~pal(values),
            fillOpacity = 0.7,
            highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.8)
          ) %>%
          addLegend(
            "bottomright", 
            pal = pal, 
            values = values,
            title = paste(input$map_question, "<br>", metric),
            opacity = 1,
            labFormat = labelFormat(suffix = "")
          )
      })
    })
    
    
    # Function to calculate mode
    get_mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    # Function to create country picker list
    get_country_picker_list <- function() {
      countries <- unique(orig_indiv_data$B_COUNTRY_ALPHA)
      country_names <- unique(orig_indiv_data$B_COUNTRY)
      setNames(as.list(countries), country_names)
    }
    
    # Initialize picker_country_list in global.R
    picker_country_list <- get_country_picker_list()
    
    
    
    
    
    # Function to get question ID from label
    get_question_id <- function(label) {
      var_info <- get_var_info()
      var_info$Col_ID[var_info$ColLab == label]
    }
    
    # Function to convert variable to numeric if possible
    convert_to_numeric <- function(x) {
      if (is.factor(x) || is.character(x)) {
        as.numeric(as.character(x))
      } else {
        as.numeric(x)
      }
    }
    
    
    
    ########################
    # Kendall
    ########################
    
    # Reactive data preparation for Kendall's analysis
    kendall_data <- eventReactive(input$kendall_run, {
      # Require both variables to be selected
      req(input$kendall_var1, input$kendall_var2)
      
      # Get variable information
      var_info <- get_var_info()
      
      # Get question IDs from labels
      var1_id <- get_question_id(input$kendall_var1)
      var2_id <- get_question_id(input$kendall_var2)
      
      # Prepare data from preprocessed numeric dataset
      data <- indiv_ordinal
      
      # Apply country filter if selected
      if (!is.null(input$kendall_countries)) {
        data <- data %>% 
          filter(B_COUNTRY_ALPHA %in% input$kendall_countries)
      }
      
      # Apply sampling for performance
      if (nrow(data) > input$kendall_sample) {
        data <- data %>% sample_n(input$kendall_sample)
      }
      
      # Select relevant columns and omit missing values
      data %>%
        select(
          var1 = !!var1_id, 
          var2 = !!var2_id, 
          country = B_COUNTRY
        ) %>%
        na.omit()  # Remove any rows with missing values
    })

    # Render correlation results
    output$kendall_results <- renderPrint({
      # Get the prepared data
      data <- kendall_data()
      
      # Check for sufficient data
      if (nrow(data) < 3) {
        return("Insufficient data to compute correlation. Need at least 3 complete observations.")
      }
      
      # Compute Kendall's correlation
      cor_test <- cor.test(data$var1, data$var2, 
                           method = "kendall", 
                           exact = FALSE)
      
      # Format and display results
      cat("Kendall's Rank Correlation Analysis\n")
      cat("===================================\n")
      cat("Variable 1: ", input$kendall_var1, "\n")
      cat("Variable 2: ", input$kendall_var2, "\n")
      cat("Countries: ", paste(input$kendall_countries, collapse = ", "), "\n")
      cat("Number of complete observations: ", nrow(data), "\n\n")
      
      cat("Correlation coefficient (tau): ", 
          round(cor_test$estimate, 4), "\n")
      cat("95% Confidence Interval: [", 
          round(cor_test$conf.int[1], 4), ", ", 
          round(cor_test$conf.int[2], 4), "]\n")
      cat("p-value: ", format.pval(cor_test$p.value, digits = 4), "\n\n")
      
      cat("Interpretation:\n")
      tau <- abs(cor_test$estimate)
      if (tau > 0.7) {
        cat("- Very strong monotonic relationship\n")
      } else if (tau > 0.5) {
        cat("- Strong monotonic relationship\n")
      } else if (tau > 0.3) {
        cat("- Moderate monotonic relationship\n")
      } else if (tau > 0.1) {
        cat("- Weak monotonic relationship\n")
      } else {
        cat("- No meaningful monotonic relationship\n")
      }
      
      if (cor_test$p.value < 0.05) {
        cat("- Statistically significant at p < 0.05\n")
      } else {
        cat("- Not statistically significant at p < 0.05\n")
      }
    })
    
    # Render the scatter plot
    output$kendall_plot <- renderPlotly({
      # Get the prepared data
      data <- kendall_data()
      
      # Check for sufficient data
      if (nrow(data) < 3) {
        return(NULL)  # Don't render plot if insufficient data
      }
      
      # Create the plot
      p <- ggplot(data, aes(x = var1, y = var2, 
                            color = country,
                            text = paste("Country:", country,
                                         "<br>Var1:", round(var1, 2),
                                         "<br>Var2:", round(var2, 2)))) +
        geom_point(alpha = 0.6, size = 2) +
        geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
        labs(
          title = paste("Relationship between", input$kendall_var1, 
                        "and", input$kendall_var2),
          x = input$kendall_var1,
          y = input$kendall_var2,
          color = "Country"
        ) +
        scale_color_viridis_d(option = "plasma") +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          plot.title = element_text(size = 14, face = "bold")
        )
      
      # Convert to interactive plot
      ggplotly(p, tooltip = "text") %>% 
        layout(legend = list(orientation = "h", y = -0.2))
    })
    
    # Render data table
    output$kendall_data <- renderDT({
      # Get the prepared data
      data <- kendall_data()
      
      # Rename columns for display
      names(data) <- c(input$kendall_var1, input$kendall_var2, "Country")
      
      # Create datatable
      datatable(
        data,
        extensions = 'Buttons',
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf'),
          columnDefs = list(
            list(targets = 0, searchable = FALSE)  # Disable search on row numbers
          )
        ),
        rownames = FALSE,
        filter = 'top'
      )
    })
    
    
    
    ########################
    # ANOVA
    ########################
    
    # Reactive data preparation for ANOVA
    anova_data <- eventReactive(input$anova_run, {
      req(input$anova_var, input$anova_countries)
      
      # Get variable information
      var_info <- get_var_info()
      var_id <- get_question_id(input$anova_var)
      
      # Prepare data from preprocessed numeric dataset
      data <- indiv_ordinal
      
      # Apply country filter if selected
      if (!is.null(input$anova_countries)) {
        data <- data %>% 
          filter(B_COUNTRY_ALPHA %in% input$anova_countries)
      }
      
      # Apply sampling for performance
      if (nrow(data) > input$anova_sample) {
        data <- data %>% sample_n(input$anova_sample)
      }
      
      # Select relevant columns and omit missing values
      data %>%
        select(value = !!var_id, country = B_COUNTRY) %>%
        na.omit()  # Remove any rows with missing values
    })
    
    # Render ANOVA results
    output$anova_results <- renderPrint({
      data <- anova_data()
      
      # Check for sufficient data and groups
      if (nrow(data) < 10) {
        return("Insufficient data: Need at least 10 observations.")
      }
      
      if (length(unique(data$country)) < 2) {
        return("Insufficient groups: Need at least 2 countries.")
      }
      
      # Run ANOVA
      model <- aov(value ~ country, data = data)
      
      # Display results
      cat("Analysis of Variance (ANOVA)\n")
      cat("============================\n")
      cat("Variable: ", input$anova_var, "\n")
      cat("Countries: ", paste(unique(data$country), collapse = ", "), "\n")
      cat("Number of complete observations: ", nrow(data), "\n\n")
      
      summary(model)
    })
    
    # Render post-hoc test results
    output$posthoc_results <- renderPrint({
      data <- anova_data()
      
      # Check for sufficient data and groups
      if (nrow(data) < 10 || length(unique(data$country)) < 2) {
        return(NULL)
      }
      
      # Run ANOVA and Tukey HSD
      model <- aov(value ~ country, data = data)
      tukey <- TukeyHSD(model)
      
      cat("Tukey Honest Significant Differences\n")
      cat("====================================\n")
      print(tukey)
    })
    
    # Render ANOVA plot
    output$anova_plot <- renderPlotly({
      data <- anova_data()
      
      # Check for sufficient data
      if (nrow(data) < 10 || length(unique(data$country)) < 2) {
        return(NULL)
      }
      
      # Create boxplot
      p <- ggplot(data, aes(x = country, y = value, fill = country)) +
        geom_boxplot(alpha = 0.8, outlier.shape = NA) +
        geom_jitter(width = 0.2, alpha = 0.3, size = 1.5) +
        labs(
          title = paste("Distribution of", input$anova_var, "by Country"),
          x = "Country",
          y = input$anova_var
        ) +
        scale_fill_viridis_d(option = "magma") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p) %>% 
        layout(legend = list(orientation = "h", y = -0.2))
    })
    
    # Render assumptions check
    output$assumptions_check <- renderPrint({
      data <- anova_data()
      
      # Check for sufficient data
      if (nrow(data) < 10 || length(unique(data$country)) < 2) {
        return(NULL)
      }
      
      model <- aov(value ~ country, data = data)
      
      cat("ANOVA Assumptions Check\n")
      cat("=======================\n\n")
      
      # Normality of residuals
      shapiro_test <- shapiro.test(residuals(model))
      cat("1. Normality of Residuals (Shapiro-Wilk test):\n")
      cat("   W =", round(shapiro_test$statistic, 4), 
          "p-value =", format.pval(shapiro_test$p.value, digits = 4), "\n")
      if (shapiro_test$p.value > 0.05) {
        cat("   -> Residuals are normally distributed (p > 0.05)\n\n")
      } else {
        cat("   -> WARNING: Residuals are not normally distributed (p < 0.05)\n\n")
      }
      
      # Homogeneity of variances
      levene_test <- car::leveneTest(value ~ as.factor(country), data = data)
      cat("2. Homogeneity of Variances (Levene's test):\n")
      cat("   F(", levene_test$Df[1], ",", levene_test$Df[2], ") =", 
          round(levene_test$`F value`[1], 4), 
          "p-value =", format.pval(levene_test$`Pr(>F)`[1], digits = 4), "\n")
      if (levene_test$`Pr(>F)`[1] > 0.05) {
        cat("   -> Variances are homogeneous across groups (p > 0.05)\n")
      } else {
        cat("   -> WARNING: Variances are not homogeneous (p < 0.05)\n")
      }
    })
    
    # Render diagnostic plots
    output$assumptions_plot <- renderPlot({
      data <- anova_data()
      
      # Check for sufficient data
      if (nrow(data) < 10 || length(unique(data$country)) < 2) {
        return(NULL)
      }
      
      model <- aov(value ~ country, data = data)
      
      # Set up 2x2 grid
      par(mfrow = c(2, 2))
      plot(model, ask = FALSE)
    })
    
    
    
    ########################
    # Linear Reg
    ########################
    
    # Reactive data preparation for regression
    regression_data <- eventReactive(input$regression_run, {
      req(input$regression_dep, input$regression_indep)
      
      # Get question IDs
      dep_id <- get_question_id(input$regression_dep)
      indep_ids <- sapply(input$regression_indep, get_question_id, USE.NAMES = FALSE)
      
      # Prepare data from preprocessed numeric dataset
      data <- indiv_ordinal
      
      # Apply country filter if selected
      if (!is.null(input$regression_country)) {
        data <- data %>% 
          filter(B_COUNTRY_ALPHA == input$regression_country)
      }
      
      # Apply sampling for performance
      if (nrow(data) > input$regression_sample) {
        data <- data %>% sample_n(input$regression_sample)
      }
      
      # Select relevant columns and omit missing values
      data <- data %>%
        select(all_of(c(dep_id, indep_ids))) %>%
        na.omit()
      
      # Store labels for display
      list(
        data = data,
        dep_label = input$regression_dep,
        indep_labels = input$regression_indep,
        dep_id = dep_id,
        indep_ids = indep_ids
      )
    })
    
    # Render model summary
    output$regression_summary <- renderPrint({
      result <- regression_data()
      data <- result$data
      
      # Check for sufficient data
      if (nrow(data) < 10) {
        return("Insufficient data: Need at least 10 complete observations.")
      }
      
      if (ncol(data) < 2) {
        return("Insufficient variables: Need at least one independent variable.")
      }
      
      # Build formula using IDs
      formula <- as.formula(paste(names(data)[1], "~", 
                                  paste(names(data)[-1], collapse = " + ")))
      
      # Run regression
      model <- lm(formula, data = data)
      
      # Display results with labels
      cat("Linear Regression Model Summary\n")
      cat("==============================\n")
      cat("Country: ", input$regression_country, "\n")
      cat("Dependent variable: ", result$dep_label, "\n")
      cat("Independent variables: ", paste(result$indep_labels, collapse = ", "), "\n")
      cat("Number of complete observations: ", nrow(data), "\n\n")
      
      summary(model)
    })
    
    # Render coefficient table
    output$regression_coef <- renderDT({
      result <- regression_data()
      data <- result$data
      
      # Check for sufficient data
      if (nrow(data) < 10 || ncol(data) < 2) {
        return(NULL)
      }
      
      # Build formula using IDs
      formula <- as.formula(paste(names(data)[1], "~", 
                                  paste(names(data)[-1], collapse = " + ")))
      model <- lm(formula, data = data)
      
      # Create coefficient table with labels
      coef_table <- broom::tidy(model) %>%
        mutate(
          term = case_when(
            term == "(Intercept)" ~ "Intercept",
            term %in% names(data) ~ {
              # Map variable names to labels
              var_id <- term
              if(var_id == names(data)[1]) {
                result$dep_label
              } else {
                idx <- which(result$indep_ids == var_id)
                if(length(idx) > 0) result$indep_labels[idx] else var_id
              }
            },
            TRUE ~ term
          ),
          p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
        )
      
      # Create datatable
      datatable(
        coef_table,
        extensions = 'Buttons',
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf')
        ),
        rownames = FALSE,
        caption = "Regression Coefficients"
      ) %>%
        formatRound(columns = c("estimate", "std.error", "statistic"), digits = 4)
    })
    
    # Render diagnostic plots
    output$regression_diag <- renderPlot({
      result <- regression_data()
      data <- result$data
      
      # Check for sufficient data
      if (nrow(data) < 10 || ncol(data) < 2) {
        return(NULL)
      }
      
      # Build formula using IDs
      formula <- as.formula(paste(names(data)[1], "~", 
                                  paste(names(data)[-1], collapse = " + ")))
      model <- lm(formula, data = data)
      
      # Set up 2x2 grid
      par(mfrow = c(2, 2))
      plot(model, ask = FALSE)
    })
    
    # Render prediction plot
    output$regression_prediction <- renderPlotly({
      result <- regression_data()
      data <- result$data
      
      # Check for sufficient data and variables
      if (nrow(data) < 10 || ncol(data) < 2) {
        return(NULL)
      }
      
      # Use first independent variable for bivariate plot
      x_var_id <- names(data)[2]
      y_var_id <- names(data)[1]
      
      # Get corresponding labels
      x_label <- result$indep_labels[1]
      y_label <- result$dep_label
      
      # Create model for bivariate relationship
      formula <- as.formula(paste(y_var_id, "~", x_var_id))
      model <- lm(formula, data = data)
      
      # Generate prediction data
      x_range <- seq(min(data[[x_var_id]], na.rm = TRUE), 
                     max(data[[x_var_id]], na.rm = TRUE), 
                     length.out = 100)
      pred_data <- data.frame(x = x_range)
      names(pred_data) <- x_var_id
      pred <- predict(model, newdata = pred_data, interval = "confidence")
      
      # Combine prediction data
      plot_data <- cbind(pred_data, pred) %>%
        rename(fit = 2, lwr = 3, upr = 4)
      
      # Create plot
      p <- ggplot() +
        geom_point(data = data, aes(x = .data[[x_var_id]], y = .data[[y_var_id]]), 
                   alpha = 0.5, color = "#3366CC") +
        geom_line(data = plot_data, aes(x = .data[[x_var_id]], y = fit), 
                  color = "#FF3366", size = 1) +
        geom_ribbon(data = plot_data, aes(x = .data[[x_var_id]], ymin = lwr, ymax = upr), 
                    alpha = 0.2, fill = "#FF3366") +
        labs(
          title = paste("Regression of", y_label, "on", x_label),
          x = x_label,
          y = y_label
        ) +
        theme_minimal()
      
      ggplotly(p)
    })
    
    

    ########################
    # Map View
    ########################
    
    # Load world map with ISO_A3 codes
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Define dynamic list of available ISO-A3 countries (can be updated)
    available_iso <- WVS7_iso_list
    
    # Filter map to show only available countries
    world_available <- world %>% filter(iso_a3 %in% available_iso)
    
    output$countrySelect <- renderUI({
      pickerInput(
        inputId = "country_picker",
        label = "Select up to 4 countries",
        choices = picker_country_list,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `max-options` = 4,
          `max-options-text` = "You can only select up to 4 countries."
        )
      )
    })
    
    # Reactive values to store selected countries
    selected <- reactiveVal(character())
    
    # Observe clicks on the map
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      iso_clicked <- click$id
      
      if (!is.null(iso_clicked) && iso_clicked %in% available_iso) {
        current <- selected()
        if (iso_clicked %in% current) {
          selected(setdiff(current, iso_clicked))  # deselect if already selected
        } else {
          selected(c(current, iso_clicked))        # add if not selected
        }
      }
    })
    
    # Sync picker with map clicks
    observe({
      updatePickerInput(session, "country_picker", selected = selected())
    })
    
    # Update from pickerInput
    observeEvent(input$country_picker, {
      # In case picker bypasses limit (e.g., programmatic update)
      selected(head(input$country_picker, 4))
    })
    
    # Render the map
    output$map <- renderLeaflet({
      leaflet(world_available,
              options = leafletOptions(
                zoomControl = FALSE,
                minZoom = 2,
                maxZoom = 5,
                dragging = TRUE)) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(0, 20, zoom = 2) %>%
        addPolygons(
          layerId = ~iso_a3,
          label = ~name,
          weight = 1,
          color = "black",
          fillColor = ~ifelse(iso_a3 %in% selected(), "green", "lightgray"),
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.8)
        )
    })
    
    # Update fill color dynamically
    observe({
      leafletProxy("map", data = world_available) %>%
        clearShapes() %>%
        addPolygons(
          layerId = ~iso_a3,
          label = ~name,
          weight = 1,
          color = "black",
          fillColor = ~ifelse(iso_a3 %in% selected(), "green", "lightgray"),
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.8)
        )
    })

    ########################
    # Corrplot chart
    ########################

    output$menuCorrPlot <- renderUI({
      dropdownButton(
        inputId = "dropdownCorrPlot",
        label = "Options",
        icon = icon("sliders"),
        status = "success",
        circle = FALSE,
        
        # Radio buttons for method
        materialSwitch(
          inputId = "corr_method",
          label = "Ellipse / Color",
          status = "success"
        ),
        
        # Radio buttons for order
        prettyRadioButtons(
          inputId = "corr_order",
          label = "Order",
          thick = TRUE,
          choices = c("FPC", "alphabet", "AOE", "hclust"),
          selected = "FPC",
          animation = "pulse",
          status = "success"
        ),
        
        noUiSliderInput(
          inputId = "corr_tl_cex",
          label = "Text Size",
          min = 0.5,
          max = 2,
          value = 1,
          step = 0.1,
          tooltips = FALSE,
          color = "green"
        ),
        
        # Radio buttons for plot type
        materialSwitch(
          inputId = "corr_type",
          label = "Type",
          status = "success"
        ),
        
        # Checkbox for diagonal - prettyCheckbox
        prettyCheckbox(
          inputId = "corr_diag",
          label = "Show Diagonal",
          value = FALSE,
          thick = TRUE,
          animation = "pulse",
          status = "success"
        ),
        
        # Checkbox for adding coefficient - prettyCheckbox
        prettyCheckbox(
          inputId = "corr_addCoef",
          label = "Show Coefficients",
          value = TRUE,
          thick = TRUE,
          animation = "pulse",
          status = "success"
        ),
        
        # Dropdown for coefficient colors
        prettyRadioButtons(
          inputId = "corr_coef_color",
          label = "Coefficient Color",
          thick = TRUE,
          choices = c("Black", "Blue", "Red"),
          selected = "Black",
          animation = "pulse",
          status = "success"
        ),
        
        # Slider for text rotation
        sliderInput(
          "corr_tl_srt",
          "Text Rotation:",
          min = 0,
          max = 90,
          value = 45
        ),
        
        # Radio buttons for background color - material switch
        materialSwitch(
          inputId = "corr_bg",
          label = "Background Color",
          status = "success"
        ),
        
        # Download button to export plot
        downloadButton("corr_downloadPlot", "Download Plot")
      )
    })
    
    observeEvent(input$pickQuestion, {
      req(input$pickQuestion)
      
      if (length(input$pickRegion) > 0) {
        selected_countries <- input$pickRegion
      } else {
        selected_countries <- levels(indiv_ordinal$B_COUNTRY)
      }
      # browser()
      xt_pickQ <- str_extract(input$pickQuestion, "^(Q|E|F|G|H)\\d+") # TODO need to check error occurring when the last section group is selected
      
      # Filter and select the data based on user inputs
      charts_data <- indiv_ordinal %>%
        dplyr::filter(B_COUNTRY %in% selected_countries) %>%
        dplyr::select(all_of(xt_pickQ))
      
      # Calculate the correlation matrix (only if there is more than one column)
      if (ncol(charts_data) > 1) {
        corr_matrix <- cor(charts_data, use = "pairwise.complete.obs")
        
        # Render the correlation plot
        output$corrChart <- renderPlot({
          corrplot::corrplot(
            corr_matrix,
            method = if (input$corr_method == TRUE) "color" else "ellipse",
            order = input$corr_order,
            tl.cex = input$corr_tl_cex,
            type = if (input$corr_type == TRUE) "full" else "upper",
            diag = input$corr_diag,
            addCoef.col = if (input$corr_addCoef) input$corr_coef_color else NULL,
            tl.srt = input$corr_tl_srt,
            bg = if (input$corr_bg == TRUE) "darkgrey" else "white"
          )
        })
      } else {
        output$corrChart <- renderPlot({
          plot(1, 1, main = "Not enough data for correlation plot", type = "n") # Placeholder if not enough data
        })
      }
      
      output$corr_downloadPlot <- downloadHandler(
        filename = function() {
          paste("corrplot", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 800, height = 600) # Save as PNG
          corrplot::corrplot(
            corr_matrix,
            method = if (input$corr_method == TRUE) "color" else "ellipse",
            order = input$corr_order,
            tl.cex = input$corr_tl_cex,
            type = if (input$corr_type == TRUE) "full" else "upper",
            diag = input$corr_diag,
            addCoef.col = if (input$corr_addCoef) input$corr_coef_color else NULL,
            tl.srt = input$corr_tl_srt,
            bg = if (input$corr_bg == TRUE) "darkgrey" else "white"
          )
          dev.off()
        }
      )
    })
    
    
    ########################
    # ANOVA - OLD VERSION - NOT IN USE AT THE MOMENT
    ########################
    
    # anovaData <- reactive({
    #   d <- indiv_ordinal[indiv_ordinal$B_COUNTRY %in% input$pickRegion, c("B_COUNTRY", input$pickQuestion)]
    #   d
    # })
    # 
    # significanceTest <- reactive({
    #   d <- anovaData()
    #   if (nrow(d) > 0) {
    #     for (col_index in 2:ncol(d)) {
    #       pairwise.t.test(d[, col_index], interaction(d$B_COUNTRY, d[, col_index]), p.adjust.method = "bonferroni")
    #     }
    #   } else {
    #     NULL
    #   }
    # })
    # 
    # anovaResults <- reactive({
    #   d <- anovaData()
    #   for (col_index in 2:ncol(d)) {
    #     anv <- aov(d[, col_index] ~ B_COUNTRY, data = d)
    #   }
    #   anv
    # })
    # 
    # output$modelSummary <- renderPrint({
    #   d <- anovaData()
    #   mdl <- lm(B_COUNTRY ~ ., data = d)
    #   mdl
    # })
    # 
    # output$anovaSummary <- renderPrint({
    #   d <- anovaData()
    #   for (col_index in 2:ncol(d)) {
    #     anv <- aov(d[, col_index] ~ B_COUNTRY, data = d)
    #     print(anv)
    #   }
    #   anv
    # })
    # 
    # output$anovaBoxplot <- renderPlot({
    #   data <- anovaData()
    # 
    #   if (length(input$pickRegion) > 0) {
    #     selected_countries <- input$pickRegion
    #   } else {
    #     output$anovaBoxplot <- renderPlot({
    #       plot(1, 1, main = "Boxplot chart can't be calculated with the current country selection. Please select up to a maximum of 4.", type = "n")
    #     })
    #   }
    #   
    #   bxplt <- data |>
    #     pivot_longer(cols = starts_with(c("Q", "E", "F_", "G_", "H_")),
    #                  names_to = "question",
    #                  values_to = "response")
    #   
    #   ggplot(bxplt, aes(x = question, y = response, fill = B_COUNTRY)) +
    #     geom_boxplot(notch = input$bxplt_notch) +
    #     labs(x = "Question", y = "Responses", title = "Distribution of Question Responses grouped by Country") +
    #     scale_fill_viridis(discrete = TRUE, option = "D") +  # Colorblind-friendly palette
    #     theme_minimal() +
    #     theme(
    #       axis.title = element_text(size = 14), # Resize axis titles
    #       axis.text = element_text(size = 12), # Resize axis text
    #       plot.title = element_text(size = 16, face = "bold"), # Resize plot title
    #       legend.position = "bottom"  # Move legend to bottom
    #     )
    # }) # TODO include labeling with explanations about colouring and outliers


    ########################
    # Control Buttons
    ########################
    
    observeEvent(input$next1, {
      updateTabsetPanel(session, "map_viewTabs",
                        selected = "correlations")
    })
    
    observeEvent(input$next2, {
      updateTabsetPanel(session, "map_viewTabs",
                        selected = "anova")
    })
    
    observeEvent(input$prev1, {
      updateTabsetPanel(session, "map_viewTabs",
                        selected = "map_view")
    })
    
    observeEvent(input$prev2, {
      updateTabsetPanel(session, "map_viewTabs",
                        selected = "correlations")
    })
    
    
    ########################
    # Missing Data chart
    ########################
    
    # TODO add vis_miss_ly code provided by Nick
    # vis_miss
    output$Missing <- renderPlot({
      vis_miss(get_C_data(), cluster = input$cluster_ctry) +
        theme(axis.text.x = element_blank())
    })
    
    output$Indiv_missing_with_ratio <- renderPlot({
      d <- sample_with_missing_ratio(orig_indiv_data, sample_size = 2500)
      
      vis_miss(d, cluster = input$cluster_indiv) +
        theme(axis.text.x = element_blank())
    })
    
    output$Top_miss_indiv <- renderPlot({
      top_miss <- miss_var_summary(orig_indiv_data) %>%
        slice_head(n = 15) %>%
        mutate(
          pct_miss = as.numeric(pct_miss),
          variable = forcats::fct_reorder(variable, pct_miss, .desc = TRUE)
        )
      
      top_miss %>%
        ggplot(aes(x = variable, y = pct_miss, fill = variable)) +
        geom_bar(stat = "identity") +
        geom_text(
          aes(label = round(pct_miss, 1)),
          vjust = -0.5,
          size = 4.5,
          fontface = "bold"
        ) +
        scale_fill_viridis_d(option = "viridis") +
        labs(
          title = "Percentage of Missing Data of Individual Responses",
          x = "Variable",
          y = "Percentage Missing",
          fill = "Variable"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    })
    
    output$Top_miss_country <- renderPlot({
      top_miss <- miss_var_summary(orig_country_data) %>%
        slice_head(n = 15) %>%
        mutate(
          pct_miss = as.numeric(pct_miss),
          variable = forcats::fct_reorder(variable, pct_miss, .desc = TRUE)
        )
      
      top_miss %>%
        ggplot(aes(x = variable, y = pct_miss, fill = variable)) +
        geom_bar(stat = "identity") +
        geom_text(
          aes(label = round(pct_miss, 1)),
          vjust = -0.5,
          size = 4.5,
          fontface = "bold"
        ) +
        scale_fill_viridis_d(option = "viridis") +
        labs(
          title = "Percentage of Missing Data in Country Data Consolidation",
          x = "Variable",
          y = "Percentage Missing",
          fill = "Variable"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    })
    
    # TODO add boxplot of variables (IQR range 0.5-5)
    
  }) # end server
#### server end ####
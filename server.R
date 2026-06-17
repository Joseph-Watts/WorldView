#' Work in progress
#' 
#' Code based on: https://shiny.posit.co/r/gallery/life-sciences/biodiversity-national-parks/
#' WVS data source: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp

#######################-
#### SERVER LOGIC #####
#######################-

shinyServer(
  
  function(input, output, session) {
    session$onSessionEnded(function() {
      stopApp()
    })
    
    
    ############################-
    #### Read in data files ####
    ############################-
    
    # WVS data are loaded once in global.R. These lightweight accessors are kept
    # for compatibility with existing server code, but avoid copying/mutating the
    # full objects on every reactive call.
    get_I_data <- reactive(indiv_data)
    # get_C_data <- reactive(country_data)
    get_var_info <- reactive(codebook_data)
    
    # indiv_data modified to have full question as name of column
    get_I_longID <- reactive({
      d.I <- get_I_data()
      d.var_info <- get_var_info()
      
      for (i in 4:293) { # from Q1 to Q290
        names(d.I)[i] <- d.var_info$ColLab[i]
      }
      d.I
    })
    
    # country_data modified to have full question as name of column - NEEDS REWORK
    # get_C_longID <- reactive({
    #   d.C <- get_C_data()
    #   d.var_info <- get_var_info()
    #   
    #   for (i in 3:421) { # from Q1 to Q290
    #     # names(d.C)[i] <- d.var_info$ColLab[i]
    #     names(d.C) <- sapply(names(d.C), function(name) {
    #       if (name %in% names(d.var_info$ColLab[i]) && !grepl("\\.", name)) {
    #         title_lookup[name]
    #       } else {
    #         name
    #       }
    #     })
    #   }
    #   d.C
    # })
    
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
      d <- codebook_data[, c(1, 2, 10)]
      d <- split(d, d$Section)
      c <- lapply(d, function(group) {
        stats::setNames(group$Col_ID, 
                        group$ColLab)
        })
      c
    }) # TODO get the list ordered by 'Col_ID' not by 'Section'
    
    # Fetch just sections
    get_sectionsOrd <- reactive({
      var_info <- get_var_info()
      sections <- as.list(unique(var_info$Section))
      sections_ord <- unique(factor(var_info$Section, 
                                    ordered = TRUE, 
                                    levels = sections))
      sections_ord <- sections_ord[-1]
      sections_ord
    })
    
    
    #############################-
    #### PDF & CODEBOOK VIEW ####
    #############################-
    
    # # Master Survey Questionnaire PDF
    # output$surveyview <- renderUI({
    #   tags$iframe(style = "height:100vh; width:100%; scrolling=yes",
    #               src = "F00011012-WVS_WAVE_7_MASTER_QUESTIONNAIRE_2017-2021_ENGLISH.pdf")
    # })
    # 
    # # Codebook PDF
    # output$codebookview <- renderUI({
    #   tags$iframe(style = "height:100vh; width:100%; scrolling=yes",
    #               src = "F00011055-WVS7_Codebook_Variables_report_V6.0.pdf")
    # })
    
    
    ####################-
    #### DataTables ####
    ####################-
    
    # # Reactive control for selecting country
    # output$raw_selectCountry <- renderUI({
    #  shinyWidgets::pickerInput(
    #     inputId = "raw_country",
    #     label = "Select Country",
    #     choices = picker_country_list,
    #     multiple = FALSE,
    #     selected = NULL,
    #     options = list(
    #       `live-search` = TRUE,
    #       `size` = 20
    #     )
    #   )
    # })
    # 
    # raw_filtering <- reactive({
    #     if(is.null(input$raw_country)) {
    #       get_I_longID() |> dplyr::select(-S007)
    #     } else {
    #       get_I_longID() |>
    #         dplyr::filter(B_COUNTRY_ALPHA == input$raw_country) |>
    #         dplyr::select(-S007)
    #       # currently, filtering does not work for multiples countries as expected, reverted back to single country selection
    #     }
    # })
    # 
    # output$raw_filtered_country <- DT::renderDataTable({
    #   DT::datatable(data = raw_filtering()|>
    #                   dplyr::rename(Country = B_COUNTRY, `Country ISO` = B_COUNTRY_ALPHA),
    #                 options = list(pageLength = 10, scrollX = TRUE))
    # })
    # 
    # # Data table - Country aggregate responses
    # output$Table_country <- DT::renderDataTable({
    #   DT::datatable(data = get_C_data() |>
    #                   dplyr::mutate(across(where(is.numeric), ~ round(., 2))) |>
    #                   dplyr::rename(Country = B_COUNTRY, `Country ISO` = B_COUNTRY_ALPHA),
    #                 options = list(scrollX = TRUE))
    # })
    
    
    #################-
    #### Missing ####
    #################-
    
    # # TODO add vis_miss_ly code provided by Nick
    # output$Missing <- renderPlot({
    #   naniar::vis_miss(get_C_data(), cluster = input$cluster_ctry, sort = input$sort_ctry) +
    #     ggplot2::theme(axis.text.x = element_blank())
    # })
    # 
    # output$Indiv_missing_with_ratio <- renderPlot({
    #   d <- sample_with_missing_ratio(get_I_data(), sample_size = 2500)
    # 
    #   naniar::vis_miss(d, cluster = input$cluster_indiv, sort = input$sort_indiv) +
    #     ggplot2::theme(axis.text.x = element_blank())
    # })
    # 
    # output$Top_miss_indiv <- renderPlot({
    #   top_miss <- naniar::miss_var_summary(get_I_data()) %>%
    #     dplyr::slice_head(n = 15) %>%
    #     dplyr::mutate(
    #       pct_miss = as.numeric(pct_miss),
    #       variable = forcats::fct_reorder(variable, pct_miss, .desc = TRUE)
    #     )
    # 
    #   top_miss %>%
    #     ggplot2::ggplot(aes(x = variable, y = pct_miss, fill = variable)) +
    #     ggplot2::geom_bar(stat = "identity") +
    #     ggplot2::geom_text(
    #       ggplot2::aes(label = round(pct_miss, 1)),
    #       vjust = -0.5,
    #       size = 4.5,
    #       fontface = "bold"
    #     ) +
    #     ggplot2::scale_fill_viridis_d(option = "viridis") +
    #     ggplot2::labs(
    #       title = "Percentage of Missing Data of Individual Responses",
    #       x = "Variable",
    #       y = "Percentage Missing",
    #       fill = "Variable"
    #     ) +
    #     ggplot2::theme_minimal() +
    #     ggplot2::theme(
    #       plot.title = ggplot2::element_text(face = "bold", size = 16),
    #       axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
    #       legend.position = "none"
    #     )
    # })
    # 
    # output$Top_miss_country <- renderPlot({
    #   top_miss <- naniar::miss_var_summary(get_C_data()) %>%
    #     dplyr::slice_head(n = 15) %>%
    #     dplyr::mutate(
    #       pct_miss = as.numeric(pct_miss),
    #       variable = forcats::fct_reorder(variable, pct_miss, .desc = TRUE)
    #     )
    # 
    #   top_miss %>%
    #     ggplot2::ggplot(aes(x = variable, y = pct_miss, fill = variable)) +
    #     ggplot2::geom_bar(stat = "identity") +
    #     ggplot2::geom_text(
    #       ggplot2::aes(label = round(pct_miss, 1)),
    #       vjust = -0.5,
    #       size = 4.5,
    #       fontface = "bold"
    #     ) +
    #     ggplot2::scale_fill_viridis_d(option = "viridis") +
    #     ggplot2::labs(
    #       title = "Percentage of Missing Data in Country Data Consolidation",
    #       x = "Variable",
    #       y = "Percentage Missing",
    #       fill = "Variable"
    #     ) +
    #     ggplot2::theme_minimal() +
    #     ggplot2::theme(
    #       plot.title = ggplot2::element_text(face = "bold", size = 16),
    #       axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
    #       legend.position = "none"
    #     )
    # })
    
    
    ####################-
    #### Univariate ####
    ####################-
    
    univariate_data <- reactive({
      req(input$univar_question, input$univar_countries)
      
      # Get question ID
      q_id <- get_question_id(input$univar_question)
      
      # Filter once, then derive the numeric copy from the same subset.
      orig_data <- indiv_data %>%
        dplyr::filter(B_COUNTRY_ALPHA %in% input$univar_countries) %>%
        dplyr::select(country = B_COUNTRY, response = dplyr::all_of(q_id)) %>%
        dplyr::mutate(country = as.character(country))
      
      num_data <- orig_data %>%
        dplyr::mutate(response = suppressWarnings(as.numeric(response)))
      
      # Determine variable type
      is_factor <- is.factor(orig_data$response)
      is_numeric <- is.numeric(num_data$response)
      n_unique <- dplyr::n_distinct(stats::na.omit(orig_data$response))
      
      list(
        orig = orig_data,
        num = num_data,
        is_factor = is_factor,
        is_numeric = is_numeric,
        n_unique = n_unique
      )
    })
    
    output$univariate_results <- renderUI({
      data <- univariate_data()
      if(is.null(data)) return("No data available")
      req(data)
      
      # Get unique country names
      country_names <- unique(data$orig$country)
      
      # Generate summary for each country and overall - with Overall first
      tabs <- lapply(c("Selected Sample", country_names), 
                     function(ctry_name) {
        if (ctry_name == "Selected Sample") {
          orig_sub <- data$orig
          num_sub <- data$num
        } else {
          orig_sub <- data$orig %>% filter(country == ctry_name)
          num_sub <- data$num %>% filter(country == ctry_name)
        }
        
        # Create tab content
        tab_content <- tagList(h3(ctry_name))  # Add country name header
        
        if (data$is_factor || (data$is_numeric && data$n_unique <= 10)) {
          # Frequency table
          freq_table <- orig_sub %>%
            dplyr::count(response) %>%
            dplyr::mutate(Percentage = round(n / sum(n) * 100, 1)) %>%
            dplyr::rename(Response = response, Count = n)
          
          tab_content <- tagList(
            tab_content,
            h4("Frequency Distribution"),
            renderTable(freq_table)
          )
        }
        
        if (data$is_numeric) {
          # Numeric summary
          desc <- num_sub %>%
            dplyr::summarise(
              n = sum(!is.na(response)),
              Mean = round(mean(response, na.rm = TRUE), 2),
              SD = round(stats::sd(response, na.rm = TRUE), 2),
              Median = round(stats::median(response, na.rm = TRUE), 2),
              Min = min(response, na.rm = TRUE),
              Max = max(response, na.rm = TRUE),
              Skewness = round(psych::skew(response), 3),
              Kurtosis = round(psych::kurtosi(response), 3)
            )
          
          tab_content <- tagList(
            tab_content,
            h4("Numeric Summary"),
            renderTable(desc)
          )
        }
        
        tab_content  # Return the content
      })
      
      # Create tabset with Overall first, then the countries
      do.call(tabsetPanel, c(
        id = "countryTabs",
        lapply(c("Selected Sample", country_names), function(name) {
          tabPanel(title = name, tabs[[which(c("Selected Sample", 
                                               country_names) == name)]])
        })
      ))
    })
    
    
    ####################-
    #### BIVARIATE #####
    ####################-
    
    # Reactive data preparation for bivariate summary
    bivariate_data <- reactive({
      req(input$bivariate_var1, input$bivariate_var2)

      # Get question IDs
      var1_id <- get_question_id(input$bivariate_var1)
      var2_id <- get_question_id(input$bivariate_var2)

      # Prepare data
      data <- indiv_data
      if (!is.null(input$bivariate_countries)) {
        data <- data %>%
          dplyr::filter(B_COUNTRY_ALPHA %in% input$bivariate_countries)
      }

      # Select relevant columns
      data %>%
        dplyr::select(var1 = !!var1_id, var2 = !!var2_id) %>%
        dplyr::mutate(
          var1 = sjlabelled::as_label(var1),
          var2 = sjlabelled::as_label(var2)
        ) %>%
        stats::na.omit()
    })

    # Render bivariate table
    output$bivariate_table <- renderDT({
      data <- bivariate_data()
      if (is.null(data) || nrow(data) == 0) return(NULL)

      # Create contingency table
      tab <- table(data$var1, data$var2)

      # Apply percentages if requested
      if (input$bivariate_type == "Row Percentages") {
        tab <- prop.table(tab, 1) * 100
      } else if (input$bivariate_type == "Column Percentages") {
        tab <- prop.table(tab, 2) * 100
      }

      # Convert to data frame for nice display
      df <- as.data.frame.matrix(tab)
      df <- cbind(`Var1 v /Var2 >` = rownames(df), df)
      rownames(df) <- NULL

      # Create datatable
      DT::datatable(
        df,
        extensions = 'Buttons',
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        rownames = FALSE,
        caption = paste("Cross-tabulation of", 
                        input$bivariate_var1, 
                        "and", 
                        input$bivariate_var2)
      ) %>%
        DT::formatRound(
          columns = 2:ncol(df),
          digits = ifelse(input$bivariate_type == "Counts", 0, 1)
        )
    })
    
    
    
    ###################-
    #### Bar chart ####
    ###################-
    
    output$bar_plot <- renderPlotly({
      input$bar_update
        req(input$bar_question, input$bar_countries)
        
        q_id <- get_question_id(input$bar_question)
        
        plot_data <- indiv_data %>%
          dplyr::filter(B_COUNTRY_ALPHA %in% input$bar_countries) %>%
          dplyr::select(country = B_COUNTRY, response = !!q_id) %>%
          dplyr::mutate(response = as.factor(response)) %>%
          dplyr::count(country, response) %>%
          dplyr::group_by(country) %>%
          dplyr::mutate(percent = n / sum(n) * 100) %>%
          stats::na.omit()
        
        if (input$bar_type == "Percentage") {
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = response, 
                                                       y = percent, 
                                                       fill = country)) +
            ggplot2::geom_bar(stat = "identity", 
                              position = ggplot2::position_dodge(preserve = "single")) +
            ggplot2::labs(y = "Percentage (%)", 
                          x = "Country", 
                          title = paste("Distribution of",
                                        input$bar_question)
                          ) +
            ggplot2::scale_fill_viridis_d()
          
        } else if (input$bar_type == "Count") {
          # p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = response, y = n, fill = country)) +
          #   ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
          #   ggplot2::labs(y = "Count", x = "Country", title = paste("Distribution of", input$bar_question)) +
          #   ggplot2::scale_fill_viridis_d()
          
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = response, 
                                                       y = n, 
                                                       fill = country)) +
            ggplot2::geom_col(position = ggplot2::position_dodge(preserve = "single")) +
            ggplot2::labs(
              y = "Count",
              x = "Response",
              title = paste("Distribution of", 
                            input$bar_question),
              fill = "Country"
            ) +
            ggplot2::scale_fill_viridis_d()
          
        } else if (input$bar_type == "Stacked") {
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = country, 
                                                       y = percent, 
                                                       fill = response)) +
            ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE)) +
            ggplot2::labs(y = "Percentage (%)", 
                 title = paste("Distribution of", input$bar_question)) +
            ggplot2::scale_fill_viridis_d(option = "D") +
            ggplot2::theme(legend.title = ggplot2::element_blank())
          
        } else {
          # Staggered view
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = response, 
                                                       y = n, 
                                                       fill = response)) +
            ggplot2::geom_col() +
            ggplot2::facet_wrap(~country, ncol = 1, scales = "fixed") +
            ggplot2::labs(y = "Count", title = paste("Distribution of", 
                                                     input$bar_question)) +
            ggplot2::scale_fill_viridis_d(option = "D") +
            ggplot2::theme(legend.position = "none")
        }
        
        # Remove x-axis title for ALL display types
        p <- p + ggplot2::labs(x = NULL) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                             hjust = 1))
        
        plotly::ggplotly(p) %>% 
          plotly::layout(legend = list(orientation = "h", 
                                       y = -0.2))
    })
    
    
    #####################-
    #### Scatterplot ####
    #####################-
    
    # output$scatter_plot <- renderPlotly({
    #   req(input$scatter_x, input$scatter_y, input$scatter_countries)
    #   
    #   req(input$scatter_x, input$scatter_y)
    #   
    #   # Get question IDs via precomputed lookup
    #   x_id <- get_question_id(input$scatter_x)
    #   y_id <- get_question_id(input$scatter_y)
    #   
    #   # Prepare only the columns needed, then sample by row count for Plotly.
    #   plot_data <- indiv_data
    #   if (!is.null(input$scatter_countries)) {
    #     plot_data <- plot_data %>%
    #       dplyr::filter(B_COUNTRY_ALPHA %in% input$scatter_countries)
    #   }
    #   
    #   plot_data <- plot_data %>%
    #     dplyr::select(x = dplyr::all_of(x_id),
    #                   y = dplyr::all_of(y_id),
    #                   country = B_COUNTRY_ALPHA) %>%
    #     stats::na.omit()
    #   
    #   if (nrow(plot_data) > input$scatter_sample) {
    #     plot_data <- plot_data %>% dplyr::slice_sample(n = input$scatter_sample)
    #   }
    #   
    #   # Create plot
    #   p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, color = country)) +
    #     ggplot2::geom_jitter(width = 0.2,
    #                          height = 0.2,
    #                          alpha = 0.35,
    #                          size = 1.5) +
    #     ggplot2::geom_smooth(method = "lm", se = FALSE) +
    #     ggplot2::labs(
    #       title = paste(input$scatter_x, "vs", input$scatter_y),
    #       x = input$scatter_x,
    #       y = input$scatter_y
    #     ) +
    #     ggplot2::theme_minimal()
    #   
    #   plotly::ggplotly(p)
    # })
    # 
    
    ##################-
    #### Corrplot ####
    ##################-
    
    # generate_corr_plot <- reactive({
    #   req(input$corr_questions, length(input$corr_questions) > 1)
    #   
    #   # Get question IDs
    #   var_info <- get_var_info()
    #   q_ids <- sapply(input$corr_questions, function(q) {
    #     var_info$Col_ID[var_info$ColLab == q]
    #   }, USE.NAMES = FALSE)
    #   
    #   # Prepare data
    #   plot_data <- indiv_data
    #   if (!is.null(input$corr_countries)) {
    #     plot_data <- plot_data %>%
    #       dplyr::filter(B_COUNTRY_ALPHA %in% input$corr_countries)
    #   }
    #   
    #   plot_data <- plot_data %>%
    #     dplyr::select(all_of(q_ids))
    #   
    #   # Compute correlation matrix
    #   cor_matrix <- stats::cor(plot_data,
    #                            use = "pairwise.complete.obs",
    #                            method = tolower(input$corr_method))
    #   
    #   # Create color palette based on selection
    #   if(input$corr_palette == "Viridis") {
    #     col <- viridis::viridis(100)
    #   } else {
    #     # Red-to-blue gradient palette
    #     col <- colorRampPalette(c("red", "white", "blue"))(100)
    #   }
    #   
    #   # Create plot with advanced options
    #   corrplot::corrplot(
    #     cor_matrix,
    #     method = if (input$corr_method_type)
    #       "color"
    #     else
    #       "ellipse",
    #     order = input$corr_order,
    #     tl.cex = input$corr_tl_cex,
    #     type = if (input$corr_type)
    #       "full"
    #     else
    #       "upper",
    #     diag = input$corr_diag,
    #     addCoef.col = if (input$corr_addCoef)
    #       tolower(input$corr_coef_color)
    #     else
    #       NULL,
    #     tl.srt = input$corr_tl_srt,
    #     col = col,
    #     bg = if (input$corr_bg)
    #       "darkgrey"
    #     else
    #       "white"
    #   )
    # }
    # )
    # 
    # # Render the correlation plot
    # output$corr_plot <- renderPlot({
    #   input$corr_update
    #   generate_corr_plot()
    # })
    # 
    # # Download handler for correlation plot
    # output$corr_download <- downloadHandler(
    #   filename = function() {
    #     paste("correlation-plot-", Sys.Date(), ".png", sep = "")
    #   },
    #   content = function(file) {
    #     # Set up PNG device with appropriate dimensions
    #     png(file,
    #         width = 1200,
    #         height = 900,
    #         res = 300)
    #     
    #     # Generate the plot
    #     generate_corr_plot()
    #     dev.off()
    #   }
    # )
    
    
    ###################-
    #### HISTOGRAM ####
    ###################-

    # Reactive data preparation for histogram
    hist_data <- reactive({
      req(input$hist_question, input$hist_countries)

      # Get question ID
      q_id <- get_question_id(input$hist_question)

      # Prepare data
      plot_data <- indiv_data %>%
        dplyr::filter(B_COUNTRY_ALPHA %in% input$hist_countries) %>%
        dplyr::select(country = B_COUNTRY, response = !!q_id) %>%
        dplyr::mutate(
          country = as.character(country),
          response = as.numeric(response)  # Ensure numeric for histogram
        ) %>%
        stats::na.omit()

      # Add metadata
      list(data = plot_data, question = input$hist_question)
    })

    # Render histogram plot
    output$hist_plot <- renderPlotly({
      data <- hist_data()
      plot_data <- data$data
      if (is.null(plot_data) || nrow(plot_data) == 0) return(NULL)

      # Calculate mean for each country
      mean_data <- plot_data %>%
        dplyr::group_by(country) %>%
        dplyr::summarise(mean = mean(response, na.rm = TRUE))

      # Create base plot
      if (input$hist_facet) {
        # Faceted view
        p <- ggplot2::ggplot(plot_data, aes(x = response, fill = country)) +
          {if (input$hist_type == "Stacked")
            ggplot2::geom_histogram(position = "stack",
                                    bins = input$hist_bins,
                                    alpha = 0.8)
            else
              ggplot2::geom_histogram(position = "identity",
                                      bins = input$hist_bins,
                                      alpha = 0.6
              )} +
          ggplot2::geom_vline(
            data = mean_data,
            ggplot2::aes(xintercept = mean, color = country),
            linetype = "dashed",
            linewidth = 1
          ) +
          ggplot2::facet_wrap( ~ country, scales = "free") +
          ggplot2::labs(
            title = paste("Distribution of", data$question),
            x = "Response Value",
            y = if (input$hist_type == "Density")
              "Density"
            else
              "Count"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            legend.position = "none",
            strip.text = ggplot2::element_text(size = 12, face = "bold")
          )
      } else {
        # Overlaid view
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = response, 
                                                     fill = country)) +
          {if (input$hist_type == "Frequency")
            ggplot2::geom_histogram(position = "identity",
                                    bins = input$hist_bins,
                                    alpha = 0.5)
            else if (input$hist_type == "Density")
              ggplot2::geom_density(alpha = 0.4, adjust = 1.5)
            else
              ggplot2::geom_histogram(position = "stack",
                                      bins = input$hist_bins,
                                      alpha = 0.8
              )} +
          ggplot2::geom_vline(
            data = mean_data,
            ggplot2::aes(xintercept = mean, color = country),
            linetype = "dashed",
            size = 1
          ) +
          ggplot2::labs(
            title = paste("Distribution of", data$question),
            x = "Response Value",
            y = if (input$hist_type == "Density")
              "Density"
            else
              "Count",
            fill = "Country"
          ) +
          ggplot2::theme_minimal()
      }

      # Add normal curve if requested
      if (input$hist_curve && input$hist_type != "Stacked") {
        # Calculate parameters outside stat_function
        x_range <- range(plot_data$response, na.rm = TRUE)
        bin_width <- diff(x_range) / input$hist_bins
        n_total <- nrow(plot_data)
        mean_val <- mean(plot_data$response, na.rm = TRUE)
        sd_val <- stats::sd(plot_data$response, na.rm = TRUE)
        
        if (input$hist_type == "Frequency") {
          p <- p +
            ggplot2::stat_function(
              fun = function(x) {
                stats::dnorm(x, mean = mean_val, sd = sd_val) * n_total * bin_width
              },
              color = "black",
              size = 1,
              linetype = "dotted"
            )
        } else if (input$hist_type == "Density") {
          p <- p +
            ggplot2::stat_function(
              fun = function(x) {
                stats::dnorm(x, mean = mean_val, sd = sd_val)
              },
              color = "black",
              size = 1,
              linetype = "dotted"
            )
        }
      }

      # Convert to plotly
      plotly::ggplotly(p) %>%
        plotly::layout(
          legend = list(orientation = "h", y = -0.2),
          hoverlabel = list(bgcolor = "white")
        )
    })
    
    
    ###########################-
    #### CORRELATION MODEL ####
    ###########################-
    
    corr_model_data <- eventReactive(input$corr_model_run, {
      # Require both variables to be selected
      req(input$corr_model_var1, input$corr_model_var2)
      
      # Get variable information
      var_info <- get_var_info()
      
      # Get question IDs from labels
      var1_id <- get_question_id(input$corr_model_var1)
      var2_id <- get_question_id(input$corr_model_var2)
      
      count_select <- input$corr_model_countries
      
      data <- indiv_data
      
      # Apply country filter if selected
      if (!is.null(count_select)) {
        data <- data %>%
          filter(B_COUNTRY_ALPHA %in% count_select)
      }
      
      # Select relevant columns and omit missing values
      data <- data %>%
        dplyr::select(var1 = !!var1_id,
               var2 = !!var2_id,
               country = B_COUNTRY) %>%
        stats::na.omit()  # Remove any rows with missing values
      
      
      # If ordered factor, convert to numeric
      if("ordered" %in% class(data$var1)){
        data$var1 <- as.numeric(data$var1)
      }
      
      if("ordered" %in% class(data$var2)){
        data$var2 <- as.numeric(data$var2)
      }
      
      data
      
    })

    # Render correlation results
    output$corr_mod_results <- renderPrint({
      # Get the prepared data
      data <- corr_model_data()
      
      # Check for sufficient data
      if (nrow(data) < 20) {
        return("Insufficient data to compute correlation. Need at least 20 complete observations.")
      }
      
      # Compute correlation
      cor_test <- stats::cor.test(data$var1,
                                  data$var2,
                                  method = tolower(input$corr_choice),
                                  exact = FALSE)
      
      # Format and display results
      cat(input$corr_choice, "'s Rank Correlation Analysis\n")
      cat("===================================\n")
      cat("Variable 1: ", input$corr_model_var1, "\n")
      cat("Variable 2: ", input$corr_model_var2, "\n")
      cat("Countries: ", paste(input$corr_model_countries, collapse = ", "), "\n")
      cat("Number of complete observations: ", nrow(data), "\n\n")
      
      cat("Correlation coefficient (tau): ", 
          round(cor_test$estimate, 4), "\n")
      cat("95% Confidence Interval: [", 
          cor_test$conf.int[1], ", ",
          cor_test$conf.int[2], "]\n")
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
    output$corr_mod_plot <- renderPlotly({
      # Get the prepared data
      data <- corr_model_data()
      
      # Check for sufficient data
      if (nrow(data) < 3) {
        return(NULL)  # Don't render plot if insufficient data
      }
      
      # Create the plot
      p <- ggplot2::ggplot(data, ggplot2::aes(
        x = var1,
        y = var2,
        color = country,
        text = paste(
          "Country:",
          country,
          "<br>Var1:",
          round(var1, 2),
          "<br>Var2:",
          round(var2, 2)
        )
      )) +
        ggplot2::geom_point(alpha = 0.6, size = 2) +
        ggplot2::geom_smooth(method = "lm",
                             se = TRUE,
                             formula = y ~ x) +
        ggplot2::geom_jitter(width = 0.2,
                             alpha = 0.3,
                             size = 1.5) +
        ggplot2::labs(
          title = paste(
            "Relationship between",
            input$corr_model_var1,
            "and",
            input$corr_model_var2
          ),
          x = input$corr_model_var1,
          y = input$corr_model_var2,
          color = "Country"
        ) +
        ggplot2::scale_color_viridis_d(option = "plasma") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "bottom",
          plot.title = ggplot2::element_text(size = 14, face = "bold")
        )
      
      # Convert to interactive plot
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(legend = list(orientation = "h", y = -0.2))
    })
    
    # Render data table
    output$corr_mod_data <- renderDT({
      # Get the prepared data
      data <- corr_model_data()
      
      # Rename columns for display
      names(data) <- c(input$corr_model_var1, input$corr_model_var2, "Country")
      
      # Create datatable
      DT::datatable(
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
    
    
    ###############-
    #### ANOVA ####
    ###############-
    
    # Reactive data preparation for ANOVA
    anova_data <- eventReactive(input$anova_run, {
      req(input$anova_var, input$anova_countries)
      
      var_id <- get_question_id(input$anova_var)
      
      data <- indiv_data
      
      # filtering based on countries
      if (!is.null(input$anova_countries)) {
        data <- data %>%
          dplyr::filter(B_COUNTRY_ALPHA %in% input$anova_countries)
      }
      
      # selecting variable
      data <- data %>%
        dplyr::select(value = dplyr::all_of(var_id), country = B_COUNTRY) %>%
        stats::na.omit()

      # If ordered factor, convert to numeric
      if("ordered" %in% class(data[ , "value"])){
        data[ , "value"] <- as.numeric(data[ , "value"])
      }
      
      data
      
    })
    
    # Fit the ANOVA once per Run Analysis click, then share the model across
    # the summary, post-hoc test, assumptions, and diagnostics outputs.
    anova_fit <- eventReactive(input$anova_run, {
      data <- anova_data()
      
      if (nrow(data) < 10 || length(unique(data$country)) < 2) {
        return(list(data = data, model = NULL))
      }
      
      list(
        data = data,
        model = stats::aov(value ~ country, data = data)
      )
    })
    
    # Render ANOVA results
    output$anova_results <- renderPrint({
      fit <- anova_fit()
      data <- fit$data
      
      if (nrow(data) < 10) {
        return("Insufficient data: Need at least 10 observations.")
      }
      
      if (length(unique(data$country)) < 2) {
        return("Insufficient groups: Need at least 2 countries.")
      }
      
      cat("Analysis of Variance (ANOVA)\n")
      cat("============================\n")
      cat("Variable: ", input$anova_var, "\n")
      cat("Countries: ", paste(unique(data$country), collapse = ", "), "\n")
      cat("Number of complete observations: ", nrow(data), "\n\n")
      
      summary(fit$model)
    })
    
    # Render post-hoc test results
    output$posthoc_results <- renderPrint({
      fit <- anova_fit()
      if (is.null(fit$model)) return(NULL)
      
      tukey <- stats::TukeyHSD(fit$model)
      
      cat("Tukey Honest Significant Differences\n")
      cat("====================================\n")
      print(tukey)
    })
    
    # Render ANOVA plot
    output$anova_plot <- renderPlotly({
      data <- anova_data()
      
      if (nrow(data) < 10 || length(unique(data$country)) < 2) {
        return(NULL)
      }
      
      p <- ggplot2::ggplot(data, ggplot2::aes(x = country, y = value, 
                                              fill = country)) +
        ggplot2::geom_boxplot(alpha = 0.8, outlier.shape = NA) +
        ggplot2::geom_jitter(width = 0.2,
                             alpha = 0.3,
                             size = 1.5) +
        ggplot2::labs(
          title = paste("Distribution of", input$anova_var, "by Country"),
          x = "Country",
          y = input$anova_var
        ) +
        ggplot2::scale_fill_viridis_d(option = "magma") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      
      plotly::ggplotly(p) %>%
        plotly::layout(legend = list(orientation = "h", y = -0.2))
    })
    
    # Render assumptions check
    output$assumptions_check <- renderPrint({
      fit <- anova_fit()
      data <- fit$data
      model <- fit$model
      
      if (is.null(model)) return(NULL)
      
      cat("ANOVA Assumptions Check\n")
      cat("=======================\n\n")
      
      residual_values <- stats::residuals(model)
      if (length(residual_values) > 5000) {
        residual_values <- sample(residual_values, 5000)
        cat("Note: Shapiro-Wilk test sampled 5,000 residuals because the test is limited to 5,000 values.\n\n")
      }
      
      shapiro_test <- stats::shapiro.test(residual_values)
      cat("1. Normality of Residuals (Shapiro-Wilk test):\n")
      cat("   W =", round(shapiro_test$statistic, 4), 
          "p-value =", format.pval(shapiro_test$p.value, digits = 4), "\n")
      if (shapiro_test$p.value > 0.05) {
        cat("   -> Residuals are normally distributed (p > 0.05)\n\n")
      } else {
        cat("   -> WARNING: Residuals are not normally distributed (p < 0.05)\n\n")
      }
      
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
      fit <- anova_fit()
      if (is.null(fit$model)) return(NULL)
      
      par(mfrow = c(2, 2))
      plot(fit$model, ask = FALSE)
    })
    
    
    ####################-
    #### Linear Reg ####
    ####################-
    
    # Reactive data preparation for regression
    regression_data <- eventReactive(input$regression_run, {
      req(input$regression_dep, input$regression_indep)

      dep_id <- get_question_id(input$regression_dep)
      indep_ids <- sapply(input$regression_indep, get_question_id, USE.NAMES = FALSE)

      if(!all(c(dep_id, indep_ids) %in% names(indiv_data))) {
        showNotification("Selected variables not in dataset", type = "error")
        return(NULL)
      }

      data <- indiv_data
      if (!is.null(input$regression_country)) {
        data <- data %>%
          dplyr::filter(B_COUNTRY_ALPHA %in% input$regression_country)
      }

      data <- data %>%
        dplyr::select(dplyr::all_of(c(dep_id, indep_ids))) %>%
        stats::na.omit()

      # If ordered factor, convert to numeric
      if("ordered" %in% class(data[ , dep_id])){
        data[ , dep_id] <- as.numeric(data[ , dep_id])
      }
      
      list(
        data = data,
        dep_label = input$regression_dep,
        indep_labels = input$regression_indep,
        dep_id = dep_id,
        indep_ids = indep_ids
      )
    })

    # Fit the full regression model once per Run Regression click and share it
    # between all model summary/coefficient/diagnostic outputs.
    regression_fit <- eventReactive(input$regression_run, {
      result <- regression_data()
      if (is.null(result)) return(NULL)
      data <- result$data

      if (nrow(data) < 10 || ncol(data) < 2) {
        return(c(result, list(model = NULL, formula = NULL)))
      }

      formula <- stats::as.formula(
        paste(names(data)[1], "~", paste(names(data)[-1], collapse = " + "))
      )

      c(result, list(
        model = stats::lm(formula, data = data),
        formula = formula
      ))
    })

    # Render model summary
    output$regression_summary <- renderPrint({
      fit <- regression_fit()
      if (is.null(fit)) return(NULL)
      data <- fit$data

      if (nrow(data) < 10) {
        return("Insufficient data: Need at least 10 complete observations.")
      }

      if (ncol(data) < 2) {
        return("Insufficient variables: Need at least one independent variable.")
      }

      cat("Linear Regression Model Summary\n")
      cat("==============================\n")
      cat("Country: ", input$regression_country, "\n")
      cat("Dependent variable: ", fit$dep_label, "\n")
      cat("Independent variables: ", paste(fit$indep_labels, collapse = ", "), "\n")
      cat("Number of complete observations: ", nrow(data), "\n\n")

      summary(fit$model)
    })

    # Render coefficient table
    output$regression_coef <- renderDT({
      fit <- regression_fit()
      if (is.null(fit) || is.null(fit$model)) return(NULL)
      data <- fit$data

      coef_table <- broom::tidy(fit$model) %>%
        dplyr::mutate(
          term = dplyr::case_when(
            term == "(Intercept)" ~ "Intercept",
            term %in% names(data) ~ {
              var_id <- term
              if (var_id == names(data)[1]) {
                fit$dep_label
              } else {
                idx <- which(fit$indep_ids == var_id)
                if (length(idx) > 0)
                  fit$indep_labels[idx]
                else
                  var_id
              }
            },
            TRUE ~ term
          ),
          p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
        )

      DT::datatable(
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
        DT::formatRound(columns = c("estimate", "std.error", "statistic"),
                        digits = 4)
    })

    # Render diagnostic plots
    output$regression_diag <- renderPlot({
      fit <- regression_fit()
      if (is.null(fit) || is.null(fit$model)) return(NULL)

      par(mfrow = c(2, 2))
      plot(fit$model, ask = FALSE)
    })

    # Render prediction plot
    output$regression_prediction <- renderPlotly({
      fit <- regression_fit()
      if (is.null(fit) || is.null(fit$model)) return(NULL)
      data <- fit$data

      # Use first independent variable for the visual bivariate relationship.
      x_var_id <- names(data)[2]
      y_var_id <- names(data)[1]
      x_label <- fit$indep_labels[1]
      y_label <- fit$dep_label

      formula <- stats::as.formula(paste(y_var_id, "~", x_var_id))
      prediction_model <- stats::lm(formula, data = data)

      x_range <- seq(min(data[[x_var_id]], na.rm = TRUE),
                     max(data[[x_var_id]], na.rm = TRUE),
                     length.out = 100)
      pred_data <- data.frame(x = x_range)
      names(pred_data) <- x_var_id
      pred <- stats::predict(prediction_model, 
                             newdata = pred_data, 
                             interval = "confidence")

      plot_data <- cbind(pred_data, pred) %>%
        dplyr::rename(fit = 2, lwr = 3, upr = 4)

      point_data <- data
      if (nrow(point_data) > 5000) {
        point_data <- dplyr::slice_sample(point_data, n = 5000)
      }

      p <- ggplot2::ggplot() +
        ggplot2::geom_point(
          data = point_data,
          ggplot2::aes(x = .data[[x_var_id]], y = .data[[y_var_id]]),
          alpha = 0.5
        ) +
        ggplot2::geom_line(
          data = plot_data,
          ggplot2::aes(x = .data[[x_var_id]], y = fit),
          linewidth = 1
        ) +
        ggplot2::geom_ribbon(
          data = plot_data,
          ggplot2::aes(x = .data[[x_var_id]], ymin = lwr, ymax = upr),
          alpha = 0.2
        ) +
        ggplot2::labs(
          title = paste("Regression of", y_label, "on", x_label),
          x = x_label,
          y = y_label
        ) +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })
    
    
    # models_phylo_lm_server(
    #   "models_phylo_lm",
    #   wvs_country = wvs_country2,
    #   codebook_data = codebook_data,
    #   lang_tree = country_phylogeny_tree,
    #   lang_country_map = country_phylogeny
    # )
    # 
    # models_phylo_glm_server(
    #   "models_phylo_glm",
    #   wvs_country = wvs_country2,
    #   codebook_data = codebook_data,
    #   lang_tree = country_phylogeny_tree,
    #   lang_country_map = country_phylogeny
    # )
    
    geo_viz_map_server(
      "geo_viz_map",
      indiv_data        = indiv_data,
      codebook_data     = codebook_data,
      world_shape       = world_shape,
      country_phylogeny = country_phylogeny,
      grouped_vars      = grouped_questions
    )
    
    phylo_viz_tree_server(
      "phylo_viz_tree",
      indiv_data = indiv_data,
      codebook_data = codebook_data,
      lang_tree = country_phylogeny_tree,
      lang_country_map = country_phylogeny,
      grouped_vars = grouped_questions
    )
    
    
    
    
    ######################-
    #### Codebook Gen ####
    ######################-

    # Browse table
    output$table <- renderDT({
      datatable(
        codebook_df[, c("question_id", "question_title", "section", "variable_class")],
        selection = "single",
        rownames = FALSE,
        options = list(
          scrollY = "500px",
          scrollX = TRUE,
          paging = FALSE,
          searching = TRUE,
          info = FALSE
        )
      )
    })
    
    # Browse details
    observeEvent(input$table_rows_selected, {
      idx <- input$table_rows_selected
      if (length(idx) == 1) {
        row <- codebook_df[idx, ]
        output$details <- renderUI({
          tagList(
            h3(row$question_id, ": ", row$question_title),
            p(strong("Section: "), row$section),
            p(strong("Question: "), row$question_text),
            p(strong("Variable class: "), row$variable_class),
            p(strong("Values: ")),
            if (length(row$values[[1]]) > 0) {
              tags$ul(lapply(row$values[[1]], tags$li))
            } else {
              p("No specific values listed.")
            }
          )
        })
      } else {
        output$details <- renderUI({ p("Select a variable from the table.") })
      }
    })
    
  }) # end server logic
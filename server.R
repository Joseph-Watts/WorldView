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
    
    # orig_country_data modified to have full question as name of column - NEEDS REWORK
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
      d <- orig_codebook_data[, c(1, 2, 10)]
      d <- split(d, d$Section)
      c <- lapply(d, function(group) {
        stats::setNames(group$Col_ID, group$ColLab)
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
    
    
    #############################-
    #### PDF & CODEBOOK VIEW ####
    #############################-
    
    # Master Survey Questionnaire PDF
    output$surveyview <- renderUI({
      tags$iframe(style = "height:100vh; width:100%; scrolling=yes",
                  src = "F00011012-WVS_WAVE_7_MASTER_QUESTIONNAIRE_2017-2021_ENGLISH.pdf")
    })
    
    # Codebook PDF
    output$codebookview <- renderUI({
      tags$iframe(style = "height:100vh; width:100%; scrolling=yes",
                  src = "F00011055-WVS7_Codebook_Variables_report_V6.0.pdf")
    })
    
    
    ####################-
    #### DataTables ####
    ####################-
    
    # Reactive control for selecting country
    output$raw_selectCountry <- renderUI({
     shinyWidgets::pickerInput(
        inputId = "raw_country",
        label = "Select Country",
        choices = picker_country_list,
        multiple = FALSE,
        selected = NULL,
        options = list(
          `live-search` = TRUE,
          `size` = 20
        )
      )
    })
    
    raw_filtering <- reactive({
        if(is.null(input$raw_country)) {
          get_I_longID() |> dplyr::select(-S007)
        } else {
          get_I_longID() |>
            dplyr::filter(B_COUNTRY_ALPHA == input$raw_country) |>
            dplyr::select(-S007)
          # currently, filtering does not work for multiples countries as expected, reverted back to single country selection
        }
    })
    
    output$raw_filtered_country <- DT::renderDataTable({
      DT::datatable(data = raw_filtering()|>
                      dplyr::rename(Country = B_COUNTRY, `Country ISO` = B_COUNTRY_ALPHA),
                    options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # Data table - Country aggregate responses
    output$Table_country <- DT::renderDataTable({
      DT::datatable(data = get_C_data() |>
                      dplyr::mutate(across(where(is.numeric), ~ round(., 2))) |>
                      dplyr::rename(Country = B_COUNTRY, `Country ISO` = B_COUNTRY_ALPHA),
                    options = list(scrollX = TRUE))
    })
    
    
    #################-
    #### Missing ####
    #################-
    
    # TODO add vis_miss_ly code provided by Nick
    output$Missing <- renderPlot({
      naniar::vis_miss(get_C_data(), cluster = input$cluster_ctry, sort = input$sort_ctry) +
        ggplot2::theme(axis.text.x = element_blank())
    })
    
    output$Indiv_missing_with_ratio <- renderPlot({
      d <- sample_with_missing_ratio(get_I_data(), sample_size = 2500)
      
      naniar::vis_miss(d, cluster = input$cluster_indiv, sort = input$sort_indiv) +
        ggplot2::theme(axis.text.x = element_blank())
    })
    
    output$Top_miss_indiv <- renderPlot({
      top_miss <- naniar::miss_var_summary(get_I_data()) %>%
        dplyr::slice_head(n = 15) %>%
        dplyr::mutate(
          pct_miss = as.numeric(pct_miss),
          variable = forcats::fct_reorder(variable, pct_miss, .desc = TRUE)
        )
      
      top_miss %>%
        ggplot2::ggplot(aes(x = variable, y = pct_miss, fill = variable)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::geom_text(
          ggplot2::aes(label = round(pct_miss, 1)),
          vjust = -0.5,
          size = 4.5,
          fontface = "bold"
        ) +
        ggplot2::scale_fill_viridis_d(option = "viridis") +
        ggplot2::labs(
          title = "Percentage of Missing Data of Individual Responses",
          x = "Variable",
          y = "Percentage Missing",
          fill = "Variable"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 16),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
          legend.position = "none"
        )
    })
    
    output$Top_miss_country <- renderPlot({
      top_miss <- naniar::miss_var_summary(get_C_data()) %>%
        dplyr::slice_head(n = 15) %>%
        dplyr::mutate(
          pct_miss = as.numeric(pct_miss),
          variable = forcats::fct_reorder(variable, pct_miss, .desc = TRUE)
        )
      
      top_miss %>%
        ggplot2::ggplot(aes(x = variable, y = pct_miss, fill = variable)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::geom_text(
          ggplot2::aes(label = round(pct_miss, 1)),
          vjust = -0.5,
          size = 4.5,
          fontface = "bold"
        ) +
        ggplot2::scale_fill_viridis_d(option = "viridis") +
        ggplot2::labs(
          title = "Percentage of Missing Data in Country Data Consolidation",
          x = "Variable",
          y = "Percentage Missing",
          fill = "Variable"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 16),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
          legend.position = "none"
        )
    })
    
    
    ####################-
    #### Univariate ####
    ####################-
    
    univariate_data <- reactive({
      req(input$univar_question, input$univar_countries)
      
      # Get question ID
      q_id <- get_question_id(input$univar_question)
      
      # Prepare data - convert country to character
      orig_data <- get_I_data() %>%
        dplyr::filter(B_COUNTRY_ALPHA %in% input$univar_countries) %>%
        dplyr::select(country = B_COUNTRY, response = all_of(q_id)) %>%
        dplyr::mutate(country = as.character(country))
      
      num_data <- indiv_ordinal %>%
        dplyr::filter(B_COUNTRY_ALPHA %in% input$univar_countries) %>% 
        dplyr::select(country = B_COUNTRY, response = all_of(q_id)) %>%
        dplyr::mutate(
          country = as.character(country),
          response = as.numeric(response)
        )
      
      # Determine variable type
      is_factor <- is.factor(orig_data$response)
      is_numeric <- is.numeric(num_data$response)
      n_unique <- length(unique(stats::na.omit(orig_data$response)))
      
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
      tabs <- lapply(c("Selected Sample", country_names), function(ctry_name) {
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
          tabPanel(title = name, tabs[[which(c("Selected Sample", country_names) == name)]])
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
      data <- orig_indiv_data
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
        caption = paste("Cross-tabulation of", input$bivariate_var1, "and", input$bivariate_var2)
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
        
        plot_data <- orig_indiv_data %>%
          dplyr::filter(B_COUNTRY_ALPHA %in% input$bar_countries) %>%
          dplyr::select(country = B_COUNTRY, response = !!q_id) %>%
          dplyr::mutate(response = as.factor(response)) %>%
          dplyr::count(country, response) %>%
          dplyr::group_by(country) %>%
          dplyr::mutate(percent = n / sum(n) * 100)
        
        if (input$bar_type == "Percentage") {
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = response, y = percent, fill = country)) +
            ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
            ggplot2::labs(y = "Percentage (%)", x = "Country", title = paste("Distribution of", input$bar_question)) +
            ggplot2::scale_fill_viridis_d()
          
        } else if (input$bar_type == "Count") {
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = response, y = n, fill = country)) +
            ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
            ggplot2::labs(y = "Count", x = "Country", title = paste("Distribution of", input$bar_question)) +
            ggplot2::scale_fill_viridis_d()
          
        } else if (input$bar_type == "Stacked") {
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = country, y = percent, fill = response)) +
            ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE)) +
            ggplot2::labs(y = "Percentage (%)", 
                 title = paste("Distribution of", input$bar_question)) +
            ggplot2::scale_fill_viridis_d(option = "D") +
            ggplot2::theme(legend.title = ggplot2::element_blank())
          
        } else {
          # Staggered view
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = response, y = n, fill = response)) +
            ggplot2::geom_col() +
            ggplot2::facet_wrap(~country, ncol = 1, scales = "fixed") +
            ggplot2::labs(y = "Count", title = paste("Distribution of", input$bar_question)) +
            ggplot2::scale_fill_viridis_d(option = "D") +
            ggplot2::theme(legend.position = "none")
        }
        
        # Remove x-axis title for ALL display types
        p <- p + ggplot2::labs(x = NULL) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        
        plotly::ggplotly(p) %>% 
          plotly::layout(legend = list(orientation = "h", y = -0.2))
    })
    
    
    #####################-
    #### Scatterplot ####
    #####################-
    
    output$scatter_plot <- renderPlotly({
      req(input$scatter_x, input$scatter_y, input$scatter_countries)
      
      req(input$scatter_x, input$scatter_y)
      
      # Get question IDs
      var_info <- get_var_info()
      x_id <- var_info$Col_ID[var_info$ColLab == input$scatter_x]
      y_id <- var_info$Col_ID[var_info$ColLab == input$scatter_y]
      
      # Prepare data
      plot_data <- get_I_data()
      if (!is.null(input$scatter_countries)) {
        plot_data <- plot_data %>%
          dplyr::filter(B_COUNTRY_ALPHA %in% input$scatter_countries)
      }
      
      # Sample data for performance
      if (nrow(plot_data) > input$scatter_sample) {
        plot_data <- plot_data %>% dplyr::sample_frac((input$scatter_sample) / 100)
      }
      
      plot_data <- plot_data %>%
        dplyr::select(x = !!x_id,
                      y = !!y_id,
                      country = B_COUNTRY_ALPHA)
      
      # Create plot
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, color = country)) +
        ggplot2::geom_point(alpha = 0.6) +
        ggplot2::geom_jitter(width = 0.2,
                             alpha = 0.3,
                             size = 1.5) +
        ggplot2::geom_smooth(method = "lm", se = FALSE) +
        ggplot2::labs(
          title = paste(input$scatter_x, "vs", input$scatter_y),
          x = input$scatter_x,
          y = input$scatter_y
        ) +
        ggplot2::theme_minimal()
      
      plotly::ggplotly(p)
    })
    
    
    ##################-
    #### Corrplot ####
    ##################-
    
    generate_corr_plot <- reactive({
      req(input$corr_questions, length(input$corr_questions) > 1)
      
      # Get question IDs
      var_info <- get_var_info()
      q_ids <- sapply(input$corr_questions, function(q) {
        var_info$Col_ID[var_info$ColLab == q]
      }, USE.NAMES = FALSE)
      
      # Prepare data
      plot_data <- indiv_ordinal
      if (!is.null(input$corr_countries)) {
        plot_data <- plot_data %>%
          dplyr::filter(B_COUNTRY_ALPHA %in% input$corr_countries)
      }
      
      plot_data <- plot_data %>%
        dplyr::select(all_of(q_ids))
      
      # Compute correlation matrix
      cor_matrix <- stats::cor(plot_data,
                               use = "pairwise.complete.obs",
                               method = tolower(input$corr_method))
      
      # Create color palette based on selection
      if(input$corr_palette == "Viridis") {
        col <- viridis::viridis(100)
      } else {
        # Red-to-blue gradient palette
        col <- colorRampPalette(c("red", "white", "blue"))(100)
      }
      
      # Create plot with advanced options
      corrplot::corrplot(
        cor_matrix,
        method = if (input$corr_method_type)
          "color"
        else
          "ellipse",
        order = input$corr_order,
        tl.cex = input$corr_tl_cex,
        type = if (input$corr_type)
          "full"
        else
          "upper",
        diag = input$corr_diag,
        addCoef.col = if (input$corr_addCoef)
          tolower(input$corr_coef_color)
        else
          NULL,
        tl.srt = input$corr_tl_srt,
        col = col,
        bg = if (input$corr_bg)
          "darkgrey"
        else
          "white"
      )
    }
    )
    
    # Render the correlation plot
    output$corr_plot <- renderPlot({
      input$corr_update
      generate_corr_plot()
    })
    
    # Download handler for correlation plot
    output$corr_download <- downloadHandler(
      filename = function() {
        paste("correlation-plot-", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        # Set up PNG device with appropriate dimensions
        png(file,
            width = 1200,
            height = 900,
            res = 300)
        
        # Generate the plot
        generate_corr_plot()
        dev.off()
      }
    )
    
    
    ###################-
    #### HISTOGRAM ####
    ###################-

    # Reactive data preparation for histogram
    hist_data <- reactive({
      req(input$hist_question, input$hist_countries)

      # Get question ID
      q_id <- get_question_id(input$hist_question)

      # Prepare data
      plot_data <- orig_indiv_data %>%
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
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = response, fill = country)) +
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
      
      # Prepare data from preprocessed numeric dataset
      data <- indiv_ordinal
      
      # Apply country filter if selected
      if (!is.null(input$corr_model_countries)) {
        data <- data %>%
          filter(B_COUNTRY_ALPHA %in% input$corr_model_countries)
      }
      
      # # Apply sampling for performance - MOMENTARILY DISABLED
      # if (nrow(data) > input$corr_model_sample) {
      #   data <- data %>% sample_n(input$corr_model_sample)
      # }
      
      # Select relevant columns and omit missing values
      data %>%
        dplyr::select(var1 = !!var1_id,
               var2 = !!var2_id,
               country = B_COUNTRY) %>%
        stats::na.omit()  # Remove any rows with missing values
    })

    # Render correlation results
    output$corr_mod_results <- renderPrint({
      # Get the prepared data
      data <- corr_model_data()
      
      # Check for sufficient data
      if (nrow(data) < 3) {
        return("Insufficient data to compute correlation. Need at least 3 complete observations.")
      }
      
      # Compute Kendall's correlation
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
      
      # Get variable information
      var_info <- get_var_info()
      var_id <- get_question_id(input$anova_var)
      
      # Prepare data from preprocessed numeric dataset
      data <- indiv_ordinal
      
      # Apply country filter if selected
      if (!is.null(input$anova_countries)) {
        data <- data %>%
          dplyr::filter(B_COUNTRY_ALPHA %in% input$anova_countries)
      }
      
      # # Apply sampling for performance - MOMENTARILY DISABLED
      # if (nrow(data) > input$anova_sample) {
      #   data <- data %>% sample_n(input$anova_sample)
      # }
      
      # Select relevant columns and omit missing values
      data %>%
        dplyr::select(value = !!var_id, country = B_COUNTRY) %>%
        stats::na.omit()  # Remove any rows with missing values
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
      model <- stats::aov(value ~ country, data = data)
      
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
      model <- stats::aov(value ~ country, data = data)
      tukey <- stats::TukeyHSD(model)
      
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
      p <- ggplot2::ggplot(data, ggplot2::aes(x = country, y = value, fill = country)) +
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
      data <- anova_data()
      
      # Check for sufficient data
      if (nrow(data) < 10 || length(unique(data$country)) < 2) {
        return(NULL)
      }
      
      model <- stats::aov(value ~ country, data = data)
      
      cat("ANOVA Assumptions Check\n")
      cat("=======================\n\n")
      
      # Normality of residuals
      shapiro_test <- stats::shapiro.test(stats::residuals(model))
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
      
      model <- stats::aov(value ~ country, data = data)
      
      # Set up 2x2 grid
      par(mfrow = c(2, 2))
      plot(model, ask = FALSE)
    })
    
    
    ####################-
    #### Linear Reg ####
    ####################-
    
    # Reactive data preparation for regression
    regression_data <- eventReactive(input$regression_run, {
      req(input$regression_dep, input$regression_indep)

      # Get question IDs
      dep_id <- get_question_id(input$regression_dep)
      indep_ids <- sapply(input$regression_indep, get_question_id, USE.NAMES = FALSE)


      if(!all(c(dep_id, indep_ids) %in% names(indiv_ordinal))) {
        showNotification("Selected variables not in dataset", type = "error")
        return(NULL)
      }

      # Prepare data from preprocessed numeric dataset
      data <- indiv_ordinal

      # Apply country filter if selected
      if (!is.null(input$regression_country)) {
        data <- data %>%
          dplyr::filter(B_COUNTRY_ALPHA %in% input$regression_country)
      }

      # # Apply sampling for performance - MOMENTARILY DISABLED
      # if (nrow(data) > input$regression_sample) {
      #   data <- data %>% sample_n(input$regression_sample)
      # }

      # Select relevant columns and omit missing values
      data <- data %>%
        dplyr::select(all_of(c(dep_id, indep_ids))) %>%
        stats::na.omit()

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
      formula <- stats::as.formula(paste(names(data)[1], "~", paste(names(data)[-1], collapse = " + ")))

      # Run regression
      model <- stats::lm(formula, data = data)

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
      formula <- stats::as.formula(paste(names(data)[1], "~", paste(names(data)[-1], collapse = " + ")))
      model <- stats::lm(formula, data = data)

      # Create coefficient table with labels
      coef_table <- broom::tidy(model) %>%
        dplyr::mutate(
          term = dplyr::case_when(
            term == "(Intercept)" ~ "Intercept",
            term %in% names(data) ~ {
              # Map variable names to labels
              var_id <- term
              if (var_id == names(data)[1]) {
                result$dep_label
              } else {
                idx <- which(result$indep_ids == var_id)
                if (length(idx) > 0)
                  result$indep_labels[idx]
                else
                  var_id
              }
            },
            TRUE ~ term
          ),
          p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
        )

      # Create datatable
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
      result <- regression_data()
      data <- result$data

      # Check for sufficient data
      if (nrow(data) < 10 || ncol(data) < 2) {
        return(NULL)
      }

      # Build formula using IDs
      formula <- stats::as.formula(paste(names(data)[1], "~", paste(names(data)[-1], collapse = " + ")))
      model <- stats::lm(formula, data = data)

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
      formula <- stats::as.formula(paste(y_var_id, "~", x_var_id))
      model <- stats::lm(formula, data = data)

      # Generate prediction data
      x_range <- seq(min(data[[x_var_id]], na.rm = TRUE),
                     max(data[[x_var_id]], na.rm = TRUE),
                     length.out = 100)
      pred_data <- data.frame(x = x_range)
      names(pred_data) <- x_var_id
      pred <- stats::predict(model, newdata = pred_data, interval = "confidence")

      # Combine prediction data
      plot_data <- cbind(pred_data, pred) %>%
        dplyr::rename(fit = 2, lwr = 3, upr = 4)

      # Create plot
      p <- ggplot2::ggplot() +
        ggplot2::geom_point(
          data = data,
          ggplot2::aes(x = .data[[x_var_id]], y = .data[[y_var_id]]),
          alpha = 0.5,
          color = "#3366CC"
        ) +
        ggplot2::geom_line(
          data = plot_data,
          ggplot2::aes(x = .data[[x_var_id]], y = fit),
          color = "#FF3366",
          linewidth = 1
        ) +
        ggplot2::geom_ribbon(
          data = plot_data,
          ggplot2::aes(x = .data[[x_var_id]], ymin = lwr, ymax = upr),
          alpha = 0.2,
          fill = "#FF3366"
        ) +
        ggplot2::labs(
          title = paste("Regression of", y_label, "on", x_label),
          x = x_label,
          y = y_label
        ) +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })
    
    
    
    
    
    
    
    
    
    
    
    
###############################################################################
###      HDR ADDITION START -  Server Logic
###############################################################################
#==========================#    
#===                    ===#
#=== HDR dataset tables ===#
#===                    ===#
#==========================#
### ====== Update category menu when HDR table is selected ===== ###
observeEvent(input$selected_HDR_table, {
  updateSelectInput(
    session,
    "selected_category",
    choices = names(HDR_DATA[[input$selected_HDR_table]])
  )
})

# --- Dynamic title ---
output$dynamic_title <- renderText({
  req(input$selected_HDR_table)
  req(input$selected_category)

  category_title <- switch(
    input$selected_category,
    "groups"    = "Human Development Groups",
    "regions"   = "Regions",
    "special"   = "Special Countries/Territories",
    "countries" = "Countries Only"
  )
  paste(input$selected_HDR_table, "—", category_title)
})



### ===== Show or hide variable definitions ===== ###
output$variable_definitions <- renderUI({
  req(input$selected_HDR_table)
  
  # Only show when checkbox is TRUE
  if (!isTRUE(input$show_var_defs)) {
    return(NULL)
  }
  
  defs <- HDR_VARIABLE_DEFINITIONS[[input$selected_HDR_table]]
  if (is.null(defs)) return(NULL)
  
  # Build table rows
  table_rows <- ""
  for (var in names(defs)) {
    table_rows <- paste0(
      table_rows,
      "<tr>",
      "<th>", var, "</th>",
      "<td>", defs[[var]], "</td>",
      "</tr>"
    )
  }
  
  # Build HTML box
  HTML(paste0("
    <div class='collapsible-content'>
      <table class='def-table'>
        ", table_rows, "
      </table>
    </div>
  "))
})




### ===== Render selected HDR table dynamically ===== ###
output$raw_table <- DT::renderDataTable({
  req(input$selected_HDR_table)
  req(input$selected_category)

  df <- HDR_DATA[[input$selected_HDR_table]][[input$selected_category]]

  numeric_cols <- sapply(df, is.numeric)
  df[numeric_cols] <- round(df[numeric_cols], 3)

  DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
})


### ===== Allow download of selected HDR dataset ===== ###
output$download_raw <- downloadHandler(
  filename = function() {
    paste0(input$selected_HDR_table, "_", input$selected_category, ".csv")
  },
  content = function(file) {
    df <- HDR_DATA[[input$selected_HDR_table]][[input$selected_category]]
    write.csv(df, file, row.names = FALSE)
  }
)





#=================================#    
#===                           ===#
#===  HDR WORLD MAP (Overview) ===#
#===                           ===#
#=================================#
# === Reactive: filter HDR data by region ===
filtered_data_area <- reactive({
  
  HDRs_master_clean %>%
    left_join(HDR_AREA_LOOKUP, by = "iso3") %>%
    rename(country = country.x) %>%
    select(-country.y) %>%
    {
      if (input$filtered_area == "World") .
      else filter(., area == input$filtered_area)
    }
})

# === Render list of countries ===
output$area_country_list <- renderUI({
  req(input$show_country_list)
  
  countries <- filtered_data_area() %>%
    pull(country) %>% sort()
  
  HTML(paste0(
    "<b>Countries in ", input$filtered_area, ":</b><br>",
    paste(countries, collapse = "<br>")
  ))
})

# === Reactive: join HDR data to world polygons ===
map_area <- reactive({
  
  df_unique <- filtered_data_area() %>%
    group_by(iso3) %>%
    slice(1) %>%
    ungroup()
  
  world_shape %>%
    left_join(df_unique, by = c("iso_a3" = "iso3"))
})

# === Render Leaflet map ===
output$world_choropleth <- renderLeaflet({
  
  shp <- map_area()
  indicator <- input$indicator
  req(indicator)
  
  # Fix factors
  if (is.factor(shp[[indicator]]))
    shp[[indicator]] <- as.numeric(shp[[indicator]])
  
  valid_vals <- shp[[indicator]][!is.na(shp[[indicator]])]
  
  pal <- colorNumeric(
    palette = "viridis",
    domain = valid_vals,
    na.color = "lightgray"
  )
  
  leaflet(shp) %>%
    addTiles() %>%
    setView(lng = 0, lat = 20, zoom = 1.5) %>%
    addPolygons(
      fillColor = ~pal(get(indicator)),
      fillOpacity = 0.8,
      weight = 1, color = "white",
      label = ~paste0(country, ": ", round(get(indicator), 2))
    ) %>%
    addLegend(
      pal = pal, values = valid_vals,
      title = indicator
    )
})




#=================================#    
#===                           ===#
#===  COUNTRY PROFILE PLOT     ===#
#===                           ===#
#=================================#
# === Function for lolliplots ========
make_lollipop_plot <- function(country_name, indicators) {
  
  # === Extract the selected indicators for the chosen country ===
  df <- HDRs_master_clean %>%
    filter(country == country_name) %>%               # keep only chosen country
    select(all_of(indicators)) %>%                    # keep only chosen indicators
    
    # Convert factor → numeric BEFORE pivot (avoids pivot_longer errors)
    mutate(across(where(is.factor), ~ as.numeric(as.character(.))))
  
  # === Convert from wide format to long format (one row per indicator) ===
  df <- df %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "indicator",
      values_to = "value"        # keep NAs for plotting “No data”
    )
  
  # === Create helper columns for plotting ===
  df <- df %>%
    mutate(
      # TRUE if number exists, FALSE if NA
      has_data = !is.na(value),
      
      # Use 0 for missing values (only for placing the dot)
      plot_value = ifelse(has_data, value, 0),
      
      # Categorise indicator type for dot color
      value_type = case_when(
        !has_data     ~ "missing",
        value < 0     ~ "negative",
        TRUE          ~ "positive"
      )
    )
  
  # Global offset so labels never overlap dots
  offset <- 0.09 * max(abs(df$value), na.rm = TRUE)
  
  # === Build the lollipop plot ===
  p <- ggplot(df, aes(x = plot_value, y = reorder(indicator, value))) +
    
    # Lollipop stems (only for indicators that have numeric data)
    geom_segment(
      data = df %>% filter(has_data),
      aes(x = 0, xend = value, yend = indicator),
      color = "gray50",
      size = 1
    ) +

    geom_point(
      aes(
        color = value_type,
        text = paste("Indicator:", indicator,
                     "<br>Value:", value)
      ),
      size = 3       # your chosen size
    ) +
    
    # === Labels (value or "No data") - shifted slightly left/right ===
  geom_text(
    aes(
      label = ifelse(has_data, round(value, 3), "No data"),
      
      # Position the label so it NEVER overlaps the dot
      x = case_when(
        !has_data ~ offset,                    # Missing -> place label to the right of 0
        value >= 0 ~ value + offset,           # Positive -> shift right
        TRUE       ~ value - offset            # Negative -> shift left
      )
    ),
    hjust = case_when(
      !df$has_data ~ 0,                        # "No data" text left aligned
      df$value >= 0 ~ 0,                       # Positive -> text left of dot
      TRUE ~ 1                                 # Negative -> text right of dot
    ),
    size = 2.5    #change size of the label value on top of the lolly
  ) +
    
    # Color scale with 3 categories
    scale_color_manual(
      values = c(
        "positive" = "steelblue",
        "negative" = "firebrick",
        "missing"  = "grey60"
      )
    ) +
    
    labs(
      title = paste("Indicators for", country_name),
      x = "Value",
      y = ""
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 8)   # REDUCE LABEL SIZE HERE
    )
  
  # === Return interactive plot ===
  plotly::ggplotly(p, tooltip = c("indicator", "value", "text"))
}



# === SERVER OUTPUT for Country Profile ========
output$country_profile_plots <- plotly::renderPlotly({
  req(input$sel_country_profil)
  req(input$sel_indicators_profil)
  
  make_lollipop_plot(
    input$sel_country_profil,
    input$sel_indicators_profil
  )
})





# === FILTER DATA BY USER-SELECTED AREA ========
# ---------- PLOTS TABLE1 ----------------------------------------------
tab1_filtered_data <- reactive({
  req(input$hdr_tab1)

  df <- tab1_Iso3
  
  # Extract countries belonging to selected area of Table1 and pull their iso3 codes
  area_iso3 <- HDR_AREA_LOOKUP %>%
    filter(area == input$hdr_tab1) %>%
    pull(iso3)
  
  # Filter Table 1 to those countries only
  df_area <- df %>%
    filter(iso3 %in% area_iso3)
  
  return(df_area)
})












































# === FILTER DATA BY USER-SELECTED AREA ========
output$hdr_tab1_plots <- renderPlotly({
  req(input$hdr_tab1_indicators)     # user must choose at least 1 indicator
  
  df_area <- tab1_filtered_data()
  
  # DEBUG
  print(unique(df_area$country))
  print(length(unique(df_area$country)))
  ## END DEBUG
  
  # ---------------------------------------------------------
  # Create a single ordering column using the FIRST selected indicator
  # This ensures country labels do NOT overlap across facets.
  # ---------------------------------------------------------
  main_var <- input$hdr_tab1_indicators[1]     # first selected indicator
  df_area <- df_area %>%
    mutate(order_var = .data[[main_var]])  # add ordering column
  
  # 4. Keep selected indicators + country + ordering variable
  df_long <- df_area %>%
    select(country, order_var, all_of(input$hdr_tab1_indicators)) %>%
    tidyr::pivot_longer(
      cols = -c(country, order_var),
      names_to = "indicator",
      values_to = "value"
    )
  
  #compute a tiny offset to avoid Plotly clipping the text " No Data"
  offset_x <- max(df_long$value, na.rm = TRUE) * 0.2   # 1% of max value
  
  
  # 5. Lollipop plot (ggplot)
  p <- ggplot(df_long,
              aes(x = value,
                  y = reorder(country, order_var)
              )) +
    
    geom_segment(aes(x = 0, xend = value,
                     y = country, yend = country),
                 color = "grey70",
                 size = 0.4) +
    
    geom_point(aes(color = indicator),
               size = 2,
               alpha = 0.9) +
    
    facet_wrap(~ indicator, scales = "free_x") +
    
    labs(
      x = "Value",
      y = "",
      title = paste("Table 1 Indicators for", input$hdr_tab1_area)
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 9),      # smaller labels
      plot.margin = margin(10, 120, 10, 10) #space for labels: TOP, RIGHT, BOTTOM, LEFT
    ) +
    coord_cartesian(clip = "off") +     # allow long labels
    
    geom_text(
      data = df_long %>% dplyr::filter(is.na(value)),
      aes(
        x = offset_x,                 # place at x = 0 on the line
        label = "No data"
      ),
      hjust = -0.2,            # small nudge to the right of y-axis
      color = "grey30",
      size = 3
    )
  
  
  # 6. Convert to interactive Plotly
  plotly::ggplotly(
    p,
    tooltip = c("country", "indicator", "value")
  )
})












###############################################################################
###        HDR ADDITION END - Server Logic
###############################################################################
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  }) # end server logic
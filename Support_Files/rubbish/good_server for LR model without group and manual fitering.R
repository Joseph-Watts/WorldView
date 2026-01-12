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

##DEBUG####
options(shiny.error = recover)

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
  
  DT::datatable(
    data = get_C_data() |>
      
      # Round numeric variables EXCEPT rank variables
      dplyr::mutate(
        across(
          where(is.numeric) & !matches("rank"),
          ~ round(.x, 2)
        )
      ) |>
      
      # Rename identifiers for display
      dplyr::rename(
        Country     = B_COUNTRY,
        `Country ISO` = B_COUNTRY_ALPHA
      ),
    
    options = list(
      scrollX = TRUE
    )
  )
  
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

# # ============================================
# # Reactive datasets
# # ============================================
# # Reactive dataset restricted to countries
# # that have an official UNDP HDR region
# # (countries without hdr_region are excluded)
# data_with_region <- reactive({
# 
#   MASTER_HDR_WVS7_CLASSIFIED %>%
#     filter(!is.na(hdr_region))
# })    
#     
#     
#  
#  
# # ==========================================================
# # COUNTRY-LEVEL DATA (HDR + WVS MASTER DATASET)
# # ==========================================================
# 
# # NOTE:
# # `country_data` is defined in global.R as:
# # country_data <- MASTER_HDR_WVS7_CLASSIFIED
# 
#        
#     
#     
# # ==========================================================
# # 1. Populate the country dropdown menu
# # ==========================================================
# # This observes the data and updates the dropdown choices
# observe({
#   
#   updateSelectizeInput(
#     session,
#     inputId = "country_select",
#     
#     # Show country names, but use ISO3 codes internally
#     choices = c(
#       "All countries" = "ALL",
#       setNames(MASTER_COUNTRY_DATA$iso3, MASTER_COUNTRY_DATA$country)
#     ),
#     
#     # Default selection
#     selected = "ALL",
#     server = TRUE   # IMPORTANT for large lists
#   )
# })
#     
# 
# 
# # ==========================================================
# # Populate the country dropdown ONCE
# # ==========================================================
# observeEvent(TRUE, {
#   
#   updateSelectizeInput(
#     session,
#     inputId = "country_select",
#     choices = c(
#       "All countries" = "ALL",
#       setNames(MASTER_COUNTRY_DATA$iso3, MASTER_COUNTRY_DATA$country)
#     ),
#     selected = "ALL",
#     server = TRUE
#   )
#   
# }, once = TRUE)
#     
# 
# 
# # ==========================================================
# # 2. Reactive dataset filtered by selected countries
# # ==========================================================
# filtered_country_data <- reactive({
#   
#   sel <- input$country_select
#   
#   # Defensive: if input temporarily empty, show full dataset
#   if (is.null(sel) || length(sel) == 0 || "ALL" %in% sel) {
#     return(MASTER_COUNTRY_DATA)
#   }
#   
#   # Otherwise filter for selected countries
#   MASTER_COUNTRY_DATA %>%
#     dplyr::filter(iso3 %in% sel)
#   
# })
#     
#     
#     
# # ==========================================================
# # 3. Render the filtered country-level raw data table
# # ==========================================================
# output$Table_country <- renderDT({
#   
#   # Render the integrated HDR + WVS country-level dataset
#   datatable(
#     filtered_country_data(),   # reactive dependency
#     options = list(
#       pageLength = 10,  # number of rows per page
#       scrollX = TRUE    # enable horizontal scrolling (many columns)
#     ),
#     rownames = FALSE
#   )%>%
#     
#     # Format ALL numeric columns to 2 decimal places
#     formatRound(
#       columns = names(MASTER_COUNTRY_DATA)[sapply(MASTER_COUNTRY_DATA, is.numeric)],
#       digits = 2
#     )
#   
# })



# ============================================
# Reactive datasets
# ============================================
# Reactive dataset restricted to countries
# that have an official UNDP HDR region
# (countries without hdr_region are excluded)
data_with_region <- reactive({
  
  MASTER_HDR_WVS7_CLASSIFIED %>%
    filter(!is.na(hdr_region))
})    



# ==========================================================
# 1. Populate the country dropdown menu
# ==========================================================
# This observes the data and updates the dropdown choices
observe({
  
  updateSelectizeInput(
    session,
    inputId = "country_select",
    
    # Show country names, but use ISO3 codes internally
    choices = c(
      "All countries" = "ALL",
      setNames(MASTER_COUNTRY_DATA$iso3, MASTER_COUNTRY_DATA$country)
    ),
    
    # Default selection
    selected = "ALL",
    server = TRUE   # IMPORTANT for large lists
  )
})



# ==========================================================
# Populate the country dropdown ONCE
# ==========================================================
observeEvent(TRUE, {
  
  updateSelectizeInput(
    session,
    inputId = "country_select",
    choices = c(
      "All countries" = "ALL",
      setNames(MASTER_COUNTRY_DATA$iso3, MASTER_COUNTRY_DATA$country)
    ),
    selected = "ALL",
    server = TRUE
  )
  
}, once = TRUE)



# ==========================================================
# 2. Reactive dataset filtered by selected countries
# ==========================================================
filtered_country_data <- reactive({
  
  sel <- input$country_select
  
  # Defensive: if input temporarily empty, show full dataset
  if (is.null(sel) || length(sel) == 0 || "ALL" %in% sel) {
    return(MASTER_COUNTRY_DATA)
  }
  
  # Otherwise filter for selected countries
  MASTER_COUNTRY_DATA %>%
    dplyr::filter(iso3 %in% sel)
  
})








# ==========================================================
# 3. Render the filtered country-level raw data table
# ==========================================================
# Named vector: var_code -> readable label
country_col_labels <- setNames(
  FULL_COUNTRY_VAR_DICT$label,
  FULL_COUNTRY_VAR_DICT$var_code
)    


output$Table_country <- renderDT({
  
  df <- filtered_country_data()
  
  # Replace column names with readable labels where available
  colnames(df) <- ifelse(
    colnames(df) %in% names(country_col_labels),
    country_col_labels[colnames(df)],
    colnames(df)  # keep original if no label exists
  )
  
  DT::datatable(
    df,
    options = list(
      pageLength = 10,
      scrollX = TRUE
    ),
    rownames = FALSE
  ) %>%
    DT::formatRound(
      columns = setdiff(
        names(df)[sapply(df, is.numeric)],
        "HDI rank, 2023"   # EXCLUDED FROM ROUNDING
      ),
      digits = 2
    )
  
})




###### Variable definition section#################
# =============================================================================
# 1. Conditional refinement UI (HDR table or WVS section selector) for RAW DATA
# =============================================================================

output$source_refinement_ui <- renderUI({
  
  req(input$var_source)  # ensure source selection exists
  
  if (input$var_source == "HDR") {
    
    # -------------------------------
    # HDR table selector
    # -------------------------------
    hdr_tables <- sort(unique(
      FULL_VARIABLE_DICTIONARY$table[
        FULL_VARIABLE_DICTIONARY$source == "HDR"
      ]
    ))
    
    selectInput(
      inputId = "hdr_table_filter",
      label   = "Filter by HDR table (optional):",
      choices = c("All tables", hdr_tables),
      selected = "All tables"
    )
    
  } else if (input$var_source == "WVS7") {
    
    # -------------------------------
    # WVS7 section selector
    # -------------------------------
    wvs_sections <- sort(unique(
      FULL_VARIABLE_DICTIONARY$table[
        FULL_VARIABLE_DICTIONARY$source == "WVS7"
      ]
    ))
    
    # Remove metadata section
    wvs_sections <- setdiff(wvs_sections, "ID")
    
    selectInput(
      inputId = "wvs_section_filter",
      label   = "Filter by WVS section (optional):",
      choices = c("All sections", wvs_sections),
      selected = "All sections"
    )
    
  } else {
    
    NULL  # Source = ALL → no refinement selector
    
  }
})




# =============================================================================
# 2. Reactive dictionary filtering logic
# =============================================================================
dict_filtered <- reactive({
  
  req(FULL_VARIABLE_DICTIONARY)
  
  dict <- FULL_VARIABLE_DICTIONARY
  
  # Filter by source (optional)
  if (input$var_source != "ALL") {
    dict <- dict %>%
      dplyr::filter(source == input$var_source)
  }
  
  # Filter by HDR table (optional)
  if (input$var_source == "HDR" &&
      !is.null(input$hdr_table_filter) &&
      input$hdr_table_filter != "All tables") {
    
    dict <- dict %>%
      dplyr::filter(table == input$hdr_table_filter)
  }
  
  # Filter by WVS section (optional)
  if (input$var_source == "WVS7" &&
      !is.null(input$wvs_section_filter) &&
      input$wvs_section_filter != "All sections") {
    
    dict <- dict %>%
      dplyr::filter(table == input$wvs_section_filter)
  }
  
  dict
})



# =============================================================================
# 3. Update variable search dropdown based on filtered dictionary
# =============================================================================
observeEvent(dict_filtered(), {
  
  dict <- dict_filtered()
  req(nrow(dict) > 0) #guard against empty dictionary
  
  updateSelectizeInput(
    session,
    inputId = "var_lookup",
    choices = setNames(
      dict$var_code,
      dict$label
    ),
    server = TRUE
  )
  
})




# =============================================================================
# 4. Display selected variable definition
# =============================================================================
output$var_definition <- renderUI({
  
  req(input$var_lookup)
  
  var_info <- FULL_VARIABLE_DICTIONARY %>%
    dplyr::filter(var_code == input$var_lookup) %>%
    dplyr::slice(1)
  
  req(nrow(var_info) == 1)
  
  tagList(
    
    tags$p(tags$strong("Source:"), var_info$source),
    
    tags$p(
      tags$strong("Table / Section:"),
      var_info$table
    ),
    
    tags$div(
      style = "
    background-color: #f8f9fa;
    padding: 12px;
    border-radius: 6px;
    max-height: 260px;
    overflow-y: auto;
  ",
      tags$p(
        ifelse(
          is.na(var_info$definition) || var_info$definition == "",
          "Definition not available in the source documentation.",
          var_info$definition
        )
      )
    )
    
  )
  
})




#==========================#    
#===                    ===#
#=== Univariate stats   ===#
#===                    ===#
#==========================#   
#==========================#    
#===                    ===#
#=== Univariate stats   ===#
#===                    ===#
#==========================#
# ==========================================================
# NEW Univariate (Individual-level): Prepare data
# ==========================================================    
univariate_indiv_data <- reactive({
  
  req(input$uni_question_indiv, input$uni_countries_indiv)
  
  # Convert question label to column ID
  q_id <- get_question_id(input$uni_question_indiv)
  
  # Original responses (factor / labelled)
  orig_data <- get_I_data() %>%
    dplyr::filter(B_COUNTRY_ALPHA %in% input$uni_countries_indiv) %>%
    dplyr::select(
      country  = B_COUNTRY,
      response = all_of(q_id)
    ) %>%
    dplyr::mutate(country = as.character(country))
  
  # Numeric version (for means, SD, etc.)
  num_data <- indiv_ordinal %>%
    dplyr::filter(B_COUNTRY_ALPHA %in% input$uni_countries_indiv) %>%
    dplyr::select(
      country  = B_COUNTRY,
      response = all_of(q_id)
    ) %>%
    dplyr::mutate(
      country  = as.character(country),
      response = as.numeric(response)
    )
  
  list(
    orig      = orig_data,
    num       = num_data,
    is_factor = is.factor(orig_data$response),
    is_numeric = is.numeric(num_data$response),
    n_unique  = length(unique(stats::na.omit(orig_data$response)))
  )
})




# =============================================================================
# Populate NEW univariate individual-level inputs
# =============================================================================
observe({
  # -------------------------------
  # Question list
  # -------------------------------
  updateSelectInput(
    session,
    "uni_question_indiv",
    choices = get_questions_I()
  )
  
  # -------------------------------
  # Country list (ISO3 values, names as labels)
  # -------------------------------
  #use WVS7 countries only
  req(wvs7_country)
  
  country_choices <- wvs7_country %>%
    dplyr::distinct(
      iso3    = B_COUNTRY_ALPHA,
      country = B_COUNTRY
    ) %>%
    dplyr::arrange(country)
  
  
  
  
  updateSelectizeInput(
    session,
    "uni_countries_indiv",
    choices = setNames(
      country_choices$iso3,     # values sent to server
      country_choices$country   # labels shown to user
    ),
    server = TRUE
  )
})



observe({
  print(input$uni_countries_indiv)
})



# ==========================================================
# NEW Univariate (Individual-level): Render tabs
# ==========================================================
output$univariate_indiv_tabs <- renderUI({
  
  data <- univariate_indiv_data()
  req(data)
  
  # ----------------------------------
  # DEBUG: countries selected but with no data
  # ----------------------------------
  missing_countries <- setdiff(
    input$uni_countries_indiv,
    unique(data$orig$country)
  )
  
  
  
  country_names <- unique(data$orig$country)
  
  tab_list <- lapply(
    c("Pooled responses (selected countries)", country_names),
    function(ctry) {
      
      if (ctry == "Pooled responses (selected countries)") {
        orig_sub <- data$orig
        num_sub  <- data$num
      } else {
        orig_sub <- dplyr::filter(data$orig, country == ctry)
        num_sub  <- dplyr::filter(data$num,  country == ctry)
      }
      
      #content <- tagList(h4(ctry))
      content <- tagList(
        h4(ctry),
        
        # -----------------------
        # Warning area (TOP)
        # -----------------------
        if (ctry != "Pooled responses (selected countries)" && nrow(num_sub) == 0) {
          tags$em("No numeric data available for this country.")
        }
      )
      
      
      
      # -------------------------------
      # Frequency table
      # -------------------------------
      if (data$is_factor || (data$is_numeric && data$n_unique <= 10)) {
        
        freq_tbl <- orig_sub %>%
          dplyr::count(response) %>%
          dplyr::mutate(
            Percentage = round(n / sum(n) * 100, 1)
          ) %>%
          dplyr::rename(
            Response = response,
            Count    = n
          )
        
        content <- tagList(
          content,
          #h4("Frequency distribution"),
          h4(
            "Frequency distribution",
            style = "color: #2C7BE5;"   # blue 
          ),
          
          tags$table(
            class = "table table-striped table-bordered",
            tags$thead(
              tags$tr(lapply(names(freq_tbl), tags$th))
            ),
            tags$tbody(
              lapply(seq_len(nrow(freq_tbl)), function(i) {
                tags$tr(
                  lapply(freq_tbl[i, ], function(x) tags$td(x))
                )
              })
            )
          )
        )
      }
      
      
      #For numeric data  
      if (data$is_numeric) {
        
        n_non_missing <- sum(!is.na(num_sub$response))
        
        if (n_non_missing > 0) {
          
          summary_tbl <- num_sub %>%
            dplyr::summarise(
              N        = n_non_missing,
              Mean     = round(mean(response, na.rm = TRUE), 2),
              SD       = round(sd(response, na.rm = TRUE), 2),
              Median   = round(median(response, na.rm = TRUE), 2),
              Min      = min(response, na.rm = TRUE),
              Max      = max(response, na.rm = TRUE),
              Skewness = round(psych::skew(response, na.rm = TRUE), 3),
              Kurtosis = round(psych::kurtosi(response, na.rm = TRUE), 3)
            )
          
          content <- tagList(
            content,
            # h4("Numeric summary"),
            h4(
              "Numeric summary",
              style = "color: #2C7BE5;"   # blue 
            ),
            
            tags$table(
              class = "table table-striped table-bordered",
              tags$thead(
                tags$tr(lapply(names(summary_tbl), tags$th))
              ),
              tags$tbody(
                tags$tr(
                  lapply(summary_tbl[1, ], function(x) tags$td(x))
                )
              )
            )
          )
          
        } else {
          
          # Message for countries with missing data
          content <- tagList(
            content,
            tags$em("No numeric data available for this country.")
          )
        }
      }
      
      
      
      
      tabPanel(ctry, content)
    }
  )
  
  #do.call(tabsetPanel, c(id = "univariate_indiv_tabs_inner", tab_list))
  do.call(tabsetPanel, c(type = "pills", tab_list))
  
})



################################################################################
# ########### COUNTRY-LEVEL ####################################################   
################################################################################

# =====================================================
# COUNTRY-LEVEL: DATA ACCESS
# =====================================================
country_master_data <- reactiveVal(MASTER_COUNTRY_DATA)
country_var_dict    <- reactiveVal(FULL_VARIABLE_DICTIONARY)




# =====================================================
# optional: Clears country selection when upstream inputs change
# =====================================================
observeEvent(
  list(
    input$country_var_source_new,
    input$country_hdr_table,
    input$country_wvs_section
  ),
  {
    updateSelectizeInput(
      session,
      "uni_question_country",
      selected = character(0)
    )
    
    updateSelectizeInput(
      session,
      "uni_countries_country",
      selected = character(0)
    )
  },
  ignoreInit = TRUE
)




#=====================================================
# 
# =====================================================
output$country_source_refinement_ui_new <- renderUI({
  
  req(input$country_var_source_new)
  
  if (input$country_var_source_new == "HDR") {
    
    hdr_tables <- sort(unique(
      FULL_VARIABLE_DICTIONARY$table[
        FULL_VARIABLE_DICTIONARY$source == "HDR"
      ]
    ))
    
    selectInput(
      inputId = "country_hdr_table",
      label   = "Filter by HDR table (optional):",
      choices = c("All tables", hdr_tables),
      selected = "All tables"
    )
    
  } else if (input$country_var_source_new == "WVS7") {
    
    wvs_sections <- sort(unique(
      FULL_VARIABLE_DICTIONARY$table[
        FULL_VARIABLE_DICTIONARY$source == "WVS7"
      ]
    ))
    
    #REMOVE METADATA SECTION
    wvs_sections <- setdiff(wvs_sections, "ID")
    
    selectInput(
      inputId = "country_wvs_section",
      label   = "Filter by WVS section (optional):",
      choices = c("All sections", wvs_sections),
      selected = "All sections"
    )
    
  } else {
    
    NULL
    
  }
})



# =====================================================
# Variable source filter (HDR / WVS / ALL)
# =====================================================
filtered_country_dictionary <- reactive({
  req(country_var_dict(), input$country_var_source_new)
  
  dict <- country_var_dict()
  
  if (input$country_var_source_new == "ALL") {
    dict
  } else {
    dplyr::filter(dict, source == input$country_var_source_new)
  }
})





# =====================================================
# Refine dictionary based on source-specific selection
# =====================================================
filtered_country_dictionary_refined <- reactive({
  
  req(input$country_var_source_new)
  
  # Use the COUNTRY dictionary
  dict <- FULL_COUNTRY_VAR_DICT
  
  # --------------------------------------------------
  # SOURCE FILTER
  # --------------------------------------------------
  if (input$country_var_source_new != "ALL") {
    dict <- dict %>%
      dplyr::filter(source == input$country_var_source_new)
  }
  
  # --------------------------------------------------
  # HDR TABLE FILTER
  # --------------------------------------------------
  if (
    input$country_var_source_new == "HDR" &&
    !is.null(input$country_hdr_table) &&
    input$country_hdr_table != "All tables"
  ) {
    dict <- dict %>%
      dplyr::filter(section == input$country_hdr_table)
  }
  
  # --------------------------------------------------
  # WVS SECTION FILTER
  # --------------------------------------------------
  if (
    input$country_var_source_new == "WVS7" &&
    !is.null(input$country_wvs_section) &&
    input$country_wvs_section != "All sections"
  ) {
    dict <- dict %>%
      dplyr::filter(section == input$country_wvs_section)
  }
  
  dict
})



# =====================================================
# Selected country-level variable -> data
# =====================================================
univariate_country_data <- eventReactive(
  list(
    input$uni_question_country,
    input$uni_countries_country
  ),
  {
    req(input$uni_question_country)
    req(input$uni_question_country %in% names(country_master_data()))
    req(length(input$uni_countries_country) > 0)
    
    country_master_data() %>%
      dplyr::filter(country %in% input$uni_countries_country) %>%
      dplyr::select(
        country,
        response = all_of(input$uni_question_country)
      )
  },
  ignoreInit = TRUE
)




# ================================================================================
# set vector of IDs to be excluded from country-level VARIABLE selector just below
# ==============================================================================
ID_VARS_GLOBAL <- c(
  "country",
  "iso3",
  "hdi_rank"
)

ID_VARS_WVS <- c(
  "B_COUNTRY",
  "B_COUNTRY_ALPHA",
  "S007"
)


# ==============================================================
# Populate country-level VARIABLE selector (readable labels)
# ==============================================================
observeEvent(filtered_country_dictionary_refined(), {
  
  dict <- filtered_country_dictionary_refined()
  
  # Exclude ID variables
  vars_to_drop <- if (input$country_var_source_new == "WVS7") {
    c(ID_VARS_GLOBAL, ID_VARS_WVS)
  } else {
    ID_VARS_GLOBAL
  }
  
  dict <- dict %>%
    dplyr::filter(!var_code %in% vars_to_drop)
  
  # Named vector: label (shown) -> var_code (used internally)
  choices <- setNames(
    dict$var_code,
    dict$label
  )
  
  updateSelectizeInput(
    session,
    inputId = "uni_question_country",
    choices = choices,
    server  = TRUE
  )
})




# =========================================================
# Populate country selector
# =========================================================
observe({
  req(country_master_data())
  
  updateSelectizeInput(
    session,
    "uni_countries_country",
    choices = sort(unique(country_master_data()$country)),
    server  = TRUE
  )
})




# ==============================================================
# OUTPUT (render only)  uiOutput("univariate_country_summary")
# ==============================================================
output$univariate_country_tabs <- renderUI({
  
  data <- univariate_country_data()
  if (is.null(data)) return(NULL)
  
  countries <- input$uni_countries_country
  if (length(countries) == 0) return(NULL)
  
  
  
  # ==============================
  # Helper: summary table renderer
  # ==============================
  render_summary_table <- function(df) {
    
    summary_tbl <- df %>%
      dplyr::summarise(
        N      = n(),
        Mean   = round(mean(response), 3),
        SD     = round(sd(response), 3),
        Median = round(median(response), 3),
        Min    = round(min(response), 3),
        Max    = round(max(response), 3),
        Skewness = ifelse(
          n() >= 3,
          round(psych::skew(response), 3),
          NA
        ),
        Kurtosis = ifelse(
          n() >= 3,
          round(psych::kurtosi(response), 3),
          NA
        )
      )
    
    tags$table(
      class = "table table-striped table-bordered",
      tags$thead(
        tags$tr(lapply(names(summary_tbl), tags$th))
      ),
      tags$tbody(
        tags$tr(lapply(summary_tbl[1, ], tags$td))
      )
    )
  }
  
  # ==============================
  # Pooled tab (ALWAYS shown)
  # ==============================
  pooled_tab <- tabPanel(
    "Pooled",
    {
      pooled_data <- data %>% filter(!is.na(response))
      
      if (nrow(pooled_data) == 0) {
        tags$p(
          style = "color: #777; font-style: italic;",
          "No data available for the current selection."
        )
      } else {
        render_summary_table(pooled_data)
      }
    }
  )
  
  # ==============================
  # Country tabs (ALWAYS created)
  # ==============================
  country_tabs <- lapply(countries, function(cty) {
    
    tabPanel(
      cty,
      {
        country_data <- data %>%
          filter(country == cty, !is.na(response))
        
        if (nrow(country_data) == 0) {
          tags$p(
            style = "color: #777; font-style: italic;",
            paste("No data available for", cty)
          )
        } else {
          render_summary_table(country_data)
        }
      }
    )
  })
  
  do.call(
    tabsetPanel,
    c(list(type = "tabs"), list(pooled_tab), country_tabs)
  )
})





################################################################################
################################################################################
############## GROUP_LEVEL #####################################################
# =========================================================
# Group selector UI: HDI, Regions, Special groups 
# =========================================================
output$group_selector_ui <- renderUI({
  
  req(input$group_type)        # wait until a group type is chosen
  
  if (input$group_type == "all") {
    return(NULL)               # no selector needed for "all groups"
  }
  
  # Select the appropriate group names based on group type
  # choices <- switch(
  #   input$group_type,
  #   groups  = HDR_GROUP_LOOKUP$groups,    # HDI groups
  #   regions = HDR_GROUP_LOOKUP$regions,   # Regions
  #   special = HDR_GROUP_LOOKUP$special    # OECD, SIDS, etc.
  # )
  
  if (input$group_type == "groups") {
    choices <- HDR_GROUP_LOOKUP$groups             # HDI groups
    
  } else if (input$group_type == "regions") {    
    choices <- HDR_GROUP_LOOKUP$regions            # Regions
    
  } else if (input$group_type == "special") {
    choices <- HDR_GROUP_LOOKUP$special            # OECD, SIDS, etc.
    
  }
  
  # Render multi-select dropdown of group names
  selectizeInput(
    inputId  = "group_names",
    label    = "Select group(s)",
    choices  = choices,
    multiple = TRUE
  )
})



# =========================================================
# GROUP NAME SELECTOR UI (creates input$group_names)
# =========================================================
observeEvent(input$group_type, {
  
  # Do nothing for "all groups"
  if (input$group_type == "all") {
    return(NULL)
  }
  
  # Choose defaults depending on group type
  if (input$group_type == "groups") {
    default_choices <- HDR_GROUP_LOOKUP$groups
  } else if (input$group_type == "regions") {
    default_choices <- HDR_GROUP_LOOKUP$regions
  } else if (input$group_type == "special") {
    default_choices <- HDR_GROUP_LOOKUP$special
  }
  
  # Select the first group by default
  updateSelectizeInput(
    session,
    inputId  = "group_names",
    selected = default_choices[1]
  )
  
}, ignoreInit = TRUE)






# =========================================================
# Group table selector UI
# =========================================================
output$group_table_ui <- renderUI({
  
  selectInput(
    inputId  = "group_table",
    label    = "Select HDR table",
    choices  = names(HDR_DATA),
    selected = "Table 1 - HDI & Components"
  )
})





# =========================================================
# Group-level: Indicator selector UI
# =========================================================
output$group_indicator_ui <- renderUI({
  
  req(input$group_type)
  
  # Do not show indicator selector for "All groups"
  if (input$group_type == "all") {
    return(NULL)
  }
  
  # Require HDR table selection
  req(input$group_table)
  
  selectizeInput(
    inputId  = "group_indicator",
    label    = "Select Indicator",
    choices  = NULL,   # populated by observer
    multiple = FALSE
  )
})





# # --------------------------------------------------
# # Group-level summary table
# # --------------------------------------------------
# output$group_level_summary <- DT::renderDT({
#   
#   req(input$group_type, input$group_table)
#   
#   # ---- Get group data ----
#   if (input$group_type == "all") {
#     df <- dplyr::bind_rows(
#       HDR_DATA[[input$group_table]]$groups,
#       HDR_DATA[[input$group_table]]$regions,
#       HDR_DATA[[input$group_table]]$special
#     )
#   } else {
#     df <- HDR_DATA[[input$group_table]][[input$group_type]] %>%
#       dplyr::filter(country %in% input$group_names)
#   }
#   
#   req(nrow(df) > 0)
#   
#   # ---- Keep numeric indicators only ----
#   df_out <- df %>%
#     dplyr::select(
#       group = country,
#       where(is.numeric),
#       -contains("rank")
#     )
#   
#   
#   df_out <- df_out %>%
#     dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))
#   
# 
#   req(ncol(df_out) > 1)
#   
#   DT::datatable(
#     df_out,
#     rownames = FALSE,
#     options = list(
#       dom = "t",      # table only (no search, no length menu)
#       scrollX = TRUE
#     )
#   )
# })







# =========================================================
# Group-level summary table
# =========================================================
output$group_level_summary <- DT::renderDT({
  
  req(input$group_type, input$group_table)
  
  # ---- Get group data ----
  if (input$group_type == "all") {
    df <- dplyr::bind_rows(
      HDR_DATA[[input$group_table]]$groups,
      HDR_DATA[[input$group_table]]$regions,
      HDR_DATA[[input$group_table]]$special
    )
  } else {
    df <- HDR_DATA[[input$group_table]][[input$group_type]] %>%
      dplyr::filter(country %in% input$group_names)
  }
  
  req(nrow(df) > 0)
  
  # ---- Keep numeric indicators only ----
  df_out <- df %>%
    dplyr::select(
      group = country,
      where(is.numeric),
      -contains("rank")
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))
  
  req(ncol(df_out) > 1)
  
  # Replace variable codes with readable labels
  current_names <- names(df_out)
  
  new_names <- ifelse(
    current_names %in% names(HDR_LABELS),
    HDR_LABELS[current_names],
    current_names
  )
  
  names(df_out) <- new_names
  
  DT::datatable(
    df_out,
    rownames = FALSE,
    # options = list(
    #   dom = "t",
    #   scrollX = TRUE
    # )
    options = list(
      dom       = "t",
      paging    = FALSE,   # <- IMPORTANT
      ordering  = TRUE,
      scrollX   = TRUE
    )
    
    
  )
})

















#==========================#   
#===                    ===#
#=== Bivariate stats    ===#
#===                    ===#
#==========================#      
#===                    ===#
#=== Bivariate stats    ===#
#===                    ===#
#==========================#  
################################################################################
# ########### INDIVIDUAL-LEVEL #################################################  
################################################################################
# --------------------------------------------------
# Bivariate data (reactive)
# --------------------------------------------------
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




# --------------------------------------------------
# Bivariate table (renderDT)
# --------------------------------------------------
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


################################################################################
############# COUNTRY-LEVEL ####################################################  
# ################################################################################
# ==========================================================
# Define ALL valid data (must be numeric)
# ==========================================================
all_numeric_country_vars <- names(MASTER_COUNTRY_DATA)[
  sapply(MASTER_COUNTRY_DATA, is.numeric)
]


# ===========================================================
# Return valid numeric country-level variables by data source
# ===========================================================

get_country_vars_by_source <- function(source) {
  
  # Case 1: User selected ALL sources
  # Simply return all numeric country-level variables
  # without filtering by dictionary
  if (source == "ALL") {
    return(all_numeric_country_vars)
  }
  
  # Case 2: Filter variables using the variable dictionary
  # Extract variable codes whose source matches
  # the selected source (e.g. "HDR", "WVS7")
  vars_from_dict <- FULL_VARIABLE_DICTIONARY$var_code[
    FULL_VARIABLE_DICTIONARY$source == source
  ]
  
  # Final safeguard
  # Keep only variables that:
  # - exist in MASTER_COUNTRY_DATA
  # - are numeric country-level variables
  intersect(vars_from_dict, all_numeric_country_vars)
}




# --------------------------------------------------
# Build readable dropdown choices for country vars
# label (shown) -> var_code (used internally)
# --------------------------------------------------
get_country_var_choices <- function(source) {
  
  vars <- get_country_vars_by_source(source)
  
  FULL_COUNTRY_VAR_DICT %>%
    dplyr::filter(var_code %in% vars) %>%
    dplyr::arrange(label) %>%
    { stats::setNames(.$var_code, .$label) }
}



# ==============================================================================
# OBSERVER TO fill the dropdown FOR VARIABLE1 and VARIABLE2 ONCE when the app starts
# ==============================================================================
### Update dropdown for 1st bivariate variable
observe({
  updateSelectizeInput(
    session,
    "bivar_country_var1",
    
    # Recompute available variables based on the selected source
    # (e.g., ALL, HDR, WVS7), ensuring they exist and are numeric
    #choices = get_country_vars_by_source(input$bivar_country_source1),
    choices  = get_country_var_choices(input$bivar_country_source1),
    
    # Clear any previously selected variable to avoid
    # invalid source–variable combinations
    selected = NULL
  )
})


# ==========================================================
# Update dropdown for 2nd bivariate variable
# ==========================================================
observe({
  updateSelectizeInput(
    session,
    "bivar_country_var2",
    
    # Recompute available variables based on the selected source
    # (e.g., ALL, HDR, WVS7), ensuring they exist and are numeric
    #choices = get_country_vars_by_source(input$bivar_country_source2),
    choices  = get_country_var_choices(input$bivar_country_source1),
    
    # Clear any previously selected variable to avoid
    # invalid source–variable combinations
    selected = NULL
  )
})



# ==========================================================
# Populate dropdowns VARIABLE1 and 2 reactively
# ==========================================================
### Populate dropdowns VARIABLE1 reactively
observeEvent(input$bivar_country_source1, {
  
  # Update the variable dropdown for the first bivariate variable
  updateSelectizeInput(
    session,
    "bivar_country_var1",
    
    # Recompute available variables based on selected source
    # (e.g. ALL, HDR, WVS7)
    choices = get_country_vars_by_source(input$bivar_country_source1),
    
    # Clear any previously selected variable to avoid
    # invalid source–variable combinations
    selected = NULL
  )
  
  # Do not trigger this observer when the app first loads
}, ignoreInit = TRUE)




### Populate dropdown VARIABLE2 reactively
observeEvent(input$bivar_country_source2, {
  
  # Update the variable dropdown for the first bivariate variable
  updateSelectizeInput(
    session,
    "bivar_country_var2",
    
    # Recompute available variables based on selected source
    # (e.g. ALL, HDR, WVS7)
    choices = get_country_vars_by_source(input$bivar_country_source2),
    
    # Clear any previously selected variable to avoid
    # invalid source–variable combinations
    selected = NULL
  )
  
  # Do not trigger this observer when the app first loads
}, ignoreInit = TRUE)




# ==========================================================
# FUNCTION that Return countries that have data for at least
# one of two selected country-level variables
# ==========================================================
get_countries_for_vars <- function(var1, var2) {
  
  # Guard clause: if either variable is not selected yet,
  # return an empty character vector
  if (is.null(var1) || is.null(var2)) {
    return(character(0))
  }
  
  MASTER_COUNTRY_DATA %>%
    
    # Keep only country name and the two selected variables
    dplyr::select(country, all_of(c(var1, var2))) %>%
    
    # Keep countries with at least one non-missing value
    # for either variable
    dplyr::filter(
      !is.na(.data[[var1]]) | !is.na(.data[[var2]])
    ) %>%
    
    # Ensure one row per country
    dplyr::distinct(country) %>%
    
    # Sort countries alphabetically for a clean UI
    dplyr::arrange(country) %>%
    
    # Return just the country names as a character vector
    dplyr::pull(country)
}




# ==========================================================
# OBSERVER to Update the list of selectable countries for the
# bivariate country-level analysis
# ==========================================================
observe({
  # Guard condition:
  # Do nothing until both bivariate variables are selected
  req(input$bivar_country_var1, input$bivar_country_var2)
  
  
  # Update the country selector based on selected variables
  updateSelectizeInput(
    session,
    "bivar_country_countries",
    
    # Keep only countries that have data for
    # at least one of the two selected variables
    choices = get_countries_for_vars(
      input$bivar_country_var1,
      input$bivar_country_var2
    ),
    
    # Clear any previously selected countries to avoid
    # invalid variable–country combinations
    selected = NULL
  )
})




# =============================================================================
# ObserverEvent to Update the country selector for the bivariate analysis
# whenever either selected variable changes
# ==============================================================================
observeEvent(
  
  # Trigger this observer when either variable 1 or
  # variable 2 changes
  {
    list(
      input$bivar_country_var1,
      input$bivar_country_var2
    )
  },
  
  # Recompute and update the list of valid countries
  {
    updateSelectizeInput(
      session,
      "bivar_country_countries",
      
      # Keep only countries that have data for at least
      # one of the selected variables
      choices = get_countries_for_vars(
        input$bivar_country_var1,
        input$bivar_country_var2
      ),
      
      # Clear any previously selected countries to avoid
      # invalid variable–country combinations
      selected = NULL
    )
  },
  
  # Do not run this observer when the app first loads
  ignoreInit = TRUE
)





# ==========================================================
# Update the country selector for the bivariate analysis
# whenever either selected variable changes
# ==========================================================
observeEvent(
  
  # Trigger the observer when either bivariate variable changes
  list(input$bivar_country_var1, input$bivar_country_var2),
  
  {
    
    # Determine which countries have usable data for the selected variables
    available_countries <- get_countries_for_vars(
      input$bivar_country_var1,
      input$bivar_country_var2
    )
    
    
    # Define preferred default countries
    default_countries <- c("Australia", "New Zealand", "United Kingdom")
    
    # Keep only defaults that are actually available
    preselected <- intersect(default_countries, available_countries)
    
    # Fallback: if none of the preferred countries are available,
    #select the first few valid countries
    if (length(preselected) == 0 && length(available_countries) > 0) {
      preselected <- head(available_countries, 3)
    }
    
    
    # Update the country selector dropdown
    updateSelectizeInput(
      session,
      "bivar_country_countries",
      
      # Valid country choices based on data availability
      choices  = available_countries,
      
      # Sensible default selection
      selected = preselected
    )
  },
  
  # ------------------------------------------------------
  # Do not run this observer when the app first loads
  # ------------------------------------------------------
  ignoreInit = TRUE
)




# ==========================================================
# Reactive dataset for bivariate country-level analysis
# ==========================================================
#This reactive progressively narrows the master dataset down to a minimal,
#clean table containing only the selected countries and the two selected variables
# — ready for bivariate analysis.
bivar_country_data <- reactive({
  # Do nothing until both variables and at least one country have been selected
  req(
    input$bivar_country_var1,
    input$bivar_country_var2,
    input$bivar_country_countries
  )
  
  # Collect the selected variables and remove duplicates
  # (in case the same variable is selected twice)
  vars <- unique(c(
    input$bivar_country_var1,
    input$bivar_country_var2
  ))
  
  # Build the bivariate dataset
  MASTER_COUNTRY_DATA %>%
    
    # Keep only the selected countries
    dplyr::filter(country %in% input$bivar_country_countries) %>%
    
    # Keep only country name and selected variables
    dplyr::select(country, dplyr::all_of(vars))
})



# ==========================================================
# Render bivariate country-level data table
# ==========================================================
output$bivar_country_table <- DT::renderDT({
  
  # Retrieve the bivariate country-level dataset
  data <- bivar_country_data()
  req(nrow(data) > 0)
  
  # --------------------------------------------------
  # Replace variable codes with readable labels
  # (presentation only – does NOT affect calculations)
  # --------------------------------------------------
  var_labels <- FULL_COUNTRY_VAR_DICT %>%
    dplyr::filter(var_code %in% names(data)) %>%
    dplyr::select(var_code, label)
  
  colnames(data) <- sapply(
    colnames(data),
    function(x) {
      lbl <- var_labels$label[var_labels$var_code == x]
      if (length(lbl) == 1) lbl else x
    }
  )
  
  # Identify numeric columns dynamically
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  
  # Build interactive DataTable
  dt <- DT::datatable(
    data,
    rownames = FALSE,
    extensions = c("Buttons"),
    options = list(
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel"),
      pageLength = 10,
      scrollX = TRUE
    )
  )
  
  # Apply numeric formatting (presentation only)
  if (length(numeric_cols) > 0) {
    dt <- DT::formatRound(
      dt,
      columns = numeric_cols,
      digits  = 2
    )
  }
  
  dt
})







################################################################################
############# GROUP-LEVEL   ####################################################  
################################################################################
# # ==========================================================
# # Populate group selector (multiple groups) with defaults
# # ==========================================================
# observe({
#   # Ensure the HDR area lookup table is available
#   req(HDR_AREA_LOOKUP)
#   
#   # Extract and sort unique group names
#   groups <- sort(
#     unique(HDR_AREA_LOOKUP$area)
#   )
#   
#   # Update the group selection dropdown
#   updateSelectizeInput(
#     session,
#     inputId  = "bivar_groups",
#     choices  = groups,        # all available groups
#     selected = groups[1:2],   # default: first two groups
#     server   = TRUE           # server-side selectize for performance
#   )
#   
# })
# 
# 
# 
# 
# 
# # ==========================================================
# # Populate variable selectors for group-level bivariate
# # ==========================================================
# # ==========================================================
# # Populate variable selectors for group-level bivariate
# # ==========================================================
# observeEvent(MASTER_COUNTRY_DATA, {
#   
#   hdr_vars <- MASTER_COUNTRY_DATA %>%
#     dplyr::select(where(is.numeric)) %>%
#     names()
#   
#   updateSelectizeInput(
#     session,
#     "bivar_group_var1",
#     choices  = hdr_vars,
#     selected = hdr_vars[1],
#     server   = TRUE
#   )
#   
#   updateSelectizeInput(
#     session,
#     "bivar_group_var2",
#     choices  = hdr_vars,
#     selected = hdr_vars[2],
#     server   = TRUE
#   )
#   
# }, once = TRUE)
# 
# 
# 
# 
# # =================================================================
# # Observer to prevent selecting the same variable twice (Optional)
# # =================================================================
# # ==========================================================
# # Validation: prevent same variable twice
# # ==========================================================
# observeEvent(
#   c(input$bivar_group_var1, input$bivar_group_var2),
#   {
#     
#     if (!is.null(input$bivar_group_var1) &&
#         !is.null(input$bivar_group_var2) &&
#         input$bivar_group_var1 == input$bivar_group_var2) {
#       
#       output$bivar_group_warning <- renderUI({
#         tags$p(
#           style = "color: #d9534f; font-style: italic;",
#           "Please select two different variables for a bivariate comparison."
#         )
#       })
#       
#     } else {
#       output$bivar_group_warning <- renderUI(NULL)
#     }
#     
#   }
# )
# 
# 
# 
# 
# # ==========================================================
# # Build group-level bivariate dataset
# # ==========================================================
# bivar_group_data <- reactive({
#   
#   req(
#     input$bivar_group_var1,
#     input$bivar_group_var2,
#     input$bivar_groups
#   )
#   
#   validate(
#     need(
#       input$bivar_group_var1 != input$bivar_group_var2,
#       "Please select two different variables."
#     ),
#     need(
#       length(input$bivar_groups) >= 2,
#       "Please select at least two groups to compare."
#     )
#   )
#   
#   MASTER_COUNTRY_DATA %>%
#     dplyr::filter(country %in% input$bivar_groups) %>%
#     dplyr::select(
#       group = country,
#       input$bivar_group_var1,
#       input$bivar_group_var2
#     )
# })
# 
# 
# 
# 
# 
# # ==========================================================
# # Render group-level bivariate summary table
# # ==========================================================
# output$bivar_group_summary <- renderUI({
# 
#   df <- bivar_group_data()
#   req(nrow(df) >= 2)
# 
#   tags$table(
#     class = "table table-striped table-bordered",
#     tags$thead(
#       tags$tr(
#         lapply(names(df), tags$th)
#       )
#     ),
#     tags$tbody(
#       lapply(seq_len(nrow(df)), function(i) {
#         tags$tr(
#           lapply(df[i, ], function(x) {
#             tags$td(ifelse(is.na(x), "NA", round(x, 3)))
#           })
#         )
#       })
#     )
#   )
# })





# ===============================
# GROUP TYPE selector
# ===============================
observe({
  
  req(HDR_GROUP_BENCHMARKS)
  
  updateSelectInput(
    session,
    "bivar_group_type",
    choices = unique(HDR_GROUP_BENCHMARKS$group_type)
  )
  
})



#===============================
# 2. Populate GROUP selector
#===============================
# Populate groups based on selected group type
observeEvent(input$bivar_group_type, {
  
  req(input$bivar_group_type)
  
  groups <- HDR_GROUP_BENCHMARKS %>%
    dplyr::filter(group_type == input$bivar_group_type) %>%
    dplyr::distinct(group) %>%
    dplyr::pull(group) %>%
    sort()
  
  updateSelectizeInput(
    session,
    inputId  = "bivar_groups",
    choices  = groups,
    selected = head(groups, 2),  # safe default
    server   = TRUE
  )
  
})






#===================================================
# 3. Populate VARIABLE selectors (readable labels)
#===================================================
observe({
  
  req(HDR_GROUP_BENCHMARKS, HDR_LABELS)
  
  vars <- HDR_GROUP_BENCHMARKS %>%
    dplyr::distinct(variable) %>%
    dplyr::pull(variable) %>%
    sort()
  
  # Build named vector: label -> var_code
  var_choices <- setNames(
    vars,
    HDR_LABELS[vars]
  )
  
  updateSelectizeInput(
    session,
    "bivar_group_var1",
    choices  = var_choices,
    selected = vars[1],
    server   = TRUE
  )
  
  updateSelectizeInput(
    session,
    "bivar_group_var2",
    choices  = var_choices,
    selected = vars[2],
    server   = TRUE
  )
})






#=================================
# 4. Build BIVARIATE DATA (LONG)
#=================================
bivar_group_data <- reactive({
  
  req(
    input$bivar_group_type,
    input$bivar_groups,
    input$bivar_group_var1,
    input$bivar_group_var2
  )
  
  validate(
    need(length(input$bivar_groups) >= 2,
         "Please select at least two groups."),
    need(input$bivar_group_var1 != input$bivar_group_var2,
         "Please select two different variables.")
  )
  
  HDR_GROUP_BENCHMARKS %>%
    dplyr::filter(
      group_type == input$bivar_group_type,
      group %in% input$bivar_groups,
      variable %in% c(
        input$bivar_group_var1,
        input$bivar_group_var2
      )
    )
})




#========================================
# 5. Group-level bivariate summary table
#========================================
output$bivar_group_summary <- DT::renderDT({
  
  # Get long-format bivariate data
  df <- bivar_group_data()
  req(nrow(df) > 0)
  
  # -----------------------------------
  # Reshape to wide format for display
  # -----------------------------------
  df_wide <- df %>%
    tidyr::pivot_wider(
      id_cols     = group,
      names_from  = variable,
      values_from = value
    ) %>%
    dplyr::select(
      group,
      all_of(input$bivar_group_var1),
      all_of(input$bivar_group_var2)
    ) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ round(.x, 2))
    )
  
  # -----------------------------------
  # Replace variable codes with labels
  # -----------------------------------
  current_names <- names(df_wide)
  
  new_names <- ifelse(
    current_names %in% names(HDR_LABELS),
    HDR_LABELS[current_names],
    current_names
  )
  
  names(df_wide) <- new_names
  
  # -----------------------------------
  # Render table
  # -----------------------------------
  DT::datatable(
    df_wide,
    rownames   = FALSE,
    extensions = "Scroller",
    options = list(
      dom        = "t",                  # table only
      scrollX    = TRUE,                 # horizontal scroll
      paging     = FALSE,                # show all groups
      ordering   = TRUE,
      columnDefs = list(
        list(className = "dt-nowrap", targets = "_all")
      )
    )
  )
})











#==========================#   
#===                    ===#
#=== VISUALISATIONS     ===#
#===                    ===#
#==========================#      
#===                    ===#
#=== SCATTERPLOT        ===#
#===                    ===#
#==========================#  
################################################################################
# ########### COUNTRY-LEVEL ####################################################  
################################################################################

# ==========================================================
#1a. OBSERVER to Populate X-axis variable choices
# ==========================================================
observe({
  
  # Get valid variable CODES
  valid_vars <- get_country_vars_by_source(
    input$scatter_country_source_x
  )
  
  # Map codes to readable labels using the dictionary
  dict <- FULL_COUNTRY_VAR_DICT %>%
    dplyr::filter(var_code %in% valid_vars)
  
  # Build named vector: label (shown) -> var_code (used)
  choices <- setNames(dict$var_code, dict$label)
  
  # Update dropdown
  updateSelectizeInput(
    session,
    inputId = "scatter_country_x",
    choices  = choices,
    selected = isolate(input$scatter_country_x),
    server   = TRUE
  )
})





# ==========================================================
# 1b. OBSERVER to Populate Y-axis variable choices
# =========================================================
observe({
  
  # Get valid variable CODES 
  valid_vars <- get_country_vars_by_source(
    input$scatter_country_source_y
  )
  
  # Map codes to readable labels using the dictionary
  dict <- FULL_COUNTRY_VAR_DICT %>%
    dplyr::filter(var_code %in% valid_vars)
  
  # Build named vector: label (shown) -> var_code (used)
  choices <- setNames(dict$var_code, dict$label)
  
  # Update dropdown
  updateSelectizeInput(
    session,
    inputId = "scatter_country_y",
    choices  = choices,
    selected = isolate(input$scatter_country_y),
    server   = TRUE
  )
})




# ==========================================================
# 2. OBSERVER to Populate country selector. 
# It populates the server with all countries per default
# ==========================================================
observe({
  # Get unique list of countries from country-level data
  countries <- MASTER_COUNTRY_DATA %>%
    dplyr::distinct(country) %>%
    dplyr::arrange(country) %>%
    dplyr::pull(country)
  
  # Add ALL option at the top
  country_choices <- c("ALL", countries)
  
  # Update country selection dropdown
  updateSelectizeInput(
    session,
    inputId = "scatter_country_countries",
    choices = country_choices,
    selected = "ALL",
    server  = TRUE
  )
})




# =============================================================================
# OBSERVER to Enforce mutual exclusivity between ALL and specific countries
# =============================================================================
observe({
  
  selected <- input$scatter_country_countries
  
  # Nothing to do if no selection
  if (length(selected) == 0) {
    return()
  }
  
  # If ALL is selected together with other countries,
  # remove ALL and keep specific countries
  if ("ALL" %in% selected && length(selected) > 1) {
    
    updateSelectizeInput(
      session,
      inputId  = "scatter_country_countries",
      selected = setdiff(selected, "ALL")
    )
  }
})




# ==========================================================
# 3. REACTIVE to build dataset
# ==========================================================
scatter_country_data <- reactive({
  
  # Ensure X and Y variables are selected
  req(
    input$scatter_country_x,
    input$scatter_country_y
  )
  
  # Start from country-level dataset
  df <- MASTER_COUNTRY_DATA %>%
    dplyr::select(
      country = country,
      x = all_of(input$scatter_country_x),
      y = all_of(input$scatter_country_y)
    )
  
  # Optional country filtering
  selected_countries <- input$scatter_country_countries
  
  # Filter only if user selected specific countries (not ALL)
  if (
    length(selected_countries) > 0 &&
    !("ALL" %in% selected_countries)
  ) {
    df <- df %>%
      dplyr::filter(country %in% selected_countries)
  }
  
  # Remove rows with missing X or Y values
  df <- df %>%
    dplyr::filter(
      !is.na(x),
      !is.na(y)
    )
  
  df
})



# ==========================================================
# 4.Detect excluded countries due to missing data
# ==========================================================
scatter_country_missing <- reactive({
  
  # Only relevant if the user selected countries
  if (length(input$scatter_country_countries) == 0) {
    return(NULL)
  }
  
  # Countries requested by the user
  selected_countries <- input$scatter_country_countries
  
  # Countries that actually remain after data cleaning
  plotted_countries <- scatter_country_data()$country
  
  # Identify selected countries that were dropped
  missing_countries <- setdiff(
    selected_countries,
    plotted_countries
  )
  
  # Return NULL if none were dropped
  if (length(missing_countries) == 0) {
    return(NULL)
  }
  
  missing_countries
})




# ==========================================================
# Country-level scatter plot
# Add log-scale axis options
# ==========================================================
# output$scatter_country_plot <- plotly::renderPlotly({
#   
#   df <- scatter_country_data()
#   req(nrow(df) > 0)
#   
#   show_labels <- isTRUE(input$scatter_country_show_labels)
#   
#   # Check whether log scaling is valid
#   log_x_ok <- isTRUE(input$scatter_country_log_x) && all(df$x > 0)
#   log_y_ok <- isTRUE(input$scatter_country_log_y) && all(df$y > 0)
#   
#   # Base plot (two branches to avoid Plotly text issues)
#   p <- if (show_labels) {
#     
#     plotly::plot_ly(
#       data = df,
#       x    = ~x,
#       y    = ~y,
#       type = "scatter",
#       mode = "markers+text",
#       text = ~country,
#       textposition = "top center",
#       hovertext = ~paste(
#         "Country:", country,
#         "<br>X:", round(x, 2),
#         "<br>Y:", round(y, 2)
#       ),
#       hoverinfo = "text",
#       marker = list(
#         size    = 9,
#         opacity = input$scatter_country_alpha
#       )
#     )
#     
#   } else {
#     
#     plotly::plot_ly(
#       data = df,
#       x    = ~x,
#       y    = ~y,
#       type = "scatter",
#       mode = "markers",
#       hovertext = ~paste(
#         "Country:", country,
#         "<br>X:", round(x, 2),
#         "<br>Y:", round(y, 2)
#       ),
#       hoverinfo = "text",
#       marker = list(
#         #size    = 9,
#         size    = input$scatter_country_point_size,  # user-controlled size
#         opacity = input$scatter_country_alpha,       # user-controlled opacity
#         color   = input$scatter_country_color,       # user-controlled color
#         symbol  = input$scatter_country_shape        # user-controlled shape
#       )
#     )
#   }
#   
#   # Apply axis scaling
#   p %>%
#     plotly::layout(
#       xaxis = list(
#         title = input$scatter_country_x,
#         type  = if (log_x_ok) "log" else "linear"
#       ),
#       yaxis = list(
#         title = input$scatter_country_y,
#         type  = if (log_y_ok) "log" else "linear"
#       ),
#       margin = list(l = 60, r = 20, b = 60, t = 30)
#     )
# })

# ==========================================================
# Country-level scatter plot
# Add log-scale axis options
# ==========================================================
output$scatter_country_plot <- plotly::renderPlotly({
  
  df <- scatter_country_data()
  req(nrow(df) > 0)
  
  show_labels <- isTRUE(input$scatter_country_show_labels)
  
  # --------------------------------------------------------
  # Look up human-readable axis labels
  # --------------------------------------------------------
  x_label <- FULL_COUNTRY_VAR_DICT %>%
    dplyr::filter(var_code == input$scatter_country_x) %>%
    dplyr::pull(label) %>%
    dplyr::first()
  
  y_label <- FULL_COUNTRY_VAR_DICT %>%
    dplyr::filter(var_code == input$scatter_country_y) %>%
    dplyr::pull(label) %>%
    dplyr::first()
  
  # Safety fallback (should not trigger, but robust)
  if (is.na(x_label) || x_label == "") {
    x_label <- input$scatter_country_x
  }
  if (is.na(y_label) || y_label == "") {
    y_label <- input$scatter_country_y
  }
  
  # --------------------------------------------------------
  # Check whether log scaling is valid
  # --------------------------------------------------------
  log_x_ok <- isTRUE(input$scatter_country_log_x) && all(df$x > 0)
  log_y_ok <- isTRUE(input$scatter_country_log_y) && all(df$y > 0)
  
  # --------------------------------------------------------
  # Base plot (two branches to avoid Plotly text issues)
  # --------------------------------------------------------
  p <- if (show_labels) {
    
    plotly::plot_ly(
      data = df,
      x    = ~x,
      y    = ~y,
      type = "scatter",
      mode = "markers+text",
      text = ~country,
      textposition = "top center",
      hovertext = ~paste(
        "Country:", country,
        "<br>X:", round(x, 2),
        "<br>Y:", round(y, 2)
      ),
      hoverinfo = "text",
      marker = list(
        size    = 9,
        opacity = input$scatter_country_alpha
      )
    )
    
  } else {
    
    plotly::plot_ly(
      data = df,
      x    = ~x,
      y    = ~y,
      type = "scatter",
      mode = "markers",
      hovertext = ~paste(
        "Country:", country,
        "<br>X:", round(x, 2),
        "<br>Y:", round(y, 2)
      ),
      hoverinfo = "text",
      marker = list(
        size    = input$scatter_country_point_size,
        opacity = input$scatter_country_alpha,
        color   = input$scatter_country_color,
        symbol  = input$scatter_country_shape
      )
    )
  }
  
  # --------------------------------------------------------
  # Apply axis titles using dictionary labels
  # --------------------------------------------------------
  p %>%
    plotly::layout(
      xaxis = list(
        title = x_label,  
        type  = if (log_x_ok) "log" else "linear"
      ),
      yaxis = list(
        title = y_label,   
        type  = if (log_y_ok) "log" else "linear"
      ),
      margin = list(l = 60, r = 20, b = 60, t = 30)
    )
})



################################################################################
############# GROUP-LEVEL ######################################################  
################################################################################
# ==========================================================
# OBSERVER to populate Source variable X (HDR tables)
# ==========================================================
observe({
  
  # Extract available HDR table names from group benchmarks
  hdr_tables <- HDR_GROUP_BENCHMARKS %>%
    dplyr::distinct(table) %>%      # unique table names
    dplyr::arrange(table) %>%       # keep a stable order. Sorts table names alphabetically
    dplyr::pull(table)              # Extracts the table column as a character vector
  
  # Add ALL option at the top
  table_choices <- c("ALL", hdr_tables)
  
  # Update Source variable X dropdown
  updateSelectInput(
    session,
    inputId = "scatter_group_source_x",
    choices = table_choices,
    selected = "ALL"
  )
})



# ==========================================================
# OBSERVER to populate Source variable Y (HDR tables)
# ==========================================================
observe({
  
  # Extract available HDR table names from group benchmarks
  hdr_tables <- HDR_GROUP_BENCHMARKS %>%
    dplyr::distinct(table) %>%      # unique table names
    dplyr::arrange(table) %>%       # keep a stable order. Sorts table names alphabetically
    dplyr::pull(table)              # Extracts the table column as a character vector
  
  # Add ALL option at the top
  table_choices <- c("ALL", hdr_tables)
  
  # Update Source variable X dropdown
  updateSelectInput(
    session,
    inputId = "scatter_group_source_y",
    choices = table_choices,
    selected = "ALL"
  )
})



# ==========================================================
# OBSERVER to populate X variable dropdown
# ==========================================================
observe({
  
  # --------------------------------------------------
  # 1. Require a selected HDR source (table or ALL)
  # --------------------------------------------------
  req(input$scatter_group_source_x)
  
  # --------------------------------------------------
  # 2. Base dataset: all group-level HDR benchmarks
  #    This table defines which variables actually
  #    exist at group level
  # --------------------------------------------------
  df_vars <- HDR_GROUP_BENCHMARKS
  
  # --------------------------------------------------
  # 3. If a specific HDR table is selected,
  #    restrict to variables from that table only
  # --------------------------------------------------
  if (input$scatter_group_source_x != "ALL") {
    df_vars <- df_vars %>%
      dplyr::filter(table == input$scatter_group_source_x)
  }
  
  # --------------------------------------------------
  # Identify valid variables for the current context
  #(i.e. variables that actually exist in the data)
  # --------------------------------------------------
  valid_vars <- df_vars %>%
    dplyr::distinct(variable) %>%
    dplyr::pull(variable)
  
  # --------------------------------------------------
  # Keep only variables that have authoritative
  # labels defined in HDR_LABELS
  # (safety check against missing labels)
  # --------------------------------------------------
  valid_vars <- valid_vars[valid_vars %in% names(HDR_LABELS)]
  
  # --------------------------------------------------
  # 6. Build named vector for Shiny dropdown
  #    - names   = variable codes (used internally)
  #    - values  = clean, unique labels (shown to user)
  # --------------------------------------------------
  choices_x <- setNames(
    valid_vars,
    HDR_LABELS[valid_vars]
  )
  
  # --------------------------------------------------
  # Sort choices alphabetically by label
  #    (improves UX and consistency)
  # --------------------------------------------------
  choices_x <- choices_x[order(names(choices_x))]
  
  # --------------------------------------------------
  # Update the X-variable selectize input
  # --------------------------------------------------
  updateSelectizeInput(
    session,
    inputId = "scatter_group_x",
    choices = choices_x,
    server  = TRUE
  )
  
})



# ==========================================================
# Populate Y-variable dropdown for GROUP-LEVEL scatter plot
# (Authoritative labels from HDR_LABELS)
# ==========================================================
observe({
  
  # --------------------------------------------------
  # Require a selected HDR source (table or ALL)
  # --------------------------------------------------
  req(input$scatter_group_source_y)
  
  # --------------------------------------------------
  # Base dataset: all group-level HDR benchmarks
  #    This table defines which variables actually
  #    exist at group level
  # --------------------------------------------------
  df_vars <- HDR_GROUP_BENCHMARKS
  
  # --------------------------------------------------
  # If a specific HDR table is selected,
  #    restrict to variables from that table only
  # --------------------------------------------------
  if (input$scatter_group_source_y != "ALL") {
    df_vars <- df_vars %>%
      dplyr::filter(table == input$scatter_group_source_y)
  }
  
  # --------------------------------------------------
  # Identify valid variables for the current context
  #    (i.e. variables that actually exist in the data)
  # --------------------------------------------------
  valid_vars <- df_vars %>%
    dplyr::distinct(variable) %>%
    dplyr::pull(variable)
  
  # --------------------------------------------------
  # Keep only variables that have authoritative
  #    labels defined in HDR_LABELS
  #    (safety check against missing labels)
  # --------------------------------------------------
  valid_vars <- valid_vars[valid_vars %in% names(HDR_LABELS)]
  
  # --------------------------------------------------
  # Build named vector for Shiny dropdown
  #    - names   = variable codes (used internally)
  #    - values  = clean, unique labels (shown to user)
  # --------------------------------------------------
  choices_y <- setNames(
    valid_vars,
    HDR_LABELS[valid_vars]
  )
  
  # --------------------------------------------------
  # Sort choices alphabetically by label
  #    (improves UX and consistency)
  # --------------------------------------------------
  choices_y <- choices_y[order(names(choices_y))]
  
  # --------------------------------------------------
  # Update the Y-variable selectize input
  # --------------------------------------------------
  updateSelectizeInput(
    session,
    inputId = "scatter_group_y",
    choices = choices_y,
    server  = TRUE
  )
  
})




# ==========================================================
# REACTIVE to create dynamically generated dataset to plot
# ==========================================================
scatter_group_data <- reactive({
  
  # Require both X and Y variables to be selected
  req(input$scatter_group_x, input$scatter_group_y)
  
  # Base dataset: group-level HDR benchmarks
  # Normalise group names to ensure safe joins
  df <- HDR_GROUP_BENCHMARKS %>%
    dplyr::mutate(
      benchmark_group = stringr::str_to_lower(group) %>%
        stringr::str_trim()
    )
  
  # Extract values for the selected X variable
  df_x <- df %>%
    dplyr::filter(variable == input$scatter_group_x) %>%
    dplyr::select(benchmark_group, x = value)
  
  # Extract values for the selected Y variable
  df_y <- df %>%
    dplyr::filter(variable == input$scatter_group_y) %>%
    dplyr::select(benchmark_group, y = value)
  
  # Combine X and Y values by group and drop incomplete pairs
  df_xy <- df_x %>%
    dplyr::inner_join(df_y, by = "benchmark_group") %>%
    dplyr::filter(!is.na(x), !is.na(y))
  
  # Add display names and number of countries per group
  df_xy %>%
    dplyr::left_join(
      HDR_GROUP_NAME_MAP,
      by = "benchmark_group"
    ) %>%
    dplyr::left_join(
      HDR_GROUP_COUNTS,
      by = c("lookup_group" = "group")
    ) %>%
    dplyr::mutate(
      group = benchmark_group   # restore display name for plotting
    )
})




# ==========================================================
# REACTIVE to Detect excluded groups due to missing data
# ==========================================================
scatter_group_missing <- reactive({
  
  req(input$scatter_group_x, input$scatter_group_y)
  
  df <- HDR_GROUP_BENCHMARKS
  
  # X values by group
  df_x <- df %>%
    dplyr::filter(variable == input$scatter_group_x) %>%
    dplyr::select(group, x = value)
  
  # Y values by group
  df_y <- df %>%
    dplyr::filter(variable == input$scatter_group_y) %>%
    dplyr::select(group, y = value)
  
  # Join X and Y WITHOUT dropping NAs
  df_xy <- df_x %>%
    dplyr::full_join(df_y, by = "group")
  
  # Groups where X or Y is missing
  missing_groups <- df_xy %>%
    dplyr::filter(is.na(x) | is.na(y)) %>%
    dplyr::pull(group)
  
  if (length(missing_groups) == 0) {
    return(NULL)
  }
  
  missing_groups
})



# ==========================================================
# RENDER for missing values 
# =========================================================
output$scatter_group_warning <- renderUI({
  
  missing <- scatter_group_missing()
  
  # Nothing to show if no groups are missing
  if (is.null(missing)) {
    return(NULL)
  }
  
  tags$p(
    style = "font-style: italic; color: #666; margin-top: 10px;",
    paste(
      "Note:",
      "The following HDR groups are not shown because data are missing",
      "for one or both selected variables:",
      paste(missing, collapse = ", ")
    )
  )
})




# ==========================================================
# Compute number of countries per HDR group (area)
# Base country counts from explicit memberships
# ==========================================================
HDR_GROUP_COUNTS <- HDR_AREA_LOOKUP %>%
  
  # Normalise group names for safe joining
  dplyr::mutate(
    group = stringr::str_to_lower(area),
    group = stringr::str_trim(group)
  ) %>%
  
  # Count countries per group
  dplyr::count(
    group,
    # n_countries: number of distinct countries belonging to each HDR group  
    name = "n_countries"
  )


cat("This is HDR_GROUP_COUNTS:\n")
print(HDR_GROUP_COUNTS)



# ==============================================================================
# COUNT countries in Developing countries = union of developing regions (UNDP rule)
# ==============================================================================
DEVELOPING_REGIONS <- c(
  "arab states",
  "east asia and the pacific",
  "europe and central asia",
  "latin america and the caribbean",
  "south asia",
  "sub-saharan africa"
)

n_developing <- HDR_AREA_LOOKUP %>%
  dplyr::mutate(
    group = stringr::str_to_lower(stringr::str_trim(area))
  ) %>%
  dplyr::filter(group %in% DEVELOPING_REGIONS) %>%
  dplyr::distinct(country) %>%
  nrow()

cat("This is DEVELOPING_REGIONS:\n")
print(n_developing)



# ==========================================================
# COUNT countries in World = all HDR countries
# ==========================================================
n_world <- MASTER_COUNTRY_DATA %>%
  dplyr::distinct(country) %>%
  nrow()

cat("This is World group ie all HDR countries:\n")
print(n_world)



# ============================================================
# INCLUDE DEVELOPING COUNTRIES and WORLD in aggregate groups
# ============================================================
HDR_GROUP_COUNTS <- HDR_GROUP_COUNTS %>%
  dplyr::bind_rows(
    tibble::tibble(
      group = "developing countries",
      n_countries = n_developing
    ),
    tibble::tibble(
      group = "world",
      n_countries = n_world
    )
  )

cat("This is HDR_GROUP_COUNTS:\n")
print(HDR_GROUP_COUNTS)





# ==========================================================
# DEFINE COLOR PALETTES LIST
# ==========================================================
GROUP_COLOUR_PALETTES <- list(
  
  default = NULL,  # let plotly choose (current behaviour)
  
  cbf = c(
    "#0072B2", "#E69F00", "#009E73", "#D55E00",
    "#CC79A7", "#56B4E9", "#F0E442", "#000000"
  ),
  
  contrast = c(
    "#000000", "#E41A1C", "#377EB8", "#4DAF4A",
    "#984EA3", "#FF7F00", "#FFFF33"
  ),
  
  grey = c(
    "#111111", "#333333", "#555555", "#777777",
    "#999999", "#BBBBBB", "#DDDDDD"
  )
)



# ==========================================================
# Enforce mutual exclusivity:
# Scaled size by number of countries vs Show group labels
# ==========================================================
# observe({
#   
#   req(input$scatter_group_size_mode)
#   
#   # If size is scaled by number of countries,
#   # automatically disable group labels
#   if (input$scatter_group_size_mode == "n_countries") {
#     
#     updateCheckboxInput(
#       session,
#       inputId = "scatter_group_show_labels",
#       value   = FALSE
#     )
#   }
# })

# Disable and uncheck "Show group labels" when size is scaled
observe({
  
  if (input$scatter_group_size_mode == "n_countries") {
    
    # Force labels OFF
    updateCheckboxInput(
      session,
      "scatter_group_show_labels",
      value = FALSE
    )
    
    # Disable the checkbox
    shinyjs::disable("scatter_group_show_labels")
    
  } else {
    
    # Re-enable checkbox when size is equal
    shinyjs::enable("scatter_group_show_labels")
  }
})




# ==========================================================
# Enforce mutual exclusivity between:
# - "Scaled by number of countries" (point size)
# - "Show group labels"
#
# When point size is scaled by number of countries,
# group labels are visually disabled to prevent
# cluttered and unreadable plots.
# ==========================================================
observe({
  
  # --------------------------------------------------
  # toggleState() enables or disables a UI input
  # based on a logical condition.
  #
  # id        : input ID to enable/disable
  # condition : TRUE  -> input enabled
  #             FALSE -> input disabled
  # --------------------------------------------------
  shinyjs::toggleState(
    
    # Checkbox controlling whether group labels are shown
    id = "scatter_group_show_labels",
    
    # Enable the checkbox ONLY when point size
    # is NOT scaled by number of countries.
    # If size mode == "n_countries", labels are disabled.
    condition = input$scatter_group_size_mode != "n_countries"
  )
})








# ==========================================================
# Group-level scatter plot (Plotly)
# =========================================================
output$scatter_group_plot <- plotly::renderPlotly({
  
  # Get data
  df <- scatter_group_data()
  req(nrow(df) > 0)
  
  # Palette and shape
  palette     <- GROUP_COLOUR_PALETTES[[input$scatter_group_palette]]
  point_shape <- input$scatter_group_shape
  
  # Determine whether labels are allowed
  show_labels <- isTRUE(input$scatter_group_show_labels) &&
    input$scatter_group_size_mode != "n_countries"
  
  # Plotly mode
  plot_mode <- if (show_labels) "markers+text" else "markers"
  
  # Text must be NULL when labels are not allowed
  #plot_text <- if (show_labels) ~group else NULL
  plot_text <- if (show_labels) df$group else NULL
  
  
  # --------------------------------------------------
  # Build plot
  # --------------------------------------------------
  if (input$scatter_group_size_mode == "n_countries") {
    
    # Size scaled by number of countries
    p <- plotly::plot_ly(
      data   = df,
      x      = ~x,
      y      = ~y,
      type   = "scatter",
      mode   = plot_mode,
      color  = ~group,
      colors = palette,
      size   = ~n_countries,
      sizes  = c(100, 10000),
      text   = plot_text,
      marker = list(
        symbol  = point_shape,
        opacity = 0.8,
        line    = list(width = 1, color = "white")
      ),
      hovertext = ~paste(
        "Group:", group,
        "<br>X:", round(x, 3),
        "<br>Y:", round(y, 3),
        "<br>Countries:", n_countries
      ),
      hoverinfo = "text"
    )
    
  } else {
    
    # Equal-size points
    p <- plotly::plot_ly(
      data   = df,
      x      = ~x,
      y      = ~y,
      type   = "scatter",
      mode   = plot_mode,
      color  = ~group,
      colors = palette,
      text   = plot_text,
      marker = list(
        symbol  = point_shape,
        size    = 20,
        opacity = 0.8,
        line    = list(width = 1, color = "white")
      ),
      hovertext = ~paste(
        "Group:", group,
        "<br>X:", round(x, 3),
        "<br>Y:", round(y, 3),
        "<br>Countries:", n_countries
      ),
      hoverinfo = "text"
    )
  }
  
  # Layout
  p %>% plotly::layout(
    showlegend = FALSE,
    xaxis = list(title = HDR_LABELS[[input$scatter_group_x]],
                 cliponaxis = FALSE   #allow labels to extend beyond plot area
    ),
    yaxis = list(title = HDR_LABELS[[input$scatter_group_y]],
                 cliponaxis = FALSE   #allow labels to extend beyond plot area
    ),
    margin = list(l = 70, r = 20, b = 70, t = 30)
  )
})











#==========================#   
#===                    ===#
#===      MODELS        ===#
#===                    ===#
#==========================#      
#===                    ===#
#=== LINEAR REGRESSION  ===#
#===                    ===#
#==========================#  
################################################################################
# ########### COUNTRY-LEVEL ####################################################  
################################################################################

# ==========================================================
#  DEFINE DATA
# ==========================================================

# Variables available for country-level regression
# ----------------------------------------------------
#idendtifiers to be excluded from dropdown menu 
ID_VARS <- c("country")

AVAILABLE_COUNTRY_VARS <- setdiff(
names(MASTER_COUNTRY_DATA),
ID_VARS
)


# Filter the variable dictionary to country-level usable vars
# -------------------------------------------------------------
#Prevents dropdowns from showing variables that don’t exist
# Automatically stays in sync if data changes later
COUNTRY_VAR_DICT <- FULL_COUNTRY_VAR_DICT %>%
dplyr::filter(var_code %in% AVAILABLE_COUNTRY_VARS)



#   HDR-only outcomes (DEFAULT)
# ----------------------------------------------
## Build named choices for HDR outcome variables
HDR_OUTCOME_CHOICES <- COUNTRY_VAR_DICT %>%
dplyr::filter(source == "HDR") %>% # Keep only variables from HDR
dplyr::select(var_code, label) %>%
dplyr::arrange(label) %>%
{ setNames(.$var_code, .$label) }



# All outcomes (ADVANCED OPTIONS)
# ----------------------------------------------------

ALL_OUTCOME_CHOICES <- COUNTRY_VAR_DICT %>%
dplyr::filter(source %in% c("HDR", "WVS7")) %>%
dplyr::select(var_code, label) %>%
dplyr::arrange(label) %>%
{ setNames(.$var_code, .$label) }



#    Explanatory variables
# -----------------------------------------------------
COUNTRY_EXPLANATORY_CHOICES <- COUNTRY_VAR_DICT %>%
dplyr::filter(source %in% c("HDR", "WVS7")) %>%
dplyr::select(var_code, label) %>%
dplyr::arrange(label) %>%
{ setNames(.$var_code, .$label) }




# ==========================================================
#  OBSERVER to select the dependent variable
# ==========================================================
# Observe changes to inputs used to control which outcomes are allowed
observe({

# If the user allows non-HDR outcomes,
# populate the dependent-variable dropdown with all available outcomes
if (isTRUE(input$allow_non_hdr_outcome)) {

updateSelectInput(
  session,
  inputId = "country_reg_dep",
  choices = ALL_OUTCOME_CHOICES
)

} else {

# Otherwise, restrict the dropdown to HDR outcomes only
# (this is the default and recommended option)
updateSelectInput(
  session,
  inputId = "country_reg_dep",
  choices = HDR_OUTCOME_CHOICES
)

}

})




# ==========================================================
#  OBSERVER to select INdependent variable(s)
# ==========================================================
observe({

updatePickerInput(
session,
inputId = "country_reg_indep",
choices = COUNTRY_EXPLANATORY_CHOICES
)

})



# ==========================================================
# Build country-level regression dataset
# Triggered by Run Regression button
# ==========================================================
country_regression_data <- eventReactive(input$country_reg_run, {

# ------------------------------------------
# Require minimal inputs
# ------------------------------------------
req(
input$country_reg_dep,
input$country_reg_indep
)

# ------------------------------------------
# Extract variable names
# ------------------------------------------
dep_var     <- input$country_reg_dep
indep_vars  <- input$country_reg_indep

# ------------------------------------------
# Safety: remove outcome from predictors
# ------------------------------------------
indep_vars <- setdiff(indep_vars, dep_var)
req(length(indep_vars) > 0)

# ------------------------------------------
# Build dataset
# ------------------------------------------
data <- MASTER_COUNTRY_DATA %>%
dplyr::select(all_of(c(dep_var, indep_vars)))

# # ------------------------------------------
# # DEBUG: Explicit confirmation that button fired
# # ------------------------------------------
# showNotification(
#   paste(
#     "Run Regression clicked.",
#     "Outcome:", dep_var,
#     "| Predictors:", paste(indep_vars, collapse = ", ")
#   ),
#   type = "message",
#   duration = 4
# )

# ------------------------------------------
# Return structured object
# ------------------------------------------
list(
data       = data,
dep_var    = dep_var,
indep_vars = indep_vars
)
})


#View(country_regression_data)



# ==========================================================
# Helper to convert variable codes to human-readable labels
# ==========================================================
get_var_label <- function(var_codes) {

labels <- FULL_COUNTRY_VAR_DICT %>%
dplyr::filter(var_code %in% var_codes) %>%
dplyr::arrange(match(var_code, var_codes)) %>%  # preserve order
dplyr::pull(label)

# Fallback if lookup fails
if (length(labels) == 0 || any(is.na(labels))) {
return(var_codes)
}

labels
}




# ==========================================================
#     Fit regression model
# ==========================================================
country_regression_model <- reactive({
reg <- country_regression_data()
req(reg)

data <- reg$data

# Drop incomplete cases
data <- stats::na.omit(data)
req(nrow(data) > 5)   # safety: avoid model built with tiny dataset n =< 5

# Build formula programmatically
formula <- stats::as.formula(
paste(
  reg$dep_var,
  "~",
  paste(reg$indep_vars, collapse = " + ")
)
)

# Fit linear model
stats::lm(formula, data = data)
})


# observeEvent(input$country_reg_run, {
#   country_regression_data()
# })





# ==========================================================
# OUTPUT summary table
# ==========================================================
output$country_reg_summary <- renderPrint({

result <- country_regression_data()
model  <- country_regression_model()

req(result, model)

data <- result$data

# ------------------------------------------
# Custom header (like individual-level)
# ------------------------------------------
cat("Linear Regression Model Summary\n")
cat("================================\n")
cat("Outcome variable: ", get_var_label(result$dep_var), "\n")
cat(
"Independent variables: ",
paste(get_var_label(result$indep_vars), collapse = ", "),
"\n"
)
cat(
"Number of complete observations: ",
nrow(data),
"\n\n"
)

# ------------------------------------------
# Standard lm() summary
# ------------------------------------------
print(summary(model))
})







# ==========================================================
# OUTPUT lm diagnostics 
# ==========================================================
output$country_reg_diagnostics <- renderPlot({

model <- country_regression_model()
req(model)

# Standard lm diagnostics (4 plots)
par(mfrow = c(2, 2))
plot(model)
})





# ==========================================================
# OUTPUT lm prediction 
# ==========================================================
output$country_reg_prediction <- renderPlotly({

model <- country_regression_model()
data  <- country_regression_data()

req(model, data)

df <- data$data

# ---------------------------------------
# Generate predictions
# ---------------------------------------
df$predicted <- predict(model, newdata = df)

# ---------------------------------------
# Build plot
# ---------------------------------------
p <- ggplot2::ggplot(
df,
ggplot2::aes(
  x = predicted,
  y = .data[[data$dep_var]]
)
) +
ggplot2::geom_point(
  color = "#2c7fb8",
  size  = 2,
  alpha = 0.7
) +
ggplot2::geom_abline(
  intercept = 0,
  slope = 1,
  linetype = "dashed",
  color = "grey40"
) +
ggplot2::labs(
  title = "Observed vs Predicted Values",
  x = "Predicted outcome",
  y = "Observed outcome"
) +
ggplot2::theme_minimal()

plotly::ggplotly(p)
})







# ==========================================================
# OBSERVER to filter countries by group
# ==========================================================

observe({

req(input$allow_non_hdr_outcome)

if (isTRUE(input$allow_non_hdr_outcome)) {

updateSelectInput(
  session,
  inputId = "country_reg_dep",
  choices = ALL_OUTCOME_CHOICES
)

} else {

updateSelectInput(
  session,
  inputId = "country_reg_dep",
  choices = HDR_OUTCOME_CHOICES
)

}
})





# ==========================================================
# Enforce mutual exclusivity:
# Group filter vs individual country selection
# ==========================================================
observe({

# Group filter is ON
if (isTRUE(input$country_reg_group_on)) {

# Clear individual country selection
updateSelectizeInput(
  session,
  inputId = "country_reg_countries",
  selected = character(0)
)

# Disable country selector
shinyjs::disable("country_reg_countries")

} else {

# Re-enable country selector
shinyjs::enable("country_reg_countries")

}
})









# ==========================================================

# ==========================================================





# ==========================================================

# ==========================================================






# ==========================================================

# ==========================================================





# ==========================================================

# ==========================================================


















}) # end server logic







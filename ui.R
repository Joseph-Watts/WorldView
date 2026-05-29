#' Code based on: https://shiny.posit.co/r/gallery/life-sciences/biodiversity-national-parks/
#' WVS data source: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp

#' Some things still needing done:
#' - Add world map plots for the country level data page
#' 
#' -----------
#' Things that would be nice to add in the future:
#' 
#' - Integrate data from outside the WVS with the global analyses
#' 
#' - Individual level world plots using the latitude and longitude coordinates 
#' provided in the WVS
#' 
#' - Plot country level data on a language phylogeny
#' 
#' - Perform more sophisticated statistical models

#################-
#### LOAD UI ####
#################-

shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  # load google analytics script
  tags$head(includeScript("www/google-analytics.js")),
  
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }", # remove shiny "red" warning messages on GUI
    ".shiny-output-error:before { visibility: hidden; }",
    HTML("")
  ),
  
  dashboardPage(
    
    skin = "green", # green palette for specific buttons -> #00a65a
    
    dashboardHeader(title = "World Values Survey", titleWidth = 300),
    
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       HTML(
                         paste0(
                           "<br>",
                           "<a href='https://www.worldvaluessurvey.org' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='images/logo_v1.png' width = '186'></a>",
                           "<br>",
                           "<p style = 'text-align: center;'><small>Data visualisation tool for <br> PSYC382: Culture and Cognition</small></p>",
                           "<br>"
                         ) # sidebar bg color -> #222d32
                       ),
                       
                       ####################### HOME #######################
                       menuItem(
                         "Home",
                         tabName = "home",
                         icon = icon("home")
                       ),
                       ####################### HOME #######################
                       
                       
                       ############## VARIABLE DOCUMENTATION ##############
                       menuItem(
                         "Codebook",
                         tabName = "codebook",
                         icon = icon('info-sign', lib = "glyphicon")
                       ),
                       
                       
                       # menuItem(
                       #   "Dataset Documentation",
                       #   tabName = "dummy",
                       #   icon = icon('info-sign', lib = "glyphicon"),
                       #   startExpanded = F,
                       #   
                       #   menuSubItem(
                       #     "Master Survey Questionnaire",
                       #     tabName = "surveyview",
                       #     icon = icon("comment", lib = "glyphicon")
                       #   ),
                       #   
                       #   menuSubItem(
                       #     "Questionnaire Codebook",
                       #     tabName = "codebookview",
                       #     icon = icon("book", lib = "glyphicon")
                       #   )
                       # ),
                       ############## VARIABLE DOCUMENTATION ##############
                       
                       
                       ################## RAW DATA TABLES #################
                       menuItem(
                         "Raw Data Tables",
                         tabName = "dummy",
                         icon = icon('list', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "Participant-level Data",
                           tabName = "EDA_indiv",
                           icon = icon("user", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Country-level Data",
                           tabName = "EDA_country",
                           icon = icon("globe", lib = "glyphicon")
                         ) #,
                         # 
                         # menuSubItem(
                         #   "Missing Data Visualizations",
                         #   tabName = "vis_miss",
                         #   icon = icon("warning-sign", lib = "glyphicon")
                         # )
                       ),
                       ################## RAW DATA TABLES #################
                       
                       
                       ################ SUMMARY STATISTICS ################
                       menuItem(
                         "Summary Statistics",
                         tabName = "dummy",
                         icon = icon('info-sign', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "Univariate Statistics",
                           tabName = "univariateStats",
                           icon = icon("chart-bar")
                         ),
                         
                         menuSubItem(
                           "Bivariate Statistics",
                           tabName = "bivariateStats",
                           icon = icon("project-diagram")
                         )
                       ), 
                       ################ SUMMARY STATISTICS ################
                       
                       
                       ################## VISUALISATIONS ##################
                       menuItem(
                         "Visualisations",
                         tabName = "dummy",
                         icon = icon('picture', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "Bar Chart",
                           tabName = "barChart",
                           icon = icon("stats", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Scatterplot",
                           tabName = "scatterParticipants",
                           icon = icon("move", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Correlation", 
                           tabName = "correlationView",
                           icon = icon("equalizer", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Histogram",
                           tabName = "histogramView",
                           icon = icon("signal")
                           # icon = icon("bi-soundwave", lib = "glyphicon")
                         )
                       ),
                       ################## VISUALISATIONS ##################
                       
                       
                       ###################### MODELS ######################
                       menuItem(
                         "Models",
                         tabName = "dummy",
                         icon = icon('screenshot', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "Correlation Models",
                           tabName = "corrModelTab",
                           icon = icon("sort-by-attributes-alt", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "ANOVA",
                           tabName = "anovaTab",
                           icon = icon("th", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Linear Regression",
                           tabName = "regressionTab",
                           icon = icon("line-chart")
                         )
                       ),
                       ###################### MODELS ######################
                       
                       
                       ######################## FAQ #######################
                       menuItem(
                         "FAQ",
                         tabName = "faq",
                         icon = icon("question-sign", lib = "glyphicon")
                       ),
                       ######################## FAQ #######################
                       
                       
                       ###################### ABOUT #######################
                       menuItem(
                         "About the Team",
                         tabName = "team",
                         icon = icon("user", lib = "glyphicon")
                       )
                       ###################### ABOUT #######################
                       
                     ) # end sidebarMenu
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "dummy"
                # INTENTIONALLY EMPTY
        ),
        
        ####################### HOME #######################
        tabItem(tabName = "home",
                includeMarkdown("www/home.md")
        ),
        ####################### HOME #######################
        
        
        ############## VARIABLE DOCUMENTATION ##############
        tabItem(tabName = "codebook",
                includeMarkdown("www/codebook.md")
        ),
        
        # tabItem(tabName = "surveyview",
        #         fluidRow(column(12, shinycssloaders::withSpinner(uiOutput("surveyview"))))
        # ),
        # 
        # tabItem(tabName = "codebookview",
        #         fluidRow(column(12, shinycssloaders::withSpinner(uiOutput("codebookview"))))
        # ),
        ############## VARIABLE DOCUMENTATION ##############
        
        
        ################## RAW DATA TABLES #################
        tabItem(tabName = "EDA_indiv",
                includeMarkdown("www/instructions/DTable.md"),
                tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                fluidRow(column(12, uiOutput("raw_selectCountry"))),
                fluidRow(column(12, DTOutput("raw_filtered_country")))
        ),
        
        tabItem(tabName = "EDA_country",
                includeMarkdown("www/instructions/DTable.md"),
                tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                fluidRow(column(12, DTOutput("Table_country")))
        ),
        
        # tabItem(tabName = "vis_miss",
        #         includeMarkdown("www/instructions/miss_vars.md"),
        #         
        #         tabsetPanel(id = "MissingViews", type = "pills",
        #                     tabPanel("Top 15 Missing", value = "top_miss",
        #                              fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Top_miss_indiv", height = "60vh")))),
        #                              fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Top_miss_country", height = "60vh"))))
        #                     ),
        #                     
        #                     tabPanel("Missing data from Individual Responses", value = "indiv_miss",
        #                              fluidRow(column(3,
        #                                dropdownButton(
        #                                  inputId = "indiv_miss_adv",
        #                                  label = "Advanced Options",
        #                                  icon = icon("sliders"),
        #                                  status = "success",
        #                                  circle = FALSE,
        #                                  materialSwitch(
        #                                    inputId = "cluster_indiv",
        #                                    label = "Cluster missingness",
        #                                    status = "success"
        #                                  ),
        #                                  materialSwitch(
        #                                    inputId = "sort_indiv",
        #                                    label = "Sort columns by missingness",
        #                                    status = "success"
        #                                  )
        #                                )
        #                              )),
        #                              fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Indiv_missing_with_ratio", height = "85vh"))))
        #                     ),
        #                     
        #                     tabPanel("Missing data from Countries responses", value = "countries_miss",
        #                              fluidRow(column(3,
        #                                dropdownButton(
        #                                  inputId = "ctry_miss_adv",
        #                                  label = "Advanced Options",
        #                                  icon = icon("sliders"),
        #                                  status = "success",
        #                                  circle = FALSE,
        #                                  materialSwitch(
        #                                    inputId = "cluster_ctry",
        #                                    label = "Cluster missingness",
        #                                    status = "success"
        #                                  ),
        #                                  materialSwitch(
        #                                    inputId = "sort_ctry",
        #                                    label = "Sort columns by missingness",
        #                                    status = "success"
        #                                  )
        #                                )
        #                              )),
        #                              fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Missing", height = "85vh"))))
        #                     )
        #         )
        # ),
        ################## RAW DATA TABLES #################
        
        
        ################ SUMMARY STATISTICS ################
        # Univariate Stats tab
        tabItem(tabName = "univariateStats",
                includeMarkdown("www/instructions/univariate_instruction.md"),
                fluidRow(shinydashboard::box(width = 4, status = "primary",
                                             selectizeInput(inputId = "univar_question",
                                                            label = "Select Question:",
                                                            choices = grouped_minus_ignored,
                                                            selected = grouped_minus_ignored[[1]][1]),
                                             pickerInput(inputId = "univar_countries",
                                                         label = "Select Countries:",
                                                         choices = picker_country_list,
                                                         multiple = TRUE,
                                                         options = list(
                                                           `actions-box` = TRUE,
                                                           `live-search` = TRUE
                                                           ),
                                                         selected = c("NZL", "AUS", "GBR")
                                                         )
                                             ),
                         shinydashboard::box(width = 8, title = "Univariate Summary", status = "primary",
                                             uiOutput("univariate_results")
                                             )
                         )
        ),
        
        # Bivariate Stats tab
        tabItem(tabName = "bivariateStats",
                includeMarkdown("www/instructions/bivariate_instruction.md"),
                fluidRow(shinydashboard::box(width = 4,
                                             status = "primary",
                                             selectizeInput(inputId = "bivariate_var1",
                                                            label = "Select Variable 1:",
                                                            choices = grouped_minus_ignored,
                                                            selected = grouped_minus_ignored[[1]][1]
                                                            ),
                                             selectizeInput(inputId = "bivariate_var2",
                                                            label = "Select Variable 2:",
                                                            choices = grouped_minus_ignored,
                                                            selected = grouped_minus_ignored[[1]][2]
                                                            ),
                                             pickerInput(inputId = "bivariate_countries",
                                                         label = "Select Countries:",
                                                         choices = picker_country_list,
                                                         multiple = TRUE,
                                                         options = list(`actions-box` = TRUE,
                                                                        `live-search` = TRUE,
                                                                        `max-options` = 5
                                                                        ),
                                                         selected = c("NZL", "AUS", "GBR")
                                                         ),
                                             radioGroupButtons(inputId = "bivariate_type",
                                                               label = "Table Type:",
                                                               choices = c("Counts", "Row Percentages", "Column Percentages"),
                                                               selected = "Counts",
                                                               status = "success"
                                                               )
                                             ),
                         shinydashboard::box(width = 8,
                                             title = "Bivariate Summary",
                                             status = "primary",
                                             shinycssloaders::withSpinner(DTOutput("bivariate_table"))
                                             )
                         )
        ),
        ################ SUMMARY STATISTICS ################
        
        
        ################## VISUALISATIONS ##################
        tabItem(tabName = "barChart",
                includeMarkdown("www/instructions/bar_instruction.md"),
                fluidRow(
                  shinydashboard::box(width = 3, status = "primary",
                      selectizeInput(
                        inputId = "bar_question",
                        label = "Select Question:",
                        choices = grouped_minus_ignored,
                        selected = grouped_minus_ignored[[1]][1],
                        size = 30
                      ),
                      pickerInput(
                        inputId = "bar_countries",
                        label = "Select Countries:",
                        choices = picker_country_list,
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE,
                          `size` = 30,
                          `max-options` = 5
                        ),
                        selected = c("NZL", "AUS", "GBR")
                      ),
                      radioGroupButtons(
                        inputId = "bar_type",
                        label = "Display Type:",
                        choices = c("Count", "Percentage", "Stacked", "Staggered"),
                        selected = "Count",
                        status = "success"
                      )
                  ),
                  shinydashboard::box(width = 9, title = "Response Distribution", status = "primary",
                      shinycssloaders::withSpinner(plotlyOutput("bar_plot", height = "600px"))
                  )
                )
        ),
        
        tabItem(tabName = "scatterParticipants",
                includeMarkdown("www/instructions/scatter_instruction.md"),
                fluidRow(
                  shinydashboard::box(width = 3, status = "primary",
                      selectizeInput(
                        inputId = "scatter_x",
                        label = "X-axis Question:",
                        choices = grouped_minus_ignored,
                        selected = grouped_minus_ignored[[1]][1]
                      ),
                      selectizeInput(
                        inputId = "scatter_y",
                        label = "Y-axis Question:",
                        choices = grouped_minus_ignored,
                        selected = grouped_minus_ignored[[1]][2]
                      ),
                      pickerInput(
                        inputId = "scatter_countries",
                        label = "Select Countries:",
                        choices = picker_country_list,
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE,
                          `size` = 30,
                          `max-options` = 5
                        ),
                        selected = c("NZL", "AUS", "GBR")
                      ),
                      sliderInput(
                        "scatter_sample",
                        "Sample Size (as % of data):",
                        min = 10, max = 100, value = 25, step = 1
                      )
                  ),
                  shinydashboard::box(width = 9, title = "Participant Scatterplot", status = "primary",
                      shinycssloaders::withSpinner(plotlyOutput("scatter_plot", height = "600px"))
                  )
                )
        ),
        
        tabItem(tabName = "correlationView",
                includeMarkdown("www/instructions/corr_instruction.md"),
                fluidRow(
                  shinydashboard::box(width = 3, status = "primary",
                      pickerInput(
                        inputId = "corr_questions",
                        label = "Select Questions:",
                        choices = grouped_minus_ignored,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE,
                                       `live-search` = TRUE,
                                       `max-options` = 8),
                        selected = grouped_minus_ignored[[1]][1:5]
                      ),
                      pickerInput(
                        inputId = "corr_countries",
                        label = "Select Countries:",
                        choices = picker_country_list,
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE,
                          `size` = 30,
                          `max-options` = 5
                        ),
                        selected = c("NZL", "AUS", "GBR")
                      ),
                      radioGroupButtons(
                        inputId = "corr_method",
                        label = "Correlation Method:",
                        choices = c("Pearson", "Spearman", "Kendall"),
                        selected = "Pearson",
                        status = "success"
                      ),
                      
                      # Advanced controls
                      dropdownButton(
                        inputId = "corr_advanced",
                        label = "Advanced Options",
                        icon = icon("sliders"),
                        status = "success",
                        circle = FALSE,
                        materialSwitch(inputId = "corr_method_type", label = "Ellipse / Color", status = "success"),
                        prettyRadioButtons(inputId = "corr_order", label = "Order", 
                                           choices = c("FPC", "alphabet", "AOE", "hclust"),
                                           selected = "FPC"),
                        noUiSliderInput(inputId = "corr_tl_cex", label = "Text Size", min = 0.5, max = 2, value = 1),
                        materialSwitch(inputId = "corr_type", label = "Type", status = "success"),
                        prettyCheckbox(inputId = "corr_diag", label = "Show Diagonal", value = FALSE),
                        prettyCheckbox(inputId = "corr_addCoef", label = "Show Coefficients", value = TRUE),
                        prettyRadioButtons(inputId = "corr_coef_color", label = "Coefficient Color", 
                                           choices = c("Black", "Blue", "Red"), selected = "Black"),
                        sliderInput("corr_tl_srt", "Text Rotation:", min = 0, max = 90, value = 45),
                        materialSwitch(inputId = "corr_bg", label = "Background Color", status = "success"),
                        
                        # Color palette selector
                        prettyRadioButtons(
                          inputId = "corr_palette",
                          label = "Color Palette:",
                          choices = c("Red-Blue", "Viridis"),
                          selected = "Red-Blue",
                          status = "success"
                        )
                      ),
                      
                      # Download button
                      div(
                        style = "margin-top: 20px; display: flex; justify-content: space-between;",
                        downloadButton("corr_download", "Download Plot", 
                                       class = "btn btn-success",
                                       style = "background-color: #4CAF50; color: white; border: none;")
                      )
                  ),
                  
                  shinydashboard::box(width = 9, title = "Correlation Matrix", status = "primary",
                      shinycssloaders::withSpinner(plotOutput("corr_plot", height = "600px"))
                  )
                )
        ),
        
        tabItem(tabName = "histogramView",
                includeMarkdown("www/instructions/histogram_instruction.md"),
                fluidRow(
                  shinydashboard::box(
                    width = 3,
                    status = "primary",
                    selectizeInput(
                      inputId = "hist_question",
                      label = "Select Question:",
                      choices = grouped_minus_ignored,
                      selected = grouped_minus_ignored[[1]][1],
                      size = 30
                    ),
                    pickerInput(
                      inputId = "hist_countries",
                      label = "Select Countries:",
                      choices = picker_country_list,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `size` = 30,
                        `max-options` = 5
                      ),
                      selected = c("NZL", "AUS", "GBR")
                    ),
                    sliderInput(
                      "hist_bins",
                      "Number of Bins:",
                      min = 5,
                      max = 50,
                      value = 20
                    ),
                    radioGroupButtons(
                      inputId = "hist_type",
                      label = "Display Type:",
                      choices = c("Density", "Frequency", "Stacked"),
                      selected = "Density",
                      status = "success"
                    ),
                    materialSwitch(
                      inputId = "hist_facet",
                      label = "Show Countries Separately",
                      status = "success",
                      value = FALSE
                    ),
                    materialSwitch(
                      inputId = "hist_curve",
                      label = "Show Normal Curve",
                      status = "success",
                      value = TRUE
                    )
                    # actionButton("hist_update", "Update Plot", class = "green-button")
                  ),
                  shinydashboard::box(
                    width = 9,
                    title = "Response Distribution",
                    status = "primary",
                    shinycssloaders::withSpinner(plotlyOutput("hist_plot", height = "600px"))
                  )
                )
        ),
        ################## VISUALISATIONS ##################
        
        
        ###################### MODELS ######################
        tabItem(tabName = "corrModelTab",
                includeMarkdown("www/instructions/corrModel_instruction.md"),
                fluidRow(
                  shinydashboard::box(width = 3, status = "primary",
                      selectizeInput(
                        inputId = "corr_model_var1",
                        label = "Select Variable 1:",
                        choices = grouped_minus_ignored,
                        selected = grouped_minus_ignored[[1]][1]
                      ),
                      selectizeInput(
                        inputId = "corr_model_var2",
                        label = "Select Variable 2:",
                        choices = grouped_minus_ignored,
                        selected = grouped_minus_ignored[[1]][2]
                      ),
                      pickerInput(
                        inputId = "corr_model_countries",
                        label = "Select Countries:",
                        choices = picker_country_list,
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        ),
                        selected = c("NZL", "AUS")
                      ),
                      radioGroupButtons(
                        inputId = "corr_choice",
                        label = "Correlation Method:",
                        choices = c("Pearson", "Spearman", "Kendall"),
                        selected = "Pearson",
                        status = "success"
                      ),
                      # sliderInput(
                      #   "corr_model_sample",
                      #   "Sample Size:",
                      #   min = 100, max = 5000, value = 1000, step = 100
                      # ),
                      actionButton("corr_model_run", "Run Analysis", class = "green-button")
                  ),
                  shinydashboard::box(width = 9, title = "Correlation", status = "primary",
                      tabsetPanel(
                        tabPanel("Results",
                                 verbatimTextOutput("corr_mod_results")),
                                 # plotlyOutput("kendall_plot")),
                        tabPanel("Data",
                                 DTOutput("corr_mod_data"))
                      )
                  )
                )
        ),
        
        tabItem(tabName = "anovaTab",
                includeMarkdown("www/instructions/anova_instruction.md"),
                fluidRow(
                  shinydashboard::box(width = 3, status = "primary",
                      selectizeInput(
                        inputId = "anova_var",
                        label = "Select Variable:",
                        choices = grouped_minus_ignored,
                        selected = grouped_minus_ignored[[1]][1]
                      ),
                      pickerInput(
                        inputId = "anova_countries",
                        label = "Select Countries:",
                        choices = picker_country_list,
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        ),
                        selected = c("NZL", "AUS")
                      ),
                      # sliderInput(
                      #   "anova_sample",
                      #   "Sample Size:",
                      #   min = 100, max = 5000, value = 1000, step = 100
                      # ),
                      actionButton("anova_run", "Run Analysis", class = "green-button")
                  ),
                  shinydashboard::box(width = 9, title = "ANOVA Results", status = "primary",
                      tabsetPanel(
                        tabPanel("ANOVA Table",
                                 verbatimTextOutput("anova_results")),
                        tabPanel("Post Hoc Tests",
                                 verbatimTextOutput("posthoc_results")),
                        tabPanel("Visualization",
                                 plotlyOutput("anova_plot")),
                        tabPanel("Assumptions",
                                 verbatimTextOutput("assumptions_check"),
                                 plotOutput("assumptions_plot"))
                      )
                  )
                )
        ),
        
        tabItem(tabName = "regressionTab",
                includeMarkdown("www/instructions/linearreg_instruction.md"),
                fluidRow(
                  shinydashboard::box(width = 3, status = "primary",
                      selectizeInput(
                        inputId = "regression_dep",
                        label = "Dependent Variable:",
                        choices = grouped_minus_ignored,
                        selected = grouped_minus_ignored[[1]][1]
                      ),
                      pickerInput(
                        inputId = "regression_indep",
                        label = "Independent Variables:",
                        choices = grouped_minus_ignored,
                        multiple = TRUE,
                        selected = grouped_minus_ignored[[1]][2:3],
                        options = list(`live-search` = TRUE)
                      ),
                      pickerInput(
                        inputId = "regression_country",
                        label = "Select Country:",
                        choices = picker_country_list,
                        multiple = TRUE,
                        selected = "NZL",
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        ),
                      ),
                      # sliderInput(
                      #   "regression_sample",
                      #   "Sample Size:",
                      #   min = 100, max = 5000, value = 1000, step = 100
                      # ),
                      actionButton("regression_run", "Run Regression", class = "green-button")
                  ),
                  shinydashboard::box(width = 9, title = "Regression Analysis", status = "primary",
                      tabsetPanel(
                        tabPanel("Model Summary",
                                 verbatimTextOutput("regression_summary")),
                        tabPanel("Diagnostics",
                                 plotOutput("regression_diag")),
                        # tabPanel("Coefficients",
                        #          DTOutput("regression_coef")),
                        tabPanel("Prediction",
                                 plotlyOutput("regression_prediction"))
                      )
                  )
                )
        ),
        
        
        ###################### MODELS ######################
        
 
        ######################## FAQ #######################
        tabItem(tabName = "faq",
                includeMarkdown("www/faq.md")
        ),
        ######################## FAQ #######################
        
        
        ###################### ABOUT #######################
        tabItem(tabName = "team",
                fluidRow(
                  column(12, h1("Meet Our Team", style = "text-align: center; color: #2c3e50;"))
                ),
                
                # Team Leadership Section
                fluidRow(
                  column(12, h3("Team Leadership", style = "color: #2c3e50; border-bottom: 2px solid #00a65a; padding-bottom: 10px;"))
                ),
                fluidRow(
                  column(4, 
                         div(class = "team-card",
                             img(src = "images/JW.png", style = "width: 150px; height: 150px; object-fit: cover; border-radius: 50%; border: 4px solid #f1f8ff; margin: 0 auto 20px; display: block;"),
                             div(style = "text-align: center;",
                                 h4("Joseph W. H. Watts", style = "color: #2c3e50; font-weight: 700; margin-top: 15px;"),
                                 h5("Project Lead", style = "color: #3498db; font-weight: 600; margin-bottom: 15px;"),
                                 p("Senior Lecturer Above the Bar, School of Psychology, Speech and Hearing", style = "color: #34495e; line-height: 1.6;"),
                                 a(href = "https://profiles.canterbury.ac.nz/Joseph-William-Harry-Watts", 
                                   target = "_blank", class = "btn btn-primary", 
                                   style = "background-color: #00a65a; color: white; border: none; padding: 8px 16px; border-radius: 4px; text-decoration: none;",
                                   "View Full Bio")
                             )
                         )
                  )
                ),
                
                # Data Science Section
                fluidRow(
                  column(12, h3("Data Science", style = "color: #2c3e50; border-bottom: 2px solid #00a65a; padding-bottom: 10px; margin-top: 40px;"))
                ),
                fluidRow(
                  column(4, #copy this entire column to add another member
                         div(class = "team-card",
                             img(src = "images/AV.png", style = "width: 150px; height: 150px; object-fit: cover; border-radius: 50%; border: 4px solid #f1f8ff; margin: 0 auto 20px; display: block;"),
                             div(style = "text-align: center;",
                                 h4("André De Vito", style = "color: #2c3e50; font-weight: 700; margin-top: 15px;"),
                                 h5("Lead Developer", style = "color: #3498db; font-weight: 600; margin-bottom: 15px;"),
                                 p("Master in Applied Data Science, Data Visualization Specialist", style = "color: #34495e; line-height: 1.6;"),
                                 a(href = "https://www.linkedin.com/in/andre-de-vito/", 
                                   target = "_blank", class = "btn btn-primary", 
                                   style = "background-color: #00a65a; color: white; border: none; padding: 8px 16px; border-radius: 4px; text-decoration: none;",
                                   "LinkedIn Profile")
                             )
                         )
                  )
                ),
                
                # Past Members Section
                fluidRow(
                  column(12, h3("Past Members", style = "color: #2c3e50; border-bottom: 2px solid #00a65a; padding-bottom: 10px; margin-top: 40px;"))
                ),
                fluidRow(
                  column(4,
                         div(class = "team-card", style = "background-color: #f8f9fa; border: 1px solid #eee;",
                             img(src = "images/NC.png", style = "width: 150px; height: 150px; object-fit: cover; border-radius: 50%; border: 4px solid #f1f8ff; margin: 0 auto 20px; display: block;",
                                 alt = "Nicki Cartlidge profile photo"),
                             div(style = "text-align: center;",
                                 h4("Nicki Cartlidge", style = "color: #2c3e50; font-weight: 700; margin-top: 15px;"),
                                 h5("Past Developer", style = "color: #7f8c8d; font-weight: 600; margin-bottom: 15px;"),
                                 p("Master in Applied Data Science, Survey Data Processing Specialist", style = "color: #34495e; line-height: 1.6;"),
                                 a(href = "https://www.linkedin.com/in/nicki-cartlidge-571b3b51/", 
                                   target = "_blank", class = "btn btn-primary", 
                                   style = "background-color: #7f8c8d; color: white; border: none; padding: 8px 16px; border-radius: 4px; text-decoration: none;",
                                   "LinkedIn Profile")
                             )
                         )
                  )
                ),
                
                # Project Information
                fluidRow(
                  column(12,
                         div(style = "margin-top: 40px; padding: 20px; background-color: #f8f9fa; border-radius: 8px;",
                             h4("Project Information", style = "color: #2c3e50;"),
                             p(HTML("<strong>World Values Survey Explorer</strong> Version 1.0.0")),
                             p("Last Updated: June 2025"),
                             p("This application was developed using R Shiny.")
                         )
                  )
                )
        )
        ###################### ABOUT #######################

        
      ) #end tabItems
    ) #end dashboardBody
    
  ) #end dashboardPage
  
) #end fluidPage
) #end Server
#' Code based on: https://shiny.posit.co/r/gallery/life-sciences/biodiversity-national-parks/
#' WVS data source: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp

#' Some things still needing done:
#' - Add basic table where users can look through the participant and country
#' level data
#' 
#' - Add world map plots for the country level data page
#' 
#' - Consider adding a section to the website where users can test whether
#' two countries differ in respect to one variable
#' 
#' - More informative variable names needed. Probably need to add a column to
#' "WVS_Dataset/Codebook manual coded index.xlsx" with display names?
#' 
#' - Also there needs to be more information on the variables on the website.
#' Potentially a page with a list or something like that?
#' 
#' - Statistics and visualizations need to developed to matched the particular 
#' kinds of variables being compared. 
#' 
#' - Make things look and work better
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
#'      

###########
# LOAD UI #
###########

shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  # load google analytics script
  tags$head(includeScript("www/google-analytics.js")),
  
  
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }", # remove shiny "red" warning messages on GUI
    ".shiny-output-error:before { visibility: hidden; }",
    HTML(
      "
      .blue-button {
                    background-color: #33aaff !important;
                    color: white !important;
                    border-color: #33aaff !important;
      }
      
      .green-button {
                    background-color: #00a65a !important;
                    color: white !important;
                    border-color: #00a65a !important;
      }
      
      /* Inactive pills */
      .nav-pills > li > a {
        background-color: #f0f0f0;
        color: #333;
        border: 2px solid #ccc;
        border-radius: 5px;
        margin-right: 5px;
      }

      /* Active pill */
      .nav-pills > li.active > a,
      .nav-pills > li.active > a:focus,
      .nav-pills > li.active > a:hover {
        background-color: #00a65a; /* Green */
        border-radius: 5px;
        border: 2px solid #00a65a;
        color: white;
      }
      
      .dropdown-menu {
        z-index: 1050 !important;  /* Higher than Leaflet controls */
      }
    "
    )
  ),
  
  dashboardPage(
    
    skin = "green",
    
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
                         "Dataset Documentation",
                         tabName = "dummy",
                         icon = icon('info-sign', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "Master Survey Questionnaire",
                           tabName = "surveyview",
                           icon = icon("comment", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Questionnaire Codebook",
                           tabName = "codebookview",
                           icon = icon("book", lib = "glyphicon")
                         )
                       ),
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
                         ),
                         
                         menuSubItem(
                           "Missing Data Visualizations",
                           tabName = "vis_miss",
                           icon = icon("warning-sign", lib = "glyphicon")
                         )
                       ),
                       ################## RAW DATA TABLES #################
                       
                       
                       ################ SUMMARY STATISTICS ################
                       menuItem(
                         "Summary Statistics",
                         tabName = "dummy",
                         icon = icon('info-sign', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "Individual-level",
                           tabName = "individualStats",
                           # tabName = "withinCountry",
                           icon = icon("user", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Country-level",
                           tabName = "betweenCountries",
                           icon = icon("globe", lib = "glyphicon")
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
                           "Bar Graph",
                           tabName = "barGraph",
                           icon = icon("stats", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Scatterplot - Participants",
                           tabName = "scatterParticipants",
                           icon = icon("equalizer", lib = "glyphicon")
                         ),
                         menuSubItem(
                           "Scatterplot - Countries",
                           tabName = "scatterCountries",
                           icon = icon("equalizer", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Correlation", 
                           tabName = "correlationView",
                           icon = icon("equalizer", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Heatmap",
                           tabName = "map_view",
                           icon = icon("fire", lib = "glyphicon")
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
                           "Kendall's Rank Correlations",
                           tabName = "kendallTab",
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
        tabItem(tabName = "surveyview",
                fluidRow(column(12, shinycssloaders::withSpinner(uiOutput("surveyview"))))
        ),
        
        tabItem(tabName = "codebookview",
                fluidRow(column(12, shinycssloaders::withSpinner(uiOutput("codebookview"))))
        ),
        ############## VARIABLE DOCUMENTATION ##############
        
        
        ################## RAW DATA TABLES #################
        tabItem(tabName = "EDA_indiv",
                includeMarkdown("www/DTable.md"),
                tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                fluidRow(column(12, DT::dataTableOutput(outputId = "Table_indiv"))),

        ),
        tabItem(tabName = "EDA_country",
                includeMarkdown("www/DTable.md"),
                tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                fluidRow(column(12, DT::dataTableOutput(outputId = "Table_country"))),
        ),
        
        tabItem(tabName = "vis_miss",
                includeMarkdown("www/miss_vars.md"),
                
                tabsetPanel(id = "MissingViews", type = "pills",
                            tabPanel("Top 15 Missing", value = "top_miss",
                                     fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Top_miss_indiv", height = "60vh")))),
                                     fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Top_miss_country", height = "60vh"))))
                            ),
                            
                            tabPanel("Missing data from Individual Responses", value = "indiv_miss",
                                     checkboxInput(inputId = "cluster_indiv",
                                                   label = "Cluster missingness",
                                                   value = FALSE),
                                     fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Indiv_missing_with_ratio", height = "85vh"))))
                            ),
                            
                            tabPanel("Missing data from Countries responses", value = "countries_miss",
                                     checkboxInput(inputId = "cluster_ctry",
                                                   label = "Cluster missingness",
                                                   value = FALSE),
                                     fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Missing", height = "85vh"))))
                            )
                )
        ),
        ################## RAW DATA TABLES #################
        
        
        ################ SUMMARY STATISTICS ################
        tabItem(tabName = "individualStats",
                fluidRow(
                  column(4, uiOutput("individualStats_selectCountry")),
                  column(8, uiOutput("individualStats_selectQuestion"))
                ),
                fluidRow(
                  column(6, tableOutput("statsSelectedQuestion")),
                  column(6, tableOutput("individualStats_totalObs"))
                ),
                tags$br(),
                tags$br(),
                fluidRow(
                  column(12, DT::dataTableOutput(outputId = "datatable_filtered_country"))
                )
        ),
        
        tabItem(tabName ="withinCountry",
                fluidRow(
                  column(3, uiOutput("wc_country_sel")),
                  column(9, shinycssloaders::withSpinner(tableOutput("c_total_obs")))
                ),
                tags$br(),
                fluidRow(
                  column(6, uiOutput("wc_qA")),
                  column(6, uiOutput("wc_qB"))
                ),
                fluidRow(
                  column(6, tableOutput("stats_wc_qA")),
                  column(6, tableOutput("stats_wc_qB"))
                ),
                fluidRow( # frequency plots
                  column(6, shinycssloaders::withSpinner(plotOutput("plot_wc_qA_levels"))),
                  column(6, shinycssloaders::withSpinner(plotOutput("plot_wc_qB_levels")))
                ),
                tags$br(),
                # fluidRow( # proportion plots
                #   column(6, shinycssloaders::withSpinner(plotOutput("plot_wc_qA_prop"))),
                #   column(6, shinycssloaders::withSpinner(plotOutput("plot_wc_qB_prop")))
                # ),
                tags$br(), # kendall test plots
                fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("test_output_plot")))),
                fluidRow(column(12, shinycssloaders::withSpinner(tableOutput("test_output_table")))),
                fluidRow(column(12, textOutput("test_output_stats")))
        ),
        
        tabItem(tabName = "betweenCountries",
                fluidRow(column(6, uiOutput("bc_question"))),
                fluidRow(
                  column(3, uiOutput("bc_countryA")),
                  column(3, shinycssloaders::withSpinner(tableOutput("cA_total_obs"))),
                  column(3, uiOutput("bc_countryB")),
                  column(3, shinycssloaders::withSpinner(tableOutput("cB_total_obs")))
                ),
                fluidRow(
                  column(6, shinycssloaders::withSpinner(tableOutput("stats_bc_cA"))),
                  column(6, shinycssloaders::withSpinner(tableOutput("stats_bc_cB")))
                ),
                fluidRow(
                  column(6, shinycssloaders::withSpinner(plotOutput("plot_bc_qcA_levels"))),
                  column(6, shinycssloaders::withSpinner(plotOutput("plot_bc_qcB_levels")))
                ),
                tags$br(),
                fluidRow(
                  column(6, shinycssloaders::withSpinner(plotOutput("plot_bc_qA_prop"))),
                  column(6, shinycssloaders::withSpinner(plotOutput("plot_bc_qB_prop")))
                )
        ),
        ################ SUMMARY STATISTICS ################
        
        
        ################## VISUALISATIONS ##################
        tabItem(tabName = "barGraph",
                fluidRow(
                  box(width = 3, title = "Controls", status = "primary",
                      selectizeInput(
                        inputId = "bar_question",
                        label = "Select Question:",
                        choices = grouped_questions,
                        selected = grouped_questions[[1]][1],
                        options = list(
                          placeholder = 'Please select a question',
                          onInitialize = I('function() { this.setValue(""); }')
                        )
                      ),
                      pickerInput(
                        inputId = "bar_countries",
                        label = "Select Countries:",
                        choices = picker_country_list,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE),
                        selected = c("NZL", "USA")
                      ),
                      radioGroupButtons(
                        inputId = "bar_type",
                        label = "Display Type:",
                        choices = c("Count", "Percentage"),
                        selected = "Percentage",
                        status = "success"
                      ),
                      actionButton("bar_update", "Update Plot", class = "green-button")
                  ),
                  box(width = 9, title = "Response Distribution", status = "primary",
                      shinycssloaders::withSpinner(plotlyOutput("bar_plot", height = "600px"))
                  )
                )
        ),
        
        tabItem(tabName = "scatterParticipants",
                fluidRow(
                  box(width = 3, title = "Controls", status = "primary",
                      selectizeInput(
                        inputId = "scatter_x",
                        label = "X-axis Question:",
                        choices = grouped_questions,
                        selected = grouped_questions[[1]][1]
                      ),
                      selectizeInput(
                        inputId = "scatter_y",
                        label = "Y-axis Question:",
                        choices = grouped_questions,
                        selected = grouped_questions[[1]][2]
                      ),
                      pickerInput(
                        inputId = "scatter_countries",
                        label = "Select Countries:",
                        choices = picker_country_list,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE),
                        selected = c("NZL", "USA")
                      ),
                      sliderInput(
                        "scatter_sample",
                        "Sample Size:",
                        min = 100, max = 5000, value = 1000, step = 100
                      ),
                      actionButton("scatter_update", "Update Plot", class = "green-button")
                  ),
                  box(width = 9, title = "Participant Scatterplot", status = "primary",
                      shinycssloaders::withSpinner(plotlyOutput("scatter_plot", height = "600px"))
                  )
                )
        ),
        
        tabItem(tabName = "correlationView",
                fluidRow(
                  box(width = 3, title = "Controls", status = "primary",
                      pickerInput(
                        inputId = "corr_questions",
                        label = "Select Questions:",
                        choices = grouped_questions,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE),
                        selected = grouped_questions[[1]][1:5]
                      ),
                      pickerInput(
                        inputId = "corr_countries",
                        label = "Select Countries:",
                        choices = picker_country_list,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE),
                        selected = names(picker_country_list) # All countries
                      ),
                      radioGroupButtons(
                        inputId = "corr_method",
                        label = "Correlation Method:",
                        choices = c("Pearson", "Spearman"),
                        selected = "Pearson",
                        status = "success"
                      ),
                      actionButton("corr_update", "Update Plot", class = "green-button")
                  ),
                  box(width = 9, title = "Correlation Matrix", status = "primary",
                      shinycssloaders::withSpinner(plotOutput("corr_plot", height = "600px"))
                  )
                )
        ),
        
        tabItem(tabName = "map_view",
                fluidRow(
                  column(12,
                         box(width = 3, title = "Controls", status = "primary",
                             selectizeInput(
                               inputId = "map_question",
                               label = "Select Question:",
                               choices = grouped_questions,
                               selected = grouped_questions[[1]][1]
                             ),
                             radioGroupButtons(
                               inputId = "map_metric",
                               label = "Display Metric:",
                               choices = c("Mean", "Median", "Mode"),
                               selected = "Mean",
                               status = "success"
                             ),
                             actionButton("map_update", "Update Map", class = "green-button")
                         )
                  )
                ),
                fluidRow(
                  column(12, leafletOutput("map", height = "700px", width = "100%"))
                )
        ),
        
        # tabItem(tabName = "barGraph", #placeholder
        #         
        # ),
        # tabItem(tabName = "scatterParticipants",
        #         # fluidRow(column(12, div(style = "float:right",
        #         #                         actionButton("next1", "Next", class = "green-button"))))
        # ),
        # 
        # tabItem(tabName = "scatterCountries",
        #         # fluidRow(column(12, div(style = "float:right",
        #         #                         actionButton("next1", "Next", class = "green-button"))))
        # ),
        # tabItem(tabName = "heatmap",
        #         # fluidRow(column(12, div(style = "float:right",
        #         #                         actionButton("next1", "Next", class = "green-button"))))
        # ),
        # tabItem(tabName = "map_view",
        #         fluidRow(column(12, uiOutput("countrySelect"))),
        #         fluidRow(column(12, leafletOutput("map", height = "60vh", width = "100%"))),
        #         # fluidRow(column(12, div(style = "float:right",
        #         #                         actionButton("next1", "Next", class = "green-button"))))
        # ),
        ################## VISUALISATIONS ##################
        
        
        ###################### MODELS ######################
        tabItem(tabName = "kendallTab",
                fluidRow(
                  box(width = 3, title = "Controls", status = "primary",
                      selectizeInput(
                        inputId = "kendall_var1",
                        label = "Select Variable 1:",
                        choices = grouped_questions,
                        selected = grouped_questions[[1]][1]
                      ),
                      selectizeInput(
                        inputId = "kendall_var2",
                        label = "Select Variable 2:",
                        choices = grouped_questions,
                        selected = grouped_questions[[1]][2]
                      ),
                      pickerInput(
                        inputId = "kendall_countries",
                        label = "Select Countries:",
                        choices = picker_country_list,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE),
                        selected = c("NZL", "USA")
                      ),
                      sliderInput(
                        "kendall_sample",
                        "Sample Size:",
                        min = 100, max = 5000, value = 1000, step = 100
                      ),
                      actionButton("kendall_run", "Run Analysis", class = "green-button")
                  ),
                  box(width = 9, title = "Kendall's Rank Correlation", status = "primary",
                      tabsetPanel(
                        tabPanel("Results",
                                 verbatimTextOutput("kendall_results"),
                                 plotlyOutput("kendall_plot")),
                        tabPanel("Data",
                                 DTOutput("kendall_data"))
                      )
                  )
                )
        ),
        
        tabItem(tabName = "anovaTab",
                fluidRow(
                  box(width = 3, title = "Controls", status = "primary",
                      selectizeInput(
                        inputId = "anova_var",
                        label = "Select Variable:",
                        choices = grouped_questions,
                        selected = grouped_questions[[1]][1]
                      ),
                      pickerInput(
                        inputId = "anova_countries",
                        label = "Select Countries:",
                        choices = picker_country_list,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE),
                        selected = c("NZL", "USA", "GBR", "AUS")
                      ),
                      sliderInput(
                        "anova_sample",
                        "Sample Size:",
                        min = 100, max = 5000, value = 1000, step = 100
                      ),
                      actionButton("anova_run", "Run Analysis", class = "green-button")
                  ),
                  box(width = 9, title = "ANOVA Results", status = "primary",
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
                fluidRow(
                  box(width = 3, title = "Controls", status = "primary",
                      selectizeInput(
                        inputId = "regression_dep",
                        label = "Dependent Variable:",
                        choices = grouped_questions,
                        selected = grouped_questions[[1]][1]
                      ),
                      selectizeInput(
                        inputId = "regression_indep",
                        label = "Independent Variables:",
                        choices = grouped_questions,
                        multiple = TRUE,
                        selected = grouped_questions[[1]][2:3]
                      ),
                      pickerInput(
                        inputId = "regression_country",
                        label = "Select Country:",
                        choices = picker_country_list,
                        multiple = FALSE,
                        selected = "NZL"
                      ),
                      sliderInput(
                        "regression_sample",
                        "Sample Size:",
                        min = 100, max = 5000, value = 1000, step = 100
                      ),
                      actionButton("regression_run", "Run Regression", class = "green-button")
                  ),
                  box(width = 9, title = "Regression Analysis", status = "primary",
                      tabsetPanel(
                        tabPanel("Model Summary",
                                 verbatimTextOutput("regression_summary")),
                        tabPanel("Diagnostics",
                                 plotOutput("regression_diag")),
                        tabPanel("Coefficients",
                                 DTOutput("regression_coef")),
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
                includeMarkdown("www/team.md")
        )
        ###################### ABOUT #######################

        
      ) #end tabItems
    ) # end dashboardBody
    
  ) #end dashboardPage
  
) #end fluidPage
) #end Server
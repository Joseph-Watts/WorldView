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
    
    dashboardHeader(title = "WorldView", titleWidth = 300),
    
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       HTML(
                         paste0(
                           "<br>",
                           "<a href='https://www.worldvaluessurvey.org' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='images/WorldView_Logo.png' width = '186'></a>",
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
                       
                       ############## VARIABLE DOCUMENTATION ##############
                       
                       
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
                           "Histogram",
                           tabName = "histogramView",
                           icon = icon("signal")
                         ),
                         
                         menuSubItem("World Map", 
                                     tabName = "geo_viz_map", 
                                  icon = icon("map-marker")
                         ),
                         
                         menuSubItem(
                                    "Phylogeny (Advanced)", 
                                     tabName = "phylo_viz_tree", 
                                     icon = icon("tree-conifer", lib = "glyphicon")
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
                         ),
                         
                         menuSubItem(
                           "PhyloLM (Advanced)",
                           tabName = "models_phylo_lm",
                           icon = icon("fullscreen", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "PhyloGLM (Advanced)",
                           tabName = "models_phylo_glm",
                           icon = icon("random", lib = "glyphicon")
                         )
                         
                       ),
                       ###################### MODELS ######################
                      
                       
                       ######################## FAQ #######################
                       # menuItem(
                       #   "FAQ",
                       #   tabName = "faq",
                       #   icon = icon("question-sign", lib = "glyphicon")
                       # ),
                       ######################## FAQ #######################
                       
                       
                       ###################### ABOUT #######################
                       # menuItem(
                       #   "About the Team",
                       #   tabName = "team",
                       #   icon = icon("user", lib = "glyphicon")
                       # )
                       ###################### ABOUT #######################
                       
                       ###################### ABOUT #######################
                       menuItem(
                         "About WorldView",
                         tabName = "about",
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
        ############## VARIABLE DOCUMENTATION ##############
        
      
        
        ################ SUMMARY STATISTICS ################
        # Univariate Stats tab
        tabItem(tabName = "univariateStats",
                includeMarkdown("www/instructions/univariate_instruction.md"),
                fluidRow(shinydashboard::box(width = 4, status = "primary",
                                             selectizeInput(inputId = "univar_question",
                                                            label = "Select Question:",
                                                            choices = grouped_questions,
                                                            selected = grouped_questions[[1]][1]),
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
                                                            choices = grouped_questions,
                                                            selected = grouped_questions[[1]][1]
                                                            ),
                                             selectizeInput(inputId = "bivariate_var2",
                                                            label = "Select Variable 2:",
                                                            choices = grouped_questions,
                                                            selected = grouped_questions[[1]][2]
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
                        choices = grouped_questions,
                        selected = grouped_questions[[1]][1],
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
        
        tabItem(tabName = "histogramView",
                includeMarkdown("www/instructions/histogram_instruction.md"),
                fluidRow(
                  shinydashboard::box(
                    width = 3,
                    status = "primary",
                    selectizeInput(
                      inputId = "hist_question",
                      label = "Select Question:",
                      choices = grouped_questions,
                      selected = grouped_questions[[1]][1],
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
        
        tabItem(tabName = "geo_viz_map",
                geo_viz_map_ui("geo_viz_map")),
        
        tabItem(tabName = "phylo_viz_tree",
                phylo_viz_tree_ui("phylo_viz_tree")),
        
        ################## VISUALISATIONS ##################
        
        
        ###################### MODELS ######################
        tabItem(tabName = "corrModelTab",
                includeMarkdown("www/instructions/corrModel_instruction.md"),
                fluidRow(
                  shinydashboard::box(width = 3, status = "primary",
                      selectizeInput(
                        inputId = "corr_model_var1",
                        label = "Select Variable 1:",
                        choices = grouped_questions,
                        selected = grouped_questions[[1]][1]
                      ),
                      selectizeInput(
                        inputId = "corr_model_var2",
                        label = "Select Variable 2:",
                        choices = grouped_questions,
                        selected = grouped_questions[[1]][2]
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
                      actionButton("corr_model_run", 
                                   "Run Analysis", 
                                   class = "green-button")
                  ),
                  shinydashboard::box(width = 9, 
                                      title = "Correlation", 
                                      status = "primary",
                      tabsetPanel(
                        tabPanel("Results",
                                 verbatimTextOutput("corr_mod_results"))#,
                                 # plotlyOutput("kendall_plot")),
                        # tabPanel("Data",
                        #          DTOutput("corr_mod_data"))
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
                        choices = grouped_questions,
                        selected = grouped_questions[[1]][1]
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
                        choices = grouped_questions,
                        selected = grouped_questions[[1]][1]
                      ),
                      pickerInput(
                        inputId = "regression_indep",
                        label = "Independent Variables:",
                        choices = grouped_questions,
                        multiple = TRUE,
                        selected = grouped_questions[[1]][2:3],
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
                      actionButton("regression_run", 
                                   "Run Regression", 
                                   class = "green-button")
                  ),
                  shinydashboard::box(width = 9, 
                                      title = "Regression Analysis", 
                                      status = "primary",
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
        
        tabItem(tabName = "models_phylo_lm",
                models_phylo_lm_ui("models_phylo_lm")),
        
        tabItem(tabName = "models_phylo_glm",
                models_phylo_glm_ui("models_phylo_glm")),
        
        ###################### MODELS ######################
        
 
        ######################## ABOUT #######################
        tabItem(tabName = "about",
                includeMarkdown("www/about.md")
        )
        ######################## ABOUT #######################
        
        
      ) #end tabItems
    ) #end dashboardBody
    
  ) #end dashboardPage
  
) #end fluidPage
) #end Server

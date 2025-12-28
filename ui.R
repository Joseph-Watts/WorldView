#' Code based on: https://shiny.posit.co/r/gallery/life-sciences/biodiversity-national-parks/
#' WVS data source: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp

#' Some things still needing done:
#' - Add world map plots for the country level data page
#' 
#' -----------
#'
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
                       # Raw Data tab
                       menuItem(
                         "Raw Data Tables",
                         tabName = "RawData",    
                         icon = icon("list", lib = "glyphicon"),
                         startExpanded = FALSE
                       ),
                       
                       #Missing data tab
                       menuItem(
                         "Missing Data Visualizations",
                         tabName = "vis_miss",
                         icon = icon("warning-sign", lib = "glyphicon")
                       ),
                       #),
                       ##################RAW DATA TABLES END #################
                       
                 
                       ### NEW SUMMARY STATTISTICS START ######
                       menuItem(
                         "Summary Statistics NEW",
                         #tabName = "summary_stats_NEW",
                         icon = icon('info-sign', lib = "glyphicon"),
                         
                       menuSubItem(
                         "Univariate Statistics (NEW)",
                         tabName = "univariateStats_new",
                         icon = icon("chart-bar")
                       ),
                       
                       menuSubItem(
                         "Bivariate Statistics (NEW)",
                         tabName = "bivariateStats_new",
                         icon = icon("project-diagram")
                       )
                       ),
                       
                       ################ NEW SUMMARY STATISTICS END ################
                       
                       
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
                       
                       
                       ################## VISUALISATIONS NEW START ##########
                       menuItem(
                         "Visualisations NEW",
                         tabName = "scatterParticipants",
                         icon = icon('move', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "Bar Chart",
                           tabName = "barChart",
                           icon = icon("stats", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Scatterplot",   #text
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
                       
                       ################## VISUALISATIONS NEW END ##################
                       
                       
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
  
      
###DASHBOARDBODY#########    
###DASHBOARDBODY#########    
###DASHBOARDBODY#########    

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



        #################################################################
        #                HDR WORLD MAP (Overview) - END
        #################################################################

        
        ############## VARIABLE DOCUMENTATION ##############
        tabItem(tabName = "surveyview",
                fluidRow(column(12, shinycssloaders::withSpinner(uiOutput("surveyview"))))
        ),
        
        tabItem(tabName = "codebookview",
                fluidRow(column(12, shinycssloaders::withSpinner(uiOutput("codebookview"))))
        ),
        ############## VARIABLE DOCUMENTATION ##############
        
        
        ################## RAW DATA TABLES #################
 
       tabItem(
         tabName = "RawData",
         
         h2("Raw Data"),
         
         tags$p(
           "This section provides access to raw datasets at different levels of analysis. ",
           "Use the tabs below to switch between participant-level and country-level data."
         ),
         
         br(),
         
         tabsetPanel(
           id = "raw_data_subtab",
           
           # =========================================================
           # Participant-level raw data
           # =========================================================
           tabPanel(
             title = "Participant-level",
             includeMarkdown("www/instructions/DTable_ind.md"),
             tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
             
             fluidRow(
               column(12, uiOutput("raw_selectCountry"))
             ),
             
             fluidRow(
               column(12, DTOutput("raw_filtered_country"))
             )
           ),
           
           # =========================================================
           # Country-level raw data (HDR + WVS)
           # =========================================================
           tabPanel(
             title = "Country-level",
             includeMarkdown("www/instructions/DTable_ctry.md"),
             # ----------------------------------------------------
             # Country selection dropdown
             # ----------------------------------------------------
             fluidRow(
               column(
                 4,
                 selectizeInput(
                   inputId = "country_select",
                   label   = "Select country:",
                   choices = NULL,
                   multiple = TRUE,
                   options = list(
                     placeholder = "Type to search countries...",
                     maxItems = NULL,
                     create = FALSE
                   )
                 )
               )
             ),
             
             br(),
             
             # ----------------------------------------------------
             # Country-level data table
             # ----------------------------------------------------
             fluidRow(
               column(12, DTOutput("Table_country"))
             ),
             
             br(),
             hr(),
             br(),
             
             # ----------------------------------------------------
             # Variable dictionary
             # ----------------------------------------------------
             h3("Variable dictionary"),
             
             tags$p(
               "Use the controls below to search for variable definitions from the ",
               "HDRs and WVS7 datasets."
             ),
             
             fluidRow(
               column(
                 4,
                 radioButtons(
                   inputId = "var_source",
                   label   = "Filter by source:",
                   choices = c(
                     "All sources" = "ALL",
                     "Human Development Reports (HDR)" = "HDR",
                     "World Values Survey Wave 7 (WVS7)" = "WVS7"
                   ),
                   selected = "ALL"
                 )
               )
             ),
             
             br(),
             
             fluidRow(
               column(4, uiOutput("source_refinement_ui"))
             ),
             
             br(),
             
             fluidRow(
               column(
                 4,
                 selectizeInput(
                   inputId = "var_lookup",
                   label   = "Select variable:",
                   choices = NULL,
                   multiple = FALSE,
                   options = list(
                     placeholder = "Type a variable name…",
                     create = FALSE
                   )
                 )
               )
             ),
             
             br(),
             
             fluidRow(
               column(12, uiOutput("var_definition"))
             )
           )
         )
       ),
       
       ################## RAW DATA TABLES #################
       
       ################## MISSING DATA  ###################
        tabItem(tabName = "vis_miss",
                includeMarkdown("www/instructions/miss_vars.md"),
                
                tabsetPanel(id = "MissingViews", type = "pills",
                            tabPanel("Top 15 Missing", value = "top_miss",
                                     fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Top_miss_indiv", height = "60vh")))),
                                     fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Top_miss_country", height = "60vh"))))
                            ),
                            
                            tabPanel("Missing data from Individual Responses", value = "indiv_miss",
                                     fluidRow(column(3,
                                       dropdownButton(
                                         inputId = "indiv_miss_adv",
                                         label = "Advanced Options",
                                         icon = icon("sliders"),
                                         status = "success",
                                         circle = FALSE,
                                         materialSwitch(
                                           inputId = "cluster_indiv",
                                           label = "Cluster missingness",
                                           status = "success"
                                         ),
                                         materialSwitch(
                                           inputId = "sort_indiv",
                                           label = "Sort columns by missingness",
                                           status = "success"
                                         )
                                       )
                                     )),
                                     fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Indiv_missing_with_ratio", height = "85vh"))))
                            ),
                            
                            tabPanel("Missing data from Countries responses", value = "countries_miss",
                                     fluidRow(column(3,
                                       dropdownButton(
                                         inputId = "ctry_miss_adv",
                                         label = "Advanced Options",
                                         icon = icon("sliders"),
                                         status = "success",
                                         circle = FALSE,
                                         materialSwitch(
                                           inputId = "cluster_ctry",
                                           label = "Cluster missingness",
                                           status = "success"
                                         ),
                                         materialSwitch(
                                           inputId = "sort_ctry",
                                           label = "Sort columns by missingness",
                                           status = "success"
                                         )
                                       )
                                     )),
                                     fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Missing", height = "85vh"))))
                            )
                )
        ),

       
        ################ SUMMARY STATISTICS NEW START ################
        # ===========================================================
        # UNIVARIATE STATS
        # ===========================================================
        tabItem(
          tabName = "univariateStats_new",
          
          # ----------------------------------------------------
          # Page header / instructions
          # ----------------------------------------------------
          #includeMarkdown("www/instructions/univariate_instruction_ind.md"),
          
          br(),
          
          # ----------------------------------------------------
          # MAIN PANEL TABS 
          # ----------------------------------------------------
          tabsetPanel(
            id = "univariate_level_tabs",
            type = "tabs",
            
            # ==========================================
            # Individual-level tab
            # ==========================================
            tabPanel(
              title = "Individual-level",
              includeMarkdown("www/instructions/univariate_instruction_ind.md"),
              fluidRow(
                column(
                  4,
                  h4("Select Question"),
                  selectInput(
                  inputId= "uni_question_indiv",
                    label= NULL,
                    choices = NULL
                  ),
                  br(),
                  h4("Select Countries"),
                  selectizeInput(
                    inputId= "uni_countries_indiv",
                    label= NULL,
                    choices = NULL,
                    multiple = TRUE
                  )
                ),
                
                column(
                  8,
                  h3("Individual-level Summary"),
                  uiOutput("univariate_indiv_tabs")  # Selected sample + country tabs
                )
              )
            ),
            
            # ==========================================
            # Country-level tab
            # ==========================================
            tabPanel(
              title = "Country-level",
              includeMarkdown("www/instructions/univariate_instruction_ctry.md"),

              fluidRow(
                column(
                  4,
                  # -------------------------------
                  # Variable dictionary controls
                  # -------------------------------
                  #h4("Select Variable"),
                  radioButtons(
                    inputId = "country_var_source_new",
                    label   = "Filter by source:",
                    choices = c(
                      "All sources" = "ALL",
                      "Human Development Reports (HDR)" = "HDR",
                      "World Values Survey Wave 7 (WVS7)" = "WVS7"
                    ),
                    selected = "ALL"
                  ),
                  
                  uiOutput("country_source_refinement_ui_new"),
                  
                  h4("Select Variable"),
                  selectizeInput(
                    inputId = "uni_question_country",
                    label   = NULL,
                    choices = NULL,
                    multiple = FALSE
                  ),
                  
                  h4("Select Countries"),
                  selectizeInput(
                    inputId = "uni_countries_country",
                    label   = NULL,
                    choices = NULL,
                    multiple = TRUE
                  )
                ),
                
                column(
                  8,
                  h3("Country-level Summary"),
                  #uiOutput("univariate_country_summary")
                  uiOutput("univariate_country_tabs")
                )
              )
            ),
            

            # ==========================================
            # group -level tab
            # ==========================================
            tabPanel(
              title = "Group-level",
              includeMarkdown("www/instructions/univariate_instruction_grp.md"),
              fluidRow(
                
                # ================= LEFT COLUMN: CONTROLS =================
                column(
                  4,
                  
                  h4("Select Group Type"),
                  radioButtons(
                    inputId = "group_type",
                    label   = NULL,
                    choices = c(
                      "All groups"                = "all",
                      "Human Development Groups" = "groups",
                      "Regions"                  = "regions",
                      "Special Groups"           = "special"
                    ),
                    selected = "all"
                  ),
                  
                  br(),
                  
                  # ------------------------------------------
                  # Select Group(s)  (HDI / Regions / Special)
                  # ------------------------------------------
                  # Select group(s) (depends on group type)
                  uiOutput("group_selector_ui"),
                  
                  br(),
                  
                  # Select HDR table (always visible)
                  uiOutput("group_table_ui"),
                
                  br(),
                  
                  #uiOutput("group_indicator_ui"),
                  
                  br(),
                  
                ),
                
                
                # ================= RIGHT COLUMN: OUTPUT =================
                column(
                  8,
                  h3("Group-level Summary"),
                  DT::DTOutput("group_level_summary")
                )
              )
            )  #end tabPanel group-level
            
          )    #end tabsetPanel
        ),     #end tabItem "univariateStats_new"


      # ===========================================================
      # BIVARIATE STATS
      # ===========================================================
        tabItem(
          tabName = "bivariateStats_new",
          
          # ----------------------------------------------------
          # Page header / instructions
          # ----------------------------------------------------
          #includeMarkdown("www/instructions/bivariate_instruction.md"),
          br(),
          
          # ----------------------------------------------------
          # MAIN PANEL TABS
          # ----------------------------------------------------
          tabsetPanel(
            id   = "bivariate_level_tabs",
            type = "tabs",
            
            # ==========================================
            # Individual-level tab
            # ==========================================
            tabPanel(
              title = "Individual-level",
              includeMarkdown("www/instructions/bivariate_instruction_ind.md"),
              fluidRow(
                shinydashboard::box(
                  width = 4,
                  status = "primary",

                  selectizeInput(
                    inputId = "bivariate_var1",
                    label   = "Select Variable 1:",
                    choices = grouped_minus_ignored,
                    selected = grouped_minus_ignored[[1]][1]
                  ),

                  selectizeInput(
                    inputId = "bivariate_var2",
                    label   = "Select Variable 2:",
                    choices = grouped_minus_ignored,
                    selected = grouped_minus_ignored[[1]][2]
                  ),

                  pickerInput(
                    inputId = "bivariate_countries",
                    label   = "Select Countries:",
                    choices = picker_country_list,
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `live-search` = TRUE,
                      `max-options` = 5
                    ),
                    selected = c("NZL", "AUS", "GBR")
                  ),

                  radioGroupButtons(
                    inputId = "bivariate_type",
                    label   = "Table Type:",
                    choices = c("Counts", "Row Percentages", "Column Percentages"),
                    selected = "Counts",
                    status = "success"
                  )
                ),

                shinydashboard::box(
                  width = 8,
                  title = "Bivariate Summary",
                  status = "primary",
                  shinycssloaders::withSpinner(
                    DTOutput("bivariate_table")
                  )
                )
              )
            ),  #end tabPanel individual-level
            
            
            
            
          
            # ==========================================
            # Country-level tab
            # ==========================================
            tabPanel(
              title = "Country-level",
              includeMarkdown("www/instructions/bivariate_instruction_ctry.md"),
              fluidRow(
                shinydashboard::box(
                  width = 4,
                  status = "primary",
                  
                  radioButtons(
                    inputId = "bivar_country_source1",
                    label   = "Variable 1 source:",
                    choices = c("ALL", "HDR", "WVS7"),
                    selected= "ALL",
                    inline  = TRUE
                  ),
                  
                  selectizeInput(
                    inputId = "bivar_country_var1",
                    label   = "Select Variable 1:",
                    choices = NULL
                  ),
                  
                  radioButtons(
                    inputId = "bivar_country_source2",
                    label   = "Variable 2 source:",
                    choices = c("ALL", "HDR", "WVS7"),
                    selected = "ALL",
                    inline  = TRUE
                  ),
                  
                  selectizeInput(
                    inputId = "bivar_country_var2",
                    label   = "Select Variable 2:",
                    choices = NULL
                  ),
                  
                  selectizeInput(
                    inputId = "bivar_country_countries",
                    label   = "Select Countries:",
                    choices = NULL,
                    multiple = TRUE
                  )
                ),
                
                shinydashboard::box(
                  width = 8,
                  title = "Country-level Bivariate Summary",
                  status = "primary",
                  DTOutput("bivar_country_table")
                )
              )
            ),
            
            
            # ==========================================
            # Group-level tab (Bivariate-HDR)
            # ==========================================
            tabPanel(
              title = "Group-level",
              includeMarkdown("www/instructions/bivariate_instruction_grp.md"),
              fluidRow(
                # ================= LEFT COLUMN: USER CONTROLS =================
                column(
                  4,
                  
                  # Select how groups are defined (HDI / Region / Special)
                  selectInput(
                    inputId = "bivar_group_type",
                    label   = "Group type",
                    choices = NULL
                  ),
                  
                  br(),
                  
                  # Select how groups are defined (e.g. HDI groups, Regions)
                  h4("Select Group Definition"),
                  selectInput(
                    inputId = "bivar_groups",
                    label   = NULL,
                    choices = NULL,   # populated server-side from HDR_AREA_LOOKUP
                    multiple = TRUE
                  ),
                  
                  br(),
                  
                  # First HDR variable (X)
                  h4("Select Variable 1"),
                  selectizeInput(
                    inputId = "bivar_group_var1",
                    label   = NULL,
                    choices = NULL,  # populated with HDR numeric variables
                    multiple = FALSE
                  ),
                  
                  br(),
                  
                  # Second HDR variable (Y)
                  h4("Select Variable 2"),
                  selectizeInput(
                    inputId = "bivar_group_var2",
                    label   = NULL,
                    choices = NULL,  # same variable list as Variable 1
                    multiple = FALSE
                  ),
                  
                  br(),
                  
                ),
                
                # ================= RIGHT COLUMN: OUTPUT =================
                column(
                  8,
                  
                  # Output: bivariate summary across groups
                  h3("Group-level Bivariate Summary"),
                  
                  # Short explanation to guide interpretation
                  helpText(
                    "This analysis compares two HDR indicators across all groups ",
                    "defined by the selected grouping."
                  ),
                  
                  shinycssloaders::withSpinner(
                    DT::DTOutput("bivar_group_summary")
                  )
                )
              )
            )

            
            
            
            
          )  # end tabsetPanel bivariate_level_tabs
        ), #end tabItem bivariate stats




      ################ SUMMARY STATISTICS NEW END ################





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
        
        # tabItem(tabName = "scatterParticipants",
        #         includeMarkdown("www/instructions/scatter_instruction.md"),
        #         fluidRow(
        #           shinydashboard::box(width = 3, status = "primary",
        #               selectizeInput(
        #                 inputId = "scatter_x",
        #                 label = "X-axis Question:",
        #                 choices = grouped_minus_ignored,
        #                 selected = grouped_minus_ignored[[1]][1]
        #               ),
        #               selectizeInput(
        #                 inputId = "scatter_y",
        #                 label = "Y-axis Question:",
        #                 choices = grouped_minus_ignored,
        #                 selected = grouped_minus_ignored[[1]][2]
        #               ),
        #               pickerInput(
        #                 inputId = "scatter_countries",
        #                 label = "Select Countries:",
        #                 choices = picker_country_list,
        #                 multiple = TRUE,
        #                 options = list(
        #                   `actions-box` = TRUE,
        #                   `live-search` = TRUE,
        #                   `size` = 30,
        #                   `max-options` = 5
        #                 ),
        #                 selected = c("NZL", "AUS", "GBR")
        #               ),
        #               sliderInput(
        #                 "scatter_sample",
        #                 "Sample Size (as % of data):",
        #                 min = 10, max = 100, value = 25, step = 1
        #               )
        #           ),
        #           shinydashboard::box(width = 9, title = "Participant Scatterplot", status = "primary",
        #               shinycssloaders::withSpinner(plotlyOutput("scatter_plot", height = "600px"))
        #           )
        #         )
        # ),
        
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



        ################## VISUALISATIONS NEW ##################
        tabItem(
          tabName= "scatterParticipants",

          #includeMarkdown("www/instructions/scatter_instruction.md"),
          br(),

          tabsetPanel(
            id = "scatterplot_tabs",
            type= "tabs",

        # ==========================================================
        # Individual-level scatter tab
        # ==========================================================
        tabPanel(
          title = "Individual-level",
          
          # Optional: instructions / help text
          includeMarkdown("www/instructions/scatter_instruction_ind.md"),
          
          fluidRow(
            
            # --------------------------------------------------
            # Left column: controls
            # --------------------------------------------------
            shinydashboard::box(
              width  = 3,
              status = "primary",
              
              h4("Select variables"),
              
              # --------------------------
              # X-axis variable
              # --------------------------
              selectizeInput(
                inputId  = "scatter_x",
                label    = "X-axis Question",
                choices  = grouped_minus_ignored,          
                selected = grouped_minus_ignored[[1]][1]   # default selection
              ),
              
              # --------------------------
              # Y-axis variable
              # --------------------------
              selectizeInput(
                inputId  = "scatter_y",
                label    = "Y-axis Question",
                choices  = grouped_minus_ignored,          
                selected = grouped_minus_ignored[[1]][2]
              ),
              
              # --------------------------
              # Country selection
              # --------------------------
              pickerInput(
                inputId  = "scatter_countries",
                label    = "Select Countries",
                choices  = picker_country_list,             
                multiple = TRUE,
                options  = list(
                  `actions-box` = TRUE,
                  `live-search` = TRUE,
                  `size`        = 30,
                  `max-options` = 5
                ),
                selected = c("NZL", "AUS", "GBR")
              ),
              
              # --------------------------
              # Optional: sample size
              # --------------------------
              sliderInput(
                inputId = "scatter_sample",
                label   = "Sample Size (as % of data)",
                min     = 10,
                max     = 100,
                value   = 25,
                step    = 1
              )
            ),
            
            # --------------------------------------------------
            # Right column
            # --------------------------------------------------
            shinydashboard::box(
              width  = 9,
              title  = "Participant Scatterplot",
              status = "primary",
              
              # IMPORTANT:
              # Must match renderPlotly() in the server
              shinycssloaders::withSpinner(
                plotlyOutput("scatter_plot", height = "600px")
              )
            )
          )
        ),


            # ==================================================
            # Country-level tab 
            # ==================================================
            tabPanel(
              title= "Country-level",
              includeMarkdown("www/instructions/scatter_instruction_ctry.md"),
              
              fluidRow(
                # ----------------------------------
                # Left panel: User controls
                # ----------------------------------
                shinydashboard::box(
                  width  = 4,
                  status = "primary",
              
                  # Section title
                  h4("Select variables"),
              
                  # --------------------------
                  # X-axis variable selection
                  # --------------------------
                  # Choose the data source for the X variable
                  # (ALL = combined dataset, HDR only, or WVS7 only)
                  radioButtons(
                    inputId = "scatter_country_source_x",
                    label   = "X variable source",
                    choices = c("ALL", "HDR", "WVS7"),
                    selected = "ALL",
                    inline  = TRUE
                  ),
              
                  # Dropdown for selecting the X variable
                  # Choices are populated dynamically in the server
                  selectizeInput(
                    inputId = "scatter_country_x",
                    label   = "X variable",
                    choices = NULL
                  ),
              
                  br(),  # visual spacing between X and Y controls
              
                  # ---------------------------
                  # Y-axis variable selection
                  # ---------------------------
              
                  # Choose the data source for the Y variable
                  radioButtons(
                    inputId = "scatter_country_source_y",
                    label   = "Y variable source",
                    choices = c("ALL", "HDR", "WVS7"),
                    selected = "ALL",
                    inline  = TRUE
                  ),
              
                  # Dropdown for selecting the Y variable
                  selectizeInput(
                    inputId = "scatter_country_y",
                    label   = "Y variable",
                    choices = NULL
                  ),
              
                  # -------------------
                  # Country selection
                  # -------------------
              
                  h4("Select countries"),
              
                  # Multi-select dropdown for countries
                  # Used to filter points shown in the scatter plot
                  selectizeInput(
                    inputId  = "scatter_country_countries",
                    label    = NULL,
                    choices  = NULL,
                    multiple = TRUE
                  ),
                  
                  
                  br(),
                  # --------------------------------------------------
                  # Display options (group level)
                  # --------------------------------------------------
                  tags$div(
                    style = "
                    background-color: #f7f9fc;
                    border-top: 3px solid #1f77b4;
                    #border-right: 4px solid #1f77b4;
                    padding: 8px 10px;
                    margin-top: 10px;
                  ",
                    
                  h4("Display options"),
                  # Control point transparency
                  sliderInput(
                    inputId = "scatter_country_alpha",
                    label   = "Point transparency",
                    min     = 0.1,
                    max     = 1,
                    value   = 0.8,
                    step    = 0.1
                  ),
                  
                  # Toggle country labels on the plot
                  checkboxInput(
                    inputId = "scatter_country_show_labels",
                    label   = "Show country labels",
                    value   = FALSE
                  ),
                  
                  
                  # Log-scale display options
                  checkboxInput(
                    inputId = "scatter_country_log_x",
                    label   = "Log-scale X axis",
                    value   = FALSE
                  ),
                  checkboxInput(
                    inputId = "scatter_country_log_y",
                    label   = "Log-scale Y axis",
                    value   = FALSE
                  ),
                  
                  
                  # Country point size
                  sliderInput(
                    inputId = "scatter_country_point_size",
                    label   = "Point size",
                    min     = 4,
                    max     = 20,
                    value   = 8,
                    step    = 1
                  ),
                  
                  
                  # Select point colour
                  radioButtons(
                    inputId = "scatter_country_color",
                    label   = "Point colour",
                    choices = c(
                      "Blue"   = "#1f77b4",
                      "Red"    = "#d62728",
                      "Green"  = "#2ca02c",
                      "Purple" = "#9467bd",
                      "Orange" = "#ff7f0e",
                      "Black"  = "#000000"
                    ),
                    selected = "#1f77b4",
                    inline   = TRUE
                  ),
                  
                  
                  # Select point shape
                  radioButtons(
                    inputId = "scatter_country_shape",
                    label   = "Point shape",
                    choices = c(
                      "Circle"        = "circle",
                      "Square"        = "square",
                      "Diamond"       = "diamond",
                      "Cross"         = "cross",
                      "Star"          = "star",
                      "Triangle up"   = "triangle-up",
                      "Triangle down" = "triangle-down",
                      "Hexagon"       = "hexagon2"
                    ),
                    selected = "circle",
                    inline   = TRUE
                  )
                  
                )
                ),
                
                
                # ----------------------------------
                # Right panel: Scatter plot output
                # ----------------------------------
                shinydashboard::box(
                  width  = 8,
                  title  = "Country-level Scatter Plot",
                  status = "primary",
                  
                  # Warning message (shown only if needed)
                  uiOutput("scatter_country_warning"),
              
                  # Scatter plot with loading spinner while rendering
                  shinycssloaders::withSpinner(
                    plotlyOutput("scatter_country_plot", height = "600px")
                  )
                )
              )

              

            ), #End tabPanel country-level 
            
            # ==========================================
            # Group-level scatterplot tab
            # ==========================================
            tabPanel(
              title= "Group-level",
              includeMarkdown("www/instructions/scatter_instruction_grp.md"),
              
              fluidRow(
                # --------------------------
                # Left column: controls
                # --------------------------
                shinydashboard::box(
                  width  = 4,
                  status = "primary",
                  
                  # ---- Variable selection ----
                  h4("Select source"),
                  
                  # Optional HDR table filter for SOURCE X
                  selectInput(
                    inputId = "scatter_group_source_x",
                    label   = "Source variable X",
                    choices = NULL    # populated in server
                  ),
                  
                  # # Optional HDR table filter for  VARIABLE X
                  selectizeInput(
                    inputId = "scatter_group_x",
                    label   = "X variable",
                    choices = NULL
                  ),
                  br(),
                  br(),

                  # Optional HDR table filter for SOURCE y
                  selectInput(
                    inputId = "scatter_group_source_y",
                    label   = "Source variable Y",
                    choices = NULL    # populated in server
                  ),
                  
                    # Optional HDR table filter for VARIABLE X
                  selectizeInput(
                    inputId = "scatter_group_y",
                    label   = "Y variable",
                    choices = NULL
                  ),
                  
                  br(),

                # --------------------------
                # Display options (highlighted without changing layout)
                # --------------------------
                tags$div(
                  style = "
                    background-color: #f7f9fc;
                    border-top: 3px solid #1f77b4;
                    #border-right: 4px solid #1f77b4;
                    padding: 8px 10px;
                    margin-top: 10px;
                  ",
                
                  h4("Display options", style = "margin-top:0;"),
                   #Group point size
                  radioButtons(
                    inputId  = "scatter_group_size_mode",
                    label    = "Point size",
                    choices  = c(
                      "Equal size"                    = "equal",
                      "Scaled by number of countries" = "n_countries"
                    ),
                    selected = "equal"
                  ),

                  
                  # Group labels (shown ONLY when point size = equal)
                  conditionalPanel(
                    condition = "input.scatter_group_size_mode != 'n_countries'",
                    
                    checkboxInput(
                      inputId = "scatter_group_show_labels",
                      label   = "Show group labels",
                      value   = FALSE
                    )
                  ),
                  
                  
                  # Color palette
                  helpText("Choose a palette that is easiest for you to read."),
                  radioButtons(
                    inputId = "scatter_group_palette",
                    label   = "Colour palette",
                    choices = c(
                      "Pastels "               = "default",
                      "Colour-blind friendly"  = "cbf",
                      "High contrast"          = "contrast",
                      "Greyscale"              = "grey"
                    ),
                    selected = "default"
                  ),
                  
                  
                #Group point shape
                  radioButtons(
                    inputId  = "scatter_group_shape",
                    label    = "Point shape",
                    choices  = c(
                      "Circle"        = "circle",
                      "Square"        = "square",
                      "Diamond"       = "diamond",
                      "Cross"         = "cross",
                      "Star"          = "star",
                      "Triangle up"   = "triangle-up",
                      "Triangle down" = "triangle-down",
                      "Hexagon"       = "hexagon2"
                      
                    ),
                    selected = "circle",
                    inline   = TRUE
                  )
                )


                ),
                
                # --------------------------
                # Right column: plot
                # --------------------------
                shinydashboard::box(
                  width  = 8,
                  status = "primary",
                  
                  h3("Group-level comparison of country averages"),
                  tags$p(
                    style = "margin-top:-10px; color:#555;",
                    "Each point represents one group; positions show average country values."
                  ),
                  
                  
                  # Warning message (shown only if needed)
                  uiOutput("scatter_group_warning"),
                  
                  shinycssloaders::withSpinner(
                    plotlyOutput("scatter_group_plot", height = "600px")
                  ),
                  
                  tags$p(
                    style = "font-size:12px; color:#777; margin-top:10px;",
                    "Point size reflects the number of countries in each group. ",
                    "Colours indicate group membership."
                  )
                )
              )
              
            ), #Edn tabPanemt Group level
            
            
          ) #end tabsetPanel scatterplot_tabs
            
          
          
          
          
          
          
          
        ),





################## VISUALISATIONS NEW END ##################
        
        
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
) #end Shiny UI
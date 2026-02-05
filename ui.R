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
                         ),
                         
                         menuSubItem(
                           "HDR Data Sources",
                           tabName = "hdr_sources",
                           icon = icon("database")
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
                         "Summary Statistics",
                         #tabName = "summary_stats_NEW",
                         icon = icon('info-sign', lib = "glyphicon"),
                         
                       menuSubItem(
                         "Univariate Statistics",
                         tabName = "univariateStats_new",
                         icon = icon("chart-bar")
                       ),
                       
                       menuSubItem(
                         "Bivariate Statistics",
                         tabName = "bivariateStats_new",
                         icon = icon("project-diagram")
                       )
                       ),
                       
                       ################ NEW SUMMARY STATISTICS END ################
                       
                       
                       ################## VISUALISATIONS NEW START ##########
                       menuItem(
                         "Visualisations",
                         tabName = "scatterParticipants",
                         icon = icon('move', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "World map",
                           tabName = "worldmap",
                           icon = icon("globe", lib = "glyphicon")
                         ),
                         
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
                         ),
                         
                         menuSubItem(
                           "Linear Mixed Models",
                           tabName = "lmmTab",
                           icon = icon("project-diagram")  # hierarchical structure
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


        
        ############## VARIABLE DOCUMENTATION ##############
        # WVS7 sources
        tabItem(tabName = "surveyview",
                fluidRow(column(12, shinycssloaders::withSpinner(uiOutput("surveyview"))))
        ),
        
        tabItem(tabName = "codebookview",
                fluidRow(column(12, shinycssloaders::withSpinner(uiOutput("codebookview"))))
        ),
        
        # HDR sources
        tabItem(
          tabName = "hdr_sources",
          includeMarkdown("www/instructions/hdr_sources.md"),
          # downloadLink(
          #   "download_hdr_tables_excelWorkbook",
          #   "Download: HDR Statistical Annex Tables (Excel)"
          # )
          downloadButton(
                    "download_hdr_tables_excelWorkbook",
                    "Download HDR Statistical Annex Tables (Excel)",
                    class = "btn-primary"
                          )
                              ),
        
        
        
        ############## VARIABLE DOCUMENTATION ##############
        
        
########################### RAW DATA TABLES ###################################
 
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

########################## RAW DATA END ########################################

       
########################## SUMMARY STATISTICS NEW START ########################
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




###################### SUMMARY STATISTICS NEW END #############################



########################### VISUALISATIONS NEW ###############################
      # -------------------------------------------
      # TabItem World map
      # -------------------------------------------
      tabItem(
        tabName = "worldmap",
        
        fluidRow(
          
          # =======================
          # LEFT CONTROL BOX
          # ======================
          shinydashboard::box(
            width = 3,
            status = "primary",
            
            # Toggle between Option A and Option B                      
            radioButtons(
              inputId = "map_mode",
              label   = "Map display mode",
              choices = c(
                "Alignment (WVS relative to development)" = "alignment",
                "Development level (HDR only)"            = "hdr"
              ),
              selected = "alignment",
              inline   = FALSE
            ),
            
            # Variable selector HDRs
            selectInput(
              inputId  = "indicator_hdr",      
              label    = "Select an indicator",
              choices  = HDR_INDICATOR_CHOICES, 
              selected = "hdi_2023"
            ),  

            
            conditionalPanel(
              condition= "input.map_mode == 'alignment'",
            selectizeInput(
              inputId = "wvs_var",
              label   = "WVS worldview indicator",
              choices = NULL
              )
            ),
            

            # Area filter (HDR structure reused)
            selectInput(
              inputId  = "filtered_area",
              label    = "Filter by group:",
              choices  = c("World", unique(HDR_AREA_LOOKUP$area)),
              selected = "World"
            ),
            
            # Optional country list
            checkboxInput(
              inputId = "show_country_list",
              label   = "Show list of countries in selected area",
              value   = FALSE
            )
          ),  # End LEFT Shinybox
          
          
          # =====================================
          # RIGHT COLUMN: interpretation + map
          # ====================================
          column(
            width = 9,
            
            # Map interpretation UI (FULL right column)
            shinydashboard::box(
              width = 12,            
              status = "info",
              solidHeader = TRUE,
              title  = "Map interpretation",
              uiOutput("worldmap_legend")
            ),
            
            
            
            
            # Choropleth UI (FULL right column)
            shinydashboard::box(
              width = 12,           
              title  = "Country-Level Choropleth Map",
              status = "primary",
              shinycssloaders::withSpinner(
                leafletOutput("world_choropleth", height = "600px")
              )
            )
          )
        ),
        
        # ---------------------------------------
        # Country list UI (full width, below)
        # ---------------------------------------
        fluidRow(
          column(
            width = 12,
            uiOutput("area_country_list")
          )
        )
      ),
      
      
      
      # -------------------------------------------
      # TabItem BarChart
      # -------------------------------------------    
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

      
      # -------------------------------------------
      # TabItem SCATTER PLOT
      # -------------------------------------------     
      tabItem(
        tabName= "scatterParticipants",
        
        #includeMarkdown("www/instructions/scatter_instruction.md"),
        br(),
        
        tabsetPanel(
          id = "scatterplot_tabs",
          type= "tabs",
          
          # ---------------------------------------------------------
          # ---------------------------------------------------------
          # Individual-level scatter tab
          # ---------------------------------------------------------
          # ---------------------------------------------------------
          tabPanel(
            title = "Individual-level",
            
            # Optional: instructions / help text
            includeMarkdown("www/instructions/scatter_instruction_ind.md"),
            
            fluidRow(
              
              
              # ========================
              # Left column: controls
              # ========================
              shinydashboard::box(
                width  = 3,
                status = "primary",
                
                h4("Select variables"),
                
                
                # ---- X-axis variable selection ----
                selectizeInput(
                  inputId  = "scatter_x",
                  label    = "X-axis Question",
                  choices  = grouped_minus_ignored,          
                  selected = grouped_minus_ignored[[1]][1]   # default selection
                ),
                
                
                # ---- Y-axis variable selection ----
                selectizeInput(
                  inputId  = "scatter_y",
                  label    = "Y-axis Question",
                  choices  = grouped_minus_ignored,          
                  selected = grouped_minus_ignored[[1]][2]
                ),
                
                
                # ---- Country selection ----
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
                
                
                # ---- Optional: sample size ----
                sliderInput(
                  inputId = "scatter_sample",
                  label   = "Sample Size (as % of data)",
                  min     = 10,
                  max     = 100,
                  value   = 25,
                  step    = 1
                )
              ),
              
              # =======================
              # Right column
              # =======================
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
          
          
          # ---------------------------------------------------------
          # ---------------------------------------------------------
          # Country-level scatter plot tab 
          # ---------------------------------------------------------
          # ---------------------------------------------------------
          tabPanel(
            title= "Country-level",
            includeMarkdown("www/instructions/scatter_instruction_ctry.md"),
            
            fluidRow(
              
              # ===============================
              # Left panel: User controls
              # ===============================
              shinydashboard::box(
                width  = 4,
                status = "primary",
                
                # Section title
                h4("Select variables"),
                
                # ---- X-axis variable selection ----
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
                
                
                # ---- Y-axis variable selection ----
                # Choose the data source for the Y variable
                
                radioButtons(
                  inputId = "scatter_country_source_y",
                  label   = "Y variable source",
                  choices = c("ALL", "HDR", "WVS7"),
                  selected = "ALL",
                  inline  = TRUE
                ),
                
                # Dropdown for selecting the Y variable
                # Choices are populated dynamically in the server
                selectizeInput(
                  inputId = "scatter_country_y",
                  label   = "Y variable",
                  choices = NULL
                ),
                
                
                # ----  Country selection ----
                
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
                
                
                # ----  DISPLAY OPTIONS -------
                # Display options (COUNTRY level)
                
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
              
              
              # ==================================
              # Right panel: Scatter plot output
              # ==================================
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
          
          # ---------------------------------------------------------
          # ---------------------------------------------------------
          # Group-level scatterplot tab
          # ---------------------------------------------------------
          # ---------------------------------------------------------        
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
                
                # ---- Variables selection ----
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
                
                
                # ---- DISPLAY OPTIONS ----
                tags$div(
                  style = "
                    background-color: #f7f9fc;
                    border-top: 3px solid #1f77b4;
                    #border-right: 4px solid #1f77b4;
                    padding: 8px 10px;
                    margin-top: 10px;
                  ",
                  
                  h4("Display options", style = "margin-top:0;"),
                  
                  # Select point size
                  radioButtons(
                    inputId  = "scatter_group_size_mode",
                    label    = "Point size",
                    choices  = c(
                      "Equal size"                    = "equal",
                      "Scaled by number of countries" = "n_countries"
                    ),
                    selected = "equal"
                  ),
                  
                  
                  # Choose to show Group labels (shown ONLY when point size = equal)
                  conditionalPanel(
                    condition = "input.scatter_group_size_mode != 'n_countries'",
                    
                    checkboxInput(
                      inputId = "scatter_group_show_labels",
                      label   = "Show group labels",
                      value   = FALSE
                    )
                  ),
                  
                  
                  # Select Color palette
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
                  
                  
                  # Select Group point shape
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
              
              # =========================
              # Right column: plot
              # ========================
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
                
                #Group-level scatterplot output
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
      

      # -------------------------------------------
      # TabItem correlation
      # -------------------------------------------
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

      
      # -------------------------------------------
      # TabItem Histogram
      # -------------------------------------------
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
##################### VISUALISATIONS NEW END ###################################




########################### MODELS START #######################################
        
        # ===========================================================
        #                 Correlation 
        # ===========================================================
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
        
        # ===========================================================
        #                 ANOVA 
        # ===========================================================
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
        

        # ===========================================================
        #                 Linear regression NEW
        # ===========================================================
        tabItem(
          tabName = "regressionTab",
          ##includeMarkdown("www/instructions/linearreg_instruction.md"),
          
          # Sub-tabs for regression level
          tabsetPanel(
            id = "regression_level",
            
            # ---------------------------------------------------------
            # ---------------------------------------------------------
            # Individual-level regression (WVS respondents)
            # --------------------------------------------------------
            # ---------------------------------------------------------
            tabPanel(
              "Individual-level (WVS)",
              includeMarkdown("www/instructions/linearreg_instruction_ind.md"),
              
              
              fluidRow(
                shinydashboard::box(width = 3, status = "primary",
                                    
                                    # Select ONE dependent variable                
                                    selectizeInput(
                                      inputId = "regression_dep",
                                      label = "Dependent Variable:",
                                      choices = grouped_minus_ignored,
                                      selected = grouped_minus_ignored[[1]][1]
                                    ),
                                    
                                    # Select ONE or MORE independent variable(s)
                                    pickerInput(
                                      inputId = "regression_indep",
                                      label = "Independent Variables:",
                                      choices = grouped_minus_ignored,
                                      multiple = TRUE,
                                      selected = grouped_minus_ignored[[1]][2:3],
                                      options = list(
                                        `live-search` = TRUE,
                                        `selected-text-format` = "count > 1"
                                      )
                                    ),
                                    
                                    # Select countries (filters respondents)
                                    pickerInput(
                                      inputId = "regression_country",
                                      label = "Select Country:",
                                      choices = picker_country_list,
                                      multiple = TRUE,
                                      selected = "NZL",
                                      options = list(
                                        `actions-box` = TRUE,
                                        `live-search` = TRUE,
                                        `selected-text-format` = "count > 1"
                                      )
                                    ),
                                    
                                    actionButton(
                                      "regression_run",
                                      "Run Regression",
                                      class = "green-button"
                                    )
                ),
                
                shinydashboard::box(width = 9, title = "Regression Analysis", status = "primary",
                                    tabsetPanel(
                                      tabPanel("Model Summary",
                                               verbatimTextOutput("regression_summary")),
                                      tabPanel("Diagnostics",
                                               plotOutput("regression_diag")),
                                      tabPanel("Prediction",
                                               plotlyOutput("regression_prediction"))
                                    )
                )
              )
            ),
            
          # ---------------------------------------------------------
          # ---------------------------------------------------------
          # Country-level regression (DEFAULT + ADVANCED UI)
          # ---------------------------------------------------------
          # ---------------------------------------------------------
            tabPanel(
              "Country-level",
              includeMarkdown("www/instructions/linearreg_instruction_ctry.md"),
              fluidRow(
                
                # ================
                # Controls (left)
                # =================
                shinydashboard::box(
                  width = 3,
                  status = "primary",

                  # Select dependent variable
                  selectInput(
                    inputId = "country_reg_dep",
                    label   = "Outcome variable (HDR):",
                    choices = HDR_var_choices
                    #choices = HDR_OUTCOME_CHOICES
                  ),
                  
                  # Select Independent variable
                  pickerInput(
                    inputId = "country_reg_indep",
                    label   = "Explanatory variables:",
                    #choices = COUNTRY_EXPLANATORY_CHOICES,
                    choices = NULL,
                    multiple = TRUE,
                    options = list(
                      `live-search` = TRUE,
                      `selected-text-format` = "count > 1"
                    )
                  ),
                  
                  tags$small(
                    "By default, all countries with complete data are included.",
                    style = "color:#666;"
                  ),
                  
                  tags$br(), tags$br(),
                  
                  #Run button
                  actionButton(
                    "country_reg_run",
                    "Run Regression",
                    class = "green-button"
                  ),
                  
                  tags$hr(),
  
                  # ------------------------------
                  # ADVANCED OPTIONS 
                  # ------------------------------
                  tags$details(
                    tags$summary(
                      tags$strong("Advanced options (use with caution)")
                    ),
                    
                    tags$br(),
                    
                    # Allow non-HDR outcome
                    checkboxInput(
                      inputId = "allow_non_hdr_outcome",
                      label   = "Allow non-HDR outcome variables",
                      value   = FALSE
                    ),
                    
                    tags$small(
                      "Survey-based measures reflect reported attitudes and should be interpreted with caution.",
                      style = "color:#a94442;"
                    ),
                    
                    tags$hr(),
                    tags$hr(),
                    
                    # helpText(
                    #   "Note: Group filtering and manual country selection cannot be combined to avoid ambiguous model definitions."
                    # ),
                    
                    tags$hr(),
                    
                    # Check box to enable manual country selection
                    checkboxInput(
                      inputId = "manual_country_select",
                      label   = "Manually select countries",
                      value   = FALSE
                    ),
                    
                    # Select countries 
                    conditionalPanel(
                      condition = "input.manual_country_select == true",
                      
                      pickerInput(
                        inputId = "country_manual_list",
                        label   = "Select countries:",
                        #choices = picker_country_list,
                        choices = NULL, 
                        multiple = TRUE,
                        options = list(
                          `live-search` = TRUE,
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 1"
                        )
                      ),
                      
                      tags$small(
                        "Warning: models with a small number of countries may be unstable.",
                        style = "color:#a94442;"
                      )
                    )
                  )
                  
                ), #end shinydashboard box control left
                
                # =================
                # Outputs (right)
                # =================
                shinydashboard::box(
                  width = 9,
                  title = "Country-level Regression",
                  status = "primary",
                  
                  tabsetPanel(
                    tabPanel(
                      "Model Summary",
                      verbatimTextOutput("country_reg_summary") # Stat summary output
                    ),
                    tabPanel(
                      "Diagnostics",
                      plotOutput(outputId = "country_reg_diagnostics", #Diagnostics output
                                  height = "700px"
                              )
                    ),
                    tabPanel(
                      "Model accuracy (Observed vs Predicted)",
                        plotlyOutput(outputId= "country_reg_prediction", height = "500px")
                    )
                  ) #end tabsetPanel output
                ) # end shinydashboard::box
              ) #end fluidrow
            ) #end tabPanel country level linear regression
            
          )
        ), # end tabItem Linear regression

        
        # ===========================================================
        # Linear Mixed Models
        # ===========================================================
          tabItem(
            tabName = "lmmTab",
            
            fluidRow(
              box(
                width = 12,
                title = "Linear Mixed Models",
                status = "primary",
                solidHeader = TRUE,
                
                tabsetPanel(
                  id = "lmm_level_tab",
                  
            # -------------------------------------------------------
            # Country-level LMM 
            # -------------------------------------------------------
                  tabPanel(
                    title = "Country-level",
                    value = "lmm_country",
                    includeMarkdown("www/instructions/lmm_instruction_ctry.md"),
                    fluidRow(
                      
                      # ================
                      # Controls (left) 
                      # ================
                      conditionalPanel(
                        condition = "input.lmm_results_tab == 'lmm_summary'",
                      
                      
                      shinydashboard::box(
                        width  = 3,
                        status = "primary",

                        tags$h4(
                          tags$span(
                            tags$div("Country-level LMM", style = "font-weight: 600;"),
                            tags$div(
                              "Data: HDR Table 2 (1990–2023)",
                              style = "font-size: 14px; color: #555;"
                            ),
                            style = "
                            background-color: #F0F7FB;
                            padding: 6px 12px;
                            border-left: 4px solid #2C7FB8;
                            display: inline-block;
                          "
                          ),
                          style = "margin-bottom: 15px;"
                        ),
                        
                        
                        ## Countries included in the analysis

                        #tags$strong("Countries included in the analysis"),
                        tags$strong("Select the countries (default is All)"),
                        
                        #Select countries
                        pickerInput(
                          inputId  = "lmm_countries",
                          label    = NULL,
                          choices = countries_table2$country,
                          multiple = TRUE,
                          options  = list(
                            `live-search` = TRUE,
                            `actions-box` = TRUE,
                            `selected-text-format` = "count > 3"
                          )
                        ),
                        
                        # FIXED EFFECTS (overview)
                        tags$div(
                          tags$span(
                            "Fixed effects",
                            style = "font-weight:700; color:#337ab7; font-size:16px;"),
                          tags$hr(style = "margin-top:4px; margin-bottom:8px; border-top:2px solid #337ab7;")
                        ),
                        
                        tags$small(
                          "Fixed effects explain systematic differences in HDI levels or trends across countries.",
                          style = "color:#666; font-style:italic;"
                        ),
                        

                        # TIME (always included)
                        tags$p(
                          tags$strong("Time (year)"),
                          tags$br(),
                          tags$small(
                            "Included by default to model HDI trends over time.",
                            style = "color:#666; font-style:italic;")
                          ),
                        
                        tags$hr(style = "border-top-width: 2px;"),

                        
                        # Fixed effects: country characteristics
                        tags$strong("Country characteristics"),
                        tags$small(
                          "Structural classifications that differ between countries but do not change over time.",
                          style = "color:#666; font-style:italic;"
                        ),

                        checkboxGroupInput(
                          inputId = "lmm_country_fixed_extra",
                          label   = NULL,
                          choices = c(
                            "HDI group" = "hdr_group",
                            "Region"   = "hdr_region")
                          ),

                        tags$hr(style = "border-top-width: 2px;"),
                        
                        # Fixed effects: policy & structural memberships
                        tags$strong("Policy & structural memberships"),
                        tags$small(
                        "Institutional or vulnerability-related classifications applied as additive fixed effects.",
                        #style = "color:#666;"
                        style = "color:#666; font-style:italic;"
                        
                        ),
                        
                        checkboxGroupInput(
                          inputId = "lmm_country_policy_effects",
                          label   = NULL,
                          choices = c(
                            "OECD member country"                  = "is_oecd",
                            "Small Island Developing State (SIDS)" = "is_sids",
                            "Least Developed Country (LDC)"        = "is_ldc",
                            "Developing Country (DCs)"        = "is_dc"
                          )
                        ),
                        
                        tags$hr(style = "border-top-width: 2px;"),
                        
                        
                        # ADVANCED OPTIONS: Time handling
                        tags$div(
                          class = "well",
                          
                          tags$strong("Advanced options"),
                          
                          # Choose to center or not the time
                          checkboxInput(
                            inputId = "lmm_center_time",
                            label   = "Center time variable",
                            value   = TRUE
                          ),
                          
                          tags$small(
                            tags$em(
                              "Centers the year variable at the average observed year, ",
                              "improving interpretability and numerical stability."
                            ),
                            style = "color:#666;"
                          )
                        ),
                        
                        
                        # RANDOM EFFECTS (country-level)
                        tags$div(
                          tags$span(
                            "Random effects",
                            style = "font-weight:700; color:#337ab7; font-size:16px;"
                          ),
                          tags$hr(style = "margin-top:4px; margin-bottom:8px; border-top:2px solid #337ab7;")
                        ),
                        
                        
                        tags$small(
                          "Random effects allow countries to deviate from the average HDI trajectory.",
                          #style = "color:#666;"
                          style = "color:#666; font-style:italic;"
                          
                        ),
                        
                        # choose RI, RS, or RI + RS
                        radioButtons(
                          inputId = "lmm_country_random_structure",
                          label   = "Country-level random effects:",
                          choices = c(
                            "Random intercept" =
                              "ri",
                            
                            "Random slope for time" =
                              "rs",
                            
                            "Random intercept and random slope" =
                              "ri_rs"
                          ),
                          selected = "ri"
                        ),
                        
                        tags$hr(),

                        # Run LMM
                        actionButton(
                          inputId = "lmm_run",
                          label   = "Run",
                          icon    = icon("play"),
                          class   = "btn-primary",
                          width   = "100%"
                        ),
                        
                      ) #end  shinydashboard::box control left
                      ), #End conditional panel 
                      
                      
                      # ======================================================
                      # Prediction toggle - ONLY in Predicted trajectories
                      # ======================================================
                      conditionalPanel(
                        condition = "input.lmm_results_tab == 'predicted'",
                        
                        shinydashboard::box(
                          width  = 3,
                          status = "primary",
                          
                          tags$strong("Predicted trajectories options"),
                          
                          radioButtons(
                            inputId = "lmm_pred_type",
                            label   = NULL,
                            choices = c(
                              "Global average (fixed effects only)" = "fixed",
                              "Country-specific (conditional)"      = "conditional"
                            ),
                            selected = "fixed"
                          )
                        )
                      ),
                      
                      # ===============================
                      # Outputs (right)
                      # ===============================
                      shinydashboard::box(
                        width  = 9,
                        title  = " Country-level LLM results",
                        status = "primary",
                        
                        tabsetPanel(
                          id = "lmm_results_tab",   
                          
                          # -------------------------------
                          # SUMMARY subtab
                          # -------------------------------
                          tabPanel(
                            title = "Model summary",
                            value = "lmm_summary",
                            verbatimTextOutput("lmm_model_summary")

                          ),
                          # -------------------------------
                          # RANDOM EFFECTS subtab
                          # -------------------------------
                          tabPanel(
                            "Random effects",
                            
                          # Random intercept plot
                            conditionalPanel(
                              condition = "input.lmm_country_random_structure == 'ri' ||
                 input.lmm_country_random_structure == 'ri_rs'",
                              
                              plotlyOutput(
                                outputId = "lmm_random_intercept_plot",
                                height   = "800px"
                              )
                            ),
                            
                            # Random slope plot
                            conditionalPanel(
                              condition = "input.lmm_country_random_structure == 'rs' ||
                 input.lmm_country_random_structure == 'ri_rs'",
                              
                              plotlyOutput(
                                outputId = "lmm_random_slope_plot",
                                height   = "800px"
                              )
                            ),
                            

                            # Intercept–slope correlation
                            conditionalPanel(
                              condition = "input.lmm_country_random_structure == 'ri_rs'",
                              
                              verbatimTextOutput("lmm_country_ranef_correlation")
                            )
                          ),
                          
                          

                          # -------------------------------
                          # DIAGNOSTICS subtab
                          # -------------------------------                          
                          tabPanel(
                            title = "Diagnostics",
                            
                            tabsetPanel(
                              
                              # Diagnostic 1: Residuals vs fitted
                              tabPanel(
                                title = "Residuals vs fitted",
                                plotlyOutput(
                                  outputId = "lmm_diag_resid_fitted",
                                  height   = "600px"
                                )
                              ),
                              
                              # Diagnostic 2: Residual Q–Q plot
                              tabPanel(
                                title = "Residual Q–Q",
                                plotlyOutput(
                                  outputId = "lmm_diag_resid_qq",
                                  height   = "600px"
                                )
                              ),
                              
                              # Diagnostic 3: Random effects Q–Q plot
                              tabPanel(
                                title = "Random effects Q–Q",
                                plotlyOutput(
                                  outputId = "lmm_diag_ranef_qq",
                                  height   = "600px"
                                )
                              ),

                              # Diagnostic 4: Scale–location plot
                              tabPanel(
                                title = "Scale–location",
                                plotlyOutput(
                                  outputId = "lmm_diag_scale_location",
                                  height   = "600px"
                                )
                              )
                            )
                          ),
                          
                          
                          # -------------------------------
                          # PREDICTED TRAJECTORIES subtab
                          # -------------------------------
                          tabPanel(
                            title = "Model-based trajectories",
                            value = "predicted",
                            p(
                              class = "text-muted",
                              "Predicted HDI trajectories based on the fitted mixed-effects model."
                            ),
                            
                            plotlyOutput(
                              "lmm_predicted_trajectories",
                              height = "550px"
                            )
                          )
                          
                        )#end tabsetPanel
                      )
                    )
                  ), #end tabPanel country-level
                  
            # -------------------------------------------------------     
            # -------------------------------------------------------
            # Group-level LMM (structure only)
            # -------------------------------------------------------
            # -------------------------------------------------------
                  tabPanel(
                    title = "Group-level",
                    value = "lmm_group",
                    
                    includeMarkdown("www/instructions/lmm_instruction_grp.md"),
                    
                    
                    fluidRow(
                      
                      # ======================
                      # Controls (left)
                      # -====================
                      shinydashboard::box(
                        width  = 3,
                        status = "primary",
                        
                        tags$h4(
                          tags$span(
                            tags$div("Group-level LMM", style = "font-weight: 600;"),
                            tags$div(
                              "Data: HDR Table 2 (1990-2023: aggregate groups)",
                              style = "font-size: 14px; color: #555;"),
                            style = " background-color: #F0F7FB; padding: 6px 12px;
                            border-left: 4px solid #2C7FB8;
                            display: inline-block;"),
                          style = "margin-bottom: 15px;"
                        ),
                        
                        
                        tags$p(
                          "Outcome: HDI",
                          tags$br(),
                          "Data: HDR Table 2 (aggregate groups)"
                        ),
                        
                          # GROUP TYPE selector
                        tags$strong("Select group type"),
                        
                        radioButtons(
                          inputId = "lmm_group_type",
                          label   = NULL,
                          choices = c(
                            "Regions"                        = "regions",
                            "Human development groups"       = "hd_groups",
                            "International reference groups" = "ref_groups"
                          ),
                          selected = "regions"
                        ),
                        
                        tags$hr(),
                        
                        # FIXED EFFECT (overview only)
                        tags$div(
                          tags$span(
                            "Fixed effects",
                            style = "font-weight:700; color:#337ab7; font-size:16px;"
                          ),
                          tags$hr(style = "margin-top:4px; margin-bottom:8px; border-top:2px solid #337ab7;")
                        ),
                        
                        tags$p(
                          tags$strong("Time (year)"),
                          tags$br(),
                          tags$small(
                            "Included by default to model HDI trends over time.",
                            style = "color:#666; font-style:italic;"
                          )
                        ),
                        
                        tags$hr(),
                        
                        # ADVANCED OPTIONS: time handling
                        tags$div(
                          class = "well",
                          
                          tags$strong("Advanced options"),
                          
                          checkboxInput(
                            inputId = "lmm_group_center_time",
                            label   = "Center time variable",
                            value   = TRUE
                          ),
                          
                          tags$small(
                            tags$em(
                              "Centers the year variable for interpretability and numerical stability."
                            ),
                            style = "color:#666;"
                          )
                        ),
                        
                        # RANDOM EFFECTS 
                        tags$div(
                          tags$span(
                            "Random effects",
                            style = "font-weight:700; color:#337ab7; font-size:16px;"
                          ),
                          tags$hr(style = "margin-top:4px; margin-bottom:8px; border-top:2px solid #337ab7;")
                        ),
                        
                        radioButtons(
                          inputId = "lmm_group_random_structure",
                          label   = "Group-level random effects:",
                          choices = c(
                            "Random intercept"                 = "ri",
                            "Random slope for time"             = "rs",
                            "Random intercept and random slope" = "ri_rs"
                          ),
                          selected = "ri"
                        ),
                        
                        tags$hr(),
                        
                        
                        #RUN BUTTON
                        actionButton(
                          inputId = "lmm_group_run",
                          label   = "Run",
                          icon    = icon("play"),
                          class   = "btn-primary",
                          width   = "100%"
                        )
                      ),
                      
                      # =====================
                      # Outputs (right)
                      # =====================
                  
                      shinydashboard::box(
                        width  = 9,
                        title  = "Group-level LMM Results",
                        status = "primary",
                        
                        tabsetPanel(
                          id = "lmm_group_results_tab",
                          
                          
                          # -------------------------------
                          # MODEL SUMMARY subtab
                          # -------------------------------                          
                          tabPanel(
                            title = "Model summary",
                            value = "summary",
                            verbatimTextOutput("lmm_group_model_summary")
                          ),
                          
                          # -------------------------------
                          # RANDOM EFFECS subtab
                          # -------------------------------
                          tabPanel(
                            title = "Random effects",
                            
                            # Random intercept plot
                            conditionalPanel(
                              condition = "input.lmm_group_random_structure == 'ri' ||
                 input.lmm_group_random_structure == 'ri_rs'",
                            
                            plotlyOutput(
                              outputId = "lmm_group_random_intercept_plot",
                              height   = "600px",
                              )
                            ), # conditionalPanel
                            
                            # Spacing only if both plots may appear
                            conditionalPanel(
                              condition = "input.lmm_group_random_structure == 'ri_rs'",
                              tags$hr()
                            ),
                            
                            
                            # Random slope plot
                            conditionalPanel(
                              condition = "input.lmm_group_random_structure == 'rs' ||
                 input.lmm_group_random_structure == 'ri_rs'",
                            plotlyOutput(
                              outputId = "lmm_group_random_slope_plot",
                              height   = "600px"
                            )
                          ),
                          
                          # Intercept–slope correlation 
                          conditionalPanel(
                            condition = "input.lmm_group_random_structure == 'ri_rs'",
                            tags$hr(),
                            tags$h5("Intercept–slope correlation"),
                            verbatimTextOutput("lmm_group_ranef_correlation"),
                            
                            tags$div(
                              style = "margin-top: 8px;
                                       font-size: 13px;
                                        color: #555;
                                      ",
                              tags$ul(
                                style = "margin-bottom: 0;",
                                tags$li("Negative correlation => groups starting higher tend to grow more slowly"),
                                tags$li("Positive correlation => groups with higher baseline HDI grow faster")
                              )
                            )
                            
                          ) # End conditionalPanel Intercept–slope correlation
                          
                          ), #End tabPanel random effects
                          
                          
                          # -------------------------------
                          # DIAGNOSTICS subtab
                          # -------------------------------                          
                          tabPanel(
                            title = "Diagnostics",
                            
                            tabsetPanel(
                              tabPanel(
                                "Residuals vs fitted",
                                plotlyOutput("lmm_group_diag_resid_fitted", height = "600px")
                              ),
                              tabPanel(
                                "Residual Q–Q",
                                plotlyOutput("lmm_group_diag_resid_qq", height = "600px")
                              ),
                              tabPanel(
                                "Random effects Q–Q",
                                plotlyOutput("lmm_group_diag_ranef_qq", height = "600px")
                              ),
                              tabPanel(
                                "Scale–Location",
                                plotlyOutput("lmm_group_diag_scale_location", height = "600px")
                              )
                            )
                          ),
                          
                          # -------------------------------
                          # PREDICTED TRAJECTORIES subtab
                          # -------------------------------
                          tabPanel(
                            title = "Model-based trajectories",
                            
                            # Choose how predictions are displayed
                            radioButtons(
                              inputId = "lmm_group_pred_view",
                              label   = "Prediction view",
                              choices = c(
                                "Individual group trajectories" = "individual",  # One line per group
                                "Global average trajectory"     = "global"       # Average across groups
                              ),
                              selected = "individual"
                            ),
                            
                            # Context-specific help text
                            # (updates when group type or view changes)
                            uiOutput("lmm_group_pred_help"),
                            
                            # Predicted trajectories plot
                            plotlyOutput(
                              outputId = "lmm_group_predicted_trajectories",
                              height   = "600px"
                            )
                          ),
                          
                        )
                      )
                    )
                  )

                )
              )
            )
          ),# end tabItem LMM
  

        ###################### MODELS END ######################
        
 
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
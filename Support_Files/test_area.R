install.packages('rsconnect')


rsconnect::setAccountInfo(name='andredevito',
                          token='4D04EB145CFAEE43137141E57B1CF4A7',
                          secret='kAQY9DjhgbkqemEz9F6q6I9ZaW+V3etzK/+/P4wj')

library(rsconnect)
rsconnect::deployApp('D:/Documents/GitHub/PSYC382_WVS7_Shiny')




###################################################################################
###################################################################################
###################################################################################

# library(shiny)
# library(DT)
# library(MASS)   # for cov.rob (robust covariance estimate)
# library(ggplot2)
# 
# ui <- fluidPage(
#   titlePanel("Mahalanobis Distance Explorer"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       checkboxGroupInput("vars", "Select variables:",
#                          choices = names(mtcars),
#                          selected = c("mpg", "disp", "hp")),
#       numericInput("cutoff", "Outlier cutoff (distance):", value = 10, min = 0),
#       helpText("Points with Mahalanobis distance above cutoff will be flagged.")
#     ),
#     
#     mainPanel(
#       DTOutput("distTable"),
#       plotOutput("distPlot")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   session$onSessionEnded(function() {
#     stopApp()
#   })
#   
#   data_selected <- reactive({
#     req(input$vars)
#     mtcars[, input$vars, drop = FALSE]
#   })
#   
#   mahalanobis_data <- reactive({
#     X <- data_selected()
#     mu <- colMeans(X)
#     S <- cov(X)
#     d <- mahalanobis(X, center = mu, cov = S)
#     
#     df <- data.frame(Car = rownames(X),
#                      Distance = d,
#                      Outlier = d > input$cutoff)
#     
#     df
#   })
#   
#   output$distTable <- renderDT({
#     datatable(mahalanobis_data(), rownames = FALSE)
#   })
#   
#   output$distPlot <- renderPlot({
#     df <- mahalanobis_data()
#     ggplot(df, aes(x = reorder(Car, Distance), y = Distance, fill = Outlier)) +
#       geom_bar(stat = "identity") +
#       coord_flip() +
#       scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
#       geom_hline(yintercept = input$cutoff, linetype = "dashed", color = "black") +
#       labs(x = "Car", y = "Mahalanobis Distance",
#            title = "Multivariate Distance from Mean") +
#       theme_minimal()
#   })
# }
# 
# shinyApp(ui, server)


###################################################################################
###################################################################################
###################################################################################
# 
# indiv_ordinal
# 
# 
# 
# 
# ggplot(data = indiv_ordinal, aes(x = B_COUNTRY, y = Q1)) +
#   geom_point() +
#   labs(
#     title = "Scatterplot",
#     x = "countries",
#     y = "Q1"
#   ) +
#   theme_minimal()
# 
# 
# 
# 
# ggplot(data = orig_country_data, aes(x = Q1, y = B_COUNTRY)) +
#   geom_point() +
#   labs(
#     title = "Scatterplot",
#     x = "Q1"
#   ) +
#   theme_minimal()


###################################################################################
###################################################################################
###################################################################################
# 
# 
# library(ggplot2)
# library(dplyr)
# 
# # Bin horsepower into intervals of 50
# mtcars_summary <- mtcars %>%
#   mutate(hp_bin = cut(hp, breaks = seq(50, 350, by = 50))) %>%
#   group_by(hp_bin) %>%
#   summarise(avg_mpg = mean(mpg), .groups = "drop")
# 
# # Plot average mpg for each hp_bin
# ggplot(mtcars_summary, aes(x = hp_bin, y = avg_mpg)) +
#   geom_col(fill = "steelblue") +
#   labs(
#     title = "Average MPG by Horsepower Range",
#     x = "Horsepower (binned)",
#     y = "Average Miles per Gallon"
#   ) +
#   theme_minimal()
# 
# 
# 
# 

###################################################################################
###################################################################################
###################################################################################
# NZ + Q6 + Q112

# comp_d <- get_country_data() |> na.omit()
test_comp_d <- orig_indiv_data %>% dplyr::filter(B_COUNTRY_ALPHA == "NZL") %>% dplyr::select("Q6", "Q112") %>% na.omit()

# v1 <- input$wc_sel_qA
test_v1 <- "Q6"
# v2 <- input$wc_sel_qB
test_v2 <- "Q112"


# v1_class <- paste0(class(comp_d[,v1]), collapse = "")
test_v1_class <- paste0(class(test_comp_d[,test_v1]), collapse = "")
# v2_class <- paste0(class(comp_d[,v2]), collapse = "")
test_v2_class <- paste0(class(test_comp_d[,test_v2]), collapse = "")
test_v_classes <- c(test_v1_class, test_v2_class)



#' Violin plot
v_plot <- ggplot(test_comp_d, 
                 aes(x = .data[[test_v1]],
                     y = .data[[test_v2]],
                     fill = .data[[test_v1]])) + 
  geom_violin(trim = FALSE,
              alpha = 0.4) +
  geom_jitter(shape = 16,
              position = position_jitter(0.15),
              alpha = 0.3) +
  geom_boxplot(width = 0.1,
               alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  theme_minimal()


  
test_comp_d_int <- test_comp_d
test_comp_d_int[,test_v1] <- as.integer(test_comp_d_int[,test_v1])

test_v_stats <- cor.test(test_comp_d_int[,test_v1], 
                    test_comp_d_int[,test_v2],
                    method = "kendall")

###################################################################################
###################################################################################
###################################################################################


orig_indiv_data |>
  filter(B_COUNTRY_ALPHA == "NZL") |>
  summarise('Number of valid observations' = sum(!is.na(.data[["Q1"]])))

orig_indiv_data |>
  filter(B_COUNTRY_ALPHA == "NZL") |>
  summarise(
    'Valid observations' = sum(!is.na(.data[["Q1"]])),
    'Missing observations' = sum(is.na(.data[["Q1"]])),
    'Total observations' = n()
  )



###################################################################################
###################################################################################
###################################################################################

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Dynamic Sidebar"),
  dashboardSidebar(
    sidebarMenuOutput("dynamicSidebar")  # Reactive sidebar
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              h2("Home Tab"),
              selectInput("menu_control", "Choose a menu group:", 
                          choices = c("Main", "Advanced"))
      ),
      tabItem(tabName = "dashboard", h2("Main Dashboard")),
      tabItem(tabName = "settings", h2("Settings Page")),
      tabItem(tabName = "advanced", h2("Advanced Analytics")),
      tabItem(tabName = "admin", h2("Admin Panel"))
    )
  )
)

server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$dynamicSidebar <- renderMenu({
    if (input$menu_control == "Main") {
      sidebarMenu(id = "tabs",
                  menuItem("Home", tabName = "home", icon = icon("home")),
                  menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
                  menuItem("Settings", tabName = "settings", icon = icon("cogs"))
      )
    } else {
      sidebarMenu(id = "tabs",
                  menuItem("Home", tabName = "home", icon = icon("home")),
                  menuItem("Advanced", tabName = "advanced", icon = icon("chart-line")),
                  menuItem("Admin", tabName = "admin", icon = icon("user-shield"))
      )
    }
  })
}

shinyApp(ui, server)






###################################################################################
###################################################################################
###################################################################################


d <- orig_codebook_data
d$Variable_Display_Logical <- as.logical(d$Variable_Display_Logical)
var_info <- d

sections <- as.list(unique(var_info$Section))
sections_ord <- factor(var_info$Section, ordered = TRUE, levels = sections)
testDD <- data.frame(group = sections_ord,
                     qvar = var_info$ColLab)
choicesgrpQ <- split(testDD$qvar, testDD$group, lex.order = FALSE)
choicesgrpQ <- choicesgrpQ[-1]
choicesgrpQ <- head(choicesgrpQ, -1)
choicesgrpQ



###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

# # ######################
# # # PACKAGE COLLECTION #
# # ######################
# library(shiny)
# 
# required_packages <- c('collapsibleTree', 'DT', 'ggplot2', 'gt', 'gtsummary', 'leaflet', 'leaflet.extras', 'naniar',
#                        'readxl', 'rnaturalearth', 'rnaturalearthdata', 'rvest', 'sf', 'shinyBS', 'shinycssloaders', 'shinydashboard',
#                        'shinyWidgets', 'tidyverse', 'tigris', 'vcd', 'dplyr', 'recipes', 'GGally', 'corrgram', 'corrplot',
#                        'ggpubr', 'rstatix', 'broom', 'AICcmodavg', 'viridis', 'scales', 'colorspace', 'plotly')
# 
# for (packageName in required_packages) {
#   if (!requireNamespace(packageName, quietly = TRUE)) {
#     install.packages(packageName)
#   }
# }
# 
# library(collapsibleTree)
# library(DT)
# library(ggplot2)
# library(gt)
# library(gtsummary)
# library(leaflet)
# library(leaflet.extras)
# library(naniar)
# library(readxl)
# library(rnaturalearth)
# library(rvest)
# library(sf)
# library(shinyBS)
# library(shinycssloaders)
# library(shinydashboard)
# library(shinyWidgets)
# library(tidyverse)
# library(tigris)
# library(vcd)
# library(dplyr)
# library(recipes)
# library(GGally)
# library(corrgram)
# library(corrplot)
# library(ggpubr)
# library(rstatix)
# library(broom)
# library(AICcmodavg)
# library(viridis)
# library(scales)
# library(colorspace)
# library(plotly)
# 
# # ####################################
# # # SETTING SEED FOR REPRODUCIBILITY #
# # ####################################
# set.seed(20241211)
# 
# # #############################################
# # # RUN THIS LINE ON THE VERY FIRST EXECUTION #
# # #############################################
# # source(file.path("Support_Files/WVS_Wave7_Setup.R"), local = TRUE)
# 
# # ###########################
# # # wave 7 - DATA WRANGLING #
# # ###########################
# source(file.path("Support_Files/WVS_Wave7_Wrangling.R"), local = TRUE)
# 
# # #####################
# # # SUPPORT FUNCTIONS #
# # #####################
# source(file.path("Support_Files/functions.R"), local = TRUE)
# 
# # ########################################
# # # GLOBAL VARIABLES AND HELPER FUNCTIONS #
# # ########################################
# 
# # Question ID mapping function
# get_question_id <- function(label) {
#   # Access the global codebook data
#   var_info <- orig_codebook_data
#   # Handle case where label might not be found
#   if (!label %in% var_info$ColLab) {
#     warning(paste("Label not found in codebook:", label))
#     return(NULL)
#   }
#   var_info$Col_ID[var_info$ColLab == label]
# }
# 
# # Function to get grouped questions
# get_groupedQs_I <- function() {
#   var_info <- orig_codebook_data
#   sections <- as.list(unique(var_info$Section))
#   sections_ord <- factor(var_info$Section, ordered = TRUE, levels = sections)
#   testDD <- data.frame(group = sections_ord,
#                        qvar = var_info$ColLab)
#   choicesgrpQ <- split(testDD$qvar, testDD$group, lex.order = FALSE)
#   choicesgrpQ <- choicesgrpQ[-1] # remove IDs and sequencing
#   choicesgrpQ <- head(choicesgrpQ, -1) # remove interviewer obs
#   choicesgrpQ
# }
# 
# # Create global grouped_questions variable
# grouped_questions <- get_groupedQs_I()
# 
# # Function to create country picker list
# get_country_picker_list <- function() {
#   countries <- unique(orig_indiv_data$B_COUNTRY_ALPHA)
#   country_names <- unique(orig_indiv_data$B_COUNTRY)
#   setNames(as.list(countries), country_names)
# }
# 
# # Initialize picker_country_list
# picker_country_list <- get_country_picker_list()
# 
# # Load world map with ISO_A3 codes
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# # Define dynamic list of available ISO-A3 countries
# available_iso <- unique(orig_indiv_data$B_COUNTRY_ALPHA)
# 
# # Filter map to show only available countries
# world_available <- world %>% filter(iso_a3 %in% available_iso)
# 
# # ########################################
# # # DATA VALIDATION CHECKS               #
# # ########################################
# 
# # Verify all required datasets are loaded
# required_datasets <- c("orig_indiv_data", "orig_country_data", "orig_codebook_data", "indiv_ordinal")
# missing_datasets <- setdiff(required_datasets, ls())
# 
# if (length(missing_datasets) > 0) {
#   stop(paste("The following required datasets are missing:", 
#              paste(missing_datasets, collapse = ", ")))
# }
# 
# # Verify critical columns exist
# if (!"B_COUNTRY_ALPHA" %in% names(orig_indiv_data)) {
#   stop("B_COUNTRY_ALPHA column missing from orig_indiv_data")
# }
# 
# if (!"ColLab" %in% names(orig_codebook_data)) {
#   stop("ColLab column missing from orig_codebook_data")
# }
# 
# # ########################################
# # # DEBUGGING HELPERS                    #
# # ########################################
# 
# # Print loaded datasets
# cat("Loaded datasets:\n")
# print(ls(pattern = "orig_|indiv_"))
# 
# # Print sample of question labels
# if (exists("grouped_questions") && length(grouped_questions) > 0) {
#   cat("\nSample question groups:\n")
#   print(names(grouped_questions)[1:min(3, length(grouped_questions))])
#   
#   cat("\nSample questions from first group:\n")
#   print(grouped_questions[[1]][1:min(3, length(grouped_questions[[1]]))])
# } else {
#   warning("grouped_questions not created successfully")
# }
# 
# # Print country list sample
# if (exists("picker_country_list") && length(picker_country_list) > 0) {
#   cat("\nSample countries:\n")
#   print(picker_country_list[1:min(5, length(picker_country_list))])
# } else {
#   warning("picker_country_list not created successfully")
# }


###################################################################################
###################################################################################
###################################################################################




regression_dep <- "Q6-Important in life: Religion"
regression_indep <- c("Q106-Incomes should be made more equal vs There should be greater incentives for indi","Q112-Perceptions of corruption in the country")
regression_country <- "New Zealand"
regression_sample <- 4000


# Get question IDs
dep_id <- get_question_id(regression_dep)
indep_ids <- unname(sapply(regression_indep, get_question_id))
indep_ids <- get_question_id(regression_indep)


view(indep_ids)


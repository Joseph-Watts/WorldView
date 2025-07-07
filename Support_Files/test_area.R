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




regression_dep <- "Q6-Important in life: Religion"
regression_indep <- c("Q106-Incomes should be made more equal vs There should be greater incentives for indi","Q112-Perceptions of corruption in the country")
regression_country <- "New Zealand"
regression_sample <- 4000


# Get question IDs
dep_id <- get_question_id(regression_dep)
indep_ids <- unname(sapply(regression_indep, get_question_id))
indep_ids <- get_question_id(regression_indep)


view(indep_ids)



###################################################################################
###################################################################################
###################################################################################

# # summary(orig_indiv_data[,4:6])
# 
# test_cty <- c("NZL", "GBR", "USA", "BRA", "IND")
# test_q <- "Q112"
# 
# x <- orig_indiv_data |>
#   filter(B_COUNTRY_ALPHA == test_cty) |>
#   select(all_of(test_q))
# 
# # summary(orig_indiv_data[,4:6])
# 
# summary(x)
# 
# skimr::skim(x)
# Hmisc::describe(x)
# psych::describe(x)
# 
# 
# 
# 
# orig_indiv_data %>%
#   filter(B_COUNTRY_ALPHA %in% test_cty) %>%
#   select(country = B_COUNTRY, response = !!test_q)


###################################################################################
###################################################################################
###################################################################################

# # Install packages if needed
# if (!require("visdat")) install.packages("visdat")
# if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("dplyr")) install.packages("dplyr")
# if (!require("tidyr")) install.packages("tidyr")
# 
# library(visdat)
# library(ggplot2)
# library(dplyr)
# library(tidyr)
# 
# # Create the plot
# vis_miss_type(mixed_data)
# 
# 
# # Visualize variable types with vis_dat
# vis_dat(orig_country_data) +
#   theme(axis.text.x = element_blank()) +
#   scale_fill_manual(values = c(
#     "numeric" = "#00C1DD",
#     "factor" = "#D90000",
#     "NA" = "#00FF00"
#   ))
# 



###################################################################################
###################################################################################
###################################################################################




for (packageName in required_packages) {
  print(packageName)
  # if (!requireNamespace(packageName)) {
  #   install.packages(packageName)
  # }
}



###################################################################################
###################################################################################
###################################################################################


DT::datatable(data = orig_country_data %>%
                mutate(across(where(is.numeric), ~ round(., 2))),
              options = list(scrollX = TRUE))




str(orig_country_data, list.len = ncol(orig_country_data))





TC <- orig_country_data
TV <- var_info

for (i in 3:421) { # from Q1 to Q290
  names(TC) <- sapply(names(TC), function(name) {
    if (name %in% names(TV$ColLab[i+1]) && !grepl("\\.", name)) {
      title_lookup[name]
    } else {
      name
    }
  })
}

TC

names(TC[,3:421])

TV$ColLab[5]


str(TC, list.len = ncol(TC))




































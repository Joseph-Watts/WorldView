test_data <- indiv_ordinal[indiv_ordinal$B_COUNTRY %in% c("Brazil", "New Zealand", "Canada", "China", "Australia", "India"), c("B_COUNTRY", "Q36", "Q49", "Q127", "Q172", "Q176", "Q192", "Q201", "Q221", "Q250", "Q251")]

bxplt_test_data <- test_data |>
  pivot_longer(cols = starts_with(c("Q", "E", "F_", "G_", "H_")),
               names_to = "question",
               values_to = "response")



aa <- orig_indiv_data %>% select(starts_with(c("B", "S", "E", "F_", "G_", "H_")))




ggplot(bxplt_test_data, aes(x = question, y = response, fill = B_COUNTRY)) +
  geom_boxplot(notch = TRUE) +
  labs(x = "Question", y = "Responses") +
  # theme_minimal() +
  theme(legend.position = "none")


anov_test <- aov(B_COUNTRY ~ ., data = test_data)

ctry_test <- aov(Q36 ~ B_COUNTRY, data = test_data)

summary(ctry_test)
tidy(ctry_test)
class(ctry_tes)

par(mfrow = c(2, 2))
plot(ctry_test)


summary(anov_test)

par(mfrow = c(2, 2))
plot(anov_test)
print(anov_test)

par(mfrow = c(1, 1))





pairwise.t.test(test_data[, 2],
                interaction(test_data$B_COUNTRY, test_data[, c(3, 4)]),
                p.adjust.method = "bonferroni")

aov(test_data[-1] ~ B_COUNTRY, data = test_data)


manova(cbind(test_data[, -1]) ~ B_COUNTRY, data = test_data)




# library(ggpubr)
# library(rstatix)
# library(datarium)

# 
# 
# 
# # boxplot ou violin plot
# # H0 and Ha
# 

# 
# set.seed(123)
# data("jobsatisfaction", package = "datarium")
# jobsatisfaction %>% sample_n_by(gender, education_level, size = 1)
# 
# jobsatisfaction %>%
#   group_by(gender, education_level) %>%
#   get_summary_stats(score, type = "mean_sd")
# 
# 
# bxp <- ggboxplot(
#   jobsatisfaction, x = "gender", y = "score",
#   color = "education_level", palette = "jco"
# )
# bxp
# 
# 
# 
# 
# 
# jobsatisfaction %>%
#   group_by(gender, education_level) %>%
#   identify_outliers(score)
# 
# # Build the linear model
# model  <- lm(score ~ gender*education_level,
#              data = jobsatisfaction)
# # Create a QQ plot of residuals
# ggqqplot(residuals(model))
# 
# # Compute Shapiro-Wilk test of normality
# shapiro_test(residuals(model))
# 
# 
# jobsatisfaction %>%
#   group_by(gender, education_level) %>%
#   shapiro_test(score)
# 
# ggqqplot(jobsatisfaction, "score", ggtheme = theme_bw()) +
#   facet_grid(gender ~ education_level)
# 
# jobsatisfaction %>% levene_test(score ~ gender*education_level)
# 
# 
# res.aov <- jobsatisfaction %>% anova_test(score ~ gender * education_level)
# res.aov
# 
# 
# # Group the data by gender and fit  anova
# model <- lm(score ~ gender * education_level, data = jobsatisfaction)
# jobsatisfaction %>%
#   group_by(gender) %>%
#   anova_test(score ~ education_level, error = model)
# 
# 
# # pairwise comparisons
# library(emmeans)
# pwc <- jobsatisfaction %>% 
#   group_by(gender) %>%
#   emmeans_test(score ~ education_level, p.adjust.method = "bonferroni") 
# pwc
# 
# res.aov
# 
# 
# jobsatisfaction %>%
#   pairwise_t_test(
#     score ~ education_level, 
#     p.adjust.method = "bonferroni"
#   )
# 
# model <- lm(score ~ gender * education_level, data = jobsatisfaction)
# jobsatisfaction %>% 
#   emmeans_test(
#     score ~ education_level, p.adjust.method = "bonferroni",
#     model = model
#   )
# 
# # Visualization: box plots with p-values
# pwc <- pwc %>% add_xy_position(x = "gender")
# bxp +
#   stat_pvalue_manual(pwc) +
#   labs(
#     subtitle = get_test_label(res.aov, detailed = TRUE),
#     caption = get_pwc_label(pwc)
#   )




# Check original missingness
original_missingness <- miss_var_summary(orig_indiv_data)
print(original_missingness)


sample_with_missing_ratio <- function(data, sample_size) {
  # Get the total number of rows in the dataset
  total_rows <- nrow(data)
  
  # Calculate the sampled dataset
  sampled_data <- data %>%
    # For each column, sample missing and non-missing rows proportionally
    reframe(across(everything(), ~ {
      missing_indices <- which(is.na(.))
      non_missing_indices <- which(!is.na(.))
      
      # Number of missing and non-missing rows to sample
      num_missing <- round(sample_size * length(missing_indices) / total_rows)
      num_non_missing <- round(sample_size * length(non_missing_indices) / total_rows)
      
      # Sample indices for missing and non-missing values
      sampled_missing <- sample(missing_indices, size = num_missing, replace = FALSE)
      sampled_non_missing <- sample(non_missing_indices, size = num_non_missing, replace = FALSE)
      
      # Combine the sampled values
      combined_indices <- sort(c(sampled_missing, sampled_non_missing))
      .[combined_indices] # Return the sampled values
    }))
  
  return(sampled_data)
}

sampled_data <- sample_with_missing_ratio(orig_indiv_data, sample_size = 2500)
vis_miss(sampled_data)










vis_miss(orig_country_data) +
  theme(axis.text.x = element_blank())



d <- sample_with_missing_ratio(orig_indiv_data, sample_size = 2500)

vis_miss(d, palette = c("lightgreen", "salmon")) +
  theme(axis.text.x = element_blank())


vis_miss(d, palette = c("skyblue", "salmon")) +
  theme(axis.text.x = element_blank())



vis_miss(d) +
  scale_fill_manual(values = c("Missing" = "salmon", "Not Missing" = "lightgreen")) +
  theme(axis.text.x = element_blank())



#==========================================================================
library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  tags$h1("checkboxGroupButtons examples"),
  
  prettyRadioButtons(
    inputId = "somevalue1",
    label = "Click me!",
    thick = TRUE,
    choices = c("Click me !", "Me !", "Or me !"),
    animation = "pulse",
    status = "info"
  ),
  verbatimTextOutput("value1"),
  
  prettyCheckbox(
    inputId = "checkbox2",
    label = "Click me!",
    thick = TRUE,
    animation = "pulse",
    status = "success"
  ),
  
  
  checkboxGroupButtons(
    inputId = "somevalue2",
    label = "With custom status:",
    choices = names(iris),
    status = "primary"
  ),
  verbatimTextOutput("value2"),
  
  checkboxGroupButtons(
    inputId = "somevalue3",
    label = "With icons:",
    choices = names(mtcars),
    checkIcon = list(
      yes = icon("square-check"),
      no = icon("square")
    )
  ),
  verbatimTextOutput("value3")
)

server <- function(input, output) {
  
  output$value1 <- renderPrint({ input$somevalue1 })
  output$value2 <- renderPrint({ input$somevalue2 })
  output$value3 <- renderPrint({ input$somevalue3 })
  
}

if (interactive())
  shinyApp(ui, server)


#=========================================================================
library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  # should be one of “Shiny”, “Flat”, “Big”, “Modern”, “Sharp”, “Round”, “Square”
  # use the Modern design
  chooseSliderSkin("Round", color = "#119946"),
  sliderInput("obs", "Customized slider1:",
              min = 0, max = 100, value = 50
  ),
  sliderInput("obs2", "Customized slider1:",
              min = 0, max = 100, value = 50
  ),
  sliderInput("obs3", "Customized slider1:",
              min = 0, max = 100, value = 50
  ),
  plotOutput("distPlot")
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
}

shinyApp(ui, server)

#=========================================================================
## Only run examples in interactive R sessions
if (interactive()) {
  
  library(shiny)
  library(shinyWidgets)
  
  ui <- fluidPage(
    uiOutput("vai"),
    tags$div(style = "height: 140px;"), # spacing
    verbatimTextOutput(outputId = "out"),
    verbatimTextOutput(outputId = "state")
  )
  
  server <- function(input, output, session) {
    
    output$vai <- renderUI({
      dropdownButton(
        inputId = "mydropdown",
        label = "Controls",
        icon = icon("sliders"),
        status = "primary",
        circle = FALSE,
        sliderInput(
          inputId = "n",
          label = "Number of observations",
          min = 10, max = 100, value = 30
        ),
        prettyToggle(
          inputId = "na",
          label_on = "NAs keeped",
          label_off = "NAs removed",
          icon_on = icon("check"),
          icon_off = icon("xmark")
        )
      )
    })
    
    output$out <- renderPrint({
      cat(
        " # n\n", input$n, "\n",
        "# na\n", input$na
      )
    })
    
    output$state <- renderPrint({
      cat("Open:", input$mydropdown_state)
    })
    
  }
  
  shinyApp(ui, server)
  
}
#==============================================================================


library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  tags$h2("Virtual Select"),
  
  fluidRow(
    column(
      width = 4,
      virtualSelectInput(
        inputId = "single",
        label = "Single select :",
        choices = month.name,
        search = TRUE
      ),
      virtualSelectInput(
        inputId = "multiple",
        label = "Multiple select:",
        choices = setNames(month.abb, month.name),
        multiple = TRUE
      ),
      virtualSelectInput(
        inputId = "onclose",
        label = "Update value on close:",
        choices = setNames(month.abb, month.name),
        multiple = TRUE,
        updateOn = "close"
      )
    ),
    column(
      width = 4,
      tags$b("Single select :"),
      verbatimTextOutput("res_single"),
      tags$b("Is virtual select open ?"),
      verbatimTextOutput(outputId = "res_single_open"),
      
      tags$br(),
      
      tags$b("Multiple select :"),
      verbatimTextOutput("res_multiple"),
      tags$b("Is virtual select open ?"),
      verbatimTextOutput(outputId = "res_multiple_open"),
      
      tags$br(),
      
      tags$b("Update on close :"),
      verbatimTextOutput("res_onclose"),
      tags$b("Is virtual select open ?"),
      verbatimTextOutput(outputId = "res_onclose_open")
    )
  )
  
  
)

server <- function(input, output, session) {
  
  output$res_single <- renderPrint(input$single)
  output$res_single_open <- renderPrint(input$single_open)
  
  output$res_multiple <- renderPrint(input$multiple)
  output$res_multiple_open <- renderPrint(input$multiple_open)
  
  output$res_onclose <- renderPrint(input$onclose)
  output$res_onclose_open <- renderPrint(input$onclose_open)
  
}

if (interactive())
  shinyApp(ui, server)
# labelRenderer example ----

library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      function colorText(data) {
        let text = `<span style='color: ${data.label};'>${data.label}</span>`;
        return text;
      }"
    )),
  ),
  tags$h1("Custom LabelRenderer"),
  br(),
  fluidRow(
    column(
      width = 6,
      virtualSelectInput(
        inputId = "search",
        label = "Color picker",
        choices = c("red", "blue", "green", "#cbf752"),
        width = "100%",
        keepAlwaysOpen = TRUE,
        labelRenderer = "colorText",
        allowNewOption = TRUE
      )
    )
  )
  
)

server <- function(input, output, session) {}

if (interactive())
  shinyApp(ui, server)

# onServerSearch example ----

library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  tags$head(
    tags$script(HTML(r"(
      // Main function that is called
      function searchLabel(searchValue, virtualSelect) {
        // Words to search for - split by a space
        const searchWords = searchValue.split(/[\s]/);

        // Update visibility
        const found = virtualSelect.options.map(opt => {
          opt.isVisible = searchWords.every(word => opt.label.includes(word));
          return opt;
        });

        virtualSelect.setServerOptions(found);
        }
      )"
    )),
  ),
  tags$h1("Custom onServerSearch"),
  br(),
  fluidRow(
    column(
      width = 6,
      virtualSelectInput(
        inputId = "search",
        label = "Better search",
        choices = c("This is some random long text",
                    "This text is long and looks differently",
                    "Writing this text is a pure love",
                    "I love writing!"
        ),
        width = "100%",
        keepAlwaysOpen = TRUE,
        search = TRUE,
        autoSelectFirstOption = FALSE,
        onServerSearch = "searchLabel"
      )
    )
  )
  
)

server <- function(input, output, session) {}

if (interactive())
  shinyApp(ui, server)

#================================================================================
library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  tags$h2("noUiSliderInput example"),
  
  noUiSliderInput(
    inputId = "noui1",
    min = 0, max = 100,
    value = 20
  ),
  verbatimTextOutput(outputId = "res1"),
  
  tags$br(),
  
  noUiSliderInput(
    inputId = "noui2", label = "Slider vertical:",
    min = 0, max = 1000, step = 50,
    value = c(100, 400), margin = 100,
    orientation = "vertical",
    width = "100px", height = "300px"
  ),
  verbatimTextOutput(outputId = "res2")
)

server <- function(input, output, session) {
  
  output$res1 <- renderPrint(input$noui1)
  output$res2 <- renderPrint(input$noui2)
  
}

if (interactive())
  shinyApp(ui, server)


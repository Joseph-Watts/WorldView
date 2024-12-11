#' Work in progress
#' 
#' Code based on: https://shiny.posit.co/r/gallery/life-sciences/biodiversity-national-parks/
#' WVS data source: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp

#####################
# SUPPORT FUNCTIONS #
#####################

#' Currently none

# ##################
# # DATA WRANGLING # - move to global.R
# ##################

# Currently done within Server Logic

################
# SERVER LOGIC #
################

shinyServer(
  
  function(input, output, session) {
    session$onSessionEnded(function() {
      stopApp()
    })
    
    ########################
    # Read in data files   
    ########################
    
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
    
    # WVS7_Individual.rds modified to have concatenated IDs 
    get_I_longID <- reactive({
      d.I <- get_I_data()
      d.var_info <- get_var_info()
      
      for (i in 4:293) {
        names(d.I)[i] <- d.var_info$ColLab[i]
      }
      d.I
    })
    
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
    
    # Fetch just sections
    get_sectionsOrd <- reactive({
      var_info <- get_var_info()
      sections <- as.list(unique(var_info$Section))
      sections_ord <- unique(factor(var_info$Section, ordered = TRUE, levels = sections))
      sections_ord
    })
    
    #' #' Grouping Questions by section for input options
    get_groupedQs_I <- reactive({
      var_info <- get_var_info()
      sections <- as.list(unique(var_info$Section))
      sections_ord <- factor(var_info$Section, ordered = TRUE, levels = sections)
      testDD <- data.frame(group = sections_ord,
                           qvar = var_info$ColLab)
      choicesgrpQ <- split(testDD$qvar, testDD$group, lex.order = FALSE)
      choicesgrpQ <- choicesgrpQ[-1]
      choicesgrpQ
    }) # this function can be copied to get countries
    
    
    
    
    
    ########################
    # PDF VIEW
    ########################
    
    # Master Survery Questionnaire PDF
    output$pdfview <- renderUI({
      tags$iframe(style = "height:100vh; width:100%; scrolling=yes",
                  src = "F00011012-WVS_WAVE_7_MASTER_QUESTIONNAIRE_2017-2021_ENGLISH.pdf")
    })
    
    
    
    
    
    ########################
    # DataTable
    ########################
    
    output$DTchoice <- renderUI ({
      radioButtons(
        "DTdata",
        "Choose Dataset",
        choices = c("Country", "Individuals"),
        selected = "Country"
      )
    })
    
    output$Table <- DT::renderDataTable({
      if (input$DTdata == "Country") {
        #DTdata<-d_country
        DTdata <- get_C_data()
      } else if (input$DTdata == "Individuals") {
        #DTdata<-d_ind_longID
        DTdata <- get_I_longID()
      }
      DT::datatable(data = DTdata, options = list(scrollX = TRUE))
    })
    

    
    
    
    
    
    ########################
    # Within country 
    ########################
    
    # Reactive control for selected required country
    output$wc_country_sel <- renderUI({
      selectInput(
        inputId = "wc_c_select",
        label = "Select Country:",
        choices = get_countries(),
        selected = "New Zealand"
      )
    })
    
    # Give total count of observations for selected country
    output$c_total_obs <- renderTable({
      d <- get_I_longID() |>
        filter(B_COUNTRY == input$wc_c_select) |>
        summarise(Observations = n())
      d
    })
    
    # Reactive control for selecting question A
    output$wc_qA <- renderUI({
      selectInput(
        inputId = "wc_sel_qA",
        label = "Select Question A",
        choices = get_groupedQs_I(),
        selected = get_groupedQs_I()[[1]][1]
      )
    })
    
    # Reactive control for selecting question B
    output$wc_qB <- renderUI({
      selectInput(
        inputId = "wc_sel_qB",
        label = "Select Question B",
        choices = get_groupedQs_I(),
        selected = get_groupedQs_I()[[1]][2]
      )
    })
    
    # get the data for the chosen country and questions 
    get_country_data <- reactive({
      d <- get_I_longID() |>
        filter(B_COUNTRY == input$wc_c_select) |>
        select(input$wc_sel_qA, input$wc_sel_qB)
      d
    })
    
    # Show count stats for Question A factor levels
    output$stats_wc_qA <- renderTable({
      get_country_data() |>
        group_by(.data[[input$wc_sel_qA]]) |>
        summarise(n = n())
    })
    
    # Show count stats for Question B factor levels
    output$stats_wc_qB <- renderTable({
      get_country_data() |>
        group_by(.data[[input$wc_sel_qB]]) |>
        summarise(n = n())
    })
    
    # Plot Question A factor level counts
    output$plot_wc_qA_levels <- renderPlot({
      ggplot(get_country_data(),
             aes(x = .data[[input$wc_sel_qA]], fill = .data[[input$wc_sel_qA]])) +
        geom_bar() +
        ggtitle("Factor levels by frequency") +
        theme_minimal()
    })
    
    # Plot Question A factor level proportions
    output$plot_wc_qA_prop <- renderPlot({
      ggplot(get_country_data(),
             aes(x = '', fill = .data[[input$wc_sel_qA]])) +
        geom_bar(position = "fill") +
        ggtitle("Factor level proportions") +
        labs(x = input$wc_sel_qA, y = "proportion")
    })
    
    # Plot Question B factor level counts
    output$plot_wc_qB_levels <- renderPlot({
      ggplot(get_country_data(),
             aes(x = .data[[input$wc_sel_qB]], fill = .data[[input$wc_sel_qB]])) +
        geom_bar() +
        ggtitle("Factor levels by frequency") +
        theme_minimal()
    })
    
    # Plot Question B factor level proportions
    output$plot_wc_qB_prop <- renderPlot({
      ggplot(get_country_data(),
             aes(x = '', fill = .data[[input$wc_sel_qB]])) +
        geom_bar(position = "fill") +
        ggtitle("Factor level proportions") +
        labs(x = input$wc_sel_qB, y = "proportion")
    })
    
    
    # Show comparison stats for each question grouped by the other
    output$stats_wc_qAqB <- renderTable({
      get_country_data() |>
        group_by(.data[[input$wc_sel_qA]], .data[[input$wc_sel_qB]]) |>
        summarise(n = n())
    }) 
    
    output$stats_wc_qBqA <- renderTable({
      get_country_data() |>
        group_by(.data[[input$wc_sel_qB]], .data[[input$wc_sel_qA]]) |>
        summarise(n = n())
    }) 
    
    within_country_compare <- reactive({
      comp_d <- get_country_data() |> na.omit()
      
      v1 <- input$wc_sel_qA
      v2 <- input$wc_sel_qB
      
      v1_class <- paste0(class(comp_d[,v1]), collapse = "")
      v2_class <- paste0(class(comp_d[,v2]), collapse = "")
      v_classes <- c(v1_class, v2_class)
      
      #' If both variables are factors (whether ordered or not)
      if(sum(v_classes == "integer") == 0){
        
        #' Heat map
        v_plot <- ggplot(comp_d, 
                         aes(x=.data[[v1]],
                             y=.data[[v2]])) + 
          geom_bin_2d() +
          theme_minimal()
        
        v_table <- tbl_summary(comp_d)
        
        #' If both factors are ordered perform a kendall cor.test
        if(sum(v_classes == "orderedfactor") == 2){
          
          comp_d_int <- comp_d
          comp_d_int[,v1] <- as.integer(comp_d_int[,v1])
          comp_d_int[,v2] <- as.integer(comp_d_int[,v2])
          
          v_stats <- cor.test(comp_d_int[,v1], 
                              comp_d_int[,v2],
                              method = "kendall")
          
          #' Otherwise, perform a chisq.test
        }else{
          
          v_stats <- chisq.test(comp_d[,v1], comp_d[,v2])
          
        }
        
        # If only one variables is an integer
      }else if(sum(v_classes == "integer") == 1){
        
        #' First, make sure that v2 is treated as the integer no matter the order
        #' that it is put in
        if(v1_class == "integer"){
          v1_orig <- v1
          v1 <- v2
          v2 <- v1_orig
        }
        
        #' Violin plot
        v_plot <- ggplot(comp_d, 
                         aes(x = .data[[v1]],
                             y = .data[[v2]],
                             fill = .data[[v1]])) + 
          geom_violin(trim = FALSE) +
          geom_jitter(shape = 16, 
                      position = position_jitter(0.1),
                      alpha = 0.2) +
          scale_fill_brewer(palette = "Pastel2") +
          theme_minimal()
        
        v_table <- tbl_summary(comp_d)
        
        if("factor" %in% v_classes){
          
          v_stats <- kruskal.test(as.formula(paste(v1, "~", v2)),
                                  data = comp_d)
          
        }else if("orderedfactor" %in% v_classes){
          
          comp_d_int <- comp_d
          comp_d_int[,v1] <- as.integer(comp_d_int[,v1])
          
          v_stats <- cor.test(comp_d_int[,v1], 
                              comp_d_int[,v2],
                              method = "kendall")
          
        }
        
        #' If both variables are integers
      } else if(sum(v_classes == "integer") == 2){
        
        #' Scatter plot with jitter
        v_plot <- ggplot(comp_d, 
                         aes(x = .data[[v2]],
                             y = .data[[v1]])) + 
          geom_point() +
          geom_jitter(shape = 16, 
                      position = position_jitter(0.4),
                      alpha = 0.4) +
          geom_smooth(method=lm) +
          theme_minimal()
        
        v_table <- tbl_summary(comp_d)
        
        comp_d_int <- comp_d
        comp_d_int[,v1] <- as.integer(comp_d_int[,v1])
        comp_d_int[,v2] <- as.integer(comp_d_int[,v2])
        
        v_stats <- cor.test(comp_d_int[,v1], 
                            comp_d_int[,v2],
                            method = "kendall")
        
      }
      
      return(list("plot" = v_plot, 
                  "table" = v_table, 
                  "stats" = v_stats))
      
    })
    
    ############################ TEST SECTION #################################
    
    output$test_output_plot <- renderPlot({
      test_output <- within_country_compare()
      test_output$plot
    })
    
    output$test_output_table <- renderTable({
      test_output <- within_country_compare()
      test_output$table
    })
    
    output$test_output_stats <- renderPrint({
      test_output <- within_country_compare()
      print(test_output$stats)
    })
    
    ###########################################################################
 
    ########################
    # Between Countries
    ########################
    
    # Reactive control for selecting BC question
    output$bc_question <- renderUI({
      selectInput(
        inputId = "bc_sel_q",
        label = "Select Question",
        choices = get_groupedQs_I(),
        selected = get_groupedQs_I()[[1]][1]
      )
    })
    
    # Reactive control for selecting Country A
    output$bc_countryA <- renderUI({
      selectInput(
        inputId = "bc_sel_cA",
        label = "Select Country:",
        choices = get_countries(),
        selected = "New Zealand"
      )
    })
    
    
    # Reactive control for selecting Country B
    output$bc_countryB <- renderUI({
      selectInput(
        inputId = "bc_sel_cB",
        label = "Select Country:",
        choices = get_countries(),
        selected = "Argentina"
      )
    })
    
    # Get question data for Country A 
    get_countryA_data <- reactive({
      # d <- get_I_longID() |>
      #   filter(B_COUNTRY == input$wc_c_select) |>
      #   select(input$wc_sel_qA,input$wc_sel_qB)
      # d
      d <- get_I_longID() |>
        filter(B_COUNTRY == input$bc_sel_cA) |>
        select(input$bc_sel_q)
      d
    }) 
    
    # Give total count of observations for selected country
    output$cA_total_obs <- renderTable({
      d <- get_countryA_data() |>
        summarise(Observations = n())
      d
    })
    
    # Get question data for Country A 
    get_countryB_data <- reactive({
      # d <- get_I_longID() |>
      #   filter(B_COUNTRY == input$wc_c_select) |>
      #   select(input$wc_sel_qA,input$wc_sel_qB)
      # d
      d <- get_I_longID() |>
        filter(B_COUNTRY == input$bc_sel_cB) |>
        select(input$bc_sel_q)
      d
    })
    
    # Give total count of observations for selected country
    output$cB_total_obs <- renderTable({
      d <- get_countryB_data() |>
        summarise(Observations = n())
      d
    })
    
    # Show count stats for Question Country A factor levels
    output$stats_bc_cA <- renderTable({
      get_countryA_data() |>
        group_by(.data[[input$bc_sel_q]]) |>
        summarise(n = n())
    }) 
    
    # Show count stats for Question Country B factor levels
    output$stats_bc_cB <- renderTable({
      get_countryB_data() |>
        group_by(.data[[input$bc_sel_q]]) |>
        summarise(n = n())
    }) 
    
    #Plot Question factor level counts for Country A
    output$plot_bc_qcA_levels <- renderPlot({
      ggplot(get_countryA_data(),
             aes(x = .data[[input$bc_sel_q]], fill = .data[[input$bc_sel_q]])) + #need to figure out this line
        geom_bar() +
        ggtitle("Country A: Question factor levels by frequency") +
        theme_minimal()
    })
    
    # Plot Question factor level proportions Country A
    output$plot_bc_qA_prop <- renderPlot({
      ggplot(get_countryA_data(),
             aes(x = '', fill = .data[[input$bc_sel_q]])) +
        geom_bar(position = "fill") +
        ggtitle("Factor level proportions") +
        labs(x = input$bc_sel_q, y = "proportion")
    })
    
    #Plot Question factor level counts for Country B
    output$plot_bc_qcB_levels <- renderPlot({
      ggplot(get_countryB_data(),
             aes(x = .data[[input$bc_sel_q]], fill = .data[[input$bc_sel_q]])) + #need to figure out this line
        geom_bar() +
        ggtitle("Country B: Question factor levels by frequency") +
        theme_minimal()
    })
    
    # Plot Question factor level proportions Country B
    output$plot_bc_qB_prop <- renderPlot({
      ggplot(get_countryB_data(),
             aes(x = '', fill = .data[[input$bc_sel_q]])) +
        geom_bar(position = "fill") +
        ggtitle("Factor level proportions") +
        labs(x = input$bc_sel_q, y = "proportion")
    })
    
    
    
    #' ---
    #' Global
    #' 
    #' This section looks at the global relationship between two variables. 
    #' 
    #' The country level dataset is very simple in that it provides country level 
    #' mean or proportions. Variation within countries has not been accounted for.
    #' 
    #' This is probably ok for the current simple purposes of the app. 
    #' More sophistication could be added in later.
    #' 

    # Reactive to get the filtered questions for varA
    filtered_choices_A <- reactive({
      # Get the list of all questions
      all_questions <- get_groupedQs_I()
      
      # Exclude the question selected in varB
      selected_B <- input$gbl_sel_varB
      all_questions[!all_questions %in% selected_B]
    })
    
    # Reactive to get the filtered questions for varB
    filtered_choices_B <- reactive({
      # Get the list of all questions
      all_questions <- get_groupedQs_I()
      
      # Exclude the question selected in varA
      selected_A <- input$gbl_sel_varA
      all_questions[!all_questions %in% selected_A]
    })
    
    
    # Reactive control for selecting first variable
    output$global_varA <- renderUI({
      selectInput(
        inputId = "gbl_sel_varA",
        label = "Select Question A",
        choices = filtered_choices_A(),
        selected = get_groupedQs_I()[[1]][1]
      )
    })
    
    # Reactive control for selecting second variable
    output$global_varB <- renderUI({
      selectInput(
        inputId = "gbl_sel_varB",
        label = "Select Question B",
        choices = filtered_choices_B(),
        selected = get_groupedQs_I()[[1]][2]
      )
    })
    
    gblvar1 <- reactive({
      var_info <- get_var_info()
      gblvar1_lookup <- var_info[which(var_info$ColLab == input$gbl_sel_varA), ]
      gv1input <- gblvar1_lookup$Col_ID
    })
    
    # gblvar2 <- reactive({
    #   gblvar2_lookup <- var_info[which(var_info$ColLab == input$Selected_Global_Var_2),]
    #   gv2input <- gblvar2_lookup$Col_ID
    # })
    
    gblvar2 <- reactive({
      var_info <- get_var_info()
      gblvar2_lookup <- var_info[which(var_info$ColLab == input$gbl_sel_varB), ]
      gv2input <- gblvar2_lookup$Col_ID
    })
    
    
    #' This reactions sections doesn't seem necessary.
    #' Something will need to change here to determine the correct plot type
    #' based on the 
    d_global <- reactive({
      d_country <- get_C_data()
      d_country[, c(gblvar1(), gblvar2())]
    })
    
    #' Here it would be good to add:
    #' 1. Global plot for variable 1
    #' 2. Global plot for variable 2
    #' 3. Summary information for each variable
    #' 4. Basic statistical test of the relationship between the variables
    
    #' This will need adapted to handle the different possible formats of the
    #' selected variables. Currently variables can be:
    #' Factor (responses at the country level have been split into different rows)
    #' Ordinal Factor (treated as integer and then mean)
    #' Integer (mean)
    
    output$global_p1 <- renderPlot({
      ggplot(d_global(), aes(x = .data[[gblvar1()]], y = .data[[gblvar2()]])) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        theme_minimal()
    })
    
    
    # Load world shapefile data from rnaturalearth
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Render list of countries in data set
    output$pickRegion <- renderUI({
      pickerInput(
        inputId = "pickRegion",
        label = "Select one or more countries",
        choices = picker_country_list,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selectAllText` = "Select all countries"
        )
      )
    })
    
    # Render topic of questions to be shown to user
    output$pickTopic <- renderUI({
      pickerInput(
        "pickTopic",
        "Select topic:",
        choices = get_sectionsOrd(),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      )
    })
    
    # Render the leaflet map
    output$worldMap <- renderLeaflet({
      # Get selected countries from the input picker
      selected_countries <- input$pickRegion
      
      # If selected countries exist, filter them; otherwise, use all countries
      if (length(selected_countries) > 0) {
        highlighted_countries <- world %>%
          dplyr::filter(name_en %in% selected_countries) # TODO a few countries do not get selected if 'select all' pressed. redo this line later
      } else {
        highlighted_countries <- world %>%
          dplyr::filter(iso_a3 %in% WVS7_part_countries$B_COUNTRY_ALPHA) # Use the entire dataset if no countries are selected
      }
      
      # Render the map
      leaflet(options = leafletOptions(
        zoomControl = FALSE,
        minZoom = 1,
        maxZoom = 1,
        dragging = FALSE
      )) %>%
        addProviderTiles("CartoDB.PositronNoLabels") %>%
        # addTiles() %>%
        # setView(lng = 174.8, lat = -36, zoom = 1) %>%
        setView(lng = 0, lat = 0, zoom = 1) %>%
        addPolygons(
          data =  highlighted_countries,
          # TODO make highlighted_countries a reactive function
          color = "green",
          weight = 1,
          fillColor = "lightgreen",
          fillOpacity = 0.75,
          label = ~ name
        )
    })
    
    # output$teste1 <- renderUI({
    #   selectInput("tstcolor", "Choose a color:", 
    #               choices = c("Red", "Green", "Blue"))
    # })
    # 
    # 
    # output$teste2 <- renderUI({
    #   selectInput("tstnumber", "Choose a number:", 
    #               choices = c("one", "two", "three"))
    # })
    
    
    # TODO add vis_miss_ly code provided by Nick
    # vis_miss
    output$Missing <- renderPlot({
      vis_miss(get_C_data(),
               cluster = input$cluster)
    })
    
    output$Missing_sampled <- renderPlot({
      downsized_data <- get_I_data() %>%
        sample_n(10000) %>%    # Select 10,000 random rows
        select(sample(1:ncol(.), 50)) # Select 50 random columns, but in the future that will change to user-selected columns showing all 100k rows
      
      vis_miss(downsized_data, cluster = input$sampled_cluster)
    })

  }) # end server

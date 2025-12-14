# modules/phylogeny/map/map_server.R

phylo_map_server <- function(id,
                             wvs_country,
                             codebook_data,
                             world_shape) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. map_var 可选变量
    observe({
      num_vars <- names(wvs_country)[sapply(wvs_country, is.numeric)]
      updateSelectInput(session, "map_var",
                        choices = num_vars,
                        selected = num_vars[1])
    })
    
    # Render variable description
    output$var_description <- renderUI({
      render_variable_description(
        data_type = "WVS",
        outcome_var = input$map_var,
        codebook_data = codebook_data
      )
    })
    
    # 2. 合并地图和 WVS 数据
    map_data <- reactive({
      req(input$map_var)
      join_world_wvs(
        world_shape   = world_shape,
        country_data  = wvs_country,
        var           = input$map_var
      )
    })
    
    # 3. 绘制 leaflet 地图
    output$map <- renderLeaflet({
      req(map_data())
      plot_wvs_world_map(
        world_sf  = map_data(),
        var       = input$map_var,
        palette   = "Viridis"
      )
    })
  })
}
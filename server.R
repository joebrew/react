function(input, output, session) {
  
  # Get a reactive object of district
  district <- reactive({
    district_static %>%
      filter(date >= yesterday - (input$days - 1),
             date < today)
  })
  
  # Get a reactive object for malaria
  malaria <- reactive({
    district() %>%
      filter(malaria)
  })
  
  # Number of days back
  n_days <- reactive({
    input$days
  })
  # Text for number of days back
  output$n_days_text <- renderText({
    paste0('Malaria cases over last ', 
           n_days(),
           ' days')
  })
  
  # Number with missing geo
  n_missing_geo <- reactive({
    length(which(is.na(malaria()$lng)))
  })
  # Text output for missing geo
  output$n_missing_geo_text <- renderText({
    paste0('Excluding those ',
           n_missing_geo(),
           ' with no geographic information')
  })
  
  # Render a time series chart
  output$ts <- renderPlot({
    dd <- district() %>%
      group_by(date,
               malaria = ifelse(malaria, 'Malaria', 'Other')) %>%
      tally()
    ggplot(data = dd,
           aes(x = date,
               y = n,
               group = malaria,
               color = malaria)) +
      geom_line(alpha = 0.5,
                lwd = 2) +
      theme_economist() +
      scale_color_manual(name = '',
                         values = c('darkred', 'darkblue')) +
      labs(title = 'Trend',
           x = 'Date',
           y = 'Cases')
    # right <- district() %>%
    #   filter(malaria) %>%
    #   group_by(date) %>%
    #   tally %>%
    #   rename(malaria = n)
    # left <- district() %>%
    #   filter(!malaria) %>%
    #   group_by(date) %>%
    #   tally %>%
    #   rename(`Not malaria` = n) 
    # dd <- left_join(left, right)
    # m1 <- mPlot(x = 'date', 
    #             y = c('malaria', 'not malaria'), type = 'Line',
    #             data = dd)
    # m1$set(pointSize = 0, lineWidth = 1)
    # m1
  })
  
  # Render a map
  output$leaflet_map <-
  renderLeaflet({
    dd <- malaria()
    l <- leaflet() %>%
      addPolygons(data = area_map,
                   color = area_map@data$color,
                   weight = 2) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addMarkers(lng = dd$lng,
                 lat = dd$lat,
                 # clusterId = "quakesCluster",
                 clusterOptions = markerClusterOptions()) %>%
      addGraticule(interval = 40, 
                   style = list(color = "#FF0000", 
                                weight = 1)) %>%
      addLayersControl(overlayGroups = c("Graticule"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE)
      l
  }
  )
  
  # Render a map for react
  output$react_map <-
    renderLeaflet({
      dd <- malaria() %>%
        sample_n(30, replace = TRUE)
      l <- leaflet() %>%
        # addPolygons(data = area_map,
        #             color = area_map@data$color,
        #             weight = 2) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        addMarkers(lng = dd$lng,
                   lat = dd$lat) %>%
        addGraticule(interval = 40, 
                     style = list(color = "#FF0000", 
                                  weight = 1)) %>%
        addLayersControl(overlayGroups = c("Graticule"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMiniMap(
          tiles = providers$Esri.WorldStreetMap,
          toggleDisplay = TRUE)
      l
    }
    )
  
  # Render a map for forecast
  output$forecast_map <-
    renderLeaflet({
      cols <- colorRampPalette(brewer.pal(9, 'Reds'))(nrow(area_map))
      l <- leaflet() %>%
        addPolygons(data = area_map,
                    col = cols,
                    fillColor= cols,
                    fillOpacity = 0.8,
                    weight = 2,
                    opacity = 0.8) %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
        addMiniMap(
          tiles = providers$Esri.WorldStreetMap,
          toggleDisplay = TRUE)
      l
    }
    )
  
  output$forecast_plot <- renderPlot({
    dates <- seq(Sys.Date(), 
                 Sys.Date() + 20,
                 by = 1)
    value <- rnorm(mean = 12, n = length(dates),
                   sd = 3)
    upr <- value + rnorm(mean = 3, n = length(dates)) + 
      seq(0, 6, length = length(dates))
    lwr <- value - rnorm(mean = 3, n = length(dates)) - 
      seq(0,6, length = length(dates))
    df <- data_frame(date = dates,
                     value = value,
                     upr = upr,
                     lwr = lwr)
    df$value <- (df$upr + df$lwr) / 2
    ggplot(data = df,
           aes(x = date,
               y= value,
               ymax = upr,
               ymin = lwr)) +
      geom_line(lwd = 2, alpha = 0.6,
                color = 'darkred') +
      geom_linerange(alpha = 0.6,
                     color = 'darkred') +
      theme_economist() +
      labs(title = 'Predicted Magude malaria cases',
           subtitle = 'Based on meterological model',
           x = 'Date',
           y = 'Cases')
  })
  
  
  # Create a reactive value for coloring
  cases_status <- reactive({
    avg_cases_lately <-
      length(which(district()$malaria)) / 
      length(unique(district()$date))
    ifelse(avg_cases_lately > (avg_cases * 2),
           'danger',
           ifelse(avg_cases_lately > avg_cases,
                  'warning',
                  'info'))
  })
  
  # Record the time that the session started.
  start_time <- as.numeric(Sys.time())
  
  # Example plot
  output$plot1 <- renderPlot({
    barplot(1:10)
  })
  
  # Number of cases yesterday
  output$cases <- renderValueBox({
    dd <- malaria()
    dd <- dd %>% filter(date == yesterday)
    st <- ifelse(nrow(dd) == 1, 'malaria case yesterday',
                 'malaria cases yesterday')
    valueBox(
      value = nrow(dd),
      color = 'red',
      icon = icon("users"),
      subtitle = st)})
  
  # Number of cases
  output$cases_range <- renderValueBox({
    dd <- malaria()
    stn <- ifelse(nrow(dd) == 1, 'cases', 'cases')
    st <- paste0('Malaria ', stn, ' over last ',
                 input$days, ' days')
    valueBox(
      value = nrow(dd),
      color = 'orange',
      icon = icon("stats", lib = 'glyphicon'),
      subtitle = st)})
  
  output$deaths <- renderValueBox({
    valueBox(
      value = 0,
      color = 'olive',
      icon = icon("skull", lib = "glyphicon"),
      subtitle = paste0('deaths over last ', input$days, ' days'))})
  
  output$action <- renderValueBox({
    valueBox(
      value = 5,
      color = 'blue',
      icon = icon("home", lib = "glyphicon"),
      subtitle = 'households require immediate action')})
  
  output$deviation <- renderValueBox({
    valueBox(
      value = 2,
      color = 'yellow',
      icon = icon("fire", lib = "glyphicon"),
      subtitle = 'health facilities have deviated from protocol')})
  
  output$reduction <- renderValueBox({
    valueBox(
      value = 1025,
      color = 'green',
      icon = icon("fire", lib = "glyphicon"),
      subtitle = 'cases averted last month (relative to Manhiça)')})
  
  
  output$bubbles_plot <- renderBubbles({
    if (nrow(district()) == 0){
      message('No rows in district')
      return()
    } else {
      message('Some rows in district')
      out <- district() %>%
        group_by(sex = ifelse(sex == '1', 'Male',
                              ifelse(sex == '2', 'Female', 
                                     'Unknown'))) %>%
        tally %>%
        mutate(sex = ifelse(is.na(sex), 'Unknown', sex))
      
      bubbles(value = out$n,
              color = 'darkgrey',
              label = out$sex,
              key = out$sex)
    }
  })
  
  output$react_scenarios <- renderTable({
    out <- data_frame(facility = c('Magude Sede',
                            'Place B', 
                            'Place C'),
               scenario = c(2, 2, 3))
    names(out) <- toupper(names(out))
    out
  }, digits = 1)
  
  output$downloadCsv <- downloadHandler(
    filename = "raw_data.csv",
    content = function(file) {
      write.csv(district(), file)
    },
    contentType = "text/csv"
  )
  
  output$rawtable <- renderPrint({
    if(input$malaria_only){
      the_data <- malaria()
    } else {
      the_data <- district()
    }
    orig <- options(width = 1000)
    print(tail(the_data, input$maxrows))
    options(orig)
  })
}


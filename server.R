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
  
  # Get reactive objects for magude stuff
  case_index <- reactive({
    case_index_static %>%
      filter(date >= yesterday - (input$days - 1),
             date < today)
  })
  agregados <- reactive({
    agregados_static %>%
      filter(date >= yesterday - (input$days - 1),
             date < today)
  })
  membros <- reactive({
    membros_static %>%
      filter(date >= yesterday - (input$days - 1),
             date < today)
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
  
  # Render a trend chart for Magude
  output$magude_trend <- 
    renderPlot({
      ggplot(data = magude_cases_all,
             aes(x = date,
                 y = n)) +
        geom_area(fill = 'darkorange', 
                  alpha = 0.6,
                  color = 'black') +
        geom_point(color = 'black', alpha = 0.7, size = 0.2) +
        labs(x = 'Date',
             y = 'Cases',
             title = 'Trend',
             subtitle = 'All Magude cases (index + actively detected)') +
        theme_fivethirtyeight()
    })
  
  # Plot of cases by health facility magude
  output$magude_by_health_facility <- 
    renderPlot({
      x <- magude_cases_by_hf %>%
        filter(date >= today - 30) %>%
        group_by(administrativePost) %>%
        summarise(n = sum(n))
      
      ggplot(data = x,
             aes(x = administrativePost,
                 y = n)) +
        geom_bar(stat = 'identity',
                 fill = 'darkorange',
                 alpha = 0.6,
                 color = 'black') +
        labs(title = 'Cases by administrative post',
             subtitle = 'Last 30 days',
             x = 'Administrative post',
             y = 'Cases') +
        geom_hline(yintercept = (10 / 7) * 30,
                   lty = 2,
                   alpha = 0.6) +
        theme_fivethirtyeight()
      
    })
  
  # magude cases map
  output$magude_map <- 
    renderLeaflet({
      mag <- area_map[area_map$NAME_2 == 'Magude',]
      fake_data <- coordinates(mag)
      fake_data <- data.frame(fake_data)
      names(fake_data) <- c('lng', 'lat')
      nrf <- nrow(fake_data)
      for (i in 1:100){
        fake_data[nrf + i,] <-
          c(jitter(fake_data$lng[sample(1:nrf,1)]),
            jitter(fake_data$lat[sample(1:nrf,1)]))
      }
      
      l <- leaflet() %>%
        addPolygons(data = mag,
                    color = 'blue',
                    weight = 2) %>%
        addProviderTiles(providers$Esri.WorldStreetMap) %>%
        addMarkers(lng = fake_data$lng,
                   lat = fake_data$lat,
                   # clusterId = "quakesCluster",
                   clusterOptions = markerClusterOptions()) %>%
        addMiniMap(
          tiles = providers$Esri.WorldStreetMap,
          toggleDisplay = TRUE) %>%
        addLegend("topleft",colors = NA,
                  labels = ' ',
                  title = "Cases (fake)",
                  opacity = 1)
      l
    })
  
  # magude_trend_by_health_facility
  output$magude_trend_by_health_facility <- renderPlot({
    x <- magude_cases_by_hf
    ggplot(data = x,
           aes(x = date,
               y = n)) +
      geom_line(alpha = 0.6) +
      geom_point(alpha = 0.7) +
      facet_wrap(~administrativePost) +
      theme_fivethirtyeight() +
      labs(title = 'Trends by health facility',
           subtitle = 'Magude') +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
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
  
  # Render a map for react case type
  output$react_map_case_type <-
    renderLeaflet({
      mag <- area_map[area_map$NAME_2 == 'Magude',]
      fake_data <- coordinates(mag)
      fake_data <- data.frame(fake_data)
      names(fake_data) <- c('lng', 'lat')
      nrf <- nrow(fake_data)
      for (i in 1:100){
        fake_data[nrf + i,] <-
          c(jitter(fake_data$lng[sample(1:nrf,1)]),
            jitter(fake_data$lat[sample(1:nrf,1)]))
      }
      fake_data$type <-
        sample(c('Followed-up',
                 'Not followed-up',
                 'Member'),
               nrow(fake_data),
               replace = TRUE)
      l <- cism_map_interactive(fake_data$lng,
                                fake_data$lat,
                                x = fake_data$type,
                                spdf = mag)%>%
        addMiniMap(
          tiles = providers$Esri.WorldStreetMap,
          toggleDisplay = TRUE)
      l
    }
    )
  
  # Render a map for react import
  output$react_map_import <-
    renderLeaflet({
      mag <- area_map[area_map$NAME_2 == 'Magude',]
      fake_data <- coordinates(mag)
      fake_data <- data.frame(fake_data)
      names(fake_data) <- c('lng', 'lat')
      nrf <- nrow(fake_data)
      for (i in 1:100){
        fake_data[nrf + i,] <-
          c(jitter(fake_data$lng[sample(1:nrf,1)]),
            jitter(fake_data$lat[sample(1:nrf,1)]))
      }
      fake_data$type <-
        sample(c('Indigenous',
                 'Imported'),
               nrow(fake_data),
               replace = TRUE)
      l <- cism_map_interactive(fake_data$lng,
                                fake_data$lat,
                                x = fake_data$type,
                                spdf = mag)%>%
        addMiniMap(
          tiles = providers$Esri.WorldStreetMap,
          toggleDisplay = TRUE)
      l
    }
    )
  
  #   Per health facility (for scenario 3):
  # - For each index case,
  # - how many were followed up?
  # - how many were treated, etc.
  # - how many imported vs. indigenous
  
  output$hf_details <- renderPlot({
    x <- case_index()
    # # Get whether followed up or not
    x$followed_up <-
      x$`Numero de agregado` %in% membros()$`Numero de agregado`
    # Randomly create how many secondary cases and how import cases, etc.
    x$secondary_cases <- sample(c(NA, 0, 1, 2, 3),
                                size = nrow(x),
                                prob = c(5, 5, 2, 1, 1),
                                replace = TRUE)
    # How many imported
    x$import <- ifelse(is.na(x$secondary_cases), NA, 0)

    # create plot data
    plot_data <- x %>%
      filter(date >= today - 30) %>%
      group_by(administrativePost) %>%
      summarise(index_cases = n(),
                followed_up = length(which(followed_up)),
                secondary_cases = sum(secondary_cases, na.rm = TRUE),
                imported_cases = sum(import, na.rm = TRUE))

    # make long
    long_data <- gather(plot_data,
                        key,
                        value,
                        index_cases: imported_cases)
    cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(long_data$key)))
    long_data$key <- gsub('_', ' ', long_data$key)
    ggplot(data = long_data,
           aes(x = administrativePost,
               y = value,
               group = key,
               fill = key)) +
      geom_bar(stat = 'identity',
               position = 'dodge') +
      scale_fill_manual(name = '',
                        values = cols) +
      theme_fivethirtyeight() +
      labs(title = 'Action by health facility',
           subtitle = 'Last 30 days')
    
  })
  
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
    df$Location <- 'Magude'
    
    right <- df %>%
      mutate(Location = 'Manhiça') %>%
      mutate(upr = (upr * 3) + rnorm(n = nrow(df), mean =0, sd = 1),
             lwr = (lwr * 2) - rnorm(n = nrow(df), mean = 0, sd = 1)) %>%
      mutate(value = (upr + lwr) / 2)
    df <- bind_rows(df, right)
    
    ggplot(data = df,
           aes(x = date,
               y= value,
               ymax = upr,
               ymin = lwr,
               group = Location,
               fill = Location)) +
      geom_line(alpha = 0.6) +
      geom_ribbon(alpha = 0.3) +
      # geom_line(lwd = 2, alpha = 0.6) +
      # geom_linerange(alpha = 0.6) +
      theme_economist() +
      labs(title = 'Predicted malaria incidence',
           subtitle = 'Based on meterological model, 95% uncertainty bands',
           x = 'Date',
           y = 'Incidence') +
      scale_fill_manual(name = '',
                         values = c('darkorange', 'darkgreen'))
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
  output$cases_manhica <- renderValueBox({
    dd <- malaria()
    dd <- dd %>% filter(date == yesterday)
    st <- ifelse(nrow(dd) == 1, 'Manhiça malaria case yesterday',
                 'Manhiça malaria cases yesterday')
    valueBox(
      value = nrow(dd),
      color = 'red',
      icon = icon("users"),
      subtitle = st)})
  
  output$cases_magude <- renderValueBox({
    x <- magude_cases_all$n[magude_cases_all$date == yesterday]
    y <- ifelse(is.na(x), 0, x)
    y <- ifelse(length(y) == 0, 0, y)
    st <- ifelse(y == 1, 'Magude malaria case yesterday',
                 'Magude malaria cases yesterday')
    valueBox(
      value = y,
      color = 'green',
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
  
  output$index_cases <- renderValueBox({
    valueBox(
      value = nrow(case_index()),
      color = 'red',
      icon = icon("user", lib = "glyphicon"),
      subtitle = paste0('index cases over last ',
                        input$days,
                        ' days'))})
  
  output$hh <- renderValueBox({
    
    index_cases_visited <- 
      length(which(agregados()$`Numero identificador de caso index` %in% case_index()$nid))
    
    valueBox(
      value = index_cases_visited,
      color = 'yellow',
      icon = icon("home", lib = "glyphicon"),
      subtitle = 'of those households already visited')})
  
  output$reduction <- renderValueBox({
    valueBox(
      value = 1025,
      color = 'green',
      icon = icon("fire", lib = "glyphicon"),
      subtitle = 'cases averted last month (relative to Manhiça)')})
  
  output$prediction <- renderValueBox({
    valueBox(
      value = 'STABLE',
      color = 'green',
      icon = icon("flag", lib = "glyphicon"),
      subtitle = 'Malaria cases prediction over next two weeks')})
  
  output$evaluation <- renderValueBox({
    valueBox(
      value = '91%',
      color = 'green',
      icon = icon("thumbs-up", lib = "glyphicon"),
      subtitle = 'model accuracy over last two weeks')})
  
  output$historical <- renderValueBox({
    valueBox(
      value = '-7%',
      color = 'orange',
      icon = icon("hand-down", lib = "glyphicon"),
      subtitle = 'overall model underprediction over last month')})
  
  # 
  # fluidRow(valueBoxOutput("cases_imported"),
  #          valueBoxOutput("cases_not_followed_up"),
  #          valueBoxOutput("cases_not_followed_up_more_7")),
  
  output$cases_imported <- renderValueBox({
    valueBox(
      value = '7',
      color = 'orange',
      icon = icon("user", lib = "glyphicon"),
      subtitle = 'cases imported over last 30 days')})
  output$cases_not_followed_up <- renderValueBox({
    valueBox(
      value = '20 of 46',
      color = 'orange',
      icon = icon("hand-down", lib = "glyphicon"),
      subtitle = 'cases not followed up over last 30 days')})
  output$cases_not_followed_up_more_7 <- renderValueBox({
    valueBox(
      value = '12',
      color = 'orange',
      icon = icon("arrow-up", lib = "glyphicon"),
      subtitle = '+ 30 day-old cases not followed up')})
  
  
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
    out <- out %>% left_join(scenarios_df)
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


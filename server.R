function(input, output, session) {
  
  # Get a reactive object of district
  district <- reactive({
    district_static %>%
      filter(date >= input$date_range[1] & 
               date <= input$date_range[2])
  })
  
  # Get a reactive object for malaria
  malaria <- reactive({
    district() %>%
      filter(malaria)
  })
  
  # Get reactive objects for magude stuff
  case_index <- reactive({
    case_index_static %>%
      filter(date >= input$date_range[1] & 
               date <= input$date_range[2])
  })
  agregados <- reactive({
    agregados_static %>%
      filter(date >= input$date_range[1] & 
               date <= input$date_range[2])
  })
  membros <- reactive({
    membros_static %>%
      filter(date >= input$date_range[1] & 
               date <= input$date_range[2])
  })
  
  index_and_membros <- reactive({
    dont_change <- c(
      # 'date', 
      # 'name', 
      # 'type',
      'Numero de agregado')#,
      # 'Nome', 
      # 'Apelido')
    x <- membros() %>%
      mutate(type = 'Secondary') %>%
      dplyr::select(`Numero de agregado`,
                    type,
                    date,
                    name,
                    Nome,
                    Apelido,
                    react_b_age,
                    `Identificação Permanente (PermID)`)
    names(x)[!names(x) %in% dont_change] <-
      paste0('secondary_', names(x)[!names(x) %in% dont_change])
    y <- case_index() %>%
      mutate(type = 'Index') %>%
      dplyr::select(`Numero de agregado`,
                    type,
                    date,
                    name,
                    Nome,
                    Apelido,
                    # longitude,
                    # latitude,
                    additionalInfo,
                    administrativePost,
                    age,
                    gender,
                    headPhone,
                    index_travel_time,
                    isResident,
                    locality,
                    telephone,
                    `Numero identificador de caso index`)
    names(y)[!names(y) %in% dont_change] <-
      paste0('index_', names(y)[!names(y) %in% dont_change])    
    joined <- full_join(x,
                        y,
                        by = dont_change)
    joined$type <- 
      ifelse(!is.na(joined$index_type), 'Index', 'Secondary')
    
    # Get coordinates
    joined <-
      joined %>%
      left_join(agregados_static %>%
                  filter(!duplicated(`Numero de agregado`)) %>%
                  dplyr::select(`Numero de agregado`,
                                longitude,
                                latitude))
    
    joined
    })
  
# Raw data for download
  raw_data <- reactive({
    if(input$which_data == 'Index only'){
      the_data <- case_index()
    } else if(input$which_data == 'Secondary only') {
      the_data <- membros()
    } else {
      the_data <- index_and_membros()
    }
    the_data
  })
  
    # Number of days back
  n_days <- reactive({
    as.numeric(input$date_range[2] - 
                 input$date_range[1]) + 1
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
  
  # Magude cases all 2 reactive
  magude_cases_all_2_reactive_agg <- reactive({
    x <- magude_cases_all_2 %>%
      filter(index) %>%
      group_by(date = date_week) %>%
      summarise(n = sum(n))
    x <- x %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2])
    x
  })
  
  magude_b <- reactive({
    x <- magude_cases_all_2 %>%
      filter(!index)
    x <- x %>%
      group_by(date = date_week) %>%
      summarise(n = sum(n))
    x <- x %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2])
    x
  })
  
  magude_c <- reactive({
    x <- magude_cases_all_2 %>%
      filter(!index)
    x <- x %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2])
    x
  })
  
  magude_d <- reactive({
    x <- magude_index_cases_by_hf %>%
      # filter(format(date, '%B') %in% input$month) %>%
      filter(date >= input$date_range[1] & 
               date <= input$date_range[2]) %>%
      mutate(name = gsub('Centro de Saude de |Centro de Saúde de ', '', name)) %>%
      group_by(name) %>%
      summarise(n = sum(n))
    x
  })
  
  magude_e <- reactive({
    x <- magude_member_cases_by_health_facility %>%
      # filter(format(date, '%B') %in% input$month) %>%
      filter(date >= input$date_range[1] & 
               date <= input$date_range[2]) %>%
      mutate(name = gsub('Centro de Saude de |Centro de Saúde de ', '', name)) %>%
      group_by(name) %>%
      summarise(n = sum(n))
    x
  })
  
  date_range <- reactive({
    input$date_range
  })
  
  # Text output for missing geo
  output$n_missing_geo_text <- renderText({
    paste0('Excluding those ',
           n_missing_geo(),
           ' with no geographic information')
  })
  
  # Render a risk map
  output$risk_map <- renderPlot({
    # Get locations of index cases
    x <- case_index_static %>%
      dplyr::select(date,
                    longitude,
                    latitude)
    ggplot() +
      geom_polygon(data = mag_map_fortified,
                   aes(x = long,
                       y = lat,
                       group = group),
                   fill = 'blue',
                   alpha = 0.2,
                   color = 'black') +
      coord_map() +
      geom_jitter(data=x, 
                 aes(x=longitude, y=latitude),
                 size = 1,
                 alpha = 0.3,
                 color = 'darkred') +
      theme_cism_map() +
      labs(title = 'Risk map',
           subtitle = 'Under construction')
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
                       options = layersControlOptions(collapsed = FALSE)) #%>%
      # addMiniMap(
      #   tiles = providers$Esri.WorldStreetMap,
      #   toggleDisplay = TRUE)
      l
  }
  )
  
  # Render a trend chart for Magude
  output$magude_trend <- 
    renderPlot({
      x <- magude_cases_all_2_reactive_agg()
      ggplot(data = x,
             aes(x = date,
                 y = n)) +
        geom_bar(stat = 'identity',
                 fill = 'darkorange',
                 color = 'black',
                 alpha = 0.6) +
        labs(x = 'Date',
             y = 'Cases',
             title = 'Trend: index cases',
             subtitle = 'Magude, passively detected cases (index) only') +
        theme_fivethirtyeight() +
        geom_label(aes(label = n))
    })
  
  
  
  # Render a trend chart for Magude with 2 lines (index or not)
  output$magude_trend_2 <- 
    renderPlot({
      x <- magude_cases_all_2_reactive_agg()
      ggplot(data = x,
             aes(x = date,
                 y = n)) +
        geom_line(alpha = 0.6) +
        geom_point(alpha = 0.6) +
        labs(x = 'Date',
             y = 'Cases',
             title = 'Detailed trend: index cases',
             subtitle = 'Magude, index cases only') +
        theme_fivethirtyeight() +
        xlim(range(magude_cases_all_2$date))
    })
  
  # Render a trend bar chart for magude for only active cases
  output$magude_trend_4 <- 
    renderPlot({
      x <- magude_b()
      ggplot(data = x,
             aes(x = date,
                 y = n)) +
        geom_bar(stat = 'identity',
                 alpha = 0.6,
                 fill = 'darkorange',
                 color = 'black') +
        labs(x = 'Date',
             y = 'Cases',
             title = 'Trend: members',
             subtitle = 'Magude, actively detected cases only (non-index)') +
        theme_fivethirtyeight() +
        geom_label(aes(label = n))
    })
  
  # Render a trend chart
  output$magude_trend_3 <- 
    renderPlot({
      x <- magude_c()
      ggplot(data = x,
             aes(x = date,
                 y = n)) +
        geom_line(alpha = 0.6) +
        geom_point(alpha = 0.6) +
        labs(x = 'Date',
             y = 'Cases',
             title = 'Detailed trend: members',
             subtitle = 'Magude, actively detected cases only (non-index)') +
        theme_fivethirtyeight() +
        xlim(range(magude_cases_all_2$date))
    })
  
  
  
  # Plot of cases by health facility magude
  output$magude_index_by_health_facility <- 
    renderPlot({
        x <- magude_d()
      
      ggplot(data = x,
             aes(x = name,
                 y = n)) +
        geom_bar(stat = 'identity',
                 fill = 'darkorange',
                 alpha = 0.6,
                 color = 'black') +
        labs(title = 'Index cases by health facility',
             subtitle = paste0(format(date_range()[1], 
                                      '%B %d, %Y'), 
                               ' - ', 
                               format(date_range()[2], '%B %d, %Y')),
             x = 'Health facility',
             y = 'Cases') +
        geom_hline(yintercept = (10 / 7) * 30,
                   lty = 2,
                   alpha = 0.6) +
        theme_fivethirtyeight() +
        theme(axis.text.x = element_text(angle = 90))
      
    })
  
  # Plot of cases by health facility magude
  output$magude_member_by_health_facility <- 
    renderPlot({
      x <- magude_e()
      ggplot(data = x,
             aes(x = name,
                 y = n)) +
        geom_bar(stat = 'identity',
                 fill = 'darkorange',
                 alpha = 0.6,
                 color = 'black') +
        labs(title = 'Member cases by health facility',
             subtitle = paste0(format(date_range()[1], 
                                      '%B %d, %Y'), 
                               ' - ', 
                               format(date_range()[2], '%B %d, %Y')),
             x = 'Health facility',
             y = 'Cases') +
        geom_hline(yintercept = (10 / 7) * 30,
                   lty = 2,
                   alpha = 0.6) +
        theme_fivethirtyeight() +
        theme(axis.text.x = element_text(angle = 90))
      
    })
  
  output$retro_map <- 
    renderPlot({
      cols <- colorRampPalette(brewer.pal(9, 'GnBu'))(length(unique(retro$year) ) - 1)
      cols <- c(cols, 'darkred')
      ggplot(data = retro, 
             aes(x = fake_date, y = n, color = year)) +
        geom_line(alpha = 0.4) +
        geom_point(alpha = 0.6) +
        geom_smooth(se = FALSE,
                    alpha = 0.9) +
        scale_x_date(labels = date_format("%B")) +
        theme_fivethirtyeight() +
        scale_color_manual(name = '',
                           values = cols) +
        # scale_y_log10() +
        labs(title = 'Historical comparison',
             subtitle = 'Current versus previous years, with locally regressed splines',
             y = 'Malaria cases') 
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
        # addMiniMap(
        #   tiles = providers$Esri.WorldStreetMap,
        #   toggleDisplay = TRUE) %>%
        addLegend("topleft",colors = NA,
                  labels = ' ',
                  title = "Cases (fake)",
                  opacity = 1)
      l
    })
  
  # magude_trend_by_health_facility
  output$magude_trend_by_health_facility <- renderPlot({
    x <- magude_index_cases_by_hf %>% ungroup
    x$name <- gsub('Centro de Saude de |Centro de Saúde de ',
                   '', x$name)
    x <- x %>%
      group_by(name, date = date_week) %>%
      summarise(n = sum(n)) %>%
      ungroup
    
    ggplot(data = x %>% 
             filter(date >= date_range()[1] & 
                      date <= date_range()[2]),
             # filter(format(date, '%B') %in% input$month),
           aes(x = date,
               y = n)) +
      geom_bar(stat = 'identity',
               alpha = 0.6,
               fill = 'darkorange',
               color = 'black') +
      facet_wrap(~name) +
      theme_fivethirtyeight() +
      labs(title = 'Trends by health facility',
           subtitle = paste0('Magude: ', paste0(format(date_range()[1], '%B %d, %Y'), ' - ', format(date_range()[2], '%B %d, %Y')))) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_label(aes(label = n))
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
                         options = layersControlOptions(collapsed = FALSE)) #%>%
        # addMiniMap(
        #   tiles = providers$Esri.WorldStreetMap,
        #   toggleDisplay = TRUE)
      l
    }
    )
  
  # Render a map for react case type
  output$react_map_case_type <-
    renderLeaflet({
      x <- index_and_membros()

      l <- cism_map_interactive(x$longitude,
                                x$latitude,
                                x = x$type,
                                spdf = mag_map) #%>%
        # addMiniMap(
        #   tiles = providers$Esri.WorldStreetMap,
        #   toggleDisplay = TRUE)
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
                                spdf = mag) #%>%
        # addMiniMap(
        #   tiles = providers$Esri.WorldStreetMap,
        #   toggleDisplay = TRUE)
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
      group_by(name) %>%
      summarise(index_cases = n(),
                followed_up = length(which(followed_up)),
                secondary_cases = sum(secondary_cases, na.rm = TRUE),
                imported_cases = sum(import, na.rm = TRUE))

    # make long
    long_data <- gather(plot_data,
                        key,
                        value,
                        index_cases: imported_cases)
    # cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(long_data$key)) - 1)
    cols <- c('darkorange', 'red', 'blue')
    long_data$key <- gsub('_', ' ', long_data$key)
    long_data <- long_data %>%
      mutate(name = gsub('Centro de Saude de |Centro de Saúde de ',
                         '',
                         name))
    ggplot(data = long_data %>%
             filter(key != 'followed up'),
           aes(x = name,
               y = value,
               group = key,
               fill = key)) +
      geom_bar(stat = 'identity',
               position = 'dodge'
               ) +
      scale_fill_manual(name = '',
                        values = cols) +
      theme_fivethirtyeight() +
      labs(title = 'Action by health facility',
           subtitle = 'Green = followed up; Red = not followed up') +
      theme(axis.text.x = element_text(angle = 90)) +
      geom_bar(stat = 'identity',
               data = long_data %>%
                 filter(key == 'followed up'),
               aes(x = name,
                   y = value),
               fill = 'green',
               alpha = 0.9,
               width = 1/3)
  })
  
  output$hf_details_table <- renderTable({
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
      group_by(name) %>%
      summarise(index_cases = n(),
                followed_up = length(which(followed_up)),
                secondary_cases = sum(secondary_cases, na.rm = TRUE),
                imported_cases = sum(import, na.rm = TRUE))
    plot_data$not_followed_up_yet <- plot_data$index_cases - plot_data$followed_up
    plot_data$not_followed_up_yet <-
      ifelse(plot_data$not_followed_up_yet == 0, '',
             paste0(plot_data$not_followed_up_yet, '  !!!'))
    for (j in which(!names(plot_data) %in% c('not_followed_up_yet', 'name'))){
      plot_data[,j] <- round(plot_data[,j])
    }
    names(plot_data) <- gsub('_', ' ', names(plot_data))
    
    plot_data
  })
  
  output$not_followed_up_yet_table <- renderDataTable({
    x <- case_index()
    # # Get whether followed up or not
    x$followed_up <-
      x$`Numero de agregado` %in% membros()$`Numero de agregado`
    x <- x %>%
      filter(!followed_up) %>%
      dplyr::select(name,
                    longitude,
                    latitude,
                    additionalInfo,
                    age,
                    gender,
                    headPhone,
                    Nome,
                    `Numero de agregado`)
    x
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
        addProviderTiles(providers$Esri.NatGeoWorldMap) #%>%
        # addMiniMap(
        #   tiles = providers$Esri.WorldStreetMap,
        #   toggleDisplay = TRUE)
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
                 n_days(), ' days')
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
      subtitle = paste0('index cases over ',
                        n_days(),
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
    fewer_cases <- 
      magude_cases_all %>%
      filter(date_week >= date_range()[1],
             date_week <= date_range()[2]) %>%
      mutate(period = 'Now') %>%
      mutate(denom = length(unique(date_week))) %>%
      dplyr::select(period, week, n, denom)
    fewer_cases <-
      fewer_cases %>%
      bind_rows(retro %>%
                  filter(year %in% as.character(2014:2015)) %>%
                  mutate(period = 'Then') %>%
                  mutate(denom = length(unique(paste0(year, week)))) %>%
                  dplyr::select(period, week, n, denom))
    fewer_cases <- fewer_cases %>%
      group_by(period) %>%
      summarise(n = sum(n),
                denom = first(denom)) %>%
      mutate(daily_cases = n / denom) %>%
      summarise(x = abs(first(daily_cases) - last(daily_cases))) %>%
      .$x
    fewer_cases <- round(fewer_cases)

    valueBox(
      value = fewer_cases,
      color = 'green',
      icon = icon("fire", lib = "glyphicon"),
      subtitle = 'fewer daily cases than this period 2014-15')})
  
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
      write.csv(raw_data(), file)
    },
    contentType = "text/csv"
  )
  
  output$rawtable <- renderDataTable({
    # orig <- options(width = 1000)
    # print(tail(raw_data(), input$maxrows))
    # options(orig)
    raw_data()
  })
  
  output$date_range <- renderUI({
    sliderInput("date_range", 
                "Choose Date Range:", 
                min = as.Date('2017-01-01'), 
                max = Sys.Date(), 
                value = c(today- 30, 
                          today)
    )
  })
  
}



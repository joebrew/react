dashboardPage(
  skin = 'blue',
  title = 'MISSION CONTROL CENTER',
  header = dashboardHeader(title = "MISSION CONTROL"),
  sidebar = dashboardSidebar(
    sliderInput("days", 
                "View data from the last n days:",
                min = 0, max = 100, value = 3, step = 1),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Raw data", tabName = "rawdata"),
      menuItem('React', tabName = 'react'),
      menuItem('Malaria forecast', tabName = 'forecast'),
      menuItem("About", tabName = "about")
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("cases"),
                valueBoxOutput("cases_range"),
                valueBoxOutput('deaths')
              ),
              fluidRow(
                # leafletOutput('leaflet_map'),
                box(
                  width = 6, 
                  status = "info", 
                  solidHeader = FALSE,
                  title = textOutput('n_days_text'),
                  footer = textOutput('n_missing_geo_text'),
                  leafletOutput('leaflet_map')
                  # plotOutput('plot1')
                  # bubblesOutput("bubbles_plot",
                  #               width = "100%",
                  #               height = 600)
                ),
                box(width = 6,
                    status = 'info',
                    title = 'Trend',
                    plotOutput('ts'))
              )
      ),
      tabItem("rawdata",
              numericInput("maxrows", "Rows to show", 25),
              checkboxInput('malaria_only',
                            label = 'Only show malaria cases',
                            value = TRUE),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadCsv", "Download as CSV")
      ),
      tabItem("react",
              fluidRow(
                valueBoxOutput('action'),
                valueBoxOutput('deviation'),
                valueBoxOutput('reduction')),
              fluidRow(
                box(
                  width = 4, 
                  status = "info",
                  title = "React scenarios",
                  tableOutput("react_scenarios")
                ),
                box(width = 8,
                    leafletOutput('react_map'))

              )
      ),
      tabItem("forecast",
              fluidRow(
                box(width = 6,
                    status = 'info',
                    title = 'Map',
                    # p('some text')
                    leafletOutput('forecast_map')
                    ),
                box(width = 6,
                    status = 'info',
                    title = 'Chart',
                    # p('some text')
                    plotOutput('forecast_plot')
                )
              )
      ),
      tabItem("about",
              fluidRow(
                column(6,
                       p('Some text.')),
                column(6,
                       p('Some more text.'))
              )
      )
    )
  )
)

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
      menuItem('React', tabName = 'react'),
      menuItem('Malaria forecast', tabName = 'forecast'),
      menuItem("Raw data", tabName = "rawdata"),
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
                    fluidRow(
                      column(2,
                             checkboxGroupInput('scenarios',
                                           'Show scenarios',
                                           choices = 1:3,
                                           selected = 1:3)),
                      column(10,
                             leafletOutput('react_map'))
                    ))

              )
      ),
      tabItem("forecast",
              fluidRow(
                valueBoxOutput('prediction'),
                valueBoxOutput('evaluation'),
                valueBoxOutput('historical')),
              fluidRow(
                box(width = 6,
                    status = 'info',
                    title = 'Map of predicted incidence over next two weeks',
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
      tabItem("rawdata",
              numericInput("maxrows", "Rows to show", 25),
              checkboxInput('malaria_only',
                            label = 'Only show malaria cases',
                            value = TRUE),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadCsv", "Download as CSV")
      ),
      tabItem("about",
              fluidRow(
                box(width = 4, 
                    status = "primary",
                    title = "Amone Felimone",
                    p('Engineering'),
                    a('A random website',
                      href = 'http://www.google.com')),
                box(width = 4, 
                    status = "primary",
                    title = "Pedro Aide",
                    p('Management'),
                    a('CISM profile',
                      href = 'http://www.manhica.org/wp/team_member/pedro-aide/')),
                box(width = 4, 
                    status = "primary",
                    title = "Francisco Saute",
                    p('Direction'),
                    a('Science of eradication profile',
                      href = 'http://scienceoferadication.org/faculty-member/saute-francisco/')),
                box(width = 4, 
                    status = "primary",
                    title = "Joe Brew",
                    p('Data visualization'),
                    a('www.economicsofmalaria.com',
                      href = 'http://www.economicsofmalaria.com')),
                box(width = 4, 
                    status = "primary",
                    title = "Andrés Jarzyna",
                    p('Engineering, API'),
                    a('A guy with the same name',
                      href = 'http://www.beneschlaw.com/ajarzyna/')),
                box(width = 4, 
                    status = "primary",
                    title = "Bea Galatas",
                    p('Design and implementation'),
                    a('ISGlobal profile',
                      href = 'https://www.isglobal.org/en/person?p_p_id=viewpersona_WAR_intranetportlet&p_p_lifecycle=0&p_p_col_id=column-3&p_p_col_count=1&_viewpersona_WAR_intranetportlet_struts_action=%2Fview%2FpersonaView&_viewpersona_WAR_intranetportlet_typeOfPeople=staff&_viewpersona_WAR_intranetportlet_personaId=4001'))
              )
      )
    )
  )
)

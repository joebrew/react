dashboardPage(
  skin = 'blue',
  title = 'MISSION CONTROL CENTER',
  header = dashboardHeader(title = "MISSION CONTROL"),
  sidebar = dashboardSidebar(
    uiOutput('date_range'),
    # sliderInput("days", 
    #             "View data from the last n days:",
    #             min = 0, max = 100, value = 30, step = 1),
    sidebarMenu(
      menuItem('Surveillance', tabName = 'react'),
      menuItem('RCD', tabName = 'react_intervention'),
      menuItem('Malaria forecast', tabName = 'forecast'),
      menuItem("Raw data", tabName = "rawdata"),
      menuItem("General", tabName = "general"),
      menuItem("About", tabName = "about")
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem("general",
              fluidRow(
                valueBoxOutput("cases_manhica"),
                valueBoxOutput("cases_magude"),
                valueBoxOutput("cases_range")
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
                ),
                box(width = 6,
                    status = 'info',
                    title = 'Trend',
                    plotOutput('ts'))
              )
      ),
      tabItem("react",
              fluidRow(
                valueBoxOutput('index_cases'),
                valueBoxOutput('hh'),
                valueBoxOutput('reduction')),
              # fluidRow(column(4),
              #          column(4),
              #          column(4)),
              fluidRow(
                column(6,
                       plotOutput('magude_trend')),
                column(6,
                       plotOutput('magude_trend_4'))
              ),
              # fluidRow(column(6,
              #                 plotOutput('magude_trend_2')),
              #          column(6,
              #                 plotOutput('magude_trend_3'))),
              fluidRow(
                column(1),
                column(10,
                       plotOutput('magude_trend_by_health_facility')),
                column(1)),
              fluidRow(column(2),
                       column(8,
                              plotOutput('retro_map')),
                       column(2)),
              fluidRow(column(2),
                       column(8,
                              plotOutput('risk_map')),
                       column(2))
      ),
      tabItem("react_intervention",
              # fluidRow(valueBoxOutput("cases_imported"),
              #          valueBoxOutput("cases_not_followed_up"),
              #          valueBoxOutput("cases_not_followed_up_more_7")),
              fluidRow(
                p('NOTE: all references to imported and secondary cases are currently placeholders only'),
                column(2),
                column(8,
                       plotOutput('hf_details')),
                column(2)
              ),
              fluidRow(
                column(1),
                p('Below is a table of the same data as the above chart.'),
                column(10,
                       tableOutput('hf_details_table')),
                column(1)
              ),
              fluidRow(
                column(1),
                column(10,
                       h2('Index cases which have not yet been followed up'),
                       p('The below table is interactive and filterable. Click on a column to sort by it. Use the box in the top right to search the whole table, or the boxes at the bottom of the table to search just one column.'),
                       dataTableOutput('not_followed_up_yet_table')),
                column(1)
              ),
              fluidRow(
                column(6,
                       plotOutput('magude_index_by_health_facility')),
                column(6,
                       plotOutput('magude_member_by_health_facility'))
              ),
              
              fluidRow(
                column(6,
                       h3('Follow-up status'),
                       p('Using real data, lots missing'),
                       leafletOutput('react_map_case_type')),
                column(6,
                       h3('Imported cases'),
                       p('Currently using fake data'),
                       leafletOutput('react_map_import'))
              )),
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
              # numericInput("maxrows", "Rows to show", 25),
              selectInput('which_data',
                          'Which data',
                          choices = c('Index only',
                                      'Secondary only',
                                      'Both'),
                          selected = 'Index only'),
              dataTableOutput("rawtable"),
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

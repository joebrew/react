the_date <- Sys.Date() +1
# the_date <- as.Date('2017-04-27')
library(tidyverse)
library(jsonlite)
library(shiny)
# library(shinySignals)
library(dplyr)
library(shinydashboard)
# library(bubbles)
library(leaflet)
library(rCharts)
library(ggthemes)
library(RColorBrewer)
library(cism)
library(RPostgreSQL)
library(dplyr)
library(cism)
library(sp)
library(readstata13)
library(scales)
library(broom)
library(rvest)
library(RSelenium)
library(httr)


# Get all the weeks we want
add_zero <- function (x, n) 
{
  x <- as.character(x)
  adders <- n - nchar(x)
  adders <- ifelse(adders < 0, 0, adders)
  for (i in 1:length(x)) {
    if (!is.na(x[i])) {
      x[i] <- paste0(paste0(rep("0", adders[i]), collapse = ""), 
                     x[i], collapse = "")
    }
  }
  return(x)
}
weeks <- expand.grid(year = 2017,
                       week = add_zero(as.character(28:36), n = 2)) %>%
  mutate(x = paste0(year, 'W', week)) %>%
  .$x

# Define a file name for the api
api_file_name <- paste0('api_', 
                        the_date,
                        '.RData')
if(api_file_name %in% dir('data')){
  load(paste0('data/',
              api_file_name))
} else {
  # rd <-rsDriver(verbose =TRUE, browser = 'phantomjs')
  rd <-rsDriver(verbose =TRUE, browser = 'chrome')
  remDr <- rd[["client"]]
  remDr$navigate("https://dbs.manhica.net:4843/dhis/dhis-web-commons/security/login.action")
  
  # Log in
  credentials <- cism::credentials_extract()
  username_entry <- remDr$findElement(using = 'xpath', '//*[(@id = "j_username")]') # Identify username_entry
  password_entry <- remDr$findElement(using = 'xpath', '//*[(@id = "j_password")]') # Identify password
  submit <- remDr$findElement(using = 'xpath', '//*[(@id = "submit")]') # Identify submission
  username_entry$sendKeysToElement(list(credentials$api_username))
  password_entry$sendKeysToElement(list(credentials$api_password))
  submit$clickElement()
  # Navigate to certain pages
  navigate <- function(url){
    remDr$navigate(url)
    txt<-remDr$findElement(using='css selector',"body")$getElementText()[[1]]
    out <- jsonlite::fromJSON(txt = txt)
    return(out)
  }
  
  # Organization units
  url <- 'https://dbs.manhica.net:4843/dhis/api/27/organisationUnits.json?paging=false'
  remDr$navigate(url)
  txt<-remDr$findElement(using='css selector',"body")$getElementText()[[1]]
  out <- jsonlite::fromJSON(txt = txt)
  ou <- out$organisationUnits
  
  # Remove Mozambique
  ou <- ou %>%
    filter(displayName != 'Mozambique')
  
  # Raw data
  # Loop through each of the organization units, and get the appropriate raw data
  raw_data_list <- list()
  counter <- 0
  for (i in 1:nrow(ou)){
    this_org_unit <- ou$id[i]
    message(this_org_unit)
    this_name <- ou$displayName[i]
    message(this_name)
    if(this_name != 'Mozambique'){
      for(w in 1:length(weeks)){
        try({
          counter <- counter + 1
          # Sys.sleep(0.5)
          this_week <- weeks[w]
          message(w)
          the_url <- paste0('https://dbs.manhica.net:4843/dhis/api/27/dataValueSets.json?orgUnit=',
                        this_org_unit,
                        '&period=',
                        this_week,
                        # '2017W30',
                        '&dataSet=KrDJDTu8M7j',
                        '&children=true')
          # go the url
          out <- navigate(url = the_url)
          # Sys.sleep(1)
          out <- out$dataValues
          out$location <- this_name
          raw_data_list[[counter]] <- out
        })
        message('x')
      }
      message('y')
    }
    message('z')
  }
  message('a')
  rd <- bind_rows(raw_data_list)
  message()
  # Then get data elements
  # https://dbs.manhica.net:4843/dhis/api/dataElements.json
  de <- navigate('https://dbs.manhica.net:4843/dhis/api/dataElements.json?paging=false')
  de <- de$dataElements
  
  # # Get second page too
  # de2 <- navigate('https://dbs.manhica.net:4843/dhis/api/dataElements.json?page=2')
  # de2 <- de2$dataElements
  # 
  # de <- bind_rows(de, de2)
  
  # Get category combinations
  cc <- navigate("https://dbs.manhica.net:4843/dhis/api/categoryOptionCombos.json?paging=false")$categoryOptionCombos
  
  # Close the browser
  remDr$close()
  # stop the selenium server
  # rd[["server"]]$stop() 
  
  # Join all together
  api <-
    # Get data elements
    left_join(rd %>% 
                dplyr::select(-attributeOptionCombo, -storedBy, -created, -lastUpdated),
              de,
              by = c('dataElement' = 'id')) %>%
    dplyr::select(-dataElement) %>%
    rename(data_element = displayName) %>%
    # Get category combinations 
    left_join(cc,
              by = c('categoryOptionCombo' = 'id')) %>%
    # dplyr::rename(id = categoryOptionCombo) %>%
    # Get organization units
    left_join(ou %>%
                rename(health_post = displayName),
              by = c('orgUnit' = 'id')) %>%
    rename(key = data_element,
           key_display = displayName) %>%
    # Keep only relevant columns
    dplyr::select(period,
                  key, key_display,
                  value, 
                  health_post)
  api <- api %>% filter(!is.na(period))
  
  # Get a year week too
  api <- api %>%
    mutate(year = as.numeric(substr(period, 1, 4)),
           week = as.numeric(substr(period, 6, nchar(period)))) %>%
    mutate(year_week = paste0(year, 'W', week))
  save(api,
       file = paste0('data/', api_file_name))
}

api$year_week <- gsub('W', '-', api$year_week)

# Get a week helper
wh <- data_frame(date = seq(as.Date('2013-01-01'),
                            as.Date(Sys.Date()),
                            by = 1))
wh <- wh %>%
  mutate(dow = weekdays(date),
         month = format(date, '%m'),
         year = as.numeric(format(date, '%Y')))
wh <- wh %>%
  mutate(dummy = ifelse(dow == 'Monday', 1, 0)) %>%
  group_by(year, month) %>%
  mutate(mondays_in = cumsum(dummy)) %>%
  ungroup %>%
  group_by(year) %>%
  mutate(mondays_in_year = cumsum(dummy)) %>%
  ungroup
# Replace the 0s with the previous weeks number and month
wh$month_week <- NA
wh$year_week <- NA
for (i in 7:nrow(wh)){
  # message(i)
  this_index <- i
  while(wh$mondays_in[this_index] == 0){
    this_index <- this_index -1
  }
  wh$month_week[i] <- 
    paste0(wh$month[this_index],
           '-',
           wh$mondays_in[this_index])
  wh$year_week[i] <- 
    paste0(wh$year[this_index],
           '-',
           wh$mondays_in_year[this_index])
}
wh <- wh %>% filter(date >= '2014-01-01')
# Get a week date
wh <- wh %>%
  group_by(year, month_week) %>%
  mutate(date_week = first(date)) %>%
  ungroup
# Slim down
wh <- wh %>%
  dplyr::select(date, date_week, month_week, year_week)

# Get a date into api
helper <- wh %>%
  group_by(year_week) %>%
  filter(date == max(date)) %>%
  ungroup %>%
  rename(date_start = date_week) %>%
  mutate(date_end = date)

api <- api %>%
  left_join(helper,
            by = 'year_week')

# Group ages, etc.
age_location <- strsplit(api$key_display, ',')
api$age_group <- unlist(lapply(age_location, function(x){x[1]}))
api$location <- unlist(lapply(age_location, function(x){x[2]}))
api$key_display <- NULL
api$value <- as.numeric(api$value)
api$location <- gsub(' ', '', api$location)
#####################################

# Get some useful aggregated tables
ts <- 
  api %>%
  mutate(type = ifelse(grepl('Casos de malária clinica|Casos de malária confirmada', key), 'Case',
                       ifelse(grepl('tratados', key), 'Treated',
                                    ifelse(grepl('testados', key), 
                                           'Tested', 
                                           ifelse(grepl('consultas', key),
                                                  'Consult', NA))))) %>%
  group_by(date, type, health_post, age_group, location) %>%
  summarise(value = sum(value)) %>%
  ungroup 



# # Read in the data manually download from sis-ma and
# # emailed by bea
# bea <- read_csv('data/BES_Magude.csv')
# 
# # Join wh stuff to bea
# bea <-
#   bea %>%
#   mutate(year = substr(Period, 1,4),
#          week = substr(Period, 6, nchar(Period))) %>%
#   mutate(year_week = paste0(year, '-', week)) %>%
#   rename(cases = Value) %>%
#   dplyr::select(year_week, cases) %>%
#   left_join(wh %>%
#               dplyr::select(year_week,
#                             month_week,
#                             date_week) %>%
#               filter(!duplicated(year_week)),
#             by = 'year_week')
# 
# 
# # Read in old OPD Magude data, sent by Bea
# old_opd <- readstata13::read.dta13('data/opd_magude_old/survmag_rrs_dis.dta')
# old_opd <-
#   old_opd %>%
#   dplyr::select(yr,
#                 week,
#                 mal_hf)
# # old_opd_us <- readstata13::read.dta13('data/opd_magude_old/survmag_rrs_us.dta')
# old_opd <-
#   old_opd %>%
#   mutate(year_week = paste0(yr, '-', week))
# 
# old_opd <-
#   old_opd %>%
#   left_join(wh %>%
#               filter(!duplicated(year_week)) %>%
#               dplyr::select(-date))
# 
# old_opd <-
#   old_opd %>% group_by(date_week) %>%
#   summarise(cases = sum(mal_hf, na.rm = T))
# 
# 
# 
# 
# 
# 
# # Create a dataframe of scenarios
# scenarios_df <- 
#   data_frame(scenario = 1:3,
#              description = c('Large-scale MDA',
#                              'Focal MDA',
#                              'Household MDA with radial extension'))
# 
# # Read in dashboard data
# district_static <- fromJSON(txt = 'data/dashboard.json')
# 
# # Reformat
# district_static$date <- 
#   as.Date(district_static$date, '%m/%d/%Y')
# 
# 
# 
# # Create lat/lng columns
# district_static <-
#   district_static %>%
#   mutate(lng = as.numeric(gps_longitude),
#          lat = as.numeric(gps_latitude))
# 
# # Remove those with no dates
# district_static <- district_static %>%
#   filter(!is.na(date))
# 
# # Get maximum date
# max_date <- max(district_static$date, na.rm = TRUE)
# 
# # For now, set today to the max date
# # today <- max_date +1
# # yesterday <- max_date
# today <- the_date
# yesterday <- the_date - 1
# 
# # Get weather malaria or not
# district_static$malaria <-
#   district_static$slidesyno == 2 |
#   district_static$thicksmear %in% as.character(2:5) |
#   district_static$rdt %in% c('2', '3')
# 
# # Get average numer of cases
# avg_cases <- length(which(district_static$malaria)) / 
#   length(unique(district_static$date))
# 
# # Get magude and manhica map
# area_map <- moz3
# area_map <- area_map[area_map$NAME_2 %in% c('Manhiça', 'Magude'),]
# area_map@data$color <- ifelse(area_map@data$NAME_2 == 'Magude',
#                               'darkgreen',
#                               'darkorange')
# 
# # Fortify area map
# area_map_fortified <- broom::tidy(area_map, id = "NAME_3")
# 
# mag_map <- moz3
# mag_map <- mag_map[mag_map$NAME_2 == 'Magude',]
# mag_map_fortified <- broom::tidy(mag_map, id = "NAME_2")
# 
# 
# # Read in dhis2 data
# 
# # Read the credentials
# # This requires a yaml with the following fields
# # # dhis2_db_dbname
# # # dhis2_db_hos,
# # # dhis2_db_port
# # # dhis2_db_username
# # # dhis2_db_password
# this_date <- the_date #Sys.Date()
# file_name <- paste0('dhis2_',
#                     this_date,
#                     '.RData')
# if(file_name %in% dir('data/')){
#   load(paste0('data/', file_name))
# } else {
#   
#   credentials <- cism::credentials_extract()
#   credentials <- credentials[grepl('dhis2_db', names(credentials))]
#   
#   # Create a connection
#   psql_connection <- dbConnect(drv = dbDriver("PostgreSQL"),
#                                dbname = credentials$dhis2_db_dbname,
#                                host = credentials$dhis2_db_host,
#                                port = credentials$dhis2_db_port,
#                                user = credentials$dhis2_db_username,
#                                password = credentials$dhis2_db_password)
#   
#   query <- paste0("SELECT g.name, a.programinstanceid, a.created, a.enrollmentdate, a.incidentdate, a.trackedentityinstanceid, a.programid,
#                   a.organisationunitid, b.latitude, b.longitude, b.programstageinstanceid, c.value attributevalue, d.code attributecode, d.name attributename,
#                   e.value dataelementvalue, f.code dataelementcode, f.name dataelementname
#                   FROM programinstance a FULL OUTER JOIN programstageinstance b ON a.programinstanceid = b.programinstanceid
#                   left JOIN trackedentityattributevalue c ON a.trackedentityinstanceid = c.trackedentityinstanceid
#                   left JOIN trackedentityattribute d ON c.trackedentityattributeid = d.trackedentityattributeid
#                   left JOIN trackedentitydatavalue e ON e.programstageinstanceid = b.programstageinstanceid
#                   left JOIN dataelement f ON e.dataelementid = f.dataelementid
#                   left JOIN organisationunit g on a.organisationunitid = g.organisationunitid")
#   x <- dbGetQuery(psql_connection, query)
#   
#   save(x, file = '~/Desktop/temp.RData')
#   
#   # Loop through each program id and make dataframes
#   programids <- sort(unique(x$programid))
#   
#   # NEED TO FIGURE OUT HOW TO KEEP att_react_index, FOR LINKING BETWEEN TABLES
#   programids <- programids[programids != 7132]
#   for (i in 1:length(programids)){
#     message(i)
#     # Get the program id
#     this_programid <- programids[i]
#     # Subset just for this programid
#     this_program <- x %>% filter(programid == this_programid)
#     # # Use attribute code, but if not available, get attribute name
#     # this_program$attributecode <-
#     #   ifelse(is.na(this_program$attributecode),
#     #          gsub(' ', '_', tolower(this_program$attributename)),
#     #               this_program$attributecode)
#     # Spread
#     out <- this_program %>%
#       dplyr::select(attributename,
#                     attributevalue,
#                     programstageinstanceid,
#                     enrollmentdate,
#                     dataelementcode,
#                     dataelementvalue,
#                     name,
#                     longitude,
#                     latitude) %>%
#       rename(date = enrollmentdate)
#     # Remove duplicate rows
#     out <- out %>% distinct(attributename,
#                             attributevalue,
#                             programstageinstanceid,
#                             dataelementcode,
#                             dataelementvalue,
#                             .keep_all = TRUE)
#     out <- spread(data = out,
#                   key = dataelementcode,
#                   value = dataelementvalue)
#     # Spread again
#     out <- out %>%
#       filter(!is.na(programstageinstanceid))
#     
#     out <- spread(data = out, 
#                   key = attributename, 
#                   value = attributevalue)
#     out <- out[,names(out) != '<NA>']
#     
#     # Filter out those rows which do not have a - in agregado
#     if("Numero de agregado" %in% names(out)){
#       out <- out %>% filter(!is.na(`Numero de agregado`))
#     }
#     
#     assign(paste0('program_id_',
#                   this_programid),
#            out,
#            envir = .GlobalEnv)
#     write_csv(out,
#               paste0('~/Desktop/',
#                      'program_id_',
#                      this_programid,
#                      '.csv'))
#   }
#   
#   # Also get data at the U.S. level
#   us_query <- "select f.value, g.startdate, g.enddate, h.name, b.code, b.name dataelementname,e.uid categoryoptioncombouid, e.name categoryoptionname
# from datasetelement a inner join dataelement b on a.dataelementid = b.dataelementid left join categorycombo c on a.categorycomboid = c.categorycomboid left join
# categorycombos_optioncombos d on c.categorycomboid = d.categorycomboid inner join categoryoptioncombo e on d.categoryoptioncomboid = e.categoryoptioncomboid inner join
# datavalue f on a.dataelementid = f.dataelementid inner join period g on f.periodid = g.periodid inner join organisationunit h on f.sourceid = h.organisationunitid;"
#   
#   y <- dbGetQuery(psql_connection, us_query)
#   
#   save(y, file = '~/Desktop/temp_us.RData')
#   
#   # Reshape the health facility data
#   us <- y %>%
#     mutate(age_group = unlist(lapply(strsplit(categoryoptionname, ','), function(x){x[1]}))) %>%
#     mutate(type = unlist(lapply(strsplit(categoryoptionname, ','), function(x){x[2]}))) %>%
#     dplyr::select(startdate,
#                   enddate,
#                   name,
#                   code,
#                   age_group,
#                   type,
#                   value) %>%
#     rename(us = name) %>%
#     mutate(us = gsub('Centro de Saude de ', '', us))
#   
#   save(program_id_277797,
#        # program_id_7132,
#        program_id_71885,
#        program_id_71914,
#        us,
#        file = paste0('data/',
#                      file_name))
# }
# 
# # 277797: case index (the people who go to the health facility and are POSITIVE)
# case_index_static <- program_id_277797
# # 71885: agregado (each row is a visited agregado)
# agregados_static <- program_id_71885
# agregados_static$longitude[agregados_static$longitude < 31] <- NA
# agregados_static$latitude[agregados_static$latitude > -20] <- NA
# # 71914 membro: every person in the same agregado, but did not go to health facility
# membros_static <- program_id_71914
# # psql -h 172.16.236.244 -p 5432 -U jbrew dhis2
# 
# # Clean up magude data
# case_index_static$date <- as.Date(case_index_static$date)
# the_date <- max(case_index_static$date, na.rm = TRUE) + 1
# 
# case_index_static$last_week <- case_index_static$date >= the_date - 6
# case_index_static$`Numero de agregado` <- 
#   unlist(lapply(strsplit(x = case_index_static$`Identificação Permanente (PermID)`, split = '-'), function(x){paste0(x[1], '-', x[2])}))
# 
# 
# 
# agregados_static$date <- as.Date(agregados_static$date)
# agregados_static$last_week <- agregados_static$date >= the_date - 6
# membros_static$date <- as.Date(membros_static$date)
# membros_static$last_week <- membros_static$date >= the_date - 6
# 
# 
# 
# # Create an object of all cases
# magude_cases_all <- 
#   case_index_static %>%
#   group_by(date) %>%
#   tally %>%
#   bind_rows(
#     membros_static %>%
#       filter(as.numeric(as.character(rdt_result)) == 1) %>% 
#       # filter(react_form_b_rdt_result == 1) %>%
#       group_by(date) %>% 
#       tally
#   ) %>%
#   group_by(date) %>%
#   summarise(n = sum(n)) %>%
#   left_join(wh,
#             by = 'date') %>%
#   mutate(week = unlist(lapply(strsplit(year_week, '-'), function(x){x[[2]]})))
# 
# 
# 
# # Create an object of all cases indicating which are index and not
# magude_cases_all_2 <- 
#   case_index_static %>%
#   group_by(date) %>%
#   tally %>%
#   mutate(index = TRUE) %>%
#   bind_rows(
#     membros_static %>%
#       filter(as.numeric(as.character(rdt_result)) == 1) %>% 
#       # filter(react_form_b_rdt_result == 1) %>%
#       group_by(date) %>% 
#       tally %>%
#       mutate(index = FALSE)
#   )  %>%
#   left_join(wh, by = 'date')
# 
# # Create an object of magude cases by health facility
# # SHOULD NOT BE JOINING ON AGREGADO, SINCE ONE INDEX CASE CAN HAVE
# # MANY AGREGADOS
# magude_index_cases_by_hf <- 
#   case_index_static %>%
#   group_by(date, 
#            name) %>%
#   tally %>%
#   # COMMENTING OUT THE BELOW, SINCE WE NOW ONLY WANT INDEX
#   # bind_rows(
#   #   membros_static %>%
#   #     # ask bea if this is correct
#   #     filter(react_form_b_rdt_result == 1) %>%
#   #     # get health facility
#   #     left_join(case_index_static %>%
#   #                 dplyr::select(`Numero de agregado`,
#   #                               name)) %>%
#   #     group_by(date, name) %>% 
#   #     tally
# # ) %>%
# group_by(date,
#          name) %>%
#   summarise(n = sum(n)) %>%
#   left_join(wh, 
#             by = 'date')
# 
# 
# magude_member_cases_by_health_facility <- 
#   membros_static %>%
#   filter(as.numeric(as.character(rdt_result)) == 1) %>% 
#     # filter(react_form_b_rdt_result == 1) %>%
#   # get health facility
#   left_join(case_index_static %>%
#               dplyr::select(`Numero de agregado`,
#                             name)) %>%
#   group_by(date, name) %>%
#   tally %>%
#   group_by(date,
#            name) %>%
#   summarise(n = sum(n)) %>%
#   left_join(wh, 
#             by = 'date')
# # Shift the dates in district_static up
# # date_shift <- as.numeric(max(case_index_static$date, na.rm = TRUE) -
# #   max(district_static$date, na.rm = TRUE))
# # district_static$date <- district_static$date + date_shift
# 
# 
# # Te a retrospective comparison
# retro <-
#   magude_cases_all %>%
#   group_by(date_week) %>%
#   summarise(n = sum(n)) %>%
#   bind_rows(old_opd %>%
#               group_by(date_week) %>%
#               summarise(n = sum(cases))) %>%
#   filter(!is.na(date_week)) %>%
#   arrange(date_week) %>%
#   left_join(wh %>%
#               dplyr::select(date_week, year_week) %>%
#               filter(!duplicated(date_week))) %>%
#   mutate(week = unlist(lapply(strsplit(year_week, '-'), function(x){x[2]})),
#          year = unlist(lapply(strsplit(year_week, '-'), function(x){x[1]})))
# retro <- retro %>%
#   dplyr::select(year, week, n) %>% 
#   mutate(fake_date = as.Date('1900-01-01') + (7 * (as.numeric(week) - 1))) 
# 
# 
# # Join to other objects
# agregados_static <-
#   agregados_static %>%
#   left_join(wh, by = 'date')
# case_index_static <-
#   case_index_static %>%
#   left_join(wh, by = 'date')
# district_static <-
#   district_static %>%
#   left_join(wh, by = 'date')
# 
# # Clean up US ##################
# us$value <- as.numeric(as.character(us$value))
# # Get total number of positive us cases
# us_static <- us
# 
# us_malaria <- 
#   us_static %>%
#   filter(code == 'positive_rdt') %>%
#   group_by(startdate,
#            enddate,
#            us) %>%
#   summarise(cases = sum(value)) %>%
#   ungroup
# 
# # ggplot(data = us_malaria,
# #        aes(x = startdate,
# #            y = cases)) +
# #   facet_wrap(~us) +
# #   geom_line()

prettify <- function (the_table, remove_underscores_columns = TRUE, cap_columns = TRUE, 
                      cap_characters = TRUE, comma_numbers = TRUE, date_format = "%B %d, %Y", 
                      round_digits = 2, remove_row_names = TRUE, remove_line_breaks = TRUE, 
                      data_table = TRUE, nrows = 5, download_options = FALSE) 
{
  column_names <- names(the_table)
  the_table <- data.frame(the_table)
  names(the_table) <- column_names
  classes <- lapply(the_table, function(x) {
    unlist(class(x))[1]
  })
  if (cap_columns) {
    names(the_table) <- Hmisc::capitalize(names(the_table))
  }
  if (remove_underscores_columns) {
    names(the_table) <- gsub("_", " ", names(the_table))
  }
  for (j in 1:ncol(the_table)) {
    the_column <- the_table[, j]
    the_class <- classes[j][1]
    if (the_class %in% c("character", "factor")) {
      if (cap_characters) {
        the_column <- as.character(the_column)
        the_column <- Hmisc::capitalize(the_column)
      }
      if (remove_line_breaks) {
        the_column <- gsub("\n", " ", the_column)
      }
    }
    else if (the_class %in% c("POSIXct", "Date")) {
      the_column <- format(the_column, format = date_format)
    }
    else if (the_class %in% c("numeric", "integer")) {
      the_column <- round(the_column, digits = round_digits)
      if (comma_numbers) {
        the_column <- scales::comma(the_column)
      }
    }
    the_table[, j] <- the_column
  }
  if (remove_row_names) {
    row.names(the_table) <- NULL
  }
  if (data_table) {
    if (download_options) {
      the_table <- DT::datatable(the_table, options = list(pageLength = nrows, 
                                                           dom = "Bfrtip", buttons = list("copy", "print", 
                                                                                          list(extend = "collection", buttons = "csv", 
                                                                                               text = "Download"))), rownames = FALSE, extensions = "Buttons")
    }
    else {
      the_table <- DT::datatable(the_table, options = list(pageLength = nrows, 
                                                           columnDefs = list(list(className = "dt-right", 
                                                                                  targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
    }
  }
  return(the_table)
}

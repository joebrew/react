# the_date <- Sys.Date()
library(tidyverse)
library(jsonlite)
library(shiny)
library(shinySignals)
library(dplyr)
library(shinydashboard)
library(bubbles)
library(leaflet)
library(rCharts)
library(ggthemes)
library(RColorBrewer)
library(cism)
library(RPostgreSQL)
library(dplyr)
library(cism)
library(sp)
# Create a dataframe of scenarios
scenarios_df <- 
  data_frame(scenario = 1:3,
             description = c('Large-scale MDA',
                             'Focal MDA',
                             'Household MDA with radial extension'))

# Read in dashboard data
district_static <- fromJSON(txt = 'data/dashboard.json')

# Reformat
district_static$date <- 
  as.Date(district_static$date, '%m/%d/%Y')



# Create lat/lng columns
district_static <-
  district_static %>%
  mutate(lng = as.numeric(gps_longitude),
         lat = as.numeric(gps_latitude))

# Remove those with no dates
district_static <- district_static %>%
  filter(!is.na(date))

# Get maximum date
max_date <- max(district_static$date, na.rm = TRUE)

# For now, set today to the max date
# today <- max_date +1
# yesterday <- max_date
today <- the_date
yesterday <- the_date - 1

# Get weather malaria or not
district_static$malaria <-
  district_static$slidesyno == 2 |
  district_static$thicksmear %in% as.character(2:5) |
  district_static$rdt %in% c('2', '3')

# Get average numer of cases
avg_cases <- length(which(district_static$malaria)) / 
  length(unique(district_static$date))

# Get magude and manhica map
area_map <- moz3
area_map <- area_map[area_map$NAME_2 %in% c('Manhiça', 'Magude'),]
area_map@data$color <- ifelse(area_map@data$NAME_2 == 'Magude',
                              'darkgreen',
                              'darkorange')

# Read in dhis2 data

# Read the credentials
# This requires a yaml with the following fields
# # dhis2_db_dbname
# # dhis2_db_hos,
# # dhis2_db_port
# # dhis2_db_username
# # dhis2_db_password
this_date <- Sys.Date()
file_name <- paste0('dhis2_',
                    this_date,
                    '.RData')
if(file_name %in% dir('data/')){
  load(paste0('data/', file_name))
} else {

  credentials <- cism::credentials_extract()
  credentials <- credentials[grepl('dhis2_db', names(credentials))]

  # Create a connection
  psql_connection <- dbConnect(drv = dbDriver("PostgreSQL"),
                               dbname = credentials$dhis2_db_dbname,
                               host = credentials$dhis2_db_host,
                               port = credentials$dhis2_db_port,
                               user = credentials$dhis2_db_username,
                               password = credentials$dhis2_db_password)

  # Amone's query
query <- paste0("SELECT g.name, a.programinstanceid, a.created, a.enrollmentdate, a.incidentdate, a.trackedentityinstanceid, a.programid,
       a.organisationunitid, a.latitude, a.longitude, b.programstageinstanceid, c.value attributevalue, d.code attributecode, d.name attributename,
       e.value dataelementvalue, f.code dataelementcode, f.name dataelementname
FROM programinstance a LEFT JOIN programstageinstance b ON a.programinstanceid = b.programinstanceid
  LEFT JOIN trackedentityattributevalue c ON a.trackedentityinstanceid = c.trackedentityinstanceid
  LEFT JOIN trackedentityattribute d ON c.trackedentityattributeid = d.trackedentityattributeid
  LEFT JOIN trackedentitydatavalue e ON e.programstageinstanceid = b.programstageinstanceid
  LEFT JOIN dataelement f ON e.dataelementid = f.dataelementid
  LEFT JOIN organisationunit g on a.organisationunitid = g.organisationunitid")
x <- dbGetQuery(psql_connection, query)
save(x, file = '~/Desktop/temp.RData')
# # Filter so that it's all after 23 February
# x <- x %>%
#   filter(enrollmentdate > '2017-02-23')

# Loop through each program id and make dataframes
programids <- sort(unique(x$programid))

# NEED TO FIGURE OUT HOW TO KEEP att_react_index, FOR LINKING BETWEEN TABLES
for (i in 1:length(programids)){
  message(i)
  # Get the program id
  this_programid <- programids[i]
  # Subset just for this programid
  this_program <- x %>% filter(programid == this_programid)
  # # Use attribute code, but if not available, get attribute name
  # this_program$attributecode <-
  #   ifelse(is.na(this_program$attributecode),
  #          gsub(' ', '_', tolower(this_program$attributename)),
  #               this_program$attributecode)
  # Spread
  out <- this_program %>%
    dplyr::select(attributename,
                  attributevalue,
                  programstageinstanceid,
                  enrollmentdate,
                  dataelementcode,
                  dataelementvalue,
                  organisationunitid) %>%
    rename(date = enrollmentdate)
  # Remove duplicate rows
  out <- out %>% distinct(attributename,
                          attributevalue,
                          programstageinstanceid,
                          dataelementcode,
                          dataelementvalue,
                          .keep_all = TRUE)
  out <- spread(data = out,
                key = dataelementcode,
                value = dataelementvalue)
  # Spread again
  out <- out %>%
    filter(!is.na(programstageinstanceid))

  out <- spread(data = out, 
                key = attributename, 
                value = attributevalue)
  out <- out[,names(out) != '<NA>']
  # out <- spread(data = out,
  #               key = dataelementcode,
  #               value = dataelementvalue)

  # Filter out those rows which do not have a - in agregado
  if("Numero de agregado" %in% names(out)){
    out <- out %>% filter(!is.na(`Numero de agregado`))
  }

  assign(paste0('program_id_',
                this_programid),
         out,
         envir = .GlobalEnv)
  write_csv(out,
            paste0('~/Desktop/',
                   'program_id_',
                          this_programid,
                   '.csv'))
}

save(program_id_277797,
     program_id_7132,
     program_id_71885,
     program_id_71914,
     file = paste0('data/',
                   file_name))
}

# 277797: case index (the people who go to the health facility and are POSITIVE)
case_index_static <- program_id_277797
# 71885: agregado (each row is a visited agregado)
agregados_static <- program_id_71885
# 71914 membro: every person in the same agregado, but did not go to health facility
membros_static <- program_id_71914
# psql -h 172.16.236.244 -p 5432 -U jbrew dhis2

# Clean up magude data
case_index_static$date <- as.Date(case_index_static$date)
the_date <- max(case_index_static$date, na.rm = TRUE) + 1

case_index_static$last_week <- case_index_static$date >= the_date - 6
case_index_static$`Numero de agregado` <- 
  unlist(lapply(strsplit(x = case_index_static$`Identificação Permanente (PermID)`, split = '-'), function(x){paste0(x[1], '-', x[2])}))
agregados_static$date <- as.Date(agregados_static$date)
agregados_static$last_week <- agregados_static$date >= the_date - 6
membros_static$date <- as.Date(membros_static$date)
membros_static$last_week <- membros_static$date >= the_date - 6



# Create an object of all cases
magude_cases_all <- 
  case_index_static %>%
  group_by(date) %>%
  tally %>%
  bind_rows(
    membros_static %>%
      # ask bea if this is correct
      filter(react_form_b_rdt_result == 1) %>%
      group_by(date) %>% 
      tally
  ) %>%
  group_by(date) %>%
  summarise(n = sum(n))

# Create an object of magude cases by health facility
magude_cases_by_hf <- 
  case_index_static %>%
  group_by(date, 
           administrativePost) %>%
  tally %>%
  bind_rows(
    membros_static %>%
      # ask bea if this is correct
      filter(react_form_b_rdt_result == 1) %>%
      # get health facility
      left_join(case_index_static %>%
                  dplyr::select(`Numero de agregado`,
                                administrativePost)) %>%
      group_by(date, administrativePost) %>% 
      tally
  ) %>%
  group_by(date,
           administrativePost) %>%
  summarise(n = sum(n))

# Shift the dates in district_static up
date_shift <- as.numeric(max(case_index_static$date, na.rm = TRUE) -
  max(district_static$date, na.rm = TRUE))
district_static$date <- district_static$date + date_shift


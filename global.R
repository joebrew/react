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
# today <- Sys.Date()
today <- max_date +1
yesterday <- max_date

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

file_name <- paste0('dhis2_', 
                    Sys.Date(),
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
query <- paste0("SELECT a.programinstanceid, a.created, a.enrollmentdate, a.incidentdate, a.trackedentityinstanceid, a.programid,
       a.organisationunitid, a.latitude, a.longitude, b.programstageinstanceid, c.value attributevalue, d.code attributecode, d.name attributename,
       e.value dataelementvalue, f.code dataelementcode, f.name dataelementname
FROM programinstance a LEFT JOIN programstageinstance b ON a.programinstanceid = b.programinstanceid
  LEFT JOIN trackedentityattributevalue c ON a.trackedentityinstanceid = c.trackedentityinstanceid
  LEFT JOIN trackedentityattribute d ON c.trackedentityattributeid = d.trackedentityattributeid
  LEFT JOIN trackedentitydatavalue e ON e.programstageinstanceid = b.programstageinstanceid
  LEFT JOIN dataelement f ON e.dataelementid = f.dataelementid")
  
x <- dbGetQuery(psql_connection, query)
save(x, file = '~/Desktop/temp.RData')
# Filter so that it's all after 23 February
x <- x %>%
  filter(incidentdate > '2017-02-23')

# Loop through each program id and make dataframes
programids <- sort(unique(x$programid))

for (i in 1:length(programids)){
  message(i)
  # Get the program id
  this_programid <- programids[i]  
  # Subset just for this programid
  this_program <- x %>% filter(programid == this_programid)
  # Spread
  out <- this_program %>%
    dplyr::select(attributename,
                  attributevalue,
                  programstageinstanceid,
                  enrollmentdate,
                  dataelementcode,
                  dataelementvalue) %>%
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
  assign(paste0('program_id_',
                this_programid),
         out,
         envir = .GlobalEnv)
}

save(program_id_277797,
     program_id_71885,
     program_id_71914,
     file = paste0('data/', 
                   file_name))
}

# psql -h 172.16.236.244 -p 5432 -U jbrew dhis2



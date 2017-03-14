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
  
  query <- 
    paste0('SELECT j.code,
           j.name,  ', # nome da viariavel
           'j.dataelementid, ',
           'j.value, ', # valor da variavel
           'j.programstageinstanceid, ', # identificador de inquerito
           'j.latitude, ',
           'j.longitude, ',
           'j.organisationunitid, ', # identificador de unidade sanitaria
           'k.completeddate, ', #dia do inquerito
           'k.enrollmentdate, ', # dia da visita
           'k.trackedentityinstanceid ', # identificador de unidade sanitaria
           'FROM (
           SELECT
           e.code,
           e.name,
           e.dataelementid,
           f.value,
           f.programstageinstanceid,
           g.latitude,
           g.longitude,
           g.organisationunitid
           
           
           FROM dataelement e INNER JOIN trackedentitydatavalue f ON e.dataelementid = f.dataelementid
           INNER JOIN programstageinstance g ON f.programstageinstanceid = g.programstageinstanceid
           
    ) j
           INNER JOIN (
           SELECT *
           FROM programstageinstance h INNER JOIN programinstance i ON h.programinstanceid = i.programinstanceid
           ) k ON j.programstageinstanceid = k.programstageinstanceid;')
  
x <- dbGetQuery(psql_connection, query)

# Spread the data
dhis2 <- spread(x %>%
              dplyr::select(code, value, programstageinstanceid),
            key = code,
            value = value)
save(dhis2,
     file = paste0('data/', 
                   file_name))
}

# psql -h 172.16.236.244 -p 5432 -U jbrew dhis2



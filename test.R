library(RPostgreSQL)
library(dplyr)
library(cism)

# Read the crednetials
credentials <- cism::credentials_extract()

# Create a connection
psql_connection <- dbConnect(drv = dbDriver("PostgreSQL"), 
                             dbname = "postgres",
                             host = credentials$dhis2_db_port, 
                             port = credentials$dhis2_db_port,
                             user = credentials$dhis2_db_username, 
                             password = credentials$dhis2_db_password)
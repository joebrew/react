library(RPostgreSQL)
library(dplyr)
library(cism)

# Read the credentials
# This requires a yaml with the following fields
# # dhis2_db_dbname
# # dhis2_db_hos,
# # dhis2_db_port
# # dhis2_db_username
# # dhis2_db_password
credentials <- cism::credentials_extract()
credentials <- credentials[grepl('dhis2_db', names(credentials))]

# Create a connection
psql_connection <- dbConnect(drv = dbDriver("PostgreSQL"), 
                             dbname = credentials$dhis2_db_dbname,
                             host = credentials$dhis2_db_host, 
                             port = credentials$dhis2_db_port,
                             user = credentials$dhis2_db_username, 
                             password = credentials$dhis2_db_password)

db_list_tables(psql_connection)

# react
Malaria elimination surveillance

## Configuration

You'll need a `credentials/credentials.yaml` which contains the following fields:

```
dhis2_db_username: xxx
dhis2_db_password: yyy
dhis2_db_host: 172.16.236.244
dhis2_db_port: 5432
api_username: yourname
api_password: yourpassword
```

## Dependencies

You'll need the following R packages:

```
tidyverse
jsonlite
shiny
dplyr
shinydashboard
leaflet
rCharts
ggthemes
RColorBrewer
cism
RPostgreSQL
dplyr
cism
sp
readstata13
scales
broom
```

## Data files

Most of the relevant data is retrieved directly within the application. However, to get things started, you'll need to populate your `data` folder with the below files:

```
data
├── BES_Magude.csv
├── dashboard.json
└── opd_magude_old
    ├── survmag_rrs_dis.dta
    └── survmag_rrs_us.dta
```

Email joebrew@gmail.com for questions / clarification.
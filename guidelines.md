Each U.S. - how many cases in the last month (> 10 or < 10 per WEEK)
  - Scenario 1: Big MDA
  - Scenario 2: Focal MDA
  - Scenario 3: Household MDA with radial focal MDA

  Each catchment area (health facility) follows one of the above 3 protocols.
  Each month-U.S. pair gets a new protocol

App should have (all data live in DHiS2, based on both health facility and APs):

- Total visits
- Total visits per health facility
- People tested
- People positive


# Meeting with Bea on 15 March

#####- On main landing page
- Include all districts from Maputo province (talk to Amone about this)

#####- On main react page
- Overall chart with trends (2016 and 2017) entire district of Magude
- One chart, last month only, number of 30 day  cum cases per health facility, stacked bars with colors indicating follow up status, horizontal line at equival of weekly 7
- 8 panels (one for each health facility) with a horiz line at 10 (threshold for scenario change) with 2 lines each: 2016 and 2017
- One map with cases

- Intervention tabset: 
  Per health facility (for scenario 3):
  - For each index case,
    - how many were followed up?
    - how many were treated, etc.
    - how many imported vs. indigenous
- One map
  - 3 Colors: index cases followed up, inex cases not followed up,
            - member cases
- Another map: regardless of whether index or member, showing where all cases are, colored by indigenousness

# 30 MARCH 2017 - with BEA

- DONE: JOE: Replace all instances of adminsitrativePost with "name"
- DONE: JOE: Barplot of cases by health facility should not be rolling 30 days - instead should be previous calendar month; give a drop-down menu to select the month, with default being previous month (but have option for current month)
- DONE: JOE: Add an additional trend chart which should have two lines (rather than one area): one for case indices, and the other for actively detected (membros)
- WAITING ON AMONE: JOE: Break up trend chart into APEs vs health facility stacked bars 
- WAITING ON AMONE: JOE: TAlk to Amona as to whether I have APEs data / where it is
- WAITING ON AMONE: JOE Cases by health facility should also be stacked bars with fill = apes vs. health facility detected
- DONE: JOE: Remove the map-in-map, since "we all know where we are, dumbass"
- BEA: Figure out if in census2 there was a catchment area proxy (question 25)
- HELENE: Send to Joe mysql name of db for census2
(Already sent: cmag16_a<-dbGetQuery(conn, "SELECT * FROM sapodk.CMAG16_FORM_A_CORE")
cmag16_b<-dbGetQuery(conn, "SELECT * FROM sapodk.CMAG16_FORM_B_CORE"))
- JOE: Use census2 if there is a catchment areas proxy, use voronoi tesselation (http://economicsofmalaria.com/magude) to create natural catchment area borders; create either choropleth or kernel density PLUS actual points (keep the expanding hotspot stuff)
- WORKING ON THIS: JOE: Add checkbox to leaflet maps so as to be able to choose whether to cluster or not
- IN BEA'S COURT: BEA: Clarify with Amone exactly what administrative post codes correspond to
- WORKING ON THIS: JOE: In the barchart of "action by health facility", order so that index and followed-up are one stacked bar (two colors), followed by imported and secondary and PANEL the chart by health facility
- DONE: JOE: REmove the minimaps from everywhere cause Bea's a jerk.
- JOE: On follow-up status map, add drop down menu to filter which kinds of cases will be shown
- JOE: Replace the number slider with a date range slider
- JOE: Replace react and react int with health facility surveillance and field surveillance (REACT)
- JOE: Make the glyphicon boxes be urls so that when clicked it gives the next level of data (from aggregate number to number by health facility to individual data)
- JOE: Talk to Amone about the 200 meter radius
- Joe: Create a table of problem instances (things like members without agregado or index case number, etc.). 
- JOE: Create a "safety" section at the end of field surveillance
  - HELENA will help us on this (1st trim preg women treated)

- Create a "branching" user experience:
- N cases during T time for P place (this tells us the scenario for Pedro)
- Index case -> Focal MDA or Bairro MDA (these are called contacts or members) -> Contacts statistics (% tested, % pos) 
- As of now there is an unknown denominator which is the houses that SHOULD be followed up. Follow-up is never just for household, always at least 200m radius or neighborhood.

- Select health facility, time period and show all index cases.
- One row for each index case, with columns showing date, name, number of people contacted, number and percent of RDT positives (just for index case household), number and percent of RDT positives (for entire MDA area excluding the household), followed-up vs not followed up (if the latter, the other columns will be empty), locality, agregado number, permid number, get code for follow-up houses from Amone

# 20 April 2017
- Implement weeks for charts. A week is the first 7 day period starting on Monday in which day 1 is in that month. A week belongs to the month in which most of those days matte DONE
- For the purposes of a "month", use only those weeks which belong to that month rather than the days which belong to that month DONE
- We are waiting on Amone to integrate APEs data into database.
- Joe: We need lng/lat data to be associated with agregados
- Joe: The only thing we need from census2 is catchment area
- 2 big things: APEs data and GPS coordinates
- Ask Amone about getting imported/indigenous algor results 

# May 4 2017
DONE - Make the "Trend: all cases" be only "Trend: index cases"
DONE - "Trends by health facility" chart should be:
DONE  - Only index cases
DONE  - Should be much larger (row on its own)
DONE  - Should include another one with just member cases
DONE- "Cases by health facility" chart should be on the intervention tab
- Use magude2 census to get lat/lng for those houses without
DONE - Rename tabs to "react: weekly surveillance"; "react: RCD"
DONE - Replace "cases averted last month"
DONE - Use RRS data sent by Bea  to get some sort of comparison between past and present
DONE  - Put a graph of 2016 / 2017 comparison at bottom of surveillance tab. One calendar year, with color of line reflecting the year
DONE:  - Action by Health Facility:
    - This shows the number of index cases, the number of them which were followed - up (there is react dhis2 tracker info)
DONE:      -The followed-up bar is green, and it should fully cover the red index case bar
      - (green means it has been fully followed up)
      - lines connection stuff
- We need to be able to see full info (via click) for not yet followed up cases
DONE:- After the graph, table with only these 4 people have not been followed up
- Raw data:
DONE:  - Option to download only index cases
DONE:  - Option to download all cases (with repeating ids for the associated index case)
DONE:- Make malaria risk map with all malaria info from last month    

Side note: RRS is not going to give us much info
- Index case info is now going to come from field (ask Amone about when that change happens)

May 16, 2017
- Make sure that "secondary" include all contacts followed up (not necessarily cases)
- Make the "index cases which have not been followed up yet" table only include those cases which went incident in last 3 days
- Map showing index cases, and secondary cases of last month
- Risk map moves to surveillance
    - Show for last 3 months, just passive clinical cases, raster
- For the points (index and secondary) map, create polygons around each set of belonging
- For raw data, be able to download the agg rrs data (like Amone's table)

# General
- See if get SIS-MA credentials (Amone, if he doesn't have, from Fabiao); write (joe) web-scraper to download automatically every time app is run

# Malaria forecast
- Work with Bea on this.    
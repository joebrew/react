# React web app

(Changes to be made in July 2017)

## Surveillance tab

- The data used for this tab should be coming from RRS (weekly health facility data collected through CISM dhis2, which can be accessed through the link http://dbs.manhica.net:4880/dhis/dhis-web-commons/security/login.action, clicking on the Magude tab and searching for the "General Report" table)
- *JOE*: Change name of tab from "Surveillance" to "Magude Surveillance"
- *AMONE*: Ensure Joe has programmatic access to these data.
- *JOE*: Create a MACEPA-style bar chart with reporting (ie, shold be one bar per week to represent expected reports)
	- Include total passive cases by HF and APE, as well as total active cases
- *JOE*: Remove orange box on top
- *JOE*: Green box on top should compare cases to same period in 2016

## General chart changes

- The Trend of Index Cases and Members should be line graphs with two lines: 
	- Number of Individuals tested 
	- Number of Individuals positive
- Particularly for the Index case graph, it would be nice to be able to break it down in "detected by APE" and "Detected by HF" 

## Changes in titles 

- Trend: index cases --> "Passive Detection: Weekly RDT tested and positive cases by HF or APEs"
- Trend: members -->  "Active Detection: Weekly RDT tested and positive cases at the community" 
- Trends by health facility --> Number of Positive Cases by HF 
- Is there a way to separate the graphs per health facility a bit more so that is clearer which graph is from which HF? (like a square or sth....) 

## General comments 

- The historical comparison is not very clear: 
- Blue smooth line not necessary 
- I would actually remove data from 2013, 2014 and probably 2015 also (otherwise, you cant zoom in to today)
- Here we should add a risk map (or a map of incidences such as de one in MACEPA's app) based on the positive cases (in HF and community) during the same transmission season. 


## other tabs and issues

RCD: 
Action by Health facility: 
The index cases should exclude those who are from outside of Magude (and specify in the graph "index cases from Magude") --> Ask AMONE about how to classify cases from inside or outside of Magude. 
Need to include the green color in the legend 
Need to specify the date range somewhere in the graph 
The Index cases and member cases by HF graphs are not really needed here (they are already in the "surveillance" tab right?) 
Instead, we should add a line graph of imported vs indigenous cases through time. 
MAPS: Both of them should specify the date range
the map on the left should be green (if RDT negative) and red (if positive RDT). 
The map on the right is ok for now. 
Malaria Forecast: 
The predicted incidence map should have a color legend 
Raw Data: 
This tab should also include RRS data and BES data 
General: 
The title of the tab should be "Malaria in Maputo" or "Maputo Province Surveillance". 
This tab should be the first of all tabs using SISMA data 
Here, we could also include the table/bar graphs as in MACEPA's app. My only concern is the calculation of INCIDENCE, which we can leave out for now, unless ANYONE of us is able to find reliable denominator data?!
Bellow the table/bar graphs, this tab should also have weekly trends of tested and positive cases for each district for which we are already collecting data. 
I think that if we can get all of this up and running by beginning of August, we will be in a very good position to start talking with MACEPAs app developers. 

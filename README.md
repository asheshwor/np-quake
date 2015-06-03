## Exploring USGS earthquake data using shiny dashboard

This is a simple implementation of Shiny dashboard to explore the Nepal earthquake (http://en.wikipedia.org/wiki/April_2015_Nepal_earthquake) data. The earthquake data used here is NOT real-time. The data was downloaded from USGS (csv format) and can be updated by replacing the file in the data folder. To filter the quakes in the vicinity of Nepal, only the quakes within the bounding box of Nepal map are used. (2/6/2015)

You can try the app live at: [not live yet] asheshwor.shinyapps.io/np-quake

### Attribution

Earthquake data: This app uses earthquake data from USGS http://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php
Map data: Map tiles from Mapbox https://www.mapbox.com/ Mapbox uses map data from Open Street Maps (http://www.openstreetmap.org/)

### R packages used

shinydashboard
leaflet
dplyr
scales
ggplot2
htmltools
rcolorbrewer

### Licence


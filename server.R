#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*          Nepal quake dashboard                                      *
#*  2015-05-31, 2015-07-12                                             *
#*                                                                     *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#* https://github.com/asheshwor/np-quake
# 2023 Jan update to fix 1) leaflet basemap error;
#   2) remove reshape2, now depreciated, package
#   3) remove the use of rgdal package
#   4) reworked leaflet to use leafletProxy
#   5) removed update graphs button, timeline, frequency etc. are all
#      interactive now
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Load packages
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(dplyr)
library(sf)
# require(reshape2)
library(scales)
require(ggplot2)
library(data.table)
# require(rgdal)
library(jsonlite)
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Read and prepare data
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
quake.file <- "data/all_month_merged.csv"  
quake <- read.csv(quake.file,
                  colClasses = c("character", "numeric", "numeric",
                                 "numeric", "numeric", "character",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "character", "character",
                                 "character", "character", "character"))
# QUAKES WITHIN THE NEPAL BOUNDING BOX
quake <- quake[(quake$longitude > 80.000 & quake$longitude < 88.183) &
                 (quake$latitude > 25.767 & quake$latitude < 30.450),]
# FUNCTION TO FORMAT DATE-TIME
formatTime <- function(timeString) {
  split1 <- strsplit(paste(timeString), "T")
  split2 <- strsplit(split1[[1]][2], "Z")
  fin <- paste0(split1[[1]][1], " ", split2[[1]][1])
}
quake$dateTime <- as.POSIXlt(sapply(quake$time, formatTime)) + 5.75*60*60
quake.sub <- quake[ , c(2:5, 16, 6:12, 14)]
quake.sub$size <- cut(quake.sub$mag,
                      c(2, 3.9, 4.9, 5.9, 6.9, 7.9),
                      labels=c("3.3 to 3.9", ">3.9 to 4.9", ">4.9 to 5.9", ">5.9 to 6.9", ">6.9 to 7.9"))
# GET DAMAGE DATA
damage <- read.csv("data/damage.csv")
damage$DISTRICT <- toupper(damage$DISTRICT)
damage.sub <- damage[damage$TOTALDEATH > 0,]
damage.sub <- damage[order(damage$TOTALDEATH, decreasing = TRUE),]
damage.sub$DISTRICT <- factor(damage.sub$DISTRICT,
                              damage.sub$DISTRICT, ordered = TRUE)
damage.sub <- damage.sub[1:12, c(2,5:6)]
# damage.sub <- melt(damage.sub, id.vars = "DISTRICT") #OLD CODE
# UPDATED TO USE pivot.longer
damage.sub <- damage.sub %>% 
  pivot_longer(cols = c(2,3),
               names_to = "variable",
               values_to = "value")
# COLOR PALLET
pallet <- colorFactor(c("gray32", "dodgerblue4",  "slateblue4", "purple", "firebrick1"),
                   domain = c("3.3 to 3.9", ">3.9 to 4.9", ">4.9 to 5.9", ">5.9 to 6.9", ">6.9 to 7.9"))
# SET MAP PROVIDER TILES
mapTiles <- "CartoDB.Voyager"
# SHINY SESSION
function(input, output, session) {
  # FILTER QUAKES WITHIN SELECTED DATERANGE
  getQuakes2 <- reactive({
    startDate <- as.POSIXlt(paste(as.character(input$daterange[1]),
                                  "00:00:01"))
    endDate <- as.POSIXlt(paste(as.character(input$daterange[2]),
                                "23:59:01"))
    quake.s2 <- quake.sub[quake.sub$dateTime > startDate &
                           quake.sub$dateTime < endDate,]
    return(quake.s2)
  })
 ## TIMELINE
 drawHist <- eventReactive(input$updateButton, {
   quake.sub <- getQuakes2()
   ggplot(quake.sub, aes(dateTime, mag, colour=size)) +
     geom_bar(stat="identity", colour="gray60",
              fill="gray60", alpha=0.5) +
     geom_point(size=3) +
     scale_colour_manual(name = "size",
                         values = c("gray32", "dodgerblue4",
                                    "slateblue4", "purple",
                                    "firebrick1")) +
   scale_x_datetime(breaks = date_breaks("1 month"),
                    labels = date_format("%d-%b-%Y")) +
     scale_y_continuous(breaks=c(seq(1,9,2))) +
   xlab("") + ylab("Magnitude") +
   theme(plot.background = element_rect(fill = "white", colour = NA),
         panel.background = element_rect(fill = "white", colour = NA),
         title = element_text(colour="black", size = 13),
         axis.title.x = element_text(hjust=1, colour="black", size = 8),
         axis.title.y = element_text(vjust=90, colour="dodgerblue4", size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         legend.position = "none")
 })
 # SUMMARY TABLE
 quakeSummaryTable <- eventReactive(input$updateButton, {
   quake.sub <- getQuakes2()
   freq <- table(quake.sub$size)
   ftab <- data.frame(cbind(names(freq),
                            freq,
                            round(prop.table(freq)*100, 2)
   ))
   names(ftab) <- c("Magnitude", "Freq.", "%")
   row.names(ftab) <- NULL
   ftab
 })
 #  FREQUENCY TABLE
 output$outFrequency <- renderTable(quakeSummaryTable())
 # QUAKE HISTOGRAM
 quakeHist <- eventReactive(input$updateButton, {
   quake.sub <- getQuakes2()
   #  draw quake histogram
   ggplot(data=quake.sub, aes(x=mag)) +
     geom_histogram(aes(fill = after_stat(count)), binwidth=0.25,
                    colour = "white") +
     xlab("Magnitude") + ylab("") +
     scale_fill_gradient("Count", low = "darkgreen", high = "red") +
     theme(plot.background = element_rect(fill = "white", colour = NA),
           panel.background = element_rect(fill = "white", colour = NA),
           title = element_text(colour="black", size = 13),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank(),
           legend.position = "right")
 })
 # READ MAP DATA
 # map <- readOGR("mapdata/nepal-district.geojson") #OLD CODE
 map <- geojsonsf::geojson_sf("mapdata/nepal-district.geojson")
 map$rn <- row.names(map)
 # tmp.map <- data.table(map@data) # OLD CODE
 tmp.map <- data.table(map %>% st_drop_geometry())
 merged <- merge(tmp.map, damage, by="DISTRICT", all.x=TRUE)
 merged$roll <- as.numeric(merged$rn)
 merged <- merged[with(merged, order(roll)), ]
 # merged$cut <- cut(damage$TOTALDEATH, breaks=c(0,10,100,500,1000,4000), include.lowest=TRUE, dig.lab=4)
 ##  POPUP
 pu2 <- paste("<b>", as.character(tmp.map$DISTRICT), "</b><br>",
             "<b>Deaths:</b><b>", as.character(merged$TOTALDEATH), "</b><br>",
             "<b>Deaths - Female:</b>", as.character(merged$DEATHFEMALE), "<br>",
             "<b>Deaths - Male:</b>", as.character(merged$DEATHMALE), "<br>",
             "<b>Deaths - Unknown:</b>", as.character(merged$DEATHUNKNOWN), "<br>",
             "<b>Injured:</b>", as.character(merged$INJURED), "<br>"
 )
#COLOR PALLET FOR DAMAGE MAP
pal2 <- colorNumeric(c(NA, "firebrick1", "firebrick4"), c(0, 10, 100, 500, 1000, 3500))
 # UPDATE MAIN MAP
 output$quakemap2 <- renderLeaflet({
   qdata <- getQuakes2()
   pu <- paste("<b>Mag:</b>", as.character(qdata$mag), "<br>",
               "<b>Depth:</b>", as.character(qdata$depth), "km<br>",
               "<b>Time:</b>", as.character.POSIXt(qdata$dateTime), "NST",
               "<br>","<b>ID:</b>", qdata$id,"<br>",
               "<b>Place:</b>", qdata$place #noticed some pecularities with the place, need to re-check
   )
   leaflet(data = qdata) %>% 
     addProviderTiles(mapTiles) %>%
     setView((80.000 + 88.183)/2, (25.767 + 30.450)/2,  zoom = 7) %>%
     addCircleMarkers(~longitude, ~latitude,
                      popup = pu,
                      radius = ~ifelse(mag < 3.9, 4, 5),
                      color = ~pallet(size),
                      stroke = FALSE, fillOpacity = 0.6) %>%
     addLegend(
       "bottomleft", pal = pallet,
       values = sort(qdata$size),
       title = "Magnitude"
       # labFormat = labelFormat()
     )
 })
#update damage map
 output$damagemap <- renderLeaflet({
   leaflet(map) %>%
     addProviderTiles(mapTiles) %>%
     setView((80.000 + 88.183)/2, (25.767 + 30.450)/2,  zoom = 7) %>%
     # addProviderTiles(mapTiles) %>%
     addPolygons(
       fillOpacity = 0.6,
       fillColor = ~pal2(merged$TOTALDEATH),
       smoothFactor = 0.5,
       color = "darkgreen", weight=2,
       popup = pu2) %>%
     addLegend("bottomleft", pal = pal2,
               values = merged$TOTALDEATH,
               title = "Total fatalities",
               labFormat = labelFormat()
     )
 })
 #update damage GRAPH
 output$damagegraph2 <- renderPlot({
   ggplot(damage.sub,
                 aes(x = DISTRICT, y = value,
                     fill=variable), colour=NA) +
     geom_bar(stat="identity", position="dodge") +
     scale_fill_manual(values = c("gray30","gray60"), name="Legend") +
     # geom_text(aes(label = value, position=value), size = 3) + 
     xlab("") + ylab("Fatalities") +
     theme(plot.background = element_rect(fill = "white", colour = NA),
           panel.background = element_rect(fill = "white", colour = NA),
           title = element_text(colour="black", size = 13),
           axis.title.x = element_text(hjust=1, colour="black", size = 8),
           axis.title.y = element_text(vjust=90, colour="black", size = 8),
           panel.border = element_blank(),
           legend.position = "bottom")
  })
 #count total quakes
 output$countQuake <- renderText(paste("There were a total of<b>",
                                       nrow(getQuakes2()),
                                       "</b> quakes recorded from <b>",
                                       as.character(input$daterange[1]),
                                       "</b>to<b>", as.character(input$daterange[2]),
                                       "</b>.<br>"))
 output$adf <- renderText({
   paste(as.character(input$daterange))
 })
 # ## update timeline 
 output$magHist <- renderPlot(
   drawHist()
 )
 # ## update histogram
 output$quakeHist <- renderPlot(quakeHist())
 #update table
 # output$quaketable <- renderDataTable(quakeDataTable())
 output$quaketable <- renderDataTable(getQuakes2()[,c(5, 4, 3, 6, 8:10, 12:13)])
 output$damagetable <- renderDataTable(damage)
}
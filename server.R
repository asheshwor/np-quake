#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*          Nepal quake dashboard                                      *
#*  2015-05-31                                                         *
#*                                                                     *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Load packages
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
library(shinydashboard)
library(leaflet)
library(dplyr)
library(scales)
require(ggplot2)
# library(data.table)
# require(rgdal)
# library(jsonlite)
# require(htmltools)
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
# filter quakes within bounding box of Nepal map
quake <- quake[(quake$longitude > 80.000 & quake$longitude < 88.183) &
                 (quake$latitude > 25.767 & quake$latitude < 30.450),]
# fn format dateTime
formatTime <- function(timeString) {
  split1 <- strsplit(paste(timeString), "T")
  split2 <- strsplit(split1[[1]][2], "Z")
  fin <- paste0(split1[[1]][1], " ",split2[[1]][1])
}
quake$dateTime <- as.POSIXlt(sapply(quake$time, formatTime)) + 5.75*60*60
# quake <- quake[with(quake, order(dateTime)), ]
quake.sub <- quake[ ,c(2:5, 16, 6:12, 14)]
quake.sub$size <- cut(quake.sub$mag,
                      c(2, 3.9, 4.9, 5.9, 6.9, 7.9),
                      labels=c("3.3 to 3.9", ">3.9 to 4.9", ">4.9 to 5.9", ">5.9 to 6.9", ">6.9 to 7.9"))
# colour pallet
pallet <- colorFactor(c("gray32", "dodgerblue4",  "slateblue4", "purple", "firebrick1"),
                   domain = c("3.3 to 3.9", ">3.9 to 4.9", ">4.9 to 5.9", ">5.9 to 6.9", ">6.9 to 7.9"))
# create html for popup
pu <- paste("<b>Mag:</b>", as.character(quake.sub$mag), "<br>",
            "<b>Depth:</b>", as.character(quake.sub$depth), "km<br>",
            "<b>Time:</b>", as.character.POSIXt(quake.sub$dateTime), "NST",
            "<br>","<b>ID:</b>", quake.sub$id,"<br>",
            "<b>Place:</b>", quake.sub$place)
# shiny session
function(input, output, session) {
  #filter quake fn
  getQuakes <- function() {
    startDate <- as.POSIXlt(paste(as.character(input$daterange[1]),
                                  "00:00:01"))
    endDate <- as.POSIXlt(paste(as.character(input$daterange[2]),
                                "23:59:01"))
    quake.sub <- quake.sub[quake.sub$dateTime > startDate &
                             quake.sub$dateTime < endDate,]
    return(quake.sub)
  }

  qm <- function() {
    quakes.sub <- getQuakes()
    tempmap <- leaflet(data=quakes.sub) %>% addProviderTiles('MapBox.asheshwor.m4g4pnci') %>%
      setView((80.000 + 88.183)/2, (25.767 + 30.450)/2,  zoom = 7) %>%
      addCircleMarkers(~longitude, ~latitude,
                       popup = pu,
                       radius = ~ifelse(mag < 3.9, 4, 5),
                       color = ~pallet(size),
                       stroke = FALSE, fillOpacity = 0.6)
    ## Add a legend once only
    if (input$updateButton == 0) {
      tempmap2 <- tempmap %>%
        addLegend(
          "bottomleft", pal = pallet,
          values = sort(quakes.sub$size),
          title = "Magnitude"
          # labFormat = labelFormat()
        )
      return(tempmap2)
    } else return(tempmap)
  }
 ## timeline
 drawHist <-    eventReactive(input$updateButton, {
   quake.sub <- getQuakes()
   ggplot(quake.sub, aes(dateTime, mag, colour=size)) +
     geom_bar(stat="identity", colour="gray60",
              fill="gray60", alpha=0.5) +
     geom_point(size=3) +
     scale_colour_manual(name = "size",
                         values = c("gray32", "dodgerblue4",
                                    "slateblue4", "purple",
                                    "firebrick1")) +
#      geom_vline(xintercept = as.numeric(quake.sub$dateTime[quake.sub$mag > 6.5]),
#                 colour="red", alpha=0.25) +
   scale_x_datetime() +
     scale_y_continuous(breaks=c(seq(1,9,2))) +
     # ylim(c(2, 8)) +
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
 quakeSummaryTable <- eventReactive(input$updateButton, {
   quake.sub <- getQuakes()
   freq <- table(quake.sub$size)
   ftab <- data.frame(cbind(names(freq),
                            freq,
                            round(prop.table(freq)*100, 2)
   ))
   names(ftab) <- c("Magnitude", "Freq.", "%")
   row.names(ftab) <- NULL
   ftab
 })
 #function to draw damage graph
#  damage <- read.csv("C:/Users/Lenovo/github/np-quake2/data/damage.csv")
#  damage$DISTRICT <- toupper(damage$DISTRICT)
#  drawdamage <- function() {
#    #convert to long format
#    damage.sub <- damage[damage$TOTALDEATH > 0,]
#    damage.sub <- damage[order(damage$TOTALDEATH, decreasing = TRUE),]
#    damage.sub$DISTRICT <- factor(damage.sub$DISTRICT, damage.sub$DISTRICT, ordered = TRUE)
#    damage.sub <- damage.sub[1:12,]
#    adf <- ggplot(damage.sub, aes(DISTRICT, TOTALDEATH)) +
#      geom_bar(stat="identity", colour=NA,
#               fill="firebrick3", alpha=0.5) +
#      # ylim(c(2, 8)) +
#      xlab("") + ylab("Fatalities") +
#      theme(plot.background = element_rect(fill = "white", colour = NA),
#            panel.background = element_rect(fill = "white", colour = NA),
#            title = element_text(colour="black", size = 13),
#            axis.title.x = element_text(hjust=1, colour="black", size = 8),
#            axis.title.y = element_text(vjust=90, colour="dodgerblue4", size = 8),
#            panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank(),
#            panel.border = element_blank(),
#            legend.position = "none")
#    return(adf)
#  }
 #  frequency table
 output$outFrequency <- renderTable(quakeSummaryTable())
 
#  quakeDataTable <- eventReactive(input$updateButton, {
#    quake.sub <- getQuakes()
#    quake.sub[,c(5, 4, 3, 6, 8:10, 12:13)]
#  })
 
 quakeHist <- eventReactive(input$updateButton, {
   #filter quakes
#    startDate <- as.POSIXlt(paste(as.character(input$daterange[1]),
#                                  "00:00:01"))
#    endDate <- as.POSIXlt(paste(as.character(input$daterange[2]),
#                                "23:59:01"))
   quake.sub <- getQuakes()
   #  draw quake histogram
   ggplot(data=quake.sub, aes(x=mag)) +
     geom_histogram(aes(fill = ..count..), binwidth=0.25,
                    colour = "white") +
     xlab("Magnitude") + ylab("") +
     scale_fill_gradient("Count", low = "darkgreen", high = "red") +
     theme(plot.background = element_rect(fill = "white", colour = NA),
           panel.background = element_rect(fill = "white", colour = NA),
           title = element_text(colour="black", size = 13),
#            axis.title.x = element_text(hjust=1, colour="black", size = 8),
#            axis.title.y = element_text(vjust=90, colour="dodgerblue4", size = 8),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank(),
           legend.position = "right")
 }
 )
 #damage map
 #read map data
#  map <- readOGR("C:/Users/Lenovo/github/np-quake2/mapdata/nepal-district.geojson", "OGRGeoJSON")
#  map$rn <- row.names(map)
#  tmp.map <- data.table(map@data)
#  merged <- merge(tmp.map, damage, by="DISTRICT", all.x=TRUE)
#  merged$roll <- as.numeric(merged$rn)
#  merged <- merged[with(merged, order(roll)),]
#  # merged$cut <- cut(damage$TOTALDEATH, breaks=c(0,10,100,500,1000,4000), include.lowest=TRUE, dig.lab=4)
#  ##  Popup
#  pu2 <- paste("<b>", as.character(tmp.map$DISTRICT), "</b><br>",
#              "<b>Deaths:</b><b>", as.character(merged$TOTALDEATH), "</b><br>",
#              "<b>Deaths - Female:</b>", as.character(merged$DEATHFEMALE), "<br>",
#              "<b>Deaths - Male:</b>", as.character(merged$DEATHMALE), "<br>",
#              "<b>Deaths - Unknown:</b>", as.character(merged$DEATHUNKNOWN), "<br>",
#              "<b>Injured:</b>", as.character(merged$INJURED), "<br>"
#  )
#  
#  ##  Draw map
#  pal2 <- colorNumeric(c(NA, "firebrick1", "firebrick4"), c(0, 10, 100, 500, 1000, 3500))
#  damagemap <- leaflet(map) %>% addProviderTiles('MapBox.asheshwor.m4g4pnci') %>%
#    setView((80.000 + 88.183)/2, (25.767 + 30.450)/2,  zoom = 7) %>%
#    addProviderTiles('MapBox.asheshwor.m4g4pnci') %>%
#    addPolygons(
#      fillOpacity = 0.6,
#      fillColor = ~pal2(merged$TOTALDEATH),
#      smoothFactor = 0.5,
#      color = "darkgreen", weight=2,
#      popup = pu2
#    ) %>%
#    addLegend("bottomleft", pal = pal2,
#              values = merged$TOTALDEATH,
#              title = "Total deaths",
#              labFormat = labelFormat()
#    )
 #update map
 output$quakemap <- renderLeaflet(qm())
 #update damage map
 # output$damagemap <- renderLeaflet(damagemap)
 #update damage map
 # output$damagegraph <- renderLeaflet(drawdamage)
 #count total quakes
 output$countQuake <- renderText(paste("There were a total of<b>",
                                       nrow(getQuakes()),
                                       "</b> quakes recorded from <b>",
                                       as.character(input$daterange[1]),
                                       "</b>to<b>", as.character(input$daterange[2]),
                                       "</b>.<br>"))
 output$adf <- renderText({
   paste(as.character(input$daterange))
 })
 #update timeline
 output$magHist <- renderPlot(
   drawHist()
 )
 #update histogram
 output$quakeHist <- renderPlot(quakeHist())
 #update table
 # output$quaketable <- renderDataTable(quakeDataTable())
 output$quaketable <- renderDataTable(getQuakes()[,c(5, 4, 3, 6, 8:10, 12:13)])
}
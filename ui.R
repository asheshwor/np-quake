#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*          Nepal quake dashboard                                      *
#*  2015-05-31                                                         *
#*                                                                     *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

library(shinydashboard)
library(leaflet)
# library(shinythemes)
header <- dashboardHeader(
  title = "Nepal quake"
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName="dashboard", icon = icon("tachometer")),
    menuItem("Data",tabName="help", icon = icon("table")),
    menuItem("About", tabName="source", icon = icon("info"))
  )
)
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(tabName ="dashboard",
            # h2("Map"),
            fluidRow(
              column(width = 12,
                     box(width = NULL, solidHeader = TRUE,
                         leafletOutput("quakemap", height = 400),
                         htmlOutput("countQuake", inline = FALSE)
                     )
              ),
              column(width=12,
                     box(#title="Select time window",
                         background = "red",
                         # solidHeader = TRUE,
                         width=NULL,
                         # collapsible=TRUE,
                         dateRangeInput("daterange", "Select date range:",
                                        start = "2015-04-12",
                                        end   = "2015-06-04",
                                        min = "2015-04-12",
                                        max = "2015-06-04"),
                         actionButton("updateButton", "Update graphs")
                         # verbatimTextOutput("adf")
                         )),
              column(width=12,
                     box(title='Quake timeline', solidHeader=TRUE,
                         background = "light-blue",
                         width = NULL,
                         collapsible = TRUE,
                         plotOutput("magHist", height=200)
                         # actionButton("refreshButton", "Draw timeline")
                     )
                     
              ),
              column(width=6,
                     box(title="Histogram",
                         background = "light-blue",
                         solidHeader = TRUE,
                         width=NULL,
                         # status = "success",
                         collapsible=TRUE,
                         plotOutput("quakeHist", height = 200)
                         # actionButton("histButton", "Draw histogram")
                     )),
              column(width=6, #3
                     box(title="Frequency table",
                         background = "light-blue",
                         # status="success",
                         solidHeader = TRUE,
                         width=NULL,
                         collapsible=TRUE,
                         tableOutput("outFrequency")
                     )) #
  )),
  ## About tab
  tabItem(tabName ="help",
          fluidRow(
          column(width=12,
                 h2("Data"),
                 p("Data source: Earthquake data downloaded from http://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php"),
                 box(title = 'Quake table', solidHeader=TRUE,
                     width = 12, collapsible=FALSE,
                     dataTableOutput("quaketable")
                 )
          ))
  ),
  ## Source tab
  tabItem(tabName ="source",
          h1("About"),
          includeHTML("about.txt")
  )
))

dashboardPage(skin='green',
  header,
  sidebar,
  body
)
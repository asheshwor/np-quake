#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*          Nepal quake dashboard                                      *
#*  2015-05-31, 2015-07-12                                             *
#*                                                                     *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# 2023 update to fix 1) leaflet basemap error;
#   2) remove reshape2, now depreciated, package
library(shinydashboard)
library(leaflet)
# library(shinythemes)
header <- dashboardHeader(
  title = "Nepal quake"
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Earthquakes", tabName="dashboard", icon = icon("bullseye")),
    menuItem("Fatalities", tabName="damage", icon = icon("ambulance")),
    menuItem("Data", tabName="data", icon = icon("table")),
    menuItem("About", tabName="about", icon = icon("circle-info"))
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
                                        end   = "2016-05-29",
                                        min = "2015-04-12",
                                        max = "2016-05-29"),
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
              column(width=8,
                     box(title="Histogram",
                         background = "light-blue",
                         solidHeader = TRUE,
                         width=NULL,
                         # status = "success",
                         collapsible=TRUE,
                         plotOutput("quakeHist", height = 200)
                         # actionButton("histButton", "Draw histogram")
                     )),
              column(width=4, #3
                     box(title="Frequency table",
                         background = "light-blue",
                         # status="success",
                         solidHeader = TRUE,
                         width=NULL,
                         collapsible=TRUE,
                         tableOutput("outFrequency")
                     )) #
  )),
  ## Damage tab
  tabItem(tabName ="damage",
            fluidRow(
              column(width = 12,
                     # h2("Fatalities and injuries"),
                     box(width = NULL, solidHeader = TRUE,
                         leafletOutput("damagemap", height = 400),
                         br(),
                         box(title='Fatalities by gender (12 most impacted districts)', solidHeader=TRUE,
                             background = "black",
                             width = NULL,
                             collapsible = TRUE,
                             plotOutput("damagegraph", height=400)
                             # 
                         )
                         # 
                     )
              ))
  ),
  ## Data tab
  tabItem(tabName ="data",
          tabsetPanel(
            tabPanel("Earthquakes", #tabPanel 1
                     fluidRow(
                       column(width=12,
                              # h2("Data"),
                              # p("Source: USGS"),
                              box(title = 'Earthquake records from USGS', solidHeader=TRUE,
                                  width = 12, collapsible=FALSE,
                                  dataTableOutput("quaketable")
                              )
                       )) #end fluidRow
            ), #end tabPanel 1
            tabPanel("Fatalities, injuries and damage", #tabPanel 2
                     fluidRow(
                       column(width=12,
                              # h2("Data"),
                              # p("Source: Ministry of Home Affairs (MoHA)/National Emergency Operation Center (NEOC) official figures for casualties and damages [5 June update] "),
                              box(title = "MoHA/NEOC official fatalities, injuries and damage figures", solidHeader=TRUE,
                                  width = 12, collapsible=FALSE,
                                  dataTableOutput("damagetable")
                              )
                       )) #end fluidRow
                     )
  )),
  ## About tab
  tabItem(tabName ="about",
          # h1("About"),
          includeHTML("about.html")
  )
))

dashboardPage(skin='green',
  header,
  sidebar,
  body
)
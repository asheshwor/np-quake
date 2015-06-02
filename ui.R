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
# library(shinythemes)

header <- dashboardHeader(
  title = "Nepal quake"
)
# ntext <- eventReactive(input$goButton, {
#   input$n
# })
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
#               column(width = 12,
#                      box(width = NULL, solidHeader = TRUE,
#                          leafletOutput("quakemap", height = 400)
#                      )
#               ),
              column(width=12,
                     box(title='Timeline', solidHeader=TRUE,
                         background = "light-blue",
                         width = NULL,
                         collapsible = TRUE,
                         htmlOutput("countQuake", inline = FALSE),
                         plotOutput("magHist", height=200),
                         actionButton("refreshButton", "Draw timeline")
                     )
                     
              ),
              column(width=3,
                     box(title="Select time window",
                         background = "green",
                         solidHeader = TRUE,
                         width=NULL,
                         collapsible=TRUE,
                         dateRangeInput("daterange", "Select date range:",
                                        start = "2015-04-12",
                                        end   = "2015-06-02"),
                         actionButton("updateButton", "Update daterange"),
                         verbatimTextOutput("adf")
                         )),
              column(width=3,
                     box(title="Frequency table",
                         background = "black",
                         status="success",
                         solidHeader = TRUE,
                         width=NULL,
                         collapsible=TRUE,
                         tableOutput("outFrequency")
                     )),
              column(width=6,
                     box(title="Histogram",
                         background = "green",
                         solidHeader = TRUE,
                         width=NULL,
                         status = "success",
                         collapsible=TRUE,
                         plotOutput("quakeHist", height = 200),
                         actionButton("histButton", "Draw histogram")
                         ))
#               column(width = 12, 
#                      box(width = NULL, status = "warning",
#                          uiOutput("magselect"),
#                          checkboxGroupInput("magnitude", "Quake magnitude",
#                                             choices = c(
#                                               Small = 4.5,
#                                               Medium = 6,
#                                               Large = 7
#                                             ),
#                                             selected = c(1, 2, 3)
#                          ),
#                          p(
#                            class = "text-muted",
#                            paste("Select magnitude"
#                            )
#                          )
#                          # actionButton("refreshButton", "Refresh map")
#                      )
#               )
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
          # includeText("about.txt"),
#           br(),
#           p("github.com/asheshwor/quake")
  )
))

dashboardPage(skin='red',
  header,
  sidebar,
  body
)
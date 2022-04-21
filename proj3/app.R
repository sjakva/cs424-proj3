# --------------------------------------------------------------
#
#   CS424 - Spring 2022
# Project 3 -- Big Yellow Taxi
#   Authors:  Jack Martin 
#     & Shoaib Jakvani
#
# --------------------------------------------------------------

# --------------------------------------------------------------
# # install.packages("shiny","tidyverse","shinydashboard","lubridate", "sf", "rgdal")
library(shiny)
library(lubridate)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(leaflet)
library(tidyverse)
library(DT)
library(data.table)
library(dplyr)
library(sf)
library(rgdal)
library(scales)

monthsAbbr <- c(
  "Jan.",
  "Feb.",
  "Mar.",
  "Apr.",
  "May.",
  "Jun.",
  "Jul.",
  "Aug.",
  "Sep.",
  "Oct.",
  "Nov.",
  "Dec."
)

#   You will only need a subset of the 23 columns in the data 
#   3.  Trip Start Timestamp    (string -> date and time)
#   5.  Trip Seconds            (int)
#   6.  Trip Miles              (float)
#   9.  Pickup Community Area   (int)
#   10. Drop-off community Area (int)
#   17. Company                 (string)
# list of needed columns
col_names <- c(
  "Trip Start Timestamp",
  "Trip Seconds", 
  "Trip Miles", 
  "Pickup Community Area", 
  "Dropoff Community Area", 
  "Company"
  )

# TaxiSelect <- fread("./Taxi_Trips_-_2019.tsv", 
#           select = col_names,
#           nrows = 10000)

# #Start-documentation for cleaning data -------------------------------------/

#   https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy
# Go two directories out project directory for tsv file
# TaxiSelect <- fread("../../Taxi_Trips_-_2019.tsv",
#                     select = col_names)

# #   filter out the rest of the data to cut it to 300 mb
# # 1) all trips less than 0.5 miles, 2) more than 100 miles, 
# # 3) less than 60 seconds, 4) greater than 5 hours, 
# # 5) all trips that either start/end outside of a Chicago community
# 
# # 1) all trips less than 0.5 miles
# TaxiSelect <- TaxiSelect[!TaxiSelect$'Trip Miles' < 0.5]
# # 2) more than 100 miles
# TaxiSelect <- TaxiSelect[!TaxiSelect$'Trip Miles' > 100]
# # 3) less than 60 seconds
# TaxiSelect <- TaxiSelect[!TaxiSelect$'Trip Seconds' < 60]
# # 4) greater than 5 hours == 18,000 seconds
# TaxiSelect <- TaxiSelect[!TaxiSelect$'Trip Seconds' > 18000]
# # 5) drop NA values (trips outside Chicago community)
# TaxiSelect <- TaxiSelect[!is.na(TaxiSelect$`Pickup Community Area`)]
# TaxiSelect <- TaxiSelect[!is.na(TaxiSelect$`Dropoff Community Area`)]
# 
# # view(unique)
# valMap = c(1:55)
# #use integers to denote different companies in order to reduce file size
# TaxiSelect$Company = mapvalues(TaxiSelect$Company, unique, valMap)
# 
# # turns timestamp of starting trip to easier data to work with: hour/date
# TaxiSelect$NewDate <- parse_date_time(TaxiSelect$'Trip Start Timestamp', "%m/%d/%Y %I:%M:%S Op")
# TaxiSelect$Hour <- hour(TaxiSelect$NewDate)
# TaxiSelect$Date <- date(TaxiSelect$NewDate)
# #removes unneeded columns to save file size
# TaxiSelect <- subset (TaxiSelect, select = -NewDate)
# TaxiSelect <- subset (TaxiSelect, select = -`Trip Start Timestamp`)
# # 
# # #writes result to csv file
# # fwrite(TaxiSelect,"TaxiData.csv")
# 
# # end-documentation of cleaning data----------------------------------------/



# merging files together start -----------------------------
tbl_fread <- 
  list.files(pattern = "*.csv") %>% 
  map_df(~fread(.))
#end-merging files together --------------------------------
view(head(tbl_fread))


# read in boundary data from 
#   https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
boundsRead = readOGR(dsn=getwd(), layer="geo_export")
view(head(boundsRead))
# summary(boundsRead)


# number of rides per day over the year
daysColNames = c("Date", "Count")
dataDaysByYear <- tbl_fread %>% 
                      group_by(as.Date(tbl_fread$Date)) %>% summarise(count=n())
# TODO: relabel values
# tbl_fread$Date <- as.Date((tbl_fread$Date), format="%b. %d")
# rename columns
colnames(dataDaysByYear) = daysColNames
view(dataDaysByYear)
# print(format(tbl_fread$Date, "%Om %d"))

# --------------------------------------------------------------


#------------------------------------
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Jack & Shoaib's Project 3", titleWidth = 300),
  dashboardSidebar(
    disable = FALSE,
    collapsed = FALSE,
    sidebarMenu(
      id = "tabs",
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("Home", tabName = "home", selected = TRUE),
      menuItem("About", tabName = "about"),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      selectInput("unitToggle", "Kilometers or miles", c('Kilometers', 'Miles')),
      selectInput("timeToggle", "Time format", c('12 hour (AM/PM)', '24 hour')),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      selectInput("commToggle", "Community Area", c('choice 1', 'choice 1')),
      selectInput("destToggle", "From/To", c('Starting from', 'Ending to')),
      selectInput("compToggle", "Taxi company", c('choice 1', 'choice 1'))
    )
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "home",
      # fluidRow is a grid of 12 width, width of 4 is a 1/3, 6 is 1/2, etc
      fluidRow(
        tabBox(
          title = "Chicago Community Areas Charts",
          width = 12,
          tabPanel("Day of year",    plotOutput("daysOfYearPlot", width = "100%")),
          tabPanel("Hour of day", "the distribution of the number of rides by hour of day based on start time (midnight through 11pm)"),
          tabPanel("Day of week", "the distribution of the number of rides by day of week (Monday through Sunday)"),
          tabPanel("Month", "the distribution of the number of rides by month of year (Jan through Dec)"),
          tabPanel("Mileage", "the distribution of the number of rides by binned mileage (with an appropriate number of bins)"),
          tabPanel("Trip time", "the distribution of the number of rides by binned trip time (with an appropriate number of bins)")
        )
      ),
      
      fluidRow(
        box(
          title = "Tables",
          solidHeader = TRUE,
          status = "warning",
          width = 9, background = "yellow"
        ),
        box(
          title = "Map",
          solidHeader = TRUE,
          status = "warning",
          width = 3, background = "yellow",
          leafletOutput("initMap")
        )
      )
    ),
    tabItem(
      tabName = "about",
      fluidRow(
        box(
          title = "About",
          solidHeader = TRUE,
          status = "warning",
          width = 12,
          
          
          h2(
            "Jack Martin and Shoaib Jakvani created this app for Project 3 of UIC's CS 424 - Visual Analytics."
          ),
          p(
            "This data is from the Chicago Data Portal. More specifically, the \'Taxi Trips - 2019\'.
                             We were tasked with analyzing and plotting entries over\n
                            the taxi trips taken in the Chicago community area in 2019, and charting them according to [FIXME: edit after project]."
          )
        )
      )
    )
    
  ))
  
)

#------------------------------------
# Define server logic
#   session as a param allows access to information and functionality relating to the session
server <- function(input, output, session) {
  # TODO: implement reactive date ----------------------------------- //
  # # changes dataset based on day
  # dateReactive <-
  #   reactive({
  #     subset(stationsAll, stationsAll$date == input$inputDate)
  #   })
  # 
  # # shifts data by one day in the past
  # observeEvent(input$left, {
  #   updateDateInput(
  #     session,
  #     "inputDate",
  #     value = input$inputDate - days(1),
  #     min = '2001-01-01',
  #     max = '2021-11-30'
  #   )
  # })
  # # shifts data by one day in the future
  # observeEvent(input$right, {
  #   updateDateInput(
  #     session,
  #     "inputDate",
  #     value = input$inputDate + days(1),
  #     min = '2001-01-01',
  #     max = '2021-11-30'
  #   )
  # })
  # ----------------------------------------------------------------- //
  
  
  output$initMap <- renderLeaflet({
    leaflet() %>% setView(lng =  -87.6000, lat = 41.9291, zoom = 10) %>%
      addProviderTiles(providers$Stamen.Terrain, options = providerTileOptions()
      ) %>% addPolygons(data = boundsRead, weight = 1.25, fillColor = "#d087e6", fillOpacity = 0.4, color ="#ffad33", opacity = 0.7,
              label = ~community, highlightOptions = highlightOptions(color = "#0f7a6c", fillOpacity = 0.8, weight = 2,
                                                        bringToFront = TRUE))
  })
  
  output$daysOfYearPlot <- renderPlot({
    # TODO: reactive title? ----------------------------------------- //
    # reactiveTitle <- paste("Number of rides by day per ", input$inputYear)
    # newDate <- dateReactive()
    # --------------------------------------------------------------- //
    
    ggplot(dataDaysByYear, aes(x = Date, y = Count)) + geom_bar(stat = "identity", fill = "#ffad33", width = 0.8) +
      labs(x = "Day", y = "Total number of rides") + theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
        scale_x_date(date_breaks = "day", date_labels = "%b. %d") + coord_cartesian(expand = FALSE)
        #           TODO: change date_breaks to 5 days if screen test fails
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

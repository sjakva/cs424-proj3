# --------------------------------------------------------------
#
#   CS424 - Spring 2022
# Project 3 -- Big Yellow Taxi
#   Authors:  Jack Martin 
#     & Shoaib Jakvani
#
# --------------------------------------------------------------

# --------------------------------------------------------------
# # install.packages("shiny","tidyverse","shinydashboard","lubridate", "sf", "rgdal", "measurements", "geojsonio)

library(shiny)
library(lubridate)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(leaflet)
library(tidyverse)
library(DT)
library(data.table)
library(sf)
library(rgdal)
library(scales)
library(measurements)
library(plyr)
library(dplyr)
library(geojsonio)


# #Start-documentation for cleaning data -------------------------------------/
#   You will only need a subset of the 23 columns in the data 
#   3.  Trip Start Timestamp    (string -> date and time)
#   5.  Trip Seconds            (int)
#   6.  Trip Miles              (float)
#   9.  Pickup Community Area   (int)
#   10. Drop-off community Area (int)
#   17. Company                 (string)
#
#
# # list of needed columns
# col_names <- c(
#   "Trip Start Timestamp",
#   "Trip Seconds", 
#   "Trip Miles", 
#   "Pickup Community Area", 
#   "Dropoff Community Area", 
#   "Company"
# )

#   https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy
# Go two directories out project directory for tsv file
# TaxiSelect <- fread("../../Taxi_Trips_-_2019.tsv",
#                     select = col_names)
# 
# # #   filter out the rest of the data to cut it to 300 mb
# # # 1) all trips less than 0.5 miles, 2) more than 100 miles, 
# # # 3) less than 60 seconds, 4) greater than 5 hours, 
# # # 5) all trips that either start/end outside of a Chicago community
# # 
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
# # 
# unique <- unique(TaxiSelect$)
# view(unique)
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
Taxi <- 
  list.files(pattern = "*.csv") %>% 
  map_df(~fread(.))
#end-merging files together --------------------------------

# adds km conversion to Taxi df
Taxi$km <- conv_unit(Taxi$`Trip Miles`, "mi", "km")
# read in boundary data from 
#   https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
boundsRead = readOGR(dsn=getwd(), layer="geo_export")
# view(head(boundsRead))
# summary(boundsRead)

#start-barchart stuff --------------------------------

# number of rides per day over the year
daysColNames = c("Date", "Count")
dataDaysByYear <- Taxi %>% 
  group_by(as.Date(Taxi$Date)) %>% dplyr::summarise(count=n())

colnames(dataDaysByYear) = daysColNames
#number of rides by hour of day based on start time (midnight through 11pm)

hoursColNames = c("Hour","Count")
dataHoursByDay <- Taxi %>%
  group_by(Taxi$Hour) %>% dplyr::summarise(count=n())
colnames(dataHoursByDay) = hoursColNames
# view(dataHoursByDay)


# number of rides by day of week (Monday through Sunday)
dayColNames = c("Day","Count")
x <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
y <-c("1","2","3","4","5","6","0")
dataWeekDay <- Taxi %>%
  group_by(as.POSIXlt(Taxi$Date)$wday) %>% dplyr::summarise(count=n())
colnames(dataWeekDay) = dayColNames

dataWeekDay2 <- dataWeekDay %>%
  slice(match(y, dataWeekDay$Day))
dataWeekDay2$Day <- factor(dataWeekDay2$Day, levels = y)
# view(dataWeekDay2)


# number of rides by month of year (Jan through Dec) TODO still needs to convert numbers to months
monthsColNames = c("Month","Count")
dataHoursByMonth <- Taxi %>%
  group_by(month(Taxi$Date)) %>% dplyr::summarise(count=n())
colnames(dataHoursByMonth) = monthsColNames
# view(dataHoursByMonth)


# number of rides by binned mileage (with an appropriate number of bins)
breaks <- c(0.5,10,20,30,40,50,60,70,80,90,100)
group_tags <- cut(Taxi$`Trip Miles`, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)
group_tags <- as_tibble(group_tags)
# view(binning)

breaks <- c(0.6,20,40,60,80,100,120,140,170)
group_tagskm <- cut(Taxi$km, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)
group_tagskm <- as_tibble(group_tagskm)


ggplot(data = group_tagskm, mapping = aes(x=value)) +
  geom_bar(fill="#ffad33",color="white") +
  labs(x='Km Driven') +
  scale_y_log10(labels = scales::comma) + 
  theme_minimal()


# number of rides by binned trip time (with an appropriate number of bins) 
period <- ms(Taxi$`Trip Seconds`)
breaks <- c(60,1800,3600,5400,7200,9000,10800,12600,14400,16200,18000)
group_tags2 <- cut(Taxi$`Trip Seconds`, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)
group_tags2 <- as_tibble(group_tags2)


#end-barchart stuff --------------------------------


# view(head(Taxi))


#begin-heatmap 
# --------------------------------------------------
# pickupList <- list()
pickupTable <- table(Taxi$`Pickup Community Area`)
pickupList <- pickupTable / sum(pickupTable) * 100
# view(pickupList)

# fromList = list()
fromTable <- table(Taxi$`Dropoff Community Area`)
fromList <- fromTable / sum(fromTable) * 100
# view(fromList)

borders = geojson_read("data.geojson", what = "sp")

namesAlpha <-borders[order(borders$community),]
# view(namesAlpha)

borders$area_numbe = as.numeric(borders$area_numbe)
borders$Dropoff = as.numeric(borders$area_num_1)
borders$Pickup = as.numeric(borders$area_num_1)

borders = borders[order(borders$area_numbe),]

for (x in 1:length(fromList))
{
  borders$Dropoff[x] = fromList[x]
}
for (x in 1:length(pickupList))
{
  borders$Pickup[x] = pickupList[x]
}
# view(borders)
# borders$Dropoff = unlist(fromList,use.names = FALSE)
# borders$Pickup = unlist(pickupList,use.names = FALSE)
# print(typeof(borders))

#end-heatmap 
# --------------------------------------------------



#dashboard stuff
#------------------------------------
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Jack & Shoaib's Project 3", titleWidth = 300),
  dashboardSidebar(
    disable = FALSE,
    collapsed = FALSE,
    sidebarMenu(
      id = "tabs",
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      
      menuItem("Home", tabName = "home", selected = TRUE),
      menuItem("About", tabName = "about"),
      
      
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      menuItem("", tabName = "c", icon = NULL),
      
      selectInput("unitToggle", "Miles or Kilometers", c('Kilometers', 'Miles'), selected = 'Miles'),
      selectInput("timeToggle", "Time format", c('12 hour (AM/PM)', '24 hour'), selected = '24 hour'),
      selectInput("commToggle", "Community Area", namesAlpha$community),
      selectInput("destToggle", "From/To", c('Starting from', 'Ending to')),
      selectInput("compToggle", "Taxi company", c('choice 1', 'choice 2'))
    )
  ),
  dashboardBody(
    tags$style(type = "text/css", "#initMap {height: calc(100vh - 150px) !important;}"),
    tabItems(
    tabItem(
      tabName = "home",
      fluidRow(
        
        # Leaflet
        column(width = 4, offset = 0, style='padding:0px; height: 100%;',
               box(
                 title = "Map",
                 solidHeader = TRUE,
                 status = "warning",
                 width=12, background = "yellow",
                 leafletOutput("initMap")
                )
               ),
        
        # 6 Graphs & Tables
        column(width = 8, offset = 0, style='padding:0px;',
               # graph/table column 1
               column(width = 6,
                      fluidRow(
                        tabBox(
                          title="Distribution of the number of rides by day of year (Jan 1 through Dec 31)",
                          # solidHeader = TRUE, status = "warning",
                          tabPanel("Graph", plotOutput("daysOfYearPlot", width = "100%", height = 300)),
                          tabPanel("Table", dataTableOutput("daysOfYearTable")), width = 12,
                        )
                      ),
                      fluidRow(
                        tabBox(
                          title="Distribution of the number of rides by hour of day based on start time (midnight through 11pm)", 
                          # solidHeader = TRUE, status = "warning",
                          tabPanel("Graph", plotOutput("hoursByDayPlot", width = "100%", height = 300)),
                          tabPanel("Table", dataTableOutput("hoursByDayTable")), width = 12,
                        )
                      ),
                      fluidRow(
                        tabBox(
                          title="Distribution of the number of rides by day of week (Monday through Sunday)",
                          # solidHeader = TRUE, status = "warning",
                          tabPanel("Graph", plotOutput("weekDayPlot", width = "100%", height = 300)),
                          tabPanel("Table", dataTableOutput("weekDayTable")), width = 12,
                        )
                      )
                    ),
               
               # graph/table column 2
               column(width = 6,
                      fluidRow(
                        tabBox(
                          title="Distribution of the number of rides by month of year (Jan through Dec)",
                          # solidHeader = TRUE, status = "warning",
                          tabPanel("Graph", plotOutput("monthOfYearPlot", width = "100%", height = 300)),
                          tabPanel("Table", dataTableOutput("monthOfYearTable")), width = 12,
                        )
                      ),
                      fluidRow(
                        tabBox(
                          title="Distribution of the number of rides by binned mileage (with an appropriate number of bins)",
                          # solidHeader = TRUE, status = "warning"
                          tabPanel("Graph", plotOutput("mileagePlot", width = "100%", height = 300)),
                          tabPanel("Table", dataTableOutput("mileageTable")), width = 12,
                        )
                      ),
                      fluidRow(
                        tabBox(
                          title="Distribution of the number of rides by binned trip time (with an appropriate number of bins)",
                          # solidHeader = TRUE, status = "warning"
                          tabPanel("Graph", plotOutput("timePlot", width = "100%", height = 300)),
                          tabPanel("Table", dataTableOutput("timeTable")), width = 12,
                        )
                      )
               )
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
                            the taxi trips taken in the Chicago community area in 2019, and charting them according to each individual community and also the entirety of Chicago."
          )
        )
      )
    )
    
  ))
  
)


#server logic
#------------------------------------
# Define server logic
#   session as a param allows access to information and functionality relating to the session
server <- function(input, output, session) {
  
  communityPickupSubsetReactive <-
    reactive({
      userCommunity = input$commToggle
      
      userCommunity <- mapvalues(userCommunity, namesAlpha$community, namesAlpha$area_numbe)
      communitySubP = subset(Taxi, Taxi$`Pickup Community Area` == userCommunity)
      communitySubP

    })
  
  communityDropoffSubsetReactive <-
    reactive({
      userCommunity = input$commToggle
      
      userCommunity <- mapvalues(userCommunity, namesAlpha$community, namesAlpha$area_numbe)
      communitySubD = subset(Taxi, Taxi$`Dropoff Community Area` == userCommunity)
      communitySubD
    })
  
  destToggleReactive <-
    reactive({
      FromTo = input$destToggle
      FromTo
    })
  
  # Update community area chosen by selecting area on the map
  observeEvent(input$initMap_shape_click,{
    # read in map input
    p <- input$initMap_shape_click
    print(p$id)
    # map area number to community string name
    areaInput <- mapvalues(p$id, namesAlpha$area_numbe, namesAlpha$community)
    print(areaInput) # prints community name
    # input$commToggle = areaInput
    # update non-reactive input by updating existing selectInput box
    updateSelectInput(session, "commToggle", selected = areaInput)
  })
  
  # Returns group used for charting data showing mileage (5th chart)
  unitUsed <- reactive({
    if (input$unitToggle == 'Miles') {
      miles <- input$unitToggle    
    } else {
      km <- input$unitToggle 
    }
  })
  
  # Renders leaflet map by reading in boundary data from borders;
  #   data from https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
  output$initMap <- renderLeaflet({
    borders
    # color palette to show heatmap
    pal <- colorNumeric("plasma", NULL)
    
    leaflet(borders) %>%
      addTiles() %>% setView(lng =  -87.6000, lat = 41.9291, zoom = 13) %>%
      addPolygons(color = "black", weight = 1, smoothFactor = 0.5, layerId = ~area_numbe,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal(Pickup), 
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE), label = ~paste0(community, ": ", formatC(Pickup, big.mark = ","),"%")
      ) %>%
      addLegend(pal = pal, values = ~Pickup, opacity = 3.0,
                labFormat = labelFormat(transform = function(x) round(x)))
  })



  # distribution of the number of rides by day of year (Jan 1 through Dec 31)
  # ---------------------------------------------------------------------- //
  #   //  Chart   //
  output$daysOfYearPlot <- renderPlot({
    newPSubset <- communityPickupSubsetReactive()
    newDSubset <- communityDropoffSubsetReactive()
    commTog <- destToggleReactive()
    
    daysColNames = c("Date", "Count")
    # 'Starting from', 'Ending to'
    if(commTog == 'Starting from')
    {
      dataDaysByYear <- newPSubset %>% 
        group_by(as.Date(newPSubset$Date)) %>% dplyr::summarise(count=n())
      
      # dataDaysByYear <- filter(dataDaysByYear, "Pickup Community Area" == commNum)
      colnames(dataDaysByYear) = daysColNames
      # Taxi$`Pickup Community Area`
      
      ggplot(dataDaysByYear, aes(x = Date, y = Count)) + geom_bar(stat = "identity", fill = "#ffad33", width = 0.8) +
        labs(x = "Day", y = "Total number of rides") + theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        scale_x_date(date_breaks = "day", date_labels = "%b. %d") + coord_cartesian(expand = FALSE)
      #           TODO: change date_breaks to 5 days if screen test fails
    }
    else
    {
      dataDaysByYear <- newDSubset %>% 
        group_by(as.Date(newDSubset$Date)) %>% dplyr::summarise(count=n())
      
      # dataDaysByYear <- filter(dataDaysByYear, "Dropoff Community Area" == commNum)
      colnames(dataDaysByYear) = daysColNames
      
      
      ggplot(dataDaysByYear, aes(x = Date, y = Count)) + geom_bar(stat = "identity", fill = "#ffad33", width = 0.8) +
        labs(x = "Day", y = "Total number of rides") + theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        scale_x_date(date_breaks = "day", date_labels = "%b. %d") + coord_cartesian(expand = FALSE)
      #           TODO: change date_breaks to 5 days if screen test fails
    }
  })
  # ---------------------------------------------------------------------- //
  #   //  Table   //
  output$daysOfYearTable <- DT::renderDataTable(DT::datatable({
    
    newPSubset <- communityPickupSubsetReactive()
    newDSubset <- communityDropoffSubsetReactive()
    daysColNames = c("Date", "Count")
    commTog <- destToggleReactive()
    if(commTog == 'Starting from')
    {
      dataDaysByYear <- newPSubset %>% 
        group_by(as.Date(newPSubset$Date)) %>% dplyr::summarise(count=n())
    }
    else
    {
      dataDaysByYear <- newDSubset %>% 
        group_by(as.Date(newDSubset$Date)) %>% dplyr::summarise(count=n())
    }
    colnames(dataDaysByYear) = daysColNames
    dataDaysByYear$Date <- format(dataDaysByYear$Date, "%b. %d")
    dataDaysByYear$Count <- formatC(dataDaysByYear$Count, big.mark = ",")
    dataDaysByYear
  }))
  # ---------------------------------------------------------------------- //



  # distribution of the number of rides by hour of day based on start time (midnight through 11pm)
  # ---------------------------------------------------------------------- //
  #   //  Chart   //
  output$hoursByDayPlot <- renderPlot({
    
    newPSubset <- communityPickupSubsetReactive()
    newDSubset <- communityDropoffSubsetReactive()
    commTog <- destToggleReactive() 
    
    hoursColNames = c("Hour","Count")
    
    if(commTog == 'Starting from')
    {
      dataHoursByDay <- newPSubset %>%
        group_by(newPSubset$Hour) %>% dplyr::summarise(count=n())
      colnames(dataHoursByDay) = hoursColNames
      
      ggplot(dataHoursByDay, aes(x = Hour, y = Count)) + geom_bar(stat = "identity", fill = "#ffad33", width = 0.8) +
        labs(x = "Hour", y = "Total number of rides") + theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
        coord_cartesian(expand = FALSE) +#+ scale_x_discrete(labels=c(0:23))
        scale_y_continuous(labels = scales::comma)
      
    }# scale_x_date(date_breaks = "day", date_labels = "%b. %d") + coord_cartesian(expand = FALSE)
    else
    {
      dataHoursByDay <- newDSubset %>%
        group_by(newDSubset$Hour) %>% dplyr::summarise(count=n())
      colnames(dataHoursByDay) = hoursColNames
      
      ggplot(dataHoursByDay, aes(x = Hour, y = Count)) + geom_bar(stat = "identity", fill = "#ffad33", width = 0.8) +
        labs(x = "Hour", y = "Total number of rides") + theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
        coord_cartesian(expand = FALSE) +#+ scale_x_discrete(labels=c(0:23))
        scale_y_continuous(labels = scales::comma)
    }
  })
  # ---------------------------------------------------------------------- //
  #   //  Table   //
  output$hoursByDayTable <- DT::renderDataTable(DT::datatable({
    newPSubset <- communityPickupSubsetReactive()
    newDSubset <- communityDropoffSubsetReactive()
    commTog <- destToggleReactive()
    
    
    hoursColNames = c("Hour","Count")
    if(commTog == 'Starting from')
    {
      dataHoursByDay <- newPSubset %>%
        group_by(newPSubset$Hour) %>% dplyr::summarise(count=n())
      colnames(dataHoursByDay) = hoursColNames
    }
    else
    {
      dataHoursByDay <- newDSubset %>%
        group_by(newDSubset$Hour) %>% dplyr::summarise(count=n())
      colnames(dataHoursByDay) = hoursColNames
    }
    # dataHoursByDay$Hour <- format(dataDaysByYear$Date, "%b. %d")
    colnames(dataHoursByDay) = hoursColNames
    dataHoursByDay$Count <- formatC(dataHoursByDay$Count, big.mark = ",")
    # as.data.frame.table(dataHoursByDay)
    # view(as.data.frame(dataHoursByDay))
    dataHoursByDay
  }))
  # ---------------------------------------------------------------------- //



  # distribution of the number of rides by day of week (Monday through Sunday)
  # ---------------------------------------------------------------------- //
  #   //  Chart   //
  output$weekDayPlot <- renderPlot({
    
    newPSubset <- communityPickupSubsetReactive()
    newDSubset <- communityDropoffSubsetReactive()
    commTog <- destToggleReactive() 
    
    dayColNames = c("Day","Count")
    x <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    y <-c("1","2","3","4","5","6","0")
    z <-c("Mon","Tues","Wed","Thur","Fri","Sat","Sun")
    
    if(commTog == 'Starting from')
    {
      dataWeekDay <- newPSubset %>%
        group_by(as.POSIXlt(newPSubset$Date)$wday) %>% dplyr::summarise(count=n())
      colnames(dataWeekDay) = dayColNames
      
      dataWeekDay2 <- dataWeekDay %>%
        slice(match(y, dataWeekDay$Day))
      dataWeekDay2$Day <- factor(dataWeekDay2$Day, levels = y)
      ggplot(dataWeekDay2, aes(x = Day, y = Count)) + geom_bar(stat = "identity", fill = "#ffad33", width = 0.8) +
        labs(x = "Day", y = "Total number of rides") + theme_bw() +
        scale_y_continuous(labels = scales::comma) +
        scale_x_discrete(labels = z) +
        theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
        coord_cartesian(expand = FALSE)
    }
    else
    {
      dataWeekDay <- newDSubset %>%
        group_by(as.POSIXlt(newDSubset$Date)$wday) %>% dplyr::summarise(count=n())
      colnames(dataWeekDay) = dayColNames
      
      dataWeekDay2 <- dataWeekDay %>%
        slice(match(y, dataWeekDay$Day))
      dataWeekDay2$Day <- factor(dataWeekDay2$Day, levels = y)
      ggplot(dataWeekDay2, aes(x = Day, y = Count)) + geom_bar(stat = "identity", fill = "#ffad33", width = 0.8) +
        labs(x = "Day", y = "Total number of rides") + theme_bw() +
        scale_y_continuous(labels = scales::comma) +
        scale_x_discrete(labels = z) +
        theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
        coord_cartesian(expand = FALSE)
    }
  })
  # ---------------------------------------------------------------------- //
  #   //  Table   //
  output$weekDayTable <- DT::renderDataTable(DT::datatable({
    newPSubset <- communityPickupSubsetReactive()
    newDSubset <- communityDropoffSubsetReactive()
    commTog <- destToggleReactive() 
    
    dayColNames = c("Day","Count")
    x <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    y <-c("1","2","3","4","5","6","0")
    if(commTog == 'Starting from')
    {
      dataWeekDay <- newPSubset %>%
        group_by(as.POSIXlt(newPSubset$Date)$wday) %>% dplyr::summarise(count=n())
      colnames(dataWeekDay) = dayColNames
      
      dataWeekDay2 <- dataWeekDay %>%
        slice(match(y, dataWeekDay$Day))
      dataWeekDay2$Day <- factor(dataWeekDay2$Day, levels = y)
    }
    else
    {
      dataWeekDay <- newDSubset %>%
        group_by(as.POSIXlt(newDSubset$Date)$wday) %>% dplyr::summarise(count=n())
      colnames(dataWeekDay) = dayColNames
      
      dataWeekDay2 <- dataWeekDay %>%
        slice(match(y, dataWeekDay$Day))
      dataWeekDay2$Day <- factor(dataWeekDay2$Day, levels = y)
    }
    dataWeekDay2$Count <- formatC(dataWeekDay2$Count, big.mark = ",")
    dataWeekDay2
  }))
  # ---------------------------------------------------------------------- //



  # distribution of the number of rides by month of year (Jan through Dec)
  # ---------------------------------------------------------------------- //
  #   //  Chart   //
  output$monthOfYearPlot <- renderPlot({
    abrev <- c("Jan","Feb","Mar","April","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    newPSubset <- communityPickupSubsetReactive()
    newDSubset <- communityDropoffSubsetReactive()
    commTog <- destToggleReactive() 
    
    monthsColNames = c("Month","Count")
    if(commTog == 'Starting from')
    {
      dataHoursByMonth <- newPSubset %>%
        group_by(month(newPSubset$Date)) %>% dplyr::summarise(count=n())
      colnames(dataHoursByMonth) = monthsColNames 
    }
    else
    {
      dataHoursByMonth <- newDSubset %>%
        group_by(month(newDSubset$Date)) %>% dplyr::summarise(count=n())
      colnames(dataHoursByMonth) = monthsColNames 
    }
    
    ggplot(dataHoursByMonth, aes(x = Month, y = Count)) + geom_bar(stat = "identity", fill = "#ffad33", width = 0.8) +
      labs(x = "Month", y = "Total number of rides") + theme_bw() +
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = abrev) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
      coord_cartesian(expand = FALSE)
    
    #+ scale_x_discrete(labels=c(0:23))
    # scale_x_date(date_breaks = "day", date_labels = "%b. %d") + coord_cartesian(expand = FALSE)
  })
  # ---------------------------------------------------------------------- //
  #   //  Table   //
  output$monthOfYearTable <- DT::renderDataTable(DT::datatable({

    newPSubset <- communityPickupSubsetReactive()
    newDSubset <- communityDropoffSubsetReactive()
    commTog <- destToggleReactive() 
    
    monthsColNames = c("Month","Count")
    if(commTog == 'Starting from')
    {
      dataHoursByMonth <- newPSubset %>%
        group_by(month(newPSubset$Date)) %>% dplyr::summarise(count=n())
      colnames(dataHoursByMonth) = monthsColNames 
    }
    else
    {
      dataHoursByMonth <- newDSubset %>%
        group_by(month(newDSubset$Date)) %>% dplyr::summarise(count=n())
      colnames(dataHoursByMonth) = monthsColNames 
    }
    
    # dataHoursByDay$Hour <- format(dataDaysByYear$Date, "%b. %d")
    dataHoursByMonth$Count <- formatC(dataHoursByMonth$Count, big.mark = ",")
    dataHoursByMonth
  }))
  # ---------------------------------------------------------------------- //



  # distribution of the number of rides  to / from this community area by binned mileage
  # ---------------------------------------------------------------------- //
  #   //  Chart   //
  output$mileagePlot <- renderPlot({
    unitsTog <- unitUsed()
    newPSubset <- communityPickupSubsetReactive()
    newDSubset <- communityDropoffSubsetReactive()
    commTog <- destToggleReactive() 
    
    # number of rides by binned mileage (with an appropriate number of bins)
    breaks <- c(0.5,10,20,30,40,50,60,70,80,90,100)
    x <- c("10","20","30","40","50","60","70","80","90","100")
    y <- c("20","40","60","80","100","120","140","170")
    
    # group_tags <- cut(Taxi$`Trip Miles`, 
    #                   breaks=breaks, 
    #                   include.lowest=TRUE, 
    #                   right=FALSE)
    # group_tags <- as_tibble(group_tags)
    if(unitsTog == 'Miles')
    {
      if(commTog == 'Starting from')
      {
        group_tags <- cut(newPSubset$`Trip Miles`, 
                          breaks=breaks, 
                          include.lowest=TRUE, 
                          right=FALSE)
      }
      else
      {      
        group_tags <- cut(newDSubset$`Trip Miles`, 
                          breaks=breaks, 
                          include.lowest=TRUE, 
                          right=FALSE)
      }
      
      ggplot(data = as_tibble(group_tags), mapping = aes(x=value)) +
        geom_bar(fill="#ffad33",color="white") +
        # stat_count(geom="text", aes(label=sprintf("%.4f",..count../length(group_tags))), vjust=-0.5) + #i dont know what this line does
        labs(x='Miles Driven') +
        scale_x_discrete(labels = x) +
        # scale_y_log10(labels = trans_format("log10", math_format(10^.x))) + #testing shit out
        scale_y_log10(labels = scales::comma) + 
        theme_minimal()
    }
    else
    {
      breaks <- c(0.6,20,40,60,80,100,120,140,170)
      if(commTog == 'Starting from')
      {
        group_tags <- cut(newPSubset$`km`, 
                          breaks=breaks, 
                          include.lowest=TRUE, 
                          right=FALSE)
      }
      else
      {      
        group_tags <- cut(newDSubset$`km`, 
                          breaks=breaks, 
                          include.lowest=TRUE, 
                          right=FALSE)
      }
      ggplot(data = as_tibble(group_tags), mapping = aes(x=value)) +
        geom_bar(fill="#ffad33",color="white") +
        # stat_count(geom="text", aes(label=sprintf("%.4f",..count../length(group_tags))), vjust=-0.5) + #i dont know what this line does
        labs(x='Km Driven') +
        scale_x_discrete(labels = y) +
        # scale_y_log10(labels = trans_format("log10", math_format(10^.x))) + #testing shit out
        scale_y_log10(labels = scales::comma) + 
        theme_minimal()
    }

    #TODO fix y axis labels and ya
    # ggplot(data = as_tibble(group_tags), mapping = aes(x=value)) +
    #   geom_bar(fill="#ffad33",color="white") +
    #   # stat_count(geom="text", aes(label=sprintf("%.4f",..count../length(group_tags))), vjust=-0.5) + #i dont know what this line does
    #   labs(x='Miles Driven') +
    #   scale_x_discrete(labels = x) +
    #   # scale_y_log10(labels = trans_format("log10", math_format(10^.x))) + #testing shit out
    #   scale_y_log10(labels = scales::comma) + 
    #   theme_minimal()
  })
  # ---------------------------------------------------------------------- //
  #   //  Table   //
  output$mileageTable <- DT::renderDataTable(
    DT::datatable({
      unitsTog <- unitUsed()
      newPSubset <- communityPickupSubsetReactive()
      newDSubset <- communityDropoffSubsetReactive()
      commTog <- destToggleReactive() 
      
      # number of rides by binned mileage (with an appropriate number of bins)
      breaks <- c(0.5,10,20,30,40,50,60,70,80,90,100)
 
      # group_tags <- cut(Taxi$`Trip Miles`, 
      #                   breaks=breaks, 
      #                   include.lowest=TRUE, 
      #                   right=FALSE)
      # group_tags <- as_tibble(group_tags)
      if(unitsTog == 'Miles')
      {
        if(commTog == 'Starting from')
        {
          group_tags <- cut(newPSubset$`Trip Miles`, 
                            breaks=breaks, 
                            include.lowest=TRUE, 
                            right=FALSE)
        }
        else
        {      
          group_tags <- cut(newDSubset$`Trip Miles`, 
                            breaks=breaks, 
                            include.lowest=TRUE, 
                            right=FALSE)
        }
      }
      else
      {
        breaks <- c(0.6,20,40,60,80,100,120,140,170)
        if(commTog == 'Starting from')
        {
          group_tags <- cut(newPSubset$`km`, 
                            breaks=breaks, 
                            include.lowest=TRUE, 
                            right=FALSE)
        }
        else
        {      
          group_tags <- cut(newDSubset$`km`, 
                            breaks=breaks, 
                            include.lowest=TRUE, 
                            right=FALSE)
        }
      }
      # group_tags
    # # 
    # #   # dataHoursByDay$Hour <- format(dataDaysByYear$Date, "%b. %d")
    # #   # dataHoursByMonth$Count <- formatC(dataHoursByMonth$Count, big.mark = ",")
    # #   # dataHoursByMonth
      as_tibble(group_tags)
      # datatable(group_tags)
    # #   group_tags
      # group_tags$Count <- formatC(group_tags$Count, big.mark = ",")
      # group_tags
    })
    # unitUsed()
  )
  # ---------------------------------------------------------------------- //



  # distribution of the number of rides  to / from this community area by binned trip time
  # ---------------------------------------------------------------------- //
  #   //  Chart   //
  output$timePlot <- renderPlot({

    newPSubset <- communityPickupSubsetReactive()
    newDSubset <- communityDropoffSubsetReactive()
    commTog <- destToggleReactive() 
    
    monthsColNames = c("Month","Count")

    breaks <- c(60,1800,3600,5400,7200,9000,10800,12600,14400,16200,18000)
    lig <- c("15","30","45","60","75","90","105","120","135","150")
    if(commTog == 'Starting from')
    {
      period <- ms(newPSubset$`Trip Seconds`)
      group_tags2 <- cut(newPSubset$`Trip Seconds`, 
                         breaks=breaks, 
                         include.lowest=TRUE, 
                         right=FALSE)
    }
    else
    {
      period <- ms(newDSubset$`Trip Seconds`)
      group_tags2 <- cut(newDSubset$`Trip Seconds`, 
                         breaks=breaks, 
                         include.lowest=TRUE, 
                         right=FALSE)
    }

    group_tags2 <- as_tibble(group_tags2)
    
    
    
    ggplot(data = group_tags2, mapping = aes(x=value)) +
      geom_bar(fill="#ffad33",color="white") +
      # stat_count(geom="text", aes(label=sprintf("%.4f",..count../length(group_tags2))), vjust=-0.5) + #i dont know what this line does
      labs(x='Duration of Ride') +
      scale_x_discrete(labels = lig) +
      # scale_y_log10(labels = trans_format("log10", math_format(10^.x))) + #testing stuff with this line
      scale_y_log10(labels = scales::comma) + 
      theme_minimal()
  })
  # ---------------------------------------------------------------------- //
  #   //  Table   //
  output$timeTable <- DT::renderDataTable(DT::datatable({

    newPSubset <- communityPickupSubsetReactive()
    newDSubset <- communityDropoffSubsetReactive()
    commTog <- destToggleReactive() 
    
    monthsColNames = c("Month","Count")
    
    breaks <- c(60,1800,3600,5400,7200,9000,10800,12600,14400,16200,18000)
    
    if(commTog == 'Starting from')
    {
      period <- ms(newPSubset$`Trip Seconds`)
      group_tags2 <- cut(newPSubset$`Trip Seconds`, 
                         breaks=breaks, 
                         include.lowest=TRUE, 
                         right=FALSE)
    }
    else
    {
      period <- ms(newDSubset$`Trip Seconds`)
      group_tags2 <- cut(newDSubset$`Trip Seconds`, 
                         breaks=breaks, 
                         include.lowest=TRUE, 
                         right=FALSE)
    }
    
    group_tags2 <- as_tibble(group_tags2)
    group_tags2
  }))
  # ---------------------------------------------------------------------- //
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

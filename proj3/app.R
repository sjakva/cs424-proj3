# --------------------------------------------------------------
#
#   CS424 - Spring 2022
# Project 3 -- Big Yellow Taxi
#   Authors:  Jack Martin 
#     & Shoaib Jakvani
#
# --------------------------------------------------------------

# --------------------------------------------------------------
# # install.packages("shiny","tidyverse","shinydashboard","lubridate")
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


#   You will only need a subset of the 23 columns in the data 
#   3.  Trip Start Timestamp    (string -> date and time)
#   5.  Trip Seconds            (int)
#   6.  Trip Miles              (float)
#   9.  Pickup Community Area   (int)
#   10. Drop-off community Area (int)
#   17. Company                 (string)

# list of needed columns
col_names <- c("Trip Start Timestamp","Trip Seconds", "Trip Miles", "Pickup Community Area", "Dropoff Community Area", "Company")


# TaxiSelect <- fread("./Taxi_Trips_-_2019.tsv", 
#           colClasses = c("date" = "Date"), select = col_names,
#           nrows = 10000)
   
# setClass('yyyymmdd-hhmmss')
# setClass('mm/dd/yyyy hh:mm:ss')

#   https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy
# Go two directories out project directory for tsv file
TaxiSelect <- fread("../../Taxi_Trips_-_2019.tsv",
          # colClasses = c("date" = "Date"), 
          select = col_names,
          nrows = 10000)

# print(parse_date_time(TaxiSelect$'Trip Start Timestamp', "%m/%d/%Y %I:%M:%S Op"))


#   filter out the rest of the data to cut it to 300 mb
# 1) all trips less than 0.5 miles, 2) more than 100 miles, 
# 3) less than 60 seconds, 4) greater than 5 hours, 
# 5) all trips that either start/end outside of a Chicago community

# 1) all trips less than 0.5 miles
TaxiSelect <- TaxiSelect[!TaxiSelect$'Trip Miles' < 0.5]
# 2) more than 100 miles
TaxiSelect <- TaxiSelect[!TaxiSelect$'Trip Miles' > 100]
# 3) less than 60 seconds
TaxiSelect <- TaxiSelect[!TaxiSelect$'Trip Seconds' < 60]
# 4) greater than 5 hours == 18,000 seconds
TaxiSelect <- TaxiSelect[!TaxiSelect$'Trip Seconds' > 18000]
# 5) drop NA values (trips outside Chicago community)
TaxiSelect <- TaxiSelect[!is.na(TaxiSelect$`Pickup Community Area`)]
TaxiSelect <- TaxiSelect[!is.na(TaxiSelect$`Dropoff Community Area`)]
view(TaxiSelect)

# str(TaxiSelect)
# 
# # view(Taxi)
# 



# --------------------------------------------------------------

#------------------------------------
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Jack Martin and Shoaib Jakvani Project 2"),
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
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "home",
      box(
        title = "Entries For All Stations On Given Day",
        solidHeader = TRUE,
        status = "primary",
        width = 12, background = "navy"
      )
    ),
    tabItem(
      tabName = "about",
      box(
        title = "About",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        
        
        h2(
          "Jack Martin and Shoaib Jakvani created this app for Project 2 of UIC's CS 424 - Visual Analytics."
        ),
        p(
          "This data is from the Chicago Data Portal. More specifically, the \'CTA - Ridership - L Station Entries - Daily total\'.
                           The main components on why we are given this project is to teach us and give better familiarity with\n
                          both the R language and Shiny and Shiny dashboard and also using leaflet and its applications. We were tasked with analyzing and plotting Entries over\n
                          specific stations over 2001-2021 and plotting graphs from Project one for each station upon interacting with the leaflet map."
        )
      )
    )
    
  ))
  
)

#------------------------------------
# Define server logic
#   session as a param allows access to information and functionality relating to the session
server <- function(input, output, session) {

}

# Run the application 
shinyApp(ui = ui, server = server)

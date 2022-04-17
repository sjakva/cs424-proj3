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
   
#   https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy
# Go two directories out project directory for tsv file
TaxiSelect <- fread("../../Taxi_Trips_-_2019.tsv",
          # colClasses = c("date" = "Date"), 
          select = col_names,
          nrows = 10000)
# 
#
#
#   TODO: filter out the rest of the data to cut it to 300 mb
# 1) all trips less than 0.5 miles, 2) more than 100 miles, 
# 3) less than 60 seconds, 4) greater than 5 hours, 
# 5) all trips that either start/end outside of a Chicago community

#     also will only be using looking at trips down to a resolution 
#     of the starting hour rather than the 15 minute intervals in  
#     the data (?) idk man

# 5) drop NA values
TaxiSelect <- TaxiSelect[!is.na(TaxiSelect$`Pickup Community Area`)]
TaxiSelect <- TaxiSelect[!is.na(TaxiSelect$`Dropoff Community Area`)]
view(TaxiSelect)
# 
# # view(Taxi)
# 
# --------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

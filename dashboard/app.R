#App.R

## SETUP

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)

options(scipen=999)

# Data Import

meteorites <- read.csv('data/results/meteorites.csv')


#UI

ui <- fluidPage(
    
    titlePanel("Meteorite Data Analysis"),
    
    fluidRow(
        column(2, selectInput(dataset,
                              "Data",
                              c("All Meteorites"))),
        column(10,leafletOutput('map'))),
                   
    fluidRow(
          tabsetPanel(
              tabPanel("All Meteorites",tableOutput("meteoritetable"))))
               )


##SERVER
server <- function(input, output, session) {

    output$map <- renderLeaflet({
        map <- leaflet() %>%
            addTiles() %>%
            addMarkers(data = points())    
} 
    
    

## Run App

shinyApp(ui = ui, server = server)a
#App.R

## SETUP

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

options(scipen=999)

# Data Import

#All Meteorites
meteorites <- read.csv(file='data/results/meteorites_lc.csv')
meteorites <- meteorites[, c(2,3,7,8,6,10,9,18,13:16,12,5)]
colnames(meteorites) <- c('id','Name','Fell_or_Found','Year', 'Mass', 'Longitude', 'Latitude', 'Land_Cover', 'Chondrite', 'Type', 'Level_3', 'Level_4', 'Group','recclass')

#Prob Density Function Land Cover
pdf <- read.csv(file = 'data/results/pdf.csv')
pdfmelt<-melt(pdf,id="x_axis")

LC_names  <- list("Oceans/Water",
                  "Cultivated Land",
                  "Pasture/Land for Grazing",
                  "Tundra",
                  "Wooded Tundra",
                  "Boreal Forest",
                  "Cool Conifer Forest",
                  "Temperate Mixed Forest",
                  "Temperate Decidious Forest",
                  "Grassland/Steppe",
                  "Hot Desert",
                  "Savanna",
                  "Tropical Woodland",
                  "Tropical Forest")

LC_values  <- list("water_pdf",
                   "cultivated_pdf",
                   "pasture_pdf",
                   "tundra_pdf",
                   "wooded_tundra_pdf",
                   "boreal_forest_pdf",
                   "conifer_forest_pdf",
                   "temperate_mixed_forest_pdf",
                   "temperate_decidous_forest_pdf",
                   "grasssland_pdf",
                   "desert_pdf",
                   "savanna_pdf",
                   "tropical_wooded_pdf",
                   "tropical_forest_pdf")

#UI

ui <- fluidPage(
    
    titlePanel("Meteorite Data Analysis"),
    
    fluidRow(
          tabsetPanel(
              tabPanel("Meteorite Map",
                       fluidRow(
                           column(1,(hr)),
                           column(2,
                                  h3("Filters:"),
                                  sliderInput("yearslider",
                                                label = "Years",
                                                min = 860, 
                                                max = 2021,
                                                value = c(860,2021),
                                                sep=""),
                                  checkboxGroupInput(inputId = "fellfoundcheckbox",
                                                     label = "Fall or Find:",
                                                     inline = TRUE,
                                                     selected = c("Fell", "Found"),
                                                     choiceNames = c("Observed Falling","Found later"),
                                                     choiceValues = c("Fell", "Found")),
                                  checkboxGroupInput(inputId = "typecheckbox",
                                                     label = "Type of Meteorite:",
                                                     inline = TRUE,
                                                     selected = c("Stony","Stony-Iron","Iron"),
                                                     choiceNames = c("Stony","Stony-Iron","Iron"),
                                                     choiceValues= c("Stony","Stony-Iron","Iron")),
                                  checkboxGroupInput(inputId = "chondritecheckbox",
                                                     label = "Chondrite or Achondrite:",
                                                     inline = TRUE,
                                                     selected = c("Chondrite","Achondrite"),
                                                     choiceNames = c("Chondrite","Achondrite"),
                                                     choiceValues = c("Chondrite","Achondrite"))),
                           column(2,(hr)(hr)(hr),
                                  sliderInput("massslider",
                                                label = "Mass (grams)",
                                                min = 0, 
                                                max = 60000000,
                                                value = c(0,60000000)),
                                  checkboxGroupInput(inputId = "meteoriteLCcheckbox",
                                                     label = "Land Cover at impact site:",
                                                     selected = LC_values,
                                                     choiceNames = LC_names,
                                                     choiceValues = LC_values)),
                           column(7,leafletOutput("meteoritemap")))),
              tabPanel("Meteorite Table",dataTableOutput('meteorite_table')),
              tabPanel("Land Cover PDFs",
                       fluidRow(
                           column(1,h3("")),
                           column(2,checkboxGroupInput(inputId = "pdfcheckbox",
                                                       label = "Choose Land Covers to display",
                                                       selected = LC_values,
                                                       choiceNames = LC_names,
                                                       choiceValues = LC_values)),
                           column(9,plotOutput("pdfplot")))
                      )
          )
    )
)                  

##SERVER
server <- function(input, output, session) {
    
    output$pdfplot = renderPlot({
        pdf_plot <- subset(pdfmelt, variable %in% input$pdfcheckbox)
        p <- ggplot()
        p <- p + geom_line(data = pdf_plot, aes(x = x_axis, y = value, color=variable, group=variable))
        p <- p + ggtitle("Probability Density Function of Mass by Land Cover Type")
        p <- p + labs(x = "Meteorite Mass (g)", y= NULL)
        p
    })
    
         output$meteorite_table = renderDataTable(meteorites)    
}
                                        
    
## Run App

shinyApp(ui = ui, server = server)
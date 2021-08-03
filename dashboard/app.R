#App.R

## SETUP

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

library(dygraphs)
library(TTR)
library(rvest)
library(xts)

options(scipen=999)

#Data Import

#All Meteorites
meteorites <- read.csv(file='data/results/meteorites_lc.csv')
meteorites <- meteorites[, c(3,7,8,6,10,9,18,13:16,12,5)]
colnames(meteorites) <- c('Name','Fell_or_Found','Year', 'Mass', 'longitude', 'latitude', 'Land_Cover', 'Chondrite', 'Type', 'Level_3', 'Level_4', 'Group','recclass')

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

LC_names2  <- list("Boreal Forest",
                   "Cool Conifer Forest",
                   "Cultivated Land",
                   "Grassland/Steppe",
                   "Hot Desert",
                   "Oceans/Water",
                   "Pasture/Land used for Grazing",
                   "Savanna",
                   "Scrubland",
                   "Temperate Decidious Forest",
                   "Temperate Mixed Forest",
                   "Tropical Forest",
                   "Tropical Woodland",
                   "Tundra", 
                   "Warm Mixed Forest",
                   "Wooded Tundra")

#Timeline
falls <- subset(meteorites, Fell_or_Found == 'Fell')
worldpop <- read.csv("data/worldpop/worldpop.csv")

worldpop <- worldpop %>% rename(pop = World.Population.over.12000.years..various.sources..2019..,
                    year = Year)

avg_worldpop <- worldpop %>% group_by(year) %>% summarize(avg=mean(pop))
avg_worldpop <- as.data.frame(avg_worldpop)
avg_worldpop <- avg_worldpop[avg_worldpop$year >= 800 & avg_worldpop$year <= 2013,]
fall_count <- count(falls, vars = Year)
fall_count$decade <- floor(fall_count$vars/10)*10

falls_decade <- aggregate(fall_count$n, by=list(Category=fall_count$decade), FUN=sum)

for (i in 1:nrow(falls_decade)){
    x <- falls_decade$Category[i]
    w <- avg_worldpop$year

    closestVal = w[which.min(abs(w-x))]
    pop <- avg_worldpop[avg_worldpop$year == closestVal,]$avg

    falls_decade$pop[i] <- pop      
}

falls_decade <- falls_decade %>% rename(
    decade = Category,
    meteorite_falls = x,
    population = pop
    )

falls_decade$falls_per_1M <- falls_decade$meteorite_falls/(falls_decade$population/1000000)

falls_decade <- falls_decade[-c(44), ] 

#Falls and Finds Gridsquares
gsfellfound <- read.csv("data/results/gridsquarefellfound.csv")

#Correlation
meteorites_corr <- read.csv(file='data/results/all.csv')
meteorites_corr<-subset(meteorites_corr, select=-c(1,2,3,5,9,10,11,12,13,15))

corrlist <- list( "Mass"       = "mass",
                  "Year"       = "year",
                  "Latitude"   = "latitude",
                  "Longitude"  = "longitude",
                  "Land Cover Type" = "lc_sample")

#KModes
kmodes <- read.csv(file='data/results/meteorites_clustered.csv')


#UI

ui <- fluidPage(
    
    titlePanel("Meteorite Data Analysis"),
    
    fluidRow(
          tabsetPanel(
              tabPanel("Meteorite Data",
                       sidebarLayout(
                          sidebarPanel(
                              h3("Filters:"),
                              sliderInput("yearslider",
                                          label = "Year",
                                          min = 800, 
                                          max = 2021,
                                          step = 5,
                                          value = c(800,2021),
                                          sep=""),
                              sliderInput("massslider",
                                          label = "Mass (Kg)",
                                          min = 0, 
                                          max = 65000,
                                          value = c(0,65000)),
                              checkboxGroupInput(inputId = "fellfoundcheckbox",
                                                 label = "Fall or Find:",
                                                 inline = TRUE,
                                                 selected = c("Fell", "Found"),
                                                 choiceNames = c("Observed Falling","Found later"),
                                                 choiceValues = c("Fell", "Found")),
                              checkboxGroupInput(inputId = "typecheckbox",
                                                 label = "Type of Meteorite:",
                                                 inline = TRUE,
                                                 selected = c("Stony","Stony-Iron","Iron","-"),
                                                 choiceNames = c("Stony","Stony-Iron","Iron","Unknown"),
                                                 choiceValues= c("Stony","Stony-Iron","Iron","-")),
                              checkboxGroupInput(inputId = "chondritecheckbox",
                                                 label = "Chondrite or Achondrite:",
                                                 inline = TRUE,
                                                 selected = c("Chondrite","Achondrite","-"),
                                                 choiceNames = c("Chondrite","Achondrite","Unknown"),
                                                 choiceValues = c("Chondrite","Achondrite","-")),
                              checkboxGroupInput(inputId = "meteoriteLCcheckbox",
                                                 label = "Land Cover at impact site:",
                                                 selected = LC_names2,
                                                 choiceNames = LC_names2,
                                                 choiceValues = LC_names2)),
                       mainPanel(
                           leafletOutput("meteoritemap"),
                           dataTableOutput('meteorite_table')))),
              tabPanel("KModes Clustering",
                      sidebarLayout(
                          sidebarPanel(
                              radioButtons(
                                  inputId="KMradio",
                                  label="Choose cluster to display",
                                  selected = NULL,
                                  inline = TRUE,
                                  choices = c(0:6))),
                          mainPanel(leafletOutput("kmodesmap"),
                                    dataTableOutput("kmodestable")))),
              tabPanel("Land Cover PDFs",
                       sidebarLayout(
                           sidebarPanel(checkboxGroupInput(inputId = "pdfcheckbox",
                                                       label = "Choose Land Covers to display",
                                                       selected = LC_values,
                                                       choiceNames = LC_names,
                                                       choiceValues = LC_values)),
                           mainPanel(plotOutput("pdfplot")))),
               tabPanel("Falls over Time",
                        sidebarLayout(
                            sidebarPanel(
                                checkboxInput(inputId = "norm_check", label = "Normalize?", value = FALSE)),
                            mainPanel(dygraphOutput("timelineplot"),
                                      plotOutput("timelinecorrplot")))),
                tabPanel("Falls vs. Finds",
                        sidebarLayout(
                            sidebarPanel(
                                checkboxGroupInput(inputId = "gridsquares",
                                                 label = "Display 1° x 1° squares with:",
                                                 inline = TRUE,
                                                 selected = c("fall", "find", "both"),
                                                 choiceNames = c("Falls Only (Red)", "Finds Only(Blue)", "Both (Green)"),
                                                 choiceValues = c("fall", "find", "both"))),
                         mainPanel(leafletOutput("gsmap"),
                                  div("Finds Only: 1063, Falls Only: 712, Both: 181")))),
              tabPanel("Correlation Tester",
                       fluidRow(
                           column(6,wellPanel(
                               selectInput("corrvariable1",
                                           "Select variable #1:",
                                           c(list("Variables" = corrlist)),
                                           selected = "mass"))),
                 column(6,wellPanel(
                               selectInput("corrvariable2",
                                           "Select variable #2:",
                                           c(list("Variables" = corrlist)),
                                           selected = "lc_sample"))),
                 plotOutput("plot_chooseyourown"))),
               tabPanel("About",
                   div("Meteorite Data Analysis"),
                   div("Visualizations by Matthew Younce"),
                   div("PSDS Capstone"))

          )
    )
)

##SERVER
server <- function(input, output, session) {
    
    reac <- reactiveValues(yearslider = c(800,2021), 
                       fellfoundcheckbox = c("Fell", "Found"), 
                       typecheckbox = c("Stony","Stony-Iron","Iron","-"),
                       chondritecheckbox = c("Chondrite","Achondrite","-"),
                       massslider = c(0,65000000),
                       meteoriteLCcheckbox = LC_names2,
                       corrvariable1 = "mass",
                       corrvariable2 = "lc_sample")
    
    observeEvent (input$yearslider, {
        reac$yearslider = input$yearslider
    })
    observeEvent (input$fellfoundcheckbox, {
        reac$fellfoundcheckbox = input$fellfoundcheckbox
    })
    observeEvent (input$typecheckbox, {
        reac$typecheckbox = input$typecheckbox
    })
    observeEvent (input$chondritecheckbox, {
        reac$chondritecheckbox = input$chondritecheckbox
    })
    observeEvent (input$massslider, {
        reac$massslider = input$massslider *1000
    })
    observeEvent (input$meteoriteLCcheckbox, {
        reac$meteoriteLCcheckbox = input$meteoriteLCcheckbox
    })

    observeEvent (input$corrvariable1, {
        reac$corrvariable1 = input$corrvariable1
    })
    observeEvent (input$corrvariable2, {
        reac$corrvariable2 = input$corrvariable2
    })
    
    output$pdfplot = renderPlot({
        pdf_plot <- subset(pdfmelt, variable %in% input$pdfcheckbox)
        p <- ggplot()
        p <- p + geom_line(data = pdf_plot, aes(x = x_axis, y = value, color=variable, group=variable))
        p <- p + ggtitle("Probability Density Function of Mass by Land Cover Type")
        p <- p + labs(x = "Meteorite Mass (g)", y= NULL)
        p
    })
    
    output$meteorite_table = renderDataTable({
            map_meteorites <- meteorites
            map_meteorites <- subset(meteorites, Fell_or_Found %in% reac$fellfoundcheckbox)
            map_meteorites <- map_meteorites[map_meteorites$Year >= reac$yearslider[1] & map_meteorites$Year <= reac$yearslider[2],]
            map_meteorites <- map_meteorites[map_meteorites$Mass >= reac$massslider[1] & map_meteorites$Mass <= reac$massslider[2],]
            map_meteorites <- subset(map_meteorites, Type %in% reac$typecheckbox)
            map_meteorites <- subset(map_meteorites, Chondrite %in% reac$chondritecheckbox)
            map_meteorites <- subset(map_meteorites, Land_Cover %in% reac$meteoriteLCcheckbox)

            map_meteorites})

    output$meteoritemap = renderLeaflet({
            map_meteorites <- meteorites
            map_meteorites <- subset(meteorites, Fell_or_Found %in% reac$fellfoundcheckbox)
            map_meteorites <- map_meteorites[map_meteorites$Year >= reac$yearslider[1] & map_meteorites$Year <= reac$yearslider[2],]
            map_meteorites <- map_meteorites[map_meteorites$Mass >= reac$massslider[1] & map_meteorites$Mass <= reac$massslider[2],]
            map_meteorites <- subset(map_meteorites, Type %in% reac$typecheckbox)
            map_meteorites <- subset(map_meteorites, Chondrite %in% reac$chondritecheckbox)
            map_meteorites <- subset(map_meteorites, Land_Cover %in% reac$meteoriteLCcheckbox)

            map <- leaflet(map_meteorites) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
            map
    })
    
    output$timelineplot <- renderDygraph({

      if (input$norm_check == FALSE) {
          falls_decade_plot <- subset(falls_decade, select = -c(falls_per_1M))
       
          dygraph(falls_decade_plot, main="Meteorite Falls", height=400) %>%
          dyOptions(drawGapEdgePoints = TRUE,fillGraph = TRUE,drawGrid=FALSE) %>%
          dyLegend(width = 400) %>%
          dySeries("meteorite_falls", label = "Falls") %>%
          dyAxis("y", label = "Meteorite Falls")  %>%
          dySeries("population", axis = "y2", label = "Population") %>%
          dyAxis("y2", label = "Population", independentTicks =TRUE)  %>%
          dyRangeSelector(height = 75)

          } else {
          falls_decade_plot <- subset(falls_decade, select = -c(meteorite_falls))
          
          dygraph(falls_decade_plot, main="Meteorite Falls (Normalized per 1 Million Population)", height=400) %>%
          dyOptions(drawGapEdgePoints = TRUE,fillGraph = TRUE,drawGrid=FALSE) %>%
          dyLegend(width = 400) %>%
          dySeries("falls_per_1M", label = "Normalized Falls") %>%
          dyAxis("y", label = "Meteorite Falls")  %>%
          dySeries("population", axis = "y2", label = "Population") %>%
          dyAxis("y2", label = "Population", independentTicks =TRUE)  %>%
          dyRangeSelector(height = 75)

        }
        })

    output$timelinecorrplot = renderPlot({

          if (input$norm_check == FALSE) {
          falls_decade_plot <- subset(falls_decade, select = -c(falls_per_1M))          
          corrplot <- ggplot(falls_decade_plot, aes(x=population, y=meteorite_falls)) + geom_point()
          corrplot <- corrplot + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)
          
          corrplot
          } else {
          falls_decade_plot <- subset(falls_decade, select = -c(meteorite_falls))         



          corrplot <- ggplot(falls_decade_plot, aes(x=population, y=falls_per_1M)) + geom_point()
          corrplot <- corrplot + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)
          
          corrplot}
        })
    
        output$kmodesmap = renderLeaflet({

            kmodes_filtered <- subset(kmodes, Cluster == input$KMradio)

            map_km <- leaflet(kmodes_filtered) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
            map_km
        })
    
    
        output$kmodestable =  renderDataTable({
            kmodes_filtered <- subset(kmodes, Cluster == input$KMradio)
            kmodes_filtered
        })
    
    
    output$gsmap = renderLeaflet({
        gsplot <- subset(gsfellfound, fallorfind %in% input$gridsquares)
        gsplot <- gsplot[-c(1) ] 
        colnames(gsplot) <- c("latitude", "longitude", "score", "group")

        getColor <- function(gsplot) {
        sapply(gsplot$score, function(score) {
            if(score == -1) {
                "blue"
            } else if(score == 1) {
                "red"
            } else {
                "green"
            }})}
        icons <- awesomeIcons(
            icon = 'ios-close',
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(gsplot))
            
        map2 <- leaflet(gsplot) %>% addTiles() %>% addAwesomeMarkers(~longitude, ~latitude, icon=icons)
            map2
    })
    
    output$plot_chooseyourown = renderPlot({
        
        corr_final <- meteorites_corr %>% select(reac$corrvariable1, reac$corrvariable2)
        
        p <- ggplot(corr_final, aes(x=corr_final[[1]], y=corr_final[[2]]))
        p <- p + geom_point(color="firebrick")
        p <- p + geom_smooth(method = lm, se = FALSE, color="blue") 
        p <- p + ggtitle(paste0(reac$corrvariable1," vs. ", reac$corrvariable2,"\nCorrelation Coeff. = ",cor(corr_final)[1,2]))
        p <- p + labs(x = reac$corrvariable1, y= reac$corrvariable2)
        p
    
    })
    
}

## Run App

shinyApp(ui = ui, server = server)
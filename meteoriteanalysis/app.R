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
meteorites <- meteorites[, c(2:14)]
colnames(meteorites) <- c('Name',
                          'Fell_or_Found',
                          'Year',
                          'Mass',
                          'longitude',
                          'latitude',
                          'Land_Cover',
                          'Chondrite',
                          'Type',
                          'Level_3', 
                          'Level_4', 
                          'Group',
                          'recclass')

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
worldpop <- read.csv("data/worldpop/worldpop.csv")
worldpop <- worldpop %>% rename(pop = World.Population.over.12000.years..various.sources..2019..,
                                year = Year)
avg_worldpop <- worldpop %>% group_by(year) %>% summarize(avg=mean(pop))
avg_worldpop <- as.data.frame(avg_worldpop)
avg_worldpop <- avg_worldpop[avg_worldpop$year >= 800 & avg_worldpop$year <= 2013,]
falls <- subset(meteorites, Fell_or_Found == 'Fell')
fall_count <- count(falls, vars = Year)
fall_count$decade <- floor(fall_count$vars/10)*10
falls_decade <- aggregate(fall_count$n, by=list(Category=fall_count$decade), FUN=sum)
for (i in 1:nrow(falls_decade)){
    x <- falls_decade$Category[i]
    w <- avg_worldpop$year
    closestVal = w[which.min(abs(w-x))]
    pop <- avg_worldpop[avg_worldpop$year == closestVal,]$avg
    falls_decade$pop[i] <- pop}
falls_decade <- falls_decade %>% rename(
    decade = Category,
    meteorite_falls = x,
    population = pop)
falls_decade$falls_per_1M <- falls_decade$meteorite_falls/(falls_decade$population/1000000)
falls_decade <- falls_decade[-c(44), ] 

finds <- subset(meteorites, Fell_or_Found == 'Found')
found_count <- count(finds, vars = Year)
found_count$decade <- floor(found_count$vars/10)*10
found_decade <- aggregate(found_count$n, by=list(Category=found_count$decade), FUN=sum)
for (i in 1:nrow(found_decade)){
    x <- found_decade$Category[i]
    w <- avg_worldpop$year
    closestVal = w[which.min(abs(w-x))]
    pop <- avg_worldpop[avg_worldpop$year == closestVal,]$avg
    found_decade$pop[i] <- pop}
found_decade <- found_decade %>% rename(
    decade = Category,
    meteorite_finds = x,
    population = pop)
found_decade$finds_per_1M <- found_decade$meteorite_finds/(found_decade$population/1000000)
found_decade <- found_decade[-c(30), ]
fall_found_decade <- merge(falls_decade,found_decade, sort = TRUE, all=TRUE)
fall_found_decade2 <-subset(fall_found_decade, select=-c(2))
falls_pop <- na.omit(subset(fall_found_decade, select=-c(1,4,5,6)))
nfalls_pop <- na.omit(subset(fall_found_decade, select=-c(1,3,5,6)))
finds_pop <- na.omit(subset(fall_found_decade, select=-c(1,3,4,6)))
nfinds_pop <- na.omit(subset(fall_found_decade, select=-c(1,3,4,5)))

#Falls and Finds Gridsquares
gsfellfound <- read.csv("data/results/gridsquarefellfound.csv")

#Correlation
meteorites_corr <- read.csv(file='data/results/all.csv')
meteorites_corr <-subset(meteorites_corr, select=-c(1,2,3,5,9,10,11,12,13,15))
corrlist <- list( "Mass"= "mass",
                  "Year"= "year",
                  "Latitude"= "latitude",
                  "Longitude"= "longitude",
                  "Land Cover Type" = "lc_sample")

#KModes
kmodes <- read.csv(file='data/results/meteorites_clustered.csv')
kmodes <- subset(kmodes, select=-c(1))
kmodes <- as.data.frame(kmodes)

#Bonus
meteorites_full <- read.csv(file='data/results/full_dataset.csv')
meteorites_full <- meteorites_full[, c(2,4,7,8,6,10,9,5)]
colnames(meteorites_full) <- c('Name',
                               'Valid',
                               'Fell_or_Found',
                               'Year',
                               'Mass',
                               'longitude',
                               'latitude',
                               'recclass')


#UI
ui <- fluidPage(
    titlePanel("Meteorite Data Analysis"),
    fluidRow(
        tabsetPanel(
            tabPanel("About",
                     sidebarLayout(
                         sidebarPanel(
                             h1("Meteorite Data Analysis"),
                             h2("Visualizations by Matthew Younce"),
                             h3("PSDS Capstone")),
                         mainPanel(
                             p(),
                             p("This analysis has been performed on a dataset of meteorites from the Meteoritical Society via NASA."),
                             div("Here is a brief description of what you will find in each tab."),
                             hr(),
                             h4("Tab 1: About"),
                             div("This page."),
                             h4("Tab 2: Meteorite Data"),
                             div("This tab shows a map and table of the meteorite data.  One subtab shows more detail after data scrubbing was complete, while the second subtab is the complete dataset from NASA."),
                             h4("Tab 3: KModes Clustering"),
                             div("This is the results of an unsupervised clustering.  KModes clustering is similar to KMeans clustering, except it is for categorical data."),
                             h4("Tab 4: Land Cover PDFs"),
                             div("This shows how likely a particular mass of meteorite is for a given land cover type. If the peak is more to the left, its more likely a meteorite in that land cover type will be less massive."),
                             h4("Tab 5: Change over Time"),
                             div("This shows if there is any change in the rate of meteorites observed falling and finds made as population increases over time."),
                             h4("Tab 6: Falls vs. Finds"),
                             div("This shows whether a 1°x1° grid square has seen a fall event, a find, or both."),
                             h4("Tab 7: Correlation Tester"),
                             div("This allows a comparison of numeric variables in the dataset.")))),
            tabPanel("Meteorite Data",
                     tabsetPanel(
                         tabPanel("Scrubbed Data",
                             sidebarLayout(
                                 sidebarPanel(
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
                                     checkboxGroupInput(inputId = 'meteoriteLCcheckbox',
                                                        label = 'Land Cover at impact site:',
                                                        selected = LC_names2,
                                                        choices = LC_names2,
                                                        inline = TRUE)),
                                 mainPanel(
                                     leafletOutput("meteoritemap"),
                                     dataTableOutput('meteorite_table')))),
                    tabPanel("Full Dataset",
                             sidebarLayout(
                                 sidebarPanel(
                                     sliderInput("bonusyearslider",
                                                 label = "Year",
                                                 min = 800, 
                                                 max = 2021,
                                                 step = 5,
                                                 value = c(800,2021),
                                                 sep=""),
                                     sliderInput("bonusmassslider",
                                                 label = "Mass (Kg)",
                                                 min = 0, 
                                                 max = 65000,
                                                 value = c(0,65000)),
                                     checkboxGroupInput(inputId = "bonusvalid",
                                                        label = "Valid or Relict:",
                                                        inline = TRUE,
                                                        selected = c("Valid", "Relict"),
                                                        choiceNames = c("Valid","Relict"),
                                                        choiceValues = c("Valid", "Relict")),
                                     checkboxGroupInput(inputId = "bonusfellfoundcheckbox",
                                                        label = "Fall or Find:",
                                                        inline = TRUE,
                                                        selected = c("Fell", "Found"),
                                                        choiceNames = c("Observed Falling","Found later"),
                                                        choiceValues = c("Fell", "Found"))),
                                 mainPanel(
                                     leafletOutput("bonusmeteoritemap"),
                                     dataTableOutput('bonusmeteorite_table')))))),
            tabPanel("KModes Clustering",
                     sidebarLayout(
                         sidebarPanel(
                             radioButtons(
                                 inputId="KMradio",
                                 label="Choose cluster to display",
                                 selected = NULL,
                                 inline = TRUE,
                                 choices = c(0:6))),
                         mainPanel(
                             leafletOutput("kmodesmap"),
                             dataTableOutput("kmodestable")))),
            tabPanel("Land Cover PDFs",
                     sidebarLayout(
                         sidebarPanel(checkboxGroupInput(inputId = "pdfcheckbox",
                                                         label = "Choose Land Covers to display",
                                                         selected = LC_values,
                                                         choiceNames = LC_names,
                                                         choiceValues = LC_values)),
                         mainPanel(plotOutput("pdfplot")))),
            tabPanel("Changes over Time",
                     sidebarLayout(
                         sidebarPanel(
                             radioButtons(inputId="yaxes",
                                 label="Falls and finds on:",
                                 selected = "y",
                                 inline = TRUE,
                                 choiceNames = c("one axis", "two axes"),
                                 choiceValues = c("y","y2")),
                             checkboxInput(inputId = "norm_check",
                                           label = "Normalize?", 
                                           value = FALSE)),
                         mainPanel(dygraphOutput("timelineplot"),
                                   fluidRow(
                                       column(6,plotOutput("timelinecorrplot1")),
                                       column(6,plotOutput("timelinecorrplot2")))))),
            tabPanel("Falls vs. Finds",
                     sidebarLayout(
                         sidebarPanel(
                             checkboxGroupInput(inputId = "gridsquares",
                                                label = "Display 1° x 1° squares with:",
                                                inline = TRUE,
                                                selected = c("fall", "find", "both"),
                                                choiceNames = c("Falls Only", "Finds Only", "Both"),
                                                choiceValues = c("fall", "find", "both"))),
                         mainPanel(leafletOutput("gsmap"),
                                   plotOutput("gsplot"),
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
                                         selected = "year"))),
                         plotOutput("plot_chooseyourown"))))))

##SERVER
server <- function(input, output, session) {
    
    reac <- reactiveValues(yearslider = c(800,2021),
                           fellfoundcheckbox = c("Fell", "Found"), 
                           typecheckbox = c("Stony","Stony-Iron","Iron","-"),
                           chondritecheckbox = c("Chondrite","Achondrite","-"),
                           massslider = c(0,65000000),
                           meteoriteLCcheckbox = LC_names2,
                           corrvariable1 = "mass",
                           corrvariable2 = "year",
                           bonusyearslider = c(800,2021),
                           bonusmassslider = c(0,65000),
                           bonusfellfoundcheckbox = c("Fell", "Found"),
                           bonusvalid = c("Valid","Relict"))
    
    observeEvent (input$yearslider, {reac$yearslider = input$yearslider})
    observeEvent (input$fellfoundcheckbox, {reac$fellfoundcheckbox = input$fellfoundcheckbox})
    observeEvent (input$typecheckbox, {reac$typecheckbox = input$typecheckbox})
    observeEvent (input$chondritecheckbox, {reac$chondritecheckbox = input$chondritecheckbox})
    observeEvent (input$massslider, {reac$massslider = input$massslider *1000})
    observeEvent (input$meteoriteLCcheckbox, {reac$meteoriteLCcheckbox = input$meteoriteLCcheckbox})
    observeEvent (input$corrvariable1, {reac$corrvariable1 = input$corrvariable1})
    observeEvent (input$corrvariable2, {reac$corrvariable2 = input$corrvariable2})
    observeEvent (input$bonusyearslider, {reac$bonusyearslider = input$bonusyearslider})
    observeEvent (input$bonusmassslider, {reac$bonusmassslider = input$bonusmassslider *1000})
    observeEvent (input$bonusfellfoundcheckbox, {reac$bonusfellfoundcheckbox = input$bonusfellfoundcheckbox})
    observeEvent (input$bonusvalid, {reac$bonusvalid = input$bonusvalid})
    
    output$pdfplot = renderPlot({
        pdf_plot <- subset(pdfmelt, variable %in% input$pdfcheckbox)
        p <- ggplot()
        p <- p + geom_line(data = pdf_plot, aes(x = x_axis, y = value, color=variable, group=variable))
        p <- p + ggtitle("Probability Density Function of Mass by Land Cover Type")
        p <- p + labs(x = "Meteorite Mass (g)", y= NULL)
        p})
    
    output$meteorite_table = renderDataTable({
        map_meteorites <- meteorites
        map_meteorites <- subset(map_meteorites, Fell_or_Found %in% reac$fellfoundcheckbox)
        map_meteorites <- map_meteorites[map_meteorites$Year >= reac$yearslider[1] & map_meteorites$Year <= reac$yearslider[2],]
        map_meteorites <- map_meteorites[map_meteorites$Mass >= reac$massslider[1] & map_meteorites$Mass <= reac$massslider[2],]
        map_meteorites <- subset(map_meteorites, Type %in% reac$typecheckbox)
        map_meteorites <- subset(map_meteorites, Chondrite %in% reac$chondritecheckbox)
        map_meteorites <- subset(map_meteorites, Land_Cover %in% reac$meteoriteLCcheckbox)
        map_meteorites})

    output$meteoritemap = renderLeaflet({
        map_meteorites <- meteorites
        map_meteorites <- subset(map_meteorites, Fell_or_Found %in% reac$fellfoundcheckbox)
        map_meteorites <- map_meteorites[map_meteorites$Year >= reac$yearslider[1] & map_meteorites$Year <= reac$yearslider[2],]
        map_meteorites <- map_meteorites[map_meteorites$Mass >= reac$massslider[1] & map_meteorites$Mass <= reac$massslider[2],]
        map_meteorites <- subset(map_meteorites, Type %in% reac$typecheckbox)
        map_meteorites <- subset(map_meteorites, Chondrite %in% reac$chondritecheckbox)
        map_meteorites <- subset(map_meteorites, Land_Cover %in% reac$meteoriteLCcheckbox)
        map <- leaflet(map_meteorites) %>% addTiles() %>% addMarkers(label = ~Name, clusterOptions = markerClusterOptions())
        map})
    
    output$bonusmeteorite_table = renderDataTable({
        map_meteorites <- meteorites_full
        map_meteorites <- map_meteorites[map_meteorites$Year >= reac$bonusyearslider[1] & map_meteorites$Year <= reac$bonusyearslider[2],]
        map_meteorites <- map_meteorites[map_meteorites$Mass >= reac$bonusmassslider[1] & map_meteorites$Mass <= reac$bonusmassslider[2],]
        map_meteorites <- subset(map_meteorites, Fell_or_Found %in% reac$bonusfellfoundcheckbox)
        map_meteorites <- subset(map_meteorites, Valid %in% reac$bonusvalid)
        map_meteorites})

    output$bonusmeteoritemap = renderLeaflet({
        map_meteorites <- meteorites_full
        map_meteorites <- map_meteorites[map_meteorites$Year >= reac$bonusyearslider[1] & map_meteorites$Year <= reac$bonusyearslider[2],]
        map_meteorites <- map_meteorites[map_meteorites$Mass >= reac$bonusmassslider[1] & map_meteorites$Mass <= reac$bonusmassslider[2],]
        map_meteorites <- subset(map_meteorites, Fell_or_Found %in% reac$bonusfellfoundcheckbox)
        map_meteorites <- subset(map_meteorites, Valid %in% reac$bonusvalid)
        map <- leaflet(map_meteorites) %>% addTiles() %>% addMarkers(label = ~Name, clusterOptions = markerClusterOptions())
        map})
    
    output$timelineplot <- renderDygraph({
        if (input$norm_check == FALSE) {
            fall_found_decade2 <- subset(fall_found_decade2, select = -c(falls_per_1M,finds_per_1M))
            dygraph(fall_found_decade2, main="Meteorites", height=400) %>%
            dyOptions(drawGapEdgePoints = TRUE,fillGraph = TRUE,drawGrid=FALSE) %>%
            dyLegend(width = 400) %>%
            dySeries("meteorite_falls", label = "Falls") %>%
            dySeries("meteorite_finds", label = "Finds", axis = input$yaxes) %>%
            dyAxis("y")  %>%
            dyAxis("y2", independentTicks=TRUE)  %>%
            dyRangeSelector(height = 75)
        } else {
            fall_found_decade2 <- subset(fall_found_decade2, select = -c(meteorite_falls, meteorite_finds))
            dygraph(fall_found_decade2, main="Meteorites (Normalized per 1 Million Population)", height=400) %>%
            dyOptions(drawGapEdgePoints = TRUE,fillGraph = TRUE,drawGrid=FALSE) %>%
            dyLegend(width = 400) %>%
            dySeries("falls_per_1M", label = "Normalized Falls") %>%
            dyAxis("y")  %>%
            dySeries("finds_per_1M", label = "Normalized Finds", axis = input$yaxes) %>%
            dyAxis("y2", independentTicks=TRUE)  %>%
            dyRangeSelector(height = 75)}})


    output$timelinecorrplot1 = renderPlot({
        if (input$norm_check == FALSE) {
            corrplot <- ggplot(falls_pop, aes(x=population, y=meteorite_falls)) + geom_point()
            corrplot <- corrplot + geom_smooth(method=lm, se=FALSE)
            corrplot <- corrplot + ggtitle(paste0("Falls vs. Population\nCorrelation Coeff. = ",cor(falls_pop)[1,2]))
            corrplot
        } else {
            corrplot <- ggplot(nfalls_pop, aes(x=population, y=falls_per_1M)) + geom_point()
            corrplot <- corrplot + geom_smooth(method=lm, se=FALSE)
            corrplot <- corrplot + ggtitle(paste0("Normalized Falls vs. Population\nCorrelation Coeff. = ",cor(nfalls_pop)[1,2]))
            corrplot}})
    
    output$timelinecorrplot2 = renderPlot({
        if (input$norm_check == FALSE) {
            corrplot2 <- ggplot(finds_pop, aes(x=population, y=meteorite_finds)) + geom_point()
            corrplot2 <- corrplot2 + geom_smooth(method=lm, se=FALSE)
            corrplot2 <- corrplot2 + ggtitle(paste0("Finds vs. Population\nCorrelation Coeff. = ",cor(finds_pop)[1,2]))
            corrplot2
        } else {
            corrplot2 <- ggplot(nfinds_pop, aes(x=population, y=finds_per_1M)) + geom_point()
            corrplot2 <- corrplot2 + geom_smooth(method=lm, se=FALSE)
            corrplot2 <- corrplot2 + ggtitle(paste0("Normalized Finds vs. Population\nCorrelation Coeff. = ",cor(nfinds_pop)[1,2]))
            corrplot2}})
    
    output$kmodesmap = renderLeaflet({
        kmodes_filtered <- subset(kmodes, Cluster == input$KMradio)
        map_km <- leaflet(kmodes_filtered) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
        map_km})
    
    output$kmodestable =  renderDataTable({
        kmodes_filtered <- subset(kmodes, Cluster == input$KMradio)
        kmodes_filtered})
    
    output$gsplot = renderPlot({
        gsplot <- subset(gsfellfound, fallorfind %in% input$gridsquares)
        gsplot <- gsplot[-c(1) ] 
        colnames(gsplot) <- c("latitude", "longitude", "score", "group")
        p <- ggplot()
        p <- p + geom_point(data = gsplot, aes(x = longitude, y = latitude, color=group, group=group))
        p <- p + labs(x = NULL, y= NULL)
        p})
            
    output$gsmap = renderLeaflet({
        gsplot <- subset(gsfellfound, fallorfind %in% input$gridsquares)
        gsplot <- gsplot[-c(1) ] 
        colnames(gsplot) <- c("latitude", "longitude", "score", "group")
        getColor <- function(gsplot) {
            sapply(gsplot$score, function(score) {
                if(score == -1) {
                    "purple"
                } else if(score == 1) {
                    "orange"
                } else {
                    "yellow"}})}
        icons <- awesomeIcons(
            icon = 'ios-close',
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(gsplot))
        map <- leaflet(gsplot) %>% addTiles() %>% addAwesomeMarkers(~longitude, ~latitude, icon=icons, label = ~group)
        map})
    
    output$plot_chooseyourown = renderPlot({
        corr_final <- meteorites_corr %>% select(reac$corrvariable1, reac$corrvariable2)
        p <- ggplot(corr_final, aes(x=corr_final[[1]], y=corr_final[[2]]))
        p <- p + geom_point(color="firebrick")
        p <- p + geom_smooth(method = lm, se = FALSE, color="blue") 
        p <- p + ggtitle(paste0(reac$corrvariable1," vs. ", reac$corrvariable2,"\nCorrelation Coeff. = ",cor(corr_final)[1,2]))
        p <- p + labs(x = reac$corrvariable1, y= reac$corrvariable2)
        p})}

## Run App
shinyApp(ui = ui, server = server)
# Duwamish River Valley Park Finder Tool

##############
# TO DO LIST #
##############
# address reactive park features function [104] - is it necessary?
# make pop ups that pull park locations
# write reactive markers when park features are selected
# make feature selection menu less ugly

# load packages ####
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)

# static plotting tests ####

# read in data
drv.parks <- read.csv("SeattleParksData_Features.csv")

# ensure coordinates are numeric and remove NAs
drv.parks <- drv.parks %>%
  mutate_at(vars(xpos, ypos), funs(as.numeric)) %>%
  filter(!is.na(xpos))

# CREATE THE MAP!
drv.parkmap <- leaflet(
  options = leafletOptions(minZoom= 12, maxZoom = 20), drv.parks) %>%
  addTiles() %>%
  fitBounds(~min(xpos), ~min(ypos), ~max(xpos), ~max(ypos)) %>%
  addCircleMarkers(lng = ~xpos,
                   lat = ~ypos)
# call map
drv.parkmap


##################
# User Interface #
##################

# !!! make points on map click-able to view the park's address


#generic line initiating the UI
ui <- shinyUI(fluidPage(
  
  #Add a title
  titlePanel("Duwamish River Valley Park Finder"),
  
  # call leaflet map
  leafletOutput(outputId = "map", width = "100%", height = "100%",),
  
  #add map as absolute panel
  absolutePanel(top = 10, right = 10,
                checkboxGroupInput(inputId = "features", label = "Select park features", 
                            choices = unique(drv.parks$feature_desc)),
),
  
  #This creates a layout with a left sidebar and main section
  #sidebarLayout(
    
    #beginning of sidebar section
    #usually includes inputs
    #sidebarPanel(),
    
    #beginning of main section
    #mainPanel()
  #)
  
  #Close the UI definition
))


##########
# SERVER #
##########

#generic line initiating the SERVER 

server <- shinyServer(function(input, output) {
  
  #########################
  # Data load and cleanup #
  #########################
 
   # read in data
  drv.parks <- read.csv("SeattleParksData_Features.csv")
  
  # ensure coordinates are numeric and remove NAs
  drv.parks <- drv.parks %>%
    mutate_at(vars(xpos, ypos), funs(as.numeric)) %>%
    filter(!is.na(xpos))
  
  #############
  # Reactives #
  #############
  
  # Reactive expression for the data subsetted to what the user selected
  # uncertain if [1] is functional/necessary
  filteredData <- reactive({
    drv.parks[drv.parks$feature_desc == input$features[1],]
  }) 
  
  # !!!!!!!!!!!!!
  # This reactive expression represents the feature select function,
  # which changes as the user makes selections in UI.
  feat <- reactive({
    colorNumeric(input$features, drv.parks$feature_desc)
  })
  
  #draw base map
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    drv.parkmap <- leaflet(drv.parks) %>%
      addTiles() %>%
      fitBounds(~min(xpos), ~min(ypos), ~max(xpos), ~max(ypos))
  })
  
 #define moving parts of map
   # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    feat <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # define any reactive elements of the app
  # Check boxes
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
    
    # Get the data set with the appropriate name
    dat <- get(input$dataset)
    colnames <- names(dat)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns", "Choose columns", 
                       choices  = colnames,
                       selected = colnames)
  })
  
  #Close the server definition
})


##############
# Launch App #
##############

#generic line that launches the app
shinyApp(ui = ui, server = server)




##################
# reference code #
##################
library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                            value = range(quakes$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)


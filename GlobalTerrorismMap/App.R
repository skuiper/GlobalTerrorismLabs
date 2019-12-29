library(shiny)
library(shinydashboard)
library(leaflet)
library(maptools)
library(magrittr)

source('initialize.R')
print("Finished initializing")

#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE USER INTERFACE                                                  #
#--------------------------------------------------------------------------------------------------------------------#

sidebar <- dashboardSidebar(
  selectInput('region', "Region", choices = c(
    "World" = "World",
    'Middle East & North Africa' = "MidEast",
    "North America" = "NorthAm",
    "South Asia" = "SouthAs",
    "Sub-Saharan Africa" = "SubSahr",
    "Europe & Central Asia" = "Eurasia",
    "Latin America & Caribbean" = "LatinAm",
    "East Asia & Pacific" = "AsiaPac"
  )),
  sliderInput("year",
              "Year",
              min = 1970,
              max = 2017,
              value = 1970,
              step = 1,
              sep = "",
              animate = animationOptions(interval = 1000)),
  htmlOutput('estimatesText'),
  checkboxInput("estimates",  "Include estimated locations", value = FALSE),
  htmlOutput('sidebarText')
)

body <- dashboardBody(
  tags$head(tags$style(HTML( # Additional style parameters
    '
                              html, body {
                                   font-size: 1em;
                                   width: 100%;
                                   height: 100%;
                              }
                              td {
                                   padding-left: 0.5vw;
                                   padding-right: 0.5vw;
                                   vertical-align: middle;
                              }
                              small {
                                   font-size: 0.85em;
                                   color: #444;
                                   font-weight: normal;
                                   font-style: italic;
                              }
                              p.cell {
                                   line-height: 70%;
                              }
                              p.numbercell{
                                   left: 0px;
                              }
                              .leaflet-popup{
                              }
                              section.sidebar .shiny-input-container {
                                padding: 0px 15px 0px 12px;
                              }
                              #info {
                                   font-size: 1.2em;
                                   max-width: 40vw;
                              }
                              .legend {
                                   white-space: nowrap;
                              }
                             '))),
  leafletOutput("Map", width='100%', height='60em')
)

ui <- dashboardPage(dashboardHeader(title="Exploring Terrorism"),
                    sidebar, body)

#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE SERVER LOGIC                                                    #
#--------------------------------------------------------------------------------------------------------------------#

server <- function(input, output) {
  #----------------------------------------------------------------------------------------------------------------#
  #                                                 CREATE MAP                                                     #
  #----------------------------------------------------------------------------------------------------------------#
  updateMarkers <- function(){
    if (input$region == "World"){
      regionEvents <-terrorismData[terrorismData$Year == input$year,]
      mapshapes <- worldshapes
    }else{
      regionName <- regionInfo[input$region, ]$Name
      regionEvents <- terrorismData[terrorismData$Year == input$year & terrorismData$MapRegion == regionName,]
      mapshapes <- worldshapes[worldshapes$region_wb == regionName,]
    }
    
    if (input$estimates == FALSE){
      regionEvents <- filter(regionEvents, MissLocation == 0)
    }
    
    # Checks if there are any events for that year and breaks if there aren't any
    if(nrow(regionEvents) == 0) return()
    
    
    
    #   Renders markers if there are
    leafletProxy('Map') %>% clearMarkers()
    leafletProxy('Map') %>% addCircleMarkers(
      lng=regionEvents$Longitude, lat=regionEvents$Latitude,
      color = "red", opacity = .2, weight = 7,
      fillColor = "yellow", fillOpacity = .7,
      radius = regionEvents$Severity,
      popup = regionEvents$info
    )
    
  }
  
  updateRegion <- function(){
    if (input$region == "World"){
      leafletProxy('Map') %>%
        clearShapes %>%
        clearControls %>%
        clearMarkers %>%
        setView(35, 40, zoom = 2)
    }else{
      region <- regionInfo[input$region,]
      regionName <- regionInfo[input$region, ]$Name
      mapshapes <- worldshapes[worldshapes$region_wb == regionName,]
      leafletProxy('Map') %>%
        clearShapes %>%
        clearControls %>%
        clearMarkers %>%
        setView(region$X, region$Y, zoom = region$Z) %>%
        addPolygons(
          data = mapshapes, layerId = ~admin,
          weight = 2, fillColor = "#12AFFF",
          color = "black", fillOpacity = 0.3)
    }
  }
  
  # Create blank map
  output$Map <- renderLeaflet({
    leaflet()  %>%
      addProviderTiles("CartoDB.Positron")
  })
  
  # Update map when new region is selected
  observeEvent({input$region}, {
    updateRegion()
    updateMarkers()
  })
  
  # Update map when new year is selected
  observeEvent({input$year}, {
    updateMarkers()
  })
  
  # Update map when estimates is selected
  observeEvent({input$estimates}, {
    updateMarkers()
  })
  
  #----------------------------------------------------------------------------------------------------------------#
  #                                             DEFINE Info Box                                                    #
  #----------------------------------------------------------------------------------------------------------------#
  output$estimatesText <- renderText({"<div style='padding:1em'> <b>Note:</b> The locations of some incidents had to be estimated 
  with the centroid of the country. <a href='https://worldmap.harvard.edu/data/geonode:country_centroids_az8'>Harvard Worldmap Country Centroids</a>.
  To include the estimated locations, click the box below. </div>
    "})
  # Text to be displayed in the side bar
  output$sidebarText <- renderText({"
          <div style='padding:1em'>
               Click on an incident for more details, or
               search the <a href='http://www.start.umd.edu/gtd/search/BrowseBy.aspx'>
               Global Terrorism database</a>.
          </div>
          <div style='padding:1em'>
               More resources for instructors: <a href='http://web.grinnell.edu/individuals/kuipers/stat2labs/GlobalTerrorism.html'>
               Stat2Labs</a>.
</div>
          "})
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

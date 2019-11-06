library(shiny)
library(shinydashboard)
library(leaflet)
library(maptools)
library(magrittr)

require('initialize.R')
print("Finished initializing")

#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE USER INTERFACE                                                  #
#--------------------------------------------------------------------------------------------------------------------#

ui <- dashboardPage(
  skin="blue",
  dashboardHeader(title="Exploring Terrorism"),
  
  dashboardSidebar(
    selectInput('region', "Region", choices = c(
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
                max = 2013,
                value = 1970,
                step = 1,
                sep = "",
                animate = animationOptions(interval = 1000)),
    
    htmlOutput('sidebarText')
  ),
  
  dashboardBody(
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
  
)

#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE SERVER LOGIC                                                    #
#--------------------------------------------------------------------------------------------------------------------#

server <- function(input, output) {
  
  # Checks if data is already loaded, and initializes data with a progress bar if not
  if(!(exists('regionData') & exists('regionInfo'))) source("initialize.R")
  
  ##########################################################################
  ############################ Creating the Maps ###########################
  ##########################################################################
  
  #    Main functions
  
  updateMarkers <- function(){
    
    regionName <- regionInfo[input$region, ]$Name
    
    regionEvents <- terrorismData[terrorismData$iyear == input$year &
                                    terrorismData$region2 == regionName,]
    
    mapshapes <- worldshapes[worldshapes$region_wb == regionName,]
    
    mapdata <- regionData[regionData$Year == input$year &
                            regionData$Country %in% mapshapes$admin,]
    
    #         Checks if there are any events for that year and breaks if there aren't any
    if(nrow(regionEvents) == 0) return()
    
    #         Renders markers if there are
    leafletProxy('Map') %>% clearMarkers()
    leafletProxy('Map') %>% addCircleMarkers(
      lng=regionEvents$longitude, lat=regionEvents$latitude,
      color = "red", opacity = .2, weight = 7,
      fillColor = "yellow", fillOpacity = .7,
      radius = regionEvents$severity,
      popup = regionEvents$info
    )
    
  }
  updateRegion <- function(){
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
  
  #    Create blank map
  output$Map <- renderLeaflet({
    leaflet()  %>%
      addProviderTiles("CartoDB.Positron")
    #                addTiles('http://services.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}.png')
  })
  
  #    Updates polygons and markers whenever year changes
  observeEvent({input$year}, {
    updateMarkers()
  })
  
  #    Reloads everything when new region selected
  observeEvent({input$region}, {
    updateRegion()
    updateMarkers()
  })
  
  
  
  ###############################################################################
  ##################### Creating the Information Box ############################
  ###############################################################################
  
  
  
  # Text to be displayed in the side bar
  output$sidebarText <- renderText({"
          <div style='padding:1em'>
               Click on an incident for more details, or
               search the <a href='http://www.start.umd.edu/gtd/search/BrowseBy.aspx'>
               Global Terrorism database</a>.
          </div>
          <div style='padding:1em'>
               <b>Note:</b> The locations of some incidents had to be estimated
               with the <a href='http://www.geonames.org/'>GeoNames database</a>.
               As a result, a few markers may appear in weird places!
          </div>
          <div style='padding:1em'>
               More resources for instructors: <a href='http://web.grinnell.edu/individuals/kuipers/stat2labs/GlobalTerrorism.html'>
               Stat2Labs</a>.
</div>
          "})
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
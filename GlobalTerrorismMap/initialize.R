library(shiny)
library(shinydashboard)
library(leaflet)
library(maptools)
library(magrittr)
library(sf)
library(maps)
library(dplyr)

worldshapes <-  sf::st_read('worldshapes/worldshapes.shp')
print("Load worldshapes")

terrorismData <- read.csv("terrorismData.csv")

terrorismData$Longitude <- jitter(terrorismData$Longitude, factor = 0.0000000001)
terrorismData$Latitude <- jitter(as.numeric(terrorismData$Latitude),factor = 0.0000000001)
options(scipen=999) # turn off scientific notification
terrorismData$info <- paste0("<b>Event ID: </b>", terrorismData$"ID", "<br/><b>Date: </b>",terrorismData$"Month","/", terrorismData$"Day", "/", terrorismData$"Year",
                            "<br/><b>Location: </b>", terrorismData$"City", ", ", terrorismData$"Country", 
                            ifelse(terrorismData$"MissLocation" == 1, paste0("<br/> Location on the map is the centroid of ", terrorismData$"Country"), ""),
                            "<br/><b>Group name: </b>", terrorismData$"GroupName",
                            "<br/><b>Target: </b>", terrorismData$"Target", "<br/><b>Attack type: </b>", terrorismData$"Attack",
                            "<br/><b>Weapon type: </b>", terrorismData$"Weapon", "<br/><b>Deaths: </b>", terrorismData$"nKill",
                            "<br/><b>Wounded: </b>", terrorismData$"nWound", 
                            "</br><a href='http://www.start.umd.edu/gtd/search/IncidentSummary.aspx?gtdid=", terrorismData$ID, "'>Database entry for this event.</a>")
print("Load terrorism data")

regionInfo <- {data.frame('Name' = c('MidEast' = 'Middle East & North Africa',
                                     'NorthAm' = 'North America',
                                     'SouthAs' = 'South Asia',
                                     'SubSahr' = 'Sub-Saharan Africa',
                                     'Eurasia' = 'Europe & Central Asia',
                                     'LatinAm' = 'Latin America & Caribbean',
                                     'AsiaPac' = 'East Asia & Pacific'),
                          'Map' = c('MidEast' = 'MidEastMap',
                                    'NorthAm' = 'NorthAmMap',
                                    'SouthAs' = 'SouthAsMap',
                                    'SubSahr' = 'SubSahrMap',
                                    'Eurasia' = 'EurasiaMap',
                                    'LatinAm' = 'LatinAmMap',
                                    'AsiaPac' = 'AsiaPacMap'),
                          'X' = c('MidEast' = 23,
                                  'NorthAm' = -95,
                                  'SouthAs' = 78,
                                  'SubSahr' = 17,
                                  'Eurasia' = 35,
                                  'LatinAm' = -85,
                                  'AsiaPac' = 130),
                          'Y' = c('MidEast' = 28,
                                  'NorthAm' = 38,
                                  'SouthAs' = 24,
                                  'SubSahr' = -5,
                                  'Eurasia' = 50,
                                  'LatinAm' = -10,
                                  'AsiaPac' = 5),
                          'Z' = c('MidEast' = 4,
                                  'NorthAm' = 4,
                                  'SouthAs' = 5,
                                  'SubSahr' = 4,
                                  'Eurasia' = 3,
                                  'LatinAm' = 3,
                                  'AsiaPac' = 3))}

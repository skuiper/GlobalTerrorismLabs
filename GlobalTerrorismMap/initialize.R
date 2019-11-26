library(shiny)
library(shinydashboard)
library(leaflet)
library(maptools)
library(magrittr)
library(sf)
library(maps)
library(dplyr)

worldshapes <-  sf::st_read('~/GTD/worldshapes/worldshapes.shp')
print("Load worldshapes")

terrorismData <- read.csv("C:/Users/stella/Documents/GTD/Map/terrorismData.csv")
terrorismData <-filter(terrorismData, !(is.na(terrorismData$Latitude) | is.na(terrorismData$Longitude)))
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

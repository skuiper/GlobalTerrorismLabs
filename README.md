# Global Terrorism Labs
Materials were created by Prof. Shonda Kuiper, Seoyeon (Stella) Lee (Grinnell College, DASIL)

This github repository contains data sets and R code to better understand trends in global terrorism. (1970 - 2017) [Global Terrorism Lab](https://github.com/skuiper/GlobalTerrorismLabs2015) was previously developed by Zachary Segall, Ying Long, and Krit Petrachaianan for 2015 summer research project. Data Visualization lab related to this dataset is presented in following [link](http://web.grinnell.edu/individuals/kuipers/stat2labs/GlobalTerrorism.html)

Deployed GTD Map shiny app could be found in following [link](http://shiny.grinnell.edu/GlobalTerrorismMap/)

Deployed GTD Plot shiny app could be found in following [link](http://shiny.grinnell.edu/GlobalTerrorismPlots/)

## Table of Contents

1. Data Source
2. Required Libraries
3. Folders in Github Repository
4. GTD Map App R Code Flowchart
5. GTD Plot App R Code Flowchart
6. Data Description
7. R Code Specific Files

## Data Source

* The Global Terrorism database by National Consortium for the Study of Terrorism and Responses to Terrorism [[here](https://www.start.umd.edu/data-tools/global-terrorism-database-gtd)] 
* WorldBank Database [here]
* Wikipedia ISO Country Code [here]
* Country Centroid Data from  [[here](https://worldmap.harvard.edu/data/geonode:country_centroids_az8)]

## Required Libraries

1. Required libraries for Data Cleaning (`DataProcessing` folder)

   ```R
   # Load Data
   library ("readxl")
   # Web Scraping
   library(rvest)
   library(stringr)
   library(readr)
   # Data manipulation
   library(dplyr)
   library(tidyr)
   ```

2. Required libraries for GTD Map app 

   ```R
   # Shiny app library
   library(shiny)
   library(shinydashboard)
   # Map visualization
   library(leaflet)
   library(maptools)
   library(magrittr)
   library(sf) # Load shape file
   library(maps)
   # Data manipulation
   library(dplyr)
   ```

3. Required libraries for GTD Plot app 

   ```R
   # Shiny app library
   library(shiny)
   library(shinydashboard)
   # Data visualization
   library(ggplot2)
   library(plyr)
   library(magrittr)
   library(ggvis)
   library(scales)
   # Data manipulation
   library(dplyr)
   library(readr)
   ```

## Folders in Github Repository

1. **DataProcessing** folder contains R files to preprocess data and data files less than 25MB (github limit)
2. **GlobalTerrorismMap** folder contains shape file for world map and R files to run Global Terrorism Map r shiny app
3. **GlobalTerrorismPlots** folder contains R files to run Global Terrorism Plots r shiny app and data files less than 25MB that is used for the app

## Global Terrorism Map R Flowchart

Required dataset: `terrorismData.csv` `worldshapes.shp`



## Global Terrorism Plots R Flowchart

Required dataset: `FullGTD.csv` `WorldBankData.csv` 

## Data Description

1. `terrorismData.csv` 
2. `FullGTD.csv`
3. `WorldBankData.csv`
4.  `ReligionPercentage.csv`

## R Code Specific Files

### DataProcessing


library(dplyr)
library(readr)

# Load Dataset
WBdata <- read.csv("WorldBankdata.csv")
GTDdata <- read.csv("C:/Users/stella/Documents/GitHub/GlobalTerrorismLabs/GlobalTerrorismPlots/fullGTD.csv")
GTDbyCountryYear <- GTDdata %>% group_by(Year, Country) %>%
  dplyr::summarize(NumIncidents= n(),
                   NumFatalities = sum(Fatalities, na.rm=TRUE),
                   NumWounded = sum(Wounded, na.rm=TRUE))

# Create Region variable for Map
GTDdata$Region <- ifelse(GTDdata$Region == "Middle East & North Africa", "Middle East & North Africa",
                       ifelse(GTDdata$Region == "North America", "North America",
                              ifelse(GTDdata$Region == "South Asia", "South Asia",
                                     ifelse(GTDdata$Region == "Sub-Saharan Africa", "Sub-Saharan Africa",
                                            ifelse((GTDdata$Region == "Western Europe"|GTDdata$Region == "Eastern Europe"|GTDdata$Region == "Central Asia"), "Europe & Central Asia",
                                                   ifelse((GTDdata$Region == "Central America & Caribbean"|GTDdata$Region == "South America"), "Latin America & Caribbean",
                                                          "East Asia & Pacific"))))))

# Options
regionOptions <- c("All" = "all",
                   "Middle East & North Africa" = "Middle East & N. Africa",
                   "Sub-Saharan Africa" = "Sub-Saharan Africa",
                   "East Asia & Pacific" = "East Asia & Pacific",
                   "South Asia" = "South Asia",
                   "Europe & Central Asia" = "Europe & Central Asia",
                   "North America" = "North America",
                   "Latin America & Caribbean" = "Latin America"  
)

attackOptions <- c("All" = "all",
                   "Hostage Taking" = "Hostage Taking",
                   "Assault" = "Assault",
                   "Bombing" = "Bombing",
                   "Assassination" = "Assassination",
                   "Facility Attack" = "Facility Attack",
                   "Hijacking" = "Hijacking",
                   "Unknown" = "Unknown"
)

targetOptions <- c("All" = "all",
                   "Armed Forces" = "Armed Forces",
                   "Government" = "Government",
                   "Private Citizens & Property"="Private Citizens & Property",
                   "Business" = "Business",
                   "Infrastructure" = "Infrastructure",
                   "Educational/Religious Organizations"="Educational/Religious",
                   "Other/Unknown" = "Other/Unknown"
)

weaponOptions <- c("All" = "all",
                   "CBRN" = "CBRN",
                   "Firearms" = "Firearms",
                   "Explosives/Bombs/Dynamite" = "Explosives",
                   "Melee" = "Melee",
                   "Incendiary" = "Incendiary",
                   "Other/Unknown" = "Other/Unknown"
)

religionOptions <- c("All" = "all",
                     "Buddhist" = "Buddhist",
                     "Catholic" = "Catholic",
                     "Hindu" = "Hindu",
                     "Jewish" = "Jewish",
                     "Muslim" = "Muslim",
                     "Orthodox" = "Orthodox",
                     "Protestant" = "Protestant",
                     "None" = "None",
                     "Other" = "Other"
)

facetOptions <- c("Attack Type" = "AttackType",
                  "Target Type" = "TargetType",
                  "Weapon Type" = "WeaponType",
                  "Region" = "Region",
                  "Success" = "Success",
                  "Religion" = "Religion"
)

colorOptions <- c("None" = "none",
                  "Region" = "Region",
                  "Religion" = "Religion")


#Custom Colors for graphs
customColors <- c("#a6cee3", "#1f78b4", "#b2df84", "#33a02c",
                  "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00")

#For Y-axis
GTDOptions <-c("Incidents" = "Incidents",
               "Fatalities" = "Fatalities",
               "Wounded" = "Wounded")

#For X-axis
WBOptions <- c("Population (millions)" = "PopulationInMillions",
               "GDP per Capita" = "GDPPerCapita",
               "Life Expectancy" = "LifeExpectancy",
               "Unemployment Rate (Female)" = "FemaleUnemploymentRate",
               "Labor Rate" = "LaborRate",
               "Children per Woman" = "ChildrenPerWoman",
               "Electricity per Capita" = "ElectricityPerCapita",
               "Child Mortality Rate" = "ChildMortalityRate")



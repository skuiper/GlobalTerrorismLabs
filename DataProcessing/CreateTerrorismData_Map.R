# Building TerrorismData for GlobalTerrorism Map

# Loading Library ----
library(dplyr)
library ("readxl")
library(rvest)
library(stringr)
library(readr)

path = "FOLDER PATH TO RAW DATA"

# Load Datasets ----
gtd70to95 <- read_excel(paste0(path, "/gtd_70to95_0718dist.xlsx"))
gtd96to13 <- read_excel(paste0(path, "/gtd_96to13_0718dist.xlsx"))
gtd14to17 <- read_excel(paste0(path, "/gtd_14to17_0718dist.xlsx"))

# Extract columns
# Modify and Extract Columns for TerrorismData ----
extractColumns <- function (df){
  temp <- data.frame (format(df$"eventid", scientific=F), df$"iyear", df$"imonth", df$"iday", 
                      df$"latitude", df$"longitude", df$"country_txt", df$"region_txt", df$"provstate", df$"city",
                      df$"gname", df$"targtype1_txt", df$"attacktype1_txt", df$"weaptype1_txt",
                      df$"nkill", df$"nwound")
  colnames(temp) <- c("ID", "Year", "Month", "Day",  
                      "Latitude", "Longitude", "Country", "Region", "ProvState", "City",
                      "GroupName", "Target", "Attack", "Weapon", "nKill", "nWound")
  return (temp)
}

## Merge datasets and save into .csv file
newgtd70 <- extractColumns(gtd70to95)
newgtd96 <-extractColumns(gtd96to13)
newgtd14 <-extractColumns(gtd14to17)

td <- rbind.data.frame(newgtd70, newgtd96, newgtd14)

# Create Severity column
td["Severity"] <- 2*log(4*td["nKill"] + td["nWound"]+1)

# Add "NumCode" (ISO 3166-1) country code ----
## Create dataframe with country code retrieved from wikipedia
isoCode <- read_html("https://en.wikipedia.org/wiki/ISO_3166-1_numeric") %>% 
  html_nodes(., "table") %>%
  html_table(., header = TRUE, fill = TRUE) %>%.[[1]]
isoCode[1,1] <- "004"
isoCode$Code <- as.numeric(isoCode$Code)
colnames(isoCode) <- c("NumCode", "Country")
row.names(isoCode) <- NULL

td$Country <- as.character(td$Country)
## Correcting Country names ----
td$Country[td$Country == "East Germany (GDR)"] <- "Germany"
td$Country[td$Country == "Czechoslovakia"] <- "Czech Republic"
td$Country[td$Country == "Serbia-Montenegro" |td$Country == "Kosovo"|td$Country == "Yugoslavia"] <- "Serbia"
td$Country[td$Country == "North Yemen" | td$Country == "South Yemen"] <- "Yemen"
td$Country[td$Country == "Democratic Republic of the Congo" | td$Country == "Zaire"] <- "Congo (Kinshasa)"
td$Country[td$Country == "People's Republic of the Congo" | td$Country == "Republic of the Congo"] <- "Congo (Brazzaville)"
td$Country[td$Country == "East Timor"] <- "Timor-Leste"
td$Country[td$Country == "Vanuatu"] <- "New Hebrides"
td$Country[td$Country == "Zimbabwe"] <- "Rhodesia"
td$Country[td$Country == "Guadeloupe"] <- "France"
td$Country[td$Country == "Martinique"] <- "France"
td$Country[td$Country == "French Guiana"] <- "France"

isoCodeModified <- isoCode
isoCodeModified <- rbind.data.frame(isoCodeModified, c(704, "South Vietnam"))
isoCodeModified <- rbind.data.frame(isoCodeModified, c(276, "West Germany (FRG)"))
isoCodeModified$Country[isoCodeModified$Country == "United States of America"] <- "United States"
isoCodeModified$Country[isoCodeModified$Country == "Korea (Democratic People's Republic of)"] <- "North Korea"
isoCodeModified$Country[isoCodeModified$Country == "Korea, Republic of"] <- "South Korea"
isoCodeModified$Country[isoCodeModified$Country == "Taiwan, Province of China"] <- "Taiwan"
isoCodeModified$Country[isoCodeModified$Country == "Czechia"] <- "Czech Republic"
isoCodeModified$Country[isoCodeModified$Country == "Russian Federation"] <- "Russia"
isoCodeModified$Country[isoCodeModified$Country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
isoCodeModified$Country[isoCodeModified$Country == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
isoCodeModified$Country[isoCodeModified$Country == "Viet Nam"] <- "Vietnam"
isoCodeModified$Country[isoCodeModified$Country == "Congo, Democratic Republic of the"] <- "Congo (Kinshasa)"
isoCodeModified$Country[isoCodeModified$Country == "Congo"] <- "Congo (Brazzaville)"
isoCodeModified$Country[isoCodeModified$Country == "Iran (Islamic Republic of)"] <- "Iran"
isoCodeModified$Country[isoCodeModified$Country == "Bolivia (Plurinational State of)"] <- "Bolivia"
isoCodeModified$Country[isoCodeModified$Country == "Bosnia and Herzegovina"] <- "Bosnia-Herzegovina"
isoCodeModified$Country[isoCodeModified$Country == "Syrian Arab Republic"] <- "Syria"
isoCodeModified$Country[isoCodeModified$Country == "Slovakia"] <- "Slovak Republic"
isoCodeModified$Country[isoCodeModified$Country == "Holy See"] <- "Vatican City"
isoCodeModified$Country[isoCodeModified$Country == "Tanzania, United Republic of"] <- "Tanzania"
isoCodeModified$Country[isoCodeModified$Country == "Saint Lucia"] <- "St. Lucia"
isoCodeModified$Country[isoCodeModified$Country == "Saint Kitts and Nevis"] <- "St. Kitts and Nevis"
isoCodeModified$Country[isoCodeModified$Country == "Macao"] <- "Macau"
isoCodeModified$Country[isoCodeModified$Country == "Brunei Darussalam"] <- "Brunei"
isoCodeModified$Country[isoCodeModified$Country == "Eswatini"] <- "Swaziland"
isoCodeModified$Country[isoCodeModified$Country == "North Macedonia"] <- "Macedonia"
isoCodeModified$Country[isoCodeModified$Country == "Lao People's Democratic Republic"] <- "Laos"
isoCodeModified$Country[isoCodeModified$Country == "Falkland Islands (Malvinas)"] <- "Falkland Islands"
isoCodeModified$Country[isoCodeModified$Country == "Palestine, State of"] <- "West Bank and Gaza Strip"
isoCodeModified$Country[isoCodeModified$Country == "Zimbabwe"] <- "Rhodesia"
isoCodeModified$Country[isoCodeModified$Country == "Moldova, Republic of"] <- "Moldova"
isoCodeModified$Country[isoCodeModified$Country == "Côte d'Ivoire"] <- "Ivory Coast"
isoCodeModified$Country[isoCodeModified$Country == "Vanuatu"] <- "New Hebrides"

#### get list of missing country codes -> Soviet Union and International
missing <- anti_join(x= td, y = isoCodeModified, by = "Country")
unique(missing$Country)

## Join td and ISO Country Code dataframe
td <- left_join(x = td, y = isoCodeModified, by = "Country")
td$Year <- as.numeric(td$Year)

# Create Estimates for missing locations ----
td <- mutate(td, MissLocation = ifelse(is.na(Latitude) | is.na(Longitude), 1, 0))

missingLocations <- filter(td, MissLocation ==1)
missingLocations$City <- as.character(missingLocations$City)

miss_by_country_year <- filter(as.data.frame(table(missingLocations$Country, missingLocations$Year)), Freq > 0)

# Load centroid location for estimations
centroids <- read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/Data/Country_Centroids.csv")
centroids <- centroids[, c(49,67:68)]
colnames(centroids) <- c("NumCode", "Center_Long", "Center_Lat")

td$NumCode <- as.numeric(td$NumCode)
# Check which countries are not mergining
not_merge <- anti_join(x=td, y=centroids, by="NumCode")
unique(not_merge$Country) # "Soviet Union"  "International"

# Merging Center_Long, Center_Lat information to terrorism data
td <- left_join(x=td, y=centroids, by="NumCode") 
for (i in 1:nrow(td)){
  if (td[i,]$MissLocation == 1){
    td[i,]$Latitude <- td[i,]$Center_Lat
    td[i,]$Longitude <- td[i,]$Center_Long
  }
}


# Create Region variable for Map
td$MapRegion <- ifelse(td$Region == "Middle East & North Africa", "Middle East & North Africa",
                       ifelse(td$Region == "North America", "North America",
                              ifelse(td$Region == "South Asia", "South Asia",
                                     ifelse(td$Region == "Sub-Saharan Africa", "Sub-Saharan Africa",
                                            ifelse((td$Region == "Western Europe"|td$Region == "Eastern Europe"|td$Region == "Central Asia"), "Europe & Central Asia",
                                                   ifelse((td$Region == "Central America & Caribbean"|td$Region == "South America"), "Latin America & Caribbean",
                                                          "East Asia & Pacific"))))))
write.csv(td, "C:/Users/stella/Documents/GTD/Map/terrorismData.csv")

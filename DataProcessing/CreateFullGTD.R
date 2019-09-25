# Merge GTD data
library ("readxl")
### web scraping library
library(rvest)
library(stringr)
library(readr)
library(dplyr)

# Load Datasets ----
gtd70to95 <- read_excel("H:/GTD/gtd_70to95_0718dist.xlsx")
gtd96to13 <- read_excel("H:/GTD/gtd_96to13_0718dist.xlsx")
gtd14to17 <- read_excel("H:/GTD/gtd_14to17_0718dist.xlsx")

# Modify and Extract Columns for Merged Version ----
extractColumns <- function (df){
  temp <- data.frame (format(df$"eventid", scientific=F), df$"iyear", df$"country_txt", df$"region_txt", df$"success", df$"attacktype1_txt",
                      df$"targtype1_txt", df$"weaptype1_txt", df$"nkill", df$"nwound")
  colnames(temp) <- c("ID", "Year", "Country", "Region", "Success", "AttackType",
                      "TargetType", "WeaponType", "Fatalities", "Wounded")
  return (temp)
}

## Merge datasets and save into .csv file
newgtd70 <- extractColumns(gtd70to95)
newgtd96 <-extractColumns(gtd96to13)
newgtd14 <-extractColumns(gtd14to17)

mergedGTD <- rbind.data.frame(newgtd70, newgtd96, newgtd14)
mergedGTD <- read.csv("H:/GTD/fullGTD.csv")
mergedGTD <- mergedGTD[, -1]

# Add "NumCode" (ISO 3166-1) country code ----
## Create dataframe with country code retrieved from wikipedia
isoCode <- read_html("https://en.wikipedia.org/wiki/ISO_3166-1_numeric") %>% 
  html_nodes(., "table") %>%
  html_table(., header = TRUE, fill = TRUE) %>%.[[1]]
isoCode[1,1] <- "004"
isoCode$Code <- as.numeric(isoCode$Code)
colnames(isoCode) <- c("NumCode", "Country")
row.names(isoCode) <- NULL
mergedGTD$Country <- as.character(mergedGTD$Country)

## Correcting Country names ----
mergedGTD$Country[mergedGTD$Country == "East Germany (GDR)"] <- "Germany"
mergedGTD$Country[mergedGTD$Country == "Czechoslovakia"] <- "Czech Republic"
mergedGTD$Country[mergedGTD$Country == "Serbia-Montenegro" |mergedGTD$Country == "Kosovo"|mergedGTD$Country == "Yugoslavia"] <- "Serbia"
mergedGTD$Country[mergedGTD$Country == "North Yemen" | mergedGTD$Country == "South Yemen"] <- "Yemen"
mergedGTD$Country[mergedGTD$Country == "Democratic Republic of the Congo" | mergedGTD$Country == "Zaire"] <- "Congo (Kinshasa)"
mergedGTD$Country[mergedGTD$Country == "People's Republic of the Congo" | mergedGTD$Country == "Republic of the Congo"] <- "Congo (Brazzaville)"
mergedGTD$Country[mergedGTD$Country == "East Timor"] <- "Timor-Leste"
mergedGTD$Country[mergedGTD$Country == "Vanuatu"] <- "New Hebrides"
mergedGTD$Country[mergedGTD$Country == "Zimbabwe"] <- "Rhodesia"

isoCodeModified <- rbind.data.frame(isoCode, c(704, "South Vietnam"))
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

#### get list of missing country codes
missing <- anti_join(x= mergedGTD, y = isoCodeModified, by = "Country")
unique(missing$Country)

## Join mergedGTD and ISO Country Code dataframe
fullGTD <- left_join(x = mergedGTD, y = isoCode, by = "Country")
fullGTD$Year <- as.numeric(fullGTD$Year)

# Add "NumIncidents" ----
### The number of incidents in the same country and year as the particular observation
countIncidents <- as.data.frame(table(fullGTD$Country, fullGTD$Year))
colnames(countIncidents) <- c("Country", "Year", "numIncidents")
countIncidents$Country <- paste0(countIncidents$Country)
countIncidents$Year <- as.numeric(countIncidents$Year)
AddedNumIncidents <- left_join(x = fullGTD, y = countIncidents, c("Country", "Year"))

# Add "Religion" ----


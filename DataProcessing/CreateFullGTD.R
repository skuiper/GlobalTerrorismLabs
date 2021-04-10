library ("readxl")
### web scraping library
library(rvest)
library(stringr)
library(readr)
library(dplyr)

raw_path = "Insert raw data directory"

gtd <- read_excel(paste0(raw_path, "globalterrorismdb_0221dist.xlsx"))

# Modify and Extract Columns for Merged Version ----
extractColumns <- function (df){
  temp <- data.frame (format(df$"eventid", scientific=F), df$"iyear", df$"country_txt", df$"region_txt", df$"provstate",
                      df$"success", df$"attacktype1_txt", df$"targtype1_txt", df$"weaptype1_txt", df$"nkill", df$"nwound")
  colnames(temp) <- c("ID", "Year", "Country", "Region", "ProvState", "Success", "AttackType",
                      "TargetType", "WeaponType", "Fatalities", "Wounded")
  return (temp)
}

gtd_df <- extractColumns(gtd)

# Add "NumCode" (ISO 3166-1) country code ----
## Create dataframe with country code retrieved from wikipedia
isoCode <- read_html("https://en.wikipedia.org/wiki/ISO_3166-1_numeric") %>% 
  html_nodes(., "table") %>%
  html_table(., header = TRUE, fill = TRUE) %>%.[[1]]
isoCode[1,1] <- "004"
isoCode$Code <- as.numeric(isoCode$Code)
colnames(isoCode) <- c("NumCode", "Country")
row.names(isoCode) <- NULL

gtd_df$Country <- as.character(gtd_df$Country)
## Correcting Country names ----
gtd_df$Country[gtd_df$Country == "East Germany (GDR)"] <- "Germany"
gtd_df$Country[gtd_df$Country == "Czechoslovakia"] <- "Czech Republic"
gtd_df$Country[gtd_df$Country == "Serbia-Montenegro" |gtd_df$Country == "Kosovo"|gtd_df$Country == "Yugoslavia"] <- "Serbia"
gtd_df$Country[gtd_df$Country == "North Yemen" | gtd_df$Country == "South Yemen"] <- "Yemen"
gtd_df$Country[gtd_df$Country == "Democratic Republic of the Congo" | gtd_df$Country == "Zaire"] <- "Congo (Kinshasa)"
gtd_df$Country[gtd_df$Country == "People's Republic of the Congo" | gtd_df$Country == "Republic of the Congo"] <- "Congo (Brazzaville)"
gtd_df$Country[gtd_df$Country == "East Timor"] <- "Timor-Leste"
gtd_df$Country[gtd_df$Country == "Vanuatu"] <- "New Hebrides"
gtd_df$Country[gtd_df$Country == "Zimbabwe"] <- "Rhodesia"
gtd_df$Country[gtd_df$Country == "Guadeloupe"] <- "France"
gtd_df$Country[gtd_df$Country == "Martinique"] <- "France"
gtd_df$Country[gtd_df$Country == "French Guiana"] <- "France"

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
missing <- anti_join(x= gtd_df, y = isoCodeModified, by = "Country")
unique(missing$Country)

## Join gtd_df and ISO Country Code dataframe
fullGTD <- dplyr::left_join(x = gtd_df, y = isoCodeModified %>% select(NumCode, Country), by = "Country")
fullGTD$Year <- as.numeric(fullGTD$Year)

# Add "NumIncidents" ----
### The number of incidents in the same country and year as the particular observation
countIncidents <- as.data.frame(table(fullGTD$Country, fullGTD$Year))
colnames(countIncidents) <- c("Country", "Year", "NumIncidents")
countIncidents$Country <- as.character(countIncidents$Country)
countIncidents$Year <- as.numeric(as.character(countIncidents$Year))
fullGTD$Country <- as.character(fullGTD$Country)
fullGTD <- left_join(x = fullGTD, y = countIncidents, c("Country", "Year"))

# Add "Religion" ----
religion <- read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/Data/ReligionPercentage.csv")
religion <- religion[,c(2,12)]
religion$Country <- as.character(religion$Country)

## Change Country name ----
unique(anti_join(x = fullGTD, y = religion, by = "Country")$Country)
religion$Country[religion$Country == "Korea, South"] <- "South Korea"
religion$Country[religion$Country == "Gaza Strip"] <- "West Bank and Gaza Strip"
religion$Country[religion$Country == "Czechia"] <- "Czech Republic"
religion$Country[religion$Country == "Congo, Democratic Republic of the"] <- "Congo (Kinshasa)"
religion$Country[religion$Country == "Holy See (Vatican City)"] <- "Vatican City"
religion$Country[religion$Country == "Eswatini"] <- "Swaziland"
religion$Country[religion$Country == "Falkland Islands (Islas Malvinas)"] <- "Falkland Islands"
religion$Country[religion$Country == "Bosnia and Herzegovina"] <- "Bosnia-Herzegovina"
religion$Country[religion$Country == "Slovakia"] <- "Slovak Republic"
religion$Country[religion$Country == "North Macedonia"] <- "Macedonia"
religion$Country[religion$Country == "Gambia, The"] <- "Gambia"
religion$Country[religion$Country == "Korea, North"] <- "North Korea"
religion$Country[religion$Country == "Saint Kitts and Nevis"] <- "St. Kitts and Nevis"
religion$Country[religion$Country == "Saint Lucia"] <- "St. Lucia"
religion$Country[religion$Country == "Vanuatu"] <- "New Hebrides"

fullGTD <- left_join(x = fullGTD, y = religion, "Country")

## Change Primary Religion manually
fullGTD$Primary[fullGTD$Country == "West Germany (FRG)"] <- fullGTD$Primary[fullGTD$Country == "Germany"][1]
fullGTD$Primary[fullGTD$Country == "South Vietnam"] <- fullGTD$Primary[fullGTD$Country == "Vietnam"][1]
# Missing Primary Religion 
# Ivory Coast

colnames(fullGTD) <- c(colnames(fullGTD)[-14], "Religion")

data_path = "Insert path to data directory"
write.csv(fullGTD, paste0(data_path,"/fullGTD.csv"), row.names = FALSE)



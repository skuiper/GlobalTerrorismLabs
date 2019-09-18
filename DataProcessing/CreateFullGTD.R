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
  # need to add "NumCode", "NumIncidents", "Religion" column based on the country
  return (temp)
}

## Merge datasets and save into .csv file
newgtd70 <- extractColumns(gtd70to95)
newgtd96 <-extractColumns(gtd96to13)
newgtd14 <-extractColumns(gtd14to17)

mergedGTD <- rbind.data.frame(newgtd70, newgtd96, newgtd14)

# Add "NumCode" (ISO 3166-1) country code ----
## Create dataframe with country code retrieved from wikipedia
isoCode <- read_html("https://en.wikipedia.org/wiki/ISO_3166-1_numeric") %>% 
  html_nodes(., "table") %>%
  html_table(., header = TRUE, fill = TRUE) %>%.[[1]]
isoCode[1,1] <- "004"
isoCode$Code <- as.numeric(isoCode$Code)
colnames(isoCode) <- c("NumCode", "Country")
row.names(isoCode) <- NULL

## Correcting Country names
mergedGTD$Country <- as.character(mergedGTD$Country)
missing <- anti_join(x= mergedGTD, y = isoCode, by = "Country")
mergedGTD$Country[mergedGTD$Country == "United States"] <- "United States of America"


## Join mergedGTD and ISO Country Code dataframe
fullGTD <- left_join(x = mergedGTD, y = isoCode, by = "Country")

# Add "NumIncidents" ----
### The number of incidents in the same country and year as the particular observation
countIncidents <- as.data.frame(table(fullGTD$NumCode, fullGTD$Year))
countIncidents$NumCode <- as.numeric(countIncidents$NumCode)
colnames(countIncidents) <- c("NumCode", "Year", "numIncidents")

# Add "Religion" ----


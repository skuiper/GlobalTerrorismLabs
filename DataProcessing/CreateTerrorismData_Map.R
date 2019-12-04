# Building TerrorismData for GlobalTerrorism Map

# Loading Library ----
library(dplyr)
library ("readxl")

# Load Datasets ----
gtd70to95 <- read_excel("H:/GTD/gtd_70to95_0718dist.xlsx")
gtd96to13 <- read_excel("H:/GTD/gtd_96to13_0718dist.xlsx")
gtd14to17 <- read_excel("H:/GTD/gtd_14to17_0718dist.xlsx")

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

mergedGTD <- rbind.data.frame(newgtd70, newgtd96, newgtd14)

# Create Severity column
mergedGTD["Severity"] <- 2*log(4*mergedGTD["nKill"] + mergedGTD["nWound"]+1)

# Create Prediction for missing locations
td <- mutate(td, MissLocation = ifelse(is.na(Latitude) | is.na(Longitude), 1, 0))

centroids <- read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/Country_Centroids.csv")

missing <- filter(td, MissLocation ==1)
missing$City <- as.character(missing$City)

miss_by_country_year <- filter(as.data.frame(table(missing$Country, missing$Year)), Freq > 0)

write.csv(mergedGTD, "H:\\GTDdata\\terrorismData.csv")

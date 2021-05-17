# Merge Worldbank Data
library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(readr)

# Load datasets----
GDPPerCapita <- read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/Data/WorldBank/WB_GDP.csv")
FemaleUnemploymentRate <-  read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/Data/WorldBank/WB_FemaleUnemployment.csv")
ElectricityPerCapita <-  read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/Data/WorldBank/WB_Electricity.csv")
Population <- read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/Data/WorldBank/WB_Pop.csv")
PopulationDensity <- read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/Data/WorldBank/WB_PopDen.csv")
ChildrenPerWoman <- read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/Data/WorldBank/WB_Fertility.csv")
ChildMortalityRate <- read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/Data/WorldBank/WB_Mortality.csv")
LaborRate <- read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/Data/WorldBank/WB_LaborForce.csv")
LifeExpectancy <- read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/Data/WorldBank/WB_LifeExpectancy.csv")

# Gather data ----
year_to_variable <- function (df, var_name){
  # Change the column names
  colnames(df) <- c("Series.Name", "Series.Code", "Country.Name", "Country.Code", 1970:2019) 
  
  df <- gather(data=df, key = year, value = var,
               "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", 
               "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", 
               "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", 
               "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
               "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
  df <- df[,c(-1,-2)]
  colnames(df) <- c("Country", "ISOCode", "Year", var_name) 
  return (df)
}

GDPPerCapita <- year_to_variable(GDPPerCapita, "GDPPerCapita")
FemaleUnemploymentRate <- year_to_variable(FemaleUnemploymentRate, "FemaleUnemploymentRate" )
ElectricityPerCapita <- year_to_variable(ElectricityPerCapita, "ElectricityPerCapita")
Population <- year_to_variable(Population, "Population")
PopulationDensity <- year_to_variable(PopulationDensity,  "PopulationDensity")
ChildrenPerWoman <- year_to_variable(ChildrenPerWoman, "ChildrenPerWoman")
ChildMortalityRate <- year_to_variable(ChildMortalityRate,"ChildMortalityRate")
LaborRate <- year_to_variable(LaborRate, "LaborRate")
LifeExpectancy <- year_to_variable(LifeExpectancy, "LifeExpectancy")

# Change population variable to Population in millions
Population$PopulationInMillions <- as.numeric(Population$Population) / 1000000

# Merge dataset
WorldBank <- full_join(x = GDPPerCapita, y = FemaleUnemploymentRate[,-2], by = c("Country", "Year"))%>%
  full_join(x = ., y = ElectricityPerCapita[,-2], by = c("Country", "Year")) %>%
  full_join(x = ., y = Population[,-2], by = c("Country", "Year")) %>%
  full_join(x = ., y = PopulationDensity[,-2], by = c("Country", "Year")) %>%
  full_join(x = ., y = ChildrenPerWoman[,-2], by = c("Country", "Year")) %>%
  full_join(x = ., y = ChildMortalityRate[,-2], by = c("Country", "Year")) %>%
  full_join(x = ., y = LaborRate[,-2], by = c("Country", "Year")) %>%
  full_join(x = ., y = LifeExpectancy[,-2], by = c("Country", "Year"))

WorldBank$ISOCode <- as.character(WorldBank$ISOCode)

isoCode <- read_html("https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes") %>% 
  html_nodes(., "table")%>%
  html_table(., header = TRUE, fill = TRUE) %>%.[[1]]%>%.[-1,c(5,6)]
colnames(isoCode) <- c("ISOCode", "NumCode")

WorldBank <- left_join(x=WorldBank, y = isoCode, by = "ISOCode")

# Resolve conflict with GTD data country name ----

gtd_path = "Insert path to data directory"
GTD <- read.csv(paste0(gtd_path,"/fullGTD.csv"))
GTD$NumCode <- as.numeric(GTD$NumCode)
WorldBank$NumCode <- as.numeric(WorldBank$NumCode)
diff <- anti_join(x=GTD, y = WorldBank, by ="NumCode")
# unique(diff$Country)
# Taiwan | Western Sahara | Vatican City  | Falkland Islands | Wallis and Futuna

write.csv(WorldBank, "WorldBankData.csv")

data_path = "Insert path to data directory"
write.csv(WorldBank, paste0(data_path,"/WorldBankData.csv"), row.names = FALSE)

# Merge GM Data

library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(readr)

# Load datasets
wb1 <- read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/WorldBank/GDPUnemploymentWorldBank.csv")
# GDP per capita, PPP | Unemployment, female | Electric power consumption (kWh per capita)

wb2 <- read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/WorldBank/PopulationWorldBank.csv")
# Population density | Population, total

wb3 <- read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/WorldBank/FertilityWorldBank.csv")
# Fertility rate, total | Mortality rate

wb4 <- read.csv("https://raw.githubusercontent.com/skuiper/GlobalTerrorismLabs/master/DataProcessing/WorldBank/LaborForceRateWorldBank.csv")
# Labor force participation

# Change the column names
colnames(wb1) <- c("Series.Name", "Series.Code", "Country.Name", "Country.Code", 1970:2017) 
colnames(wb2) <- c("Series.Name", "Series.Code", "Country.Name", "Country.Code", 1970:2017) 
colnames(wb3) <- c("Series.Name", "Series.Code", "Country.Name", "Country.Code", 1970:2017) 
colnames(wb4) <- c("Series.Name", "Series.Code", "Country.Name", "Country.Code", 1970:2017) 

GDPPerCapita <- filter(wb1, Series.Code =="NY.GDP.PCAP.PP.KD")
FemaleUnemploymentRate <-  filter(wb1, Series.Code =="SL.UEM.TOTL.FE.NE.ZS")
ElectricityPerCapita <-  filter(wb1, Series.Code =="EG.USE.ELEC.KH.PC")
Population <- filter(wb2, Series.Code =="SP.POP.TOTL")
PopulationDensity <- filter(wb2, Series.Code =="EN.POP.DNST")
ChildrenPerWoman <- filter(wb3, Series.Code =="SP.DYN.TFRT.IN")
ChildMortalityRate <- filter(wb3, Series.Code =="SH.DYN.MORT")
LabourRate <- filter(wb4, Series.Code =="SL.TLF.ACTI.ZS")

# Gather data
year_to_variable <- function (df){
  df <- gather(data=df, key = year, value = var,
               "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980",
               "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992",
               "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
               "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
  return (df[,c(-1,-2)])
}

GDPPerCapita <- year_to_variable(GDPPerCapita)
colnames(GDPPerCapita) <- c("Country", "ISOCode", "Year", "GDPPerCapita")
FemaleUnemploymentRate <- year_to_variable(FemaleUnemploymentRate)
colnames(FemaleUnemploymentRate) <- c("Country", "ISOCode","Year", "FemaleUnemploymentRate")
ElectricityPerCapita <- year_to_variable(ElectricityPerCapita)
colnames(ElectricityPerCapita) <- c("Country", "ISOCode", "Year", "ElectricityPerCapita")
Population <- year_to_variable(Population)
colnames(Population) <- c("Country", "ISOCode", "Year", "Population")
PopulationDensity <- year_to_variable(PopulationDensity)
colnames(PopulationDensity) <- c("Country", "ISOCode", "Year",  "PopulationDensity")
ChildrenPerWoman <- year_to_variable(ChildrenPerWoman)
colnames(ChildrenPerWoman) <- c("Country", "ISOCode", "Year",  "ChildrenPerWoman")
ChildMortalityRate <- year_to_variable(ChildMortalityRate)
colnames(ChildMortalityRate) <- c("Country", "ISOCode", "Year",  "ChildMortalityRate")
LabourRate <- year_to_variable(LabourRate)
colnames(LabourRate) <- c("Country", "ISOCode", "Year", "LabourRate")

# Change population variable to Population in millions
Population$PopulationInMillions <- as.numeric(Population$Population) / 1000000

# Merge dataset
WorldBank <- full_join(x = GDPPerCapita, y = FemaleUnemploymentRate[,-2], by = c("Country", "Year"))%>%
  full_join(x = ., y = ElectricityPerCapita[,-2], by = c("Country", "Year")) %>%
  full_join(x = ., y = Population[,-2], by = c("Country", "Year")) %>%
  full_join(x = ., y = PopulationDensity[,-2], by = c("Country", "Year")) %>%
  full_join(x = ., y = ChildrenPerWoman[,-2], by = c("Country", "Year")) %>%
  full_join(x = ., y = ChildMortalityRate[,-2], by = c("Country", "Year")) %>%
  full_join(x = ., y = LabourRate[,-2], by = c("Country", "Year"))

# Resolve conflict with GTD data country name ----
GTD <- read.csv("H:/GTDdata/GTDfinal.csv")
GTD$Country <- as.character(GTD$Country)
WorldBank$Country <- as.character(WorldBank$Country)

isoCode <- read_html("https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes") %>% 
  html_nodes(., "table")%>%
  html_table(., header = TRUE, fill = TRUE) %>%.[[1]]%>%.[-1,c(5,6)]
colnames(isoCode) <- c("ISOCode", "NumCode")

isoCode$ISOCode <- as.character(isoCode$ISOCode)
WorldBank$ISOCode <- as.character(WorldBank$ISOCode)
t <- left_join(x=WorldBank, y = isoCode, by = "ISOCode")
GTD$NumCode <- as.character(GTD$NumCode)
t$NumCode <- as.character(t$NumCode)
temp <- left_join(x=GTD, y = t, by ="NumCode")

write.csv(WorldBank, "H:/GTDdata/WorldBankData.csv")

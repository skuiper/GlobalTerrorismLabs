# Merge GM Data

library(dplyr)
library(tidyr)

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
  return (df[,c(-1,-2, -4)])
}

GDPPerCapita <- year_to_variable(GDPPerCapita)
colnames(GDPPerCapita) <- c("Country", "Year", "GDPPerCapita")
FemaleUnemploymentRate <- year_to_variable(FemaleUnemploymentRate)
colnames(FemaleUnemploymentRate) <- c("Country", "Year", "FemaleUnemploymentRate")
ElectricityPerCapita <- year_to_variable(ElectricityPerCapita)
colnames(ElectricityPerCapita) <- c("Country", "Year",  "ElectricityPerCapita")
Population <- year_to_variable(Population)
colnames(Population) <- c("Country", "Year", "Population")
PopulationDensity <- year_to_variable(PopulationDensity)
colnames(PopulationDensity) <- c("Country", "Year", "PopulationDensity")
ChildrenPerWoman <- year_to_variable(ChildrenPerWoman)
colnames(ChildrenPerWoman) <- c("Country", "Year",  "ChildrenPerWoman")
ChildMortalityRate <- year_to_variable(ChildMortalityRate)
colnames(ChildMortalityRate) <- c("Country", "Year",  "ChildMortalityRate")
LabourRate <- year_to_variable(LabourRate)
colnames(LabourRate) <- c("Country", "Year",  "LabourRate")

# Change population variable to Population in millions
Population$PopulationInMillions <- as.numeric(Population$Population) / 1000000

# Merge dataset
WorldBank <- full_join(x = GDPPerCapita, y = FemaleUnemploymentRate, by = c("Country", "Year")) %>%
  full_join(x = ., y = ElectricityPerCapita, by = c("Country", "Year")) %>%
  full_join(x = ., y = Population, by = c("Country", "Year")) %>%
  full_join(x = ., y = PopulationDensity, by = c("Country", "Year")) %>%
  full_join(x = ., y = ChildrenPerWoman, by = c("Country", "Year")) %>%
  full_join(x = ., y = ChildMortalityRate, by = c("Country", "Year")) %>%
  full_join(x = ., y = LabourRate, by = c("Country", "Year"))

codes <- as.data.frame(table(wb1$Country.Name, wb1$Country.Code))[,-3]
colnames(codes) <- c("Country", "ISOCode")

WorldBank <- full_join(x=WorldBank, y=codes, by="Country")
write.csv(WorldBank, filepath)

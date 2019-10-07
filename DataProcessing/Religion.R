# Religion by Country Dataset Retrieval
# Source: Central Intelligence Agency (CIA) World Factbook

# Religion Categories ----
# Buddhist
# Catholic
# Christian
# Hindu
# Jewish
# Muslim
# None/Other
# Orthodox

# Required Packages ----
## Web Scraping
library(rvest)
library(stringr)
library(readr)

# Retrieve Religion Table from Web pages ----
html <- read_html("https://www.cia.gov/library/publications/the-world-factbook/fields/401.html")
table <- html_nodes(html, "table")
religionTable <- html_table(table, header=TRUE, fill = TRUE)[[1]]
# write.csv(religionTable, "H:\\GTD\\ReligionRawData.csv") # save for future purpose
# str(religionTable)

# Data manipulation ----
removeParenthesis <- function (str) {
  return (str_replace_all(str, "\\(.+?\\)", " "))
}
# Extract percentages from the raw data ----
## Get the percentage distribution for each country so that we can see the diversity of religion in the country and find the primary religion
## Using the first religion in the text as primary have some edge cases such as Germany, Hong Kong where highest percentage of people have none/other religion

religionPerc <- religionTable
religionPerc[,2] <- removeParenthesis(religionPerc[,2])

# Check how many different percentages are there in
max = 0
for (i in 1:nrow(religionPerc)){
  temp = length(str_extract_all(religionPerc[i,2], "\\d+(\\.\\d+){0,1}%")[[1]])
  if (temp > max){
    max = temp
    print(i)
  }
}

# Create Dataframe 
for (i in 1:nrow(religionPerc)){
  relvec <- str_extract_all(religionPerc[i,2], "((?=^)|(?=\\s+))(.+?)(\\d+(\\.\\d+){0,1})(?=%)")[[1]]
  otherperc <- 0
  for (j in 1:length(relvec)){
    info <- relvec[i]
    num <- as.numeric(str_extract_all(info, \\d+(\\.\\d+){0,1}))
    if (str_detect(info, regex('Buddhist',ignore_case = TRUE))){
      
    }
  }
}
str_extract_all(religionPerc[225,2], "((?=^)|(?=\\s+))(.+?)(\\d+(\\.\\d+){0,1})(?=%)")
colnames(religion) <- c("Country", "Buddhist", "Catholic", "Hindu", "Jewish", "Muslim", "Orthodox", "Protestant", "None", "Other")

# Extract Primary Religion for each country ----

# 


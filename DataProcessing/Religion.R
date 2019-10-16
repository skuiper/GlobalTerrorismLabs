# Religion by Country Dataset Retrieval
# Source: Central Intelligence Agency (CIA) World Factbook

# Religion Categories ----
# Buddhist
# Catholic
# Protestant
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
library(dplyr)

# Retrieve Religion Table from Web pages ----
html <- read_html("https://www.cia.gov/library/publications/the-world-factbook/fields/401.html")
table <- html_nodes(html, "table")
religionTable <- html_table(table, header=TRUE, fill = TRUE)[[1]]
# write.csv(religionTable, "H:\\GTD\\ReligionRawData.csv") # save for future purpose
# str(religionTable)

# Data manipulation ----
removeParenthesis <- function (str) {
  str <- str_replace_all(str, "note:(.+)", " ")
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
percs <- NULL

for (i in 1:nrow(religionPerc)){
  print(i)
  relvec <- str_extract_all(religionPerc[i,2], "((?=^)|(?=\\s+))(.+?)(\\d+(\\.\\d+){0,1})(?=%)")[[1]]
  if (rlang::is_empty(relvec)){
    print(paste0("EDGE ", i, " doesn't have percentage fix this later"))
  }else{
    vec <- rep(0, times= 9)
    for (j in 1:length(relvec)){
      info <- relvec[j]
      nums <- str_extract_all(info, "\\d+(\\.\\d+){0,1}")
      num <- as.numeric(nums[[1]][1])
      if (str_detect(info, regex('Buddhist', ignore_case = TRUE))| str_detect(info, regex('Buddhism', ignore_case = TRUE))){
        vec[1] <- num
      }else if (str_detect(info, regex('Catholic', ignore_case = TRUE))){
        vec[2] <- num
      }else if (str_detect(info, regex('Hindu', ignore_case = TRUE))){
        vec[3] <- num
      }else if (str_detect(info, regex('Jewish', ignore_case = TRUE))){
        vec[4] <- num
      }else if (str_detect(info, regex('Muslim', ignore_case = TRUE))){
        vec[5] <- num
      }else if (str_detect(info, regex('Orthodox', ignore_case = TRUE))){
        vec[6] <- vec[6] + num
      }else if (str_detect(info, regex('Protestant', ignore_case = TRUE)) | str_detect(info, regex('Christian', ignore_case = TRUE))
                | str_detect(info, regex('Lutheran', ignore_case = TRUE)) | str_detect(info, regex('Evangelical', ignore_case = TRUE))
                | str_detect(info, regex('Pentecostal', ignore_case = TRUE))){
        vec[7] <- vec[7] + num
      }else if (str_detect(info, regex('None', ignore_case = TRUE)) | str_detect(info, regex('atheist', ignore_case = TRUE))
                | str_detect(info, regex('unaffiliated', ignore_case = TRUE)) | str_detect(info, regex('no religion', ignore_case = TRUE))){
        vec[8] <- num
      }else {
        if (str_detect(info, regex('other', ignore_case = TRUE)) == FALSE & str_detect(info, regex('unspecified', ignore_case = TRUE)) == FALSE){ # when it was not explicitly "other"
          print(paste0(j, " ", info))
        }
        vec[9] <- vec[9] + num
      }
    }
    percs <- rbind.data.frame(percs, c(as.character(religionPerc[i,1]), vec), stringsAsFactors = FALSE)
  }
}
colnames(percs) <- c("Country", "Buddhist", "Catholic", "Hindu", "Jewish", "Muslim", "Orthodox", "Protestant", "None", "Other")
#write.csv(percs, "H:\\GTD\\ReligionPercs.csv") # save for future purpose
# change to numeric values for percentages
cols.num <- colnames(percs)[-1]
percs[cols.num] <- sapply(percs[cols.num], as.numeric)

# Manual fixes for missed categorization ex. Church of Norway ----
# Armenia: Armenian Apostolic = Orthodox 92.6%
Armenia <- percs[9,]
Armenia[7] <- Armenia[7] + 92.6
Armenia[10] <- Armenia[10] - 92.6
percs[9,] <- Armenia
# Belarus: non-believers = 41.1%
Belarus <- percs[18,]
Belarus[9] <- 41.1
Belarus[10] <- Belarus[10] - 41.1
percs[18,] <- Belarus
# Kiribati: Kiribati Uniting Church = Protestant (31.3%)
Kiribati <- percs[104,]
Kiribati[8] <- Kiribati[8] + 31.3
Kiribati[10] <- Kiribati[10] - 31.3
percs[104,] <- Kiribati
# Norway: Church of Norway = Protestant (70.6%)
Norway <- percs[147,]
Norway[8] <- Norway[8] + 70.6
Norway[10] <- Norway[10] - 70.6
percs[147,] <- Norway
# Sweden: Church of Sweden = Protestant (Lutheran) (60.2%)
Sweden <- percs[184,]
Sweden[8] <- Sweden[8] + 60.2
Sweden[10] <- Sweden[10] - 60.2
percs[184,] <- Sweden

# Extract Primary Religion for each country
primary <- colnames(percs[,2:10])[apply(percs[,2:10],1,which.max)]
percs <- cbind.data.frame(percs, Primary = primary)

# Add religion without percentage data (22 countries)
withoutperc <- anti_join(religionPerc, percs, by = "Country")
withoutperc$Country

Catholic <- c("Andorra", "Equatorial Guinea", "Guatemala", "Holy See (Vatican City)", "Saint Barthelemy", "Saint Martin", "San Marino")
Other <- c("Korea, North", "South Sudan")
Protestant <- c("Greenland", "Guernsey", "Isle of Man", "Jersey", "Madagascar", "Northern Mariana Islands")
Orthodox <- c("Ukraine")
Muslim <- c("Eritrea", "Maldives", "Saudi Arabia", "Somalia", "Sudan", "Western Sahara")

for (i in 1:nrow(withoutperc)){
  country <- withoutperc[i,1]
  primary <- ifelse(country %in% Catholic, "Catholic", 
                    ifelse(country %in% Protestant, "Protestant", 
                           ifelse(country %in% Muslim, "Muslim",
                                  ifelse(country %in% Other, "Other", "Orthodox"))))
  percs <- rbind.data.frame(percs, c(country, NA, NA, NA, NA, NA, NA, NA, NA, NA, primary), stringsAsFactors = FALSE)
}

write.csv(percs, "H:\\GTD\\ReligionPercentage.csv") # save for future purpose


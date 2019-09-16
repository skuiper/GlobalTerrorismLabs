####################################################################
# Title: Global Terrorism Plots
# Version: Plots 1.3 - merged Scatterplots
# Author: Ying Long and Zachary Segall, with help from Shonda Kuiper, 
#         Pamela Fellers
# Date: 7/01/15
# Abstract: An shiny application for taking data and making it 
#   into a variety of interactive plots (scatterplot, stacked-line plot, and 
#   bar plot) which deal with data from the global terrorism database (GTD). 
#   The scatterplot compares data from the global terrorism database (GTD) with #   socio-economic data from Gapminder. The stacked-line plot allows us to 
#   look at incidents, fatalities, or wounded over time, colored by a variety 
#   of factors (ex. weapon type). The bar graph shows us how many countries 
#   have certain number of incidents, fatalities, or wounded in a certain year.
#
# Files:
#   -server.R: includes code that supports data processing for UI
#   -ui.R: includes the code for the design of the UI
#   -global.R: includes the Data to be used in plots and UI specifications
# Terminology:
#   Country-Year: In the scatterplot, each point represents a particular 
#   country and year (referred to as a **country-year**). For example, we can 
#   look at all the incidents that occurred in the US in 1984, and the point 
#   would be labeled  # "United States 1984" in the app.
# Added souces: 
####################################################################
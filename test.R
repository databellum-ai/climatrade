# IAI Constituent Subindicies
# The IAI is driven by reader interest on Investopedia across three categories of topics:
#   -Macroeconomic_Anxiety (macroeconomic anxiety, inflation/"Inflación", deflation/"Deflación") 
# -Market_Anxiety(market anxiety, short selling/"Venta corta", volatility/"Volatilidad")
# -DebtCredit_Anxiety(debt anxiety, credit anxiety, default on debt/"Impago", solvency/"Solvencia", bankruptcy/"Quiebra financiera")

# ===============================================
# Extract search trends from Google Trends
# ===============================================
# https://www.rdocumentation.org/packages/gtrendsR/versions/1.3.5/topics/gtrends

library(tidyverse)
library(gtrendsR)


queryTrends_test <- gtrends(
  keyword = "mercedes benz", 
  time = "all", 
  onlyInterest = TRUE) # only interest-over-time is faster

# Extract dataframe containing hits over time
searches_iot <- queryTrends_test %>% .$interest_over_time

head(searches_iot)


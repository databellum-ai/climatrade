# ===============
# CLEAN ENVIRONMENT
# ---------------
# rm(list = ls())

# ===============
# Environment constants
# ---------------
absoluteInitialDate <- "1960-01-01"

# ===============
# PACKAGES REQUIRED
# ---------------
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(gtrendsR)) install.packages("gtrendsR", repos = "http://cran.us.r-project.org") # To get GoogleTrends data
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org") # To improve charts
if(!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
if(!require(spotifyr)) install.packages("spotifyr", repos = "http://cran.us.r-project.org")
if(!require(rnoaa)) install.packages("rnoaa", repos = "http://cran.us.r-project.org")
if(!require(OECD)) install.packages("OECD", repos = "http://cran.us.r-project.org")
if(!require(gtools)) install.packages("gtools", repos = "http://cran.us.r-project.org")
if(!require(tidygeocoder)) install.packages("tidygeocoder", repos = "http://cran.us.r-project.org") # Resolve coordinates of cities/places
if(!require(openSkies)) install.packages("openSkies", repos = "http://cran.us.r-project.org") # Air traffic data from OpenSkies
if(!require(suncalc)) install.packages("suncalc", repos = "http://cran.us.r-project.org") # Moon and Sun daily data
if(!require(ROAuth)) install.packages("ROAuth", repos = "http://cran.us.r-project.org") # Moon and Sun daily data
if(!require(twitteR)) install.packages("twitteR", repos = "http://cran.us.r-project.org") # Moon and Sun daily data
if(!require(syuzhet)) install.packages("syuzhet", repos = "http://cran.us.r-project.org") # sentiment analysis
if(!require(openxlsx)) install.packages("openxlsx", repos = "http://cran.us.r-project.org") # import/export Excel
if(!require(tidyquant)) install.packages("tidyquant", repos = "http://cran.us.r-project.org") # To get stock data
if(!require(imputeTS)) install.packages("imputeTS", repos = "http://cran.us.r-project.org") # To impute values in time series
# INSTALL SCRAPE PACKAGES:
  # + Ensure java installed
  if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")  # to scrape static pages
  if(!require(RSelenium)) install.packages("RSelenium", repos = "http://cran.us.r-project.org")  # to scrape dynamic  pages, includes binman
  if(!require(wdman)) install.packages("wdman", repos = "http://cran.us.r-project.org")  # for chromedriver install
  # + Install chromedriver with function chrome() of package "wdman":
  # cDrv <- chrome()


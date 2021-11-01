# -----------------------------------------------------------------
# -----------------------------------------------------------------
# Data transformation from .RDS extracted files
# -----------------------------------------------------------------
# -----------------------------------------------------------------


# =========================================
# Transform weather from NOAA
# =========================================
allStationsData_ts <- readRDS("data/data_weather_ts.rds") # time-series format
allStationsData_sp <- readRDS("data/data_weather_sp.rds") # spread format


# =========================================
# Transform Moon angle data
# =========================================


# =========================================
# Transform Twitter posts related data
# =========================================


# =========================================
# Transform from news
# =========================================


# =========================================
# Transform from FIFA Ranking
# =========================================


# =========================================
# Transform stock prices from Yahoo Finance
# =========================================


# =========================================
# Transform searches from Google Trends
# =========================================



# =========================================
# Extract leading indicators from OECD
# =========================================



# =========================================
# Extract music downloads from SPOTIFY
# =========================================


# =======================================
# Save to RDS
# =======================================
# allData <- rbind(..., ..., ...)
# head(allData)
# saveRDS(allData, "data/data_extracted.rds")
# write.csv(allData, "data/data_extracted.csv")
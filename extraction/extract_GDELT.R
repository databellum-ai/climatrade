# ================================
# Extract GDELT data
# ================================
# BIGQUERY INTERACTIVO VIA WEB: https://console.cloud.google.com/bigquery?project=applied-flag-330811
# https://www.r-bloggers.com/2020/03/google-big-query-with-r/
# https://bigrquery.r-dbi.org/
# https://www.infoworld.com/article/3622926/how-to-use-r-with-bigquery.html

library(tidyverse)
library(openxlsx)
library(bigrquery)
library(lubridate)

print("Extracting GDELT events")

# Prepare filter for SQL query
initialDateGDELT_sqlInt <- "19790101"
currenDate <- Sys.time()
currentDate_sqlInt <- as.character(10000*year(currenDate) + 100*month(currenDate) + day(currenDate))
dateFilter <- paste0("SQLDATE BETWEEN ",initialDateGDELT_sqlInt," AND ", currentDate_sqlInt)

billing <- "applied-flag-330811"
bq_user()# active user: databellum.ai@gmail.com

sql <- paste0("SELECT
SQLDATE date, 
EventRootCode, 
COUNT(*) Events,
SUM(NumArticles) Articles,
(SUM(AvgTone * NumArticles) / SUM(NumArticles)) tone, 
(SUM(GoldsteinScale * NumArticles) / SUM(NumArticles)) goldstein
FROM `gdelt-bq.full.events`
WHERE (", dateFilter, ") AND (IsRootEvent = 1) 
GROUP BY SQLDATE, EventRootCode
ORDER BY date DESC, EventRootCode")

# call bigquery
GDELTevents <- bq_table_download(bq_project_query(billing, sql), n_max = 1000000)
head(GDELTevents)

# group by date and extract only Tone and Goldstein scale
GDELTevents <- GDELTevents %>% mutate(date = ymd(date)) %>% group_by(date) %>% summarise(tone = mean(tone), goldstein = mean(goldstein)) %>% mutate(countryCode = NA)

# # Save to RDS
saveRDS(GDELTevents, "data/data_GDELT_ts.rds")

print("GDELT events extraction process FINISHED")


# DECIDIR valores parámetros: EventRootCode, QuadClass
# PONDERACIÓN (sum?/avg?... GoldsteinScale, mentions, sources, articles 


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

print("Extracting GDELT events")

# Prepare filter for SQL query
initialDateGDELT_sqlInt <- "19790101"
currenDate <- Sys.time()
currentDate_sqlInt <- as.character(10000*year(currenDate) + 100*month(currenDate) + day(currenDate))
dateFilter <- paste0("SQLDATE BETWEEN ",initialDateGDELT_sqlInt," AND ", currentDate_sqlInt)
eventRootCodeFilter <- "'14','15','16','17','18','19','20'"
quadClassFilter <- "3, 4"

billing <- "applied-flag-330811"
bq_user()# active user: databellum.ai@gmail.com

sql <- paste0("SELECT
  COUNT(*) totalEvents,
  SQLDATE date,
  SUM(AvgTone) toneSum,
  AVG(AvgTone) toneAvg,
  SUM(GoldsteinScale) GoldsteinScaleSum,
  AVG(GoldsteinScale) GoldsteinScaleAvg,
  AVG(NumMentions) mentionsAvg,
  AVG(NumSources) sourcesAvg,
  AVG(NumArticles) articlesAvg, 
  SUM(NumMentions) mentionsSum, 
  SUM(NumSources) sourcesSum, 
  SUM(NumArticles) articlesSum
FROM `gdelt-bq.full.events`
WHERE (", dateFilter, ")
  AND EventRootCode IN (", eventRootCodeFilter, ")
  AND (QuadClass IN (", quadClassFilter,"))
  AND (IsRootEvent = 1)
GROUP BY SQLDATE")
GDELTevents <- bq_table_download(bq_project_query(billing, sql), n_max = 10000)
class(GDELTevents)
head(GDELTevents)

# # Save to RDS
GDELTevents <- GDELTevents %>% mutate(countryCode = NA)
saveRDS(GDELTevents, "data/data_GDELT_ts.rds")

print("GDELT events extraction process FINISHED")


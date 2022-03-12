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

billing <- "applied-flag-330811"
bq_user()# active user: databellum.ai@gmail.com

sql <- "SELECT Actor1Name, COUNT(*) AS `NumEvents` FROM `gdelt-bq.full.events` GROUP BY `Actor1Name`"

GDELTevents <- bq_table_download(bq_project_query(billing, sql), n_max = 10000)
class(GDELTevents)
head(GDELTevents)

# # Save to RDS
saveRDS(GDELTevents, "data/data_GDELT_ts.rds")

print("GDELT events extraction process FINISHED")


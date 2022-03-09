library(tidyverse)
library(openxlsx)

# -----------------------------------
# -----------------------------------
# GDELTTOOLS...
# https://cran.r-project.org/web/packages/GDELTtools/GDELTtools.pdf
# install.packages("GDELTtools")
library(GDELTtools)



df10 <- GetGDELT(start_date="2022-03-05", end_date="2022-03-07")
class(df10)
head(df10)
names(df10)
nrow(df10)
unique(df10$Actor1CountryCode)
write.xlsx(df10[1:10000,], "data/test_gdelt.xlsx")
View(df10)
names(df10)
df10







# -----------------------------------
# -----------------------------------
# BIGQUERY 
# BIGQUERY INTERACTIVO VIA WEB: https://console.cloud.google.com/bigquery?project=applied-flag-330811
# https://www.r-bloggers.com/2020/03/google-big-query-with-r/
# https://bigrquery.r-dbi.org/
# https://www.infoworld.com/article/3622926/how-to-use-r-with-bigquery.html

# install.packages("bigrquery")
library(bigrquery)

billing <- "applied-flag-330811"
bq_user()# active user: databellum.ai@gmail.com
sql <- "SELECT Actor1Name, COUNT(*) AS `NumEvents` FROM `c` GROUP BY `Actor1Name`"
obtainedData <- bq_table_download(bq_project_query(billing, sql), n_max = 10000)
class(obtainedData)
head(obtainedData)




# -----------------------------------
# -----------------------------------
#  BIG QUERY...
#
# GENERAL BIGQUERY:

# -----------------------------------
# -----------------------------------

# BIGQUERY DESDE CONSOLA:

# https://www.youtube.com/watch?v=UHc3Gsvz2Ss


# -----------------------------------
# -----------------------------------
# BIGQUERY CON R:
#
# https://www.r-bloggers.com/2020/03/google-big-query-with-r/
# https://bigrquery.r-dbi.org/
# https://www.infoworld.com/article/3622926/how-to-use-r-with-bigquery.html
library(bigrquery)
billing <- "applied-flag-330811" # replace this with your project ID 
sql <- "SELECT COUNT(*) FROM `gdelt-bq.full.events`"

tb <- bq_project_query(billing, sql)
bq_table_download(tb, n_max = 10)



library(dplyr)
con <- dbConnect(
  bigrquery::bigquery(),
  project = "applied-flag-330811",
  dataset = "gdelt-bq.full",
  billing = "applied-flag-330811"
)

dbListTables(con)
# Is it OK to cache OAuth access credentials in the folder C:/Users/bab635/AppData/Local/gargle/gargle/Cache between R sessions?
skeds <- tbl(con, "schedules")

# SELECT COUNT(*) FROM `gdelt-bq.full.events`

# -----------------------------------
# -----------------------------------
# GDELTTOOLS...

# https://cran.r-project.org/web/packages/GDELTtools/GDELTtools.pdf

# install.packages("GDELTtools")

library(GDELTtools)
df10 <- GetGDELT(start_date="2022-01-31", end_date="2022-02-02")
class(df10)
head(df10)
names(df10)
nrow(df10)
unique(df10$Actor1CountryCode)


view(df1[1,1:50])
view(df1[1,51:58])


## Not run:
df1 <- GetGDELT(start_date="2022-01-01", end_date="2022-01-03")
df2 <- GetGDELT(start_date="2022-01-01", end_date="2022-01-03",
                row_filter=ActionGeo_CountryCode=="US")
df3 <- GetGDELT(start_date="2022-01-01", end_date="2022-01-03",
                row_filter=Actor2Geo_CountryCode=="RS" & NumArticles==2 & is.na(Actor1CountryCode),
                1:5)
df4 <- GetGDELT(start_date="2022-01-01", end_date="2022-01-03",
                row_filter=Actor2Code=="COP" | Actor2Code=="MED",
                contains("date"), starts_with("actor"))
# Specify a local folder to store the downloaded files
df5 <- GetGDELT(start_date="2022-01-01", end_date="2022-01-03",
                row_filter=ActionGeo_CountryCode=="US",
                local_folder = "~/gdeltdata")




# -----------------------------------
# -----------------------------------
#  GDELTR2...


# https://asbcllc.com/blog/2017/august/intro_to_programming_with_gdeltr2/index.html
# https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/
# https://www.rdocumentation.org/packages/gdeltr2/versions/0.3.11023026

# install.packages("devtools")
# devtools::install_github("abresler/gdeltr2")

library(gdeltr2)

df_gkg <- get_gdelt_codebook_ft_api(code_book = "gkg")
test <- get_codes_gkg_themes()
class(test)

test2 <- get_data_gkg_days_summary()

test3 <-get_data_gdelt_period_event_totals()

get_clean_count_data(all_counts, extra_key = NA, count_col = "idArticle.tone", return_wide = F)

my_themes <-
  c("ECON_WORLDCURRENCIES_CHINESE_YUAN", # stories about china's currency -- god way to find stories about china's economy
    "ECON_BUBBLE", # articles about economic bubble
    "TAX_FNCACT_BROKER", # articles about brokers of things
    "ECON_HOUSING_PRICES", # articls about housing prices
    "ECON_BITCOIN", # articles about bitcoin
    "ELECTION_FRAUD", # articles about election fraud
    "SOC_POINTSOFINTEREST_GOVERNMENT_BUILDINGS", # articles about government buildings
    "WB_1277_BANKRUPTCY_AND_LIQUIDATION", # articles about bankruptcy
    "WB_639_REPRODUCTIVE_MATERNAL_AND_CHILD_HEALTH", # articles about pregnancy and child health
    "WB_2151_CHILD_DEVELOPMENT", # articles about child development
    "TAX_FNCACT_BUILDER" # articles about builders
  )

set.seed(1234)

random_themes <-
  df_gkg %>% pull(idGKGTheme) %>% sample(3)

my_themes <- 
  c(my_themes, random_themes)
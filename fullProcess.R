# PTE: Normalization (OJO los stocks sin base diaria)
# PTE: ampliar rango de fecha de la API o usar GDELT para sentiments
# PTE: decidir cómo incorporaré features categóricas (p.e. moonSum.weekday) que no están en la seed
# PTE:  estandarizar los esquemas y nomenclatura de los .RDS de datos de origen + Eliminar los .RDS "geo" y calcular a partir de los datos
# PTE: multiseed
# PTE: decidir si utilizo "popular", "recent" o "mixed" al leer de Twitter


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# INITIALIZATION
# -Environment initialization
# -Data scope to retrieve 
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ===============
# Initialize environment. Load all packages required
source("initialization/initialize.R") 

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# RAW DATA EXTRACTION
# INPUT: several internet sources, APIs, etc.
# OUTPUT: .RDS files from each source
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ===============
# KEYS
source("keys_APIs.R")
# ===============
# Data backup before new extraction
# We use a new directory named by date and time to store current .RDS files, etc. (all content)
library(stringr)
dir_from <- "data"
dir_to <- str_remove_all(paste0("dataExtracted_", as.character(Sys.time())), "[-: ]")
dir_to <- file.path("backup", dir_to)
dir.create(dir_to)
file.copy(list.files(dir_from, full.names = TRUE), 
          dir_to, 
          recursive = TRUE)
# ===============
# Data extraction from each source
source("extraction/extract_searchsGTrends.R")# Extract searches from Google Trends
source("extraction/extract_stocksPrices.R") # Extract stock prices from Yahoo Finance
source("extraction/extract_airTraffic.R")# Extract air traffic data
source("extraction/extract_indicatorsOECD.R")# Extract leading indicators from OECD
source("extraction/extract_moonSunData.R")# Extract Moon and Sun related data (phase, night hours)
source("extraction/extract_sentimentsTwitter.R")# Extract Twitter posts sentiment data
source("extraction/extract_rankingFIFA.R")# Extract from FIFA Ranking
source("extraction/extract_musicSPOTIFY.R") # Extract music trends from SPOTIFY
# ===============
# Standardize geography
# Based on all extracted data (.RDS files), we generate editable geography codes proposal and read its revised (manually edited) version
source("extraction/extract_standardizeGeography.R") # Prepare a standard geography proposal ("userEdition/standardGeography_DRAFT.xlsx") coding to mix data from disparate sources
# Now, an authorized administrator edits the draft and saves as "userEdition/standardGeography.xlsx"
# ===============
# Merge all extracted data into a single raw file, still to refine
source("extraction/mergeExtractedData.R") 



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# TRANSFORMATION
# Input: .RDS files extracted
# We transform into a single requested dataset
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ===============
# Establish what data we need to extract
# Determine what stocks, feautres, concepts and geography locations we want to extract in next phase.
# This is an extensive "raw" that will be narrowed in further phases
source("transformation/obtainSeedSpecs.R")
# ===============
# Data in .RDS files is preprocessed for use (consolidation, geography dimensioning, imputation, normalization)
source("transformation/buildDataset.R") 




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# EDA (Exploratory data analysis
# Charts and EDA actions to understand collected data
# 
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ===============
# Graphical exploration of data
source("EDA/EDA.R")




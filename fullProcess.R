# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# INITIALIZATION
# -Environment initialization
# -Data scope to retrieve 
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ===============
# INITIALIZE ENVIRONMENT
# ---------------
# Load all packages required
source("initialization/initialize.R") 

# ===============
# ESTABLISH WHAT DATA WE NEED TO EXTRACT
# ---------------
# Determine what stocks, concepts and geography locations we want to extract in next phase.
# This is an extensive "raw" that will be narrowed in further phases
source("initialization/load_extractionScope.R")






# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# RAW DATA EXTRACTION
# INPUT: several internet sources, APIs, etc.
# OUTPUT: .RDS files from each source
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ===============
# EXTRACTION
# ---------------
# Data are extracted from their original locations (using APIs, etc.) into ./data/*.RDS files
source("extraction/extract.R")

# ===============
# STANDARDIZE GEOGRAPHY
# ---------------
# Based on all extracted data (.RDS files), we generate editable geography codes proposal and read its revised (manually edited) version
source("extraction/extract_standardizeGeography.R") # Prepare a standard geography proposal ("userEdition/standardGeography_DRAFT.xlsx") coding to mix data from disparate sources
# Now, an authorized administrator edits the draft and saves as "userEdition/standardGeography.xlsx"



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# TRANSFORMATION
# Input: .RDS files extracted
# We transform into a single requested dataset
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ===============
# CATALOGS GENERATION
# ---------------
# For information purposes, we read from the .RDS files to create two catalogs of Features and Standard Geolocations
source("transformation/makeCatalogs.R") 
print("SOME FEATURES:")
print(all_features[1:50])
print("SOME DIMENSIONS:")
head(std_geo_revised)
print(std_geo_list[1:50])
# Spreadsheet files available for reference (they are just outputs) at:
# "userEdition/standardGeography.xlsx"
# "userEdition/featuresCatalog_output.xlsx"

# ===============
# Determine what specific data we need to transform
# ---------------
source("transformation/load_predictionHyphotesis.R") 

# ===============
# Data in .RDS files is preprocessed for use (consolidation, geography dimensioning, imputation, normalization)
# ---------------
source("transformation/buildDataset.R") 





# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# EDA (Exploratory data analysis
# Charts and EDA actions to understand collected data
# 
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ===============
# GRaphical exploration of data
# ---------------
source("EDA/EDA.R")




# https://cran.r-project.org/web/packages/OECD/OECD.pdf
# https://data.oecd.org/
# https://data.oecd.org/leadind/composite-leading-indicator-cli.htm
# MEI_CLI (Composite Leading Indicators (MEI))

# All datasets
dsets <- get_datasets()
head(dsets, 10)
nrow(dsets)
view(dsets)
search_dataset("Composite Leading Indicators", dsets)


# Get data example 1
browse_metadata("EPL_OV") # Get metadata of a dataset (web) 
df <- get_dataset("EPL_OV")

# Get data example 2
browse_metadata("EPL_OV") # Get metadata of a dataset (web) 
df <- get_dataset("EPL_OV", 
                  filter = list(c("DEU", "FRA"), c("EPRC_V1", "EPRC_V2")), 
                  start_time = 2008, 
                  end_time = 2010)
head(df, 10)

# Get data example 3
browse_metadata("PATS_REGION")
df <- get_dataset("PATS_REGION", 
                  filter = "PCT_A.INVENTORS.BEL+BE10+BE21.TOTAL+BIOTECH+ICT", 
                  start_time = 2008, 
                  end_time = 2010, 
                  pre_formatted = TRUE)
head(df, 10)  

# Get data example 4
browse_metadata("MEI_CLI") # Get metadata of a dataset (web) 
df <- get_dataset("MEI_CLI", 
                  filter = list(c("DEU", "FRA"), c("EPRC_V1", "EPRC_V2")), 
                  start_time = 2008, 
                  end_time = 2010)
head(df, 10)


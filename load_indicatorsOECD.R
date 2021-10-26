# https://data.oecd.org/leadind/composite-leading-indicator-cli.htm
# MEI_CLI (Composite Leading Indicators (MEI)) | https://www.oecd.org/sdd/leading-indicators/41629509.pdf
# https://fgeerolf.com/data/oecd/index.html | https://fgeerolf.com/data/oecd/MEI_CLI.html
# https://stats.oecd.org/Index.aspx?DataSetCode=MEI_CLI
# Inicators:
# LOLITOAA:	Amplitude adjusted (CLI)
# BSCICP03: Standardised, amplitude adjusted (Long term average=100) Business Confidence Indicator (BCI)
# CSCICP03: Standardised, amplitude adjusted (Long term average=100), Consumer Confidence Indicator (CCI)



# Collect countries (not online updated. See: https://fgeerolf.com/data/oecd/MEI_CLI.html)
# Load RDS:
countries_OECD <- readRDS("./data/geo_OECD.rds")
head(countries_OECD)

# ===========================
# Historical data (1960-2020) calculation
# Let's add historical data archived: EVENTUAL extraction of data 1960-2020: not necessary to run query, instead, data are read from archived .RDS
leadingIndicatorsOECD_1960_2020 <- readRDS("./data/leadingIndicatorsOECD_1960_2020.rds")
# # Extract data from OECD
# leadingIndicatorsOECD_1960_2020 <- get_dataset("MEI_CLI", 
#                                      filter = list(c("LOLITOAA", "BSCICP03", "CSCICP03"), "",c("M")), 
#                                      start_time = "1960", 
#                                      end_time = "2020")
# leadingIndicatorsOECD_1960_2020 <- leadingIndicatorsOECD %>% 
#   mutate(obsDate = paste(obsTime, "-15", sep = "")) %>% 
#   select(Date = obsDate,
#          Indicator = SUBJECT, 
#          Country = LOCATION, 
#          Value = obsValue)
# leadingIndicatorsOECD_1960_2020 <- leadingIndicatorsOECD
# leadingIndicatorsOECD_1960_2020
# # Save to RDS
# saveRDS(leadingIndicatorsOECD_1960_2020, "./data/leadingIndicatorsOECD_1960_2020.rds")
# ===========================

# Extract fresh data (>=2021) from OECD
leadingIndicatorsOECD <- get_dataset("MEI_CLI", 
                                     filter = list(c("LOLITOAA", "BSCICP03", "CSCICP03"), "",c("M")), 
                                     start_time = selected_initial_year_OECD, 
                                     end_time = selected_end_year_OECD)
leadingIndicatorsOECD <- leadingIndicatorsOECD %>% 
  mutate(obsDate = paste(obsTime, "-15", sep = "")) %>% 
  select(Date = obsDate,
         Indicator = SUBJECT, 
         Country = LOCATION, 
         Value = obsValue)

# Now we join current (fresh) and historical data:
leadingIndicatorsOECD <- rbind(leadingIndicatorsOECD, leadingIndicatorsOECD_1960_2020)
head(leadingIndicatorsOECD)

# Final makeup (rename indicators, spread)
leadingIndicatorsOECD <- leadingIndicatorsOECD %>% 
  mutate(Indicator = replace(Indicator, Indicator == "LOLITOAA", "OECD_CLI"), 
         Indicator = replace(Indicator, Indicator == "BSCICP03", "OECD_BCI"), 
         Indicator = replace(Indicator, Indicator == "CSCICP03", "OECD_CCI")) %>% 
  spread(key = Indicator, value = Value)

# Chart
leadingIndicatorsOECD %>%
  ggplot(aes(as_date(Date), OECD_CLI, color=Country)) + geom_line()

# Save to RDS
saveRDS(countries_OECD, "./data/geo_OECD.rds") # Countries master table
saveRDS(leadingIndicatorsOECD, "./data/data_OECD.rds") # Values indicators

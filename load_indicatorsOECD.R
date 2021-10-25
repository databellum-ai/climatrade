# PTE: Extraer tabla de Country/Location de OECD
# PTE: Comprobar datos extraídos contra un gráfico


# https://cran.r-project.org/web/packages/OECD/OECD.pdf
# https://data.oecd.org/
# https://data.oecd.org/leadind/composite-leading-indicator-cli.htm
# MEI_CLI (Composite Leading Indicators (MEI)) | https://www.oecd.org/sdd/leading-indicators/41629509.pdf | https://fgeerolf.com/data/oecd/MEI_CLI.html
# https://fgeerolf.com/data/oecd/index.html | https://stats.oecd.org/Index.aspx?DataSetCode=MEI_CLI
# Some useful functions from OECD:
dsets <- get_datasets()
head(dsets, 10)
view(dsets)
search_dataset("Composite Leading Indicators", dsets)


selected_initial_year_OECD <- "2018"
selected_end_year_OECD <-as.character(year(Sys.Date()))

browse_metadata("MEI_CLI") # Get metadata of a dataset (web) 
# https://fgeerolf.com/data/oecd/MEI_CLI.html
# LOLITOAA:	Amplitude adjusted (CLI)
# BSCICP03: Standardised, amplitude adjusted (Long term average=100) Business Confidence Indicator (BCI)
# CSCICP03: Standardised, amplitude adjusted (Long term average=100), Consumer Confidence Indicator (CCI)
leadingIndicatorsOECD <- get_dataset("MEI_CLI", filter = list(c("LOLITOAA", "BSCICP03", "CSCICP03"), "",c("M")), 
                  start_time = selected_initial_year_OECD, 
                  end_time = selected_end_year_OECD)
leadingIndicatorsOECD <- leadingIndicatorsOECD %>% 
  mutate(obsDate = paste(obsTime, "-15", sep = "")) %>% 
  select(Indicator = SUBJECT, 
         Country = LOCATION, 
         obsDate, 
         obsValue)

leadingIndicatorsOECD %>%
  filter(Country %in% c("ESP"),
         Indicator == "BSCICP03") %>%
  group_by(Indicator, obsDate) %>% summarize(valor = mean(obsValue)) %>% 
  ggplot(aes(x = obsDate, y = obsValue)() + geom_line())

head(leadingIndicatorsOECD)
unique(leadingIndicatorsOECD$obsDate)



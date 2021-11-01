# ================================
# Utilities, tools
# ================================
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidyquant)) install.packages("tidyquant", repos = "http://cran.us.r-project.org")
library(tidyquant)
library(ggplot2)
library(tidyverse)

# ================================
# Retrieve all holding/stocks in a given Index/Exchange
# ================================
tq_exchange_options() # To see available Exchanges
chosenExchange <- "NYSE"
myExchangeCompanies <- tq_exchange(chosenExchange) # tq-exchange() allows users to read in all the names for a given exchange.
head(myExchangeCompanies)
nrow(myExchangeCompanies)

tq_index_options() # To see available Indexes
chosenIndex <- "SP500"
myIndexCompanies <- tq_index(chosenIndex) # All holdings in a index
head(myIndexCompanies)
nrow(myIndexCompanies)
# ==================
# ==================


# ==================================================
# Scraping example (to retrieve SP500 tickers from Wikipedia)
# ==================================================
# Load libraries required for scraping web page 
library(rvest)
library(janitor)
# get the URL for the wikipedia page with all SP500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the SP500 table using rvest
tickers <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # one way to get table
  #html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  # easier way to get table
  html_nodes(xpath = '//*[@id="constituents"]') %>% 
  html_table()
#create a vector of tickers
sp500tickers <- tickers[[1]]
sp500tickers = sp500tickers %>% 
  mutate(Symbol = case_when(Symbol == "BRK.B" ~ "BRK-B", Symbol == "BF.B" ~ "BF-B", TRUE ~ as.character(Symbol)))
head(sp500tickers)
# ==================
# ==================

# ================================
# Utilities, tools
# ================================

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


# ==================================================
# Extended list of relevant cities:
# ==================================================
# relevantCities <- data.frame(
#   id = c("NewYork", "Oviedo", "Amsterdam", "Paris", "HongKong", "Chicago", "London", "LosAngeles", "Beijng", "Madrid", "Albacete", "Frankfurt", "Tokyo", "Seoul", "Singapore", "Dubai"),
#   latitude = c(40.70623940806975, 43.3620683921967, 52.37288021193839, 48.8613182352403, 22.32029644568666, 41.88632188372439, 51.50702741724013, 34.05084926622552, 39.905384001792335, 40.425619645599916, 39.267266932791685, 50.12095925753092, 35.687667406759765, 37.568428507484775, 1.2929342653888358, 25.214919588761404),
#   longitude = c(-74.00883633105707, -5.84817121485434, 4.896615844580131, 2.3003412809927495, 114.19091287611904, -87.67086062661967, -0.12701173276875632, -118.25389861960555, 116.37699181234836, -3.7025627487487984, -1.5500112927257998, 8.637929689001126, 139.76554769212072, 126.9780904050825, 103.84642630591777, 55.2762538818061))

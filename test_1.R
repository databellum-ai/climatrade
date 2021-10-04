install.packages("quantmod")

library(tidyquant)

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

# Downloading Apple price using quantmod

getSymbols("AAPL", from = '2017-01-01',
           to = "2018-03-01",warnings = FALSE,
           auto.assign = TRUE)

head(AAPL)

class(AAPL)

chart_Series(AAPL)

chart_Series(AAPL['2017-12/2018-03'])

tickers = c("AAPL", "NFLX", "AMZN", "K", "O")

getSymbols(tickers,
           from = "2017-01-01",
           to = "2017-01-15")


prices <- map(tickers,function(x) Ad(get(x)))
prices <- reduce(prices,merge)
colnames(prices) <- tickers
head(prices)

class(prices)


aapl <- tq_get('AAPL',
               from = "2017-01-01",
               to = "2018-03-01",
               get = "stock.prices")
head(aapl)

class(aapl)

aapl %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  theme_classic() +
  labs(x = 'Date',
       y = "Adjusted Price",
       title = "Apple price chart") +
  scale_y_continuous(breaks = seq(0,300,10))


tickers = c("AAPL", "NFLX", "AMZN", "K", "O")

prices <- tq_get(tickers,
                 from = "2017-01-01",
                 to = "2017-03-01",
                 get = "stock.prices")
head(prices)

prices %>%
  group_by(symbol) %>%
  slice(1)

prices %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line()

# ================================
# This markdown will serve as a brief tutorial for reading in finance data. Luckily, the package tidyquant offers the ability to interact directly with serval APIs in order to access up to date stock info. Goals:
# 1) Acquire finance data
# 2) Make a tidy data set
# 3) Write it out
# The ‘tidyquant’ library was developed to turn the quantmod and xts package outputs into a tidier format.

#install.packages("tidyquant")
library(tidyquant)
library(ggplot2)

# Let’s pull Yahoo Finance data from a single stock. You must know the correct abbreviation for that stock.
google <- tq_get(x = "GOOG")
names(google)

# The default of tq_get() grabs the date, volume, opening, highest, lowest, closing, and adjusted price. These data come from Yahoo Finanace. This line is the same as quantmod::getSymbols(), but data are returned in tibble (tbl_df) format.
# 
# There’s many more options for the types of finance data to read in. All options accessed by the following function.

tq_get_options()

# The description for each of these can be found here:

?tq_get

# Putting a question mark in front of a function will bring up the help file in the bottom right tile. Take a look at the other types of data to read in. The options and sources for the “get =” argument are outlined in the help file. For example, “stock.prices”, “stock.prices.japan”, “key.stats”, and “dividends” are sourced from Yahoo Finance.
# 
# More potentially helpful arguments are “from = YYYY-MM-DD” and “to = YYYY-MM-DD”, which allow the user to customize the range of data. Each row read in represents a day of stats.
# 
# tq-exchange() allows users to read in all the names for a given exhange.

tq_exchange_options()

nyse <- tq_exchange("NYSE")

# We can combine these past two functions to read in longitudinal data for a set of stock symbols.

full <- tq_get(x = nyse$symbol[1], get = "stock.prices") %>% add_column(symbol = nyse$symbol[1])
for (s in nyse$symbol[2:20]){
  single <- try(tq_get(x = s, get = "stock.prices") %>% add_column(symbol = s))
  full <- try(rbind(full, single))
}


# This loop got us 10 yrs of daily price data from the first 20 listed names in the NYSE.
# Let’s look at the structure of this data set

str(full)

# Dates are their own class, and the labels for symbols are character strings. All numeric information is of type numeric.
# Let’s look at all the different stocks in here. Obviously, you can change which stocks you pull from the NYSE depending on your personal portfolio.
unique(full$symbol)

# Here’s our company names.
# The way in which this data set is structured reflects the main principle of tidy data: each row is a unique observation. In our case, each row refers to a day’s worth of stock prices for a single company.
# Given the tidy structure, we can easily plot these data

ggplot(data = full, aes(x = date, y = adjusted, color = symbol)) + 
  geom_line(aes(group = symbol)) + 
  theme_classic() + 
  theme(legend.position = "none") + 
  ylab("Adjusted Price") + 
  ggtitle("10 Years of NYSE Stock Prices")

# Lastly, we will write out this data set in two different formats. The first is csv, which can be opened with Excel and is compatible with many programs. Before writing any files, it’s a good idea to check the working directory again just to make sure.

getwd() 
write.csv(full, file = "nyse_stock_prices.csv")
# The second format is “.RData” which takes up less space in comparison to csv.

save(full, file = "nyse_stock_prices.RData")


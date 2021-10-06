# ================================
# https://www.codingfinance.com/post/2018-03-27-download-price/
# ================================
install.packages("tidyquant")
install.packages("tidyverse")


library(tidyquant)
library(ggplot2)
library(tidyverse)

tickers = c("AAPL", 
            "NFLX", 
            "AMZN", 
            "K", 
            "O")

prices <- tq_get(tickers,
                 from = "2017-01-01",
                 to = "2017-03-01",
                 get = "stock.prices")
head(prices)
class(prices)

prices %>%
  group_by(symbol) %>%
  slice(1)

prices %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line()

prices %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line() +
  facet_wrap(~symbol,scales = 'free_y') +
  theme_classic() +
  labs(x = 'Date',
       y = "Adjusted Price",
       title = "Price Chart") +
  scale_x_date(date_breaks = "month",
               date_labels = "%b\n%y")


# ================================
# https://quantdev.ssri.psu.edu/tutorials/obtaining-finance-data-tidyquant
# ================================
# Let’s pull Yahoo Finance data from a single stock.
google <- tq_get(x = "GOOG")
names(google)

# There’s many more options for the types of finance data to read in. All options accessed by the following function.
tq_get_options()
# The description for each of these can be found here:
?tq_get

# tq-exchange() allows users to read in all the names for a given exchange.
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



source("10_initialize.R")

library(tidyverse)

df_planetMood <- readRDS("data/df_planetMood.rds")  # Load dataset for analysis
names(df_planetMood)

# https://www.educba.com/arima-model-in-r/
# https://rpubs.com/riazakhan94/arima_with_example

data <- df_planetMood[,c("date","VIX")] %>% arrange(date)
data = ts(data[,2],start = c(2017,1),frequency = 7)
plot(data, xlab='Year on sale', ylab = 'Number of Textile sold')

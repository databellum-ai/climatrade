
# RNN
# https://blogs.rstudio.com/ai/posts/2017-12-20-time-series-forecasting-with-recurrent-neural-networks/
# install.packages("keras")

dir.create("~/Downloads/jena_climate", recursive = TRUE)
download.file(
  "https://s3.amazonaws.com/keras-datasets/jena_climate_2009_2016.csv.zip",
  "~/Downloads/jena_climate/jena_climate_2009_2016.csv.zip"
)
unzip(
  "~/Downloads/jena_climate/jena_climate_2009_2016.csv.zip",
  exdir = "~/Downloads/jena_climate"
)

library(tibble)
library(readr)

data_dir <- "~/Downloads/jena_climate"
fname <- file.path(data_dir, "jena_climate_2009_2016.csv")
data <- read_csv(fname)
glimpse(data)
head(data)

library(ggplot2)
ggplot(data, aes(x = 1:nrow(data), y = `T (degC)`)) + geom_line()

# -----------
sequence_generator <- function(start) {
  value <- start - 1
  function() {
    value <<- value + 1
    value
  }
}
gen <- sequence_generator(10)
gen()
# -----------

head(data)
# remove date column
data <- data.matrix(data[,-1])
# scale
train_data <- data[1:200000,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)
head(data)

# generator
generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }           
    list(samples, targets)
  }
}
# Now, let’s use the abstract generator function to instantiate three generators: one for training, one for validation, and one for testing
lookback <- 1440
step <- 6
delay <- 144
batch_size <- 128

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 200000,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 200001,
  max_index = 300000,
  step = step,
  batch_size = batch_size
)

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 300001,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- (300000 - 200001 - lookback) / batch_size

# How many steps to draw from test_gen in order to see the entire test set
test_steps <- (nrow(data) - 300001 - lookback) / batch_size



library(keras)
evaluate_naive_method <- function() {
  batch_maes <- c()
  for (step in 1:val_steps) {
    c(samples, targets) %<-% val_gen()
    preds <- samples[,dim(samples)[[2]],2]
    mae <- mean(abs(preds - targets))
    batch_maes <- c(batch_maes, mae)
  }
  print(mean(batch_maes))
}

evaluate_naive_method()
celsius_mae <- 0.29 * std[[2]]
celsius_mae


# A basic machine-learning approach
# install.packages("tensorflow")
library(keras)

model <- keras_model_sequential() %>% 
  layer_flatten(input_shape = c(lookback / step, dim(data)[-1])) %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)


# ===============================================
# ===============================================
# Time Series Forecasting with GRNN in R: the tsfgrnn Package
# https://cran.r-project.org/web/packages/tsfgrnn/vignettes/tsfgrnn.html

# install.packages("tsfgrnn")
library(tsfgrnn)
pred <- grnn_forecasting(UKgas, h = 4)
pred$prediction
plot(pred)
library(ggplot2)
autoplot(pred)

pred <- grnn_forecasting(timeS = 1:10, h = 2, lags = c(1, 3), msas = "MIMO", transform = "none")
grnn_examples(pred)
grnn_weights(pred)
summary(pred)
library(ggplot2)
plot_example(pred, 1)
plot_example(pred, 4)
predr <- grnn_forecasting(1:10, h = 2, lags = c(1, 3), msas = "recursive", transform = "none")
predr$prediction
grnn_examples(predr)
plot_example(predr, position = 1, h = 1)
grnn_weights(predr)[[1]]
plot_example(predr, position = 1, h = 2)
grnn_weights(predr)[[2]]

pred <- grnn_forecasting(ts(1:20), h = 4, lags = 1:2)
ro <- rolling_origin(pred, h = 4)
print(ro$test_sets)
print(ro$predictions)
print(ro$errors)
ro$global_accu
ro$h_accu
plot(ro, h = 4)
ro <- rolling_origin(pred, h = 4, rolling = FALSE)
print(ro$test_sets)

pred <- grnn_forecasting(USAccDeaths, h = 12, lags = 1:12, sigma = 100)
plot(pred)
pred <- grnn_forecasting(USAccDeaths, h = 12, lags = 1:12, sigma = 0.05)
plot(pred)

set.seed(5)
timeS <- ts(1:10 + rnorm(10, 0, .2))
pred <- grnn_forecasting(timeS, h = 3, transform = "none")
plot(pred)
pred2 <- grnn_forecasting(timeS, h = 3, transform = "additive")
plot(pred2)


# ===============================================
# ===============================================
# Multivariate Time Series Forecasting with Deep Learning
# https://towardsdatascience.com/multivariate-time-series-forecasting-with-deep-learning-3e7b3e2d2bcf



# ===============================================
# ===============================================
# https://www.r-bloggers.com/2020/12/bayesian-forecasting-for-uni-multivariate-time-series/

# 1 – univariate time serieslibrary(datasets)
plot(Nile)
Nile
head(Nile)

X <- matrix(Nile, ncol=1)
index_train <- 1:floor(nrow(X)*0.8)
X_train <- matrix(X[index_train, ], ncol=1)
X_test <- matrix(X[-index_train, ], ncol=1)

obj <- nnetsauce::sklearn$linear_model$BayesianRidge()
print(obj$get_params())

fit_obj <- nnetsauce::MTS(obj = obj) 
fit_obj$fit(X_train)
preds <- fit_obj$predict(h = nrow(X_test), level=95L, return_std=TRUE)

n_test <- nrow(X_test)
xx <- c(1:n_test, n_test:1)
yy <- c(preds$lower, rev(preds$upper))
plot(1:n_test, drop(X_test), type='l', main="Nile",
     ylim = c(500, 1200))
polygon(xx, yy, col = "gray", border = "gray")
points(1:n_test, drop(X_test), pch=19)
lines(1:n_test, drop(X_test))
lines(1:n_test, drop(preds$mean), col="blue", lwd=2)


# -----------
# 2 - multivariate time series
# library(devtools)
# devtools::install_github("Techtonique/nnetsauce/R-package")

library(fpp)
plot(fpp::usconsumption)                         

X <- as.matrix(fpp::usconsumption)
index_train <- 1:floor(nrow(X)*0.8)
X_train <- X[index_train, ]
X_test <- X[-index_train, ]

obj <- nnetsauce::sklearn$linear_model$BayesianRidge()
fit_obj2 <- nnetsauce::MTS(obj = obj)

fit_obj2$fit(X_train)
preds <- fit_obj2$predict(h = nrow(X_test), level=95L,
                          return_std=TRUE) # standardize output+#plot against X_test


n_test <- nrow(X_test)

xx <- c(1:n_test, n_test:1)
yy <- c(preds$lower[,1], rev(preds$upper[,1]))
yy2 <- c(preds$lower[,2], rev(preds$upper[,2]))

par(mfrow=c(1, 2))
# 95% credible intervals
plot(1:n_test, X_test[,1], type='l', ylim=c(-2.5, 3),
     main="consumption")
polygon(xx, yy, col = "gray", border = "gray")
points(1:n_test, X_test[,1], pch=19)
lines(1:n_test, X_test[,1])
lines(1:n_test, preds$mean[,1], col="blue", lwd=2)

plot(1:n_test, X_test[,2], type='l', ylim=c(-2.5, 3),
     main="income")
polygon(xx, yy2, col = "gray", border = "gray")
points(1:n_test, X_test[,2], pch=19)
lines(1:n_test, X_test[,2])
lines(1:n_test, preds$mean[,2], col="blue", lwd=2)


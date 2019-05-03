# Holdback ####

library(reshape2)
library(forecast)

hb_data <- read.csv("capped_data.csv")
hb_data <- select(hb_data, ID, Date, Weekly_Sales)
hb_data$Transform <- log(hb_data$Weekly_Sales)

hb_dates <- hb_data$Date[1:130]
hb_train <- filter(hb_data, Date %in% hb_dates)
hb_test <- filter(hb_data, !Date %in% hb_dates)

hb_data_ts <- ts(dcast(hb_train, Date~ID), frequency = 52)
hb_data_ts <- hb_data_ts[,-1]

hb_arima <- lapply(hb_data_ts, function(x) stlf(x, method="arima", h=13, stepwise=FALSE, approx=FALSE))
hb_ets <- lapply(hb_data_ts, function(x) stlf(x, method="ets", h=13))
hb_naive <- lapply(hb_data_ts, function(x) stlf(x, method="naive", h=13))

# Holdback Analysis.R ####
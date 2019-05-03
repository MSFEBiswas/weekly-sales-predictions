data <- read.csv("data+features.csv")
data <- na.omit(data)

valid_ID <- read.csv("num_of_data.csv")
capped_data <- fun_negative_cap(data)
number <- sample(1:2660, 1, replace=TRUE)
list <- fun_random_ml(capped_data, valid_ID, num = number, d = "gaussian")

mean(list$GBM$train$MAPE)
mean(list$GBM$test$MAPE)
mean(list$RF$train$MAPE)
mean(list$RF$test$MAPE)

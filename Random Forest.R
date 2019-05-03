# Random Forest ####

library(randomForest)

data <- read.csv("data+features.csv")
data <- na.omit(data)
valid_ID <- read.csv("valid_ID.csv")
data <- filter(data, ID %in% valid_ID$ID)

index <- sample(1:nrow(data), round(0.8*nrow(data)))
train_set <- data[index,]
test_set <- data[-index,]
train_final <- select(train_set, ID, Date, Weekly_Sales)
test_final <- select(test_set, ID, Date, Weekly_Sales)

headers <- names(data)
rf_formula <- as.formula(paste("Weekly_Sales~", paste(headers[!headers %in% c("X", "ID", "Date", "Weekly_Sales")], collapse = "+")))

count_train <- 0
count_test <- 0

for (i in 1:2660) {
  print(i)
  rf_train <- filter(train_set, ID %in% valid_ID$ID[i])
  rf_test <- filter(test_set, ID %in% valid_ID$ID[i])
  rf_fit <- train(rf_formula, rf_train, method = "rf", n.trees = 100)
  predict_train <- predict(rf_fit, rf_train, n.trees = 100)
  predict_test <- predict(rf_fit, rf_test, n.trees = 100)
  MAPE_train <- 100*abs(rf_train$Weekly_Sales - predict_train)/rf_train$Weekly_Sales
  MAPE_test <- 100*abs(rf_test$Weekly_Sales - predict_test)/rf_test$Weekly_Sales
  for (j in 1:nrow(rf_train)) {
    train_final$Forecast[count_train+j] = predict_train[j]
    train_final$MAPE[count_train+j] = MAPE_train[j]
  }
  for (j in 1:nrow(rf_test)) {
    test_final$Forecast[count_test+j] = predict_test[j]
    test_final$MAPE[count_test+j] = MAPE_test[j]
  }
  count_train <- count_train + nrow(rf_train)
  count_test <- count_test + nrow(rf_test)
}
mean(train_final$MAPE)
mean(test_final$MAPE)

MAPE_train <- data.table(train_final)
MAPE_train <- MAPE_train[,list(mean_error=mean(MAPE), sd_error=sd(MAPE)), by=ID]

MAPE_test <- data.table(test_final)
MAPE_test <- MAPE_test[,list(mean_error=mean(MAPE), sd_error=sd(MAPE)), by=ID]

write.csv(train_final, "rf_train.csv")
write.csv(test_final, "rf_test.csv")

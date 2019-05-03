# Holdback Analysis.R ####

valid_ID <- read.csv("valid_ID.csv")

df_hb_arima <- data.frame(t(sapply(hb_arima, c)))
df_hb_ets <- data.frame(t(sapply(hb_ets, c)))
df_hb_naive <- data.frame(t(sapply(hb_naive, c)))

in_time <- matrix(ncol = 4)
out_of_time <- matrix(ncol = 4)
colnames(in_time) <- c("ID", "ARIMA", "ETS", "Naive")
colnames(out_of_time) <- colnames(in_time)
ID_dummy <- valid_ID$ID

a1 <- df_hb_arima$fitted
b1 <- df_hb_ets$fitted
c1 <- df_hb_naive$fitted

a2 <- df_hb_arima$mean
b2 <- df_hb_ets$mean
c2 <- df_hb_naive$mean

for (i in 1:2660) {
  for (j in 1:130) {
    dummy <- data.frame(ID=ID_dummy[i], ARIMA=exp(a1[[i]][[j]]), ETS=exp(b1[[i]][[j]]), Naive=exp(c1[[i]][[j]]))
    in_time <- rbind(in_time, dummy)
  }
  for (j in 1:13) {
    dummy <- data.frame(ID=ID_dummy[i], ARIMA=exp(a2[[i]][[j]]), ETS=exp(b2[[i]][[j]]), Naive=exp(c2[[i]][[j]]))
    out_of_time <- rbind(out_of_time, dummy)
  }
}
in_time <- in_time[-1,]
out_of_time <- out_of_time[-1,]

hb_train <- select(hb_train, ID, Date, Weekly_Sales)
hb_train <- arrange(hb_train, ID, Date)
hb_test <- select(hb_test, ID, Date, Weekly_Sales)
hb_test <- arrange(hb_test, ID, Date)

hb_train$ARIMA <- in_time$ARIMA
hb_train$ETS <- in_time$ETS
hb_train$Naive <- in_time$Naive

hb_test$ARIMA <- out_of_time$ARIMA
hb_test$ETS <- out_of_time$ETS
hb_test$Naive <- out_of_time$Naive

hb_train$MAPE_ARIMA <- abs((hb_train$ARIMA - hb_train$Weekly_Sales)/hb_train$Weekly_Sales)*100
hb_train$MAPE_ETS <- abs((hb_train$ETS - hb_train$Weekly_Sales)/hb_train$Weekly_Sales)*100
hb_train$MAPE_Naive <- abs((hb_train$Naive - hb_train$Weekly_Sales)/hb_train$Weekly_Sales)*100

hb_test$MAPE_ARIMA <- abs((hb_test$ARIMA - hb_test$Weekly_Sales)/hb_test$Weekly_Sales)*100
hb_test$MAPE_ETS <- abs((hb_test$ETS - hb_test$Weekly_Sales)/hb_test$Weekly_Sales)*100
hb_test$MAPE_Naive <- abs((hb_test$Naive - hb_test$Weekly_Sales)/hb_test$Weekly_Sales)*100

mean(hb_train$MAPE_ARIMA)
mean(hb_train$MAPE_ETS)
mean(na.omit(hb_train$MAPE_Naive))

mean(hb_test$MAPE_ARIMA)
mean(hb_test$MAPE_ETS)
mean(na.omit(hb_test$MAPE_Naive))

write.csv(hb_train, "hb_in_time.csv")
write.csv(hb_test, "hb_out_of_time.csv")
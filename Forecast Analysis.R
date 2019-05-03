# Forecast Analysis ####

fcast_analysis <- select(capped_data, ID, Date, Weekly_Sales)
fcast_analysis <- arrange(fcast_analysis, ID)

df_arima <- data.frame(t(sapply(mod_arima,c)))
df_ets <- data.frame(t(sapply(mod_ets,c)))
df_naive <- data.frame(t(sapply(mod_naive,c)))
df_auto_arima <- data.frame(t(sapply(fcast_auto_arima,c)))

forecast_dummy <- matrix(ncol = 5)
colnames(forecast_dummy) <- c("ID", "ARIMA", "ETS", "Naive", "Auto_ARIMA")
valid_ID <- arrange(valid_ID, ID)
ID_dummy <- valid_ID$ID

a <- df_arima$fitted
b <- df_ets$fitted
c <- df_naive$fitted
d <- df_auto_arima$fitted

for (i in 1:2660) {
  for (j in 1:143) {
    dummy <- data.frame(ID=ID_dummy[i], ARIMA=exp(a[[i]][[j]]), ETS=exp(b[[i]][[j]]), Naive=exp(c[[i]][[j]]), Auto_ARIMA=exp(d[[i]][[j]]))
    forecast_dummy <- rbind(forecast_dummy, dummy)
  }
}
forecast_dummy <- forecast_dummy[-1,]

fcast_analysis$ARIMA <- forecast_dummy$ARIMA
fcast_analysis$ETS <- forecast_dummy$ETS
fcast_analysis$Naive <- forecast_dummy$Naive
fcast_analysis$Auto_ARIMA <- forecast_dummy$Auto_ARIMA

fcast_analysis$MAPE_ARIMA <- 100*abs(fcast_analysis$ARIMA - fcast_analysis$Weekly_Sales)/fcast_analysis$Weekly_Sales
fcast_analysis$MAPE_ETS <- 100*abs(fcast_analysis$ETS - fcast_analysis$Weekly_Sales)/fcast_analysis$Weekly_Sales
fcast_analysis$MAPE_Naive <- 100*abs(fcast_analysis$Naive - fcast_analysis$Weekly_Sales)/fcast_analysis$Weekly_Sales
fcast_analysis$MAPE_Auto_ARIMA <- 100*abs(fcast_analysis$Auto_ARIMA - fcast_analysis$Weekly_Sales)/fcast_analysis$Weekly_Sales

mean(fcast_analysis$MAPE_ARIMA)
mean(fcast_analysis$MAPE_ETS)
mean(na.omit(fcast_analysis$MAPE_Naive))
mean(fcast_analysis$MAPE_Auto_ARIMA)

MAPE_table <- data.table(fcast_analysis)
MAPE_table <- MAPE_table[,list(mean_error_ARIMA=mean(MAPE_ARIMA), mean_error_ETS=mean(MAPE_ETS), mean_error_Naive=mean(na.omit(MAPE_Naive)), mean_error_Auto_ARIMA=mean(MAPE_Auto_ARIMA)), by=ID]

write.csv(fcast_analysis, "forecast_analysis.csv")

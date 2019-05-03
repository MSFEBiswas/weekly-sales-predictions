# Capping ####

# NA Capping ####

fun_na_cap <- function(df.data, df.valid) {
  condition <- df.valid$ID
  df.data <- filter(df.data, ID %in% df.valid$ID)
}

# Negative Value Capping ####

fun_negative_cap <- function(df.data) {
  for (i in 2:(nrow(df.data)-1)) {
    if (df.data$Weekly_Sales[i] < 10) {
      df.data$Weekly_Sales[i] = 0.5*(df.data$Weekly_Sales[i-1]+df.data$Weekly_Sales[i+1])
    }
  }
  return(df.data)
}

# Quantile Capping ####

fun_quantile_cap <- function(df.data, df.valid) {
  df.quantile <- data.table(df.data)
  df.quantile <- df.quantile[, list(min=min(Weekly_Sales), P1=quantile(Weekly_Sales, 0.01), P5=quantile(Weekly_Sales, 0.05), P10=quantile(Weekly_Sales, 0.1), P25=quantile(Weekly_Sales, 0.25), P50=quantile(Weekly_Sales, 0.5), P90=quantile(Weekly_Sales, 0.9), P95=quantile(Weekly_Sales, 0.95), P99=quantile(Weekly_Sales, 0.99), max=max(Weekly_Sales)), by=ID]
  df.capped <- df.data
  
  for (i in 1:nrow(df.valid)) {
    for (j in 1:(nrow(data)/nrow(valid_ID))) {
      if (df.data$Weekly_Sales[(nrow(data)/nrow(valid_ID))*(i-1)+j] > df.quantile$P99[i]) {
        df.capped$Weekly_Sales[(nrow(data)/nrow(valid_ID))*(i-1)+j] = df.quantile$P99[i]
      }
      if (df.data$Weekly_Sales[(nrow(data)/nrow(valid_ID))*(i-1)+j] < df.quantile$P1[i]) {
        df.capped$Weekly_Sales[(nrow(data)/nrow(valid_ID))*(i-1)+j] = df.quantile$P1[i]
      }
    }
  }
  
  df.capped <- df.capped[,-1]
  return(df.capped)
}

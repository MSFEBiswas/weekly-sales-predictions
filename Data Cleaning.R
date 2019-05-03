# Data Cleaning ####

# Number of Data Points in each ID ####

fun_numdata <- function(df.data) {
  check_df <- select(df.data, ID, Weekly_Sales)
  count = 1
  num_of_data <- matrix(ncol = 2)
  colnames(num_of_data) <- c("ID", "Number")
  for (i in 1:(nrow(check_df)-1)) {
    if (check_df$ID[i] == check_df$ID[i+1]) {
      count = count + 1
    }
    else {
      dummy <- data.frame(ID=check_df$ID[i], Number=count)
      num_of_data <- rbind(num_of_data, dummy)
      count = 1
    }
  }
  dummy <- data.frame(ID=check_df$ID[nrow(check_df)], Number=count)
  num_of_data <- rbind(num_of_data, dummy)
  num_of_data <- data.frame(num_of_data[-1,])
  return(num_of_data)
}

# Valid ID ####

fun_valid <- function(df.numdata) {
  valid_ID <- matrix(ncol = 2)
  colnames(valid_ID) <- c("ID", "Number")
  for (i in 1:nrow(df.numdata)) {
    if (df.numdata$Number[i] == max(df.numdata$Number)) {
      dummy <- data.frame(ID=df.numdata$ID[i], Number=df.numdata$Number[i])
      valid_ID <- rbind(valid_ID, dummy)
    }
  }
  valid_ID <- valid_ID[-1,]
  return(valid_ID)
}

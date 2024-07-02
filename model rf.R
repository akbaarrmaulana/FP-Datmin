# Load necessary libraries
library(dplyr)
library(caret)
library(randomForest)

# Read the data
df <- read.csv("Hotel Reservations.csv")
dim(df)
df <- cbind(df[, 1:11], date = as.Date(paste(df$arrival_date, df$arrival_month, df$arrival_year, sep="-"), format="%d-%m-%Y"), df[, 12:ncol(df)])
head(df["date"])
##mengecek data null pada kolom date
subset(df,is.na(date),c(arrival_year,arrival_month, arrival_date, date))
##data 29 Februari 2018 tidak ada pada kalendar, maka data dihapus dari original dataset
df <- df[complete.cases(data$date), ]
dim(df)
# Apply label encoding
columns_to_encode <- c('market_segment_type', 'type_of_meal_plan', 'room_type_reserved', 'booking_status')

for (column in columns_to_encode) {
  data[[column]] <- as.numeric(factor(df[[column]]))
}

# Ensure the target variable is a factor
df$booking_status <- as.factor(df$booking_status)

# Split data into features (X) and target (y)
X <- df %>% select(-Booking_ID, -booking_status, -arrival_date, -arrival_month, -arrival_year)
y <- df$booking_status

# Combine the selected features and target for modeling
data_model <- data.frame(X, booking_status = y)
dim(data_model)
# Split the data into training and testing sets
set.seed(42)
train_indices <- createDataPartition(data_model$booking_status, p = 0.8, list = FALSE)
train_data <- data_model[train_indices, ]
test_data <- data_model[-train_indices, ]

# Verify the dimensions of the train and test sets
dim(train_data)
dim(test_data)

rf_model <- randomForest(booking_status ~ ., data = train_data)
rf_predictions <- predict(rf_model, newdata = test_data)

confusionMatrix(rf_predictions, test_data$booking_status)



predict_booking_status <- function(user_input) {
  user_input_df <- as.data.frame(t(user_input))
  colnames(user_input_df) <- colnames(X)
  
  user_input_df <- user_input_df %>%
    mutate(across(where(is.numeric), as.numeric))
  
  prediction <- predict(rf_model, newdata = user_input_df)
  
  pred <- ifelse(prediction==1,"Canceled","Not canceled")
  return(pred)
}

str(data_model)

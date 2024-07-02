#library
library(tidyverse)
library(skimr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)
library(plotly)
library(waffle)
library(reshape2)
library(caret)
#Problem
##Can you predict if the customer is going to honor the reservation or cancel it ?

#import data
data <-  read.csv("Hotel Reservations.csv")
head(data)

#data preprocessing
str(data)
summary(data)
skim(data)

#pengecekan missing value dan duplicated data
colSums(is.na(data))##no missing value
duplicated_rows <- duplicated(data)
data[duplicated_rows, ]
dim(data[duplicated_rows, ])##no duplicate data

#unique values
sapply(data, function(x) length(unique(x)))
#for categorical data
table(data$booking_status)
table(data$market_segment_type)
table(data$room_type_reserved)
table(data$type_of_meal_plan)

#Perbaikan jenis data
##booking status menjadi 0 1
data$booking_status <- ifelse(data$booking_status == "Canceled", 1, 0)
data$booking_status <- as.integer(data$booking_status)
head(data["booking_status"])

##room type menjadi 1 2 3..7
data$room_type_reserved<- factor(data$room_type_reserved)
data$room_type_reserved<-as.integer(data$room_type_reserved)
head(data["room_type_reserved"])

##meal_type menjadi 0 1 2 3
data$type_of_meal_plan <- gsub("Not Selected", 0, data$type_of_meal_plan) # Mengganti "Not Selected" dengan 0
data$type_of_meal_plan <- gsub("Meal Plan ", "", data$type_of_meal_plan)
data$type_of_meal_plan <- as.integer(data$type_of_meal_plan)
head(data["type_of_meal_plan"])

#mengubah data tanggal
data <- cbind(data[, 1:11], date = as.Date(paste(data$arrival_date, data$arrival_month, data$arrival_year, sep="-"), format="%d-%m-%Y"), data[, 12:ncol(data)])
head(data["date"])
##mengecek data null pada kolom date
subset(data,is.na(date),c(arrival_year,arrival_month, arrival_date, date))
##data 29 Februari 2018 tidak ada pada kalendar, maka data dihapus dari original dataset
data <- data[complete.cases(data$date), ]
dim(data)
str(data)
skim(data)
saveRDS(data,"dataclean.rds")
###data telah dicleaning dan siap digunakan

#EDA dan Visualisasi
#Grafik booking_status Bookings
countplot <- ggplot(data, aes(x = booking_status, fill = booking_status)) + 
  geom_bar(fill=c("0" = "#48cae4", "1" = "#FF7F7F")) +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.64) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.caption = element_text(color = "#1D3557", size = 9.5),
        axis.text = element_text(color = "#0B1F65"))+
  guides(fill = "none")

data_summary <- data %>%
  group_by(booking_status) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Membuat pie chart
piechart<-ggplot(data_summary, aes(x = "", y = percentage, fill = factor(booking_status))) +
  geom_bar(stat = "identity", width = 1) +                
  coord_polar(theta = "y") +                              
  guides(fill = guide_legend(title = "Booking Status", ncol = 1)) + 
  geom_text(aes(label = paste0(round(percentage, 2), "%")),   
            position = position_stack(vjust = 0.5)) +     
  theme_void() +        
  scale_fill_manual(values = c("0" = "#48cae4", "1" = "#FF7F7F"))+
  theme(legend.position = "bottom")            

grid.arrange(countplot, 
             piechart, 
             ncol = 2, widths = c(4, 3.5), top = "Distribution of booking_status Bookings")

tgplot1 <- function(data){
  #Grafik booking_status Bookings
  #barchart
  countplot <- ggplot(data, aes(x = booking_status, fill = booking_status)) + 
    geom_bar(fill=c("0" = "#48cae4", "1" = "#FF7F7F")) +
    geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.64) +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          plot.caption = element_text(color = "#1D3557", size = 9.5),
          axis.text = element_text(color = "#0B1F65"))+
    guides(fill = "none")
  
  data_summary <- data %>%
    group_by(booking_status) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)
  
  # pie chart
  piechart<-ggplot(data_summary, aes(x = "", y = percentage, fill = factor(booking_status))) +
    geom_bar(stat = "identity", width = 1) +                
    coord_polar(theta = "y") +                              
    guides(fill = guide_legend(title = "Booking Status", ncol = 1)) + 
    geom_text(aes(label = paste0(round(percentage, 2), "%")),   
              position = position_stack(vjust = 0.5)) +     
    theme_void() +        
    scale_fill_manual(values = c("0" = "#48cae4", "1" = "#FF7F7F"))+
    theme(legend.position = "bottom")            
  
  plot <- grid.arrange(countplot, 
               piechart, 
               ncol = 2, widths = c(4, 3.5))
  return(plot)
}

tgplot2<- function(data){
#line chart book count each day
bookdate<-data %>%
  group_by(date,booking_status)%>%
  summarise(count = n()) 
bd <- ggplot(bookdate,aes(x=date,y=count, color = as.factor(booking_status), group = booking_status))+
        geom_line() +
        scale_color_manual(values = c("0" = "#48cae4", "1" = "#FF7F7F"))+
        labs(x = "Date",
             y = "Count",
             color = "Status") +
        theme_minimal()+
        scale_x_date(date_breaks = "1 month", date_labels = "%m-%y")
  
  ggplotly(bd)
}

tgplot3<- function(data){
#line chart average price per room
pr <-   ggplot(data, aes(x = date, y = avg_price_per_room)) + 
        geom_smooth(method="auto") +
        theme(panel.grid = element_blank(),
              panel.background = element_blank(),
              plot.caption = element_text(color = "#1D3557", size = 9.5),
              axis.text = element_text(color = "#0B1F65"))+
        labs(x = "Month", y = "Average Price per Room") +
        scale_x_date(date_breaks = "1 month", date_labels = "%m-%y")
  ggplotly(pr)
}

tgplot4<- function(data){  
#barchart meal plan types  
countplot <- ggplot(data, aes(x = type_of_meal_plan, fill = booking_status)) +
  geom_bar(position="dodge") +
  labs(x = "", y = "", fill = "booking_status") +
  geom_text(stat='count', aes(label=after_stat(count)),position=position_dodge(width = 0.85), vjust=-0.2) +
  theme(legend.position = c(0.98, 0.98),
        legend.justification = c(1, 1))

wafflechart <- waffle(prop.table(table(data$type_of_meal_plan)) * 100,rows=11,reverse = TRUE,size=1.5, legend_pos = "bottom") +
  theme(legend.direction = "vertical")+
  theme(legend.spacing.y = unit(-0.5,"cm"))

grid.arrange(countplot, wafflechart, ncol = 2, widths = c(2, 1),top="Distribution of Meal Plan Types by Cancellation Status")}

#room types
data$room_type_reserved <- as.factor(data$room_type_reserved)
data$booking_status <- as.factor(data$booking_status)
tgplot5<- function(data){
# Create the count plot
countplot2 <- ggplot(data, aes(x = room_type_reserved, fill = booking_status)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "", fill = "Booking Status") +
  geom_text(stat = 'count', aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3.1) +
  theme(legend.position = c(0.98, 0.98),
        legend.justification = c(1, 1)) +
  scale_x_discrete()  # Use scale_x_discrete for categorical variables
# Create the waffle chart
waffle_data <- prop.table(table(data$room_type_reserved)) * 100
wafflechart2 <- waffle(waffle_data, rows = 11, reverse = TRUE, size = 1.5, legend_pos = "bottom") +
  theme(legend.direction = "vertical",
        legend.spacing.y = unit(-0.5, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10)) +
  guides(fill = guide_legend(override.aes = list(size = 3)))
# Combine the plots
grid.arrange(countplot2, wafflechart2, ncol = 2, widths = c(2, 1), top = "Distribution of Room Types Reserved by Cancellation Status")}

tgplot6<- function(data){
  #market segment
  countplot3 <- ggplot(data, aes(x = market_segment_type, fill = booking_status)) +
    geom_bar(position="dodge") +
    labs(x = "", y = "", fill = "booking_status") +
    geom_text(stat='count', aes(label=after_stat(count)),position=position_dodge(width = 0.9), vjust=-0.5,size =3.1) +
    theme(legend.position = c(0.25, 0.98),
          legend.justification = c(1, 1))

  wafflechart3 <- waffle(prop.table(table(data$market_segment_type)) * 100, rows=11, reverse = TRUE, size=1.5, legend_pos = "bottom") +
    theme(legend.direction = "vertical",
          legend.spacing.y = unit(-0.4,"cm"),
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          panel.grid = element_blank())

  plot <-grid.arrange(countplot3, wafflechart3, ncol = 2, widths = c(2, 1),top="Distribution of Market Segments by Cancellation Status")
  return(plot)
}


#Number of week &weekend Nights
tgplot7 <- function(data) {
  # Number of week & weekend Nights
  hist_weekend_nights <- ggplot(data) +
    geom_histogram(aes(x = no_of_weekend_nights), binwidth = 1, color = "white", fill = "#E97979") +
    labs(y = "Count", x = "") +
    coord_cartesian(xlim = c(0, 5)) +
    ggtitle("Distribution of Number of Weekend Nights") +
    theme(plot.title = element_text(size = 11))
  
  hist_week_nights <- ggplot(data) +
    geom_histogram(aes(x = no_of_week_nights), binwidth = 1, color = "white", fill = "#665C91") +
    labs(x = "", y = "") +
    coord_cartesian(xlim = c(0, 11)) +
    ggtitle("Distribution of Number of Week Nights") +
    theme(plot.title = element_text(size = 11))
  
  # Convert ggplot objects to plotly objects
  plotly_weekend_nights <- ggplotly(hist_weekend_nights)
  plotly_week_nights <- ggplotly(hist_week_nights)
  
  # Arrange plots side by side using subplot from plotly
  subplot(plotly_week_nights, plotly_weekend_nights, nrows = 1)
}

tgplot8<- function(data){
#GRAFIK Jumlah dan Kategori Pengunjung (adults and children)
hist_adults <- ggplot(data) +
  geom_histogram(aes(x = no_of_adults),binwidth = 1,color="white",fill="#437C17") +
  labs( y = "Count",x="") +
  ggtitle("Distribution of the Number of Adults") +
  theme(text=element_text(size=10))

hist_children <- ggplot(data) +
  geom_histogram(aes(x = no_of_children),binwidth = 1, color="white",fill="#99C68E") +
  labs(x = "", y = "") +
  coord_cartesian(xlim = c(0, 3)) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  ggtitle("Distribution of the Number of Children") +
  theme(text=element_text(size=10))

pha <- ggplotly(hist_adults)
phc <- ggplotly(hist_children)
subplot(pha,phc, nrows = 1)
}


###################

# Load necessary libraries
library(dplyr)
library(caret)
library(randomForest)

# Read the data
head(data)
data$booking_status
# Split data into features (X) and target (y)
X <- data %>% select(-Booking_ID, -booking_status, -arrival_date, -arrival_month, -arrival_year,-date)
y <- data$booking_status
X$market_segment_type <- as.numeric(factor(X$market_segment_type))
X$room_type_reserved <- as.numeric(factor(X$room_type_reserved))
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


#library
library(tidyverse)
library(skimr)
library(ggplot2)
library(dplyr)
library(gridExtra)

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

skim(data)
saveRDS(data,"dataclean.rds")
###data telah dicleaning dan siap digunakan

#EDA dan Visualisasi
#Grafik Canceled Bookings
countplot <- ggplot(data, aes(x = booking_status, fill = booking_status)) + 
  geom_bar(fill=c("0" = "#ADD8E6", "1" = "#FF7F7F")) +
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
  scale_fill_manual(values = c("0" = "#ADD8E6", "1" = "#FF7F7F"))+
  theme(legend.position = "bottom")            

grid.arrange(countplot, 
             piechart, 
             ncol = 2, widths = c(4, 3.5), top = "Distribution of Canceled Bookings")

tgplot1 <- function(data){
  #Grafik Canceled Bookings
  countplot <- ggplot(data, aes(x = booking_status, fill = booking_status)) + 
    geom_bar(fill=c("0" = "#ADD8E6", "1" = "#FF7F7F")) +
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
    scale_fill_manual(values = c("0" = "#ADD8E6", "1" = "#FF7F7F"))+
    theme(legend.position = "bottom")            
  
  plot <- grid.arrange(countplot, 
               piechart, 
               ncol = 2, widths = c(4, 3.5))
  return(plot)
}
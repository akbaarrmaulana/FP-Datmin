#library
library(tidyverse)
library(skimr)
library(ggplot2)
library(dplyr)

#Problems
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
###data telah dicleaning dan siap digunakan
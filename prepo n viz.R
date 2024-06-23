#library
library(tidyverse)
library(skimr)
library(ggplot2)
library(dplyr)

#import data
data <-  read.csv("Hotel Reservations.csv")
head(data)

#data preprocessing
str(data)
summary(data)
skim(data)

#pengecekan missing value dan duplicated data
colSums(is.na(data))
#no missing value
duplicated_rows <- duplicated(data)
dim(data[duplicated_rows, ])
#no duplicate data
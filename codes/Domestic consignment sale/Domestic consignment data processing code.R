rm(list = ls())
install.packages("ggplot2")
library(tidyverse)
#위판장별 위탁판매 집계
setwd("C:/Users/user/Desktop/R project/수산물 위판 (2013~)/위탁판매 집계")
getwd()
name_1 <- "해양수산부_위판장별 어종별 위탁판매 집계 - 2013년 01월(월간).csv"
norm <- 0
year <- 13

for (i in 1:87){
  if (i == 1) {
    data <- read.csv(name_1,header = T)
    norm <-norm+1
  }
  else {
    norm<-norm+1
    if (norm == 13) {
      norm <- 1
      year <- year+1
      substr(name_1,28 , 29) <- as.character(year)
    }
    if (norm < 10){
      substr(name_1, 32,33) <-paste0(as.character(0),as.character(norm))
    }
    else {
      substr(name_1, 32, 33) <- as.character(norm)
    }
    data<-rbind(data, read.csv(name_1, header = T))
  }
}
head(data)
tail(data)


data1 <- as_tibble(data)
for (i in 1:18) {
  if (i == 1) {
    data1[[i]] <-as.Date.factor(data1[[i]], format = "'%Y%m%d'")
  }
  else if(i == 3 || i == 5 || i == 7 || i == 9 || i == 10 || i == 11) {
    data1[[i]] <- data1[[i]]
  }
  else if(i == 18) {
    data1[[i]] <-as.Date.factor(data1[[i]], format = "'%Y-%m-%d'")
  }
  else {
    data1[[i]] <- as.numeric(gsub("'","",data1[[i]]))
  }
}
View(data)
View(data1)

View(is.na(data1))

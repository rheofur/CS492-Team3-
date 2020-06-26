library(dplyr)

setwd("C:\\Users\\HP\\Desktop\\R proj")

#na.strings is '...'
production_dollar <- read.csv("production_dollar_13-18.csv", na.strings='...', stringsAsFactors = FALSE)
trade_dollar <- read.csv("trade_dollar_13-17.csv", na.strings='...', stringsAsFactors = FALSE)

#summary of raw data
summary(production_dollar)
summary(trade_dollar)

production_df <- data.frame(production_dollar, stringsAsFactors = FALSE)
trade_df <- data.frame(trade_dollar, stringsAsFactors = FALSE)

#preprocessing
#rename cols for convinience
colnames(production_df) <- c("Country", "CPC_Class", "A", "B", "C", "D", "y13", "y14", "y15", "y16", "y17", "y18")
colnames(trade_df) <- c("Country", "CPC_Class", "Trade_Flow", "A", "y13", "y14", "y15","y16","y17")

#drop last 2 row / remove irrelvant columns
production_df <- production_df[-c(nrow(production_df)-1, nrow(production_df)), ]
production_df <- select(production_df, -c(A,B,C,D))

trade_df <- trade_df[-c(nrow(trade_df)-1, nrow(trade_df)), ]
trade_df <- select(trade_df, -A)

summary(production_df)
summary(trade_df)

#save primarily processed data
write.csv(production_df, "Processed_production.csv")
write.csv(trade_df, "Processed_trade.csv")
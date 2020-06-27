#############
#data import
#############
library(dplyr)

#dataset
wholesale <- read.csv("도매.csv", encoding="UTF-8-BOM")
trade <- read.csv("수출입.csv", encoding="UTF-8-BOM")
fishery <- read.csv("어업.csv", encoding="UTF-8-BOM")
observation <- read.csv("해양관측.csv", encoding="UTF-8-BOM")
sale <- read.csv("위판.csv", encoding="UTF-8-BOM", quote="'")

#수산물표준코드
code <- read.csv("2016년수산물표준코드.csv", encoding="UTF-8-BOM")
wholesale$species <- sapply(wholesale$productcode, function(x) code$소분류명[which(x==code$수산물품목코드)] %>% as.character)
sale$species <- sapply(sale$수산물표준코드, function(x) code$소분류명[which(as.character(x)==code$수산물품목코드)] %>% as.character)

View(wholesale)
View(trade)
View(fishery)
View(observation)
View(code)

#도매시장코드
code <- read.csv("도매시장매핑.csv", encoding="UTF-8-BOM")
wholesale$market <- sapply(wholesale$marketcode, function(x) code$market[which(x==code$marketcode)])

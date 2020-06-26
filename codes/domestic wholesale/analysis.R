library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(lubridate)
library(zoo)

fishery <- read.csv("data.csv")
fishery$date <- as.Date(fishery$date)
fishery$price <- as.numeric(fishery$price)
View(fishery)
str(fishery)

code <- read.csv("2016년수산물표준코드.csv")[, c(code="수산물품목코드", name="소분류명")]
names(code) <- c("code", "name")
marketcode <- read.csv("도매시장매핑.csv")
fishery$market <- sapply(fishery$marketcode, function(x) {marketcode$market[which(x == marketcode$marketcode)]})
View(fishery)

#농수산물 표준코드 게
#해면갑각류/게류 == 6302
#내수면갑각류/민물게류 == 7302

#월별분류코드 
#as.yearmon(format(as.Date(date), "%Y-%m"))
####시간별 게 판매####

crabs <- fishery %>% 
  filter(substr(productcode, 1, 4) %in% c(6302, 7302)) %>%
  group_by(productcode, date) %>%
  summarize(amount=sum(amount, na.rm=TRUE), price=sum(price, na.rm=TRUE))
crabs$product <- sapply(crabs$productcode, function(x) as.character(code$name[which(x==code$code)]))
crabnames <- levels(factor(crabs$product))
View(crabs)

for (c in crabnames) {
  data <- crabs %>% 
    filter(product == c) %>%
    group_by(product, time=month(date)) %>%
    summarize(amount=sum(amount, na.rm=TRUE), price=sum(price, na.rm=TRUE), ratio=sum(price, na.rm=TRUE)/sum(amount, na.rm=TRUE))
  p1 <- ggplot(data=data, aes(x=time)) + 
      geom_line(aes(y=amount), color="tomato") + 
      facet_grid(~product)
  p2 <- ggplot(data=data, aes(x=time)) + 
    geom_line(aes(y=price), color="orange") +
    facet_grid(~product)
  p3 <- ggplot(data=data, aes(x=time)) + 
    geom_line(aes(y=ratio), color="lightslateblue") +
    facet_grid(~product)
  g <- arrangeGrob(p1, p2, p3, nrow=1)
  ggsave(paste0("pattern_", c, ".png"), g)
}

####도매시장별 게 판매####

p <- fishery %>%
  filter(substr(productcode, 1, 4) %in% c(6302, 7302)) %>%
  group_by(market, productcode) %>%
  summarize(amount=sum(amount,na.rm=TRUE), price=sum(price,na.rm=TRUE), ratio=sum(price,na.rm=TRUE)/sum(amount,na.rm=TRUE)) %>%
  ungroup %>%
  mutate(productcode = sapply(productcode, function(x) code$name[which(x==code$code)] %>% as.character)) %>%
  rename(product = productcode) %>%
  group_by(market) %>%
  top_n(5, wt=ratio) %>%
  arrange(ratio) %>%
  ggplot(aes(y=market, weight=ratio, fill=product))
p1 <- p + geom_bar(position="fill")
p2 <- p + geom_bar()
g <- arrangeGrob(p2, p1, nrow=1)
ggsave(paste0("market_top5.ratio.png"), g)

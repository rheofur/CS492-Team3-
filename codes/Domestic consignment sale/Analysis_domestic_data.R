library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)

####국내 위판자료 분석 코드
##selling_domestic(1,2,3,4).RData 필요

load("selling_domestic1.RData")
load("selling_domestic2.RData")
load("selling_domestic3.RData")
load("selling_domestic4.RData")
selling_domestic<-rbind(selling_domestic1,selling_domestic2,selling_domestic3,selling_domestic4)
rm(selling_domestic1,selling_domestic2,selling_domestic3,selling_domestic4)
selling_domestic  <- na.omit(selling_domestic) 


selling_d_tot <- selling_domestic %>%
  group_by(위판일자) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
d_year <- cbind(year = as.numeric(format(selling_d_tot$위판일자,"%Y")),selling_d_tot[,-1])
d_year <- d_year %>%
  group_by(year) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
d_year
ggplot(d_year[-8,], aes(year, 물량.KG.)) + geom_line() +
  ggtitle("Total production in korean consigment sale") + geom_smooth(method = lm) 

#생선별 분류 - 전국 위판 기록상의 생산량 상위 6가지
fish_ordered <- selling_domestic %>%
  group_by(수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
fish_ordered_6 <- as.integer(c(fish_ordered[order(fish_ordered$물량.KG., decreasing = T)[1:6],1])$수산물표준코드명)

ggplot(fish_ordered %>% filter(as.integer(수산물표준코드명) %in% fish_ordered_6),
       aes(x = 수산물표준코드명, y = 평균가, fill = 수산물표준코드명)) + geom_col()
ggplot(fish_ordered %>% filter(as.integer(수산물표준코드명) %in% fish_ordered_6),
       aes(x = 수산물표준코드명, y = 물량.KG., fill = 수산물표준코드명)) + geom_col()
grid.arrange(a, b, nrow = 1, ncol = 2)

#example: 가자미
#가자미 전체 기간의 가격, 생산량
fish_gajami<- selling_domestic %>%
  filter(수산물표준코드 == 610200) %>%
  group_by(위판일자, 어종상태명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
a <-ggplot(fish_gajami, aes(x = 위판일자, y = 평균가)) + geom_line(aes(color = 어종상태명))
b <-ggplot(fish_gajami, aes(x = 위판일자, y = 물량.KG.)) + geom_line(aes(color = 어종상태명))
#가자미 연별 가격 추이
fish_gajami_year <- cbind(year = as.numeric(format(fish_gajami$위판일자,"%Y")),fish_gajami[,-1])
fish_gajami_year <- fish_gajami_year %>%
  group_by(year, 어종상태명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
c <-ggplot(fish_gajami_year, aes(x = year, y = 평균가)) + geom_line(aes(color = 어종상태명))
d <-ggplot(fish_gajami_year, aes(x = year, y = 물량.KG.)) + geom_line(aes(color = 어종상태명))
grid.arrange(a, b, c, d, nrow = 2, ncol = 2)
#example: 문어
fish_tako<- selling_domestic %>%
  filter(수산물표준코드 == 640407) %>%
  group_by(위판일자, 어종상태명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))

a <-ggplot(fish_tako, aes(x = 위판일자, y = 평균가)) + geom_line(aes(color = 어종상태명)) + geom_area(alpha = 0.1)
b <-ggplot(fish_tako, aes(x = 위판일자, y = 물량.KG.)) + geom_line(aes(color = 어종상태명))

fish_tako_year <- cbind(year = as.numeric(format(fish_tako$위판일자,"%Y")),fish_tako[,-1])
fish_tako_year <- fish_tako_year %>%
  group_by(year, 어종상태명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
c <-ggplot(fish_tako_year, aes(x = year, y = 평균가)) + geom_line(aes(color = 어종상태명))
d <-ggplot(fish_tako_year, aes(x = year, y = 물량.KG.)) + geom_line(aes(color = 어종상태명))
grid.arrange(a, b, c, d, nrow = 2, ncol = 2)


#지역별 분류
#example: 강릉시
city_gn <- selling_domestic %>%
  filter(산지조합명 == "'강릉시수산업협동조합'") %>%
  group_by(위판일자, 수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
city_gn_total <- city_gn %>%
  group_by(수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
#강릉시 내 생산량 상위 수산물 조사
a<-ggplot(city_gn_total[order(city_gn_total$물량.KG., decreasing = T)[1:12],], aes(x = 수산물표준코드명, y = 물량.KG.)) + geom_col(aes(fill = 수산물표준코드명))
city_gn_5 <- as.integer(c(city_gn_total[order(city_gn_total$물량.KG., decreasing = T)[1:5],1])$수산물표준코드명)
#생산량 상위 수산물의 일별 평균가, 생산량 추이
b<-ggplot(city_gn %>% filter(as.integer(수산물표준코드명) %in% city_gn_5), aes(x = 위판일자, y = 평균가)) + geom_line(aes(color = 수산물표준코드명))
c<-ggplot(city_gn %>% filter(as.integer(수산물표준코드명) %in% city_gn_5), aes(x = 위판일자, y = 물량.KG.)) + geom_line(aes(color = 수산물표준코드명))
city_gn_year <- cbind(year = as.numeric(format(city_gn$위판일자,"%Y")),city_gn[,-1])
city_gn_year <- city_gn_year %>%
  group_by(year,수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
#생산량 상위 수산물의 연별 평균가, 생산량 추이
d<-ggplot(city_gn_year %>% filter(as.integer(수산물표준코드명) %in% city_gn_5), aes(x = year, y = 평균가)) + geom_line(aes(color = 수산물표준코드명))
e<-ggplot(city_gn_year %>% filter(as.integer(수산물표준코드명) %in% city_gn_5), aes(x = year, y = 물량.KG.)) + geom_line(aes(color = 수산물표준코드명))
grid.arrange(b, c, d, e, nrow = 2, ncol = 2)


#(살)오징어
squid<- selling_domestic %>%
  filter(수산물표준코드 == 640501) %>%
  group_by(위판일자) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
squid_region<-selling_domestic %>%
  filter(수산물표준코드 == 640501) %>%
  group_by(위판일자,산지조합명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
squid_tot<-squid_region %>%
  group_by(산지조합명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
squid_re_5 <- as.integer(c(squid_tot[order(squid_tot$물량.KG., decreasing = T)[1:5],1])$산지조합명)

ggplot(squid_region, aes(x = 위판일자, y = 물량.KG.)) + geom_line(aes(color = 산지조합명))
ggplot(squid_region %>% filter(산지조합명 == "'속초시수산업협동조합'"), aes(x = 위판일자, y = 물량.KG.)) + geom_line(aes(color = 산지조합명))
ggplot(squid_region %>% filter(as.integer(산지조합명) %in% squid_re_5), aes(x = 위판일자, y = 물량.KG.)) + geom_line(aes(color = 산지조합명))


ggplot(squid, aes(x = 위판일자, y = 평균가)) + geom_line(aes(color = 어종상태명)) + geom_area(alpha = 0.1)
ggplot(squid, aes(x = 위판일자, y = 물량.KG.)) + geom_point(color = 'magenta') +
  geom_smooth(method = lm) + ggtitle("Amount of consignment sale of squid")

squid_year <- cbind(year = as.numeric(format(squid$위판일자,"%Y")),squid[,-1])
squid_year <- squid_year %>%
  group_by(year) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))

b <- ggplot(squid_year, aes(x = year, y = 평균가)) + geom_line() + 
   ggtitle("Squid annual price")+geom_smooth(method = lm, col = 'red')
c <- ggplot(squid_year, aes(x = year, y = 물량.KG.)) + geom_line() +
   ggtitle("Squid annual production")+geom_smooth(method = lm, col = 'red')
grid.arrange(b,c,nrow = 1)
t.test(squid_year$평균가,squid_year$물량.KG., paired = T)

#환경 데이터 분석 w. squid&crab
env <- read.csv("envdata.csv")
env <- env[,c(-8,-9,-10,-11,-16)]
env_date <- env %>%
  filter(!is.na(air.t)) %>%
  filter(!is.na(pressure)) %>%
  filter(!is.na(sea.t)) %>%
  group_by(date) %>%
  summarise(air.t = mean(air.t), pressure = mean(pressure), sea.t = mean(sea.t))
view(env_date)
ggplot(env_date, aes(x = as.numeric(date), y = air.t)) +
  xlab("time") + ylab("sea temperature")+
  geom_point() + ggtitle("Sea temperature during 2009~2019")+
  stat_smooth(method = lm, col = "red")


env_month <- cbind(date = format(as.Date(env_date$date),"%Y-%m"),env_date[,-1])
summary(env_month)
env_month <- env_month %>%
  group_by(date) %>%
  summarise(air.t = mean(air.t), pressure = mean(pressure), sea.t = mean(sea.t))
ggplot(env_month, aes(x = date, y = sea.t)) + geom_col()
ggplot(env_month, aes(x = date, y = pressure)) + geom_col()
ggplot(env_month, aes(x = date, y = air.t)) + geom_col()


env_year <- cbind(year = format(as.Date(env_date$date),"%Y"),env_date[,-1])
summary(env_year)

env_year <- env_year %>%
  group_by(year) %>%
  summarise(air.t = mean(air.t), pressure = mean(pressure), sea.t = mean(sea.t))
ggplot(env_year[-1,], aes(x = as.numeric(year), y = sea.t)) + geom_point(col = "blue") + geom_smooth(method = lm)
ggplot(env_year[-1,], aes(x = as.numeric(year), y = pressure)) + geom_point(col = "blue")+ geom_smooth(method = lm)
ggplot(env_year[-1,], aes(x = as.numeric(year), y = air.t)) + geom_point(col = "blue")+ geom_smooth(method = lm)
View(env_year)

env_year[c(-1,-2,-3,-4),]$sea.t; squid_year_t[-8,]$물량.KG.

a <- ggplot(env_year[-1:-4,], aes(x = as.numeric(year),y = sea.t)) + ggtitle("Average sea T during period") +
  geom_line() + xlab("time(year, since 2013)") + ylab("sea temperature(C)")+
  geom_smooth(method = lm, formula = y~x)
squid_year
grid.arrange(a,b,c,nrow = 1, ncol = 3)

var.test(env_year[c(-1,-2,-3,-4),]$sea.t,squid_year[-8,]$물량.KG.)
t.test(env_year[c(-1,-2,-3,-4),]$sea.t,squid_year[-8,]$물량.KG., paired = T)



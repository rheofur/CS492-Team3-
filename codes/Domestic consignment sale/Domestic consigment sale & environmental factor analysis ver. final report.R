#selling_domestic(1,2,3,4).RData 필요
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(tseries)
library(forecast)
library(moonBook)
library(urca)

selling_domestic<-rbind(selling_domestic1,selling_domestic2,selling_domestic3,selling_domestic4)
rm(selling_domestic1,selling_domestic2,selling_domestic3,selling_domestic4)
selling_domestic<-na.omit(selling_domestic)

#생선별 분류 - 전국 위판 기록상의 생산량 상위 6가지
fish_ordered <- selling_domestic %>%
  group_by(수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
fish_ordered_6 <- as.integer(c(fish_ordered[order(fish_ordered$물량.KG., decreasing = T)[1:6],1])$수산물표준코드명)

a<-ggplot(fish_ordered %>% filter(as.integer(수산물표준코드명) %in% fish_ordered_6),
       aes(x = 수산물표준코드명, y = 평균가, fill = 수산물표준코드명)) + geom_col()
b<-ggplot(fish_ordered %>% filter(as.integer(수산물표준코드명) %in% fish_ordered_6),
       aes(x = 수산물표준코드명, y = 물량.KG., fill = 수산물표준코드명)) + geom_col()
grid.arrange(a, b, nrow = 1, ncol = 2)

#게류 분석 데이터
fish_crab<- selling_domestic %>%
  filter(수산물표준코드 >= 630200) %>%
  filter(수산물표준코드 < 630300) %>%
  group_by(위판일자, 수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
fish_c_tot <- fish_crab %>%
  group_by(수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))

#국내 게류 전체 플롯
ggplot(fish_c_tot, aes(x = 수산물표준코드명, y = 평균가)) +
  geom_col(aes(fill = 수산물표준코드명))
ggplot(fish_c_tot, aes(x = 수산물표준코드명, y = 물량.KG.)) +
  geom_col(aes(fill = 수산물표준코드명))

fish_c_ordered_6 <- as.integer(c(fish_c_tot[order(fish_c_tot$물량.KG., decreasing = T)[1:6],1])$수산물표준코드명)
fish_c_year <- cbind(year = as.numeric(format(fish_crab$위판일자,"%Y")),fish_crab[,-1])
fish_c_year <- fish_c_year %>%
  group_by(year,수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
#국내 게류 생산량 상위 6종 전체기간 평균가, 생산량 그래프
ggplot(fish_crab %>% filter(as.integer(수산물표준코드명) %in% fish_c_ordered_6),
       aes(x = 수산물표준코드명, y = 평균가, fill = 수산물표준코드명)) + geom_col()
ggplot(fish_crab %>% filter(as.integer(수산물표준코드명) %in% fish_c_ordered_6),
       aes(x = 수산물표준코드명, y = 물량.KG., fill = 수산물표준코드명)) + geom_col()
#국내 게류 생산량 상위 6종 연별 평균가, 생산량 그래프
ggplot(fish_c_year %>% filter(as.integer(수산물표준코드명) %in% fish_c_ordered_6), aes(x = year, y = 평균가)) +
  geom_line(aes(color = 수산물표준코드명))
ggplot(fish_c_year %>% filter(as.integer(수산물표준코드명) %in% fish_c_ordered_6), aes(x = year, y = 물량.KG.)) +
  geom_line(aes(color = 수산물표준코드명))
ggplot(fish_crab %>% filter(as.integer(수산물표준코드명) %in% fish_c_ordered_6), aes(x = 위판일자, y = 물량.KG.)) + geom_point(aes(color = 수산물표준코드명)) 


#환경 데이터 분석 w. squid&crab
#getwd()
#setwd("C:/Users/user/Desktop/R project")
env <- read.csv("envdata.csv")
env <- env[,c(-8,-9,-10,-11,-16)]
env <- na.omit(env)
env_year <- cbind(year = format(as.Date(env_date$date),"%Y"),env_date[,-1])
env_year <- env_year %>%
  group_by(year) %>%
  summarise(air.t = mean(air.t), pressure = mean(pressure), sea.t = mean(sea.t))
env_date <- env %>%
  group_by(date) %>%
  summarise(air.t = mean(air.t), pressure = mean(pressure), sea.t = mean(sea.t))
view(env_date)
ggplot(env_date, aes(x = as.numeric(date), y = air.t)) +
  xlab("time") + ylab("sea temperature")+
  geom_point() + ggtitle("Sea temperature during 2009~2019")+
  stat_smooth(method = lm, col = "red")

env_lm <- lm(data = env_date,formula = sea.t~ as.numeric(date))
env_lm


#붉은대게 ~ 오징어 (기사 참조)
ggplot(fish_crab %>% filter(수산물표준코드명 == "'붉은대게'"), aes(x = 위판일자, y = 물량.KG.)) + geom_point(aes(color = 수산물표준코드명)) # geom_abline()
#(살)오징어
squid<- selling_domestic %>%
  filter(수산물표준코드 == 640501) %>%
  group_by(위판일자) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
ggplot(squid, aes(x = 위판일자, y = 평균가)) + geom_line(color = "skyblue") + geom_area(alpha = 0.1)
ggplot(squid, aes(x = 위판일자, y = 물량.KG.)) + geom_point(color = 'magenta') +
  geom_smooth(method = lm) + ggtitle("Amount of consignment sale of squid")

squid_year <- cbind(year = as.numeric(format(squid$위판일자,"%Y")),squid[,-1])
squid_year <- squid_year %>%
  group_by(year) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))

ggplot(squid_year, aes(x = year, y = 평균가)) + geom_line() + 
  ggtitle("Squid annual price")+geom_smooth(method = lm, col = 'red')
ggplot(squid_year, aes(x = year, y = 물량.KG.)) + geom_line() +
  ggtitle("Squid annual production")+geom_smooth(method = lm, col = 'red')
lm1 <- lm(year~물량.KG.,squid_year)
lm2 <- lm(year~sea.t,env_year)
env_ts<-ts(env_date[,4], start=c(2009,1,1), end = c(2019,12,31),frequency = 12)
plot(env_ts)
acf(env_ts)
pacf(env_ts)
adf.test(env_ts, k= 0)

env_year[c(-1,-2,-3,-4),c(1,4)]
squid_year[c(-8),c(1,3)]
cor(env_year[c(-1,-2,-3,-4),4],squid_year[c(-8),c(2,3)])

#게류 종별 수온과의 correlation 비교
fish_c_y<-fish_c_year[
  order(fish_c_year$수산물표준코드명,fish_c_year$year,fish_c_year$평균가,fish_c_year$물량.KG.),]
for (i in 1:9) {
  a<-cor(env_year[c(-1,-2,-3,-4),4],fish_c_y[(i*8-7):(i*8-1),c(3,4)])
  show(fish_c_y[i*8-7,2])
  show(a)
}


#꽃게 ~ ARIMA로 평균가,생산량 분석
fish_fcrab <- (fish_crab%>% filter(수산물표준코드명 == "'꽃게'"))[,-2]
#평균가 예측
ggplot(fish_crab %>% filter(수산물표준코드명 == "'꽃게'"), aes(x = 위판일자, y = 평균가)) + geom_point(aes(color = 수산물표준코드명)) +
  geom_smooth(method = lm, formula = y~poly(x,1))+ ggtitle("Amount of consigment sale of gazami crab")
tsData1<- ts(data = fish_fcrab$평균가,
             start = c(2013,1),  end = c(2019,12), frequency = 12)
plot(stl(tsData1, s.window="periodic"))
adf.test(diff(log(tsData1)), alternative= "stationary", k=0)
auto.arima(diff(log(tsData1)))
tsdiag(auto.arima(diff(log(tsData1))))
fit1 <- arima(log(tsData1), c(1, 0, 1), 
              seasonal = list(order = c(0, 1, 1),period = 12))
f_pred_val<- predict(fit1, n.ahead = 3*12)
ts.plot(tsData1, 2.718^f_pred_val$pred, log = "y", lty = c(1,3))
2.718^f_pred_val$pred #예측한 평균가의 data

#생산량 예측
ggplot(fish_crab %>% filter(수산물표준코드명 == "'꽃게'"), aes(x = 위판일자, y = 물량.KG.)) + geom_point(aes(color = 수산물표준코드명)) +
  geom_smooth(method = lm, formula = y~poly(x,1))+ ggtitle("Amount of consigment sale of gazami crab")
tsData2<- ts(data = fish_fcrab$물량.KG.,
            start = c(2013,1),  end = c(2019,12), frequency = 12)
plot(stl(tsData2, s.window="periodic"))
adf.test(diff(log(tsData2)), alternative= "stationary", k=0)
auto.arima(diff(log(tsData2)))
tsdiag(auto.arima(diff(log(tsData2))))
fit2 <- arima(log(tsData), c(1, 0, 1), 
             seasonal = list(order = c(0, 1, 1),period = 12))
f_pred_prod<- predict(fit2, n.ahead = 3*12)
ts.plot(tsData2, 2.718^f_pred_prod$pred, log = "y", lty = c(1,3))
2.718^f_pred_prod$pred #예측한 생산량의 data


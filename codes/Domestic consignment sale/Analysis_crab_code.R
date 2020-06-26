#selling_domestic(1,2,3,4).RData 필요
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)

selling_domestic<-rbind(selling_domestic1,selling_domestic2,selling_domestic3,selling_domestic4)
rm(selling_domestic1,selling_domestic2,selling_domestic3,selling_domestic4)
selling_domestic  <- selling_domestic %>%
  filter(평균가 > 0 )

#게류 분석 데이터
fish_crab<- selling_domestic %>%
  filter(수산물표준코드 >= 630200) %>%
  filter(수산물표준코드 < 630300) %>%
  group_by(위판일자, 수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
fish_c_tot <- fish_crab %>%
  group_by(수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
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
grid.arrange(a, b, c, d, nrow = 2, ncol = 2)

fish_crab_re <- selling_domestic %>%  
  filter(수산물표준코드 >= 630200) %>%
  filter(수산물표준코드 < 630300) %>%
  filter(!is.na(수산물표준코드명)) %>%
  group_by(위판일자, 수산물표준코드명, 산지조합명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
fish_c_r_tot <- fish_crab_re %>%
  group_by(산지조합명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
fish_c_r_6 <- as.integer(c(fish_c_r_tot[order(fish_c_r_tot$물량.KG., decreasing = T)[1:6],1])$산지조합명)
fish_c_r_year <- cbind(year = as.numeric(format(fish_crab_re$위판일자,"%Y")),fish_crab_re[,-1])
fish_c_r_year <- fish_c_r_year %>%
  filter(as.integer(산지조합명) %in% fish_c_r_6) %>%
  filter(물량.KG. > 0) %>%
  group_by(year, 수산물표준코드명, 산지조합명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
#지역별 게류 생산량 연간 추이
ggplot(fish_c_r_year %>% filter(as.integer(수산물표준코드명) %in% fish_c_ordered_6), aes(x = year, y = 물량.KG., fill = 산지조합명)) + 
  geom_col()


#붉은대게, 꽃게 분석
ggplot(fish_crab %>% filter(as.integer(수산물표준코드명) %in% fish_c_ordered_6), aes(x = 위판일자, y = 물량.KG.)) + geom_point(aes(color = 수산물표준코드명)) 
ggplot(fish_crab %>% filter(수산물표준코드명 == "'붉은대게'"), aes(x = 위판일자, y = 물량.KG.)) + geom_point(aes(color = 수산물표준코드명)) # geom_abline()
ggplot(fish_crab %>% filter(as.integer(수산물표준코드명) %in% fish_c_ordered_6), aes(x = 위판일자, y = 물량.KG.)) + geom_point(aes(color = 수산물표준코드명)) 

red_crab_re <- fish_crab_re %>%
  filter(수산물표준코드명 == "'붉은대게'")
view(red_crab_re)
ggplot(red_crab_re, aes(x = 위판일자, y = 물량.KG.)) + geom_line(aes(color = 산지조합명))
ggplot(red_crab_re %>% filter(산지조합명 == "'속초시수산업협동조합'"), aes(x = 위판일자, y = 물량.KG.)) + geom_line(col = "red")


ggplot(fish_crab %>% filter(as.integer(수산물표준코드명) %in% fish_c_ordered_6), aes(x = 위판일자, y = 물량.KG.)) + geom_point(aes(color = 수산물표준코드명)) 
ggplot(fish_crab %>% filter(수산물표준코드명 == "'꽃게'"), aes(x = 위판일자, y = 물량.KG.)) + geom_point(aes(color = 수산물표준코드명)) +
  geom_smooth(method = lm, formula = y~poly(x,1))+ ggtitle("Amount of consigment sale of gazami crab")

fcrab_re <- fish_crab_re %>%
  filter(수산물표준코드명 == "'꽃게'")
ggplot(fcrab_re, aes(x = 위판일자, y = 물량.KG.)) + geom_line(aes(color = 산지조합명))
ggplot(fcrab_re %>% filter(산지조합명 == "'속초시수산업협동조합'"), aes(x = 위판일자, y = 물량.KG.)) + geom_line(col = "red")


library(dplyr)
library(ggplot2)
library(reshape2)
library(zoo)

fishery_data <- read.csv("data.csv")
fishery <- fishery_data %>% melt(id.vars=1:6)
View(fishery_data)
View(fishery)

####어업종류별 생산 조사####

#생산량
fishery %>%
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  ggplot() + aes(x=type, weight=value, fill=variable) + geom_bar()

#생산금액
fishery %>%
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  ggplot() + aes(x=type, weight=value, fill=variable) + geom_bar()

#가성비
split <- fishery %>% split(fishery$variable)
amount <- rbind(split[[1]], split[[2]], split[[3]]) %>%
  group_by(type) %>%
  summarize(value=sum(value, na.rm=TRUE))
price <- rbind(split[[4]], split[[5]], split[[6]]) %>%
  group_by(type) %>%
  summarize(value=sum(value, na.rm=TRUE))
data.frame(type=amount$type, value=price$value/amount$value) %>%
  ggplot() + aes(x=type, weight=value) + geom_bar()


####지역별 생산 조사: 국내####

#생산량
fishery %>%
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  filter(location != "태평양", location != "인도양", location != "대서양", location != "남빙양") %>%
  arrange(desc(value)) %>%
  ggplot() + aes(y=reorder(location, value), weight=value, fill=variable) + geom_bar()

#생산금액
fishery %>%
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  filter(location != "태평양", location != "인도양", location != "대서양", location != "남빙양") %>%
  arrange(desc(value)) %>%
  ggplot() + aes(y=reorder(location, value), weight=value, fill=variable) + geom_bar()

####지역별 생산 조사: 원양####

#생산량
fishery %>%
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  filter(location == "태평양" | location == "인도양" | location == "대서양" | location == "남빙양") %>%
  arrange(desc(value)) %>%
  ggplot() + aes(y=reorder(location, value), weight=value, fill=variable) + geom_bar()

#생산금액
fishery %>%
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  filter(location == "태평양" | location == "인도양" | location == "대서양" | location == "남빙양") %>%
  arrange(desc(value)) %>%
  ggplot() + aes(y=reorder(location, value), weight=value, fill=variable) + geom_bar()

####시간별 생산 조사####

#연도별 생산량
fishery %>% #variable별
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  filter(time %/% 100 < 2020) %>%
  group_by(year = time %/% 100, variable) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot + aes(x=year, y=value, color=variable) + geom_line() + geom_point()
fishery %>% #총계
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  filter(time %/% 100 < 2020) %>%
  group_by(year = time %/% 100) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot + aes(x=year, y=value) + geom_line() + geom_point()

#연도별 생산금액
fishery %>% #variable별
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  filter(time %/% 100 < 2020) %>%
  group_by(year = time %/% 100, variable) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot + aes(x=year, y=value, color=variable) + geom_line()
fishery %>% #총계
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  filter(time %/% 100 < 2020) %>%
  group_by(year = time %/% 100) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot + aes(x=year, y=value) + geom_line()

#월별 생산량
fishery %>% #variable별
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  mutate(time = as.Date(paste0(time %/% 100, "-", time %% 100, "-01"))) %>%
  group_by(variable, time) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot + aes(x=time, y=value, color=variable) + geom_line() + facet_grid(~variable)
fishery %>% #총계
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  mutate(time = as.Date(paste0(time %/% 100, "-", time %% 100, "-01"))) %>%
  group_by(time) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot + aes(x=time, y=value) + geom_line()
fishery %>% #heatmap
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  group_by(year = time %/% 100, month = time %% 100) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot + aes(x=year, y=month, fill=value) + geom_tile() + scale_fill_viridis_c()

#월별 생산금액
fishery %>% #variable별
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  mutate(time = as.Date(paste0(time %/% 100, "-", time %% 100, "-01"))) %>%
  group_by(variable, time) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot + aes(x=time, y=value, color=variable) + geom_line() +facet_grid(~variable)
fishery %>% #총계
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  mutate(time = as.Date(paste0(time %/% 100, "-", time %% 100, "-01"))) %>%
  group_by(time) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot + aes(x=time, y=value) + geom_line()
fishery %>% #heatmap
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  group_by(year = time %/% 100, month = time %% 100) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot + aes(x=year, y=month, fill=value) + geom_tile() + scale_fill_viridis_c()


####판매방식별 생산 조사####

#생산량
fishery %>%
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  ggplot() + aes(x=market, weight=value, fill=variable) + geom_bar()

#생산금액
fishery %>%
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  ggplot() + aes(x=market, weight=value, fill=variable) + geom_bar()

####지역별 품종 조사: 국내####

#생산량 기준
fishery %>%
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  filter(location != "태평양", location != "인도양", location != "대서양", location != "남빙양") %>%
  group_by(location, species) %>%
  summarize(value=sum(value, na.rm=TRUE)) %>%
  top_n(n=5, wt=value) %>%
  group_by(location) %>%
  arrange(desc(value), .by_group=TRUE) %>%
  ggplot() + aes(y=location, weight=value, fill=species) + geom_bar()

#생산금액 기준
fishery %>%
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  filter(location != "태평양", location != "인도양", location != "대서양", location != "남빙양") %>%
  group_by(location, species) %>%
  summarize(value=sum(value, na.rm=TRUE)) %>%
  top_n(n=5, wt=value) %>%
  group_by(location) %>%
  arrange(desc(value), .by_group=TRUE) %>%
  ggplot() + aes(y=location, weight=value, fill=species) + geom_bar()

####지역별 품종 조사: 원양####

#생산량 기준
fishery %>%
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  filter(location == "태평양" | location == "인도양" | location == "대서양" | location == "남빙양") %>%
  group_by(location, species) %>%
  summarize(value=sum(value, na.rm=TRUE)) %>%
  top_n(n=5, wt=value) %>%
  group_by(location) %>%
  arrange(desc(value), .by_group=TRUE) %>%
  ggplot() + aes(y=location, weight=value, fill=species) + geom_bar()

#생산금액 기준
fishery %>%
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  filter(location == "태평양" | location == "인도양" | location == "대서양" | location == "남빙양") %>%
  group_by(location, species) %>%
  summarize(value=sum(value, na.rm=TRUE)) %>%
  top_n(n=5, wt=value) %>%
  group_by(location) %>%
  arrange(desc(value), .by_group=TRUE) %>%
  ggplot() + aes(y=location, weight=value, fill=species) + geom_bar()

####지역별 판매방식 조사: 국내####

#생산량 기준
fishery %>%
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  filter(location != "태평양", location != "인도양", location != "대서양", location != "남빙양") %>%
  group_by(location, market) %>%
  summarize(value=sum(value, na.rm=TRUE)) %>%
  ggplot() + aes(y=location, weight=value, fill=market) + geom_bar()

#생산금액 기준
fishery %>%
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  filter(location != "태평양", location != "인도양", location != "대서양", location != "남빙양") %>%
  group_by(location, market) %>%
  summarize(value=sum(value, na.rm=TRUE)) %>%
  ggplot() + aes(y=location, weight=value, fill=market) + geom_bar()

#원양은 전부 비계통판매임.


####품종별 판매방식 조사####

#생산량 기준
fishery %>%
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  group_by(species, market) %>%
  summarize(value=sum(value, na.rm=TRUE))
  ggplot() + aes(y=reorder(species, value), weight=value, fill=market) + geom_bar()

#생산량 기준
fishery %>%
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  group_by(species, market) %>%
  summarize(value=sum(value, na.rm=TRUE)) %>%
  ggplot() + aes(y=reorder(species, value), weight=value, fill=market) + geom_bar()

####생산량과 생산금액####

##amount vs. price plot##
#종합
fishery_data %>%
  group_by(amount_live, amount_fresh, amount_frozen, price_live, price_fresh, price_frozen) %>%
  summarize() %>%
  ggplot() + geom_point(aes(x=amount_live, y=price_live), color="coral") + geom_point(aes(x=amount_live, y=price_live), color="slateblue") + geom_point(aes(x=amount_live, y=price_live), color="green")

#합계
fishery_data %>%
  group_by(amount_live, amount_fresh, amount_frozen, price_live, price_fresh, price_frozen) %>%
  summarize(amount=sum(amount_live, amount_fresh, amount_frozen, na.rm=TRUE),
            price=sum(price_live, price_fresh, price_frozen, na.rm=TRUE)) %>%
  ggplot(aes(x=amount, y=price)) + geom_point()

#활어
fishery_data %>%
  ggplot(aes(x=amount_live, y=price_live)) + geom_point()

#선어
fishery_data %>%
  ggplot(aes(x=amount_fresh, y=price_fresh)) + geom_point()

#냉동
fishery_data %>%
  ggplot(aes(x=amount_frozen, y=price_frozen)) + geom_point()

####시간별 특정 종 생산####
fishery %>%
  filter(speciescode == "'140418") %>%
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  mutate(year=time%/%100, month=time%%100, ym=as.yearmon(paste0(year, "-", month))) %>%
  ggplot(aes(x=ym, y=value)) + geom_point()
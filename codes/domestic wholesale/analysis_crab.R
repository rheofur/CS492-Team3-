library(dplyr)
library(ggplot2)
library(reshape2)

fishery_data <- read.csv("data.csv")
View(fishery_data)

fishery_cast <- fishery_data %>%
  filter(grepl("게", species), species != "성게류") 
fishery <- fishery_cast %>% melt(id.vars=1:6)
View(fishery)

####지역별 게 생산 동향####

##생산량##
fishery %>%
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  ggplot() + aes(y=location, weight=value, fill=species) + geom_bar()

##생산금액##
fishery %>%
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  ggplot() + aes(y=location, weight=value, fill=species) + geom_bar()

####시간별 게 생산 동향####

##연간 생산량##
fishery %>% #품종별 종합
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  filter(time%/%100 < 2020) %>%
  group_by(year=time%/%100, species) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot() + aes(x=year, y=value, color=species) + geom_line() + geom_point()
for (n in levels(factor(fishery$species))) { #품종별
  print(fishery %>%
          filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
          filter(time%/%100 < 2020) %>%
          filter(species == n) %>%
          group_by(year=time%/%100, species) %>%
          summarize(value = sum(value, na.rm=TRUE)) %>%
          ggplot() + aes(x=factor(year), y=value) + geom_line() + geom_point() + facet_grid(~species))
}
fishery %>% #계
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  filter(time%/%100 < 2020) %>%
  group_by(year=time%/%100) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot() + aes(x=year, y=value) + geom_line() + geom_point()

##월간 생산량##
fishery %>% #품종별 종합
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  mutate(time = as.Date(paste0(time%/%100, "-", time%%100, "-01"), "%Y-%m-%d")) %>%
  group_by(time, species) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot() + aes(x=time, y=value, color=species) + geom_line() + facet_grid(~species)
for (n in levels(factor(fishery$species))) { #품종별
  print(fishery %>% #line graph
          filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
          filter(species == n) %>%
          mutate(time = as.Date(paste0(time%/%100, "-", time%%100, "-01"), "%Y-%m-%d")) %>%
          group_by(time, species) %>%
          summarize(value = sum(value, na.rm=TRUE)) %>%
          ggplot() + aes(x=time, y=value) + geom_line() + facet_grid(~species))
  print(fishery %>% #heatmap
          filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
          filter(species == n) %>%
          group_by(year=time%/%100, month=time%%100, species) %>%
          summarize(value = sum(value, na.rm=TRUE)) %>%
          ggplot() + aes(x=year, y=desc(month), fill=value) + geom_tile() + facet_grid(~species))
}
fishery %>% #계
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  mutate(time = as.Date(paste0(time%/%100, "-", time%%100, "-01"), "%Y-%m-%d")) %>%
  group_by(time) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot() + aes(x=time, y=value) + geom_line()
fishery %>% #계 heatmap
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  group_by(year=time%/%100, month=time%%100) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot() + aes(x=year, y=desc(month), fill=value) + geom_tile()

##연간 생산금액##
fishery %>% #품종별 종합
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  filter(time%/%100 < 2020) %>%
  group_by(year=time%/%100, species) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot() + aes(x=year, y=value, color=species) + geom_line() + geom_point() + facet_grid(~species)
for (n in levels(factor(fishery$species))) { #품종별
  print(fishery %>%
          filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
          filter(time%/%100 < 2020) %>%
          filter(species == n) %>%
          group_by(year=time%/%100, species) %>%
          summarize(value = sum(value, na.rm=TRUE)) %>%
          ggplot() + aes(x=year, y=value) + geom_line() + geom_point() + facet_grid(~species))
}
fishery %>% #계
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  filter(time%/%100 < 2020) %>%
  group_by(year=time%/%100) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot() + aes(x=year, y=value) + geom_line() + geom_point()

##월간 생산금액##
fishery %>% #품종별 종합
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  mutate(time = as.Date(paste0(time%/%100, "-", time%%100, "-01"), "%Y-%m-%d")) %>%
  group_by(time, species) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot() + aes(x=time, y=value, color=species) + geom_line() + facet_grid(~species)
for (n in levels(factor(fishery$species))) { #품종별
  print(fishery %>% #line graph
          filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
          filter(species == n) %>%
          mutate(time = as.Date(paste0(time%/%100, "-", time%%100, "-01"), "%Y-%m-%d")) %>%
          group_by(time, species) %>%
          summarize(value = sum(value, na.rm=TRUE)) %>%
          ggplot() + aes(x=time, y=value) + geom_line() + facet_grid(~species))
  print(fishery %>% #heatmap
          filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
          filter(species == n) %>%
          group_by(year=time%/%100, month=time%%100, species) %>%
          summarize(value = sum(value, na.rm=TRUE)) %>%
          ggplot() + aes(x=year, y=desc(month), fill=value) + geom_tile() + facet_grid(~species))
}
fishery %>% #계
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  mutate(time = as.Date(paste0(time%/%100, "-", time%%100, "-01"), "%Y-%m-%d")) %>%
  group_by(time) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot() + aes(x=time, y=value) + geom_line()
fishery %>% #계 heatmap
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  group_by(year=time%/%100, month=time%%100) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ggplot() + aes(x=year, y=desc(month), fill=value) + geom_tile()

##연간 가격/양 비율##
fishery_cast %>% #품종별 종합
  filter(time%/%100 < 2020) %>%
  group_by(year=time%/%100, species) %>%
  summarize(value = (sum(price_live, na.rm=T)+sum(price_fresh, na.rm=T)+sum(price_frozen, na.rm=T))/(sum(amount_live, na.rm=T)+sum(amount_fresh, na.rm=T)+sum(amount_frozen, na.rm=T))) %>%
  ggplot() + aes(x=year, y=value, color=species) + geom_line() + geom_point() + facet_grid(~species)
for (n in levels(factor(fishery$species))) { #품종별
  print(fishery_cast %>% #품종별 종합
          filter(time%/%100 < 2020) %>%
          filter(species == n) %>%
          group_by(year=time%/%100, species) %>%
          summarize(value = (sum(price_live, na.rm=T)+sum(price_fresh, na.rm=T)+sum(price_frozen, na.rm=T))/(sum(amount_live, na.rm=T)+sum(amount_fresh, na.rm=T)+sum(amount_frozen, na.rm=T))) %>%
          ggplot() + aes(x=year, y=value) + geom_line() + geom_point() + facet_grid(~species))
}
fishery_cast %>% #계
  filter(time%/%100 < 2020) %>%
  group_by(year=time%/%100) %>%
  summarize(value = (sum(price_live, na.rm=T)+sum(price_fresh, na.rm=T)+sum(price_frozen, na.rm=T))/(sum(amount_live, na.rm=T)+sum(amount_fresh, na.rm=T)+sum(amount_frozen, na.rm=T))) %>%
  ggplot() + aes(x=year, y=value) + geom_line() + geom_point()

##월간 가격/양 비율##
fishery_cast %>% #품종별 종합
  mutate(time = as.Date(paste0(time%/%100, "-", time%%100, "-01"))) %>%
  group_by(time, species) %>%
  summarize(value = (sum(price_live, na.rm=T)+sum(price_fresh, na.rm=T)+sum(price_frozen, na.rm=T))/(sum(amount_live, na.rm=T)+sum(amount_fresh, na.rm=T)+sum(amount_frozen, na.rm=T))) %>%
  ggplot() + aes(x=time, y=value, color=species) + geom_line() + facet_grid(~species)
for (n in levels(factor(fishery$species))) { #품종별
  print(fishery_cast %>% #line graph
          filter(species == n) %>%
          mutate(time = as.Date(paste0(time%/%100, "-", time%%100, "-01"))) %>%
          group_by(time, species) %>%
          summarize(value = (sum(price_live, na.rm=T)+sum(price_fresh, na.rm=T)+sum(price_frozen, na.rm=T))/(sum(amount_live, na.rm=T)+sum(amount_fresh, na.rm=T)+sum(amount_frozen, na.rm=T))) %>%
          ggplot() + aes(x=time, y=value) + geom_line() + facet_grid(~species))
  print(fishery_cast %>% #heatmap
          filter(species == n) %>%
          group_by(year=time%/%100, month=time%%100, species) %>%
          summarize(value = (sum(price_live, na.rm=T)+sum(price_fresh, na.rm=T)+sum(price_frozen, na.rm=T))/(sum(amount_live, na.rm=T)+sum(amount_fresh, na.rm=T)+sum(amount_frozen, na.rm=T))) %>%
          ggplot() + aes(x=year, y=desc(month), fill=value) + geom_tile() + facet_grid(~species))
}
fishery_cast %>% #계
        mutate(time = as.Date(paste0(time%/%100, "-", time%%100, "-01"))) %>%
        group_by(time) %>%
        summarize(value = (sum(price_live, na.rm=T)+sum(price_fresh, na.rm=T)+sum(price_frozen, na.rm=T))/(sum(amount_live, na.rm=T)+sum(amount_fresh, na.rm=T)+sum(amount_frozen, na.rm=T))) %>%
        ggplot() + aes(x=time, y=value) + geom_line()
fishery_cast %>% #계 heatmap
        group_by(year=time%/%100, month=time%%100) %>%
        summarize(value = (sum(price_live, na.rm=T)+sum(price_fresh, na.rm=T)+sum(price_frozen, na.rm=T))/(sum(amount_live, na.rm=T)+sum(amount_fresh, na.rm=T)+sum(amount_frozen, na.rm=T))) %>%
        ggplot() + aes(x=year, y=desc(month), fill=value) + geom_tile()

####게 품종 별 판매방식 동향####

##생산량 기준##
fishery %>%
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  ggplot() + aes(x=species, weight=value, fill=market) + geom_bar()

##생산금액 기준##
fishery %>%
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  ggplot() + aes(x=species, weight=value, fill=market) + geom_bar()

####게 품종 별 생산방식 동향####

##생산량 기준##
fishery %>%
  filter(variable == "amount_live" | variable == "amount_fresh" | variable == "amount_frozen") %>%
  ggplot() + aes(x=species, weight=value, fill=type) + geom_bar()

##생산금액 기준##
fishery %>%
  filter(variable == "price_live" | variable == "price_fresh" | variable == "price_frozen") %>%
  ggplot() + aes(x=species, weight=value, fill=type) + geom_bar()


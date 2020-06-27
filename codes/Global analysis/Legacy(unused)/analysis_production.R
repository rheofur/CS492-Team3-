library(dplyr)
library(ggplot2)

setwd("C:\\Users\\HP\\Desktop\\R proj")

production <- read.csv("Processed_production.csv", stringsAsFactors = FALSE, row.names=1)
summary(production)

##Analysis on Production data##

#A1) Analysis yearly production(in dollar) per country
country_prod <- summarise(group_by(production, Country), s13 = sum(y13, na.rm=TRUE), 
                          s14=sum(y14, na.rm=TRUE),
                          s15=sum(y15, na.rm=TRUE),
                          s16=sum(y16, na.rm=TRUE),
                          s17=sum(y17, na.rm=TRUE),
                          s18=sum(y18, na.rm=TRUE),
                          avg=mean(c(s13,s14,s15,s16,s17,s18)))

summary(country_prod)
head(arrange(country_prod, -avg), 20) #take a look at top countrys
tail(arrange(country_prod, -avg), 20) #and bottoms
arranged_country_prod <- arrange(country_prod, -avg)
#try plot top 10 countries as pie chart
ggplot(data=arranged_country_prod[1:20, ], aes(x="", y=avg, fill=Country))+
  geom_bar(stat="identity", width=1)+
  coord_polar(theta="y")

#A2) Anaylsis per CPC class
cpc_prod <- summarise(group_by(production, CPC_Class), s13 = sum(y13, na.rm=TRUE), 
                          s14=sum(y14, na.rm=TRUE),
                          s15=sum(y15, na.rm=TRUE),
                          s16=sum(y16, na.rm=TRUE),
                          s17=sum(y17, na.rm=TRUE),
                          s18=sum(y18, na.rm=TRUE),
                          avg=mean(c(s13,s14,s15,s16,s17,s18)))

summary(cpc_prod)
head(arrange(cpc_prod, -avg), 20) #sorted view
arranged_cpc_prod <- arrange(cpc_prod, -avg)
#try plot in pie chart
ggplot(data=arranged_cpc_prod[1:20, ], aes(x="", y=avg, fill=CPC_Class))+
  geom_bar(stat="identity", width=1)+
  coord_polar(theta="y")

#A3) Per each CPC class, which country produces the most? (top3 for each)
cpc_country_prod <- summarise(group_by(production, CPC_Class, Country), total=sum(y13,y14,y15,y16,y17,y18, na.rm=TRUE))
top_countries <- cpc_country_prod %>% arrange(-total) %>% group_by(CPC_Class) %>% slice(1:3)

#bar chart(partial)
ggplot(data=top_countries[1:18, ], aes(x="", y=log(total), fill=Country))+
  facet_grid(facets=. ~ CPC_Class)+
  geom_bar(stat="identity", width=1)

#A4) Which CPC class each country produces the most? (top3 for each)
country_cpc_prod <- summarise(group_by(production, Country, CPC_Class), total=sum(y13,y14,y15,y16,y17,y18, na.rm=TRUE))
top_cpc <- country_cpc_prod %>% arrange(-total) %>% group_by(Country) %>% slice(1:3)

#bar chart(partial)
ggplot(data=top_cpc[1:14, ], aes(x="", y=log(total), fill=CPC_Class))+
  facet_grid(facets=. ~ Country)+
  geom_bar(stat="identity", width=1)

#A5) Try ggmap(scalable with production of each country)

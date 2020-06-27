library(dplyr)
library(ggplot2)

setwd("C:\\Users\\HP\\Desktop\\R proj")

trade <- read.csv("Processed_trade.csv", stringsAsFactors = FALSE, row.names=1)
summary(trade)

##Analysis on Trade data##

#A1) Analysis with grouping by Trade flow for each country
flow_country_trade <- summarise(group_by(trade, Trade_Flow, Country), s13 = sum(y13, na.rm=TRUE), 
                          s14=sum(y14, na.rm=TRUE),
                          s15=sum(y15, na.rm=TRUE),
                          s16=sum(y16, na.rm=TRUE),
                          s17=sum(y17, na.rm=TRUE),
                          avg=mean(c(s13,s14,s15,s16,s17)))

#Export & Import
flow_country_export <- filter(flow_country_trade, Trade_Flow == "Exports")
flow_country_import <- filter(flow_country_trade, Trade_Flow == "Imports")

summary(flow_country_export)
summary(flow_country_import)

#View top/bottom export/import countries
head(arrange(flow_country_export, -avg), 20)
tail(arrange(flow_country_export, -avg), 20)
head(arrange(flow_country_import, -avg), 20)
tail(arrange(flow_country_import, -avg), 20)

#plot as pie chart(for top 10)
ggplot(data=arrange(flow_country_export, -avg)[1:10, ], aes(x="", y=avg, fill=Country))+
  geom_bar(stat="identity", width=1)+
  coord_polar(theta="y")
ggplot(data=arrange(flow_country_import, -avg)[1:10, ], aes(x="", y=avg, fill=Country))+
  geom_bar(stat="identity", width=1)+
  coord_polar(theta="y")

#A2) Analysis for each CPC class
flow_cpc_trade <- summarise(group_by(trade, Trade_Flow, CPC_Class), s13 = sum(y13, na.rm=TRUE), 
                                s14=sum(y14, na.rm=TRUE),
                                s15=sum(y15, na.rm=TRUE),
                                s16=sum(y16, na.rm=TRUE),
                                s17=sum(y17, na.rm=TRUE),
                                avg=mean(c(s13,s14,s15,s16,s17)))

#Export & Import
flow_cpc_export <- filter(flow_cpc_trade, Trade_Flow == "Exports")
flow_cpc_import <- filter(flow_cpc_trade, Trade_Flow == "Imports")

summary(flow_cpc_export)
summary(flow_cpc_import)

#View top/bottom export/import classes
head(arrange(flow_cpc_export, -avg), 20)
tail(arrange(flow_cpc_export, -avg), 20)
head(arrange(flow_cpc_import, -avg), 20)
tail(arrange(flow_cpc_import, -avg), 20)

#plot as pie chart(for top 10)
ggplot(data=arrange(flow_cpc_export, -avg)[1:20, ], aes(x="", y=avg, fill=CPC_Class))+
  geom_bar(stat="identity", width=1)+
  coord_polar(theta="y")
ggplot(data=arrange(flow_cpc_import, -avg)[1:20, ], aes(x="", y=avg, fill=CPC_Class))+
  geom_bar(stat="identity", width=1)+
  coord_polar(theta="y")

#minor A1/2) What about other flows?
levels(as.factor(trade$Trade_Flow)) #looking at 'other' trade flows

#"proving" no processed production
sum(select(filter(trade, Trade_Flow == "Processed production"), y13:y17))
# Conclusion : There are no processed production in our time set

flow_country_re <- filter(flow_country_trade, Trade_Flow == "Reexports")
flow_cpc_re <- filter(flow_cpc_trade, Trade_Flow == "Reexports")

#view top members
head(arrange(flow_country_re, -avg), 10)
head(arrange(flow_cpc_re, -avg), 10)

#A3)Try country/CPC class cross analysis
s_trade <- mutate(group_by(trade, Trade_Flow, Country, CPC_Class), total=sum(y13,y14,y15,y16,y17, na.rm=TRUE))
exp <- filter(s_trade, Trade_Flow=="Exports")
imp <- filter(s_trade, Trade_Flow=="Imports")
res <- filter(s_trade, Trade_Flow=="Reexports")

#View each countries top 3 CPC classes
top_cpc_exp <- exp %>% arrange(-total) %>% group_by(Country) %>% slice(1:3)
top_cpc_imp <- imp %>% arrange(-total) %>% group_by(Country) %>% slice(1:3)
top_cpc_res <- res %>% arrange(-total) %>% group_by(Country) %>% slice(1:3)

#bar chart(partial)
ggplot(data=top_cpc_exp[1:21, ], aes(x="", y=log(total), fill=CPC_Class))+
  facet_grid(facets=. ~ Country)+
  geom_bar(stat="identity", width=1)

ggplot(data=top_cpc_imp[1:22, ], aes(x="", y=log(total), fill=CPC_Class))+
  facet_grid(facets=. ~ Country)+
  geom_bar(stat="identity", width=1)

ggplot(data=top_cpc_res[1:21, ], aes(x="", y=log(total), fill=CPC_Class))+
  facet_grid(facets=. ~ Country)+
  geom_bar(stat="identity", width=1)

#A4)Try CPC/Country class cross analysis
#View each cpc class top 3 countries
top_country_exp <- exp %>% arrange(-total) %>% group_by(CPC_Class) %>% slice(1:3)
top_country_imp <- imp %>% arrange(-total) %>% group_by(CPC_Class) %>% slice(1:3)
top_country_res <- res %>% arrange(-total) %>% group_by(CPC_Class) %>% slice(1:3)

#bar chart(partial)
ggplot(data=top_country_exp[1:21, ], aes(x="", y=log(total), fill=Country))+
  facet_grid(facets=. ~ CPC_Class)+
  geom_bar(stat="identity", width=1)

ggplot(data=top_country_imp[1:21, ], aes(x="", y=log(total), fill=Country))+
  facet_grid(facets=. ~ CPC_Class)+
  geom_bar(stat="identity", width=1)

ggplot(data=top_country_res[1:21, ], aes(x="", y=log(total), fill=Country))+
  facet_grid(facets=. ~ CPC_Class)+
  geom_bar(stat="identity", width=1)

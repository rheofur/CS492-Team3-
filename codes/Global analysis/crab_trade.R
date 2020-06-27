library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)

setwd("C:\\Users\\HP\\Desktop\\R proj\\data")

####preprocessing####

trade_ton <- as.data.frame(read.csv("HS_trade_ton(76-17).csv", na.strings='...', stringsAsFactors = FALSE))
trade_dol <- as.data.frame(read.csv("HS_trade_dollar(76-17).csv", na.strings='...', stringsAsFactors = FALSE))

#drop unnecssary columns & rename columns
p_cols <- c("Country", "Commodity", "Flow", "a", paste0('y', 1976:2017)) #a is unnecessary!
colnames(trade_ton) <- p_cols
colnames(trade_dol) <- p_cols
trade_ton <- select(trade_ton, -a)
trade_dol <- select(trade_dol, -a)
#add 'total' col
trade_ton$total <- rowSums(trade_ton[, 4:45])
trade_dol$total <- rowSums(trade_dol[, 4:45])
#drop rows where total==0
trade_ton <- filter(trade_ton, total!=0)
trade_dol <- filter(trade_dol, total!=0)
#drop 'total' row(last row)
trade_ton <- trade_ton[-nrow(trade_ton), ]
trade_dol <- trade_dol[-nrow(trade_dol), ]

##preprocessing done! save as "processed"##
write.csv(trade_ton, "Pro_trade_ton(76-17).csv")
write.csv(trade_dol, "Pro_trade_dol(76-17).csv")

rm(list=ls())

####analysis####
trade_ton <- as.data.frame(read.csv("Pro_trade_ton(76-17).csv", row.names=1))
trade_dol <- as.data.frame(read.csv("Pro_trade_dol(76-17).csv", row.names=1))

#Brief overview
summary(trade_ton)
summary(trade_dol)
str(trade_ton)
str(trade_dol)

####A0) Find 'crabs'####
spe <- as.character(levels(trade_ton$Commodity))
crabs <- spe[unique(c(grep("Crab", spe, fixed=TRUE), grep("crab", spe, fixed=TRUE)))]

####A1) View trade per country####
co_tr_ton <- trade_ton %>% group_by(Flow, Country) %>% summarise(gross_ton = sum(total))
co_tr_dol <- trade_dol %>% group_by(Flow, Country) %>% summarise(gross_dol = sum(total))
co_tr <- merge(co_tr_ton, co_tr_dol, by=c("Country", "Flow")) %>% mutate(ratio=gross_dol/gross_ton)
#Note - Processed production is only in trade_ton & trade_ton/dol doesn't match exactly
#For simplicity, first focus on common inner-joined data

#top 10 countries
#Export
p1 <- ggplot(data=filter(co_tr, Flow=='Exports'), aes(x=Country, y=gross_ton, fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Exports'), -gross_ton)$Country[10:1])+
  ggtitle("Top 10 Exporting Countries(Ton)")+
  coord_flip()

p2 <- ggplot(data=filter(co_tr, Flow=='Exports'), aes(x=Country, y=gross_dol, fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Exports'), -gross_dol)$Country[10:1])+
  ggtitle("Top 10 Exporting Countries(Dollar)")+
  coord_flip()

p3 <- ggplot(data=filter(co_tr, Flow=='Exports'), aes(x=Country, y=ratio, fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Exports'), -ratio)$Country[10:1])+
  ggtitle("Top 10 Exporting Countries(Dollar/Ton)")+
  coord_flip()

grid.arrange(p1,p2,p3, nrow=3)

#Import
p1 <- ggplot(data=filter(co_tr, Flow=='Imports'), aes(x=Country, y=gross_ton, fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Imports'), -gross_ton)$Country[10:1])+
  ggtitle("Top 10 Importing Countries(Ton)")+
  coord_flip()

p2 <- ggplot(data=filter(co_tr, Flow=='Imports'), aes(x=Country, y=gross_dol, fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Imports'), -gross_dol)$Country[10:1])+
  ggtitle("Top 10 Importing Countries(Dollar)")+
  coord_flip()

p3 <- ggplot(data=filter(co_tr, Flow=='Imports'), aes(x=Country, y=ratio, fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Imports'), -ratio)$Country[10:1])+
  ggtitle("Top 10 Importing Countries(Dollar/Ton)")+
  coord_flip()

grid.arrange(p1,p2,p3, nrow=3)

#Reexport
p1 <- ggplot(data=filter(co_tr, Flow=='Reexports'), aes(x=Country, y=gross_ton, fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Reexports'), -gross_ton)$Country[10:1])+
  ggtitle("Top 10 Reexporting Countries(Ton)")+
  coord_flip()

p2 <- ggplot(data=filter(co_tr, Flow=='Reexports'), aes(x=Country, y=gross_dol, fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Reexports'), -gross_dol)$Country[10:1])+
  ggtitle("Top 10 Reexporting Countries(Dollar)")+
  coord_flip()

p3 <- ggplot(data=filter(co_tr, Flow=='Reexports'), aes(x=Country, y=ratio, fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Reexports'), -ratio)$Country[10:1])+
  ggtitle("Top 10 Reexporting Countries(Dollar/Ton)")+
  coord_flip()

grid.arrange(p1,p2,p3, nrow=3)

#histogram
p1 <- ggplot(data=filter(co_tr, Flow=='Exports'), aes(x=log(gross_ton), y=..density..))+
  geom_histogram(bins=40, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for Exports(Ton)")

p2 <- ggplot(data=filter(co_tr, Flow=='Exports'), aes(x=log(gross_dol), y=..density..))+
  geom_histogram(bins=40, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for Exports(Dollar)")

grid.arrange(p1,p2, nrow=1)

p1 <- ggplot(data=filter(co_tr, Flow=='Imports'), aes(x=log(gross_ton), y=..density..))+
  geom_histogram(bins=40, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for Imports(Ton)")

p2 <- ggplot(data=filter(co_tr, Flow=='Imports'), aes(x=log(gross_dol), y=..density..))+
  geom_histogram(bins=40, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for Imports(Dollar)")

grid.arrange(p1,p2, nrow=1)

p1 <- ggplot(data=filter(co_tr, Flow=='Reexports'), aes(x=log(gross_ton), y=..density..))+
  geom_histogram(bins=20, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for Reexports(Ton)")

p2 <- ggplot(data=filter(co_tr, Flow=='Reexports'), aes(x=log(gross_dol), y=..density..))+
  geom_histogram(bins=20, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for Reexports(Dollar)")

grid.arrange(p1,p2, nrow=1)

#Violin Plot
p1 <-ggplot(data=co_tr, aes(x=Flow, y=log(gross_ton), fill=Flow))+
  geom_violin(alpha=0.3)+
  geom_boxplot(width=.1, fill="white")+
  ggtitle("Violin plot for each Trade(Ton)")

p2 <- ggplot(data=co_tr, aes(x=Flow, y=log(gross_dol), fill=Flow))+
  geom_violin(alpha=0.3)+
  geom_boxplot(width=.1, fill="white")+
  ggtitle("Violin plot for each Trade(Dollar)")

grid.arrange(p1,p2, nrow=1)

####'Time slice' analysis####
#in ton
t_sliced <- data.frame()
m<-1
for (i in seq(1970,2017,10)){
  if (i==2010){
    t <- trade_ton %>% select(Country, Commodity, Flow, y2010:y2017) %>%
      mutate(total = rowSums(select(., 4:10))) %>%
      group_by(Country, Flow) %>% 
      summarise(gross_ton = mean(total))
  }
  else if (i==1970){
    t <- trade_ton %>% select(Country, Commodity, Flow, y1976:y1979) %>%
      mutate(total = rowSums(select(., 4:7))) %>%
      group_by(Country, Flow) %>% 
      summarise(gross_ton = mean(total))
  }
  else{
    t <- trade_ton %>% select(Country, Commodity, Flow, paste0('y',i):paste0('y',i+9)) %>%
      mutate(total = rowSums(select(., 4:13))) %>%
      group_by(Country, Flow) %>% 
      summarise(gross_ton = mean(total))
  }
  t <- as.data.frame(t)
  t$years <- paste0(i)
  t_sliced <- rbind(t_sliced, t)
  m <- m+1
}
#check ratio
(t_sliced %>% filter(Flow=='Imports') %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_ton)))$total / 
  (t_sliced%>% filter(Flow=='Imports') %>% arrange(-gross_ton) %>% group_by(years) %>% summarise(total=sum(gross_ton)))$total * 100

ggplot(data=t_sliced %>% filter(Flow=='Imports') %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10), 
       aes(x=years, y=gross_ton, fill=Country))+
  geom_bar( stat="identity", width=0.8, color="black")+
  theme(legend.position = 'bottom')+
  ggtitle("Top 10 Importing Countries for each Decades(Ton)")+
  coord_flip()

(t_sliced %>% filter(Flow=='Exports') %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_ton)))$total / 
  (t_sliced%>% filter(Flow=='Exports') %>% arrange(-gross_ton) %>% group_by(years) %>% summarise(total=sum(gross_ton)))$total * 100

ggplot(data=t_sliced %>% filter(Flow=='Exports') %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10), 
       aes(x=years, y=gross_ton, fill=Country))+
  geom_bar( stat="identity", width=0.8, color="black")+
  theme(legend.position = 'bottom')+
  ggtitle("Top 10 Exporting Countries for each Decades(Ton)")+
  coord_flip()

(t_sliced %>% filter(Flow=='Reexports') %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_ton)))$total / 
  (t_sliced%>% filter(Flow=='Reexports') %>% arrange(-gross_ton) %>% group_by(years) %>% summarise(total=sum(gross_ton)))$total * 100

ggplot(data=t_sliced %>% filter(Flow=='Reexports') %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10), 
       aes(x=years, y=gross_ton, fill=Country))+
  geom_bar( stat="identity", width=0.8, color="black")+
  theme(legend.position = 'bottom')+
  ggtitle("Top 10 Reexporting Countries for each Decades(Ton)")+
  coord_flip()

(t_sliced %>% filter(Flow=='Processed production') %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_ton)))$total / 
  (t_sliced%>% filter(Flow=='Processed production') %>% arrange(-gross_ton) %>% group_by(years) %>% summarise(total=sum(gross_ton)))$total * 100

ggplot(data=t_sliced %>% filter(Flow=='Processed production') %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10), 
       aes(x=years, y=gross_ton, fill=Country))+
  geom_bar( stat="identity", width=0.8, color="black")+
  theme(legend.position = 'bottom')+
  ggtitle("Top 10 Processed production Countries for each Decades(Ton)")+
  coord_flip()
#in dol
t_sliced <- data.frame()
m<-1
for (i in seq(1970,2017,10)){
  if (i==2010){
    t <- trade_dol %>% select(Country, Commodity, Flow, y2010:y2017) %>%
      mutate(total = rowSums(select(., 4:10))) %>%
      group_by(Country, Flow) %>% 
      summarise(gross_dol = mean(total))
  }
  else if (i==1970){
    t <- trade_dol %>% select(Country, Commodity, Flow, y1976:y1979) %>%
      mutate(total = rowSums(select(., 4:7))) %>%
      group_by(Country, Flow) %>% 
      summarise(gross_dol = mean(total))
  }
  else{
    t <- trade_dol %>% select(Country, Commodity, Flow, paste0('y',i):paste0('y',i+9)) %>%
      mutate(total = rowSums(select(., 4:13))) %>%
      group_by(Country, Flow) %>% 
      summarise(gross_dol = mean(total))
  }
  t <- as.data.frame(t)
  t$years <- paste0(i)
  t_sliced <- rbind(t_sliced, t)
  m <- m+1
}
(t_sliced %>% filter(Flow=='Imports') %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_dol)))$total / 
  (t_sliced%>% filter(Flow=='Imports') %>% arrange(-gross_dol) %>% group_by(years) %>% summarise(total=sum(gross_dol)))$total * 100

ggplot(data=t_sliced %>% filter(Flow=='Imports') %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10), 
       aes(x=years, y=gross_dol, fill=Country))+
  geom_bar( stat="identity", width=0.8, color="black")+
  theme(legend.position = 'bottom')+
  ggtitle("Top 10 Importing Countries for each Decades(1000$)")+
  coord_flip()

(t_sliced %>% filter(Flow=='Exports') %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_dol)))$total / 
  (t_sliced%>% filter(Flow=='Exports') %>% arrange(-gross_dol) %>% group_by(years) %>% summarise(total=sum(gross_dol)))$total * 100

ggplot(data=t_sliced %>% filter(Flow=='Exports') %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10), 
       aes(x=years, y=gross_dol, fill=Country))+
  geom_bar( stat="identity", width=0.8, color="black")+
  theme(legend.position = 'bottom')+
  ggtitle("Top 10 Exporting Countries for each Decades(1000$)")+
  coord_flip()

(t_sliced %>% filter(Flow=='Reexports') %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_dol)))$total / 
  (t_sliced%>% filter(Flow=='Reexports') %>% arrange(-gross_dol) %>% group_by(years) %>% summarise(total=sum(gross_dol)))$total * 100

ggplot(data=t_sliced %>% filter(Flow=='Reexports') %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10), 
       aes(x=years, y=gross_dol, fill=Country))+
  geom_bar( stat="identity", width=0.8, color="black")+
  theme(legend.position = 'bottom')+
  ggtitle("Top 10 Reexporting Countries for each Decades(1000$)")+
  coord_flip()

#Korea specific
k_ton <- data.frame()
for (c in levels(trade_ton$Flow)){
  tc <- data.frame(matrix(colSums((trade_ton %>% filter(Country == 'Korea, Republic of' & Flow==c))[4:45])))
  colnames(tc) <- c("Tons")
  tc$years <- 1976:2017
  tc$Flow <- c
  k_ton <- rbind(k_ton, tc)
}
ggplot(data=k_ton %>% filter(Tons>0), aes(x=years, y=log(Tons), col=Flow))+
  geom_point()+
  geom_line()+
  ggtitle("Trade Flow of Korea from 1976 to 2017 (Ton)")

k_dol <- data.frame()
for (c in levels(trade_dol$Flow)){
  tc <- data.frame(matrix(colSums((trade_dol %>% filter(Country == 'Korea, Republic of' & Flow==c))[4:45])))
  colnames(tc) <- c("Dollars")
  tc$years <- 1976:2017
  tc$Flow <- c
  k_dol <- rbind(k_dol, tc)
}
ggplot(data=k_dol%>% filter(Dollars>0), aes(x=years, y=log(Dollars), col=Flow))+
  geom_point()+
  geom_line()+
  ggtitle("Trade Flow of Korea from 1976 to 2017 (1000$)")

####A2)Per commodity analysis####
cm_tr_ton <- trade_ton %>% group_by(Flow, Commodity) %>% summarise(gross_ton = sum(total))
cm_tr_dol <- trade_dol %>% group_by(Flow, Commodity) %>% summarise(gross_dol = sum(total))
cm_tr <- merge(cm_tr_ton, cm_tr_dol, by=c("Commodity", "Flow")) %>% mutate(ratio=gross_dol/gross_ton)
#Note - Processed production is only in trade_ton & trade_ton/dol doesn't match exactly
#For simplicity, first focus on common inner-joined data

#top 10 Commodities
#Export
p1 <- ggplot(data=filter(cm_tr, Flow=='Exports'), aes(x=Commodity, y=gross_ton, fill=Commodity))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Exports'), -gross_ton)$Commodity[10:1])+
  ggtitle("Top 10 Exporting Commodity(Ton)")+
  coord_flip()

p2 <- ggplot(data=filter(cm_tr, Flow=='Exports'), aes(x=Commodity, y=gross_dol, fill=Commodity))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Exports'), -gross_dol)$Commodity[10:1])+
  ggtitle("Top 10 Exporting Commodity(Dollar)")+
  coord_flip()

p3 <- ggplot(data=filter(cm_tr, Flow=='Exports'), aes(x=Commodity, y=ratio, fill=Commodity))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Exports'), -ratio)$Commodity[10:1])+
  ggtitle("Top 10 Exporting Commodity(Dollar/Ton)")+
  coord_flip()

grid.arrange(p1,p2,p3, nrow=3)

#Import
p1 <- ggplot(data=filter(cm_tr, Flow=='Imports'), aes(x=Commodity, y=gross_ton, fill=Commodity))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Imports'), -gross_ton)$Commodity[10:1])+
  ggtitle("Top 10 Importing Commodity(Ton)")+
  coord_flip()

p2 <- ggplot(data=filter(cm_tr, Flow=='Imports'), aes(x=Commodity, y=gross_dol, fill=Commodity))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Imports'), -gross_dol)$Commodity[10:1])+
  ggtitle("Top 10 Importing Commodity(Dollar)")+
  coord_flip()

p3 <- ggplot(data=filter(cm_tr, Flow=='Imports'), aes(x=Commodity, y=ratio, fill=Commodity))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Imports'), -ratio)$Commodity[10:1])+
  ggtitle("Top 10 Importing Commodity(Dollar/Ton)")+
  coord_flip()

grid.arrange(p1,p2,p3, nrow=3)

#Reexport
p1 <- ggplot(data=filter(cm_tr, Flow=='Reexports'), aes(x=Commodity, y=gross_ton, fill=Commodity))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Reexports'), -gross_ton)$Commodity[10:1])+
  ggtitle("Top 10 Reexporting Commodity(Ton)")+
  coord_flip()

p2 <- ggplot(data=filter(cm_tr, Flow=='Reexports'), aes(x=Commodity, y=gross_dol, fill=Commodity))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Reexports'), -gross_dol)$Commodity[10:1])+
  ggtitle("Top 10 Reexporting Commodity(Dollar)")+
  coord_flip()

p3 <- ggplot(data=filter(cm_tr, Flow=='Reexports'), aes(x=Commodity, y=ratio, fill=Commodity))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Reexports'), -ratio)$Commodity[10:1])+
  ggtitle("Top 10 Reexporting Commodity(Dollar/Ton)")+
  coord_flip()

grid.arrange(p1,p2,p3, nrow=3)

#histogram
p1 <- ggplot(data=filter(cm_tr, Flow=='Exports'), aes(x=log(gross_ton), y=..density..))+
  geom_histogram(bins=40, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for Exports(Ton)")

p2 <- ggplot(data=filter(cm_tr, Flow=='Exports'), aes(x=log(gross_dol), y=..density..))+
  geom_histogram(bins=40, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for Exports(Dollar)")

grid.arrange(p1,p2, nrow=1)

p1 <- ggplot(data=filter(cm_tr, Flow=='Imports'), aes(x=log(gross_ton), y=..density..))+
  geom_histogram(bins=40, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for Imports(Ton)")

p2 <- ggplot(data=filter(cm_tr, Flow=='Imports'), aes(x=log(gross_dol), y=..density..))+
  geom_histogram(bins=40, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for Imports(Dollar)")

grid.arrange(p1,p2, nrow=1)

p1 <- ggplot(data=filter(cm_tr, Flow=='Reexports'), aes(x=log(gross_ton), y=..density..))+
  geom_histogram(bins=20, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for Reexports(Ton)")

p2 <- ggplot(data=filter(cm_tr, Flow=='Reexports'), aes(x=log(gross_dol), y=..density..))+
  geom_histogram(bins=20, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for Reexports(Dollar)")

grid.arrange(p1,p2, nrow=1)

#Violin Plot
p1 <-ggplot(data=cm_tr, aes(x=Flow, y=log(gross_ton), fill=Flow))+
  geom_violin(alpha=0.3)+
  geom_boxplot(width=.1, fill="white")+
  ggtitle("Violin plot for each Trade(Ton)")

p2 <- ggplot(data=cm_tr, aes(x=Flow, y=log(gross_dol), fill=Flow))+
  geom_violin(alpha=0.3)+
  geom_boxplot(width=.1, fill="white")+
  ggtitle("Violin plot for each Trade(Dollar)")

grid.arrange(p1,p2, nrow=1)

#crabs proportion
sum(filter(cm_tr, Commodity %in% crabs)$gross_ton) / sum(cm_tr$gross_ton)
sum(filter(cm_tr, Commodity %in% crabs)$gross_dol) / sum(cm_tr$gross_dol)

#crab plots
ggplot(data=cm_tr, aes(x=Commodity, y=log(gross_ton), fill=Flow))+
  geom_bar(position="dodge",stat="identity", width=.8, col='black')+
  scale_x_discrete(limits=crabs)+
  ggtitle("Crab Trades(Ton)")+
  coord_flip() 

ggplot(data=cm_tr, aes(x=Commodity, y=log(gross_dol), fill=Flow))+
  geom_bar(position="dodge",stat="identity", width=.8, col='black')+
  scale_x_discrete(limits=crabs)+
  ggtitle("Crab Trades(Dollar)")+
  coord_flip() 

ggplot(data=cm_tr, aes(x=Commodity, y=log(ratio), fill=Flow))+
  geom_bar(position="dodge",stat="identity", width=.8, col='black')+
  scale_x_discrete(limits=crabs)+
  ggtitle("Crab Trades(Dollar/Ton)")+
  coord_flip() 

####'Time slice' analysis####
#in ton
t_sliced <- data.frame()
m<-1
for (i in seq(1970,2017,10)){
  if (i==2010){
    t <- trade_ton %>% filter(Commodity %in% crabs) %>%
      select(Country, Commodity, Flow, y2010:y2017) %>%
      mutate(total = rowSums(select(., 4:10))) %>%
      group_by(Commodity, Flow) %>% 
      summarise(gross_ton = mean(total))
  }
  else if (i==1970){
    t <- trade_ton %>% filter(Commodity %in% crabs) %>%
      select(Country, Commodity, Flow, y1976:y1979) %>%
      mutate(total = rowSums(select(., 4:7))) %>%
      group_by(Commodity, Flow) %>% 
      summarise(gross_ton = mean(total))
  }
  else{
    t <- trade_ton %>% filter(Commodity %in% crabs) %>%
      select(Country, Commodity, Flow, paste0('y',i):paste0('y',i+9)) %>%
      mutate(total = rowSums(select(., 4:13))) %>%
      group_by(Commodity, Flow) %>% 
      summarise(gross_ton = mean(total))
  }
  t <- as.data.frame(t)
  t$years <- paste0(i)
  t_sliced <- rbind(t_sliced, t)
  m <- m+1
}
ggplot(data=t_sliced %>% filter(Flow=='Imports') %>% arrange(-gross_ton) %>% group_by(years), 
       aes(x=years, y=gross_ton, fill=Commodity))+
  geom_bar(stat="identity", width=0.8, color="black")+
  theme(legend.position = 'bottom')+
  ggtitle("Crab Imports for each Decades(Ton)")+
  coord_flip()

ggplot(data=t_sliced %>% filter(Flow=='Exports') %>% arrange(-gross_ton) %>% group_by(years), 
       aes(x=years, y=gross_ton, fill=Commodity))+
  geom_bar(stat="identity", width=0.8, color="black")+
  theme(legend.position = 'bottom')+
  ggtitle("Crab Exports for each Decades(Ton)")+
  coord_flip()

ggplot(data=t_sliced %>% filter(Flow=='Reexports') %>% arrange(-gross_ton) %>% group_by(years), 
       aes(x=years, y=gross_ton, fill=Commodity))+
  geom_bar(stat="identity", width=0.8, color="black")+
  theme(legend.position = 'bottom')+
  ggtitle("Crab Reexports for each Decades(Ton)")+
  coord_flip()

ggplot(data=t_sliced %>% filter(Flow=='Processed production') %>% arrange(-gross_ton) %>% group_by(years), 
       aes(x=years, y=gross_ton, fill=Commodity))+
  geom_bar(stat="identity", width=0.8, color="black")+
  theme(legend.position = 'bottom')+
  ggtitle("Crab Processed productions for each Decades(Ton)")+
  coord_flip()
#in dol
t_sliced <- data.frame()
m<-1
for (i in seq(1970,2017,10)){
  if (i==2010){
    t <- trade_dol %>% filter(Commodity %in% crabs) %>%
      select(Country, Commodity, Flow, y2010:y2017) %>%
      mutate(total = rowSums(select(., 4:10))) %>%
      group_by(Commodity, Flow) %>% 
      summarise(gross_dol = mean(total))
  }
  else if (i==1970){
    t <- trade_dol %>% filter(Commodity %in% crabs) %>%
      select(Country, Commodity, Flow, y1976:y1979) %>%
      mutate(total = rowSums(select(., 4:7))) %>%
      group_by(Commodity, Flow) %>% 
      summarise(gross_dol = mean(total))
  }
  else{
    t <- trade_dol %>% filter(Commodity %in% crabs) %>%
      select(Country, Commodity, Flow, paste0('y',i):paste0('y',i+9)) %>%
      mutate(total = rowSums(select(., 4:13))) %>%
      group_by(Commodity, Flow) %>% 
      summarise(gross_dol = mean(total))
  }
  t <- as.data.frame(t)
  t$years <- paste0(i)
  t_sliced <- rbind(t_sliced, t)
  m <- m+1
}
ggplot(data=t_sliced %>% filter(Flow=='Imports') %>% arrange(-gross_dol) %>% group_by(years), 
       aes(x=years, y=gross_dol, fill=Commodity))+
  geom_bar(stat="identity", width=0.8, color="black")+
  theme(legend.position = 'bottom')+
  ggtitle("Crab Imports for each Decades(1000$)")+
  coord_flip()

ggplot(data=t_sliced %>% filter(Flow=='Exports') %>% arrange(-gross_dol) %>% group_by(years), 
       aes(x=years, y=gross_dol, fill=Commodity))+
  geom_bar(stat="identity", width=0.8, color="black")+
  theme(legend.position = 'bottom')+
  ggtitle("Crab Exports for each Decades(1000$)")+
  coord_flip()

ggplot(data=t_sliced %>% filter(Flow=='Reexports') %>% arrange(-gross_dol) %>% group_by(years), 
       aes(x=years, y=gross_dol, fill=Commodity))+
  geom_bar(stat="identity", width=0.8, color="black")+
  theme(legend.position = 'bottom')+
  ggtitle("Crab Reexports for each Decades(1000$)")+
  coord_flip()

#Korea specific
trade_ton %>% filter(Country == 'Korea, Republic of'& Commodity %in% crabs)
k_ton <- data.frame()
for (f in levels(trade_ton$Flow)){
  for (c in crabs){
    tc <- data.frame(matrix(colSums((trade_ton %>% filter(Flow == f & Country == 'Korea, Republic of' & Commodity == c))[4:45])))
    colnames(tc) <- c("Tons")
    tc$years <- 1976:2017
    tc$Flow <- f
    tc$Commodity <- c
    k_ton <- rbind(k_ton, tc)
  }
}

ind <- 4
ggplot(data=k_ton %>% filter(Commodity==crabs[ind] & Tons>0), aes(x=years, y=Tons, col=Flow))+
  geom_point()+
  geom_line()+
  ggtitle(paste(crabs[ind], "Trades in Korea from 1976 to 2017 (Ton)"))

k_dol <- data.frame()
for (f in levels(trade_dol$Flow)){
  for (c in crabs){
    tc <- data.frame(matrix(colSums((trade_dol %>% filter(Flow == f & Country == 'Korea, Republic of' & Commodity == c))[4:45])))
    colnames(tc) <- c("Dollars")
    tc$years <- 1976:2017
    tc$Flow <- f
    tc$Commodity <- c
    k_dol <- rbind(k_dol, tc)
  }
}

ind <- 4
ggplot(data=k_dol %>% filter(Commodity==crabs[ind] & Dollars>0), aes(x=years, y=Dollars, col=Flow))+
  geom_point()+
  geom_line()+
  ggtitle(paste(crabs[ind], "Trades in Korea from 1976 to 2017 (1000$)"))

#global
k_ton <- data.frame()
for (f in levels(trade_ton$Flow)){
  for (c in crabs){
    tc <- data.frame(matrix(colSums((trade_ton %>% filter(Flow == f & Commodity == c))[4:45])))
    colnames(tc) <- c("Tons")
    tc$years <- 1976:2017
    tc$Flow <- f
    tc$Commodity <- c
    k_ton <- rbind(k_ton, tc)
  }
}

ind <- 4
ggplot(data=k_ton %>% filter(Commodity==crabs[ind] & Tons>0), aes(x=years, y=log(Tons), col=Flow))+
  geom_point()+
  geom_line()+
  ggtitle(paste(crabs[ind], "Trades from 1976 to 2017 (Ton)"))

k_dol <- data.frame()
for (f in levels(trade_dol$Flow)){
  for (c in crabs){
    tc <- data.frame(matrix(colSums((trade_dol %>% filter(Flow == f & Commodity == c))[4:45])))
    colnames(tc) <- c("Dollars")
    tc$years <- 1976:2017
    tc$Flow <- f
    tc$Commodity <- c
    k_dol <- rbind(k_dol, tc)
  }
}

ind <- 4
ggplot(data=k_dol %>% filter(Commodity==crabs[ind] & Dollars>0), aes(x=years, y=log(Dollars), col=Flow))+
  geom_point()+
  geom_line()+
  ggtitle(paste(crabs[ind], "Trades from 1976 to 2017 (1000$)"))

####A3) Per Commodity/country analysis####
cm_co_ton <- trade_ton %>% group_by(Flow, Commodity, Country) %>% summarise(gross_ton = sum(total))
cm_co_dol <- trade_dol %>% group_by(Flow, Commodity, Country) %>% summarise(gross_dol = sum(total))
cm_co <- merge(cm_co_ton, cm_co_dol, by=c("Flow","Commodity", "Country")) %>% mutate(ratio=gross_dol/gross_ton)

#take a look at top 3 for top 10 countries
#Export
ggplot(data=cm_co %>% filter(Flow=='Exports') %>% arrange(-gross_ton) %>% group_by(Commodity) %>% slice(1:3), 
             aes(x=Commodity, y=gross_ton, fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Exports'), -gross_ton)$Commodity[10:1])+
  ggtitle("Top 3 Countries of Top 10 Exports(Ton)")+
  coord_flip()

ggplot(data=cm_co %>% filter(Flow=='Exports') %>% arrange(-gross_dol) %>% group_by(Commodity) %>% slice(1:3), 
             aes(x=Commodity, y=gross_dol, fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Exports'), -gross_dol)$Commodity[10:1])+
  ggtitle("Top 3 Countries of Top 10 Exports(Dollar)")+
  coord_flip()

ggplot(data=cm_co %>% filter(Flow=='Exports') %>% arrange(-ratio) %>% group_by(Commodity) %>% slice(1:3), 
             aes(x=Commodity, y=log(ratio), fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Exports'), -ratio)$Commodity[10:1])+
  ggtitle("Top 3 Countries of Top 10 Exports(Dollar/Ton)")+
  coord_flip()

#Import
ggplot(data=cm_co %>% filter(Flow=='Imports') %>% arrange(-gross_ton) %>% group_by(Commodity) %>% slice(1:3), 
       aes(x=Commodity, y=gross_ton, fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Imports'), -gross_ton)$Commodity[10:1])+
  ggtitle("Top 3 Countries of Top 10 Imports(Ton)")+
  coord_flip()

ggplot(data=cm_co %>% filter(Flow=='Imports') %>% arrange(-gross_dol) %>% group_by(Commodity) %>% slice(1:3), 
       aes(x=Commodity, y=gross_dol, fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Imports'), -gross_dol)$Commodity[10:1])+
  ggtitle("Top 3 Countries of Top 10 Imports(Dollar)")+
  coord_flip()

ggplot(data=cm_co %>% filter(Flow=='Imports') %>% arrange(-ratio) %>% group_by(Commodity) %>% slice(1:3), 
       aes(x=Commodity, y=log(ratio), fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Imports'), -ratio)$Commodity[10:1])+
  ggtitle("Top 3 Countries of Top 10 Imports(Dollar/Ton)")+
  coord_flip()

#Reexport
ggplot(data=cm_co %>% filter(Flow=='Reexports') %>% arrange(-gross_ton) %>% group_by(Commodity) %>% slice(1:3), 
       aes(x=Commodity, y=gross_ton, fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Reexports'), -gross_ton)$Commodity[10:1])+
  ggtitle("Top 3 Countries of Top 10 Reexports(Ton)")+
  coord_flip()

ggplot(data=cm_co %>% filter(Flow=='Reexports') %>% arrange(-gross_dol) %>% group_by(Commodity) %>% slice(1:3), 
       aes(x=Commodity, y=gross_dol, fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Reexports'), -gross_dol)$Commodity[10:1])+
  ggtitle("Top 3 Countries of Top 10 Reexports(Dollar)")+
  coord_flip()

ggplot(data=cm_co %>% filter(Flow=='Reexports') %>% arrange(-ratio) %>% group_by(Commodity) %>% slice(1:3), 
       aes(x=Commodity, y=log(ratio), fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(cm_tr, Flow=='Reexports'), -ratio)$Commodity[10:1])+
  ggtitle("Top 3 Countries of Top 10 Reexports(Dollar/Ton)")+
  coord_flip()

#take a look at crabs only
crabs_cm_co <- filter(cm_co, Commodity %in% crabs)

#Since there are 'too much' countries, we only look top 3 countries
#exports
ggplot(data=crabs_cm_co %>% filter(Flow=='Exports') %>% arrange(-gross_ton) %>% group_by(Commodity) %>% slice(1:3)
       ,aes(x=Commodity, y=log(gross_ton), fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Top 3 Countries of Crab Exports(Ton)")+
  coord_flip()

ggplot(data=crabs_cm_co %>% filter(Flow=='Exports') %>% arrange(-gross_dol) %>% group_by(Commodity) %>% slice(1:3)
       ,aes(x=Commodity, y=log(gross_dol), fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Top 3 Countries of Crab Exports(Dollar)")+
  coord_flip()

ggplot(data=crabs_cm_co %>% filter(Flow=='Exports') %>% arrange(-ratio) %>% group_by(Commodity) %>% slice(1:3)
       ,aes(x=Commodity, y=log(ratio), fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Top 3 Countries of Crab Exports(Dollar/Ton)")+
  coord_flip()

#imports
ggplot(data=crabs_cm_co %>% filter(Flow=='Imports') %>% arrange(-gross_ton) %>% group_by(Commodity) %>% slice(1:3)
       ,aes(x=Commodity, y=log(gross_ton), fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Top 3 Countries of Crab Imports(Ton)")+
  coord_flip()

ggplot(data=crabs_cm_co %>% filter(Flow=='Imports') %>% arrange(-gross_dol) %>% group_by(Commodity) %>% slice(1:3)
       ,aes(x=Commodity, y=log(gross_dol), fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Top 3 Countries of Crab Imports(Dollar)")+
  coord_flip()

ggplot(data=crabs_cm_co %>% filter(Flow=='Imports') %>% arrange(-ratio) %>% group_by(Commodity) %>% slice(1:3)
       ,aes(x=Commodity, y=log(ratio), fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Top 3 Countries of Crab Imports(Dollar/Ton)")+
  coord_flip()

#Reexport
ggplot(data=crabs_cm_co %>% filter(Flow=='Reexports') %>% arrange(-gross_ton) %>% group_by(Commodity) %>% slice(1:3)
       ,aes(x=Commodity, y=log(gross_ton), fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Top 3 Countries of Crab Reexports(Ton)")+
  coord_flip()

ggplot(data=crabs_cm_co %>% filter(Flow=='Reexports') %>% arrange(-gross_dol) %>% group_by(Commodity) %>% slice(1:3)
       ,aes(x=Commodity, y=log(gross_dol), fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Top 3 Countries of Crab Reexports(Dollar)")+
  coord_flip()

ggplot(data=crabs_cm_co %>% filter(Flow=='Reexports') %>% arrange(-ratio) %>% group_by(Commodity) %>% slice(1:3)
       ,aes(x=Commodity, y=log(ratio), fill=Country))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Top 3 Countries of Crab Reexports(Dollar/Ton)")+
  coord_flip()

#####A4)Per country/species analysis#####
co_cm_ton <- trade_ton %>% group_by(Flow, Country, Commodity) %>% summarise(gross_ton = sum(total))
co_cm_dol <- trade_dol %>% group_by(Flow, Country, Commodity) %>% summarise(gross_dol = sum(total))
co_cm <- merge(co_cm_ton, co_cm_dol, by=c("Flow","Country", "Commodity")) %>% mutate(ratio=gross_dol/gross_ton)

#take a look at top 3 for top 10 countries
#Export
ggplot(data=co_cm %>% filter(Flow=='Exports') %>% arrange(-gross_ton) %>% group_by(Country) %>% slice(1:3), 
       aes(x=Country, y=gross_ton, fill=Commodity))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Exports'), -gross_ton)$Country[10:1])+
  ggtitle("Top 3 Exports of Top 10 Countries(Ton)")+
  coord_flip()

ggplot(data=co_cm %>% filter(Flow=='Exports') %>% arrange(-gross_dol) %>% group_by(Country) %>% slice(1:3), 
       aes(x=Country, y=gross_dol, fill=Commodity))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Exports'), -gross_dol)$Country[10:1])+
  ggtitle("Top 3 Exports of Top 10 Countries(Dollar)")+
  coord_flip()

ggplot(data=co_cm %>% filter(Flow=='Exports') %>% arrange(-ratio) %>% group_by(Country) %>% slice(1:3), 
       aes(x=Country, y=ratio, fill=Commodity))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Exports'), -ratio)$Country[10:1])+
  ggtitle("Top 3 Exports of Top 10 Countries(Dollar/Ton)")+
  coord_flip()

#Import
ggplot(data=co_cm %>% filter(Flow=='Imports') %>% arrange(-gross_ton) %>% group_by(Country) %>% slice(1:3), 
       aes(x=Country, y=gross_ton, fill=Commodity))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Imports'), -gross_ton)$Country[10:1])+
  ggtitle("Top 3 Imports of Top 10 Countries(Ton)")+
  coord_flip()

ggplot(data=co_cm %>% filter(Flow=='Imports') %>% arrange(-gross_dol) %>% group_by(Country) %>% slice(1:3), 
       aes(x=Country, y=gross_dol, fill=Commodity))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Imports'), -gross_dol)$Country[10:1])+
  ggtitle("Top 3 Imports of Top 10 Countries(Dollar)")+
  coord_flip()

ggplot(data=co_cm %>% filter(Flow=='Imports') %>% arrange(-ratio) %>% group_by(Country) %>% slice(1:3), 
       aes(x=Country, y=ratio, fill=Commodity))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Imports'), -ratio)$Country[10:1])+
  ggtitle("Top 3 Imports of Top 10 Countries(Dollar/Ton)")+
  coord_flip()

#Reexport
ggplot(data=co_cm %>% filter(Flow=='Reexports') %>% arrange(-gross_ton) %>% group_by(Country) %>% slice(1:3), 
       aes(x=Country, y=gross_ton, fill=Commodity))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Reexports'), -gross_ton)$Country[10:1])+
  ggtitle("Top 3 Reexports of Top 10 Countries(Ton)")+
  coord_flip()

ggplot(data=co_cm %>% filter(Flow=='Reexports') %>% arrange(-gross_dol) %>% group_by(Country) %>% slice(1:3), 
       aes(x=Country, y=gross_dol, fill=Commodity))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Reexports'), -gross_dol)$Country[10:1])+
  ggtitle("Top 3 Reexports of Top 10 Countries(Dollar)")+
  coord_flip()

ggplot(data=co_cm %>% filter(Flow=='Reexports') %>% arrange(-ratio) %>% group_by(Country) %>% slice(1:3), 
       aes(x=Country, y=ratio, fill=Commodity))+
  geom_bar(stat="identity", width=.9)+
  scale_x_discrete(limits=arrange(filter(co_tr, Flow=='Reexports'), -ratio)$Country[10:1])+
  ggtitle("Top 3 Reexports of Top 10 Countries(Dollar/Ton)")+
  coord_flip()

####A5) Crab Analysis####
#filter crabs
crab_dol <- trade_dol %>% filter(Commodity %in% crabs)
crab_ton <- trade_ton %>% filter(Commodity %in% crabs)
crab_ratio <- merge(crab_dol, crab_ton, by=c("Country", "Commodity", "Flow")) %>%
  mutate(p13 = ifelse(y13.y==0, NaN, y13.x/y13.y), p14 = ifelse(y14.y==0, NaN, y14.x/y14.y), 
         p15 = ifelse(y15.y==0, NaN, y15.x/y15.y), p16 = ifelse(y16.y==0, NaN, y16.x/y16.y), 
         p17 = ifelse(y17.y==0, NaN, y17.x/y17.y)) %>%
  select(Country, Commodity, Flow, p13, p14, p15, p16, p17)

crab_sp <- crab_ratio %>% group_by(Commodity, Flow) %>% 
  summarise(pt13 = mean(p13, na.rm=TRUE), pt14 = mean(p14, na.rm=TRUE), pt15 = mean(p15, na.rm=TRUE),
            pt16 = mean(p16, na.rm=TRUE), pt17 = mean(p17, na.rm=TRUE))

library(tidyr)
im_crab_sp <- as.data.frame(crab_sp) %>% filter(Flow=='Imports') %>% select(-Flow)
t_crab_im <- as.data.frame(t(as.matrix(im_crab_sp %>% select(-Commodity))))
rownames(t_crab_im) <- 2013:2017
t_crab_im$year = 2013:2017
t_crab_im %>%
  gather(key, value, V1:V20) %>%
  ggplot(aes(x=year, y=value, color=key))+
  scale_color_hue(labels=im_crab_sp$Commodity)+
  geom_point()+
  geom_line()+
  ggtitle("Yearly (Dollar/Ton) value of Crab Commodity (Imports)")

ex_crab_sp <- as.data.frame(crab_sp) %>% filter(Flow=='Exports') %>% select(-Flow)
t_crab_ex <- as.data.frame(t(as.matrix(ex_crab_sp %>% select(-Commodity))))
rownames(t_crab_ex) <- 2013:2017
t_crab_ex$year = 2013:2017
t_crab_ex %>%
  gather(key, value, V1:V18) %>%
  ggplot(aes(x=year, y=value, color=key))+
  scale_color_hue(labels=ex_crab_sp$Commodity)+
  geom_point()+
  geom_line()+
  ggtitle("Yearly (Dollar/Ton) value of Crab Commodity (Exports)")

####correlation analysis####

##take a look at correlation between crab and squid
squids <- spe[unique(c(grep("squid", spe, fixed=TRUE), grep("Squid", spe, fixed=TRUE)))]
countries <- levels(trade_ton$Country)

cs_ton <- trade_ton %>% filter(Commodity %in% squids | Commodity %in% crabs)
cs_dol <- trade_dol %>% filter(Commodity %in% squids | Commodity %in% crabs)

ton_cor_list <- list()
n <- 1
for (f in levels(cs_ton$Flow)){
  cs_cor <- cs_ton %>% filter(Flow==f) %>% select(Commodity, paste0('y', 1976)) %>% 
    group_by(Commodity) %>% summarise(y1976 = sum(y1976))
  cs_cor <- as.data.frame(cs_cor)
  for(i in 1977:2017){
    temp <- cs_ton %>% filter(Flow==f) %>% select(Commodity, paste0('y', i))
    colnames(temp) <- c('Commodity', 't')
    cs_cor <- merge(cs_cor, 
                    temp %>% group_by(Commodity) %>% summarise(t=sum(t)),
                    by='Commodity')
    colnames(cs_cor)[colnames(cs_cor)=='t'] <- paste0('y', i)
  }
  coms <- cs_cor$Commodity
  cs_cor <- as.data.frame(t(as.matrix(cs_cor[2:43])))
  colnames(cs_cor) <- coms
  ton_cor_list[[n]] <- cs_cor
  n <- n + 1
}

names(ton_cor_list) <- levels(cs_ton$Flow)
str(ton_cor_list)
ggplot(data=melt(cor(ton_cor_list[[1]])), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='white')+
  scale_fill_gradient2(low='blue',high='red',mid='white',midpoint=0, limit=c(-1,1))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, vjust=1, size=10, hjust=1))+
  coord_fixed()+
  ggtitle(names(ton_cor_list)[1])

ggplot(data=melt(cor(ton_cor_list[[2]])), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='white')+
  scale_fill_gradient2(low='blue',high='red',mid='white',midpoint=0, limit=c(-1,1))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, vjust=1, size=10, hjust=1))+
  coord_fixed()+
  ggtitle(names(ton_cor_list)[2])

ggplot(data=melt(cor(ton_cor_list[[3]])), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='white')+
  scale_fill_gradient2(low='blue',high='red',mid='white',midpoint=0, limit=c(-1,1))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, vjust=1, size=10, hjust=1))+
  coord_fixed()+
  ggtitle(names(ton_cor_list)[3])

ggplot(data=melt(cor(ton_cor_list[[4]])), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='white')+
  scale_fill_gradient2(low='blue',high='red',mid='white',midpoint=0, limit=c(-1,1))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, vjust=1, size=10, hjust=1))+
  coord_fixed()+
  ggtitle(names(ton_cor_list)[4])

##Only take a look at Korea & China
cs_ton <- trade_ton %>% filter(Commodity %in% squids | Commodity %in% crabs, Country=='China' | Country=='Korea, Republic of')
cs_dol <- trade_dol %>% filter(Commodity %in% squids | Commodity %in% crabs, Country=='China' | Country=='Korea, Republic of')

ton_cor_list <- list()
n <- 1
for (f in levels(cs_ton$Flow)){
  cs_cor <- cs_ton %>% filter(Flow==f) %>% select(Commodity, paste0('y', 1976)) %>% 
    group_by(Commodity) %>% summarise(y1976 = sum(y1976))
  cs_cor <- as.data.frame(cs_cor)
  for(i in 1977:2017){
    temp <- cs_ton %>% filter(Flow==f) %>% select(Commodity, paste0('y', i))
    colnames(temp) <- c('Commodity', 't')
    cs_cor <- merge(cs_cor, 
                    temp %>% group_by(Commodity) %>% summarise(t=sum(t)),
                    by='Commodity')
    colnames(cs_cor)[colnames(cs_cor)=='t'] <- paste0('y', i)
  }
  coms <- cs_cor$Commodity
  cs_cor <- as.data.frame(t(as.matrix(cs_cor[2:43])))
  colnames(cs_cor) <- coms
  ton_cor_list[[n]] <- cs_cor
  n <- n + 1
}

names(ton_cor_list) <- levels(cs_ton$Flow)
str(ton_cor_list)
ggplot(data=melt(cor(ton_cor_list[[1]])), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='white')+
  scale_fill_gradient2(low='blue',high='red',mid='white',midpoint=0, limit=c(-1,1))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, vjust=1, size=10, hjust=1))+
  coord_fixed()+
  ggtitle(paste("Korea & China", names(ton_cor_list)[1]))

ggplot(data=melt(cor(ton_cor_list[[2]])), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='white')+
  scale_fill_gradient2(low='blue',high='red',mid='white',midpoint=0, limit=c(-1,1))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, vjust=1, size=10, hjust=1))+
  coord_fixed()+
  ggtitle(paste("Korea & China", names(ton_cor_list)[2]))

ggplot(data=melt(cor(ton_cor_list[[3]])), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='white')+
  scale_fill_gradient2(low='blue',high='red',mid='white',midpoint=0, limit=c(-1,1))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, vjust=1, size=10, hjust=1))+
  coord_fixed()+
  ggtitle(paste("Korea & China", names(ton_cor_list)[3]))

# No reexports

##Try 'whole' commodity
ton_cor_total_list <- list()
n <- 1
for (f in levels(trade_ton$Flow)){
  cs_cor <- trade_ton %>% filter(Flow==f) %>% select(Commodity, paste0('y', 1976)) %>% 
    group_by(Commodity) %>% summarise(y1976 = sum(y1976))
  cs_cor <- as.data.frame(cs_cor)
  for(i in 1977:2017){
    temp <- trade_ton %>% filter(Flow==f) %>% select(Commodity, paste0('y', i))
    colnames(temp) <- c('Commodity', 't')
    cs_cor <- merge(cs_cor, 
                    temp %>% group_by(Commodity) %>% summarise(t=sum(t)),
                    by='Commodity')
    colnames(cs_cor)[colnames(cs_cor)=='t'] <- paste0('y', i)
  }
  coms <- cs_cor$Commodity
  cs_cor <- as.data.frame(t(as.matrix(cs_cor[2:43])))
  colnames(cs_cor) <- coms
  ton_cor_total_list[[n]] <- cs_cor
  n <- n + 1
}

names(ton_cor_total_list) <- levels(cs_ton$Flow)
str(ton_cor_total_list)
ggplot(data=melt(cor(ton_cor_total_list[[1]])), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='white')+
  scale_fill_gradient2(low='blue',high='red',mid='white',midpoint=0, limit=c(-1,1))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, vjust=1, size=0, hjust=1),
        axis.text.y=element_text(angle=45, vjust=1, size=0, hjust=1))+
  coord_fixed()+
  ggtitle(names(ton_cor_total_list)[1])

##narrow down to Korea/China
ton_cor_total_list <- list()
n <- 1
for (f in levels(trade_ton$Flow)){
  cs_cor <- trade_ton %>% filter(Flow==f, Country=='China' | Country=='Korea, Republic of') %>% select(Commodity, paste0('y', 1976)) %>% 
    group_by(Commodity) %>% summarise(y1976 = sum(y1976))
  cs_cor <- as.data.frame(cs_cor)
  for(i in 1977:2017){
    temp <- trade_ton %>% filter(Flow==f, Country=='China' | Country=='Korea, Republic of') %>% select(Commodity, paste0('y', i))
    colnames(temp) <- c('Commodity', 't')
    cs_cor <- merge(cs_cor, 
                    temp %>% group_by(Commodity) %>% summarise(t=sum(t)),
                    by='Commodity')
    colnames(cs_cor)[colnames(cs_cor)=='t'] <- paste0('y', i)
  }
  coms <- cs_cor$Commodity
  cs_cor <- as.data.frame(t(as.matrix(cs_cor[2:43])))
  colnames(cs_cor) <- coms
  ton_cor_total_list[[n]] <- cs_cor
  n <- n + 1
}

names(ton_cor_total_list) <- levels(cs_ton$Flow)
str(ton_cor_total_list)
temp <- melt(cor(ton_cor_total_list[[1]]))
ggplot(data=temp, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='white')+
  scale_fill_gradient2(low='blue',high='red',mid='white',midpoint=0, limit=c(-1,1))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, vjust=1, size=0, hjust=1),
        axis.text.y=element_text(angle=45, vjust=1, size=0, hjust=1))+
  coord_fixed()+
  ggtitle(paste("Korea&China", names(ton_cor_total_list)[1]))

melt(cor(ton_cor_total_list[[1]])) %>% filter(abs(value) > 0.7 & Var1 != Var2) %>% arrange(-abs(value))
melt(cor(ton_cor_total_list[[1]])) %>% filter(Var1 != Var2) %>% arrange(-abs(value))

high_crab <- list()
for (i in 1:3){
  high_crab[[i]] <- melt(cor(ton_cor_total_list[[i]])) %>% filter(abs(value) > 0.7 & Var1 != Var2, Var1 %in% crabs) %>% arrange(-abs(value))
}
high_uninon <- merge(high_crab[[1]], high_crab[[2]], by=c('Var1', 'Var2'))

ggplot(data=trade_ton %>% filter(Commodity %in% crabs), 
       aes(x=Country, y=total, fill=Flow), group=Commodity)+
  geom_bar(stat='identity')

#just taking a look at agar
ggplot(data=trade_ton %>% filter(Commodity == 'Agar - agar'), aes(x=Country, y=total, fill=Flow))+
  geom_bar(stat='identity')

lm_temp <- ton_cor_total_list[[1]] %>% select(squids, crabs)
colnames(lm_temp) <- c('s_preserved','s_frozen','s_live','s_smoked' ,'c_preserved', 'c_smoked', 'c_live', 'c_frozen')
summary(lm(s_live ~ c_live, data=lm_temp))

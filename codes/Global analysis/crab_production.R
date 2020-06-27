library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(reshape2)

setwd("C:\\Users\\HP\\Desktop\\R proj\\data")

####preprocessing####
prod_ton <- as.data.frame(read.csv("Raw_prod_ton(50-18).csv", na.strings='...', stringsAsFactors = FALSE))
prod_dol <- as.data.frame(read.csv("Raw_prod_dollar(84-18).csv", na.strings='...', stringsAsFactors = FALSE))

#drop unnecssary columns & rename columns
p_cols <- c(c("Country", "Species", "a", "b","c","d"), paste0(rep('y', 68), 1950:2018)) #a~d are unnecessary!
colnames(prod_ton) <- p_cols
prod_ton <- select(prod_ton, -a, -b, -c, -d)
p_cols <- c(c("Country", "Species", "a", "b","c","d"), paste0(rep('y', 34), 1984:2018)) #a~d are unnecessary!
colnames(prod_dol) <- p_cols
prod_dol <- select(prod_dol, -a, -b, -c, -d)
#add 'total' col
prod_ton$total <- rowSums(prod_ton[, 3:71])
prod_dol$total <- rowSums(prod_dol[, 3:37])
#drop rows where total==0
prod_ton <- filter(prod_ton, total!=0)
prod_dol <- filter(prod_dol, total!=0)
#drop 'total' row(last row)
prod_ton <- prod_ton[-nrow(prod_ton), ]
prod_dol <- prod_dol[-nrow(prod_dol), ]

##preprocessing done! save as "processed"##
write.csv(prod_ton, "Pro_prod_ton(50-18).csv")
write.csv(prod_dol, "Pro_prod_dol(84-18).csv")

####analysis####
rm(list=ls())
prod_ton <- as.data.frame(read.csv("Pro_prod_ton(50-18).csv", row.names=1))
prod_dol <- as.data.frame(read.csv("Pro_prod_dol(84-18).csv", row.names=1))

####Brief overview####

summary(prod_ton)
summary(prod_dol)
str(prod_ton)
str(prod_dol)

#A0) Find 'crabs'
spe <- as.character(levels(prod_ton$Species))
crabs <- spe[unique(c(grep("Crab", spe, fixed=TRUE), grep("crab", spe, fixed=TRUE)))]

####A1) View total production per country####
country_prod_ton <- prod_ton %>% group_by(Country) %>% summarise(gross_ton = sum(total))
country_prod_dol <- prod_dol %>% group_by(Country) %>% summarise(gross_dol = sum(total))
#merge data for plotting & calculating ratio
country_prod <- merge(country_prod_ton, country_prod_dol, by='Country') %>% mutate(ratio = gross_dol/gross_ton)

#top 10 countries
ggplot(data=country_prod, aes(x=Country, y=log(gross_ton), fill=Country))+
  geom_bar(stat="identity", width=0.95)+
  scale_x_discrete(limits=arrange(country_prod, -gross_ton)$Country[1:10])+
  ggtitle("Top production Countries (all time, Ton)")+
  coord_flip()

ggplot(data=country_prod, aes(x=Country, y=log(gross_dol), fill=Country))+
  geom_bar(stat="identity", width=0.95)+
  scale_x_discrete(limits=arrange(country_prod, -gross_dol)$Country[1:10])+
  ggtitle("Top production Countries (all time, 1000$)")+
  coord_flip()

ggplot(data=country_prod, aes(x=Country, y=ratio, fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(country_prod, -ratio)$Country[1:10])+
  ggtitle("Barplot of top production Country(Dollar/Ton)")

#histogram
ggplot(data=country_prod, aes(x=log(gross_ton), y=..density..))+
  geom_histogram(bins=40, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.2)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Distribution of production(Ton)")

ggplot(data=country_prod, aes(x=log(gross_dol), y=..density..))+
  geom_histogram(bins=40, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.2)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Distribution of  production(1000$)")

#scatter plot
ggplot(data=country_prod, aes(x=log(gross_ton), y=log(gross_dol)))+
  geom_point(size=1)+
  stat_smooth(method=lm)+
  ggtitle("Log(gross_ton) vs Log(gross_dol)")

####'Time slice' analysis####
#in tons
t_sliced <- data.frame()
m<-1
for (i in seq(1950,2018,10)){
  if (i==2010){
    t <- prod_ton %>% select(Country, Species, y2010:y2018) %>%
      mutate(total = rowSums(select(., 3:11))) %>%
      group_by(Country) %>% 
      summarise(gross_ton = sum(total))
  }
  else{
    t <- prod_ton %>% select(Country, Species, paste0('y',i):paste0('y',i+9)) %>%
      mutate(total = rowSums(select(., 3:12))) %>%
      group_by(Country) %>% 
      summarise(gross_ton = sum(total))
  }
  t <- as.data.frame(t)
  t$years <- paste0(i)
  t_sliced <- rbind(t_sliced, t)
  m <- m+1
}
t_sliced %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_ton))
t_sliced %>% arrange(-gross_ton) %>% group_by(years) %>% summarise(total=sum(gross_ton))
#check ratio
(t_sliced %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_ton)))$total / 
  (t_sliced %>% arrange(-gross_ton) %>% group_by(years) %>% summarise(total=sum(gross_ton)))$total * 100

ggplot(data=t_sliced %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10), 
       aes(x=years, y=gross_ton, fill=Country))+
  geom_bar(position='fill', stat="identity", width=0.8, color="black")+
  ggtitle("Top 10 Production Countries for each Decades(Ton, Percentage)")+
  theme(legend.position = 'bottom')

#in dollars
t_sliced <- data.frame()
m<-1
for (i in seq(1980,2018,10)){
  if (i==2010){
    t <- prod_dol %>% select(Country, Species, y2010:y2018) %>%
      mutate(total = rowSums(select(., 3:11))) %>%
      group_by(Country) %>% 
      summarise(gross_dol = sum(total))
  }
  else if (i==1980){
    t <- prod_dol %>% select(Country, Species, y1984:y1989) %>%
      mutate(total = rowSums(select(., 3:8))) %>%
      group_by(Country) %>% 
      summarise(gross_dol = sum(total))
  }
  else{
    t <- prod_dol %>% select(Country, Species, paste0('y',i):paste0('y',i+9)) %>%
      mutate(total = rowSums(select(., 3:12))) %>%
      group_by(Country) %>% 
      summarise(gross_dol = sum(total))
  }
  t <- as.data.frame(t)
  t$years <- paste0(i)
  t_sliced <- rbind(t_sliced, t)
  m <- m+1
}
# t_sliced %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10)
# ggplot(data=t_sliced %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10), 
#        aes(x=years, y=log(gross_dol), fill=Country))+
#   geom_bar(stat="identity", width=0.9, color="white")+
#   ggtitle("Top 10 Production Countries for each Decades(1000$)")+
#   coord_flip()
t_sliced %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_dol))
t_sliced %>% arrange(-gross_dol) %>% group_by(years) %>% summarise(total=sum(gross_dol))
#check ratio
(t_sliced %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_dol)))$total / 
(t_sliced %>% arrange(-gross_dol) %>% group_by(years) %>% summarise(total=sum(gross_dol)))$total * 100

ggplot(data=t_sliced %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10), 
       aes(x=years, y=gross_dol, fill=Country))+
  geom_bar(position='fill', stat="identity", width=0.8, color="black")+
  ggtitle("Top 10 Production Countries for each Decades(1000$, Percentage)")+
  theme(legend.position = 'bottom')

#Korea specific
k_prod_ton <- data.frame(matrix(colSums((prod_ton %>% filter(Country=='Korea, Republic of'))[3:71])))
colnames(k_prod_ton) <- c("Tons")
k_prod_ton$years <- 1950:2018
ggplot(data=k_prod_ton, aes(x=years, y=Tons))+
  geom_point()+
  geom_line()+
  geom_smooth(method=loess, se=FALSE, aes(color='LOESS'))+
  geom_smooth(method=lm, se=FALSE, aes(color='Linear'))+
  ggtitle("Production of Korea from 1950 to 2018 (Ton)")

k_prod_dol <- data.frame(matrix(colSums((prod_dol %>% filter(Country=='Korea, Republic of'))[3:37])))
colnames(k_prod_dol) <- c("Dollars")
k_prod_dol$years <- 1984:2018
ggplot(data=k_prod_dol, aes(x=years, y=Dollars))+
  geom_point()+
  geom_line()+
  geom_smooth(method=loess, se=FALSE, aes(color='LOESS'))+
  geom_smooth(method=lm, se=FALSE, aes(color='Linear'))+
  ggtitle("Production of Korea from 1984 to 2018 (1000$)")

merge(k_prod_dol, k_prod_ton, by='years') %>% gather(key, value, Tons, Dollars) %>%
ggplot(aes(x=years, y=value, color=key))+
  geom_point()+
  geom_line()+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Production of Korea from 1984 to 2018 (Merged)")

####A2) Analysis per species####
sp_prod_ton <- prod_ton %>% group_by(Species) %>% summarise(gross_ton = sum(total))
sp_prod_dol <- prod_dol %>% group_by(Species) %>% summarise(gross_dol = sum(total))
sp_prod <- merge(sp_prod_ton, sp_prod_dol, by='Species') %>% mutate(ratio = gross_dol/gross_ton)

##top 10 analysis
ggplot(data=sp_prod, aes(x=Species, y=gross_ton, fill=Species))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(sp_prod, -gross_ton)$Species[10:1])+
  ggtitle("Barplot of top production Species(Ton)")+
  coord_flip()

ggplot(data=sp_prod, aes(x=Species, y=gross_dol, fill=Species))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(sp_prod, -gross_dol)$Species[10:1])+
  ggtitle("Barplot of top production Species(Dollar)")+
  coord_flip()

ggplot(data=sp_prod, aes(x=Species, y=log(ratio), fill=Species))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(sp_prod, -ratio)$Species[10:1])+
  ggtitle("Barplot of top production Species(Log(Dollar/Ton))")+
  coord_flip()

#histogram
ggplot(data=sp_prod, aes(x=log(gross_ton), y=..density..))+
  geom_histogram(bins=40, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for production(Ton)")

ggplot(data=sp_prod, aes(x=log(gross_dol), y=..density..))+
  geom_histogram(bins=40, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for production(Dollar)")

#scatter plot
ggplot(data=sp_prod, aes(x=log(gross_ton), y=log(gross_dol), color=ifelse(species %in% crabs, 'crab', NA)))+
  geom_point()+
  stat_smooth(aes(group=-1), method=lm)+
  ggtitle("Log(gross_ton) vs Log(gross_dol)")

#Proportion of crabs in total Production
sum(filter(sp_prod, Species %in% crabs)$gross_ton) / sum(sp_prod$gross_ton)
sum(filter(sp_prod, Species %in% crabs)$gross_dol) / sum(sp_prod$gross_dol)

#plot for crabs
p1 <- ggplot(data=sp_prod, aes(x=Species, y=log(gross_ton), fill=Species))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Barplot of top production Crabs(Ton)")+
  coord_flip()

p2 <- ggplot(data=sp_prod, aes(x=Species, y=log(gross_dol), fill=Species))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Barplot of top production Crabs(Dollar)")+
  coord_flip()

p3 <- ggplot(data=sp_prod, aes(x=Species, y=log(gross_ton), fill=Species))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Barplot of top production Crabs(Dollar/Ton)")+
  coord_flip()

####'Time slice' analysis####
#in tons
t_sliced <- data.frame()
m<-1
for (i in seq(1950,2018,10)){
  if (i==2010){
    t <- prod_ton %>% select(Country, Species, y2010:y2018) %>%
      mutate(total = rowSums(select(., 3:11))) %>%
      group_by(Species) %>% 
      summarise(gross_ton = sum(total))
  }
  else{
    t <- prod_ton %>% select(Country, Species, paste0('y',i):paste0('y',i+9)) %>%
      mutate(total = rowSums(select(., 3:12))) %>%
      group_by(Species) %>% 
      summarise(gross_ton = sum(total))
  }
  t <- as.data.frame(t)
  t$years <- paste0(i)
  t_sliced <- rbind(t_sliced, t)
  m <- m+1
}

t_sliced %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_ton))
t_sliced %>% arrange(-gross_ton) %>% group_by(years) %>% summarise(total=sum(gross_ton))
#check ratio
(t_sliced %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_ton)))$total / 
  (t_sliced %>% arrange(-gross_ton) %>% group_by(years) %>% summarise(total=sum(gross_ton)))$total * 100

ggplot(data=t_sliced %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10), 
       aes(x=years, y=gross_ton, fill=Species))+
  geom_bar(position='fill', stat="identity", width=0.8, color="black")+
  ggtitle("Top 10 Production Species for each Decades(Ton, Percentage)")+
  theme(legend.position = 'bottom')

t_sliced <- data.frame()
m<-1
for (i in seq(1950,2018,10)){
  if (i==2010){
    t <- prod_ton %>% filter(Species %in% crabs) %>%
      select(Country, Species, y2010:y2018) %>%
      mutate(total = rowSums(select(., 3:11))) %>%
      group_by(Species) %>% 
      summarise(gross_ton = sum(total))
  }
  else{
    t <- prod_ton %>% filter(Species %in% crabs) %>%
      select(Country, Species, paste0('y',i):paste0('y',i+9)) %>%
      mutate(total = rowSums(select(., 3:12))) %>%
      group_by(Species) %>% 
      summarise(gross_ton = sum(total))
  }
  t <- as.data.frame(t)
  t$years <- paste0(i)
  t_sliced <- rbind(t_sliced, t)
  m <- m+1
}
t_sliced %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10)
ggplot(data=t_sliced %>% arrange(-gross_ton) %>% group_by(years), 
       aes(x=years, y=log(gross_ton), fill=Species))+
  geom_bar(stat="identity", width=0.9, color="white")+
  ggtitle("Crab Production for each Decades(Ton)")+
  coord_flip()

ggplot(data=t_sliced %>% arrange(-gross_ton) %>% group_by(years), 
       aes(x=years, y=gross_ton, fill=Species))+
  geom_bar(position='fill', stat="identity", width=0.8, color="black")+
  ggtitle("Crab Production for each Decades(Ton, Percentage)")+
  theme(legend.position = 'bottom')

#in dollars
t_sliced <- data.frame()
m<-1
for (i in seq(1980,2018,10)){
  if (i==2010){
    t <- prod_dol %>% select(Country, Species, y2010:y2018) %>%
      mutate(total = rowSums(select(., 3:11))) %>%
      group_by(Species) %>% 
      summarise(gross_dol = sum(total))
  }
  else if (i==1980){
    t <- prod_dol %>% select(Country, Species, y1984:y1989) %>%
      mutate(total = rowSums(select(., 3:8))) %>%
      group_by(Species) %>% 
      summarise(gross_dol = sum(total))
  }
  else{
    t <- prod_dol %>% select(Country, Species, paste0('y',i):paste0('y',i+9)) %>%
      mutate(total = rowSums(select(., 3:12))) %>%
      group_by(Species) %>% 
      summarise(gross_dol = sum(total))
  }
  t <- as.data.frame(t)
  t$years <- paste0(i)
  t_sliced <- rbind(t_sliced, t)
  m <- m+1
}

t_sliced %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_dol))
t_sliced %>% arrange(-gross_dol) %>% group_by(years) %>% summarise(total=sum(gross_dol))
#check ratio
(t_sliced %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10) %>% summarise(total=sum(gross_dol)))$total / 
  (t_sliced %>% arrange(-gross_dol) %>% group_by(years) %>% summarise(total=sum(gross_dol)))$total * 100

ggplot(data=t_sliced %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10), 
       aes(x=years, y=gross_dol, fill=Species))+
  geom_bar(position='fill', stat="identity", width=0.8, color="black")+
  ggtitle("Top 10 Production Species for each Decades(1000$, Percentage)")+
  theme(legend.position = 'bottom')

t_sliced <- data.frame()
m<-1
for (i in seq(1980,2018,10)){
  if (i==2010){
    t <- prod_dol %>% filter(Species %in% crabs) %>%
      select(Country, Species, y2010:y2018) %>%
      mutate(total = rowSums(select(., 3:11))) %>%
      group_by(Species) %>% 
      summarise(gross_dol = sum(total))
  }
  else if (i==1980){
    t <- prod_dol %>% filter(Species %in% crabs) %>%
      select(Country, Species, y1984:y1989) %>%
      mutate(total = rowSums(select(., 3:8))) %>%
      group_by(Species) %>% 
      summarise(gross_dol = sum(total))
  }
  else{
    t <- prod_dol %>% filter(Species %in% crabs) %>%
      select(Country, Species, paste0('y',i):paste0('y',i+9)) %>%
      mutate(total = rowSums(select(., 3:12))) %>%
      group_by(Species) %>% 
      summarise(gross_dol = sum(total))
  }
  t <- as.data.frame(t)
  t$years <- paste0(i)
  t_sliced <- rbind(t_sliced, t)
  m <- m+1
}
t_sliced %>% arrange(-gross_dol) %>% group_by(years) %>% slice(1:10)
ggplot(data=t_sliced %>% arrange(-gross_dol) %>% group_by(years), 
       aes(x=years, y=log(gross_dol), fill=Species))+
  geom_bar(stat="identity", width=0.9, color="white")+
  ggtitle("Crab Production for each Decades(1000$)")+
  coord_flip()

ggplot(data=t_sliced %>% arrange(-gross_dol) %>% group_by(years), 
       aes(x=years, y=gross_dol, fill=Species))+
  geom_bar(stat="identity", width=0.8, color="black")+
  ggtitle("Crab Production for each Decades(1000$)")+
  theme(legend.position = 'bottom')

####Korea specific####
prod_ton%>%filter(Country=='Korea, Republic of' & Species %in% crabs)
#Korea only produces Chinese mitten crab!
k_crab_ton <- data.frame(matrix(colSums((prod_ton %>% filter(Country=='Korea, Republic of' & Species %in% crabs))[3:71])))
colnames(k_crab_ton) <- c("Tons")
k_crab_ton$years <- 1950:2018
ggplot(data=k_crab_ton %>% filter(Tons>0), aes(x=years, y=Tons))+
  geom_point()+
  geom_line()+
  geom_smooth(method=loess, se=FALSE, aes(col='LOESS'))+
  ggtitle("Production Chinese mitten crab in Korea from 1950 to 2018 (Ton)")

prod_dol%>%filter(Country=='Korea, Republic of' & Species %in% crabs) #just to double check
k_crab_dol <- data.frame(matrix(colSums((prod_dol %>% filter(Country=='Korea, Republic of' & Species %in% crabs))[3:37])))
colnames(k_crab_dol) <- c("Dollars")
k_crab_dol$years <- 1984:2018
ggplot(data=k_crab_dol %>% filter(Dollars>0), aes(x=years, y=Dollars))+
  geom_point()+
  geom_line()+
  geom_smooth(method=loess, se=FALSE, aes(col='LOESS'))+
  ggtitle("Production Chinese mitten crab in Korea from 1984 to 2018 (1000$)")

summary(lm((k_crab_dol %>% filter(Dollars > 0))$Dollars ~ (k_crab_ton %>% filter(Tons > 0))$Tons))
merge(k_crab_dol, k_crab_ton, by='years') %>% gather(key, value, Tons, Dollars) %>% filter(value>0) %>%
  ggplot(aes(x=years, y=value, color=key))+
  geom_point()+
  geom_line()+
  ggtitle("Production Chinese mitten crab in Korea from 1984 to 2018 (Merged)")

####A3)Per species/country analsyis####
sp_co_ton <- prod_ton %>% group_by(Species, Country) %>% summarise(gross_ton = sum(total))
sp_co_dol <- prod_dol %>% group_by(Species, Country) %>% summarise(gross_dol = sum(total))
sp_co <- merge(sp_co_ton, sp_co_dol, by=c('Species', 'Country'))%>% mutate(ratio = gross_dol/gross_ton)

#take a look at top 3 for top 10 species
ggplot(data=sp_co %>% arrange(-gross_ton) %>% group_by(Species) %>% slice(1:3), 
       aes(x=Species, y=log(gross_ton), fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(sp_prod, -gross_ton)$Species[10:1])+
  ggtitle("Top 3 Country of top 10 production Species(Ton)")+
  coord_flip()

ggplot(data=sp_co %>% arrange(-gross_dol) %>% group_by(Species) %>% slice(1:3), 
       aes(x=Species, y=log(gross_dol), fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(sp_prod, -gross_dol)$Species[10:1])+
  ggtitle("Top 3 Country of top 10 production Species(Dollar)")+
  coord_flip()

ggplot(data=sp_co %>% arrange(-ratio) %>% group_by(Species) %>% slice(1:3), 
       aes(x=Species, y=log(ratio), fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(sp_prod, -ratio)$Species[10:1])+
  ggtitle("Top 3 Country of top 10 production Species(Dollar/Ton)")+
  coord_flip()

#take a look at crabs only
crab_sp_cp <- filter(sp_co, Species %in% crabs)

ggplot(data=crab_sp_cp, aes(x=Species, y=gross_ton, fill=Country))+
  geom_bar(position='fill',stat="identity", width=0.95, col='black')+
  scale_x_discrete(limits=crabs)+
  ggtitle("Crab production(Ton)")+
  theme(legend.position = 'bottom')+
  coord_flip()

ggplot(data=crab_sp_cp, aes(x=Species, y=gross_dol, fill=Country))+
  geom_bar(position='fill', stat="identity", width=0.95, col='black')+
  scale_x_discrete(limits=crabs)+
  ggtitle("Crab production(Dollar)")+
  theme(legend.position = 'bottom')+
  coord_flip()

ggplot(data=crab_sp_cp, aes(x=Species, y=ratio, group=Species, col=Country))+
  geom_boxplot()+
  geom_point(size=1)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Crab production(Dollar/Ton)")+
  theme(legend.position = 'bottom')+
  coord_flip()

#cut down to top 3 only
p4 <- ggplot(data=crab_sp_cp %>% arrange(-gross_ton) %>% group_by(Species) %>% slice(1:3), 
       aes(x=Species, y=log(gross_ton), fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Crab production - Top 3 countries(Ton)")+
  coord_flip()

p5 <- ggplot(data=crab_sp_cp %>% arrange(-gross_dol) %>% group_by(Species) %>% slice(1:3), 
       aes(x=Species, y=log(gross_dol), fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Crab production - Top 3 countries(Dollar)")+
  coord_flip()

p6 <- ggplot(data=crab_sp_cp %>% arrange(-ratio) %>% group_by(Species) %>% slice(1:3), 
       aes(x=Species, y=log(ratio), fill=Country))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=crabs)+
  ggtitle("Crab production - Top 3 countries(Dollar/Ton)")+
  coord_flip()

####A4)Per country/species analysis####
co_sp_ton <- prod_ton %>% group_by(Country, Species) %>% summarise(gross_ton = sum(total))
co_sp_dol <- prod_dol %>% group_by(Country, Species) %>% summarise(gross_dol = sum(total))
co_sp <- merge(co_sp_ton, co_sp_dol, by=c('Country', 'Species'))%>% mutate(ratio = gross_dol/gross_ton)

#take a look at top 3 for top 10 countries
ggplot(data=co_sp %>% arrange(-gross_ton) %>% group_by(Country) %>% slice(1:3), 
       aes(x=Country, y=log(gross_ton), fill=Species))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(country_prod, -gross_ton)$Country[10:1])+
  ggtitle("Top 3 species of top 10 production countries(Ton)")+
  coord_flip()

ggplot(data=co_sp %>% arrange(-gross_dol) %>% group_by(Country) %>% slice(1:3), 
       aes(x=Country, y=log(gross_dol), fill=Species))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(country_prod, -gross_dol)$Country[10:1])+
  ggtitle("Top 3 species of top 10 production countries(Dollar)")+
  coord_flip()

ggplot(data=co_sp %>% arrange(-ratio) %>% group_by(Country) %>% slice(1:3), 
       aes(x=Country, y=log(ratio), fill=Species))+
  geom_bar(stat="identity", width=1)+
  scale_x_discrete(limits=arrange(country_prod, -ratio)$Country[10:1])+
  ggtitle("Top 3 species of top 10 production countries(Dollar/Ton)")+
  coord_flip()

#Korea only
c_t <- co_sp %>% filter(Country == 'Korea, Republic of') %>% arrange(-gross_ton)
c_t
sum((c_t %>% slice(1:10))$gross_ton) / 
  sum(c_t$gross_ton)
sum((c_t %>% filter(Species %in% crabs))$gross_ton) / 
  sum(c_t$gross_ton)
ggplot(data=co_sp %>% filter(Country == 'Korea, Republic of') %>% arrange(-gross_ton) %>% slice(1:10), 
       aes(x=Country, y=gross_ton, fill=Species))+
  geom_bar(position='fill', stat="identity", width=0.9, col='black')+
  ggtitle("Top species of Korea(Ton)")+
  coord_polar("y", start=0)+
  theme(legend.position = 'bottom')

c_t <- co_sp %>% filter(Country == 'Korea, Republic of') %>% arrange(-gross_dol)
c_t
sum((c_t %>% slice(1:10))$gross_dol) / 
  sum(c_t$gross_dol)
sum((c_t %>% filter(Species %in% crabs))$gross_dol) / 
  sum(c_t$gross_dol)
ggplot(data=co_sp %>% filter(Country == 'Korea, Republic of') %>% arrange(-gross_dol) %>% slice(1:10), 
       aes(x=Country, y=gross_dol, fill=Species))+
  geom_bar(position='fill', stat="identity", width=0.9, col='black')+
  ggtitle("Top species of Korea(1000$)")+
  coord_polar("y", start=0)+
  theme(legend.position = 'bottom')

#crab plotting
grid.arrange(p1,p2,p3,p4,p5,p6, nrow=2)

####A5) 'Crab' Analayis####
ggplot(data=sp_prod, aes(x=log(ratio), y=..density..))+
  geom_histogram(bins=40, fill="blue", color="white",alpha=0.5)+
  geom_density(fill="red", colour=NA, alpha=0.3)+
  geom_line(stat='density')+
  expand_limits(y=0)+
  ggtitle("Histogram for production(Dollar/Ton)")

ggplot(data=sp_prod, aes(x="", y=log(ratio), color=ifelse(Species %in% crabs, 'crab', NA)))+
  geom_violin(alpha=0.3, aes(group=1))+
  geom_boxplot(width=.1, fill="white")+
  ggtitle("Violin plot for (Dollar/Ton)")

ggplot(data=sp_prod, aes(x=gross_ton, y=gross_dol))+
  geom_point()+
  stat_smooth(method=lm)+
  ggtitle("gross_ton vs gross_dol")

ggplot(data=sp_prod, aes(x=log(gross_ton), y=log(gross_dol)))+
  geom_point()+
  stat_smooth(method=lm)+
  ggtitle("log(gross_ton) vs log(gross_dol)")

summary(lm(data=sp_prod, log(gross_dol) ~ log(gross_ton)))
summary(lm(data=sp_prod, gross_dol ~ gross_ton))

ggplot(data=sp_prod, aes(x=Species, y=log(ratio), color=ifelse(Species %in% crabs, 'crab', NA)))+
  scale_x_discrete(limits=(sp_prod %>% arrange(-ratio))$Species)+
  geom_point()+
  ggtitle("Scatter Plot of (Dollar/Ton)")

####per crab analysis####
c_ton <- data.frame()
for (c in crabs){
  tc <- data.frame(matrix(colSums((crab_ton %>% filter(Species==c))[3:71])))
  colnames(tc) <- c("Tons")
  tc$years <- 1950:2018
  tc$Species <- c
  c_ton <- rbind(c_ton, tc)
}
ggplot(data=c_ton %>% filter(Tons > 0), aes(x=years, y=log(Tons), col=Species))+
  #geom_point()+
  geom_line()+
  ggtitle("Production of crabs from 1950 to 2018 (Ton)")

c_dol <- data.frame()
for (c in crabs){
  tc <- data.frame(matrix(colSums((crab_dol %>% filter(Species==c))[3:37])))
  colnames(tc) <- c("Dollars")
  tc$years <- 1984:2018
  tc$Species <- c
  c_dol <- rbind(c_dol, tc)
}
ggplot(data=c_dol %>% filter(Dollars > 0), aes(x=years, y=log(Dollars), col=Species))+
  #geom_point()+
  geom_line()+
  ggtitle("Production of crabs from 1950 to 2018 (1000$)")

####Statements####
#Q1 - Is crab/squid related?
#Squids as cuttlefish, octopus
squids <- spe[unique(c(grep("Octopus", spe, fixed=TRUE), grep("cuttle", spe, fixed=TRUE)))]

cs_ton <- prod_ton %>% filter(Species %in% squids | Species %in% crabs)
cs_dol <- prod_dol %>% filter(Species %in% squids | Species %in% crabs)

# Not found in global data...

#Q2 - Does Chinese Production affect Korea?
t_sliced <- data.frame()
m<-1
for (i in seq(1950,2018,10)){
  if (i==2010){
    t <- prod_ton %>% select(Country, Species, y2010:y2018) %>%
      mutate(total = rowSums(select(., 3:11))) %>%
      group_by(Country) %>% 
      summarise(gross_ton = sum(total))
  }
  else{
    t <- prod_ton %>% select(Country, Species, paste0('y',i):paste0('y',i+9)) %>%
      mutate(total = rowSums(select(., 3:12))) %>%
      group_by(Country) %>% 
      summarise(gross_ton = sum(total))
  }
  t <- as.data.frame(t)
  t$years <- paste0(i)
  t_sliced <- rbind(t_sliced, t)
  m <- m+1
}

#total - not really meaningful
t_sliced %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10)
ck_ton <- merge(t_sliced %>% filter(Country == "China") %>% select(gross_ton, years),
                t_sliced %>% filter(Country == "Korea, Republic of") %>% select(gross_ton, years),
                by = 'years')
colnames(ck_ton) <- c("years","China", "Korea")
cor.test(ck_ton$China, ck_ton$Korea)

#only crabs?
t_sliced <- data.frame()
m<-1
for (i in seq(1950,2018,10)){
  if (i==2010){
    t <- prod_ton %>% filter(Species %in% crabs) %>% select(Country, Species, y2010:y2018) %>%
      mutate(total = rowSums(select(., 3:11))) %>%
      group_by(Country) %>% 
      summarise(gross_ton = sum(total))
  }
  else{
    t <- prod_ton %>% filter(Species %in% crabs) %>% select(Country, Species, paste0('y',i):paste0('y',i+9)) %>%
      mutate(total = rowSums(select(., 3:12))) %>%
      group_by(Country) %>% 
      summarise(gross_ton = sum(total))
  }
  t <- as.data.frame(t)
  t$years <- paste0(i)
  t_sliced <- rbind(t_sliced, t)
  m <- m+1
}

t_sliced %>% arrange(-gross_ton) %>% group_by(years) %>% slice(1:10)
ck_ton <- merge(t_sliced %>% filter(Country == "China") %>% select(gross_ton, years),
                t_sliced %>% filter(Country == "Korea, Republic of") %>% select(gross_ton, years),
                by = 'years')
colnames(ck_ton) <- c("years","China", "Korea")
cor.test(ck_ton$China, ck_ton$Korea)

#view entire species/no time cluster
ck_ton <- merge(sp_co_ton %>% filter(Country == "China") %>% select(Species, gross_ton),
       sp_co_ton %>% filter(Country == "Korea, Republic of") %>% select(Species, gross_ton),
       by='Species')
colnames(ck_ton) <- c("Species","China", "Korea")
cor.test(ck_ton$Korea, ck_ton$China)

#Country correlations - by time
countries <- levels(prod_ton$Country)
country_wise <- data.frame(matrix(rep(1, 70), ncol=1))
for (i in countries){
  country_wise <- cbind(country_wise, colSums( (prod_ton %>% filter(Country == i))[3:72] ))
}
colnames(country_wise) <- c("dead", countries)
country_wise$dead <- NULL
koreawise <- data.frame(cor(country_wise)) %>% select("Korea..Republic.of")
colnames(koreawise) <- c("corr")
koreawise$Country <- countries
koreawise %>% arrange(-corr)

#by species
sp_wise <- prod_ton %>% filter(Country == countries[1]) %>% select(Species, total)
for (i in seq(2, length(countries), 1)){
  sp_wise <- merge(sp_wise,
                   prod_ton %>% filter(Country == countries[i]) %>% select(Species, total),
                   by='Species', all.x=TRUE, all.y=TRUE)
  colnames(sp_wise) <- c('Species', countries[1:i])
}
sp_wise[is.na(sp_wise)] <- 0

cor_sp <- cor(sp_wise[2:212])
high_cor <- data.frame()
for (i in seq(2, 211, 1)){
  for (j in seq(1, i-1, 1)){
    if(abs(cor_sp[i, j]) > 0.7)
      high_cor <- rbind(high_cor, data.frame(A=c(countries[i]),B=c(countries[j]), val=c(cor_sp[i,j])))
  }
}


sp_korea <- data.frame(cor(sp_wise[2:212])) %>% select("Korea..Republic.of")
colnames(sp_korea) <- c("corr")
sp_korea$Country <- countries
sp_korea %>% arrange(-corr) %>% head(11)
sp_korea %>% arrange(-corr) %>% tail(11)

#narrow time a bit more(from 2010~)
sp_wise <- as.data.frame(prod_ton[, -c(3:62, 72)] %>% group_by(Country, Species) %>%
  summarise(total=sum(y2010, y2011, y2012, y2013, y2014, y2015, y2016, y2017, y2018)) %>%
           filter(Country == countries[1])) %>% select(Species, total)
for (i in seq(2, length(countries), 1)){
  sp_wise <- merge(sp_wise,
                   as.data.frame(prod_ton[, -c(3:62, 72)] %>% group_by(Country, Species) %>%
                     summarise(total=sum(y2010, y2011, y2012, y2013, y2014, y2015, y2016, y2017, y2018)) %>%
                     filter(Country == countries[i])) %>% select(Species, total),
                   by='Species', all.x=TRUE, all.y=TRUE)
  colnames(sp_wise) <- c('Species', countries[1:i])
}
sp_wise[is.na(sp_wise)] <- 0

for(i in countries){
  if(sum(sp_wise[, i]) == 0){
    sp_wise[, i] <- NULL
  }
}
#this leaves only 203 countries
cor_sp <- cor(sp_wise[2:204])

ggplot(data=melt(cor_sp), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='white')+
  scale_fill_gradient2(low='blue',high='red',mid='white',midpoint=0, limit=c(-1,1))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, vjust=1, size=0, hjust=1),
        axis.text.y=element_text(angle=45, vjust=1, size=0, hjust=1))+
  coord_fixed()+
  ggtitle("Correlation hitmap")

countries_2010 <- colnames(cor_sp)
high_cor <- data.frame()
for (i in seq(2, 203, 1)){
  for (j in seq(1, i-1, 1)){
    if(abs(cor_sp[i, j]) > 0.7)
      high_cor <- rbind(high_cor, data.frame(A=c(countries_2010[i]),B=c(countries_2010[j]), val=c(cor_sp[i,j])))
  }
}
high_cor
write.csv(high_cor, "High Correlations.csv")

as.data.frame(cor_sp) %>% select("Korea, Republic of")
sp_korea <- data.frame(cor(sp_wise[2:204])) %>% select("Korea..Republic.of")
colnames(sp_korea) <- c("corr")
sp_korea$Country <- countries_2010
sp_korea %>% arrange(-corr) %>% head(11)
sp_korea %>% arrange(-corr) %>% tail(11)

#species correlation
c_wise <- prod_ton %>% filter(Species == spe[1]) %>% select(Country, total)
for (i in seq(2, length(spe), 1)){
  c_wise <- merge(c_wise,
                   prod_ton %>% filter(Species == spe[i]) %>% select(Country, total),
                   by='Country', all.x=TRUE, all.y=TRUE)
  colnames(c_wise) <- c('Country', spe[1:i])
}
c_wise[is.na(c_wise)] <- 0

cor_c <- cor(c_wise[2:621])
sp_prime <- colnames(data.frame(cor_c))
crabs_prime <- sp_prime[unique(c(grep("Crab", sp_prime, fixed=TRUE), grep("crab", sp_prime, fixed=TRUE)))]
crab_cor <- data.frame(cor_c) %>% select(crabs_prime)
colnames(crab_cor) <- paste0('crab', 1:14)
crab_cor$Species <- sp_prime
crab_cor %>% arrange(-crab1) %>% head(11) %>% select(crab1, Species)
crab_cor %>% arrange(-crab2) %>% head(11) %>% select(crab2, Species)
crab_cor %>% arrange(-crab3) %>% head(11) %>% select(crab3, Species)
crab_cor %>% arrange(-corr) %>% tail(11)

####Clustering####
rm(list=ls())
#in tons
prod_ton <- as.data.frame(read.csv("Prod_sp_ton(50-18).csv", na.strings='...', stringsAsFactors = FALSE))
#drop unnecssary columns & rename columns
p_cols <- c(c("e", "Species", "a", "b","c","d"), paste0(rep('y', 68), 1950:2018)) #a~d are unnecessary!
colnames(prod_ton) <- p_cols
prod_ton <- select(prod_ton, -a, -b, -c, -d, -e)
#add 'total' col
prod_ton$total <- rowSums(prod_ton[, 3:70])
#drop rows where total==0
prod_ton <- filter(prod_ton, total!=0)
#drop 'total' row(last row)
prod_ton <- prod_ton[-nrow(prod_ton), ]

spe <- as.character(prod_ton$Species)
crabs <- crabs <- spe[unique(c(grep("Crab", spe, fixed=TRUE), grep("crab", spe, fixed=TRUE)))]
t_sliced <- data.frame(matrix(rep(0, length(spe)), ncol=1))
t_sliced$Species <- spe
m<-1
for (i in seq(1950,2018,10)){
  if (i==2010){
    t <- prod_ton %>% select(Species, y2010:y2018) %>%
      mutate(total = rowSums(select(., 3:10))) %>%
      group_by(Species) %>%
      summarise(gross_ton = sum(total))
  }
  else{
    t <- prod_ton %>% select(Species, paste0('y',i):paste0('y',i+9)) %>%
      mutate(total = rowSums(select(., 3:11))) %>%
      group_by(Species) %>%
      summarise(gross_ton = sum(total))
  }
  t <- as.data.frame(t)
  colnames(t) <-c("Species", paste0("y", i))
  t_sliced <- merge(t_sliced, t, by='Species')
  m <- m+1
}
t_sliced <- t_sliced[,-2]
t_sliced <- t_sliced %>% filter(y2000 != 0 & y2010 != 0)
t_sliced

vk <- 10
pmat <- scale(log(t_sliced[, 7:8]))
d <- dist(pmat, method='euclidean')
pfit <- hclust(d, method="ward.D")
plot(pfit, labels=t_sliced[,1])
rect.hclust(pfit, k=vk)
groups <- cutree(pfit, k=vk)
summary(as.factor(groups))
for(i in 1:vk){
  print(paste("cluster", i, 'with total', sum(t_sliced[groups==i, 1] %in% crabs), 'crabs'))
  print(t_sliced[groups==i, 1])
}


pclusters <- kmeans(pmat, vk, nstart=100, iter.max=100)
summary(pclusters)
pclusters$size
for(i in 1:vk){
  print(paste("cluster", i, 'with total', sum(t_sliced[groups==i, 1] %in% crabs), 'crabs'))
  print(t_sliced[pclusters$cluster==i, 1])
}

ggplot(data=t_sliced, aes(x=log(y2000), y=log(y2010), color=as.factor(groups)))+
  geom_point()+
  ggtitle("Scatter Plot of Clustered Species")

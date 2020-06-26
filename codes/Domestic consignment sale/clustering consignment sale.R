library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
requiredPackages=c("flexclust","NbClust","rattle","cluster","fMultivar","ggplot2")
for(p in requiredPackages){
  if(!require(p,character.only=TRUE)) install.packages(p)
}
selling_domestic<-rbind(selling_domestic1,selling_domestic2,selling_domestic3,selling_domestic4)
rm(selling_domestic1,selling_domestic2,selling_domestic3,selling_domestic4)
selling_domestic  <- selling_domestic %>%
  filter(!is.na(평균가)) %>%
  filter(!is.na(물량.KG.))

#clustering, need selling_domestic
sd_year<- cbind(year = as.numeric(format(selling_domestic$위판일자,"%Y")),selling_domestic[,-1])
sd_year <- sd_year %>%
  filter(물량.KG. > 0) %>%
  group_by(year, 수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
View(sd_year)

sd_2013 <- sd_year %>%
  filter(year == 2013) %>%
  group_by(수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
sd_2014 <- sd_year %>%
  filter(year == 2014) %>%
  group_by(수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
sd_2015 <- sd_year %>%
  filter(year == 2015) %>%
  group_by(수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
sd_2016 <- sd_year %>%
  filter(year == 2016) %>%
  group_by(수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
sd_2017 <- sd_year %>%
  filter(year == 2017) %>%
  group_by(수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
sd_2018 <- sd_year %>%
  filter(year == 2018) %>%
  group_by(수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
sd_2019 <- sd_year %>%
  filter(year == 2019) %>%
  group_by(수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
View(sd_2019)
sd_tot <- full_join(
  full_join(
    full_join(
      full_join(
        full_join(
          full_join(sd_2013,sd_2014, by = "수산물표준코드명"),
          sd_2015, by = "수산물표준코드명"),
        sd_2016, by = "수산물표준코드명"),
      sd_2017, by = "수산물표준코드명"),
    sd_2018, by = "수산물표준코드명"),
  sd_2019, by = "수산물표준코드명")
sd_tot <- filter_all(sd_tot, all_vars(!is.na(.)))
View(sd_tot)
summary(is.na(sd_tot))
#time normalization
for (i in 1:7) {
  print(2*i, 2*i+1)
  sd_tot[,16-2*i] <- sd_tot[,16-2*i] / sd_tot[,2]
  sd_tot[,17-2*i] <- sd_tot[,17-2*i] / sd_tot[,3]
}
View(sd_tot)

vars.to.use = colnames(sd_tot)[-1]
pmatrix = scale(sd_tot[,vars.to.use])
head(pmatrix)
pcenter = attr(pmatrix, "scaled:center"); pcenter
pscale = attr(pmatrix, "scaled:scale"); pscale
d = dist(pmatrix, method="euclidean") # Create the distance matrix.
pfit = hclust(d, method = "ward.D")
plot(pfit, labels=sd_tot$수산물표준코드명) 
rect.hclust(pfit, k = 6)
groups = cutree(pfit, k=6)
print_clusters = function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    View(sd_tot[labels==i,c("수산물표준코드명")])
  }
}
print_clusters(groups, 6)

km_sd<-kmeans(sd_tot[,-1],6)
sd_km<-cbind(sd_tot,km_sd$cluster)
print_clusters = function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    View(sd_km[labels==i,c("수산물표준코드명")])
  }
}
print_clusters(groups, 6)

crabs <- selling_domestic %>% 
  filter(수산물표준코드 >= 630200) %>%
  filter(수산물표준코드 < 630300) %>%
  group_by(위판일자, 수산물표준코드명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
crabs <- cbind(sapply(crabs[,1], as.numeric),crabs[,-1])
crabs


vars.to.use = colnames(crabs)[-2]
pmatrix = scale(crabs[,vars.to.use])
head(pmatrix)
pcenter = attr(pmatrix, "scaled:center"); pcenter
pscale = attr(pmatrix, "scaled:scale"); pscale
d = dist(pmatrix, method="euclidean") # Create the distance matrix.
pfit = hclust(d, method = "ward.D")
plot(pfit, labels=crabs$수산물표준코드명) 
rect.hclust(pfit, k = 6)
groups = cutree(pfit, k=6)
print_clusters = function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    View(sd_tot[labels==i,c("수산물표준코드명")])
  }
}
print_clusters(groups, 6)


#cluster 임시 분석용 코드
fish_dd<- selling_domestic %>%
  filter(수산물표준코드명 == "'새조개'") %>%
  group_by(위판일자, 어종상태명) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))

#ggplot(fish_dd, aes(x = 위판일자, y = 평균가)) + geom_line(aes(color = 어종상태명)) + geom_area(alpha = 0.1)
#ggplot(fish_dd, aes(x = 위판일자, y = 물량.KG.)) + geom_line(aes(color = 어종상태명))

fish_dd_year <- cbind(year = as.numeric(format(fish_dd$위판일자,"%Y")),fish_dd[,-1])
fish_dd_year <- fish_dd_year %>%
  filter(year != 2020) %>%
  group_by(year) %>%
  summarise(평균가 = mean(평균가), 물량.KG. = sum(물량.KG.))
ggplot(fish_dd_year, aes(x = year, y = 평균가)) + geom_line()
ggplot(fish_dd_year, aes(x = year, y = 물량.KG.)) + geom_line()

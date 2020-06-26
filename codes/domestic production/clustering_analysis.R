library(dplyr)
library(ggplot2)
library(data.table)

fishery_data <- read.csv("data.csv")
View(fishery_data)

####cluster method 1: 시간별 생산량####

##Clustering
fishery_cluster <- fishery_data %>% 
  filter(!is.na(amount_live), !is.na(amount_fresh), !is.na(amount_frozen)) %>%
  mutate(amount=amount_live+amount_fresh+amount_frozen) %>%
  setDT %>%
  dcast(species+speciescode+location~time, value.var=c("amount"))
View(fishery_cluster)

pmatrix <- fishery_cluster[, -c(1:3)]
pmatrix <- pmatrix %>% scale
View(pmatrix)

#hierarchial clustering
d=dist(pmatrix, method="euclidean") #distance matrix
pfit=hclust(d, method="ward.D") #hierarchial clutering
plot(pfit, labels=fishery_cluster$species)
rect.hclust(pfit, k=5)
groups <- cutree(pfit, k=5)
cluster_h.1 <- fishery_cluster[, 1:3] %>% mutate(group = groups)
View(cluster_h.1)

#k-means clustering
pclusters = kmeans(pmatrix, 5, nstart=100, iter.max=100)
groups = pclusters$cluster
cluster_k.1 <- fishery_cluster[, 1:3] %>% mutate(group = groups)
View(cluster_k.1)

##Cute Crabbo
cluster_h.1 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, fill=factor(group)) + geom_bar()
cluster_k.1 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, fill=factor(group)) + geom_bar()
cluster_h.1 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, y=location, fill=factor(group)) + geom_tile()
cluster_k.1 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, y=location, fill=factor(group)) + geom_tile()


##characteristics of clusters
#species type & classification1
p <- ggplot(data=cluster_h.1) 
p <- p + aes(x=factor(group), fill=substr(as.character(speciescode), 1, 3)) 
p <- p + scale_fill_discrete(name = "type", labels=c("해면어류", "해면갑각류", "해면연체동물류", "양식어류", "양식갑각류", "내수면어류", "내수면갑각류")) 
p + geom_bar()
#species type & classification1
p <- ggplot(data=cluster_k.1) 
p <- p + aes(x=factor(group), fill=substr(as.character(speciescode), 1, 3)) 
p <- p + scale_fill_discrete(name = "type", labels=c("해면어류", "해면갑각류", "해면연체동물류", "양식어류", "양식갑각류", "내수면어류", "내수면갑각류")) 
p + geom_bar()

#species type & classification1
p <- ggplot(data=cluster_h.1) 
p <- p + aes(fill=factor(group), y=substr(as.character(speciescode), 1, 3))
p <- p + scale_y_discrete(name="type", label=c("해면어류", "해면갑각류", "해면연체동물류", "양식어류", "양식갑각류", "내수면어류", "내수면갑각류"))
p + geom_bar()
#species type & classification1
p <- ggplot(data=cluster_k.1) 
p <- p + aes(fill=factor(group), y=substr(as.character(speciescode), 1, 3))
p <- p + scale_y_discrete(name="type", label=c("해면어류", "해면갑각류", "해면연체동물류", "양식어류", "양식갑각류", "내수면어류", "내수면갑각류"))
p + geom_bar()

#location1
cluster_h.1 %>%
  ggplot() + aes(x=factor(group), fill=location) + geom_bar() 
cluster_k.1 %>%
  ggplot() + aes(x=factor(group), fill=location) + geom_bar()

#location2
cluster_h.1 %>%
  ggplot() + aes(y=location, fill=factor(group)) + geom_bar() 
cluster_k.1 %>%
  ggplot() + aes(y=location, fill=factor(group)) + geom_bar()

####cluster method 2: 시간별 생산금액####

##Clustering
fishery_cluster <- fishery_data %>% 
  filter(!is.na(price_live), !is.na(price_fresh), !is.na(price_frozen)) %>%
  mutate(price=amount_live+amount_fresh+amount_frozen) %>%
  setDT %>%
  dcast(species+speciescode+location~time, value.var=c("price"))
View(fishery_cluster)

pmatrix <- fishery_cluster[, -c(1:3)]
pmatrix <- pmatrix %>% scale
View(pmatrix)

#hierarchial clustering
d=dist(pmatrix, method="euclidean") #distance matrix
pfit=hclust(d, method="ward.D") #hierarchial clutering
plot(pfit, labels=fishery_cluster$species)
rect.hclust(pfit, k=5)
groups <- cutree(pfit, k=5)
cluster_h.2 <- fishery_cluster[, 1:3] %>% mutate(group = groups)
View(cluster_h.2)

#k-means clustering
pclusters = kmeans(pmatrix, 5, nstart=100, iter.max=100)
groups = pclusters$cluster
cluster_k.2 <- fishery_cluster[, 1:3] %>% mutate(group = groups)
View(cluster_k.2)

##Cute Crabbo
#species
cluster_h.2 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, fill=factor(group)) + geom_bar()
cluster_k.2 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, fill=factor(group)) + geom_bar()
#species&location
cluster_h.2 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, y=location, fill=factor(group)) + geom_tile()
cluster_k.2 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, y=location, fill=factor(group)) + geom_tile()

##characteristics of clusters
#species type & classification1
p <- ggplot(data=cluster_h.2) 
p <- p + aes(x=factor(group), fill=substr(as.character(speciescode), 1, 3)) 
p <- p + scale_fill_discrete(name = "type", labels=c("해면어류", "해면갑각류", "해면연체동물류", "양식어류", "양식갑각류", "내수면어류", "내수면갑각류")) 
p + geom_bar()
#species type & classification1
p <- ggplot(data=cluster_k.2) 
p <- p + aes(x=factor(group), fill=substr(as.character(speciescode), 1, 3)) 
p <- p + scale_fill_discrete(name = "type", labels=c("해면어류", "해면갑각류", "해면연체동물류", "양식어류", "양식갑각류", "내수면어류", "내수면갑각류")) 
p + geom_bar()

#species type & classification1
p <- ggplot(data=cluster_h.2) 
p <- p + aes(fill=factor(group), y=substr(as.character(speciescode), 1, 3))
p <- p + scale_y_discrete(name="type", label=c("해면어류", "해면갑각류", "해면연체동물류", "양식어류", "양식갑각류", "내수면어류", "내수면갑각류"))
p + geom_bar()
#species type & classification1
p <- ggplot(data=cluster_k.2) 
p <- p + aes(fill=factor(group), y=substr(as.character(speciescode), 1, 3))
p <- p + scale_y_discrete(name="type", label=c("해면어류", "해면갑각류", "해면연체동물류", "양식어류", "양식갑각류", "내수면어류", "내수면갑각류"))
p + geom_bar()

#location1
cluster_h.2 %>%
  ggplot() + aes(x=factor(group), fill=location) + geom_bar() 
cluster_k.2 %>%
  ggplot() + aes(x=factor(group), fill=location) + geom_bar()

#location2
cluster_h.2 %>%
  ggplot() + aes(y=location, fill=factor(group)) + geom_bar() 
cluster_k.2 %>%
  ggplot() + aes(y=location, fill=factor(group)) + geom_bar()

####cluster method 3: 시간별 생산금액/생산량 비율####
fishery_cluster <- fishery_data %>%
  mutate(ratio = (price_live+price_fresh+price_frozen)/(amount_live+amount_fresh+amount_frozen)) %>%
  filter(is.finite(ratio)) %>%
  setDT %>%
  dcast(species+speciescode+location~time, value.var=c("ratio"))
View(fishery_cluster)

pmatrix <- fishery_cluster[, -c(1:3)]
pmatrix <- pmatrix %>% scale
View(pmatrix)

#hierarchial clustering
d=dist(pmatrix, method="euclidean") #distance matrix
pfit=hclust(d, method="ward.D") #hierarchial clutering
plot(pfit, labels=fishery_cluster$species)
rect.hclust(pfit, k=5)
groups <- cutree(pfit, k=5)
cluster_h.3 <- fishery_cluster[, 1:3] %>% mutate(group = groups)
View(cluster_h.3)

#k-means clustering
pclusters = kmeans(pmatrix, 5, nstart=100, iter.max=100)
groups = pclusters$cluster
cluster_k.3 <- fishery_cluster[, 1:3] %>% mutate(group = groups)
View(cluster_k.3)

##Cute Crabbo
cluster_h.3 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, fill=factor(group)) + geom_bar()
cluster_k.3 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, fill=factor(group)) + geom_bar()
cluster_h.3 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, y=location, fill=factor(group)) + geom_tile()
cluster_k.3 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, y=location, fill=factor(group)) + geom_tile()

##characteristics of clusters
#species type & classification1
p <- ggplot(data=cluster_h.3) 
p <- p + aes(x=factor(group), fill=substr(as.character(speciescode), 1, 3)) 
p <- p + scale_fill_discrete(name = "type", labels=c("해면어류", "해면갑각류", "해면연체동물류", "양식어류", "양식갑각류", "내수면어류", "내수면갑각류")) 
p + geom_bar()
#species type & classification1
p <- ggplot(data=cluster_k.3) 
p <- p + aes(x=factor(group), fill=substr(as.character(speciescode), 1, 3)) 
p <- p + scale_fill_discrete(name = "type", labels=c("해면어류", "해면갑각류", "해면연체동물류", "양식어류", "양식갑각류", "내수면어류", "내수면갑각류")) 
p + geom_bar()

#species type & classification2
p <- ggplot(data=cluster_h.3) 
p <- p + aes(fill=factor(group), y=substr(as.character(speciescode), 1, 3))
p <- p + scale_y_discrete(name="type", label=c("해면어류", "해면갑각류", "해면연체동물류", "양식어류", "양식갑각류", "내수면어류", "내수면갑각류"))
p + geom_bar()
#species type & classification2
p <- ggplot(data=cluster_k.3) 
p <- p + aes(fill=factor(group), y=substr(as.character(speciescode), 1, 3))
p <- p + scale_y_discrete(name="type", label=c("해면어류", "해면갑각류", "해면연체동물류", "양식어류", "양식갑각류", "내수면어류", "내수면갑각류"))
p + geom_bar()

#location1
cluster_h.3 %>%
  ggplot() + aes(x=factor(group), fill=location) + geom_bar() 
cluster_k.3 %>%
  ggplot() + aes(x=factor(group), fill=location) + geom_bar()

#location2
cluster_h.3 %>%
  ggplot() + aes(y=location, fill=factor(group)) + geom_bar() 
cluster_k.3 %>%
  ggplot() + aes(y=location, fill=factor(group)) + geom_bar()

####cluster method 4: 활어/선어/냉동별 생산금액/생산량 비율####

##Clustering
fishery_cluster <- fishery_data %>% 
  mutate(ratio_live = price_live/amount_live,
         ratio_fresh = price_fresh/amount_fresh,
         ratio_frozen = price_frozen/amount_frozen,
         ratio_total = (price_live+price_fresh+price_frozen)/(amount_live+amount_fresh+amount_frozen)) %>%
  filter(is.finite(ratio_live), is.finite(ratio_fresh), is.finite(ratio_frozen), is.finite(ratio_total))
View(fishery_cluster)

pmatrix <- fishery_cluster[, c(13, 14, 15, 16)]
pmatrix <- scale(pmatrix)
View(pmatrix)

#hierarchial clustering
d=dist(pmatrix, method="euclidean") #distance matrix
pfit=hclust(d, method="ward.D") #hierarchial clutering
groups <- cutree(pfit, k=3)
cluster_h.4 <- fishery_cluster[, -c(13, 14, 15, 16)] %>% mutate(group = groups)
View(cluster_h.4)

#k-means clustering
pclusters = kmeans(pmatrix, 3, nstart=100, iter.max=100)
groups = pclusters$cluster
cluster_k.4 <- fishery_cluster[, -c(13, 14, 15, 16)] %>% mutate(group = groups)
View(cluster_k.4)

##Cute Crabbo
cluster_h.4 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, fill=factor(group)) + geom_bar()
cluster_k.4 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, fill=factor(group)) + geom_bar()
cluster_h.4 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, y=location, fill=factor(group)) + geom_tile()
cluster_k.4 %>% 
  filter(grepl("게", species), species != "성게류") %>%
  ggplot() + aes(x=species, y=location, fill=factor(group)) + geom_tile()

##characteristics of clusters
cluster_k.4 %>% #species type & classification
  ggplot() + aes(x=group, fill=substr(as.character(speciescode), 1, 3)) + scale_fill_discrete(name = "type", labels=c("해면어류", "해면갑각류", "해면연체동물류", "양식갑각류", "내수면어류", "내수면갑각류")) + geom_bar() 
cluster_k.4 %>% #type
  ggplot() + aes(x=group, fill=type) + geom_bar()
cluster_k.4 %>% #year
  ggplot() + aes(x=group, fill=factor(time%/%100)) + geom_bar()
cluster_k.4 %>% #month
  ggplot() + aes(x=group, fill=factor(time%%100)) + geom_bar()
cluster_k.4 %>% #market
  ggplot() + aes(x=group, fill=market) + geom_bar()
cluster_k.4 %>% #location
  ggplot() + aes(x=group, fill=location) + geom_bar()

##Plotting
cluster_h.4 %>% 
  ggplot() + aes(x=amount_live, y=price_live, color=factor(group)) + geom_point()
cluster_h.4 %>% 
  ggplot() + aes(x=amount_fresh, y=price_fresh, color=factor(group)) + geom_point()
cluster_h.4 %>% 
  ggplot() + aes(x=amount_frozen, y=price_frozen, color=factor(group)) + geom_point()

cluster_k.4 %>% 
  ggplot() + aes(x=amount_live, y=price_live, color=factor(group)) + geom_point()
cluster_k.4 %>% 
  ggplot() + aes(x=amount_fresh, y=price_fresh, color=factor(group)) + geom_point()
cluster_k.4 %>% 
  ggplot() + aes(x=amount_frozen, y=price_frozen, color=factor(group)) + geom_point()


#Plot with whole observations
fishery_data %>%
  ggplot() + geom_point(aes(x=amount_live, y=price_live), color="darkgrey") + geom_point(data=cluster_k.4, aes(x=amount_live, y=price_live, color=factor(group)))
fishery_data %>%
  ggplot() + geom_point(aes(x=amount_fresh, y=price_fresh), color="darkgrey") + geom_point(data=cluster_k.4, aes(x=amount_fresh, y=price_fresh, color=factor(group)))
fishery_data %>%
  ggplot() + geom_point(aes(x=amount_frozen, y=price_frozen), color="darkgrey") + geom_point(data=cluster_k.4, aes(x=amount_frozen, y=price_frozen, color=factor(group))) 

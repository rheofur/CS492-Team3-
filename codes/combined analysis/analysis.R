library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(zoo)
library(forecast)

############################
 #fishery analysis by time#
############################

for (year in 2010:2020) {
  #transform into viable form
  fishery_time <- fishery %>%
    filter(time %/% 100 == year) %>%
    melt(1:6) %>%
    group_by(species, time) %>%
    summarize(value = sum(value[variable=="price_live" | variable=="price_fresh" | variable=="price_frozen"], na.rm=TRUE) / 
                sum(value[variable=="amount_live" | variable=="amount_fresh" | variable=="amount_frozen"], na.rm=TRUE)) %>%
    dcast(formula=species~time, value="value")
  fishery_time <- fishery_time[rowSums((!is.finite(as.matrix(fishery_time[, -1])))) == 0, ]
  
  #cluster analysis
  pmatrix <- fishery_time[, -1]
  pmatrix <- pmatrix %>% t %>% scale %>% t %>% scale
  finite <- rowSums((!is.finite(pmatrix))) == 0
  pmatrix <- pmatrix[finite, ]
  d=dist(pmatrix, method="euclidean")
  #hierarchial clutering
  pfit <- hclust(d, method="ward.D")
  groups <- cutree(pfit, k=5)
  clusters.h <- data.frame(fishery_time[finite, 1], group = groups)
  #k-means clustering
  pclusters <- kmeans(pmatrix, 5, nstart=100, iter.max=100)
  groups <- pclusters$cluster
  clusters.k <- data.frame(fishery_time[finite, 1], group = groups)
  
  #correlation analysis
  fishery_time_corr <- fishery_time[finite, -1]
  rownames(fishery_time_corr) <- fishery_time$species[finite]
  cormat <- round(cor(t(fishery_time_corr)), 2)
  pfit <- hclust(as.dist(1-cormat), method="ward.D")
  groups <- cutree(pfit, k=5)
  cormat <- cormat[pfit$order, pfit$order]
  cormat.melt <- melt(cormat)
  corrplot <- ggplot(data=cormat.melt) + aes(x=Var1, y=Var2, fill=value) + geom_tile() + scale_fill_gradient2(low="slateblue", mid="black", high="coral") + theme(text=element_text(size=10), axis.text.x=element_text(angle=90))
  clusters.corr <- data.frame(species=fishery_time$species[finite], group=groups)
  
  #data output
  write.csv(clusters.h, paste0("data/", year, "_hierarchial_clustering.ratio.csv"), row.names=FALSE)
  write.csv(clusters.k, paste0("data/", year, "_kmeans_clustering.ratio.csv"), row.names=FALSE)
  write.csv(clusters.corr, paste0("data/", year, "_correlation_clustering.ratio.csv"), row.names=FALSE)
  write.csv(cormat.melt, paste0("data/", year, "_correlation.ratio.csv"), row.names=FALSE)
  ggsave(paste0("data/", year, "_correlation_plot.ratio.png"), corrplot)
}

########################################
 #comparison of two fishery statistics#
########################################
fishery %>%
  filter(species=="민꽃게" | species == "참홍어") %>%
  group_by(species, time) %>%
  summarize(value = sum(price_live, price_fresh, price_frozen, na.rm=TRUE)/sum(amount_live, amount_fresh, amount_frozen, na.rm=TRUE)) %>%
  ggplot() + aes(x=as.yearmon(paste0(time%/%100, "-", time%%100)), y=value, color=species) + geom_line()

##########################
 #trade analysis by time#
##########################

for (year in 2020) {
  #transform into viable form
  trade_time <- trade %>%
    filter(time %/% 100 == year) %>%
    group_by(product, time) %>%
    summarize(value = sum(price, na.rm=TRUE)/sum(amount, na.rm=TRUE)) %>%
    dcast(formula=product~time, value="value")
  trade_time <- trade_time[rowSums((!is.finite(as.matrix(trade_time[, -1])))) == 0, ]
  
  #cluster analysis
  pmatrix <- trade_time[, -1]
  pmatrix <- pmatrix %>% t %>% scale %>% t %>% scale
  finite <- rowSums((!is.finite(pmatrix))) == 0
  pmatrix <- pmatrix[finite, ]
  d=dist(pmatrix, method="euclidean")
  #hierarchial clutering
  pfit <- hclust(d, method="ward.D")
  groups <- cutree(pfit, k=5)
  clusters.h <- data.frame(trade_time[finite, 1], group = groups)
  #k-means clustering
  pclusters <- kmeans(pmatrix, 2, nstart=100, iter.max=100)
  groups <- pclusters$cluster
  clusters.k <- data.frame(trade_time[finite, 1], group = groups)
  
  #correlation analysis
  trade_time_corr <- trade_time[finite, -1]
  rownames(trade_time_corr) <- trade_time$product[finite]
  cormat <- round(cor(t(trade_time_corr)), 2)
  pfit <- hclust(as.dist(1-cormat), method="ward.D")
  groups <- cutree(pfit, k=5)
  cormat <- cormat[pfit$order, pfit$order]
  cormat.melt <- melt(cormat)
  corrplot <- ggplot(data=cormat.melt) + aes(x=Var1, y=Var2, fill=value) + geom_tile() + scale_fill_gradient2(low="slateblue", mid="black", high="coral") + theme(text=element_text(size=10), axis.text.x=element_text(angle=90))
  clusters.corr <- data.frame(product=trade_time$product[finite], group=groups)
  
  #data output
  write.csv(clusters.h, paste0("data/trade/", year, "_hierarchial_clustering.ratio.csv"), row.names=FALSE)
  write.csv(clusters.k, paste0("data/trade/", year, "_kmeans_clustering.ratio.csv"), row.names=FALSE)
  write.csv(clusters.corr, paste0("data/trade/", year, "_correlation_clustering.ratio.csv"), row.names=FALSE)
  write.csv(cormat.melt, paste0("data/trade/", year, "_correlation.ratio.csv"), row.names=FALSE)
  ggsave(paste0("data/", year, "_correlation_plot.ratio.png"), corrplot)
}

######################################
 #comparison of two trade statistics#
######################################
trade %>%
  filter(product =="꽃게" | product == "대하") %>%
  group_by(species, time) %>%
  summarize(value = sum(price, na.rm=TRUE)) %>%
  ggplot() + aes(x=as.yearmon(paste0(time%/%100, "-", time%%100)), y=value, color=species) + geom_line()
##############################
 #wholesale analysis by time#
##############################

for (year in 2013:2019) {
  #transform into viable form
  wholesale_time <- wholesale %>%
    filter(year(time) == year) %>%
    group_by(product, time = year(time) * 100 + month(time)) %>%
    summarize(value = sum(amount, na.rm=TRUE)) %>%
    dcast(formula=product~time, value="value")
  wholesale_time <- wholesale_time[rowSums((!is.finite(as.matrix(wholesale_time[, -1])))) == 0, ]
  
  #cluster analysis
  pmatrix <- wholesale_time[, -1]
  pmatrix <- pmatrix %>% t %>% scale %>% t %>% scale
  finite <- rowSums((!is.finite(pmatrix))) == 0
  pmatrix <- pmatrix[finite, ]
  d=dist(pmatrix, method="euclidean")
  #hierarchial clutering
  pfit <- hclust(d, method="ward.D")
  groups <- cutree(pfit, k=5)
  clusters.h <- data.frame(wholesale_time[finite, 1], group = groups)
  #k-means clustering
  pclusters <- kmeans(pmatrix, 5, nstart=100, iter.max=100)
  groups <- pclusters$cluster
  clusters.k <- data.frame(wholesale_time[finite, 1], group = groups)
  
  #correlation analysis
  wholesale_time_corr <- wholesale_time[finite, -1]
  rownames(wholesale_time_corr) <- wholesale_time$product[finite]
  cormat <- round(cor(t(wholesale_time_corr)), 2)
  pfit <- hclust(as.dist(1-cormat), method="ward.D")
  groups <- cutree(pfit, k=5)
  cormat <- cormat[pfit$order, pfit$order]
  cormat.melt <- melt(cormat)
  corrplot <- ggplot(data=cormat.melt) + aes(x=Var1, y=Var2, fill=value) + geom_tile() + scale_fill_gradient2(low="slateblue", mid="black", high="coral") + theme(text=element_text(size=10), axis.text.x=element_text(angle=90))
  clusters.corr <- data.frame(product=wholesale_time$product[finite], group=groups)
  
  #data output
  write.csv(clusters.h, paste0("data/", year, "_hierarchial_clustering.amount.csv"), row.names=FALSE)
  write.csv(clusters.k, paste0("data/", year, "_kmeans_clustering.amount.csv"), row.names=FALSE)
  write.csv(clusters.corr, paste0("data/", year, "_correlation_clustering.amount.csv"), row.names=FALSE)
  write.csv(cormat.melt, paste0("data/", year, "_correlation.amount.csv"), row.names=FALSE)
  ggsave(paste0("data/", year, "_correlation_plot.amount.png"), corrplot)
}

#########################################
 #comparison of two wholesale statistics#
#########################################
wholesale %>%
  filter(product=="꽃게" | product == "대하", time %/% 100 == 2019) %>%
  group_by(product, time) %>%
  summarize(value = sum(price_live, price_fresh, price_frozen, na.rm=TRUE)) %>%
  ggplot() + aes(x=as.yearmon(paste0(time%/%100, "-", time%%100)), y=value, color=product) + geom_line()

############################
 #fishery analysis by year#
############################

#transform into viable form
fishery_time <- fishery %>%
  melt(1:6) %>%
  group_by(species, time=time%/%100) %>%
  summarize(value = mean(value[variable=="amount_live" | variable=="amount_fresh" | variable=="amount_frozen"], na.rm=TRUE)) %>%
  dcast(formula=species~time, value="value")
fishery_time <- fishery_time[rowSums((!is.finite(as.matrix(fishery_time[, -1])))) == 0, ]

#cluster analysis
pmatrix <- fishery_time[, -1]
pmatrix <- pmatrix %>% t %>% scale %>% t %>% scale
finite <- rowSums((!is.finite(pmatrix))) == 0
pmatrix <- pmatrix[finite, ]
d=dist(pmatrix, method="euclidean")
#hierarchial clutering
pfit <- hclust(d, method="ward.D")
groups <- cutree(pfit, k=5)
clusters.h <- data.frame(fishery_time[finite, 1], group = groups)
#k-means clustering
pclusters <- kmeans(pmatrix, 5, nstart=100, iter.max=100)
groups <- pclusters$cluster
clusters.k <- data.frame(fishery_time[finite, 1], group = groups)

#correlation analysis
fishery_time_corr <- fishery_time[finite, -1]
rownames(fishery_time_corr) <- fishery_time$species[finite]
cormat <- round(cor(t(fishery_time_corr)), 2)
pfit <- hclust(as.dist(1-cormat), method="ward.D")
groups <- cutree(pfit, k=5)
cormat <- cormat[pfit$order, pfit$order]
cormat.melt <- melt(cormat)
corrplot <- ggplot(data=cormat.melt) + aes(x=Var1, y=Var2, fill=value) + geom_tile() + scale_fill_gradient2(low="slateblue", mid="black", high="coral") + theme(text=element_text(size=10), axis.text.x=element_text(angle=90))
clusters.corr <- data.frame(species=fishery_time$species[finite], group=groups)

#data output
write.csv(clusters.h, paste0("data/hierarchial_clustering.amount.csv"), row.names=FALSE)
write.csv(clusters.k, paste0("data/kmeans_clustering.amount.csv"), row.names=FALSE)
write.csv(clusters.corr, paste0("data/correlation_clustering.amount.csv"), row.names=FALSE)
write.csv(cormat.melt, paste0("data/correlation.amount.csv"), row.names=FALSE)
ggsave(paste0("data/correlation_plot.amount.png"), corrplot)

##########################
 #trade analysis by year#
##########################

#transform into viable form
trade_time <- trade %>%
  filter(import.export == "수입") %>%
  group_by(product, time=date%%100) %>%
  summarize(value = sum(amount, na.rm=TRUE)) %>%
  dcast(formula=product~time, value="value")
trade_time <- trade_time[rowSums((!is.finite(as.matrix(trade_time[, -1])))) == 0, ]

#cluster analysis
pmatrix <- trade_time[, -1]
pmatrix <- pmatrix %>% t %>% scale %>% t %>% scale
finite <- rowSums((!is.finite(pmatrix))) == 0
pmatrix <- pmatrix[finite, ]
d=dist(pmatrix, method="euclidean")
#hierarchial clutering
pfit <- hclust(d, method="ward.D")
groups <- cutree(pfit, k=5)
clusters.h <- data.frame(trade_time[finite, 1], group = groups)
#k-means clustering
pclusters <- kmeans(pmatrix, 5, nstart=100, iter.max=100)
groups <- pclusters$cluster
clusters.k <- data.frame(trade_time[finite, 1], group = groups)

#correlation analysis
trade_time_corr <- trade_time[finite, -1]
rownames(trade_time_corr) <- trade_time$product[finite]
cormat <- round(cor(t(trade_time_corr)), 2)
pfit <- hclust(as.dist(1-cormat), method="ward.D")
groups <- cutree(pfit, k=5)
cormat <- cormat[pfit$order, pfit$order]
cormat.melt <- melt(cormat)
corrplot <- ggplot(data=cormat.melt) + aes(x=Var1, y=Var2, fill=value) + geom_tile() + scale_fill_gradient2(low="slateblue", mid="black", high="coral") + theme(text=element_text(size=10), axis.text.x=element_text(angle=90))
clusters.corr <- data.frame(product=trade_time$product[finite], group=groups)

#data output
write.csv(clusters.h, paste0("data/hierarchial_clustering.amount.import.csv"), row.names=FALSE)
write.csv(clusters.k, paste0("data/kmeans_clustering.amount.import.csv"), row.names=FALSE)
write.csv(clusters.corr, paste0("data/correlation_clustering.amount.import.csv"), row.names=FALSE)
write.csv(cormat.melt, paste0("data/correlation.amount.import.csv"), row.names=FALSE)
ggsave(paste0("data/correlation_plot.amount.import.png"), corrplot)

##############################
 #wholesale analysis by time#
##############################

#transform into viable form
wholesale_time <- wholesale %>%
  group_by(product, time = year(as.Date(time))) %>%
  summarize(value = sum(price, na.rm=TRUE)/sum(amount, na.rm=TRUE)) %>%
  dcast(formula=product~time, value="value")
wholesale_time <- wholesale_time[rowSums((!is.finite(as.matrix(wholesale_time[, -1])))) == 0, ]

#cluster analysis
pmatrix <- wholesale_time[, -1]
pmatrix <- pmatrix %>% t %>% scale %>% t %>% scale
finite <- rowSums((!is.finite(pmatrix))) == 0
pmatrix <- pmatrix[finite, ]
d=dist(pmatrix, method="euclidean")
#hierarchial clutering
pfit <- hclust(d, method="ward.D")
groups <- cutree(pfit, k=5)
clusters.h <- data.frame(wholesale_time[finite, 1], group = groups)
#k-means clustering
pclusters <- kmeans(pmatrix, 5, nstart=100, iter.max=100)
groups <- pclusters$cluster
clusters.k <- data.frame(wholesale_time[finite, 1], group = groups)

#correlation analysis
wholesale_time_corr <- wholesale_time[finite, -1]
rownames(wholesale_time_corr) <- wholesale_time$product[finite]
cormat <- round(cor(t(wholesale_time_corr)), 2)
pfit <- hclust(as.dist(1-cormat), method="ward.D")
groups <- cutree(pfit, k=5)
cormat <- cormat[pfit$order, pfit$order]
cormat.melt <- melt(cormat)
corrplot <- ggplot(data=cormat.melt) + aes(x=Var1, y=Var2, fill=value) + geom_tile() + scale_fill_gradient2(low="slateblue", mid="black", high="coral") + theme(text=element_text(size=10), axis.text.x=element_text(angle=90))
clusters.corr <- data.frame(product=wholesale_time$product[finite], group=groups)

#data output
write.csv(clusters.h, paste0("data/hierarchial_clustering.ratio.csv"), row.names=FALSE)
write.csv(clusters.k, paste0("data/kmeans_clustering.ratio.csv"), row.names=FALSE)
write.csv(clusters.corr, paste0("data/correlation_clustering.ratio.csv"), row.names=FALSE)
write.csv(cormat.melt, paste0("data/correlation.ratio.csv"), row.names=FALSE)
ggsave(paste0("data/correlation_plot.ratio.png"), corrplot)

#########################
 #Wholesale vs. fishery#
#########################

codeMapping <- read.csv("fisherycode_standardcode.csv")
wholesale_compare <- wholesale %>%
  group_by(code=productcode, time=year(date)*100+month(date)) %>% 
  summarize(amount_wholesale=sum(amount, na.rm=TRUE),
            price_wholesale=sum(price, na.rm=TRUE)) %>%
  ungroup
fishery_compare <- fishery %>%
  melt(1:6) %>%
  group_by(fishery_code=speciescode, time) %>%
  summarize(amount_fishery=sum(value[variable=="amount_live" | variable=="amount_fresh" | variable=="amount_frozen"], na.rm=TRUE),
            price_fishery=sum(value[variable=="price_live" | variable=="price_fresh" | variable=="price_frozen"], na.rm=TRUE)) %>% 
  left_join(codeMapping) %>%
  ungroup %>%
  select(code, time, amount_fishery, price_fishery)
compare <- left_join(wholesale_compare, fishery_compare)
View(compare)

for(c in 630204:630206) {
  values <- compare %>%
    filter(code==c) %>%
    group_by(time = as.yearmon(paste0(time%/%100, "-", time%%100))) %>%
    summarize(amount1 = sum(amount_wholesale, na.rm=TRUE),
              price1 = sum(price_wholesale, na.rm=TRUE),
              ratio1 = sum(price_wholesale, na.rm=TRUE)/sum(amount_wholesale, na.rm=TRUE),
              amount2 = sum(amount_fishery, na.rm=TRUE),
              price2 = sum(price_fishery, na.rm=TRUE),
              ratio2 = sum(price_fishery, na.rm=TRUE)/sum(amount_fishery, na.rm=TRUE),)
  p1 <- ggplot(data=values) +
    aes(x=ratio1, y=ratio2) +
    geom_point() + 
    stat_smooth(method="lm") + 
    xlab("wholesale") + ylab("fishery") + 
    ggtitle(code$소분류명[which(code$수산물품목코드==c)])
  p2 <- ggplot(data=values, aes(x=time)) +
    geom_line(aes(y=ratio1, color="도매")) +
    geom_line(aes(y=ratio2, color="생산")) +
    ylab("price") +
    ggtitle(code$소분류명[which(code$수산물품목코드==c)])
  print(code$소분류명[which(code$수산물품목코드==c)])
  cat("\n")
  print(c(amount=cor(values$amount1, values$amount2),
    price=cor(values$price1, values$price2),
    ratio=cor(values$ratio1, values$ratio2)))
  ggsave(paste0(code$소분류명[which(code$수산물품목코드==c)],"_도매_생산.ratio1.png"), p1)
  ggsave(paste0(code$소분류명[which(code$수산물품목코드==c)],"_도매_생산.ratio2.png"), p2)
}

######################
 #Wholesale vs. sale#
######################
wholesale_compare <- wholesale %>%
  group_by(code=productcode, time=as.Date(date)) %>% 
  summarize(amount_wholesale=sum(amount, na.rm=TRUE),
            price_wholesale=sum(price, na.rm=TRUE)) %>%
  ungroup
sale_compare <- sale %>%
  group_by(code=as.character(수산물표준코드), time=as.Date(위판일자)) %>% 
  summarize(amount_sale=sum(물량.KG.*수량, na.rm=TRUE),
            price_sale=sum(총.판매액, na.rm=TRUE)) %>% 
  ungroup
compare <- left_join(wholesale_compare, sale_compare)
View(compare)

for(c in 630204:630212) {
  values <- compare %>%
    filter(code==c) %>%
    group_by(time = as.yearmon(paste0(year(time), "-", month(time)))) %>%
    summarize(amount1 = sum(amount_wholesale, na.rm=TRUE),
              price1 = sum(price_wholesale, na.rm=TRUE),
              ratio1 = sum(price_wholesale, na.rm=TRUE)/sum(amount_wholesale, na.rm=TRUE),
              amount2 = sum(amount_sale, na.rm=TRUE),
              price2 = sum(amount_sale, na.rm=TRUE),
              ratio2 = sum(price_sale, na.rm=TRUE)/sum(amount_sale, na.rm=TRUE),)
  p1 <- ggplot(data=values) +
    aes(x=ratio1, y=ratio2) +
    geom_point() + 
    stat_smooth(method="lm") + 
    xlab("wholesale") + ylab("fishery") + 
    ggtitle(code$소분류명[which(code$수산물품목코드==c)])
  p2 <- ggplot(data=values, aes(x=time)) +
    geom_line(aes(y=ratio1, color="도매")) +
    geom_line(aes(y=ratio2, color="생산")) +
    ylab("price") +
    ggtitle(code$소분류명[which(code$수산물품목코드==c)])
  print(code$소분류명[which(code$수산물품목코드==c)])
  cat("\n")
  print(c(amount=cor(values$amount1, values$amount2),
          price=cor(values$price1, values$price2),
          ratio=cor(values$ratio1, values$ratio2)))
  ggsave(paste0(code$소분류명[which(code$수산물품목코드==c)],"_도매_생산.ratio1.png"), p1)
  ggsave(paste0(code$소분류명[which(code$수산물품목코드==c)],"_도매_생산.ratio2.png"), p2)
}

########################
 #Wholesale vs. import#
########################
crabbo <- "왕게"
wholesale_compare <- wholesale[wholesale$product=="왕게", ] %>%
  group_by(time=as.yearmon(date)) %>%
  summarize(amount_wholesale=sum(amount, na.rm=TRUE),
            price_wholesale=sum(price, na.rm=TRUE)) %>%
  ungroup
trade_compare <- trade[grep(crabbo, trade$product), ] %>%
  filter(import.export=="수입") %>%
  group_by(time=as.yearmon(paste0(date%/%100, "-", date%%100)), country) %>% 
  summarize(amount_trade=sum(amount, na.rm=TRUE),
            price_trade=sum(price, na.rm=TRUE)) %>%
  ungroup
compare <- left_join(trade_compare, wholesale_compare)

values <- compare %>%
  group_by(time) %>%
  summarize(amount1 = sum(amount_wholesale, na.rm=TRUE),
            price1 = sum(price_wholesale, na.rm=TRUE),
            ratio1 = sum(price_wholesale, na.rm=TRUE)/sum(amount_wholesale, na.rm=TRUE),
            amount2 = sum(amount_trade, na.rm=TRUE),
            price2 = sum(amount_trade, na.rm=TRUE),
            ratio2 = sum(price_trade, na.rm=TRUE)/sum(amount_trade, na.rm=TRUE),)
p1 <- ggplot(data=values) +
  aes(x=ratio1, y=ratio2) +
  geom_point() + 
  stat_smooth(method="lm") + 
  xlab("wholesale") + ylab("import") +
  ggtitle(crabbo)
p2 <- ggplot(data=values, aes(x=time)) +
  geom_line(aes(y=price1, color="도매")) +
  geom_line(aes(y=price2, color="수입")) +
  ylab("price") +
  ggtitle(crabbo)
print(c(amount=cor(values$amount1, values$amount2),
        price=cor(values$price1, values$price2),
        ratio=cor(values$amount1, values$amount2)))
print(p1)
print(p2)
ggsave(paste0(crabbo,"_도매_수입.price1.png"), p1)
ggsave(paste0(crabbo,"_도매_수입.amount2.png"), p2)

################################
 #wholesale analysis by market#
################################

wholesale %>%
  filter(substr(productcode, 1, 4) %in% c(6302, 7302)) %>%
  group_by(market, productcode) %>%
  summarize(amount=sum(amount,na.rm=TRUE), price=sum(price,na.rm=TRUE), ratio=sum(price,na.rm=TRUE)/sum(amount,na.rm=TRUE)) %>%
  ungroup %>%
  mutate(productcode = sapply(productcode, function(x) code$name[which(x==code$code)] %>% as.character)) %>%
  rename(product = productcode) %>%
  group_by(market) %>%
  top_n(5, wt=ratio) %>% write.csv("도매시장 주요판매품종별 판매금액_양비율.csv", quote=FALSE, row.names=FALSE)
ggplot(aes(y=market, weight=ratio, fill=product)) + geom_bar(position="fill")

################################
 #wholesale analysis by location#
################################

wholesale %>%
  filter(substr(productcode, 1, 4) %in% c(6302, 7302), !is.na(location)) %>%
  group_by(location, productcode) %>%
  summarize(amount=sum(amount,na.rm=TRUE), price=sum(price,na.rm=TRUE), ratio=sum(price,na.rm=TRUE)/sum(amount,na.rm=TRUE)) %>%
  top_n(5, wt=amount) %>% 
  group_by(productcode) %>%
  arrange(amount) %>% write.csv("품종별 주요 산지.csv", quote=FALSE, row.names=FALSE)
ggplot(aes(y=productcode, weight=ratio, fill=location)) + geom_bar(position="fill")
########################################
 #predicting future wholesale#
########################################
wholesale_predict <- wholesale %>%
  filter(productcode == 630205) %>%
  group_by(time=as.yearmon(date)) %>%
  summarize(amount = sum(amount, na.rm=TRUE), price = sum(price, na.rm=TRUE), 
            ratio = sum(price, na.rm=TRUE)/sum(amount, na.rm=TRUE))
amount <- ts(wholesale_predict$amount)
price <- ts(wholesale_predict$price, start = c(2013,1), frequency=12)
ratio <- ts(wholesale_predict$ratio, start = c(2013,1), frequency=12)
autoplot(forecast(amount))
autoplot(forecast(price))
autoplot(forecast(ratio))

########################################
#predicting future wholesale#
########################################
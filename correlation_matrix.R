r1Dinner <- read.csv("/Users/Crystal/Desktop/restaurants_cluster_freq/r1Dinner_freq.csv", stringsAsFactors = FALSE)
r1Dinner <- t(r1Dinner)
r1Dinner <- as.data.frame(r1Dinner, stringsAsFactors = FALSE)
names(r1Dinner) <- c(r1Dinner[1,] %>% unlist())
r1Dinner <- r1Dinner[2:nrow(r1Dinner),]
r1Dinner$order_id <- 0
r1Dinner <- r1Dinner[, c(ncol(r1Dinner), 2:(ncol(r1Dinner)-1))]

for (i in 1:nrow(r1Dinner)){
  r1Dinner[i,]$order_id<- rownames(r1Dinner)[i]
}

r1Dinner$order_id <- gsub("X","",r1Dinner$order_id)
r1Dinner$order_id <- as.integer(r1Dinner$order_id) 

r1Cluster <- read.csv("/Users/Crystal/Desktop/restaurants_cluster_freq/r1Dinner.csv", stringsAsFactors = FALSE)

names(r1Cluster)[1] <- "order_id"
r1Cluster <- r1Cluster[,c(1,27)]
View(r1Cluster)
r1DinnerCluster <- merge(r1Cluster, r1Dinner, by='order_id')
r1Dinner_R1D1 <- sqldf("SELECT * FROM r1DinnerCluster WHERE cluster='R1D1'")
r1Dinner_R1D2 <- sqldf("SELECT * FROM r1DinnerCluster WHERE cluster='R1D2'")
r1Dinner_R1D3 <- sqldf("SELECT * FROM r1DinnerCluster WHERE cluster='R1D3'")
r1Dinner_r1D1 <- r1Dinner_r1D1

r1Dinner_R1D1_cor <- r1Dinner_R1D1[,3:length(r1Dinner_R1D1)]
#r1Dinner_R1D1_cor <- cor(r1Dinner_R1D1_cor)

for (i in 1:length(r1Dinner_R1D1_cor)){
  r1Dinner_R1D1_cor[,i] <- as.numeric(r1Dinner_R1D1_cor[,i])
}

cor_R1D1 <- cor(r1Dinner_R1D1_cor)
View(cor_R1D1)

cor_R1D1_try <- as.data.frame(cor_R1D1)
library(reshape2)
melted_cor_R1D1 <- melt(cor_R1D1_try)
melted_cor_R1D1$Var1 <- 0
melted_cor_R1D1 <- melted_cor_R1D1[,c(3,1,2)]

for (i in 1:117){
  melted_cor_R1D1[i,1] <- rownames(cor_R1D1_try)[i]
}


for (i in 235:13689){
  melted_cor_R1D1[i,1] <- melted_cor_R1D1[(i-117),1]
}


library(ggplot2)
ggplot(data = melted_cor_R1D1, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + scale_fill_gradient(limits=c(-1,1))



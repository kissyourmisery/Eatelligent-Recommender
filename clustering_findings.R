excel_sheets('Transactions.xlsx')
t <- read_excel('Transactions.xlsx', sheet = 'Transactions')

library(data.table)
t1 <- data.table(t)

t$`Order Time` <- strptime(t$`Order Time`, "%Y-%m-%d %H:%M:%S")
t$Date <- strptime(t$Date, "%Y-%m-%d")
t <- t[order(t$`Restaurant Name`,t$Date,t$`Order Id`,t$`Order Time`),]

#Add lunch or dinner
t$ParseOrderTime <- strftime(t$`Order Time`,"%Y-%m-%d %H:%M:%S")
t$TimeOnly <- strptime(substr(t$ParseOrderTime,12,19),'%H:%M:%S')
t$Mealtime <- sapply(t$ParseOrderTime, function(x) {
  threshold <- strptime(paste0(substr(x,1,10),' 15:00:00'),"%Y-%m-%d %H:%M:%S")
  if(difftime(threshold,strptime(x,"%Y-%m-%d %H:%M:%S")) > 0) {
    y <- 'Lunch'
  }
  else {
    y <- 'Dinner'
  }
  return(y)
})

#split by restaurant and mealtime
restaurants <- list()
for (i in sort(unique(t$`Restaurant Name`))) {
  h1 <- sprintf("Inside Restaurant %d",i)
  print(h1)
  temp <- t[t$`Restaurant Name`==i,]
  for(mealtime in unique(temp$Mealtime)) {
    temp2 <- temp[temp$Mealtime==mealtime,]
    label <- paste0("r",i,mealtime)
    restaurants[[label]] <- temp2
    h2 <- sprintf("Attached %s to list",label)
    print(h2)
    h0 <- sprintf("No.of rows: %d",nrow(temp2))
    print(h0)
  }
}

#summary stats: order by volume
summary <- c()
for (label in names(restaurants)) {
  temp <- data.table(restaurants[[label]])
  summ_dish <- temp[,list('count'=.N),by='Dish Name']
  summ_dish <- summ_dish[order(-summ_dish$count),]
  summary[[label]] <- summ_dish
}

# add tf-idf
newrests <- c()
for (label in names(restaurants)) {
  a <- summary[[label]]
  temp <- restaurants[[label]]
  norders <- length(unique(temp$`Order Id`))
  h1 <- sprintf("Total orders in %s: %d", label,norders)
  print(h1)
  temp$idf <- sapply(temp$`Dish Name`, function(x){
    if(nrow(a[a$`Dish Name`==x,]) > 0) {
      nt <- a[a$`Dish Name`==x,]$count
    }
    else{
      nt <- 0
    }
    idf <- log(norders/(1+nt))
    h2 <- sprintf("Idf for %s: %.2f",x,idf)
    print(h2)
    return(idf)
  })
  temp$tfidf <- temp$`Item Quantity`*temp$idf
  newrests[[label]] <- temp
}

#cast to tf-idf matrix
casted <- c()
for (label in names(newrests)) {
  temp <- newrests[[label]]
  temp <- temp[,c('Order Id','Dish Name','tfidf')]
  print(head(temp))
  onehot <- dcast(temp,`Dish Name` ~ `Order Id`,fun.agg = mean, value.var = 'tfidf')
  #cast to matrix to enable whole matrix replacement methods
  onehot <- as.matrix(onehot)
  #replace NaN with 0
  onehot[is.nan(onehot)] <- 0
  #replaced to NA? Replace again to 0
  onehot[is.na(onehot)] <- 0
  # save as rownames
  dishes <- unlist(onehot[,1])
  #remove first col
  onehot <- onehot[,-1]
  #cast to numeric
  onehot <- apply(onehot,2,as.numeric)
  #apply rownames
  rownames(onehot) <- dishes
  casted[[label]] <- onehot
}

#compute distance matrix based on cosine similarity and conduct mds
library(lsa)
mdsdata <- c()
for (label in names(casted)) {
  cos <- 1 - cosine(casted[[label]])
  #cos <- 1 - cos
  data <- cmdscale(cos,eig=TRUE, k=2)
  #data$cluster <- kmeans(cosine,3)
  mdsdata[[label]] <- data
}

# Cluster using partitioning around medoids (pam)
pamdata <- c()
for (label in names(casted)) {
  data <- pam(t(casted[[label]]),3)
  pamdata[[label]] <- data
}

#Append cluster labels to transactions data and write to csv
newdat <- c()
for (label in names(newrests)) {
  temp <- newrests[[label]]
  clusters <- data.frame('Order.Id'=names(pamdata[[label]]$clustering),'cluster'=pamdata[[label]]$clustering)
  print(head(clusters))
  temp$Order.Id <- temp$`Order Id`
  temp <- merge(temp,clusters,by ='Order.Id')
  temp$cluster <- apply(temp,1, function(x) {
    if(x['Mealtime'] == 'Lunch'){
      y <- paste0('R',x['Restaurant Name'],'L',x['cluster'])
    }
    else {
      y <- paste0('R',x['Restaurant Name'],'D',x['cluster'])
    }
    return(y)
  })
  newdat[[label]] <-temp
  write.csv(temp, paste0(label,'.csv'), row.names = F)
}

# Coordinates on mds and write to csv
for (label in names(mdsdata)) {
  temp <- mdsdata[[label]]$points
  #temp$Order.Id <- rownames(temp)
  write.csv(temp,paste0('mds_',label,'.csv'),row.names = T)
}

#Write order x standardized freq matrix to csv
for (label in names(casted)) {
  write.csv(t(casted[[label]]),paste0(label,'_freq.csv'), row.names=T)
}

#R2 Lunch looks like there are just 2 clusters! Plot the correlation matrix
data <- pam(t(casted[['r2Lunch']]),2)
pamdata[['r2Lunch']] <- data
c1_names <- names(data$clustering[data$clustering==1])
c2_names <- names(data$clustering[data$clustering==2])
temp <- t(casted[['r2Lunch']])
#Filter frequency matrix by cluster and create correlation matrix
clust1 <- temp[rownames(temp) %in% c1_names,]
cor1 <- cor(clust1)
write.csv(cor1,'r2Lunch_clust1_corr.csv',row.names=T)
clust2<- temp[rownames(temp) %in% c2_names,]
cor2 <- cor(clust2)
cor2[is.na(cor2)] <- 0
write.csv(cor2,'r2Lunch_clust2_corr.csv',row.names=T)

#Repeat process for R3 Dinner
#R3Dinner train on 3 clusters
data <- pam(t(casted[['r3Dinner']]),3)
pamdata[['r3Dinner']] <- data

c1_names <- names(data$clustering[data$clustering==1])
c2_names <- names(data$clustering[data$clustering==2])
c3_names <- names(data$clustering[data$clustering==3])

temp <- t(casted[['r3Dinner']])
#Filter frequency matrix by cluster and create correlation matrix
clust1 <- temp[rownames(temp) %in% c1_names,]
cor1 <- cor(clust1)
cor1[is.na(cor1)] <- 0
write.csv(cor1,'r3Dinner_clust1_corr.csv',row.names=T)
clust2<- temp[rownames(temp) %in% c2_names,]
cor2 <- cor(clust2)
cor2[is.na(cor2)] <- 0
write.csv(cor2,'r3Dinner_clust2_corr.csv',row.names=T)
clust3<- temp[rownames(temp) %in% c3_names,]
cor3 <- cor(clust3)
cor3[is.na(cor3)] <- 0
write.csv(cor3,'r3Dinner_clust3_corr.csv',row.names=T)



#Further cluster clust1 into 2 clusters
#cos <- 1 - cosine(clust1)
#cos <- 1 - cos
#data <- cmdscale(cos,eig=TRUE, k=2)
#data$cluster <- kmeans(cosine,3)
#mdsdata[['r2Lunch_firstclust']] <- data
#data <- pam(t(clust1),2)
#pamdata[['r2Lunch']] <- data


# Plot by mds by cluster and save to png
for(label in names(mdsdata)) {
  temp <- mdsdata[[label]]$points
  print(head(temp))
  png(paste0(label,'_mds.png'),width=600,height=500,type='cairo')
  plot(temp[,1],temp[,2],col=pamdata[[label]]$clustering)
  dev.off()
}

#PCA alternative
#pcadata <- c()
#for (label in names(casted)) {
 # data <- prcomp(casted[[label]])
 # pcadata[[label]] <- data
#}




barplot()
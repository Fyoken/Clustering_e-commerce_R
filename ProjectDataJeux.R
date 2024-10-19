setwd("/data/Downloads")
set.seed(420)
library(dbscan)

# We chose R15
dataset <- read.csv("games.csv", header=TRUE)
dataframe <- as.data.frame(dataset)
dataframeyears <- c()
for(i in 1:1512){
  dataframeyears[i] <- as.numeric(unlist(strsplit(dataset$Release.Date[i],", "))[2])
}

# Expectation
plot(dataset$Plays,dataset$Rating,type="p",main="Plays/Rating",xlab="Plays",ylab="Rating")
plot(dataset$Playing,dataset$Rating,type="p",main="Playing/Rating",xlab="Playing",ylab="Rating")
plot(dataset$Wishlist,dataset$Rating,type="p",main="Whishlist/Rating",xlab="Wishlist",ylab="Rating")
plot(dataset$Playing,dataframeyears,type="p",main="Playing/Release.Date",xlab="Playing",ylab="Release.Date")
plot(dataset$Plays,dataframeyears,type="p",main="Plays/Release.Date",xlab="Plays",ylab="Release.Date")

# Minimum function of vector X with j not equal to i
minimum <- function(X,i) {
  Min <- Inf
  for(j in X){
    if(j<Min && j!=i){
      Min <- i
    }
  }
  return(Min)
}

# K-means
k <- 15
dataframe <- NULL
dataframe$Playing <- dataset$Playing
dataframe$Release.Date <- dataframeyears

# We plot the values of the dataset for Lloyd and MacQueen algorithms
km <- kmeans(dataframe,k,100,5,"Lloyd")
plot(dataset$V1,dataset$V2,type="p",col=km$cluster,main="Clustering of R15 (Kmeans with k = 15 and algorithm = Lloyd)",xlab="x coordinate",ylab="y coordinate")
centers <- as.data.frame(km$centers)
points(centers,pch=15)


# betweenss gives us the sum of squares between clusters while totss gives us the whole TD²
TD2K15L <- km$totss # 12773
efficiencyK15L <- km$betweenss/km$totss # ~98-99 %
centers <- as.data.frame(km$centers)

# We calculate every single silouhette for each point
silhouettes<-c()
for (i in 1:600) {
  j <- as.integer(km$cluster[i])
  a <- sqrt((centers$V1[j]-dataset$V1[i])**2+(centers$V2[j]-dataset$V2[i])**2)
  b <- minimum(sqrt((centers$V1-dataset$V1[i])**2+(centers$V2-dataset$V2[i])**2),i)
  silhouettes[i]<-((b-a)/max(a,b))
}
silhouettes
silhouetteK15L <- mean(silhouettes) # ~0.995

# dataset$cluster<-km$cluster
# View(dataset)

km <- kmeans(dataframe,k,100,5,"MacQueen")
plot(dataset$V1,dataset$V2,type="p",col=km$cluster,main="Clustering of R15 (Kmeans with k = 15 and algorithm = MacQueen)",xlab="x coordinate",ylab="y coordinate")
centers <- as.data.frame(km$centers)
points(centers,pch=15)
TD2K15M <- km$totss # 12773
efficiencyK15M <- km$betweenss/km$totss # ~98-99 %

centers <- as.data.frame(km$centers)

# We calculate every single silouhette for each point
silhouettes<-c()
for (i in 1:600) {
  j <- as.integer(km$cluster[i])
  a <- sqrt((centers$V1[j]-dataset$V1[i])**2+(centers$V2[j]-dataset$V2[i])**2)
  b <- minimum(sqrt((centers$V1-dataset$V1[i])**2+(centers$V2-dataset$V2[i])**2),i)
  silhouettes[i]<-((b-a)/max(a,b))
}
silhouettes
silhouetteK15M <- mean(silhouettes) # ~0.995

k1 <- 10

km <- kmeans(dataframe,k1,100,5,"Lloyd")
plot(dataset$V1,dataset$V2,type="p",col=km$cluster,main="Clustering of R15 (Kmeans with k = 10 and algorithm = Lloyd)",xlab="x coordinate",ylab="y coordinate")
centers <- as.data.frame(km$centers)
points(centers,pch=15)
TD2K10L <- km$totss # 12773
efficiencyK10L <- km$betweenss/km$totss # ~93-97 %

centers <- as.data.frame(km$centers)

# We calculate every single silouhette for each point
silhouettes<-c()
for (i in 1:600) {
  j <- as.integer(km$cluster[i])
  a <- sqrt((centers$V1[j]-dataset$V1[i])**2+(centers$V2[j]-dataset$V2[i])**2)
  b <- minimum(sqrt((centers$V1-dataset$V1[i])**2+(centers$V2-dataset$V2[i])**2),i)
  silhouettes[i]<-((b-a)/max(a,b))
}
silhouettes
silhouetteK10L <- mean(silhouettes) # ~0.987



km <- kmeans(dataframe,k1,100,5,"MacQueen")
plot(dataset$V1,dataset$V2,type="p",col=km$cluster,main="Clustering of R15 (Kmeans with k = 10 and algorithm = MacQueen)",xlab="x coordinate",ylab="y coordinate")
centers <- as.data.frame(km$centers)
points(centers,pch=15)
TD2K10M <- km$totss # 12773
efficiencyK10M <- km$betweenss/km$totss # ~91-96 %

centers <- as.data.frame(km$centers)

# We calculate every single silouhette for each point
silhouettes<-c()
for (i in 1:600) {
  j <- as.integer(km$cluster[i])
  a <- sqrt((centers$V1[j]-dataset$V1[i])**2+(centers$V2[j]-dataset$V2[i])**2)
  b <- minimum(sqrt((centers$V1-dataset$V1[i])**2+(centers$V2-dataset$V2[i])**2),i)
  silhouettes[i]<-((b-a)/max(a,b))
}
silhouettes
silhouetteK10M <- mean(silhouettes) # ~0.987

k2 <- 20

km <- kmeans(dataframe,k2,100,5,"Lloyd")
plot(dataset$V1,dataset$V2,type="p",col=km$cluster,main="Clustering of R15 (Kmeans with k = 20 and algorithm = Lloyd)",xlab="x coordinate",ylab="y coordinate")
centers <- as.data.frame(km$centers)
points(centers,pch=15)
TD2K20L <- km$totss # 12773
efficiencyK20L <- km$betweenss/km$totss # ~99 % but empty clusters possible

centers <- as.data.frame(km$centers)

# We calculate every single silouhette for each point
silhouettes<-c()
for (i in 1:600) {
  j <- as.integer(km$cluster[i])
  a <- sqrt((centers$V1[j]-dataset$V1[i])**2+(centers$V2[j]-dataset$V2[i])**2)
  b <- minimum(sqrt((centers$V1-dataset$V1[i])**2+(centers$V2-dataset$V2[i])**2),i)
  silhouettes[i]<-((b-a)/max(a,b))
}
silhouettes
silhouetteK20L <- mean(silhouettes) # ~0.997 but empty clusters possible

km <- kmeans(dataframe,k2,100,5,"MacQueen")
plot(dataset$V1,dataset$V2,type="p",col=km$cluster,main="Clustering of R15 (Kmeans with k = 20 and algorithm = MacQueen)",xlab="x coordinate",ylab="y coordinate")
centers <- as.data.frame(km$centers)
points(centers,pch=15)
TD2K20M <- km$totss # 12773
efficiencyK20M <- km$betweenss/km$totss # ~99 % but empty clusters possible

centers <- as.data.frame(km$centers)

# We calculate every single silouhette for each point
silhouettes<-c()
for (i in 1:600) {
  j <- as.integer(km$cluster[i])
  a <- sqrt((centers$V1[j]-dataset$V1[i])**2+(centers$V2[j]-dataset$V2[i])**2)
  b <- minimum(sqrt((centers$V1-dataset$V1[i])**2+(centers$V2-dataset$V2[i])**2),i)
  silhouettes[i]<-((b-a)/max(a,b))
}
silhouettes
silhouetteK20M <- mean(silhouettes) # ~0.997 but empty clusters possible

# TD² and simplified silhouette give us the same results: all of them are good clusterings 
# but k=15 seems to be the best compromise between less good quality (k=10) and empty clusters (k=20)


# OPTICS
opt <- optics(dataframe, eps = 0.5, minPts = 12)
plot(opt)
clustOpt <- extractDBSCAN(opt,0.5)
plot(dataset$V1,dataset$V2,type="p",col=clustOpt$cluster,main="Clustering of R15 (OPTICS with epsilon = 0.5 and MinPts = 12)",xlab="x coordinate",ylab="y coordinate")

means1 <- c()
means2 <- c()
dataset$optcluster <- clustOpt$cluster

# Gives the mean of cluster i
Mean <- function(V1,V2,cluster,i) {
  Y1 <- c()
  Y2 <- c()
  k <- 1
  for (j in 1:600) {
    if (cluster[j]==i) {
      Y1[k] <- V1[j]
      Y2[k] <- V2[j]
      k <- k+1
    }
  }
  return(c(mean(Y1),mean(Y2)))
}
for(i in 1:max(clustOpt$cluster)) {
  means1[i] <- Mean(dataset$V1,dataset$V2,dataset$optcluster,i)[1]
  means2[i] <- Mean(dataset$V1,dataset$V2,dataset$optcluster,i)[2]
}

points(means1,means2,pch=15)

# Expectation
plot(dataset$V1,dataset$V2,type="p",col=dataset$V3,main="Clustering of R15 (Expectation)",xlab="x coordinate",ylab="y coordinate")
meansE1 <- c()
meansE2 <- c()
for(i in 1:max(dataset$V3)) {
  meansE1[i] <- Mean(dataset$V1,dataset$V2,dataset$V3,i)[1]
  meansE2[i] <- Mean(dataset$V1,dataset$V2,dataset$V3,i)[2]
}

points(meansE1,meansE2,pch=15)

# meanError ~= 0.4
meanError <- sum(sqrt((means1-meansE1)^2+(means2-meansE2)^2))

numberOfNoisePoints <- 0
for (i in 1:600) {
  if (clustOpt$cluster[i]==0) {
    numberOfNoisePoints <- numberOfNoisePoints + 1
  }
}

# 13 noise points ~= 2.16 % of the dataset
numberOfNoisePoints

# DBSCAN
db <- dbscan(dataframe, eps = 0.5, minPts = 12)
plot(dataset[1:2], col=db$cluster+1, main="DBSCAN clustering of R15")
meanscenters <- c()

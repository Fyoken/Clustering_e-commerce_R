# Load required packages
library(magrittr)
library(tidymodels)
library(hopkins)

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


silhouette <- function(km,df) {
  centers <- as.data.frame(km$centers)
  
  # We calculate every single silouhette for each point
  silhouettes<-c()
  for (i in 1:4338) {
    j <- as.integer(km$cluster[i])
    a <- sqrt((centers$Amount[j]-df$Amount[i])**2+(centers$Frequency[j]-df$Frequency[i])**2)
    b <- minimum(sqrt((centers$Amount-df$Amount[i])**2+(centers$Frequency-df$Frequency[i])**2),i)
    silhouettes[i]<-((b-a)/max(a,b))
  }
  silhouettes
  return(mean(silhouettes))
}


# Read CSV dataset
setwd("/data/Downloads")
raw_df <- read.csv("data.csv")

# Data preprocessing
df <- raw_df %>%
  select(-c(StockCode, InvoiceDate, Description, Country)) %>%
  filter(Quantity > 0, UnitPrice > 0) %>%
  mutate(Total = Quantity * UnitPrice) %>%
  select(-c(Quantity, UnitPrice)) %>%
  na.omit()

# Calculate Amount and Frequency
Amount <- df %>%
  group_by(CustomerID) %>%
  summarize(Amount = sum(Total)) %>%
  rename(CustomerID = CustomerID)

Frequency <- df %>%
  group_by(CustomerID) %>%
  summarize(Frequency = n()) %>%
  rename(CustomerID = CustomerID)

# Merge Amount and Frequency dataframes
df1 <- left_join(Amount, Frequency, by = "CustomerID") %>%
  select(-c(CustomerID))

# Plot boxplots for Amount and Frequency
par(mfrow = c(1, 2))
boxplot(df1$Amount, main = "Amount", col = "red")
boxplot(df1$Frequency, main = "Frequency", col = "blue")

# Outlier detection using Isolation Forest algorithm
#set.seed(123)
#library(randomForest)
#model <- randomForest(df1, ntree = 150, keep.inbag = FALSE, replace = FALSE, classwt = NULL)
#anomaly <- data.frame(scores = predict(model, newdata = df1, type = "anomaly"), anomaly = model$votes[, 1])
#df2 <- cbind(df1, anomaly$scores)

# Remove outliers
#df2 <- df2 %>%
 # filter(anomaly == 0) %>%
  #select(-c(scores, anomaly))

# Scale data
#df3 <- df2 %>%
 # select(-c(CustomerID)) %>%
  #scale()
df3 <- df1

# Determine optimal number of clusters using Elbow Method
wss <- sapply(1:30, function(k){kmeans(df3, k, nstart = 25 )$tot.withinss})
plot(1:30, wss, type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K", ylab="Total within-clusters sum of squares")

# Perform K-means clustering with 5 clusters
set.seed(123)
kmeans_model <- kmeans(df3, centers = 5, nstart = 25, algorithm = "Lloyd")
plot(df3$Amount,df3$Frequency,type="p",col=kmeans_model$cluster,main="Clustering of Frequency/Amount (Kmeans with k = 5 and algorithm = Lloyd)",xlab="x coordinate",ylab="y coordinate")
centers <- as.data.frame(kmeans_model$centers)
points(centers,pch=15)

# Calculate Silhouette Score
score <- silhouette(kmeans_model,df3)
cat("Silhouette Average Score:", score, "\n")


# Hierarchical clustering with Ward's method and Euclidean distance
dendrogram <- hclust(dist(df3), method = "ward.D2")
plot(dendrogram, main = "Dendrogram", xlab = "Index", ylab = "Distance")


# OPTICS
eps <- 40
opt <- optics(df3, eps = eps, minPts = 15)
plot(opt)
clustOpt <- extractDBSCAN(opt,eps)
plot(df3$Amount,df3$Frequency,type="p",col=clustOpt$cluster,main="Clustering of Frequency/Amount (OPTICS with epsilon = 40 and MinPts = 15)",xlab="x coordinate",ylab="y coordinate",xlim = c(0,4000), ylim=c(0,400))
clustOpt

means1 <- c()
means2 <- c()
df1$optcluster <- clustOpt$cluster

# Gives the mean of cluster i
Mean <- function(V1,V2,cluster,i) {
  Y1 <- c()
  Y2 <- c()
  k <- 1
  for (j in 1:600) {
    if (cluster[j]==i) {
      Y1[k] <- as.numeric(V1[j])
      Y2[k] <- as.numeric(V2[j])
      k <- k+1
    }
  }
  return(c(mean(na.omit(Y1)),na.omit(mean(Y2))))
}
for(i in 1:max(clustOpt$cluster)) {
  means1[i] <- Mean(df1$Amount,df1$Frequency,df1$optcluster,i)[1]
  means2[i] <- Mean(df1$Amount,df1$Frequency,df1$optcluster,i)[2]
}

points(means1,means2,pch=15,col="purple")


numberOfNoisePoints <- 0
for (i in 1:4338) {
  if (clustOpt$cluster[i]==0) {
    numberOfNoisePoints <- numberOfNoisePoints + 1
  }
}

# 906 noise points ~= 21 % of the dataset
numberOfNoisePoints


# We calculate every single silouhette for each point
silhouettes <- c()
for (i in 1:4338) {
  if (clustOpt$cluster[i]!=0){
    j <- as.integer(clustOpt$cluster[i])
    a <- sqrt((means1[j]-df1$Amount[i])**2+(means2[j]-df1$Frequency[i])**2)
    b <- minimum(sqrt((means1-df1$Amount[i])**2+(means2-df1$Frequency[i])**2),i)
    silhouettes[i]<-((b-a)/max(a,b))
  } else {
    silhouettes[i] <- 0
  }
}
silhouettes
silhouetteOpticsWithNoise <- mean(silhouettes) # ~0.55

TD2Opticss <- c(0,0,0,0,0)
for (i in 1:4338) {
  if (clustOpt$cluster[i]!=0) {
    j <- as.integer(clustOpt$cluster[i])
    d2 <- (means1[j]-df1$Amount[i])**2+(means2[j]-df1$Frequency[i])**2
    TD2Opticss[j] <- TD2Opticss[j]+d2
  }
}
TD2Optics <- sum(TD2Opticss)

# Other OPTICS to compare
opt2 <- optics(df3, eps = 80, minPts = 20)
plot(opt2)
clustOpt2 <- extractDBSCAN(opt2,80)
plot(df3$Amount,df3$Frequency,type="p",col=clustOpt2$cluster,main="Clustering of Frequency/Amount (OPTICS with epsilon = 80 and MinPts = 20)",xlab="x coordinate",ylab="y coordinate",xlim = c(0,4000), ylim=c(0,400))
means1 <- c()
means2 <- c()
df1$optcluster2 <- clustOpt2$cluster

for(i in 1:2) {
  means1[i] <- Mean(df1$Amount,df1$Frequency,df1$optcluster2,i)[1]
  means2[i] <- Mean(df1$Amount,df1$Frequency,df1$optcluster2,i)[2]
}

points(means1,means2,pch=15,col="purple")


numberOfNoisePoints <- 593 # 13.6%

# We calculate every single silouhette for each point
silhouettes <- c()
for (i in 1:4338) {
  if (clustOpt2$cluster[i]!=0){
    j <- as.integer(clustOpt2$cluster[i])
    a <- sqrt((means1[j]-df1$Amount[i])**2+(means2[j]-df1$Frequency[i])**2)
    b <- minimum(sqrt((means1-df1$Amount[i])**2+(means2-df1$Frequency[i])**2),i)
    silhouettes[i]<-((b-a)/max(a,b))
  } else {
    silhouettes[i] <- 0
  }
}
silhouettes
silhouetteOpticsWithNoise2 <- mean(silhouettes) # worse

TD2Opticss <- c(0,0)
for (i in 1:4338) {
  if (clustOpt2$cluster[i]!=0) {
    j <- as.integer(clustOpt2$cluster[i])
    d2 <- (means1[j]-df1$Amount[i])**2+(means2[j]-df1$Frequency[i])**2
    TD2Opticss[j] <- TD2Opticss[j]+d2
  }
}
TD2Optics2 <- sum(TD2Opticss) # worse


# DBSCAN
db <- dbscan(df3, eps = 40, minPts = 15)
plot(df1$Amount,df1$Frequency, col=db$cluster+1, main="DBSCAN clustering of Frequency/Amount, , epsilon = 40, minPoints = 15",xlim=c(0,3000),ylim=c(0,800))
df1$clusterdb <- db$cluster
meansdb1 <- c(0,0,0,0,0)
meansdb2 <- c(0,0,0,0,0)
k <- c(0,0,0,0,0)
for (i in 1:4338) {
  if (df1$clusterdb[i]>0) {
    j <- as.integer(df1$clusterdb[i])
    meansdb1[j] <- meansdb1[j] + df1$Amount[i]
    meansdb2[j] <- meansdb2[j] + df1$Frequency[i]
    k[j] <- k[j] + 1
  }
}
meansdb1 <- meansdb1/k
meansdb2 <- meansdb2/k

points(meansdb1,meansdb2,pch=15,col="purple")

# We calculate every single silouhette for each point
silhouettes <- c()
for (i in 1:4338) {
  if (db$cluster[i]!=0){
    j <- as.integer(db$cluster[i])
    a <- sqrt((meansdb1[j]-df1$Amount[i])**2+(meansdb2[j]-df1$Frequency[i])**2)
    b <- minimum(sqrt((meansdb1-df1$Amount[i])**2+(meansdb2-df1$Frequency[i])**2),i)
    silhouettes[i]<-((b-a)/max(a,b))
  } else {
    silhouettes[i] <- 0
  }
}
silhouettes
silhouetteDB <- mean(silhouettes) # ~0.56

TD2DBs <- c(0,0,0,0,0)
for (i in 1:4338) {
  if (db$cluster[i]!=0) {
    j <- as.integer(db$cluster[i])
    d2 <- (meansdb1[j]-df1$Amount[i])**2+(meansdb2[j]-df1$Frequency[i])**2
    TD2DBs[j] <- TD2DBs[j]+d2
  }
}
TD2DB <- sum(TD2DBs) # 698659076
# 903 noise points = 21%

# DBSCAN 2
db2 <- dbscan(df3, eps = 80, minPts = 20)
plot(df1$Amount,df1$Frequency, col=db2$cluster+1, main="DBSCAN clustering of Frequency/Amount, , epsilon = 80, minPoints = 20",xlim=c(0,4000),ylim=c(0,800))
df1$clusterdb2 <- db2$cluster
meansdb1 <- c(0,0)
meansdb2 <- c(0,0)
k <- c(0,0)
for (i in 1:4338) {
  if (df1$clusterdb2[i]>0) {
    j <- as.integer(df1$clusterdb2[i])
    meansdb1[j] <- meansdb1[j] + df1$Amount[i]
    meansdb2[j] <- meansdb2[j] + df1$Frequency[i]
    k[j] <- k[j] + 1
  }
}
meansdb1 <- meansdb1/k
meansdb2 <- meansdb2/k

points(meansdb1,meansdb2,pch=15,col="purple")

# We calculate every single silouhette for each point
silhouettes <- c()
for (i in 1:4338) {
  if (db2$cluster[i]!=0){
    j <- as.integer(db2$cluster[i])
    a <- sqrt((meansdb1[j]-df1$Amount[i])**2+(meansdb2[j]-df1$Frequency[i])**2)
    b <- minimum(sqrt((meansdb1-df1$Amount[i])**2+(meansdb2-df1$Frequency[i])**2),i)
    silhouettes[i]<-((b-a)/max(a,b))
  } else {
    silhouettes[i] <- 0
  }
}
silhouettes
silhouetteDB2 <- mean(silhouettes) # ~0.52 worse

TD2DBs <- c(0,0)
for (i in 1:4338) {
  if (db2$cluster[i]!=0) {
    j <- as.integer(db2$cluster[i])
    d2 <- (meansdb1[j]-df1$Amount[i])**2+(meansdb2[j]-df1$Frequency[i])**2
    TD2DBs[j] <- TD2DBs[j]+d2
  }
}
TD2DB2 <- sum(TD2DBs) # 1774501604 worse
# 591 noise points = 13.6%
     
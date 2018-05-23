#install packages
install.packages("Rserve")
library(Rserve)
Rserve(args='--vanilla')

# set WD
setwd("~/Desktop/Pharma_Hackathon")

# import the dataset
cosmetics = read.csv("pharma2.csv")

# Observe the dataset
str(cosmetics)

# Subset the products sold/profit/profitmargin data
sold = cosmetics[,c(2,9)]
profit = cosmetics[,c(2,12)]
profitmargin = cosmetics[,c(2,13)]

# Plot subset data
plot(sold, main = "Products Sold", pch =20, cex =2)
plot(profit, main = "Profit", pch =20, cex =2)
plot(profitmargin, main = "Margin", pch =20, cex =2)

# Perform K-Means with 2 clusters
set.seed(7)
km1 = kmeans(sold, 2, nstart=100)

# Plot results
plot(sold, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)

km1a = kmeans(profit, 2, nstart=100)
plot(profit, col =(km1a$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)

km1b = kmeans(profitmargin, 2, nstart=100)
plot(profitmargin, col =(km1b$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)

# Check for the optimal number of clusters given the data
mydata <- sold
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

mydataa <- profit
wssa <- (nrow(mydataa)-1)*sum(apply(mydataa,2,var))
for (i in 2:15) wssa[i] <- sum(kmeans(mydataa,
                                     centers=i)$withinss)

plot(1:15, wssa, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

mydatab <- profitmargin
wssb <- (nrow(mydatab)-1)*sum(apply(mydatab,2,var))
for (i in 2:15) wssb[i] <- sum(kmeans(mydatab,
                                     centers=i)$withinss)

plot(1:15, wssb, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

# Perform K-Means with the optimal number of clusters identified from the Elbow method
set.seed(7)
km2 = kmeans(sold, 4, nstart=100)
km2a = kmeans(profit, 4, nstart=100)
km2b = kmeans(profitmargin, 4, nstart=100)

# Examine the result of the clustering algorithm
km2
km2a
km2b

# Plot results
plot(sold, col =(km2$cluster +1) , main="K-Means result with 4 clusters (Sold)", pch=20, cex=2)

plot(profit, col =(km2a$cluster +1) , main="K-Means result with 4 clusters (Profit)", pch=20, cex=2)

plot(profitmargin, col =(km2b$cluster +1) , main="K-Means result with 4 clusters (Margin)", pch=20, cex=2)

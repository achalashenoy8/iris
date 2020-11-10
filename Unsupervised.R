install.packages("NbClust")
install.packages("ggplot2")
install.packages("factoextra")
library(NbClust)
library(factoextra)
Iris = read.csv("C:/Users/achala/Desktop/R Project/Iris.csv")
View(Iris)
Iris.features = Iris
Iris.features$Species<-NULL
View(Iris.features)
results <- kmeans(Iris.features, 3)
results
table(Iris$Species,results$cluster)


mydata  = Iris.features
mydata = as.data.frame(unclass(mydata))
summary(mydata)
dim(mydata)
# We can now remove any records that have NAs
myDataClean = na.omit(mydata)
dim(myDataClean)
summary(myDataClean)
scaled_data = as.matrix(scale(myDataClean))
kmm = kmeans(scaled_data,3,nstart = 50,iter.max = 15)
kmm

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- scaled_data
wss <- sapply(1:k.max,function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Within-clusters sum of squares")
#Scatter Plot for finding centroid clusters
plot(Iris[c("PetalLengthCm","PetalWidthCm")],col=results$cluster)
plot(Iris[c("SepalLengthCm","SepalWidthCm")],col=results$cluster)


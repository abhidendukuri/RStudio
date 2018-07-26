# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
df <- scale(wine[-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234) {
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(df)  

# Exercise 2:
#   * How many clusters does this method suggest?
#     15 clusters
#
#   * Why does this method work? What's the intuition behind it?
#     This method works because it shows that as you add more clusters, the Within Groups Sum of Squares
#     starts to decrease which in turns means that the means are approaching the same value 
#
#   * Look at the code for wssplot() and figure out how it works
#     First it creates a wss array that depends on the number of rows in the dataset
#     Next, the function iterates through the specified number of clusters (in this case 15) and creates a 
#       point on the graph that indicates the cluster's WSS value
#     After iterating through all the clusters, it then plots the graph



# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest? 3


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df, centers = 3)
fit.km

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

kmOut <- table(fit.km$cluster, wine$Type)
randIndex(kmOut)

# Considering the outputs, I would say this is a good clustering and
# especially since the randIndex() adjusted rank index function determined
# about a 0.897 fit which is considered very good


# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
clusplot(df, fit.km$cluster)

# Looking at the clusplot, it is very noticeable that the outlined cluster circles
# almost entirely envelop each respective cluster component almost perfectly

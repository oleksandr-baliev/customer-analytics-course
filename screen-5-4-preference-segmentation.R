# Title     : TODO
# Objective : TODO
# Created by: oleksandr.baliev

# First, we need to install these packages
library(psych)
library(tidyverse)
library(data.table)
library(olsrr)
library(corrplot)

# Specify your path under "/Users/./"" to the file car_pref1.csv
car_pref1 <- read.csv("csv/car_pref1.csv")

psych::describe(car_pref1)

# 1. Standardize the variables of interest
car_pref1[, 2:11] <- scale(car_pref1[, 2:11])

psych::describe(car_pref1)

#
# Hierarchical cluster analysis
#
# 2. Run a cluster analysis on a distance matrix and using the Ward method
car_ward <- hclust(dist(car_pref1[, 2:11]), method="ward.D2")

#- Do you recall from the earlier practice with hierarchial clustering how to generate the resulting agglomeration schedule?

# Scree plot
plot(rev(car_ward$height), # rev is used to plot from low to high values on Y axis
     type = "b",           # to display both the points and lines
     ylab = "Dissimilarity measure",
     xlab = "Number of clusters",
     main = "Scree plot",
     col = "darkblue",
     pch = 16)             # specify the plot symbol: 16 = filled circle
abline(v = 5, lty = 2, col = "darkred") # draw a vertical line at v = 5


# Dendrogram
library(dendextend)
plot(set(as.dendrogram(car_ward),
         "branches_k_color", # to highlight the cluster solution with a color
         k = 5),
     ylab = "Distance",
     main = "Dendrogram",
     cex = 0.2)             # Size of labels
rect.hclust(car_ward, k = 2, border = "darkblue")  # draw red borders around 2 clusters
rect.hclust(car_ward, k = 5, border = "darkred")  # draw red borders around 5 clusters

#Based on the scree plot and the dendrogram, we can choose a two-cluster solution, a four-cluster solution, a five-cluster solution, or a six-cluster solution.
# Since it may be argued that the two-cluster solution is too simple to give an adequate representation of the differences in preference, we shall proceed with the five-cluster solution (but note that the other two choices of 4 or 6 clusters may also be reasonable)
memb <- cutree(car_ward, k = 5)

table(memb)

#
# K-means cluster analysis
#
cent <- NULL
for(k in 1:5){
  cent <- rbind(cent, colMeans(car_pref1[memb == k, , drop = FALSE]))
}

# average preference levels for each of the ten brands in each cluster
round(cent[, 2:11], 3)

# K-means clustering
# The second stage of the analysis is running the K-means clustering method.
# Recall that K-means needs two inputs: the number of clusters K and an initial choice (seeds) for the cluster centers - these will then be updated as the algorithm progresses.
# We shall use the results from the previous hierarchical clustering as inputs into K-means.
# Thus, we are looking for K = 5 clusters, and we shall take as initial cluster centers the values computed in the table above (the centroids of the clusters identified by Ward’s method).


set.seed(1)
car_kmeans <- kmeans(car_pref1[, 2:11], centers = cent[, 2:11], iter.max = 10)

# The printed output displays:
# - the cluster means or centers: A matrix whose rows are a cluster number (1 to K) and whose columns are variables
# - the clustering vector: A vector of indices (from 1:K) indicating the cluster to which each observation in the data set is allocated
car_kmeans

# The kmeans() function returns a list of components, the most informative of which are the following:
#
# cluster: A vector of indices (from 1:K) indicating the cluster to which each point is allocated
# centers: A matrix of cluster centers (cluster means)
# size: The number of observations in each cluster
str(car_kmeans)

#- totss: The total sum of squares (TSS) that measures the total variance in the data.
#- withinss: Vector of within-cluster sum of squares, one component per cluster
#- tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss)
#- betweenss: The between-cluster sum of squares, i.e. totss???tot.withinss

car_kmeans$size

car_kmeans$centers

# We now can calculate the change in each cluster from the K-means clustering - this will be the difference between the original cluster centers (recall that these were earlier computed with the Ward’s method and saved in cent[i, 2:11]) and the cluster centers of the final solution:
change <- NULL
for (i in 1:5){
  change <- rbind(change, car_kmeans$centers[i,]-cent[i,2:11])
}

round(change, 3)

#he table of pairwise distances suggests that clusters 4 and 5 are the furthest away from each other (in terms of preferences),
# while clusters 2 and 3 are the closest.
dist(car_kmeans$centers)

#
# Interpretation
#

# We may store the resulted clustering solution in our dataset so that each observation in a row has an assigned cluster.
car_pref1 <- cbind(car_pref1, cluster2 = car_kmeans$cluster)


# Note that the overall average for our ten preference variables is 0 because we standardized the values prior to the cluster analysis.
# The average value for any standardized variable in a cluster should then be interpreted as a deviation from the average pattern in the data. It is possible to proceed with the interpretation on the basis of the matrix giving cluster averages alone but, as often the case, it is easier to gain more insights if we visualize these values in a graphical form.
# To this end we can generate a heat map of the matrix using the following code (note that the code is substantially more complex than what we covered so far, and you are not expected to use it; it is included here, however, for reference in case you would like to play with it)

# Overall average in the sample
round(colMeans(car_pref1[, 2:11]), 3)

# Average for Cluster 1
round(colMeans(car_pref1[car_pref1$cluster == 1, 2:11]), 3)

# Average for each cluster with one step
aggregate(car_pref1[, 2:11],
          by = list(cluster = car_pref1$cluster),
          FUN = mean)

library(tidyverse)

# From the average car preferences we see that the cluster 1 evaluates all cars highly and especially prefers Chrysler (by almost two standard deviations compared to the sample average), the only minivan included in the study.
# Clusters 2, 3, and 4, on the contrary, have a lower than sample average preference for Chrysler.
# Clusters 4 and 5 seem to have opposite preferences for most of the cars; for example, cluster 4 expresses high preferences for BMW, Infiniti, Lexus, and Mercedes, whereas cluster 5 values them much lower.

# Using this profiling approach, it is possible to label the clusters in a similar way as when we used factor loadings to label the factors in factor analysis. How would you name the clusters?


dt.cluster <- aggregate(car_pref1[, 2:11],
                        by = list(cluster = car_pref1$cluster),
                        FUN = mean)
dt.cluster %>%
  gather(carmake, value, -cluster) %>% # to transfrom from wide to long format
  mutate(carmake = fct_rev(factor(carmake))) %>% # to reverse the order of car makes' names on the plot
  ggplot(aes(x = factor(cluster), y = carmake)) +
  geom_tile(aes(fill = round(value, digits = 2))) +
  geom_text(aes(label = round(value, digits = 2)), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Average\n value", low = "lightgrey", high = "darkblue") +
  theme_minimal() +
  labs(title = "Average car preferences in each cluster",
       x = "Cluster",
       y = " ") +
  theme(legend.position="right",
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.ticks = element_blank())


# install.packages("factoextra")
library(factoextra)
#  If there are more than two dimensions (variables), fviz_cluster will perform principal component analysis (PCA) and plot the data points according to the first two principal components that explain the majority of the variance.
#  In our case, we have ten preference variables, thus PCA is necessary.
#  Such a plot allows us to visualize the relative separation between the clusters.
fviz_cluster(car_kmeans, data = car_pref1) +
  theme_bw()

# Title     : TODO
# Objective : TODO
# Created by: oleksandr.baliev

# First, we need to install these packages
library(psych)
library(tidyverse)
library(data.table)
library(olsrr)
library(corrplot)

# Specify your path under "/Users/…/"" to the files
car_pref1 <- read.csv("csv/car_pref1.csv")
car_pref2 <- read.csv("csv/car_pref2.csv")

# STEPS FOR CLUSTER VALIDATION
#
# - Split data into two random samples: calibration (car_pref1) and validation (car_pref2).
# - Use clustering method to cluster calibration data. Determine the appropriate number of clusters and calculate centroids.
# - Using the cluster centroids from the calibration data, assign each observation from the validation sample to the closest centroid. We denote this cluster solution KS1.
# - Use the same clustering method from step 2 to cluster the validation data. Choose the solution with the same number of clusters as determined in step 2. We denote this duster solution KS2.
# - Cluster solutions KS1 and KS2 represent different assignments of the same set of observations to clusters. To assess the agreement between the two solutions, we cross-tabulate KS1 versus KS2.


# Standardize the variables of interest
car_pref1[, 2:11] <- scale(car_pref1[, 2:11])
car_pref2[, 2:11] <- scale(car_pref2[, 2:11])

psych::describe(car_pref1)
psych::describe(car_pref2)

# Step 2

cent <- matrix(c(-0.009, 0.929, 0.464, 0.466, 0.715, 2.174, 0.524, 0.099, 0.429, 1.023,
                 0.207, 0.715, -0.197, 0.566, -0.076, -0.310, -0.165, -0.109, 0.308, -0.297,
                 0.093, -0.804, -0.475, -0.362, -0.683, -0.046, -0.178, 0.498, 0.078, 0.085,
                 0.254, -0.344, 0.696, -0.573, 0.546, -0.373, 0.538, -0.408, -0.477, -0.287,
                 -1.885, 0.295, -0.548, 0.612, -0.057, -0.231, -1.246, -0.126, -0.296, 0.420),
               nrow = 5, ncol = 10, byrow = TRUE)
cent

set.seed(1)
car_kmeans1 <- kmeans(car_pref1[, 2:11], centers = cent, iter.max = 10)
car_kmeans1

str(car_kmeans1)

round(car_kmeans1$centers, 3)

#
# Step 3, validation?
#
# Next, we assign each observation from the validation sample (car_pref2) to the closest centroid (car_kmeans1$centers) that we obtained from the clustering on the calibration sample (car_pref1).
# Note that this can be accomplished by running the K-means procedure with the centroids as initial cluster centers and with one single iteration - this effectively just assigns each data point in the validation sample to the closest centroid.
set.seed(1)
car_kmeans2 <- kmeans(car_pref2[, 2:11], centers = car_kmeans1$centers, iter.max = 1)
car_kmeans2

# The cluster assignment of each observation in the validation sample is saved in car_kmeans2$cluster (called “Clustering vector” in the above output). We store this solution as KS1 into data car_pref2.vali.
car_pref2.vali <- cbind(car_pref2, KS1 = car_kmeans2$cluster)
car_pref2.vali[, c("StudentID", "KS1")]

# New centers from kmeans on car_pref2 using centroids from kmeans on car_pref1:
round(car_kmeans2$centers, 3)

#
# Step 4
#
car_kmeans3 <- kmeans(car_pref2[,2:11], centers = cent, iter.max = 10)
car_kmeans3

# The new cluster assignment of each observation in the validation sample is saved in car_kmeans3$cluster (called “Clustering vector” in the above output). We store this solution as KS2 into data car_pref2.vali
car_pref2.vali <- cbind(car_pref2.vali, KS2 = car_kmeans3$cluster)
car_pref2.vali[, c("StudentID", "KS1", "KS2")]

#
# Step 5
#
# Cluster solutions KS1 and KS2 represent different assignments of the same set of observations from the validation sample to clusters.
# To assess the agreement between the two solutions, we cross-tabulate KS1 versus KS2
# Most of the observations in the table fall into five cells, and there is relatively little scatter throughout the table. This indicates that there is a fairly strong agreement between the two solutions.
table(KS1 = car_pref2.vali$KS1, KS2 = car_pref2.vali$KS2)

# It is possible to assess the robustness of the cluster solution on the basis of the cross-tabulation matrix alone, as we have just done here.
# But, as often the case, it is easier to gain more insights if we visualize these values in a graphical form. To this end we can generate a heat map of the matrix using the following code
library(tidyverse)
vali_crossstab = as.data.table(table(KS1 = car_pref2.vali$KS1, KS2 = car_pref2.vali$KS2)) # store as a data.table for further use

# The heatmap makes it visually clear that the two solutions are in broad agreement, with most of the observations falling into just five table cells. The resulting clusters thus seem relatively robust, which concludes the validation exercise.
vali_crossstab %>%
  as.data.table() %>%   # store as a data.table
  mutate(KS1 = as.numeric(KS1), KS2 = as.numeric(KS2)) %>% # convert variables from character to numeric format
  rename(value = N) %>%
  ggplot(aes(y = factor(KS1), x = factor(KS2))) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Frequency", low = "lightgrey", high = "darkblue") +
  theme_minimal() +
  labs(title = "Validation of the cluster solution for car_pref2",
       x = "KS2",
       y = "KS1") +
  theme(legend.position="right",
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.ticks = element_blank())

# library(factoextra)
# fviz_cluster(car_kmeans, data = car_pref1) +
#   theme_bw()

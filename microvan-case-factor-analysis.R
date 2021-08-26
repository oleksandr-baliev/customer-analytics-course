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
microvanData <- read.delim(file= "csv/microvan_group_ass.csv", sep = ";")

psych::describe(microvanData)

hist(microvanData$mvliking,
     xlab = "Concept liking (mvliking)", # x axis label
     main = "Histogram",                 # plot title
     col = "grey")                       # color

# The histograms do not suggest any serious problems with the data. For example, there appear to be no obvious outliers, nor any “long tails”
# For most of the variables, the data are unimodal rather than multi-modal, although that for mvliking does show many replies in the lowest and highest categories; this in itself is not a data problem, just an indication of a great range of opinion in the concept-liking variable.
microvanData %>%
  select(-"subjnumb") %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram() +
  theme_minimal()

# Scatterplot matrix for the variables 2 to 11
# Uncomment to plot -> it  takes some time to finish
# pairs(microvanData[, 2:11], lower.panel = NULL)

#
# Regression analysis
#

# Regression diagnostic plots visually assess a goodness of fir for the regression model. Problems that can be identified with the help of the diagnostic plots include:
# - Heteroscedastic data (points at widely varying distances from the line)
# - Nonlinear relationships
# - Outliers
regressionModel <- lm(microvanData$mvliking ~
             microvanData$kidtrans +
             microvanData$miniboxy +
             microvanData$lthrbetr +
             microvanData$secbiggr +
             microvanData$safeimpt +
             microvanData$buyhghnd +
             microvanData$pricqual +
             microvanData$prmsound +
             microvanData$perfimpt +
             microvanData$tkvacatn +
             microvanData$noparkrm +
             microvanData$homlrgst +
             microvanData$envrminr +
             microvanData$needbetw +
             microvanData$suvcmpct +
             microvanData$next2str +
             microvanData$carefmny +
             microvanData$shdcarpl +
             microvanData$imprtapp +
             microvanData$lk4whldr +
             microvanData$kidsbulk +
             microvanData$wntguzlr +
             microvanData$nordtrps +
             microvanData$stylclth +
             microvanData$strngwrn +
             microvanData$passnimp +
             microvanData$twoincom +
             microvanData$nohummer +
             microvanData$aftrschl +
             microvanData$accesfun
            ,
             data = microvanData)
summary(regressionModel)

confint(regressionModel)

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
# https://data.library.virginia.edu/diagnostic-plots/
plot(regressionModel)
par(mfrow=c(1,1)) # Change back to 1 x 1

plot(regressionModel$residuals)

# Run step-wise regression
ols_step_both_p(regressionModel, pent = 0.05, prem = 0.05, details = TRUE)

bestRegressionModel <- lm(microvanData$mvliking ~
                        microvanData$noparkrm   +
                      microvanData$carefmny   +
                      microvanData$perfimpt   +
                      microvanData$suvcmpct   +
                      microvanData$shdcarpl   +
                      microvanData$twoincom   +
                      microvanData$lthrbetr   +
                      microvanData$homlrgst   +
                      microvanData$strngwrn,
                      data = microvanData)

summary(bestRegressionModel)
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
# https://data.library.virginia.edu/diagnostic-plots/
plot(bestRegressionModel)
par(mfrow=c(1,1)) # Change back to 1 x 1

plot(bestRegressionModel$residuals)

#
# Factor analysis part
#

microvanSurveyData <- microvanData[,3:32]

#Uncomment to for better factor analysis interpretations
names(microvanSurveyData)[names(microvanSurveyData) == 'kidtrans'] <- "We need a car that helps transport our kids and their friends."
names(microvanSurveyData)[names(microvanSurveyData) == 'miniboxy'] <- "Current minivans are simply too boxy and large."
names(microvanSurveyData)[names(microvanSurveyData) == 'lthrbetr'] <- "Leather seats are dramatically better than cloth."
names(microvanSurveyData)[names(microvanSurveyData) == 'secbiggr'] <- "If we got a second car, it would need to be bigger than a standard sedan."
names(microvanSurveyData)[names(microvanSurveyData) == 'safeimpt'] <- "Auto safety is very important to me."
names(microvanSurveyData)[names(microvanSurveyData) == 'buyhghnd'] <- "We tend to buy higher‐end cars."
names(microvanSurveyData)[names(microvanSurveyData) == 'pricqual'] <- "Car prices strongly reflect underlying production quality."
names(microvanSurveyData)[names(microvanSurveyData) == 'prmsound'] <- "A premium sound and entertainment system helps on long car trips."
names(microvanSurveyData)[names(microvanSurveyData) == 'perfimpt'] <- "Performance is very important in a car."
names(microvanSurveyData)[names(microvanSurveyData) == 'tkvacatn'] <- "We try to take as many vacations as possible."
names(microvanSurveyData)[names(microvanSurveyData) == 'noparkrm'] <- "Our current residence doesn't have a lot of parking room."
names(microvanSurveyData)[names(microvanSurveyData) == 'homlrgst'] <- "Our home is among the largest in the neighborhood."
names(microvanSurveyData)[names(microvanSurveyData) == 'envrminr'] <- "The environmental impact of automobiles is relatively minor."
names(microvanSurveyData)[names(microvanSurveyData) == 'needbetw'] <- "There needs to be something between a sedan and a minivan."
names(microvanSurveyData)[names(microvanSurveyData) == 'suvcmpct'] <- "I like SUVs more than minivans since they're more compact."
names(microvanSurveyData)[names(microvanSurveyData) == 'next2str'] <- "My next car will be a two‐seater."
names(microvanSurveyData)[names(microvanSurveyData) == 'carefmny'] <- "We are careful with money."
names(microvanSurveyData)[names(microvanSurveyData) == 'shdcarpl'] <- "I think everyone should carpool or take public transportation."
names(microvanSurveyData)[names(microvanSurveyData) == 'imprtapp'] <- "Most of our appliances are imported."
names(microvanSurveyData)[names(microvanSurveyData) == 'lk4whldr'] <- "Four‐wheel drive is a very attractive option."
names(microvanSurveyData)[names(microvanSurveyData) == 'kidsbulk'] <- "Our kids tend to take a lot of bulky items and toys with them."
names(microvanSurveyData)[names(microvanSurveyData) == 'wntguzlr'] <- "I will buy what I want even if it is a “gas guzzler”."
names(microvanSurveyData)[names(microvanSurveyData) == 'nordtrps'] <- "We don’t go on road trips with the family."
names(microvanSurveyData)[names(microvanSurveyData) == 'stylclth'] <- "We tend to purchase stylish clothes for the family."
names(microvanSurveyData)[names(microvanSurveyData) == 'strngwrn'] <- "Warranty protection needs to be strong on a new car."
names(microvanSurveyData)[names(microvanSurveyData) == 'passnimp'] <- "Passion for one’s job is more important than pay."
names(microvanSurveyData)[names(microvanSurveyData) == 'twoincom'] <- "Our family would find it hard to subsist on just one income."
names(microvanSurveyData)[names(microvanSurveyData) == 'nohummer'] <- "I am not interested in owning a vehicle like a Hummer."
names(microvanSurveyData)[names(microvanSurveyData) == 'aftrschl'] <- "We engage in more after‐school activities than most families."
names(microvanSurveyData)[names(microvanSurveyData) == 'accesfun'] <- "Accessories really make a car more fun to drive."

psych::describe(microvanSurveyData)
summary(microvanSurveyData)

variables <- scale(microvanSurveyData)
corellations <- cor(variables)

# print pretty
upper<-round(corellations, 3) # we round the results to the 3d digit after comma
upper[upper.tri(cor)]<-""
upper<-as.data.frame(upper)
upper

# As the correlation matrix is rather large (13 dimensions), additional insights can be obtained by visualizing its heatmap (a correlogram)
corrplot(corellations,
         method = "number",
         type = "upper",
         order = "hclust", # reorder by the size of the correlation coefficients
         tl.cex = 1, # font size of the variable labels
         tl.col = "black", # color of the variable labels
         tl.srt = 45, # rotation angle for the variable labels
         number.cex = 0.8 # font size of the coefficients
)

eigenvalues <- eigen(corellations)$values

# individual percentage of variance explained by a dimension
eigenvalues / length(eigenvalues)
# The cumulative percentages of variance explained are:
cumsum(eigenvalues/length(eigenvalues))
scree(corellations, pc = TRUE, factors = FALSE)

numberOfFactors <- 3

plot(cumsum(eigenvalues/length(eigenvalues)),
    type = "o", # type of plot: "o" for points and lines 'overplotted'
    col = "darkblue",
    pch = 16, # plot symbol: 16 = filled circle
    cex = 1, # size of plot symbols
    xlab = "Number of factors", # a title for the x axis
    ylab = "Cumulative variance explained", # a title for the y axis
    lwd = 2) # line width
abline(v = numberOfFactors, lwd = 2, col = "grey") # draw a vertical line at v = 3

# factorAnalysis <- principal(r = corellations,
#                  nfactors = numberOfFactors,
#                  rotate="varimax",
#                  scores = TRUE)
factorAnalysis <- fa(r = corellations,
   nfactors = numberOfFactors,
   fm = "pa",
   rotate = "varimax")

print(factorAnalysis,
      digits = 3, # to round numbers to the third digit
      cut = 0.4, # to show only values > 0.35
      sort = TRUE # to sort rows by loading size
)

sort(factorAnalysis$communality)

loadings <- unclass(factorAnalysis$loadings)
round(loadings, 3)

factorScores <- factor.scores(variables, unclass(factorAnalysis$loadings))$scores
microvanFactoredData <- data.frame(microvanData[,1:2], factorScores, microvanData[,33:39])
microvanFactoredData[["subjnumb"]] <- NULL

# Plot graphs all factor to all demographics
# microvanInterimData <- data.frame(mvliking = microvanData[,2], factorScores, microvanData[,33:39])
# pairs(microvanInterimData, lower.panel = NULL)

# plot(x = microvanFactoredData$PA4,
#      y = microvanFactoredData$PA5,
#      # col = bimodal$cluster,  # specify how to color the observations
#      xlab = "PA1",
#      ylab = "PA2",
#      xlim = c(-2.2, 2.2),
#      ylim = c(-2.2, 2.2),
#      pch = 16, # specify the plot symbol: 16 = filled circle
#      cex = 1)  # specify the symbol size


# Run the clustering
#
# Hierarchical cluster analysis
#


clusterAnalysisData <- data.frame(factorScores[1:200,])
clusterAnalysisValidationData <- data.frame(factorScores[201:400,])

# Uncomment if you don't need validation
clusterAnalysisData <- data.frame(factorScores)

# Uncomment if you want to try to run the cluster analysis with all 30 variables
# clusterAnalysisData <- microvanSurveyData

# 2. Run a cluster a  nalysis on a distance matrix and using the Ward method
microvan_ward <- hclust(dist(clusterAnalysisData), method="ward.D2")

#- Do you recall from the earlier practice with hierarchial clustering how to generate the resulting agglomeration schedule?

# Scree plot
plot(rev(microvan_ward$height), # rev is used to plot from low to high values on Y axis
     type = "b",           # to display both the points and lines
     ylab = "Dissimilarity measure",
     xlab = "Number of clusters",
     main = "Scree plot",
     col = "darkblue",
     pch = 16)             # specify the plot symbol: 16 = filled circle

kClusters <- 3
abline(v = kClusters, lty = 2, col = "darkred") # draw a vertical line at v = 5

# Dendrogram
library(dendextend)
plot(set(as.dendrogram(microvan_ward),
         "branches_k_color", # to highlight the cluster solution with a color
         k = kClusters),
     ylab = "Distance",
     main = "Dendrogram",
     cex = 0.2)             # Size of labels
rect.hclust(microvan_ward, k = kClusters, border = "darkblue")  # draw red borders around 2 clusters

membersInCluster <- cutree(microvan_ward, k = kClusters)
table(membersInCluster)
#
# K - means factor
#

# 1. Defining the initial cluster centroids
centroids <- NULL
for (k in 1:kClusters) {
  centroids <- rbind(centroids, colMeans(clusterAnalysisData[membersInCluster == k, , drop = FALSE]))
}

round(centroids, 3)

set.seed(1)
kMeansCluster <- kmeans(clusterAnalysisData, centers = centroids, iter.max = 10)
# The printed output displays:
# - the cluster means or centers: A matrix whose rows are a cluster number (1 to K) and whose columns are variables
# - the clustering vector: A vector of indices (from 1:K) indicating the cluster to which each observation in the data set is allocated
kMeansCluster

# The kmeans() function returns a list of components, the most informative of which are the following:
#
# cluster: A vector of indices (from 1:K) indicating the cluster to which each point is allocated
# centers: A matrix of cluster centers (cluster means)
# size: The number of observations in each cluster
#- totss: The total sum of squares (TSS) that measures the total variance in the data.
#- withinss: Vector of within-cluster sum of squares, one component per cluster
#- tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss)
#- betweenss: The between-cluster sum of squares, i.e. totss???tot.withinss
str(kMeansCluster)

# the change in each cluster from the K-means clustering
changePerCluster <- NULL
for (i in 1:kClusters){
  changePerCluster <- rbind(changePerCluster, kMeansCluster$centers[i,]-centroids[i,])
}

round(changePerCluster, 3)

# that can help to see how far cluster are from each others.
dist(kMeansCluster$centers)

# Add cluster index as separate column
# microvanFactoredData <- cbind(microvanFactoredData, cluster = kMeansCluster$cluster)
# microvanFactoredData[["cluster"]] <- NULL
clusterAnalysisData <- cbind(clusterAnalysisData, cluster = kMeansCluster$cluster)

# Overall average in the sample
round(colMeans(clusterAnalysisData), 3)

# TODO: uncomment and try again, the issue with small vector is- > "operator is invalid for atomic vectors"
# Average for Cluster 1
# round(colMeans(clusterAnalysisData[clusterAnalysisData == 1, ]), 3)
# Average for each cluster with one step
aggregate(clusterAnalysisData,
          by = list(cluster =clusterAnalysisData$cluster),
          FUN = mean)

dt.cluster <- aggregate(clusterAnalysisData,
                        by = list(cluster =clusterAnalysisData$cluster),
                        FUN = mean)
# TODO: find better solution to fix extra column
dt.cluster <- dt.cluster[, -1]

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
  labs(title = "Average microvan preferences in each cluster",
       x = "Cluster",
       y = " ") +
  theme(legend.position="right",
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.ticks = element_blank())

library(factoextra)
fviz_cluster(kMeansCluster, data = clusterAnalysisData)
  + theme_bw()
#
#
# Validation..
#
#

set.seed(1)
kMeansClusterValidation <- kmeans(clusterAnalysisValidationData, centers = kMeansCluster$centers, iter.max = 1)
kMeansClusterValidation

validation <- cbind(clusterAnalysisValidationData, KS1 = kMeansClusterValidation$cluster)
round(kMeansClusterValidation$centers, 3)

kMeansClusterValidationFull <- kmeans(clusterAnalysisValidationData, centers = centroids, iter.max = 10)
kMeansClusterValidationFull

validation <- cbind(validation, KS2 = kMeansClusterValidationFull$cluster)

table(KS1 = validation$KS1, KS2 = validation$KS2)
vali_crossstab = as.data.table(table(KS1 = validation$KS1, KS2 = validation$KS2))

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
  labs(title = "Validation of the cluster solution for microvan validation data",
       x = "KS2",
       y = "KS1") +
  theme(legend.position="right",
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.ticks = element_blank())

#
# reality check with social data
#

colnames(microvanData[,33:39])
microvanFinalData <- data.frame(mvliking = microvanData[,2], factorScores, microvanData[,33:39], cluster = kMeansCluster$cluster)

# Rename for easier interpretation
microvanFinalData <- microvanFinalData %>%
  rename(
    affluent = PA1,
    compact = PA2,
    family = PA3

  )
# colnames(microvanFinalData)

cluster1 <- subset(microvanFinalData, microvanFinalData[,"cluster"] == 1)
cluster2 <- subset(microvanFinalData, microvanFinalData[,"cluster"] == 2)
cluster3 <- subset(microvanFinalData, microvanFinalData[,"cluster"] == 3)

cluster1[["cluster"]] <- NULL
cluster2[["cluster"]] <- NULL
cluster3[["cluster"]] <- NULL

# Build graphs per cluster
pairs(cluster1, lower.panel = NULL)
pairs(cluster2, lower.panel = NULL)
pairs(cluster3, lower.panel = NULL)

# Build all to all graphs
# pairs(microvanFinalData, lower.panel = NULL)

# Regression part

# Regression diagnostic plots visually assess a goodness of fir for the regression model. Problems that can be identified with the help of the diagnostic plots include:
# - Heteroscedastic data (points at widely varying distances from the line)
# - Nonlinear relationships
# - Outliers
regressionModel <- lm(microvanFinalData$mvliking ~
                        microvanFinalData$affluent +
                          microvanFinalData$small_size_vehicles +
                          microvanFinalData$family_with_kids +
                          microvanFinalData$safety_and_security +
                          microvanFinalData$env_friendly +
                          microvanFinalData$income +
                          microvanFinalData$age +
                          microvanFinalData$miles +
                          microvanFinalData$numkids +
                          microvanFinalData$female +
                          microvanFinalData$educ +
                          microvanFinalData$recycle
  ,
                      data = microvanFinalData)
summary(regressionModel)

confint(regressionModel)

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
# https://data.library.virginia.edu/diagnostic-plots/
plot(regressionModel)
par(mfrow=c(1,1)) # Change back to 1 x 1

plot(regressionModel$residuals)

# Run step-wise regression
ols_step_both_p(regressionModel, pent = 0.05, prem = 0.05, details = TRUE)

# TODO: Adjust with result from OLS
bestRegressionModel <- lm(microvanFinalData$mvliking ~
                          microvanFinalData$small_size_vehicles +
                          microvanFinalData$env_friendly +
                          microvanFinalData$age +
                          microvanFinalData$female +
                          microvanFinalData$safety_and_security +
                          microvanFinalData$educ,
                      data = microvanFinalData)

summary(bestRegressionModel)
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
# https://data.library.virginia.edu/diagnostic-plots/
plot(bestRegressionModel)
par(mfrow=c(1,1)) # Change back to 1 x 1


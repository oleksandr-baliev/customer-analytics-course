# Title     : TODO
# Objective : TODO
# Created by: oleksandr.baliev
# Created on: 01.08.21

# First, we need to install these packages
library(psych)
library(tidyverse)
library(data.table)
library(olsrr)
library(corrplot)

# The data available for analysis consists of 50 observations, one for each of the states of the United States, on each of 13 variables representing different economic activities, as follows:
#
# Agric: agriculture, forestry, and fishing;
# Mining: mining;
# Constr: construction;
# Durmfr: manufacturing (durable goods);
# Nondmfr: manufacturing (nondurable goods);
# Transp: transportation;
# Commun: communications;
# Utils: electricity, gas, and sanitation;
# Whole: wholesale trade;
# Retail: retail trade;
# Fire: fiduciary, insurance, and real estate;
# Service: services; and
# Govt: government.

# Each observation represents the share (expressed as a percentage) of that particular economic activity in the gross state product (GSP) of that state. The goal of the analysis is to investigate whether the information captured in the data can be reduced to fewer than 13 dimensions, and to interpret these dimensions in order to allow easier understanding of the dynamics of economic activities.

# Load data to the memory
gsp_share <- read.csv("csv/gsp_share.csv")

# The structure of the data and summary statistics for all variables can be produced with the R commands str and psych::describe(gsp_share), respectively. Note that state is a character variable with codes from 1 to 50.
str(gsp_share)
psych::describe(gsp_share)

# save the variables of interest for further analysis into a separate vector, while omitting the first column of the dataset (acronyms of the U.S. state names) and standardizing the values with the command scale()
vars <- scale(gsp_share[,-1])

# finding the correlation among our variables of interest
cor <- cor(vars)
# correlation of vars  is the same as for cor(gsp_share[,-1]) without scale() function applied
cor

# print pretty
upper<-round(cor,3) # we round the results to the 3d digit after comma
upper[upper.tri(cor)]<-""
upper<-as.data.frame(upper)
upper

# As the correlation matrix is rather large (13 dimensions), additional insights can be obtained by visualizing its heatmap (a correlogram)
corrplot(cor,
         method = "number",
         type = "upper",
         order = "hclust", # reorder by the size of the correlation coefficients
         tl.cex = 1, # font size of the variable labels
         tl.col = "black", # color of the variable labels
         tl.srt = 45, # rotation angle for the variable labels
         number.cex = 0.8 # font size of the coefficients
)

# calculate eigenvalues
EV <- eigen(cor)$values
EV

# individual percentage of variance explained by a dimension
EV / length(EV)

# The cumulative percentages of variance explained are:
cumsum(EV/length(EV))

# The first criterion retains only those eigenvalues that are greater than one (this is also called Kaiser’s rule), and in this case it suggests retaining the first five dimensions
# first three dimensions together account for 57% of variance in the initial variables.
# In this case, for the sake of parsimony, we decide to proceed with retaining the first three dimensions.
scree(cor, pc = TRUE, factors = FALSE)

# Shares for the cumulative variance explained
# It is apparent from the plot that no single dominant dimension captures the lion’s share of variability in the original data. The first three eigenvalues account for 57 percent of the total variance;
# the first five eigenvalues (all greater than 1) account for 77 percent.
plot(cumsum(EV/length(EV)),
     type = "o", # type of plot: "o" for points and lines 'overplotted'
     col = "darkblue",
     pch = 16, # plot symbol: 16 = filled circle
     cex = 1, # size of plot symbols
     xlab = "Number of factors", # a title for the x axis
     ylab = "Cumulative variance explained", # a title for the y axis
     lwd = 2) # line width
abline(v = 3, lwd = 2, col = "grey") # draw a vertical line at v = 3

#
#
# principal component analysis (PCA)
#
#

# r - is the correlation matrix of the original variables.
# nfactors - specifies the number of components to extract (1 by default).
# rotate - indicates the rotation method to be applied (“none”, “varimax”, “quartimax”, “promax”, “oblimin”, “simplimax”, and “cluster”).
# scores - specifies whether or not to calculate factor scores (FALSE by default).
PCA <- principal(r = cor,
                 nfactors = 4,
                 rotate="varimax",
                 scores = TRUE)

print(PCA,
      digits = 3, # to round numbers to the third digit
      cut = 0.35, # to show only values > 0.35
      sort = TRUE # to sort rows by loading size
)
# PC1-PC3 - are the loadings for the three extracted principal components (dimensions). They are the correlation coefficients among the original variables and the first three principal components labelled PC1, PC2, PC3.
# h2 - shows the communalities that are computed as the sum of squared component loadings for each original variable over all the extracted components, i.e. h2k=∑3q=1l2kq with k=1,...,13 the index of the original variable and q=1,2,3 the index of the extracted component.
# u2 (“uniqueness”) - is the variance of the error term, i.e. the proportion of variation that is not explained by the three extracted components (u2k=1−h2k).
# com - is the complexity index of the component loadings for a variable (not relevant for us here).
#
# SS loadings - is a sum of squared loadings per component over all initial variables and corresponds to the eigenvalue for each extracted component
# Proportion Var - shows the proportion of variation in the data that is explained by the extracted component. For example, the first component (PC1) explains 24.9% of variation in the data.
# Cumulative Var - is the cumulative proportion of variation in the data that is explained by all extracted components. Three components together explain 57.2% of variation in the data.
#

# The proportion of variance in each of the original variables accounted for by the first three principal components are the communalities
PCA$communality

# Again, the first three principal components capture more information in some areas of economic activity (e.g., fiduciary and real estate at 81.9 percent) than in others (e.g., utilities at 24.9 percent).
# The communalities (h2) are all roughly between 0.25 and 0.85, which means that between 25% and 85% of the variation in each of the original input variables has been retained by the first three principal components.
# Whether this is sufficient or whether more dimensions are needed, is again a question for the researcher.
# Another way to assess it is noticing that in 10 out of 13 variables, the amount of variance explained is nearly 50 percent or more (e,g., more than 80 percent of the variance in fiduciary, insurance, and real estate is explained by the first three components).
# The table also shows that less than 40 percent of the original variance is captured in three of the 13 variables: agriculture, forestry, and fishing; communication; and gas, electricity, and sanitation. If these variables are deemed to be reliable and important for subsequent analyses, then it might make sense to retain a fourth principal component and see if the variance accounted for in these variables increases.

# The matrix of loadings:
L <- as.data.table(unclass(PCA$loadings), keep.rownames = T)
L

# how the different measures of economic activity relate to each other, it is also helpful to plot the principal component loadings
plot(x = L$PC1, y = L$PC2,
     col ="darkblue",
     pch = 16,        # plot symbol: 16 = filled circle
     cex = 1,         # size of plot symbols
     xlab = "PC1",    # a title for the x axis
     ylab = "PC2",    # a title for the y axis
     xlim = c(-1,1),  # x axis values from -1 to 1
     ylim = c(-1,1))  # y axis values from -1 to 1
# add point labels
text(L$PC1, L$PC2,
     labels = L$rn,
     pos = 3,
     cex = 0.8,
     col = "darkred")
# add vertical and horizontal lines
abline(h = 0, lwd = 2, col = "grey") # draw a horizontal line at h = 0
abline(v = 0, lwd = 2, col = "grey") # draw a vertical line at v = 0

#
#
# Perceptual map
#
#

# the scores are the projections of the original data units into the new (3-dimensional) component space, instead of the original 13-dimensional variable space.
# extract un-rotated scores of principal components
PCA.scores <- factor.scores(vars, unclass(PCA$loadings))$scores
PCA.scores

# The component (factor) scores are standardized values with an average of 0 and a standard deviation of 1. We can add the component scores to the original data set in three new columns, so that for each observation (each US state) in the data we also have recorded its scores on the new three dimensional space:
gsp_share.scores <- cbind(gsp_share, PCA.scores)
gsp_share.scores

# Plotting the principal component scores on a (perceptual) map can give us an idea of the location of the various states in the principal component space.
# The plot axes are the factor score values for a pair of the extracted components.
# Since we can look only at two components at once, in order to get a full understanding, when we extract more than two factors we need to consider plotting all pairs of factors (in our case this leads to three plots).
# Below we have a perceptual map for PC1 and PC2; the other two maps can be generated and interpreted similarly.
# PC1 + PC2 map
# The first thing apparent from the plot is the existence of two rather substantial outliers. Both Alaska and Wyoming have values of 3.6 on the first dimension (principal component); the next nearest state has a value less than 1.5. It turns out that these high values can be traced to extreme values on two dimensions of economic activity: mining and transportation. In Wyoming, 31.6 percent of GSP comes from mining. In Alaska, it is 22.4 percent. This is compared to a median level across all 50 states of less than 1 percent. Similarly, for transportation, the percentages for Alaska and Wyoming are 12.1 and 6.4, respectively (compared to a median of 3 percent). These are some of the most substantial outliers in the entire data set. Because the objective of principal components is to account for as much variance (information) as possible, it is not surprising that the first principal component reflects the underlying dimensions where these outlying observations occur.
plot(x = gsp_share.scores$PC1,
     y = gsp_share.scores$PC2,
     xlab = "PC1", ylab = "PC2",
     xlim = c(-3, 7), ylim = c(-3, 3),
     pch = 16, cex = 1, col = "blue")

abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
# add point labels
text(x = gsp_share.scores$PC1,
     y = gsp_share.scores$PC2,
     labels = gsp_share.scores$state,
     cex = 1,
     adj = 1.2,
     col = "black")

# PC1 + PC3 map
plot(x = gsp_share.scores$PC1,
     y = gsp_share.scores$PC3,
     xlab = "PC1", ylab = "PC3",
     xlim = c(-3, 7), ylim = c(-3, 3),
     pch = 16, cex = 1, col = "blue")

abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
# add point labels
text(x = gsp_share.scores$PC1,
     y = gsp_share.scores$PC3,
     labels = gsp_share.scores$state,
     cex = 1,
     adj = 1.2,
     col = "black")

# PC2 + PC3 map
plot(x = gsp_share.scores$PC2,
     y = gsp_share.scores$PC3,
     xlab = "PC2", ylab = "PC3",
     xlim = c(-3, 7), ylim = c(-3, 3),
     pch = 16, cex = 1, col = "blue")

abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
# add point labels
text(x = gsp_share.scores$PC2,
     y = gsp_share.scores$PC3,
     labels = gsp_share.scores$state,
     cex = 1,
     adj = 1.2,
     col = "black")

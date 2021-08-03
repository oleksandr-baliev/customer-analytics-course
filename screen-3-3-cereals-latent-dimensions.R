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

# Factor analysis can be used to reduce the dimensionality of the attribute data and uncover a smaller number of underlying latent dimensions of customer preferences (factors that account for a major amount of the variance in the original measures).

rte_cereal <- read.csv("csv/rte_cereal.csv")
rte_cereal

psych::describe(rte_cereal)

# The 12 cereal brands are listed below:
#
# - All Bran
# - Cerola Muesli
# - Just Right
# - Kellogg’s Corn Flakes
# - Komplete
# - NutriGrain
# - Purina Muesli
# - Rice Bubbles
# - Special K
# - Sustain
# - Vitabrit
# - Weetbix

# keep only data, without Respondent and Brand
vars <- scale(rte_cereal[,3:27])
vars

cor <- cor(vars)

upper<-round(cor,3) # we round the results to the 3d digit after comma
upper[upper.tri(cor)]<-""
upper<-as.data.frame(upper)
upper

# heatmap
corrplot(cor,
         method = "number",
         type = "upper",
         order = "hclust", # reorder by the size of the correlation coefficients
         tl.cex = 0.8, # font size of the variable labels
         tl.col = "black", # color of the variable labels
         tl.srt = 45, # rotation angle for the variable labels
         number.cex = 0.5 # font size of the coefficients
)

#
# Eigenvalues and scree plot
#
scree(cor, pc = TRUE, factors = FALSE)

# The scree plot suggests that we may be justified in extracting four factors; note that there appears to be an “elbow” in the amount of variance accounted for after the fourth eigenvalue.
# Using a proportionality criterion (i.e., each common factor should account for at least as much variation as one of the original variables in the analysis, which is directly analogous to the rationale underlying the rule for retaining factors with eigenvalues larger than one), the inclusion of the fifth factor is marginal at best.

# Individual percentages of variance explained by each factor
# length() - finds the total number of elements in a vector
EV <- eigen(cor)$values
EV/length(EV)

cumsum(EV/length(EV))

#Recall that we decided to retain four factors. The first four factors together account for 58% of variance in the initial variables. Whether it suffices, is a question that the analyst should consider during the interpretation
# Shares for the cumulative variance explained
plot(cumsum(EV/length(EV)),
     type = "o", # type of plot: "o" for points and lines 'overplotted'
     col = "darkblue",
     pch = 16, # plot symbol: 16 = filled circle
     cex = 1, # size of plot symbols
     xlab = "Number of factors", # a title for the x axis
     ylab = "Cumulative variance explained", # a title for the y axis
     lwd = 2) # line width
abline(v = 4, lwd = 2, col = "grey") # draw a vertical line at v = 4
abline(h = 0.6, lwd = 2, col = "grey") # draw a vertical line at h = 60%

#
#
# Execute the exploratory factor analysis (EFA)
#
#

# We will first analyze the unrotated solution, then use factor rotations to see if interpretability improves

#
# Unrotated factor solution
#

# computes the loadings, communalities, specificities, and several other measures, retains four factors and saves the factor scores.
# r - is the correlation matrix of the original variables.
# nfactors - specifies the number of factors to extract (1 by default).
# fm - specifies the factor estimation method (we use pa here for common factor analysis).
# rotate - indicates the rotation method to be applied (“none”, “varimax”, “quartimax”, “promax”, “oblimin”, “simplimax”, and “cluster”; the default is “oblimin”).
EFA1 <- fa(r = cor,
           nfactors = 4,
           fm = "pa",
           rotate = "none")

# output analysis:
# PA1-PA4 - are the loadings for the four extracted factors (dimensions).
# h2 - shows the communalities that are computed as a sum of squared factor loadings for each original variable over all the extracted factors, i.e. h2k=∑4q=1l2kq with k=1,...,25 the index of original variable and q=1,...,4 the index of the extracted factor.
# u2 (“uniqueness”) - is the variance of the error term, i.e. the proportion of variation that is not explained by the four extracted factors (u2k=1−h2k).
# com - is the complexity index of the component loadings for a variable (not relevant for us here).
print(EFA1,
      digits = 3, # to round numbers to the third digit
      cut = 0.35, # to show only values > 0.35
      sort = TRUE # to sort rows by loading size
)
# To understand how well the extracted factors explain the variation in the data
# we look at  "SS loadings", "Proportion Var" and "Cumulative Var"
# SS loadings - is a sum of squared loadings per factor over all initial variables and corresponds to the eigenvalue for each extracted factor
# Proportion Var - shows the proportion of variation in the data that is explained by the extracted factor. For example, the first factor (PA1) explains 24.3% of variation in the data.
# Cumulative Var -  is the cumulative proportion of variation in the data that is explained by all extracted factors. The first four factors together explain 50.5% of data variation.

# The proportion of variance in each of the original variables accounted for by the first four factors are the communalities and can be displayed in increasing order with the following code
# (note that this represents the information in the h2 column of the previous output)
sort(EFA1$communality)

# Explanataion:
#
# he attribute “Easy” has a communality value of 0.17. This implies that 83 percent (= 1.00 - 0.17) of the variation in this particular measure is unique to the attribute and not explained by the four factors.
# This is potentially a concern: For this particular variable, we are capturing almost none of its information in the common factor solution.
#
# There are several potential reasons for this.
# - One might be that we have chosen to extract too few factors, and as a result we have limited the amount of information we can account for in this particular measure.
#   In general, it is not a bad idea to try extracting an additional factor to see if it changes the results (i.e., does the communality of the attribute increase?).
#   If so, the ability to account for additional variation in some important measure may justify the complexity of a solution with a higher number of factors.
# - A low communality may however also be taken as an indication of a poor measure (i.e., one in which there is considerable noise associated with measurement error).
#   In this particular case, it seems that “Easy” is an attribute that applies equally well to all items in the product category (after all, any ready-to-eat cereal should be easy to prepare).
#   Within the category, it may be very difficult for consumers to distinguish between cereal brands on the basis of ease of use.
#   If this is the case, then we would expect most of the variance on this attribute to be nonsystematic;
#   as a result, the factor solution would have a relatively high specific variance component.
#
# Generally, some form of pretesting to establish if the measure is appropriate in a given survey context will help avoid this problem.

# Factor loadings
L <- unclass(EFA1$loadings)
round(L, 3)

#
# Rotated factor solution
#

EFA2 <- fa(r = cor,
           nfactors = 4,
           fm = "pa",
           rotate = "varimax")

print(EFA2,
      digits = 3, # to round numbers to the third digit
      cut = 0.35, # to show only values > 0.35
      sort = TRUE # to sort rows by loading size
)

#
# Perceptual map
#
# Frequently, factor analysis is not an end in itself but an intermediate step on the way to further analysis of the data
# For example, Roberts and Lattin (1991) used factor analysis to identify a smaller number of underlying dimensions that they might use in building a model of consideration for ready-to-eat cereals
# For this type of subsequent analysis, we need to know the location of each original observation in the reduced factor space.

# The factor score coefficients for the four-factor varimax-rotated solution for the cereal data
EFA2$weights

# We use these coefficients to calculate factor scores for each of the 235 observations in the data set.
# Recall that these scores are the projections of the original data units into the new (four-dimensional) factor space, instead of the original 25-dimensional variable space.
# Then we shall calculate the average factor scores for each of the 12 rated brands.

# extract rotated factor scores
EFA2.scores <- factor.scores(vars, unclass(EFA2$loadings))$scores
head(EFA2.scores) # to show the first 6 observations

# The factor scores are standardized values with mean equal to zero and standard deviation equal to 1
# We can add the factor scores to the original data set in four new columns, so that for each observation (each respondent rating a brand) in the data we also have recorded its scores on the new four dimensional space.
rte_cereal.scores <- cbind(rte_cereal, EFA2.scores)
head(rte_cereal.scores)

# Average values for factor scores for each brand
mean.scores <- aggregate(rte_cereal.scores[, c("PA1", "PA2", "PA3", "PA4")],
                         by = list(Brand = rte_cereal.scores$Brand),
                         FUN = mean)
mean.scores

# Plotting the average factor scores on a (perceptual) map can give us an idea of the location of the various brands in the four-factor space.
# The plot axes are the average factor scores for a pair of the extracted components, and points are labels with brand names.
# Since we can look only at two factors at once in a bivariate plot, in order to get a full understanding, when we extract more than two factors we need to consider plotting all pairs (in our case this leads to six plots since we have four factors).
# Below we show two pairwise plots, or perceptual maps (Factor 1 versus Factor 2, and Factor 3 versus Factor 4)
# ----------------------
# Factor 1 vs. Factor 2
# ----------------------
plot(x = mean.scores$PA1,
     y = mean.scores$PA2,
     xlab = "Factor 1: Healthful", ylab = "Factor 2: Artificial",
     xlim = c(-2, 2), ylim = c(-2, 2),
     pch = 16, cex = 1, col = "blue")

abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
# add point labels
text(x = mean.scores$PA1,
     y = mean.scores$PA2,
     labels = mean.scores$Brand,
     cex = 1,
     adj = 1.2,
     col = "black")

# ----------------------
# Factor 3 vs. Factor 4
# ----------------------
plot(x = mean.scores$PA3,
     y = mean.scores$PA4,
     xlab = "Factor 3: Non-Adult", ylab = "Factor 4: Interesting",
     xlim = c(-2, 2), ylim = c(-2, 2),
     pch = 16, cex = 1, col = "blue")

abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
# add point labels
text(x = mean.scores$PA3,
     y = mean.scores$PA4,
     labels = mean.scores$Brand,
     cex = 1,
     adj = 1.2,
     col = "black")

# Recall that because the factor scores are the standardized values, we interpret them by reference to average patterns rather than in absolute terms.
# The average factor scores on the perceptual maps reveal the relative positions of these cereals.
# For example, “Weetbix” and “Vitabrits” (each in the form of a shredded-wheat type biscuit) are both perceived similarly: healthful, nonartificial, and uninteresting.
# The three different brands “Cerola”, “Komplete”, and “Purina” which cluster together in the factor space, turn out to be all muesli-type cereals;
# thus it is not surprising that their relative positions are similar.

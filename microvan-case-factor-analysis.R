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

# TODO: shall we still keep 'mvliking'?
microvanSurveyData <- microvanData[,3:32]

vars <- scale(microvanSurveyData)
cor <- cor(vars)

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

EV <- eigen(cor)$values

# individual percentage of variance explained by a dimension
EV / length(EV)
# The cumulative percentages of variance explained are:
cumsum(EV/length(EV))
scree(cor, pc = TRUE, factors = FALSE)

numberOfFactors = 5

plot(cumsum(EV/length(EV)),
    type = "o", # type of plot: "o" for points and lines 'overplotted'
    col = "darkblue",
    pch = 16, # plot symbol: 16 = filled circle
    cex = 1, # size of plot symbols
    xlab = "Number of factors", # a title for the x axis
    ylab = "Cumulative variance explained", # a title for the y axis
    lwd = 2) # line width
abline(v = numberOfFactors, lwd = 2, col = "grey") # draw a vertical line at v = 3

EFA1 <- fa(r = cor,
           nfactors = numberOfFactors,
           fm = "pa",
           rotate = "varimax")

print(EFA1,
      digits = 3, # to round numbers to the third digit
      cut = 0.5, # to show only values > 0.?
      sort = TRUE # to sort rows by loading size
)

sort(EFA1$communality)

L <- unclass(EFA1$loadings)
round(L, 3)

# PCA <- principal(r = cor,
#                  nfactors = numberOfFactors,
#                  rotate="varimax",
#                  scores = TRUE)
#
# print(PCA,
#       digits = 3, # to round numbers to the third digit
#       cut = 0.35, # to show only values > 0.35
#       sort = TRUE # to sort rows by loading size
# )
# PCA$communality
#
# L <- as.data.table(unclass(PCA$loadings), keep.rownames = T)

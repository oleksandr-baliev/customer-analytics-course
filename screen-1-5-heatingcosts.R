# Title     : TODO
# Objective : TODO
# Created by: oleksandr.baliev
# Created on: 17.07.21

# First, we need to install these packages
library(psych)
library(tidyverse)
library(data.table)
library(olsrr)

# Load data to the memory
HCP <- read.csv("csv/HeatingCost.csv")

str(HCP)

HCP <- as.data.table(lapply(HCP, as.numeric))

# obtain summary statistics
psych::describe(HCP)
summary(HCP)

# display average values in the whole sample for the specified variables:
colMeans(HCP[ , c("Heating.Cost", "Minimum.Temperature")])

#ompute specific statistics of selected variables for each value of a given grouping variable.
# For example, with the following code we find average values of “Heating.Cost” and “Minimum.Temperature”
# for groups of houses with each given value of the “Age” variable, and store these average values in a new data table mean.HCP
mean.HCP <- aggregate(HCP[, c("Heating.Cost", "Minimum.Temperature")], # to select the specified variables (columns)
                     by = list(Age = HCP$Age),
                      FUN = mean)

# The cor() function computes the correlation matrix for all variables in the dataset.
CorHCP<-cor(HCP)
CorHCP

# Display correlation in a beautiful way
upper <- round(CorHCP, 2)
upper[upper.tri(CorHCP)] <- ""
upper <- as.data.frame(upper)
upper

# For nominal (categorical) data and numerical variables with few distinct values, we can analyze the following information:
# Frequencies: The number of observations for a particular category
# Proportions: The percent that each category accounts for out of the whole
# Marginals: The totals in a cross tabulation by row or column
# counts for Windows categories
table(HCP$Windows)
addmargins(prop.table(table(HCP$Windows)))
#   1    8    9   10   11   12   14  Sum
# 0.05 0.15 0.25 0.25 0.15 0.05 0.10 1.00

# to explore the number of houses with a particular number of windows and with heating equipment of a particular age, we can produce a cross classification table (cross-tabulation)
# dnn: the names/labels to be given to the dimensions in the result (row, column)
# rows = Windows; columns = Age categories
table(HCP$Windows, HCP$Age,
      dnn = c("windows", "age"))


# FREQUENCY MARGINALS
freqtable = table(HCP$Windows, HCP$Age,
                  dnn = c("windows", "age"))

# row marginals - totals for each windows category across age groups
# Marginals show the total counts or percentages across columns or rows in a contingency table.
margin.table(freqtable, 1)

# column marginals - totals for each age level across windows categories
margin.table(freqtable, 2)

addmargins(table(HCP$Windows, HCP$Age,
                 dnn = c("windows", "age")))

# REGRESSION MODEL
# Consider that we want to evaluate the impact of Minimum.Temperature on Heating.Cost: heating cost = b0 + b1 x (temperature) + e
model1 <- lm(Heating.Cost ~ Minimum.Temperature, data = HCP)
summary(model1)

#To obtain the 95% confidence intervals for all coefficients from the model, simply type:
confint(model1)

# Multiple linear regression
# Let us perform a regression analysis of heating cost as a function of all the available explanatory variables, i.e.
# heating cost = b0 + b1 x (temperature) + b2 x (insulation) + b3 x (age) + b4 x (windows) + e
model2 <- lm(HCP$Heating.Cost ~ HCP$Minimum.Temperature + HCP$Insulation + HCP$Age + HCP$Windows, data = HCP)
summary(model2)

confint(model2)

# Stepwise regression analysis
# The arguments of the ols_step_both_p function are as follows:
# fit: Model to fit. We need to use the lm() function before running ols_stepwise(), as indeed we have done here.
# pent: Threshold for the p-value used to enter a variable into the stepwise model.
# prem: Threshold for the p-value used to exclude a variable from the stepwise model.
# details: Print (or not) the details of each step
ols_step_both_p(model2, pent = 0.1, prem = 0.3, details = TRUE)
#Note that the variables Age and Windows are excluded from the final model.
# This can be due to a moderate correlation of Age and Minimum.Temperature (-0.49) which may signal multicollinearity,
# as well as to the low correlation between Windows and Heating.Cost.
# The results thus suggest that Minimum.Temperature and Insulation can explain the variations in the heating costs
# without the need to account for the age of the heating equipment and the number of windows.


plot(HCP$Minimum.Temperature, model1$residuals)
plot(model2)

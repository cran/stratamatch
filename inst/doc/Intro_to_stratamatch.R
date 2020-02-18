## ---- warning=FALSE, message = FALSE, include = FALSE--------------------
knitr::opts_chunk$set(warning = TRUE, message = TRUE, fig.align = "center", fig.height = 4, fig.width = 4)

## ------------------------------------------------------------------------
library(stratamatch)
set.seed(125)

# make sample data set of 5000 observations
dat <- make_sample_data(n = 5000)

# print the first few rows of the sample_data
head(dat)

## ------------------------------------------------------------------------
# manually stratify dat based on categorial and binary variables
m.strat <- manual_stratify(data = dat, treat ~ B1 + B2 + C1)

# try printing the result
m.strat

summary(m.strat)

## ------------------------------------------------------------------------
# show the first few rows of the stratified data set
head(m.strat$analysis_set)

## ------------------------------------------------------------------------
a.strat <- auto_stratify(dat, treat = "treat", prognosis = outcome ~ X1 + X2,
                         pilot_fraction = 0.1, size = 400)

# print and summarize the result from running auto_stratify
a.strat

summary(a.strat)

## ------------------------------------------------------------------------
# get strata table for manually stratified data set
m.strat$strata_table

## ------------------------------------------------------------------------
# show strata table for the automatically stratified data
a.strat$strata_table

## ------------------------------------------------------------------------
# issue table for manually stratified data
m.strat$issue_table

## ------------------------------------------------------------------------
# issue table for automatically stratified data
a.strat$issue_table

## ------------------------------------------------------------------------
# size-ratio plot for manually stratified data
plot(m.strat)
# plot(m.strat, type = "SR") will give the same output
# plot(m.strat, label = TRUE) will allow the user to click points to label them

## ------------------------------------------------------------------------
# size-ratio plot for automatically stratified data
plot(a.strat)

## ------------------------------------------------------------------------
# propensity score histograms for stratum 1 from manually stratified data
plot(m.strat, type = "hist", propensity = treat ~ X2 + X1 + B1 + B2, stratum = 1)

## ------------------------------------------------------------------------
# propensity score histograms for stratum 1 from automatically stratified data
plot(a.strat, type = "hist", propensity = treat ~ X2 + X1 + B1 + B2, stratum = 1)

## ------------------------------------------------------------------------
# make a Fisher-Mill plot
plot(a.strat, type = "FM", propensity = treat ~ X1 + X2, stratum = 1)

## ------------------------------------------------------------------------
# diagnostic plots for prognostic model
plot(a.strat, type = "residual")
# plot(a.strat$prognostic_model) will do the same thing - see below

## ------------------------------------------------------------------------
# extract prognostic model from a.strat
progmod <- a.strat$prognostic_model

# as an example, summarize model coefficients
summary(progmod)

## ------------------------------------------------------------------------
# match the automatically stratified data
mymatch <- strata_match(a.strat, treat ~ X1 + X2 + B1 + B2, k = 1)

# summarize matching results
summary(mymatch)

## ------------------------------------------------------------------------
# add match information as a column in the data set
matched_data <- a.strat$analysis_set
matched_data$match <- as.character(mymatch)

head(matched_data)


## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = TRUE,
  message = TRUE,
  fig.align = "center",
  fig.height = 5,
  fig.width = 6
)

## -----------------------------------------------------------------------------
library(stratamatch)
library(dplyr)
set.seed(123)
mydata <- make_sample_data(n = 5000)

## -----------------------------------------------------------------------------
a.strat1 <- auto_stratify(mydata, "treat",
                         prognosis = outcome ~ X1 + X2,
                         group_by_covariates = c("C1", "B2"), size = 500)

## -----------------------------------------------------------------------------
mysplit <- split_pilot_set(mydata, "treat", group_by_covariates = c("C1", "B2"))

## -----------------------------------------------------------------------------
a.strat2 <- auto_stratify(mysplit$analysis_set, "treat",
                          prognosis = outcome ~ X1 + X2,
                          pilot_sample = mysplit$pilot_set, size = 500)

## -----------------------------------------------------------------------------
library(glmnet)

# fit model on pilot set
x_pilot <- model.matrix(outcome ~ X1 + X2 + B1 + B2 + C1,
                        data = mysplit$pilot_set)

y_pilot <- mysplit$pilot_set %>%
  dplyr::select(outcome) %>%
  as.matrix()

cvlasso <- cv.glmnet(x_pilot, y_pilot, family = "binomial")

## -----------------------------------------------------------------------------
coef(cvlasso)

## -----------------------------------------------------------------------------
# estimate scores on analysis set
x_analysis <- model.matrix(outcome ~ X1 + X2 + B1 + B2 + C1,
                        data = mysplit$analysis_set)

lasso_scores <- predict(cvlasso, newx = x_analysis, s = "lambda.min", type = "response")

# pass the scores to auto_stratify
a.strat_lasso <- auto_stratify(data = mysplit$analysis_set,
                             treat = "treat",
                             outcome = "outcome",
                             prognosis = lasso_scores,
                             pilot_sample = mysplit$pilot_set,
                             size = 500)

## -----------------------------------------------------------------------------
cvenet <- cv.glmnet(x_pilot, y_pilot, family = "binomial", alpha = 0.2)

enet_scores <- predict(cvenet, newx = x_analysis, s = "lambda.min", type = "response")

# pass the scores to auto_stratify
a.strat_enet <- auto_stratify(data = mysplit$analysis_set,
                             treat = "treat",
                             outcome = "outcome",
                             prognosis = enet_scores,
                             pilot_sample = mysplit$pilot_set,
                             size = 500)

## -----------------------------------------------------------------------------
library(randomForest)
forest <- randomForest(as.factor(outcome) ~ X1 + X2 + B1 + B2, data = mysplit$pilot_set)

## -----------------------------------------------------------------------------
forest_scores <- predict(forest,
                         newdata = mysplit$analysis_set,
                         type = "prob")[,1]

a.strat_forest <- auto_stratify(data = mysplit$analysis_set,
                             treat = "treat",
                             outcome = "outcome",
                             prognosis = forest_scores,
                             pilot_sample = mysplit$pilot_set,
                             size = 500)

## -----------------------------------------------------------------------------
mahalmatch <- strata_match(a.strat2, model = treat ~ X1 + X2 + B1 + B2,
                           method = "mahal", k = 2)

summary(mahalmatch)

## -----------------------------------------------------------------------------
fullmahalmatch <- strata_match(a.strat2, model = treat ~ X1 + X2 + B1 + B2,
                           method = "mahal", k = "full")

summary(fullmahalmatch)

## -----------------------------------------------------------------------------
library(optmatch)

# mahalanobis distance matrix for within-strata matching
mahaldist <- match_on(treat ~ X1 + X2 + B1 + B2, 
                      within = exactMatch(treat ~ stratum,
                                          data = a.strat2$analysis_set),
                      data = a.strat2$analysis_set)

# add propensity score caliper
propdist <- match_on(glm(treat ~ X1 + X2 + B1 + B2,
                         family = "binomial",
                         data = a.strat2$analysis_set))

mahalcaliper <- mahaldist + caliper(propdist, width = 1)

mahalcaliper_match <- pairmatch(mahalcaliper, data = a.strat2$analysis_set)

summary(mahalcaliper_match)


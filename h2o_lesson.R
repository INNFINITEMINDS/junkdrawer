# notes courtesy of Antoine Chambaz

## ############
## INTRODUCTION
## ############

## http://tinyurl.com/h2o-meetups
## (all the way down the list)

## possible to export  fitted models to java ("mojo"  object, improved version
## of "pojo" objects), but they're bigger in this form than in binary form

## About neural networks, accessible through 'Deep Water'

## ## - Convolutional neural networks (CNNs) -> good for image data
## ## (need GPU)
## ## - Recurrent Neural Networks (RNNs) -> good for sequence learning!
## ## (need GPU)
## ## - Multilayer Perceptrons (MLPs) -> 
  
## H2O and Spark...
## ## 'Sparkling Water'
## ## 'sparklyr' package

## AutoML: data, outcome, risk, time allowed -> 'best' model given constraints

## ########
## EXAMPLES
## ########

example <- c(TRUE, FALSE)[2]

if (example) {
  library(h2o)
  localH2O <- h2o.init() ## nthreads=-1 ## all cores
  train <- h2o.importFile(path="train.csv")
  ## better than:
  ## train <- as.h2o(myDataFrame)
  ## which takes place in R

  y <- "Class"
  x <- setdiff(names(train), y)
  fit <- h2o.gbm(x=x, y=y, training_frame=train)
  pred <- h2o.predict(fit, test)
}

if (example) {
  library(h2o)
  localH2O <- h2o.init() ## nthreads=-1 ## all cores
  train <- h2o.importFile(path="train.csv")
  ## better than:
  ## train <- as.h2o(myDataFrame)
  ## which takes place in R

  hidden_opt <- list(c(200, 200), c(100, 300, 100), c(500, 500))
  l1_opt <- c(1e-5, 1e-7)
  hyper_params <- list(hidden=hidden_opt, l1=l1_opt)

  y <- "Class"
  x <- setdiff(names(train), y)

  ## ##
  ## Cartesian grid search
  ## ##
  grid <- h2o.grid(algorithm="deeplearning",
                   hyper_params=hyper_params,
                   x=x, y=y,
                   training_frame=train,
                   validation_frame=valid##
                   ##, nfold=5 ## to request cross-validation
                   ##, seed=12345 ## for reproducibility
                   )
  ## 'test' only for *final* evaluation
  ## 'validation' for validation

  ## ##
  ## Random grid search
  ## ##
  search_criteria <- list(strategy="RandomDiscrete",
                          max_runtime_seconds=600)
  ## more subtle:
  ## list(strategy="RandomDiscrete",
  ##      stopping_metric="AUTO",
  ##      stopping_tolerance=1e-3,
  ##      stopping_rounds=5)
  grid <- h2o.grid(algorithm="deeplearning",
                   hyper_params=hyper_params,
                   search_criteria=search_criteria,
                   x=x, y=y,
                   training_frame=train,
                   validation_frame=valid##
                   ##, nfold=5 ## to request cross-validation
                   ##, seed=12345 ## for reproducibility
                   )
}

if (example) {
  library(h2o)
  localH2O <- h2o.init() ## nthreads=-1 ## all cores
  train <- h2o.importFile(path="train.csv")
  ## better than:
  ## train <- as.h2o(myDataFrame)
  ## which takes place in R

  y <- "Class"
  x <- setdiff(names(train), y)

  library(h2oEnsemble)
  
  ## create list of all base models
  models <- c(gbm_models, rf_models, dl_models, glm_models)
  ## a list of lists, each a 'cross-validated model object', eg
  ## gbm1 <- h2o.gbm(..., nfolds=5)

  ## stacking
  stack <- h2o.stack(models=models,
                     response_frame=train[,y],
                     metalearner=metalearner)
}

## #########
## IN ACTION
## #########

## http://tinyurl.com/h2o-tutorials-r

## cloned to './h2o-tutorials'


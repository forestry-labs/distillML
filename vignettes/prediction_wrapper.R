## ---- include=FALSE, echo = FALSE, results='hide'-----------------------------
# Load the required packages
library(Distillery)
library(Rforestry)
library(ggplot2)
library(dplyr)


## ---- echo = TRUE-------------------------------------------------------------
# Load the required packages
library(Distillery)
library(nnet)

# Load in data 
data("iris")
set.seed(491)
data <- iris

# Train a mutinomial logistic regression model on the data set
multilog <- multinom(Species ~., data = data)
head(predict(multilog, type = "probs"))

# Create a predictor wrapper for the model (specify task, class, type)
setosa_predictor <- Predictor$new(model = multilog,
                                  data=data,
                                  y="Species",
                                  task = "classification",
                                  class = "setosa",
                                  type = "probs")

# Predictions returned for setosa probabilities
head(predict(setosa_predictor, iris))


## -----------------------------------------------------------------------------
library(dbarts)
cat.index <- which(names(data) == "Species") # BART requires numerical features

# create a BART model
bart.model <- bart(x.train = data[, -c(1, cat.index)],
                   y.train = data[, 1],
                   keeptrees = T,
                   ndpost = 500,
                   verbose = F)

# default predictions result in multiple columns
predict(bart.model, data[, -c(1, cat.index)])[1:5, 1:5]

# create user-specified function
predict_bart <- function(object, newdata){
  return(colMeans(predict(object, newdata)))
}

# initialize Predictor object for BART
bart_predictor <- Predictor$new(model = bart.model,
                                data = data[, -cat.index],
                                predict.func = predict_bart,
                                y = "Sepal.Length",
                                task = "regression")

# predict with the Predictor object
predict(bart_predictor, data[, -c(1, cat.index)])[1:5, ]


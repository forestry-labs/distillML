## ---- include=FALSE, echo = FALSE, results='hide'-----------------------------
# Load the required packages
library(distillML)
library(Rforestry)
library(ggplot2)
library(dplyr)


## ---- echo = TRUE-------------------------------------------------------------
# Load the required packages
library(distillML)
library(Rforestry)

# Load in data
data("iris")
set.seed(491)
data <- iris

# Split data into train and test sets
train <- data[1:100, ]
test <- data[101:150, ]

# Train a random forest on the data set
forest <- forestry(x=train[, -1],
                   y=train[, 1])

# Create a predictor wrapper for the forest
forest_predictor <- Predictor$new(model = forest,
                                  data=data,
                                  y="Sepal.Length",
                                  task = "regression")

forest_interpret <- Interpreter$new(predictor = forest_predictor)


## ---- echo=TRUE---------------------------------------------------------------
# Distilling the Model With Default Settings
distilled_model <- distill(forest_interpret)


## -----------------------------------------------------------------------------
print(distilled_model)

# get weights for each PDP curve
print(distilled_model$weights)


## -----------------------------------------------------------------------------
# Sparse Model
sparse_model <- distill(forest_interpret,
                           cv = T,
                           params.cv.glmnet = list(lower.limits = 0,
                                                   intercept = F,
                                                   alpha = 1))
print(distilled_model$weights)
print(sparse_model$weights)


## ---- echo=TRUE---------------------------------------------------------------
# make predictions
original.preds <- predict(forest, test[,-1])
distill.preds <- predict(distilled_model, test[,-1])[,1]
sparse.preds <- predict(sparse_model, test[,-1])[,1]

# compare MSE
rmse <- function(preds){
  return(sqrt(mean((test[,1] - preds)^2)))
}

print(data.frame(original = rmse(original.preds),
                 distill = rmse(distill.preds),
                 distill.sparse = rmse(sparse.preds)))



## -----------------------------------------------------------------------------
# create dataframe of data we'd like to plot
plot.data <- data.frame(original.preds,
                        distill.preds,
                        sparse.preds)

# plots for both default distilled model and sparse model
default.plot <- ggplot(data = plot.data, aes(x = original.preds,
                                             y = distill.preds)) +
                geom_point() + geom_abline(col = "red")

sparse.plot <- ggplot(data = plot.data, aes(x = original.preds,
                                             y = sparse.preds)) +
                geom_point() + geom_abline(col = "red")

plot(default.plot)
plot(sparse.plot)


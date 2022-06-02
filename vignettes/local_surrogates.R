## ---- include=FALSE, echo = FALSE, results='hide'-----------------------------
# Load the required packages
library(Distillery)
library(Rforestry)
library(ggplot2)


## ---- echo = TRUE-------------------------------------------------------------
# Load the required packages
library(Distillery)
library(Rforestry)

# Load in data 
data("iris")
set.seed(491)
data <- iris

# Train a random forest on the data set
forest <- forestry(x=data[,-1],
                   y=data[,1])

# Create a predictor wrapper for the forest
forest_predictor <- Predictor$new(model = forest,
                                  data=data,
                                  y="Sepal.Length",
                                  task = "regression")

# Create the interpreter object
forest_interpret <- Interpreter$new(predictor = forest_predictor)


## ---- echo=TRUE---------------------------------------------------------------
# Make the bivariate PDP function
local.surr <- localSurrogate(forest_interpret,
                             features.2d = data.frame(col1 = c("Sepal.Width",
                                                               "Sepal.Width"),
                                                      col2 = c("Species",
                                                               "Petal.Width")))

# examples of the plot
plot(local.surr$plots$Sepal.Width.Species)
plot(local.surr$plots$Sepal.Width.Petal.Width)

# examples of the weak learner
plot(local.surr$models$Sepal.Width.Species)
plot(local.surr$models$Sepal.Width.Petal.Width)



## -----------------------------------------------------------------------------
# Include interactions and let the maximum depth be 3
local.surr <- localSurrogate(forest_interpret,
                             features.2d = data.frame(col1 = c("Sepal.Width"),
                                                      col2 = c("Petal.Width")),
                             interact = T,
                             params.forestry = list(ntree = 1, maxDepth = 3))

# Plot the resulting local surrogate model
plot(local.surr$models$Sepal.Width.Petal.Width)



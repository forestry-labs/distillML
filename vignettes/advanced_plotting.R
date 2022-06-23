## -----------------------------------------------------------------------------
library(MASS)
library(distillML)
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

# We specify grid.size for clarity (grid.size = 50 by default)
forest_interpreter <- Interpreter$new(forest_predictor,
                                      grid.size = 50)
print(forest_interpreter)


## -----------------------------------------------------------------------------
# The values of Sepal.Width to be plotted by PDP and ICE curves
print(forest_interpreter$grid.points$Sepal.Width)

# The number of grid.points is equal to the grid.size
print(length(forest_interpreter$grid.points$Sepal.Width))
print(forest_interpreter$grid.size)

# The value of Sepal.Width we center the PDP and ICE curves
print(forest_interpreter$center.at$Sepal.Width)

# plot the PDP and ICE curves
plot(forest_interpreter, features = "Sepal.Width")


## -----------------------------------------------------------------------------
# Set new grid points
set.grid.points(forest_interpreter, "Sepal.Width",
                values = seq(2, 4.5, length.out = 100))

# Set new center
set.center.at(forest_interpreter, "Sepal.Width",
              mean(seq(2, 4.5, length.out = 100)))

# New plot
plot(forest_interpreter, features = "Sepal.Width")



## -----------------------------------------------------------------------------
# ALE plot
plot(forest_interpreter, features = "Sepal.Width", method = "ale")


## -----------------------------------------------------------------------------
# Clustering Based on the Predicted Values of the ICE Curves
plot(forest_interpreter,
     features = "Sepal.Width",
     method = "ice",
     clusters = 4,
     clusterType = "preds")

# Clustering Based on the Change in Predicted Values of the ICE Curves
plot(forest_interpreter,
     features = "Sepal.Width",
     method = "ice",
     clusters = 4,
     clusterType = "gradient")


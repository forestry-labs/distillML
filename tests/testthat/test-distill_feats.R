test_that("Tests that distillation with selected features is working", {

  library(Rforestry)
  library(Distillery)
  set.seed(491)

  data <- MASS::crabs
  levels(data$sex) <- list(Male = "M", Female = "F")
  levels(data$sp) <- list(Orange = "O", Blue = "B")
  colnames(data) <- c("Species","Sex","Index","FrontalLobe",
                      "RearWidth", "CarapaceLength","CarapaceWidth","BodyDepth")
  data <- data[, c(-1,-2,-3)]

  test_ind <- sample(1:nrow(data), nrow(data)%/%5)
  train_reg <- data[-test_ind,]
  test_reg <- data[test_ind,]

  # Train a random forest on the data set
  forest <- forestry(x=train_reg[,-ncol(train_reg)],
                     y=train_reg[,ncol(train_reg)])

  # Create a predictor wrapper for the forest
  forest_predictor <- Predictor$new(model = forest,
                                    data=train_reg,
                                    y="BodyDepth",
                                    task = "regression")

  # Initialize an interpreter
  forest_interpret <- Interpreter$new(predictor = forest_predictor)

  # Standard distillation
  surrogate.model <- distill(forest_interpret)
  surrogate.model$weights
  expect_equal(all.equal(names(surrogate.model$weights), c("FrontalLobe","RearWidth",
                                                           "CarapaceLength","CarapaceWidth")),TRUE)

  # Try passing some feature indices to the model to select specific features
  surrogate.model <- distill(forest_interpret,
                             features = c(1,2,3))
  surrogate.model$weights

  expect_equal(all.equal(names(surrogate.model$weights), c("FrontalLobe","RearWidth", "CarapaceLength")),TRUE)

  surrogate.model <- distill(forest_interpret,
                             features = c(2,3))
  surrogate.model$weights
  expect_equal(all.equal(names(surrogate.model$weights), c("RearWidth", "CarapaceLength")),TRUE)

  rm(list=ls())
})

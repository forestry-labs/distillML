test_that("Tests that the local surrogate wrapper is working", {

  library(Rforestry)
  set.seed(491)

  data <-iris

  test_ind <- sample(1:nrow(data), nrow(data)%/%5)
  train_reg <- data[-test_ind,]
  test_reg <- data[test_ind,]

  # Train a random forest on the data set
  forest <- forestry(x=train_reg[,-1],
                     y=train_reg[,1])

  # Create a predictor wrapper for the forest
  forest_predictor <- Predictor$new(model = forest,
                                    data=train_reg,
                                    y="Sepal.Length",
                                    task = "regression")

  # Initialize an interpreter
  forest_interpret <- Interpreter$new(predictor = forest_predictor)
  expect_equal(all.equal(forest_interpret$features,
                         c( "Sepal.Width",  "Petal.Length", "Petal.Width",  "Species"  )),TRUE)

  # Test surrogate + interaction term
  local.surr <- localSurrogate(forest_interpret,
                               features.2d = data.frame(feat.1 = c("Petal.Width"),
                                                        feat.2 = c("Petal.Length")),
                               interact = TRUE)
  expect_equal(names(local.surr$plots), names(local.surr$models))
  expect_equal(names(local.surr$plots), "Petal.Width.Petal.Length")
  expect_equal(local.surr$models[[1]]@ntree, 1)
  expect_equal(ncol(local.surr$models[[1]]@processed_dta$processed_x), 3)


  # Test user-supplied parameters
  local.surr <- localSurrogate(forest_interpret,
                               features.2d = data.frame(feat.1 = c("Petal.Length"),
                                                     feat.2 = c("Species")),
                               params.forestry = list(maxDepth = 4, ntree = 2))
  expect_equal(names(local.surr$plots), names(local.surr$models))
  expect_equal(names(local.surr$plots), "Petal.Length.Species")
  expect_equal(local.surr$models[[1]]@ntree, 2)
  expect_equal(local.surr$models[[1]]@maxDepth, 4)

  rm(list=ls())
})


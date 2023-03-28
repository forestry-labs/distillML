test_that("Tests that distillation with selected features is working", {

  library(Rforestry)
  set.seed(491)

  data <- iris

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

  feat.index <- which(forest_interpret$features %in% c("Sepal.Width", "Petal.Width", "Petal.Length"))

  # Standard distillation
  context("Default distillation function")
  surrogate.model <- distill(forest_interpret, features = feat.index)
  surrogate.model$weights
  expect_equal(all.equal(names(surrogate.model$weights),
                         c("Sepal.Width", "Petal.Length","Petal.Width")),
               TRUE)
  expect_equal(sum(surrogate.model$weights < 0), 0)

  expect_equal(surrogate.model$grid$Petal.Length[,1],
               forest_interpret$predictor$data[forest_interpret$data.points, "Petal.Length"])

  expect_equal(surrogate.model$grid$Petal.Length[,2],
               forest_interpret$pdp.1d$Petal.Length(forest_interpret$predictor$data[forest_interpret$data.points, "Petal.Length"]),
               tolerance = 1e4)

  # Equal Grid Spacing
  context("Snap.train F for distill")
  surrogate.model <- distill(forest_interpret, features = feat.index, snap.train = FALSE)
  expect_equal(all.equal(names(surrogate.model$weights),
                         c("Sepal.Width", "Petal.Length","Petal.Width")),
               TRUE)
  expect_equal(sum(surrogate.model$weights < 0), 0)
  expect_equal(surrogate.model$grid$Petal.Width[,1],
               forest_interpret$grid.points$Petal.Width)

  expect_equal(surrogate.model$grid$Petal.Width[,2],
               forest_interpret$pdp.1d$Petal.Width(forest_interpret$grid.points$Petal.Width),
               tolerance = 1e4)

  # No snapping to grid
  context("Snap.grid FALSE for distill")
  surrogate.model <- distill(forest_interpret, features = feat.index, snap.grid = FALSE)
  expect_equal(all.equal(names(surrogate.model$weights),
                         c("Sepal.Width", "Petal.Length","Petal.Width")),
               TRUE)
  expect_equal(sum(surrogate.model$weights < 0), 0)
  expect_equal(surrogate.model$grid, NA)

  # Specific feature test
  context("distill with specified features")
  surrogate.model <- distill(forest_interpret, features = feat.index)
  expect_equal(all.equal(names(surrogate.model$weights),
                         c("Sepal.Width", "Petal.Length","Petal.Width")),
               TRUE)
  expect_equal(sum(surrogate.model$weights < 0), 0)
  expect_equal(length(surrogate.model$grid), 3)

  # Check that we can pass parameters to glmnet
  context("distill with user-specified parameters for fitting")
  surrogate.model <- distill(forest_interpret, features = feat.index,
                             params.glmnet = list(family = "gaussian",
                                                  alpha = 1,
                                                  lambda = 0,
                                                  intercept = FALSE))
  expect_equal(all.equal(names(surrogate.model$weights),
                         c("Sepal.Width", "Petal.Length","Petal.Width")),
               TRUE)
  expect_equal(sum(surrogate.model$weights >0), 3)


  # Check that we can do cross validation with cv.glmnet
  context("distill with cross validation")
  # surrogate.model <- distill(forest_interpret, cv = TRUE)
  # expect_equal(all.equal(names(surrogate.model$weights),
  #                        c("Species_Blue", "Species_Orange", "Sex_Male", "Sex_Female", "Index",
  #                          "FrontalLobe","RearWidth","CarapaceLength","CarapaceWidth")),
  #              TRUE)
  # expect_equal(sum(surrogate.model$weights < 0), 0)
  #
  # surrogate.model <- distill(forest_interpret, cv = TRUE,
  #                            params.cv.glmnet = list(upper.limits = 0,
  #                                                    intercept = FALSE,
  #                                                    alpha = 1))
  # expect_equal(all.equal(names(surrogate.model$weights),
  #                        c("Species_Blue", "Species_Orange", "Sex_Male", "Sex_Female", "Index",
  #                          "FrontalLobe","RearWidth","CarapaceLength","CarapaceWidth")),
  #              TRUE)
  # expect_equal(sum(surrogate.model$weights > 0), 0)

  rm(list=ls())
})

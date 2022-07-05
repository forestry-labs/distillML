test_that("Tests that the local surrogate wrapper is working", {

  library(Rforestry)
  set.seed(491)

  data <- MASS::crabs
  levels(data$sex) <- list(Male = "M", Female = "F")
  levels(data$sp) <- list(Orange = "O", Blue = "B")
  colnames(data) <- c("Species","Sex","Index","FrontalLobe",
                      "RearWidth", "CarapaceLength","CarapaceWidth","BodyDepth")


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
  expect_equal(all.equal(forest_interpret$features,
                         c( "Species","Sex","Index","FrontalLobe","RearWidth","CarapaceLength","CarapaceWidth")),TRUE)

  # Test surrogate + interaction term
  local.surr <- localSurrogate(forest_interpret,
                               features.2d = data.frame(feat.1 = c("FrontalLobe"),
                                                        feat.2 = c("RearWidth")),
                               interact = TRUE)
  expect_equal(names(local.surr$plots), names(local.surr$models))
  expect_equal(names(local.surr$plots), "FrontalLobe.RearWidth")
  expect_equal(local.surr$models[[1]]@ntree, 1)
  expect_equal(ncol(local.surr$models[[1]]@processed_dta$processed_x), 3)


  # Test user-supplied parameters
  local.surr <- localSurrogate(forest_interpret,
                               features.2d = data.frame(feat.1 = c("FrontalLobe"),
                                                     feat.2 = c("Sex")),
                               params.forestry = list(maxDepth = 4, ntree = 2))
  expect_equal(names(local.surr$plots), names(local.surr$models))
  expect_equal(names(local.surr$plots), "FrontalLobe.Sex")
  expect_equal(local.surr$models[[1]]@ntree, 2)
  expect_equal(local.surr$models[[1]]@maxDepth, 4)

  rm(list=ls())
})


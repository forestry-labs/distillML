test_that("Tests that the surrogate models are working", {

  library(Rforestry)
  set.seed(491)

  data <- MASS::crabs
  levels(data$sex) <- list(Male = "M", Female = "F")
  levels(data$sp) <- list(Orange = "O", Blue = "B")
  colnames(data) <- c("Species","Sex","Index","FrontalLobe",
                      "RearWidth", "CarapaceLength","CarapaceWidth","BodyDepth")

  test_ind <- sample(1:nrow(data), 180)
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

  # Check three distillation models
  context("Try the prediction method for surrogates.")
  default <- distill(forest_interpret)
  expect_equal(sum(is.na(forest_interpret$saved$PDP.1D)),
               length(forest_interpret$features))

  snaptogrid <- distill(forest_interpret, snap.train = FALSE)
  expect_equal(sum(is.na(forest_interpret$saved$PDP.1D)), 0)

  recalculate <- distill(forest_interpret, snap.grid = FALSE)

  default.preds <- predict(default,
                           test_reg[, -which(names(test_reg) == "BodyDepth")])
  snaptogrid.preds <- predict(snaptogrid,
                           test_reg[, -which(names(test_reg) == "BodyDepth")])
  recalculate.preds <- predict(recalculate,
                           test_reg[, -which(names(test_reg) == "BodyDepth")])

  expect_equal(dim(default.preds), c(nrow(test_reg), 1))
  expect_equal(dim(snaptogrid.preds), c(nrow(test_reg), 1))
  expect_equal(dim(recalculate.preds), c(nrow(test_reg), 1))

  # Check that the snap.grid option only calculates the features selected
  context("Snap.train additional testing")
  feat.index <- which(forest_interpret$features %in% c("Sex", "FrontalLobe", "RearWidth"))
  forest_interpret <- Interpreter$new(predictor = forest_predictor)
  surr.model <- distill(forest_interpret, features = feat.index, snap.train = FALSE)

  expect_equal(sum(is.na(forest_interpret$saved$PDP.1D)),
               length(forest_interpret$features)-3)

  rm(list=ls())
})

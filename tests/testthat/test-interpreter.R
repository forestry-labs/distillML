test_that("Tests that the predictor wrapper is working", {

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

  expect_equal(length(forest_interpret$data.points[1:10]),
               10)

  expect_equal(length(forest_interpret$functions.1d),
               0)
  # Get predictions from a pdp function
  context("Try pdp predictions")

  # Get the names of the pdp functions
  func_names <- names(forest_interpret$functions.1d)
  expect_equal(func_names, NULL)

  pred_pdp <- forest_interpret$pdp.1d$`RearWidth`(c(10,11,12,14,15))
  pred_pdp_alt <- forest_interpret$pdp.1d[[5]](c(10,11,12,14,15))

  pred_pdp_2 <- forest_interpret$pdp.2d$Species$Sex(data.frame(col1 = data$Species,
                                                               col2 = data$Sex))

  pred_pdp_2_alt <- forest_interpret$pdp.2d[[1]][[2]](data.frame(col1 = data$Species,
                                                               col2 = data$Sex))

  expect_equal(all.equal(pred_pdp,pred_pdp_alt, tolerance = 1e-8), TRUE)
  expect_equal(all.equal(pred_pdp_2,pred_pdp_2_alt, tolerance = 1e-8), TRUE)

  expect_equal(length(pred_pdp), 5)
  expect_equal(length(pred_pdp_2), 200)

  rm(list=ls())
})

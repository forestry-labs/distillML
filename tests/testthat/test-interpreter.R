test_that("Tests that the predictor wrapper is working", {

  library(Rforestry)
  set.seed(491)

  data <- MASS::crabs
  levels(data$sex) <- list(Male = "M", Female = "F")
  levels(data$sp) <- list(Orange = "O", Blue = "B")
  colnames(data) <- c("Species","Sex","Index","Frontal Lobe",
                      "Rear Width", "Carapace Length","Carapace Width","Body Depth")


  test_ind <- sample(1:nrow(data), nrow(data)%/%5)
  train_reg <- data[-test_ind,]
  test_reg <- data[test_ind,]

  # Train a random forest on the data set
  forest <- forestry(x=train_reg[,-ncol(train_reg)],
                     y=train_reg[,ncol(train_reg)])

  # Create a predictor wrapper for the forest
  forest_predictor <- Predictor$new(model = forest,
                                    data=train_reg,
                                    y="Body Depth",
                                    task = "regression")

  # Initialize an interpreter
  forest_interpret <- Interpreter$new(predictor = forest_predictor)

  expect_equal(length(forest_interpret$data.points[1:10]),
               10)

  expect_equal(length(forest_interpret$functions.1d),
               7)
  # Get predictions from a pdp function
  context("Try pdp predictions")
  pred_pdp <- forest_interpret$functions.1d$`Rear Width`(c(10,11,12))

  pred_pdp_2 <- forest_interpret$functions.2d$`Rear Width`$`Frontal Lobe`(matrix(c(10, 20), nrow=1))

  expect_equal(length(pred_pdp), 3)

  expect_equal(length(pred_pdp_2), 1)


})

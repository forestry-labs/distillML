test_that("Tests that the ale plots function is working", {

  library(Rforestry)
  set.seed(491)

  data <-iris

  test_ind <- sample(1:nrow(data), nrow(data)%/%5)
  train_reg <- data[-test_ind,]
  test_reg <- data[test_ind,]

  # Train a random forest on the data set
  forest <- forestry(x=train_reg[,-c(1)],
                     y=train_reg[,1])

  # Create a predictor wrapper for the forest
  forest_predictor <- Predictor$new(model = forest,
                                    data=train_reg,
                                    y="Sepal.Length",
                                    task = "regression")

  # Initialize an interpreter
  forest_interpret <- Interpreter$new(predictor = forest_predictor)


  p <- plot(forest_interpret, method = "ale", features = c("Sepal.Width"))
  expect_equal(names(p),"Sepal.Width")
  rm(list=ls())
})

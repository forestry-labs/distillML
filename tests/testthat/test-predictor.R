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


  context("Check class attributes")
  expect_equal(forest_predictor$task,"regression")

  expect_equal(forest_predictor$batch.size, 1000)


  expect_equal(forest_predictor$model@ntree, 500)

  context("Check the model in predictor")
  expect_equal(all.equal(forest_predictor$model@processed_dta$y[1:10],
            c(-2.0895765, -1.8855020, -1.7397346, -1.7397346,
              -1.2732787, -1.0983578, -1.3024322, -0.9525903,
              -0.9525903, -0.9234368),
            tolerance = 1e-4), TRUE)


})

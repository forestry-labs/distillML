test_that("Tests that the local surrogate wrapper is working", {

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

  # Now test the surrogate method
  #forest_plot <- plot(forest_interpret,
  #                    method = "pdp+ice",
  #                           features.2d = data.frame(col1 = c("Frontal Lobe"),
  #                                                    col2 = c("Rear Width")))

  #forest_surrogate <- localSurrogate(forest_plot,
  #                                   interact = FALSE)
  # plot(forest_surrogate$models[[1]])
  #expect_equal(forest_surrogate$models[[1]]@ntree, 1)


  # Try with interaction
  #forest_surrogate <- localSurrogate(forest_plot,
  #                                   interact = TRUE)
  #expect_equal(ncol(forest_surrogate$models[[1]]@processed_dta$processed_x), 3)

  #Try with categorical features
  #forest_plot <- plot(forest_interpret,
  #                           features.2d = data.frame(col1 = c("Frontal Lobe"),
  #                                                    col2 = c("Species")))

  #forest_surrogate <- localSurrogate(forest_plot)

  #expect_gt(length(forest_surrogate$models[[1]]@processed_dta$categoricalFeatureCols_cpp),
  #          0)
  # plot(forest_surrogate$models[[1]])
})

test_that("Tests that the plotting functions are working", {

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

  context("Check plotting method subroutines")
  # Check the set.center method
  set.center.at(forest_interpret, "Sepal.Width", 2)
  expect_equal(forest_interpret$center.at$Sepal.Width, 2)

  # Check the set.grid.points method
  set.grid.points(forest_interpret, "Petal.Length", c(1,2,3))
  expect_equal(forest_interpret$grid.points$Petal.Length,  c(1,2,3))

  # Check ICE plots, PDP plots, 2-D PDP plots and "save" feature
  ice.plots <- predict_ICE.Plotter(forest_interpret, features = "Petal.Length")
  expect_equal(dim(ice.plots$Petal.Length), c(3, 121))
  expect_equal(forest_interpret$saved$ICE$Petal.Length, ice.plots$Petal.Length)

  pdp.plots <- predict_PDP.1D.Plotter(forest_interpret, features = "Petal.Width")
  expect_equal(dim(pdp.plots$Petal.Width), c(50,2))
  expect_equal(forest_interpret$saved$PDP.1D$Petal.Width, pdp.plots$Petal.Width)

  pdp.2d.plots <- predict_PDP.2D.Plotter(forest_interpret,
                                         feat.2d = data.frame(feat1 = "Petal.Width",
                                                              feat2 = "Petal.Length"))
  expect_equal(dim(pdp.2d.plots$`Petal.Length, Petal.Width`), c(150, 3))
  expect_equal(forest_interpret$saved$PDP.2D$`Petal.Length, Petal.Width`,
               pdp.2d.plots$`Petal.Length, Petal.Width`)

  # Should delete previously done calculations
  set.grid.points(forest_interpret, "Petal.Width", c(0,1.5,2.0))
  set.grid.points(forest_interpret, "Petal.Length", c(0,1.5,2.0))

  expect_equal(all(is.na(forest_interpret$saved$ICE)), TRUE)
  expect_equal(all(is.na(forest_interpret$saved$PDP.1D)), TRUE)
  expect_equal(all(is.na(forest_interpret$saved$PDP.2D)), TRUE)

  # Check that save == F parameter works
  ice.plots <- predict_ICE.Plotter(forest_interpret,
                                   features = "Petal.Width",
                                   save = FALSE)
  expect_equal(dim(ice.plots$Petal.Width), c(3, 121))
  expect_equal(forest_interpret$saved$ICE$Petal.Width, NA)

  pdp.plots <- predict_PDP.1D.Plotter(forest_interpret,
                                      features = "Petal.Width",
                                      save = FALSE)
  expect_equal(dim(pdp.plots$Petal.Width), c(3,2))
  expect_equal(forest_interpret$saved$PDP.1D$Petal.Width, NA)

  pdp.2d.plots <- predict_PDP.2D.Plotter(forest_interpret,
                                         feat.2d = data.frame(feat1 = "Petal.Width",
                                                              feat2 = "Petal.Length"),
                                         save = FALSE)
  expect_equal(dim(pdp.2d.plots$`Petal.Length, Petal.Width`), c(9, 3))
  expect_equal(forest_interpret$saved$PDP.2D$`Petal.Length, Petal.Width`,
               NA)



  # Initialize a plotter
  context("Try PDP plotting")

  forest_plot <- plot(forest_interpret, method = "pdp+ice",features = "Petal.Length")
  expect_equal(names(forest_plot), "Petal.Length")
  expect_equal(predict_ICE.Plotter(forest_interpret, features = "Petal.Length")[[1]],
               forest_interpret$saved$ICE$Petal.Length)
  expect_equal(predict_PDP.1D.Plotter(forest_interpret, features = "Petal.Length")[[1]],
               forest_interpret$saved$PDP.1D$Petal.Length)

  multiple_plot <- plot(forest_interpret, method = "pdp+ice",
                        features = c("Petal.Length", "Sepal.Width"))
  expect_equal(names(multiple_plot), c("Petal.Length", "Sepal.Width"))

  ice_plot <- plot(forest_interpret,
                   method = "ice",
                   features = "Petal.Length",
                   clusters = 4,
                   clusterType = "preds")
  ice_plot <- plot(forest_interpret,
                   method = "ice",
                   features = "Petal.Length",
                   clusters = 4,
                   clusterType = "gradient")
  expect_equal(names(ice_plot), "Petal.Length")


  twodim_plot <- plot(forest_interpret,
                      features.2d = data.frame(feat1 = c("Petal.Length", "Petal.Length"),
                                               feat2 = c("Sepal.Width", "Petal.Width")))
  expect_equal(names(twodim_plot), c("Petal.Length.Sepal.Width", "Petal.Length.Petal.Width"))
  expect_equal(forest_interpret$saved$PDP.2D$`Petal.Length, Sepal.Width`,
               predict_PDP.2D.Plotter(forest_interpret,
                                      feat.2d = data.frame(feat1 = c("Petal.Length"),
                                                               feat2 = c("Sepal.Width")))[[1]])
  expect_equal(forest_interpret$saved$PDP.2D$`Petal.Length, Petal.Width`,
               predict_PDP.2D.Plotter(forest_interpret,
                                      feat.2d = data.frame(feat1 = c("Petal.Length"),
                                                               feat2 = c("Petal.Width")))[[1]])

  combined_plot <- plot(forest_interpret,
                        method = "pdp+ice",
                        features = c("Petal.Length", "Petal.Width"),
                        features.2d = data.frame(feat1 = c("Petal.Length", "Petal.Length"),
                                                 feat2 = c("Sepal.Width", "Petal.Width")))
  expect_equal(names(combined_plot),
               c("Petal.Length", "Petal.Width",
                 "Petal.Length.Sepal.Width", "Petal.Length.Petal.Width"))

  context("Try ALE plotting")

  ale.plots <- plot(forest_interpret, method = "ale",
                    features = c("Petal.Length", "Petal.Width"))
  expect_equal(names(ale.plots), c("Petal.Length", "Petal.Width"))
  expect_equal(dim(forest_interpret$ale.grid$Petal.Length$ale), c(38, 3))
  expect_equal(dim(forest_interpret$ale.grid$Petal.Width$ale), c(26, 3))

  smooth_ale <- plot(forest_interpret,
                     features = c("Petal.Width"),
                     smooth = TRUE,
                     method = "ale",
                     smooth.binsize = 2,
                     smooth.type = "box",
                     smooth.npoints = 500)

  smooth_pdp <- plot(forest_interpret,
                     features = c("Petal.Width"),
                     smooth = TRUE,
                     smooth.type = "normal")

  x <- iris$Sepal.Length
  y <- iris$Sepal.Width


  rf <- forestry(x = data.frame(x=x
  ), y = y, seed = 101)

  pdta <- data.frame(x = x,
                     Y = y)

  p3 <- Predictor$new(model = rf,
                      data=pdta,
                      y="Y",
                      task = "regression")

  fi3 <- Interpreter$new(predictor=p3)


  context("Test plot with 1 feature")
  p <- plot(fi3,
       method="pdp",
       features="x")

  expect_equal(names(p), "x")

  rm(list=ls())


})

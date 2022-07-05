test_that("Tests that the plotting functions are working", {

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

  context("Check plotting method subroutines")
  # Check the set.center method
  set.center.at(forest_interpret, "CarapaceWidth", 2)
  expect_equal(forest_interpret$center.at$CarapaceWidth, 2)

  # Check the set.grid.points method
  set.grid.points(forest_interpret, "CarapaceWidth", c(1,2,3))
  expect_equal(forest_interpret$grid.points$CarapaceWidth,  c(1,2,3))

  # Check ICE plots, PDP plots, 2-D PDP plots and "save" feature
  ice.plots <- predict_ICE.Plotter(forest_interpret, features = "CarapaceWidth")
  expect_equal(dim(ice.plots$CarapaceWidth), c(3, 161))
  expect_equal(forest_interpret$saved$ICE$CarapaceWidth, ice.plots$CarapaceWidth)

  pdp.plots <- predict_PDP.1D.Plotter(forest_interpret, features = "CarapaceWidth")
  expect_equal(dim(pdp.plots$CarapaceWidth), c(3,2))
  expect_equal(forest_interpret$saved$PDP.1D$CarapaceWidth, pdp.plots$CarapaceWidth)

  pdp.2d.plots <- predict_PDP.2D.Plotter(forest_interpret,
                                         feat.2d = data.frame(feat1 = "CarapaceWidth",
                                                              feat2 = "RearWidth"))
  expect_equal(dim(pdp.2d.plots$`CarapaceWidth, RearWidth`), c(150, 3))
  expect_equal(forest_interpret$saved$PDP.2D$`CarapaceWidth, RearWidth`,
               pdp.2d.plots$`CarapaceWidth, RearWidth`)

  # Should delete previously done calculations
  set.grid.points(forest_interpret, "CarapaceWidth", c(1.5, 2.5, 3.5))
  expect_equal(all(is.na(forest_interpret$saved$ICE)), TRUE)
  expect_equal(all(is.na(forest_interpret$saved$PDP.1D)), TRUE)
  expect_equal(all(is.na(forest_interpret$saved$PDP.2D)), TRUE)

  # Check that save == F parameter works
  ice.plots <- predict_ICE.Plotter(forest_interpret,
                                   features = "CarapaceWidth",
                                   save = FALSE)
  expect_equal(dim(ice.plots$CarapaceWidth), c(3, 161))
  expect_equal(forest_interpret$saved$ICE$CarapaceWidth, NA)

  pdp.plots <- predict_PDP.1D.Plotter(forest_interpret,
                                      features = "CarapaceWidth",
                                      save = FALSE)
  expect_equal(dim(pdp.plots$CarapaceWidth), c(3,2))
  expect_equal(forest_interpret$saved$PDP.1D$CarapaceWidth, NA)

  pdp.2d.plots <- predict_PDP.2D.Plotter(forest_interpret,
                                         feat.2d = data.frame(feat1 = "CarapaceWidth",
                                                              feat2 = "RearWidth"),
                                         save = FALSE)
  expect_equal(dim(pdp.2d.plots$`CarapaceWidth, RearWidth`), c(150, 3))
  expect_equal(forest_interpret$saved$PDP.2D$`CarapaceWidth, RearWidth`,
               NA)



  # Initialize a plotter
  context("Try PDP plotting")

  forest_plot <- plot(forest_interpret, method = "pdp+ice",features = "FrontalLobe")
  expect_equal(names(forest_plot), "FrontalLobe")
  expect_equal(predict_ICE.Plotter(forest_interpret, features = "FrontalLobe")[[1]],
               forest_interpret$saved$ICE$FrontalLobe)
  expect_equal(predict_PDP.1D.Plotter(forest_interpret, features = "FrontalLobe")[[1]],
               forest_interpret$saved$PDP.1D$FrontalLobe)

  multiple_plot <- plot(forest_interpret, method = "pdp+ice",
                        features = c("FrontalLobe", "RearWidth"))
  expect_equal(names(multiple_plot), c("FrontalLobe", "RearWidth"))

  ice_plot <- plot(forest_interpret,
                   method = "ice",
                   features = "FrontalLobe",
                   clusters = 4,
                   clusterType = "preds")
  ice_plot <- plot(forest_interpret,
                   method = "ice",
                   features = "FrontalLobe",
                   clusters = 4,
                   clusterType = "gradient")
  expect_equal(names(ice_plot), "FrontalLobe")


  twodim_plot <- plot(forest_interpret,
                      features.2d = data.frame(feat1 = c("FrontalLobe", "FrontalLobe"),
                                               feat2 = c("Sex", "RearWidth")))
  expect_equal(names(twodim_plot), c("FrontalLobe.Sex", "FrontalLobe.RearWidth"))
  expect_equal(forest_interpret$saved$PDP.2D$`FrontalLobe, Sex`,
               predict_PDP.2D.Plotter(forest_interpret,
                                      feat.2d = data.frame(feat1 = c("FrontalLobe"),
                                                               feat2 = c("Sex")))[[1]])
  expect_equal(forest_interpret$saved$PDP.2D$`FrontalLobe, RearWidth`,
               predict_PDP.2D.Plotter(forest_interpret,
                                      feat.2d = data.frame(feat1 = c("FrontalLobe"),
                                                               feat2 = c("RearWidth")))[[1]])

  combined_plot <- plot(forest_interpret,
                        method = "pdp+ice",
                        features = c("FrontalLobe", "CarapaceWidth"),
                        features.2d = data.frame(feat1 = c("FrontalLobe", "FrontalLobe"),
                                                 feat2 = c("Sex", "RearWidth")))
  expect_equal(names(combined_plot),
               c("FrontalLobe", "CarapaceWidth",
                 "FrontalLobe.Sex", "FrontalLobe.RearWidth"))

  context("Try ALE plotting")

  ale.plots <- plot(forest_interpret, method = "ale",
                    features = c("FrontalLobe", "RearWidth"))
  expect_equal(names(ale.plots), c("FrontalLobe", "RearWidth"))
  expect_equal(dim(forest_interpret$ale.grid$FrontalLobe$ale), c(50, 3))
  expect_equal(dim(forest_interpret$ale.grid$RearWidth$ale), c(49, 3))

  smooth_ale <- plot(forest_interpret,
                     features = c("RearWidth"),
                     smooth = TRUE,
                     method = "ale",
                     smooth.binsize = 2,
                     smooth.type = "box",
                     smooth.npoints = 500)

  smooth_pdp <- plot(forest_interpret,
                     features = c("RearWidth"),
                     smooth = TRUE,
                     smooth.type = "normal")

  rm(list=ls())


})

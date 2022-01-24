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

  # Initialize a plotter
  forest_plot <- Plotter$new(forest_interpret, features = c("Frontal Lobe"),
                             features.2d = data.frame(col1 = c("Frontal Lobe", "Frontal Lobe"),
                                                      col2 = c("Species", "Sex")))

  expect_equal(forest_plot$features, "Frontal Lobe")

  expect_equal(forest_plot$features.2d[1,1], "Frontal Lobe")

  expect_equal(forest_plot$features.2d[1,2], "Species")

  plots <- plot(forest_plot)
  expect_equal(ncol(plots[[1]]$data), 4)
  expect_equal(nrow(plots[[1]]$data), 4830)


  context("Try centering function")

  old_center <- forest_plot$center.at$`Frontal Lobe`
  set.center.at(forest_plot, "Frontal Lobe", 2)
  new_center <- forest_plot$center.at$`Frontal Lobe`
  expect_gt(old_center, new_center)

  # Reset center
  set.center.at(forest_plot, "Frontal Lobe", 12)

  context("Try grid points function")

  old_pts <- forest_plot$grid.points$`Frontal Lobe`
  set.grid.points(forest_plot, "Frontal Lobe", seq(from  = 7, to = 23, length.out = 100))
  new_pts <- forest_plot$grid.points$`Frontal Lobe`
  expect_gt(length(new_pts), length(old_pts))
})

test_that("Tests if the PDP ranking function is working", {

  library(Rforestry)
  set.seed(491)

  data <-iris

  test_ind <- sample(1:nrow(data), nrow(data)%/%5)
  train_reg <- data[-test_ind,]
  test_reg <- data[test_ind,]

  # Train a random forest on the data set
  forest <- forestry(x=train_reg[,-c(1)],
                     y=train_reg[,1],
                     seed = 491)

  # Create a predictor wrapper for the forest
  forest_predictor <- Predictor$new(model = forest,
                                    data=train_reg,
                                    y="Sepal.Length",
                                    task = "regression")

  # Initialize an interpreter
  forest_interpret <- Interpreter$new(predictor = forest_predictor)

  context("Check PDP ranking methodologies without new observation")
  skip_if_not_mac()
  expect_equal(names(sort(pdp.rank(forest_interpret))),
               c("Sepal.Width", "Species", "Petal.Width", "Petal.Length"))

  skip_if_not_mac()
  expect_equal(names(sort(pdp.rank(forest_interpret, rank.method = "FO.Derivative"))),
               c("Species", "Sepal.Width", "Petal.Width", "Petal.Length"))

  context("Check PDP ranking methodologies with new observation")
  new.obs1 <- data[test_ind[1], -which(names(data)==forest_predictor$y)]

  skip_if_not_mac()
  expect_equal(names(sort(pdp.rank(forest_interpret, rank.method = 'Variance',
                                   pdp.weight.obs = new.obs1))),
               c("Species", "Sepal.Width", "Petal.Length", "Petal.Width" ))

  skip_if_not_mac()
  expect_equal(names(sort(pdp.rank(forest_interpret, rank.method = 'FO.Derivative',
                                   pdp.weight.obs = new.obs1))),
               c("Species", "Sepal.Width", "Petal.Length", "Petal.Width"))

  new.obs2 <- data[test_ind[2], -which(names(data)==forest_predictor$y)]

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, rank.method = 'Variance',
                            pdp.weight.obs = new.obs2)
                    - c(0.0032, 0.0366, 0.0181, 0)) < 0.001), TRUE)

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, rank.method = 'FO.Derivative',
                             pdp.weight.obs = new.obs2)
                    - c(0.1893, 0.6418, 0.4071, -1)) < 0.001), TRUE)

  context("Check weighted PDP ranking methodologies")
  skip_if_not_mac()
  expect_equal(names(sort(pdp.rank(forest_interpret, pdp.weight.obs = new.obs1,
                                   weight.pdp = TRUE))),
               c("Species", "Sepal.Width", "Petal.Length", "Petal.Width"))

  skip_if_not_mac()
  expect_equal(names(sort(pdp.rank(forest_interpret, rank.method = 'FO.Derivative', pdp.weight.obs = new.obs1,
                                   weight.pdp = TRUE))),
               c("Species", "Sepal.Width", "Petal.Length", "Petal.Width"))

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, pdp.weight.obs = new.obs1,
                             weight.pdp = TRUE)
                    - c(0.0003, 0.0004, 0.0015, 0)) < 0.001), TRUE)

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, pdp.weight.obs = new.obs2, rank.method = 'FO.Derivative',
                             weight.pdp = TRUE)
                    - c(0.1757, 0.6200, 0.4685, -1)) < 0.001), TRUE)

  context("Check PDP rankings subject to different quantiles")
  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, pdp.weight.obs = new.obs2, rank.method = 'FO.Derivative',
                             weight.pdp = TRUE, quantile.dist = 40)
                    - c(0.4277, 0.9170, 0.5441, -1)) < 0.001), TRUE)

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, pdp.weight.obs = new.obs2,
                             weight.pdp = TRUE, quantile.dist = 60)
                    - c( 0.0257, 0.1215, 0.0632, 0)) < 0.001), TRUE)


  context("Check PDP ranking stopping mechanisms")
  off.obs <- data[test_ind[1], -c(2)]
  two.obs <- data[test_ind[1:2], -which(names(data)==forest_predictor$y)]
  withy.obs <-  data[test_ind[1], ]
  linear <- lm(Sepal.Length ~ ., data = train_reg)
  linear_predictor <- Predictor$new(model = linear,
                                    data=train_reg,
                                    y="Sepal.Length",
                                    task = "regression")
  linear_interpret <- Interpreter$new(predictor = linear_predictor)

  samples_interpret <- Interpreter$new(predictor = forest_predictor, samples = 100)

  expect_error(pdp.rank(linear),
               "Object given is not of the interpreter class.",
               fixed = TRUE)
  expect_error(pdp.rank(forest_interpret, pdp.weight.obs = as.matrix(new.obs1)),
               "New Observation is not in valid form. Please convert pdp.weight.obs to a data frame.",
               fixed = TRUE)
  expect_error(pdp.rank(forest_interpret, pdp.weight.obs = two.obs),
               "Please reduce data frame to one row (i.e. one new observation).",
               fixed = TRUE)
  expect_error(pdp.rank(forest_interpret, pdp.weight.obs = new.obs1, weight.pdp = 100),
               "weight.pdp must be TRUE or FALSE.",
               fixed = TRUE)
  expect_error(pdp.rank(forest_interpret, pdp.weight.obs = new.obs1, quantile.dist = -100),
               "Please set quantile.dist to a value greater than or equal to 0.",
               fixed = TRUE)
  expect_error(pdp.rank(linear_interpret, pdp.weight.obs = new.obs1),
               "Weighted PDP option via new observation is not compatible with non-forestry objects.",
               fixed = TRUE)
  expect_error(pdp.rank(forest_interpret, pdp.weight.obs = withy.obs),
               "Please set pdp.weight.obs to the correct number of features that of the training data.",
               fixed = TRUE)
  expect_error(pdp.rank(forest_interpret, pdp.weight.obs = off.obs),
               "Please set the names of the pdp.weight.obs vector to that of the training data.",
               fixed = TRUE)
  expect_error(pdp.rank(samples_interpret),
               "Please set the \'samples\' parameter in the Interpreter object passed in as pdp.rank's \'object\' parameter as the number of rows in the train data.",
               fixed = TRUE)
})

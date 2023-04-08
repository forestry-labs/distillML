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
                                  new.obs = new.obs1))),
               c("Sepal.Width", "Species", "Petal.Width", "Petal.Length"))

  skip_if_not_mac()
  expect_equal(names(sort(pdp.rank(forest_interpret, rank.method = 'FO.Derivative',
                                  new.obs = new.obs1))),
               c("Species", "Sepal.Width", "Petal.Width", "Petal.Length"))

  new.obs2 <- data[test_ind[2], -which(names(data)==forest_predictor$y)]

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, rank.method = 'Variance',
                            new.obs = new.obs2)
                    - c(0.0257, 0.1215, 0.0632, 0.0325)) < 0.001), TRUE)

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, rank.method = 'FO.Derivative',
                             new.obs = new.obs2)
                    - c(0.1027, 0.2607, 0.2830, -1)) < 0.001), TRUE)

  ### START NEED CHANGE ###
  context("Check feature based PDP ranking methodologies")
  skip_if_not_mac()
  expect_equal(names(sort(pdp.rank(forest_interpret, new.obs = new.obs1,
                                   feature = 'Petal.Width'))),
               c("Sepal.Width", "Species", "Petal.Width", "Petal.Length"))

  skip_if_not_mac()
  expect_equal(names(sort(pdp.rank(forest_interpret, rank.method = 'FO.Derivative',
                                   new.obs = new.obs1, feature = 'Petal.Width'))),
               c("Species", "Sepal.Width", "Petal.Width", "Petal.Length"))

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, rank.method = 'Variance',
                             new.obs = new.obs2, feature = 'Species')
                    - c(0.0256, 0.1257, 0.0579, 0.0303)) < 0.001), TRUE)

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, rank.method = 'FO.Derivative',
                             new.obs = new.obs2, feature = 'Species')
                    - c(0.1048, 0.2755, 0.2711, -1)) < 0.001), TRUE)

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, rank.method = 'Variance',
                             new.obs = new.obs2, feature = 'Petal.Width')
                    - c(0.0280, 0.1328, 0.0726, 0.0354)) < 0.001), TRUE)

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, rank.method = 'FO.Derivative',
                             new.obs = new.obs2, feature = 'Petal.Width')
                    - c(0.1084, 0.2846, 0.2945, -1)) < 0.001), TRUE)

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, rank.method = 'Variance',
                             new.obs = new.obs2, feature = 'Petal.Width', qt = 20)
                    - c(0.0270, 0.1306, 0.0582, 0.0306)) < 0.001), TRUE)

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, rank.method = 'FO.Derivative',
                             new.obs = new.obs2, feature = 'Petal.Width', qt = 40)
                    - c(0.1077, 0.2860, 0.2593, -1)) < 0.001), TRUE)

  ### END NEED CHANGE ###

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
  expect_error(pdp.rank(forest_interpret, new.obs = as.matrix(new.obs1)),
               "New Observation is not in valid form. Please convert new.obs to a data frame.",
               fixed = TRUE)
  expect_error(pdp.rank(forest_interpret, new.obs = two.obs),
               "Please reduce data frame to one row (i.e. one new observation).",
               fixed = TRUE)
  expect_error(pdp.rank(linear_interpret, new.obs = new.obs1),
               "Weighted PDP option via new observation is not compatible with non-forestry objects.",
               fixed = TRUE)
  expect_error(pdp.rank(forest_interpret, new.obs = withy.obs),
               "Please set new.obs to the correct number of features that of the training data.",
               fixed = TRUE)
  expect_error(pdp.rank(forest_interpret, new.obs = off.obs),
               "Please set the names of the new.obs vector to that of the training data.",
               fixed = TRUE)
  expect_error(pdp.rank(forest_interpret, feature = 'Petal.Width'),
               "Feature based PDP ranking method requires a new observation.",
               fixed = TRUE)
  expect_error(pdp.rank(forest_interpret, new.obs = new.obs1, feature = 'species'),
               "Feature based PDP ranking method requires a valid feature.",
               fixed = TRUE)
  expect_error(pdp.rank(samples_interpret),
               "Please set the \'samples\' parameter in the Interpreter object passed in as pdp.rank's \'object\' parameter as the number of rows in the train data.",
               fixed = TRUE)
})

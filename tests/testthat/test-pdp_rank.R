test_that("Tests the PDP ranking functions are working", {

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
                     y=train_reg[,ncol(train_reg)], seed = 491)

  # Create a predictor wrapper for the forest
  forest_predictor <- Predictor$new(model = forest,
                                    data=train_reg,
                                    y="BodyDepth",
                                    task = "regression")

  # Initialize an interpreter
  forest_interpret <- Interpreter$new(predictor = forest_predictor)

  context("Check PDP ranking methodologies without new observation")
  skip_if_not_mac()
  expect_equal(names(sort(pdp.rank(forest_interpret))),
               c("Sex", "Index", "Species", "RearWidth", "CarapaceWidth",
                 "CarapaceLength", "FrontalLobe"))

  skip_if_not_mac()
  expect_equal(names(sort(pdp.rank(forest_interpret, rank.method = "FO.Derivative"))),
               c("Species", "Sex", "Index", "RearWidth", "FrontalLobe",
                 "CarapaceWidth", "CarapaceLength"))

  context("Check PDP ranking methodologies with new observation")
  new.obs1 <- data[test_ind[1], -which(names(data)==forest_predictor$y)]

  skip_if_not_mac()
  expect_equal(names(sort(pdp.rank(forest_interpret, rank.method = 'Variance',
                                  new.obs = new.obs1))),
               c("Sex", "Index", "Species" , "RearWidth", "CarapaceWidth",
                 "FrontalLobe", "CarapaceLength"))
  skip_if_not_mac()
  expect_equal(names(sort(pdp.rank(forest_interpret, rank.method = 'FO.Derivative',
                                  new.obs = new.obs1))),
               c("Species", "Sex", "Index", "RearWidth", "FrontalLobe",
                 "CarapaceWidth", "CarapaceLength"))

  new.obs2 <- data[test_ind[2], -which(names(data)==forest_predictor$y)]

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, rank.method = 'FO.Derivative',
                            new.obs = new.obs2)
                    - c(-1, -1, 0.1485, 0.6345, 0.3823, 0.8468, 0.7364)) < 0.001),
               TRUE)

  skip_if_not_mac()
  expect_equal(all((pdp.rank(forest_interpret, rank.method = 'Variance',
                            new.obs = new.obs2)
                    - c(0.0496, 0.0004, 0.0444, 1.3000, 0.1870, 1.2186, 0.6162)) < 0.001),
               TRUE)

  context("Check PDP ranking stopping mechanisms")
  off.obs <- data[test_ind[1], -c(1)]
  two.obs <- data[test_ind[1:2], -which(names(data)==forest_predictor$y)]
  withy.obs <-  data[test_ind[1], ]
  linear <- lm(BodyDepth ~ ., data = train_reg)
  linear_predictor <- Predictor$new(model = linear,
                                    data=train_reg,
                                    y="BodyDepth",
                                    task = "regression")
  linear_interpret <- Interpreter$new(predictor = linear_predictor)

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

})

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

  expect_equal(forest_predictor$model@ntree, 500)

  model_data <- forest_predictor$model@processed_dta$processed_x
  wrapper_data <- forest_predictor$data

  expect_equal(all.equal(names(model_data),
                         names(wrapper_data)[-8]), TRUE)

  context("Check the model in predictor")
  expect_equal(all.equal(forest_predictor$model@processed_dta$y[1:10],
                         forest@processed_dta$y[1:10],
                         tolerance = 1e-4),
               TRUE)


  context("Check predictions are equal ")
  wrapped_preds <- predict(forest_predictor, test_reg[,-ncol(test_reg)])
  standard_preds <- predict(forest, test_reg[,-ncol(test_reg)])

  expect_equal(all.equal(wrapped_preds[,1],
                         standard_preds,
                         tolerance = 1e-4),
               TRUE)

  context("Try passing parameters to the predict function")
  # Try passing parameters to the predict function
  wrapped_preds <- predict(forest_predictor, train_reg[,-ncol(train_reg)], aggregation = "oob")
  standard_preds <- predict(forest, train_reg[,-ncol(train_reg)], aggregation = "oob")

  expect_equal(all.equal(wrapped_preds[,1],
                         standard_preds,
                         tolerance = 1e-4),
               TRUE)

  context("Try giving the forest a custom prediction function")
  # Now try giving the forest a custom prediction function
  forest_predictor_debiased <- Predictor$new(model = forest,
                                             data=train_reg,
                                             predict.func = correctedPredict,
                                             y="Body Depth",
                                             task = "regression")

  wrapped_preds <- predict(forest_predictor, test_reg[,-ncol(test_reg)], nrounds = 0)
  standard_preds <- correctedPredict(forest, test_reg[,-ncol(test_reg)], nrounds = 0)


  context("Try a non forestry model")
  mod2 <- lm(Sepal.Length ~., data = iris[,-5])

  linear_predictor <- Predictor$new(model = mod2,
                                    data=mod2$model,
                                    y="Sepal.Length",
                                    task = "regression")

  preds_wrapped <- predict(linear_predictor, iris[,-c(1,5)])
  preds_std <- predict(mod2, iris[,-c(1,5)])

  expect_equal(all.equal(preds_wrapped[,1],
                         unname(preds_std)), TRUE)

  context("Try a classification model")
  mod <- suppressWarnings(glm(Species ~ ., data=iris, family = "binomial"))

  preds <- predict(mod, iris, type = "response")


  predictor <- Predictor$new(model = mod,
                             data = iris,
                             y = "Species",
                             task = "classification",
                             type = "response")

  expect_equal(all.equal(predict(predictor, iris)[,1],
              unname(predict(mod, iris, type = "response")),
              tolerance = 1e-4),
              TRUE)

  rm(list=ls())
})

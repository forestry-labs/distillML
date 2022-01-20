# Distillation For Machine Learning Models

This package provides several methods for model distillation and interpretability 
for general black box machine learning models.

## General Prediction Wrapper

The test data set we use to demonstrate this is the MASS Boston data set for 
regression tasks, and the crabs data set for classification tasks.
```
library(MASS)
library(interpret)

data <- MASS::Boston
set.seed(491)
test_ind <- sample(1:nrow(data), nrow(data)%/%5)
train_reg <- data[-test_ind,]
test_reg <- data[test_ind,]


data_class <- MASS::crabs
test_ind <- sample(1:nrow(data_class), nrow(data_class)%/%5)
train_class <- data_class[-test_ind,]
test_class <- data_class[test_ind,]

```

Try the same example with linear regression.

```
# linear regression
linreg <- lm(crim ~., data=train_reg)
linreg_predictor <- Predictor$new(model = linreg, data = train_reg, y="crim",
                                  predict = predict, task = "regression")
actual <- predict(linreg, test_reg)
method <- linreg_predictor$predict(test_reg) 
sum(actual != method) # all the same values

linreg_predictor$print()

linreginterpret <- Interpreter$new(predictor = linreg_predictor,
                                   feature = "indus")
linreginterpret$plot()
```

Try logistic regression.

```
# logistic regression
# Interpreter not yet implemented for classification (only for single variable probs)
logreg <- glm(sex ~., data=train_class, family=binomial(logit))
logreg_predictor <- Predictor$new(model=logreg, data=train_class, y="sex",
                                  type = "response", task = "classification")
actual <- predict(logreg, test_class, type = "response")
method <- logreg_predictor$predict(test_class)
sum(actual!= method)
head(method)
logreg_predictor$print()
```

Try decision trees.

```
# decision tree for regression
library(rpart)
dectree <- rpart(crim ~., data=train_reg, method="anova")

dectree_predictor <- Predictor$new(model = dectree, data = train_reg, y="crim",
                                  predict = predict, task = "regression")
actual <- predict(dectree, test_reg)
method <- dectree_predictor$predict(test_reg) 
sum(actual != method) # all the same values

dectree_predictor$print()

dectreeinterp <- Interpreter$new(predictor = dectree_predictor,
                                  feature = "dis")
dectreeinterp$plot()

```

```
# decision tree for classification
dectree <- rpart(sex ~., data=train_class, method = "class")
dectree_predictor <- Predictor$new(model = dectree, data = train_class, y="sex",
                                  type = "prob", task = "classification")
actual <- predict(dectree, test_class, type= "prob")
method <- dectree_predictor$predict(test_class) 
sum(actual != method) # all the same values

head(method)

dectree_predictor$print()
```

## Conditional PDP Plots

Given a categorical variable to group the observations by, we can decompose the 
partial dependence plot by the values of the grouping variable. 
This allows us to examine the predictions of the model at a level of granularity
that can be controlled by the chosen categorical variable.

```
# Showcase class variable
dectree_predictor <- Predictor$new(model = dectree, data = train_class, y="sex",
                                  type = "prob", task = "classification", class = "F")
method <- dectree_predictor$predict(test_class) 
head(method) # only shows female probabilites

dectreeinterp <- Interpreter$new(predictor = dectree_predictor,
                                  feature = "RW")
dectreeinterp$plot()
```


```
# random forest
library(Rforestry)
forest <- forestry(x=train_reg[,-which(names(train_reg)=="crim")],
                   y=train_reg[,which(names(train_reg)=="crim")])
forest_predictor <- Predictor$new(model = forest, data=train_reg, y="crim",
                                  task = "regression")
actual <- predict(forest, test_reg[,-which(names(test_reg)=="crim")])
method <- forest_predictor$predict(test_reg)

head(data.frame(actual,method)) # all the same values
forest_predictor$print()

forestinterp <- Interpreter$new(predictor = forest_predictor,
                                feature = "indus")
forestinterp$plot()

```


```
#Xgboost
library(xgboost)
# feeding in its own function
predict_xgboost <- function(model, newdata){
  return(predict(model, xgb.DMatrix(data=as.matrix(newdata))))
}

xgb_model <- xgboost(data = xgb.DMatrix(label = train_reg$crim,
                                        data = as.matrix(train_reg[,-which(names(train_reg)=="crim")])),
                     verbose = 0, nrounds = 100)

xgb_predictor <- Predictor$new(model = xgb_model, data=train_reg, 
                               predict.func = predict_xgboost, y="crim",
                               task = "regression")

actual <- predict(xgb_model, xgb.DMatrix(data=as.matrix(
  test_reg[,-which(names(test_reg)=="crim")])))
method <- xgb_predictor$predict(test_reg)

head(cbind(actual, method)) # same values

xgb_predictor$print()

xgbinterp <- Interpreter$new(predictor = xgb_predictor,
                             feature = "indus")
xgbinterp$plot()

```

## TODO
It would be nice to have in the future:
- ALE Plots
- PDP Plots
- PDP functions accessible
- A PDP surrogate model implemented
- Some sort of loneliness index implemented
- When the covariates of the observation are in the support of the training set, 
  create bootstrap confidence bands for the PDP predictions.


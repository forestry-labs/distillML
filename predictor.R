#' Predictor Object
#' @importFrom R6 R6Class

#' @title The class that wraps a machine learning model in order to provide a
#'  standardized method for predictions for different models.
#' @description Predictor class description
#' @field data The training data for the model
#' @field model The object model
#' @field task The prediction task the model is trained on
#' @field class The class of the model
#' @field prediction.function The prediction function used by the model.
#' @field batch.size The batch size for the model
#' @field y The name of the outcome feature in `data`
#' @examples
#' data <- MASS::Boston
#' set.seed(491)
#' test_ind <- sample(1:nrow(data), nrow(data)%/%5)
#' train_reg <- data[-test_ind,]
#' test_reg <- data[test_ind,]
#'
#'
#' data_class <- MASS::crabs
#' test_ind <- sample(1:nrow(data_class), nrow(data_class)%/%5)
#' train_class <- data_class[-test_ind,]
#' test_class <- data_class[test_ind,]
#'
#'
#' linreg <- lm(crim ~., data=train_reg)
#' linreg_predictor <- Predictor$new(model = linreg, data = train_reg, y="crim",
#'                                   predict = predict, task = "regression")
#' actual <- predict(linreg, test_reg)
#' method <- linreg_predictor$predict(test_reg)
#' sum(actual != method) # all the same values
#'
#' linreg_predictor$print()
#' @export
Predictor <- R6::R6Class("Predictor",
    public = list(
    data = NULL,
    model = NULL,
    task = NULL,
    class = NULL,
    prediction.function = NULL,
    batch.size = NULL,
    y = NULL,

    #' @param model A trained model.
    #' @param data A dataframe containing the training data.
    #' @param predict.func A function to run predictions for the model.
    #' @param y The name of the outcome variable in data.
    #' @param task The prediction class, either `classification` or `regression`
    #' @param class The class of predictions done.
    #' @param type The type of regression.
    #' @param batch.size The size of the batch used to create pdp grids
    #' @return A `Predictor` object.
    #' @note
    #' A wrapper to pass through an ML algorithm (rpart, etc.) through the
    #' interpretability functions, the data used to create the algorithm
    #'
    #' The necessary variables are model, data, y. The other variables are
    #' optional, and depend on the use cases. Type should be used only when
    #' a prediction function is NOT specified.
    #'
    #' The outputs of the algorithm must be the values if it is regression, or
    #' probabilities if classification. For classification problems with more than
    #' two categories, the output comes out as vectors of probabilities for each
    #' category. Because this is for ML interpretability, other types of
    #' predictions (ex: predictions that spit out the factor) are not allowed.
    initialize = function(model=NULL,
                          data=NULL,
                          predict.func=NULL,
                          y=NULL,
                          task = NULL,
                          class=NULL,
                          type=NULL,
                          batch.size = 1000) {

      # check that batch size is at least 1 and number
      checkmate::assert_number(batch.size, lower=1)

      # checks for model input
      if (is.null(model)){
        stop("Model not given.")
      }

      # takes in data, makes sure that it is a dataframe by the end or stops
      if (is.null(data)){
        stop("Data not given.")
        if (!inherits(data, "data.table") || !inherits(data, "data.frame")){
          stop("Data is not a data.frame or data.table object.")
        }
      if (inherits(data, "data.table")){
          data.table::setDF(data)
        }
      }

      # checks for valid y input
      if (is.null(y) || !is.character(y) || !(y%in%names(data))){
        stop("Y has not been given, is not a character variable, or is not a variable in the given data.")
      }

      # checks for valid task
      if (!is.null(task)){
        if (!(task %in% c("regression", "classification"))){
          stop("Only regression or classification tasks are supported.")
        }
      }

      #' prediction method must be constructed, with optional arguments of
      #' type and task
      if (is.null(predict.func)){
        if (is.null(type)){
          predict.func.final <- function(model, newdata) predict(model, newdata)
        } else{
          predict.func.final <- function(model, newdata) predict(model, newdata, type = type)
        }
      } else{
        predict.func.final <- function(model, newdata){
          pred <- do.call(predict.func, list(model, newdata = newdata))
          data.frame(pred, check.names = FALSE)
        }
      }

      self$data <- data
      self$class <- class
      self$model <- model
      self$task <- task
      self$prediction.function <- predict.func.final
      self$batch.size <- batch.size
      self$y <- y
    }
    )
)


#' @name predict.Predictor
#' @title Predict method for Predictor class
#' @description Gives predictions for a predictor object
#' @param object Predictor object to use.
#' @param newdata The dataframe to use for the predictions.
#' @export
predict.Predictor = function(object, newdata){
  # check that the new data is a dataframe
  checkmate::assert_data_frame(newdata)
  newdata <- as.data.frame(newdata)

  # drop Y variable to make sure that only features are used
  if (object$y %in% names(newdata)){
    newdata <- newdata[, -which(names(newdata) == object$y), drop = FALSE]
  }

  # if we predict 1 target
  preds <- object$prediction.function(object$model, newdata)
  preds <- data.frame(preds)

  # check for valid prediction
  #if (private$predictionCheck == FALSE){
    # number of rows in predictions
  #  if (nrow(preds)!= nrow(newdata)){
  #    stop("Number of predictions do not match the number of observations.")
  #  }
  #  # check for missing values
  #  if (sum(is.na(preds))!=0){
  #    stop("Predictions have missing values.")
  #  }
  #  private$predictionCheck <- TRUE
  #}

  # Class variable (if classification)
  if (!is.null(object$class) && ncol(preds) > 1){
    preds <- preds[, object$class, drop = FALSE]
  }
  rownames(preds) <- NULL
  return(preds)
}

#' @name print.Predictor
#' @title Printing method for Predictor class
#' @description Gives information for a predictor object
#' @param object The Predictor to print
#' @export
print.Predictor = function(
  object
){
  cat("Prediction Task:", object$task, "\n")
  if (object$task == "classification"){
    cat("Classes: ", paste(unique(object$data[,which(names(object$data)==object$y)]), collapse=" "))
  }
}


#' @importFrom R6 R6Class

#' @title Predictor class description
#' @description A wrapper class for generic ML algorithms (xgboost, RF, BART, rpart, etc.)
#' in order to standardize the predictions given by different algorithms to be
#' compatible with the interpretability functions.
#'
#' The necessary variables are model, data, y. The other variables are
#' optional, and depend on the use cases. Type should be used only when
#' a prediction function is NOT specified.
#'
#' The outputs of the algorithm must be the values if it is regression, or
#' probabilities if classification. For classification problems with more than
#' two categories, the output comes out as vectors of probabilities for the
#' specified "class" category. Because this is for ML interpretability,
#' other types of predictions (ex: predictions that spit out the factor) are not allowed.
#'
#' @field data The training data that was used during training for the model. This should
#'        be a data frame matching the data frame the model was given for training, which
#'        includes the label or outcome.
#' @field model The object corresponding to the trained model that we want to make a
#'        Predictor object for. If this model doesn't have a generic predict method,
#'        the user has to provide a custom predict function that accepts a data frame.
#' @field task The prediction task the model is trained to perform (`classification` or `regression`).
#' @field class The class for which we get predictions. We specify this to get the predictions
#'        (such as probabilites) for an observation being in a specific class (e.g. Male or Female).
#'        This parameter is necessary for classification predictions with more than a single vector
#'        of predictions.
#' @field prediction.function An optional parameter if the model doesn't have a
#'        generic prediction function. This should take a data frame and return a
#'        vector of predictions for each observation in the data frame.
#' @field y The name of the outcome feature in the `data` data frame.
#' @examples
#'
#' library(distillML)
#' library(Rforestry)
#' set.seed(491)
#' data <- iris
#'
#' test_ind <- sample(1:nrow(data), nrow(data)%/%5)
#' train_reg <- data[-test_ind,]
#' test_reg <- data[test_ind,]
#'
#'
#' forest <- forestry(x=data[,-1],
#'                    y=data[,1])
#'
#' forest_predictor <- Predictor$new(model = forest, data=train_reg,
#'                                   y="Sepal.Length", task = "regression")
#'
#' @export
Predictor <- R6::R6Class("Predictor",
    public = list(
    data = NULL,
    model = NULL,
    task = NULL,
    class = NULL,
    prediction.function = NULL,
    y = NULL,

    #' @param model The object corresponding to the trained model that we want to make a
    #'        Predictor object for. If this model doesn't have a generic predict method,
    #'        the user has to provide a custom predict function that accepts a data frame.
    #' @param data The training data that was used during training for the model. This should
    #'        be a data frame matching the data frame the model was given for training, including
    #'        the label or outcome.
    #' @param predict.func An optional parameter if the model doesn't have a
    #'        generic prediction function. This should take a data frame and return a
    #'        vector of predictions for each observation in the data frame.
    #' @param y The name of the outcome feature in the `data` data frame.
    #' @param task The prediction task the model is trained to perform (`classification` or `regression`).
    #' @param class The class for which we get predictions. We specify this to get the predictions
    #'        (such as probabilites) for an observation being in a specific class (e.g. Male or Female).
    #'        This parameter is necessary for classification predictions with more than a single vector
    #'        of predictions.
    #' @param type The type of predictions done (i.e. 'response' for predicted probabliities for classification).
    #'        This feature should only be used if no predict.func is specified.
    #' @return A `Predictor` object.
    #' @note
    #' The class that wraps a machine learning model in order to provide a
    #'  standardized method for predictions for different models.

    initialize = function(model=NULL,
                          data=NULL,
                          predict.func=NULL,
                          y=NULL,
                          task = NULL,
                          class=NULL,
                          type=NULL) {

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
      rownames(data) <- 1:nrow(data)

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

      #' prediction method must be constructed, with optional argument of type
      if (is.null(predict.func)){
        if (is.null(type)){
          predict.func.final <- function(model, newdata, ...) predict(model, newdata, ...)
        } else{
          predict.func.final <- function(model, newdata, ...) predict(model, newdata, type = type, ...)
        }
      } else{
        predict.func.final <- function(model, newdata, ...){
          preds <- do.call(predict.func, list(model, newdata = newdata, ...))
          data.frame(preds, check.names = FALSE)
        }
      }

      self$data <- data
      self$class <- class
      self$model <- model
      self$task <- task
      self$prediction.function <- predict.func.final
      self$y <- y
    }
    )
)


#' @name predict-Predictor
#' @rdname predict-Predictor
#' @title Predict method for Predictor class
#' @description Gives a single column of predictions from a model that
#'  is wrapped by the Predictor object
#' @param object The Predictor object to use to make predictions.
#' @param newdata The data frame to use for the independent features in the prediction.
#' @param ... Additional arguments that are passed to the model predict function.
#'   For instance, these can be different aggregation options (aggregation = "oob")
#'   that are accepted by the prediction function of the model.
#' @return A data frame with a single column containing the predictions for each
#'  row of the newdata data frame.
#' @export
predict.Predictor = function(object,
                             newdata,
                             ...){
  # check that the new data is a dataframe
  checkmate::assert_data_frame(newdata)
  newdata <- as.data.frame(newdata)

  # drop Y variable to make sure that only features are used
  if (object$y %in% names(newdata)){
    newdata <- newdata[, -which(names(newdata) == object$y), drop = FALSE]
  }

  # if we predict 1 target
  preds <- object$prediction.function(object$model, newdata, ...)
  preds <- data.frame(preds)

  # if we predict more than 1 class
  if (ncol(preds)>1){
    if (is.null(object$class)){
      stop("The predictions have more than one column. Please give a class variable
           to specify the column wanted.")
    }
    preds <- preds[, object$class, drop = FALSE]
  }
  rownames(preds) <- NULL
  return(preds)
}

#' @name print-Predictor
#' @rdname print-Predictor
#' @title The Printing method for Predictor class
#' @description Prints the task of an instance of the Predictor class.
#' @param x The Predictor object to print
#' @param ... Additional arguments passed to the print function.
#' @export
print.Predictor = function(
  x,
  ...
){
  cat("Prediction Task:", x$task, "\n")
  if (x$task == "classification"){
    cat("Classes: ", paste(unique(x$data[,which(names(x$data)==x$y)]), collapse=" "))
  }
}


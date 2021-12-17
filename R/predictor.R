#' Predictor Object
#'
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
#'
library(R6)
Predictor <- R6Class("Predictor",
    public = list(
    data = NULL,
    model = NULL,
    task = NULL,
    class = NULL,
    prediction.function = NULL,
    batch.size = NULL,
    y = NULL,

    initialize = function(model=NULL, data=NULL, predict.func=NULL,
                          y=NULL, task = NULL, class=NULL, type=NULL,
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
    },

    predict = function(newdata){
      # check that the new data is a dataframe
      checkmate::assert_data_frame(newdata)
      newdata <- as.data.frame(newdata)

      # drop Y variable to make sure that only features are used
      if (self$y %in% names(newdata)){
        newdata <- newdata[, -which(names(newdata) == self$y), drop = FALSE]
      }

      # if we predict 1 target
      preds <- self$prediction.function(self$model, newdata)
      preds <- data.frame(preds)

      # check for valid prediction
      if (private$predictionCheck == FALSE){
        # number of rows in predictions
        if (nrow(preds)!= nrow(newdata)){
          stop("Number of predictions do not match the number of observations.")
        }
        # check for missing values
        if (sum(is.na(preds))!=0){
          stop("Predictions have missing values.")
        }
        private$predictionCheck <- TRUE
      }

      # Class variable (if classification)
      if (!is.null(self$class) && ncol(preds) > 1){
        preds <- preds[, self$class, drop = FALSE]
      }
      rownames(preds) <- NULL
      return(preds)
    },

    print = function(){
      cat("Prediction Task:", self$task, "\n")
      if (self$task == "classification"){
        cat("Classes: ", paste(unique(self$data[,which(names(self$data)==self$y)]), collapse=" "))
      }
    }
    ),
    private = list(
      predictionCheck = FALSE
    )
    )


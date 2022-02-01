#' Predictor Object
#' @importFrom R6 R6Class

#' @title Predictor class description
#' @description The class that wraps a machine learning model in order to provide a
#'  standardized method for predictions for different models.
#' @field data The training data for building the model
#' @field model The object model
#' @field task The prediction task the model performs (i.e. classification or regression)
#' @field class The class for which we get predictions. We specify this to get the predictions
#'        (such as probabilites) for an observation being in a specific class (e.g. Male or Female).
#'        This parameter is necessary for classification predictions with more than a single vector
#'        of predictions.
#' @field prediction.function The prediction function used by the model
#' @field y The name of the outcome feature in `data`.
#' @examples
#' library(interpret)
#' library(Rforestry)
#' set.seed(491)
#' data <- MASS::crabs
#'
#' levels(data$sex) <- list(Male = "M", Female = "F")
#' levels(data$sp) <- list(Orange = "O", Blue = "B")
#' colnames(data) <- c("Species","Sex","Index","Frontal Lobe",
#' "Rear Width", "Carapace Length","Carapace Width","Body Depth")
#'
#' test_ind <- sample(1:nrow(data), nrow(data)%/%5)
#' train_reg <- data[-test_ind,]
#' test_reg <- data[test_ind,]
#'
#'
#' forest <- forestry(x=train_reg[,-which(names(train_reg)=="Carapace Width")],
#' y=train_reg[,which(names(train_reg)=="Carapace Width")])
#'
#' forest_predictor <- Predictor$new(model = forest, data=train_reg,
#' y="Carapace Width", task = "regression")
#'
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

    #' @param model A trained model.
    #' @param data A dataframe containing the training data.
    #' @param predict.func A function to run predictions for the model.
    #' @param y The name of the outcome variable in data.
    #' @param task The prediction class, either `classification` or `regression`
    #' @param class The class of outcomes we want to see predictions for.
    #' @param type The type of predictions done (i.e. 'response' for predicted probabliities for classification).
    #'        This feature should only be used if no predict.funcc is specified.
    #' @return A `Predictor` object.
    #' @note
    #' A wrapper to pass an ML algorithm (rpart, etc.) through the
    #' interpretability functions, the data used to create the algorithm
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
      self$y <- y
    }
    )
)


#' @name predict.Predictor
#' @title Predict method for Predictor class
#' @description Gives a single column of predictions for a predictor object
#' @param object Predictor object to use.
#' @param newdata The dataframe to use for the predictions.
#' @return A single column dataframe containing the predictions for all values
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

#' @name print.Predictor
#' @title Printing method for Predictor class
#' @description Gives the task of the given predictor object.
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


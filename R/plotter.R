#' @include predictor.R
#' @importFrom R6 R6Class

#' @title The class for different interpretability methods.
#' @description Interpreter class description
#' @field predictor The predictor object to use as a standardised wrapper for the model
#' @field data.points The indices of the data points used for the PDP/ALE. This
#' overwrites the "samples" initialization or the batch size variable from the predictor class.
#' @field functions.1d Functions giving average value across data.points with a given value and feature
#' @field functions.2d Functions giving 2D pdp functions
#' @field method The chosen interpretability method for the black-box model.
#' @examples
#' Needs to be rewritten
#' @export
Interpreter <- R6::R6Class(
  "Interpreter",
  public = list(
    predictor = NULL,
    features = NULL,
    data.points = NULL,
    functions.1d = NULL,
    functions.2d = NULL,
    method = NULL,
    # functions.ale = to come later

    # initialize an interpreter object
    #' @param predictor The trained predictor object for the model that we want to
    #'  interpret.
    #' @param samples The samples used for the PDP/ALE. Overwrites the batch size
    #' parameter from the predictor object.
    #' @param data.points The indices of the data points used for the PDP/ALE. This
    #' overwrites the "samples" parameter or the batch size from the predictor.
    #' @param method The chosen interpretability method for the black-box model.
    #' @return An `Interpreter` object.
    #' @note
    #' PDP and ICE Implementation
    initialize = function(predictor = NULL,
                          samples = NULL,
                          data.points = NULL,
                          method = "pdp") {
      # check to see if predictor is a valid predictor object
      if (is.null(predictor)) {
        stop("Predictor not given.")
      }
      if (!(inherits(predictor, "Predictor"))) {
        stop("Predictor given is not of the Predictor class.")
      }

      # determine valid features
      # remove y variable from consideration
      possible <-
        names(predictor$data)[names(predictor$data) != predictor$y]
      # find valid features in the data: more than 1 value
      possible <-
        names(which(sapply(
          sapply(predictor$data[, possible], unique), length
        ) > 1))
      # one of the following: int, factor, numeric
      possible <-
        possible[which(sapply(predictor$data[, possible], class) %in%
                         c("numeric", "factor", "integer"))]
      features <- possible


      # check to see if valid sample number
      # if data.points is not specified
      data <- predictor$data
      if (is.null(data.points)) {
        if (is.null(samples)) {
          # set samples to either batch size or whole dataset
          N <- predictor$batch.size
          N <- min(N, nrow(data))
          samples <- N
        } else {
          # get number of datapoints equal to samples
          checkmate::assert_numeric(samples)
          samples <- as.integer(samples)
          if (samples > nrow(samples) | samples < 1) {
            stop("Invalid sample number.")
          }
        }
        # set data.points to be worked with (# of datapoints = sample number)
        data.points <- sample(1:nrow(data), samples)
      }
      # if data points were specified
      else {
        checkmate::assert_numeric(data.points)
        if (any(!(data.points %in% 1:nrow(data)))) {
          stop("The data points are not in the training data.")
        }
      }

      # check that method is supported
      checkmate::assert_character(method)
      if (!(method %in% c("pdp", "ale"))) {
        stop("This method is not supported by the function.")
      }

      # create prediction functions for the features
      if (method == "pdp") {
        # for 1-dimensional functions
        pdp.function <- function(feature) {
          force(feature)
          pdp.function.2 <- function(val) {
            if (length(val) == 1){
              data <- predictor$data[data.points, , drop = FALSE]
              data[, feature] <- val
              mean(predict(predictor, data)[, 1])
            }
            else{
              return.vals <- c()
              for (v in val){
                data <- predictor$data[data.points, ,drop = FALSE]
                data[, feature] <- v
                return.vals <- c(return.vals, mean(predict(predictor, data)[, 1]))
              }
              return.vals
            }
          }
          return(pdp.function.2)
        }

        functions.pdp <- list()
        for (feature in features) {
          temp.list <- list(pdp.function(feature = feature))
          functions.pdp <- append(functions.pdp, temp.list)
        }
        names(functions.pdp) <- features

        # for 2d features
        pdp.function.2d <- function(feature.1, feature.2){
          force(feature.1)
          force(feature.2)
          pdp.function.2d.2 <- function(val.1, val.2){
            data <- predictor$data[data.points, , drop = FALSE]
            data[, feature.1] <- val.1
            data[, feature.2] <- val.2
            mean(predict(predictor, data)[,1])
          }
          return(pdp.function.2d.2)
        }
        functions.pdp.2d <- list()
        for (feature.1 in features){
          # list of functions for each
          temp.list <- list()
          for (feature.2 in features){
            if (feature.1 != feature.2){
              temp <- pdp.function.2d(feature.1 = feature.1, feature.2 = feature.2)
            }
            else {
              temp <- "Please use the one-dimensional functions."
            }
            temp.list <- append(temp.list, temp)
          }
          names(temp.list) <- features
          functions.pdp.2d <- append(functions.pdp.2d, list(temp.list))
        }
        names(functions.pdp.2d) <- features
      }
      else {
        stop("Other methods have not been implemented yet for general functions.")
      }

      # initialize all variables belonging to this class
      self$features <- features
      self$predictor <- predictor
      self$data.points <- data.points
      self$functions.1d <- functions.pdp
      self$functions.2d <- functions.pdp.2d
      self$method <- method
    }
  )
)

# allows user to set data.points
set.data.points = function(object, data.points) {
  checkmate::assert_numeric(data.points)
  if (!(inherits(object, "Interpreter"))) {
    stop("Object given is not of the Interpreter class.")
  }
  if (any(!(data.points %in% 1:nrow(object$predictor$data)))) {
    stop("The data points are not in the data.")
  }
  return(Interpreter$new(object$predictor,
                                object$features,
                                data.points = data.points,
                                object$method))
}

# allows user to set type of interpretability method
set.method = function(object, method){
  checkmate::assert_character(method)
  if (!(inherits(object, "Interpreter"))) {
    stop("Object given is not of the Interpreter class.")
  }
  if (!(method %in% c("pdp", "ale"))){
    stop("This method is not supported by the function.")
  }
  return(Interpreter$new(object$predictor,
                                object$features,
                                object$data.points,
                                method = method))
}


#' @include predictor.R
#' @importFrom R6 R6Class

#' @title Interpreter class description
#' @description The class for different interpretability methods.
#' @field predictor The predictor object to use as a standardized wrapper for the model
#' @field features All possible features that can be interpreted with respect to the model
#' @field data.points The indices of the data points in the training data used for the PDP/ALE.
#' @field functions.1d List of functions giving single feature interpretations of the model.
#' @field functions.2d List of functions giving two-feature interpretations of the model
#' @field method The chosen interpretability method for the black-box model ("pdp" or "ale").
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
#' forest_interpret <- Interpreter$new(predictor = forest_predictor)
#'
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
    #'                  interpret.
    #' @param samples The number of observations used for the interpretability
    #'                method. If no number is given, the default set is the
    #'                minimum between 1000 and the number of rows in the
    #'                training data set.
    #' @param data.points The indices of the data points used for the PDP/ALE. This
    #'                    overwrites the "samples" parameter above.
    #' @param method The chosen interpretability method for the black-box model.
    #'               By default, this method is set to partial dependence plots
    #'               ("pdp"). This can either be set to "pdp" or "ale".
    #' @return An `Interpreter` object.
    #' @note
    #' A wrapper to pass a predictor object (see: predictor.R) for interpreting
    #' its predictions with respect to one or two features. The two methods
    #' for interpreting a model based on one or two features are partial
    #' dependence plots (PDP), which averages over the marginal distribution, and
    #' accumulated local effects (ALE), which averages over the conditional
    #' distribution.
    #'
    #' The necessary variable is the predictor object. The other variables are
    #' optional, but it may be useful to specify the number of samples or the
    #' data.points if the training data is very large (reduces time for computations).
    #'
    #' For the output, the model returns an interpreter object with two lists of
    #' functions: one for interpreting a single feature's role in the black-box
    #' model, and the other for intepreting a pair of features' role in the
    #' black-box model. These interpretability functions are built for each
    #' possible feature (or pair of features). Each of these functions return
    #' a vector of averaged predictions equal in length to the number of
    #' values (or number of rows) input into the function.
    #'
    initialize = function(predictor = NULL,
                          samples = 1000,
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
        checkmate::assert_numeric(samples) # check that sample is numeric
        samples <- min(samples, nrow(data))
        if (samples > nrow(data) | samples < 1) {
          stop("Invalid sample number.")
        }
        samples <- as.integer(samples)
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
            return.vals <- c()
            for (v in val){
              data <- predictor$data[data.points, ,drop = FALSE]
              data[, feature] <- v
              return.vals <- c(return.vals, mean(predict(predictor, data)[, 1]))
            }
            return.vals
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
        # requires a matrix/dataframe with two columns for predictions
        pdp.function.2d <- function(feature.1, feature.2){
          force(feature.1)
          force(feature.2)
          pdp.function.2d.2 <- function(values){
            results <- c()
            data <- predictor$data[data.points, , drop = FALSE]
            for (i in 1:nrow(values)){
              data[, feature.1] <- values[i,1]
              data[, feature.2] <- values[i,2]
              results <- c(results, mean(predict(predictor, data)[,1]))
            }
            results
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
#' @name set.data.points
#' @title Modify the Training Data Used for Interpretability
#' @description Sets the data points used for interpretation
#' @param object The Interpreter object
#' @param data.points The training data indices used to set which observations
#'                    are used for interpretability
#' @export
set.data.points = function(object,
                           data.points)
{
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

# allows user to set data.points
#' @name set.method
#' @title Modify the Method Used for Interpretability
#' @description Sets a new method used for interpertability
#' @param object The Interpreter object
#' @param method The new method to generate the interpretability functions
#' @export
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


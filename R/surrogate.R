#' @include predictor.R
#' @include interpret.R
#' @include helper.R
#' @importFrom R6 R6Class
#' @importFrom stats predict
#' @import data.table
#' @import ggplot2
#' @import glmnet


#' @title Surrogate class description
#' @description The class for distilled surrogate models.
#' @field interpreter The interpreter object to use as a standardized wrapper for the model
#' @field weights The weights used to recombine the PDPs into a surrogate for the original model
#' @field intercept The intercept term we use for our predictions
#' @field feature.centers The center value for the features determined in the model
#' @field center.mean Boolean value that determines whether we use the mean-centered
#'                    data for our predictions
#' @field grid A list of PDPS that determine our prediction.
#' @field snap.grid Boolean that determines whether we use grid.points
#'
#'
#' @export

Surrogate <- R6::R6Class(
 "Surrogate",
 public = list(
   interpreter = NULL,
   weights = NULL,
   intercept = NULL,
   feature.centers = NULL,
   center.mean = NULL,
   grid = NULL,
   snap.grid = NULL,

   # initialize a surrogate object
   #' @param interpreter The interpreter object we want to build a surrogate model for.
   #' @param weights The weights for each given feature
   #' @param intercept The baseline value. If uncentered, this is 0, and if centered, this will be the mean of the predictions
   #'                  of the original model based on the grid points set.
   #' @param feature.centers The baseline value for the effect of each feature. If uncentered, this is 0.
   #' @param center.mean A boolean value that shows whether this model is a centered or uncentered model
   #' @param grid The values that we snap to if have snap.grid turned on
   #' @param snap.grid Boolean that determines if we use previously calculated values or re-predict using the functions.
   #' @return A surrogate model object that we can use for predictions
   #' @note Do not initalize this class on its own. It is automatically created by the distill function for the interpreter class.
   initialize = function(interpreter,
                         weights,
                         intercept,
                         feature.centers,
                         center.mean,
                         grid,
                         snap.grid){
     # check to see if valid interpreter
     if (!(inherits(interpreter, "Interpreter"))){
       stop("Interpreter given is not of the interpreter class.")
     }

     # check to see if valid weights (nonnegaative)
     if (any(length(weights)!= length(interpreter$features))){
       stop("Number of weights do not match number of features.")
     }
     if (any(names(weights)!= interpreter$features)){
       stop("Mismatch between feature names of weights.")
     }
     if (any(weights < 0)){
       stop("Weights cannot be negative.")
     }

     # if center.mean is off, then the intercept should be 0
     if (center.mean == 0 && intercept !=0){
       stop("Non-centered predictions should have no intercept term.")
     }

     # checks for feature.centers
     if (any(!(names(feature.centers) %in% interpreter$features))){
       stop("Feature centers are not the same as the features in the interpreter.")
     }

     if (center.mean == 0 & any(feature.centers != 0)){
       stop("Uncentered predictions should have no feature centers.")
     }

     # check grid
     if (any(names(grid)!=interpreter$features)){
       stop("Grid does not have the same features as the interpreter.")
     }

     self$interpreter <- interpreter
     self$weights <- weights
     self$intercept <- intercept
     self$feature.centers <- feature.centers
     self$center.mean <- center.mean
     self$grid <- grid
     self$snap.grid <- snap.grid
   }
 )
)

#' @name predict-Surrogate
#' @rdname predict-Surrogate
#' @title Prediction method for the distilled surrogate model
#' @description Predicts outputs given new data
#' @param object A surrogate object distilled from the interpreter
#' @param newdata The dataframe to use for the predictions
#' @export
#'
predict.Surrogate = function(object,
                             newdata,
                             ...){
  if (!(inherits(object, "Surrogate"))){
    stop("Object given is not of the surrogate class.")
  }
  # check that this is a valid dataframe
  checkmate::assert_data_frame(newdata)
  newdata <- as.data.frame(newdata)
  if (any(!(names(object$weights) %in% names(newdata)))){
    stop("Given data is missing at least one feature.")
  }

  # look at the subset of the data for which we have weights
  newdata <- newdata[, names(object$weights)]

  # two forms: centered and uncentered
  # if centered, then we need to center the current grid predictions
  # if uncentered, then we need not do anything
  preds <- data.frame(surrogate.preds = rep(object$intercept, nrow(newdata)))

  for (feature in names(object$weights)){
    # grid points for this specific feature
    ref <- object$grid[[feature]]
    if (object$snap.grid){
      # if snap.grid is T, then find the closest point in our existing grid points
      pred <- c()
      for (i in 1:length(newdata[,feature])){
        # find closest grid point for each i
        index <- which.min(abs(ref[,1]-newdata[i,feature])) # find the index of the closest grid point
        pred <- c(pred, ref[index,2]) # add grid point's value in PDP
      }
    }
    else{
      # if snap.grid is F, then we do not snap to the grid
      pred <- object$interpreter$functions.1d[[feature]](newdata[,feature])
    }

    pred <- pred - object$feature.centers[[feature]] # subtract mean of grid points
    preds <- preds + object$weights[[feature]]*pred
  }
  return(preds)
}


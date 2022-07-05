#' @include predictor.R
#' @include interpret.R
#' @importFrom R6 R6Class
#' @importFrom stats predict
#' @import ggplot2
#' @import glmnet
#' @import mltools


#' @title Surrogate class description
#' @description The class for distilled surrogate models.
#' @field interpreter The interpreter object to use as a standardized wrapper for the model
#' @field features The indices of the features in the data used in the surrogate model
#' @field weights The weights used to recombine the PDPs into a surrogate for the original model
#' @field intercept The intercept term we use for our predictions
#' @field feature.centers The center value for the features determined in the model
#' @field center.mean Boolean value that determines whether we use the mean-centered
#'                    data for our predictions
#' @field grid A list of PDPS that determine our prediction.
#' @field snap.grid Boolean that determines whether we use grid.points
#' @examples
#' \donttest{
#' library(distillML)
#' library(Rforestry)
#' set.seed(491)
#' data <- MASS::crabs
#'
#' levels(data$sex) <- list(Male = "M", Female = "F")
#' levels(data$sp) <- list(Orange = "O", Blue = "B")
#' colnames(data) <- c("Species","Sex","Index","Frontal Lobe",
#' "Rear Width", "Carapace Length","Carapace Width","Body Depth")
#'
#' test_ind <- sample(1:nrow(data), 180)
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
#' # Both initializations of a surrogate class result in the same surrogate model
#' surrogate.model <- distill(forest_interpret)
#' surrogate.model <- distill(forest_interpret,
#'                            center.mean = TRUE,
#'                            features = 1:length(forest_interpret$features),
#'                            cv = FALSE,
#'                            snap.grid = TRUE,
#'                            snap.train = TRUE)
#'
#'}
#' @export

Surrogate <- R6::R6Class(
 "Surrogate",
 public = list(
   interpreter = NULL,
   features = NULL,
   weights = NULL,
   intercept = NULL,
   feature.centers = NULL,
   center.mean = NULL,
   grid = NULL,
   snap.grid = NULL,

   # initialize a surrogate object
   #' @param interpreter The interpreter object we want to build a surrogate model for.
   #' @param features The indices of features in the training data used for the surrogate model
   #' @param weights The weights for each given feature after the surrogate model is fit.
   #' @param intercept The baseline value. If uncentered, this is 0, and if centered,
   #'                  this will be the mean of the predictions of the original model
   #'                  on the training data.
   #' @param feature.centers The baseline value for the effect of each feature. If uncentered, this is 0.
   #' @param center.mean A boolean value that shows whether this model is a centered or uncentered model
   #' @param grid A list of dataframes containing the pre-calculated values used to generate predictions
   #'             if snap.grid is TRUE
   #' @param snap.grid Boolean that determines if we use previously calculated values
   #'                  or re-predict using the functions.
   #' @return A surrogate model object that we can use for predictions
   #' @note Do not initalize this class on its own. It is automatically created
   #'       by the distill function for the interpreter class.
   initialize = function(interpreter,
                         features,
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
     if (any(weights < 0)){
       stop("Weights cannot be negative.")
     }

     # if center.mean is off, then the intercept should be 0
     if (center.mean == 0 && intercept !=0){
       stop("Non-centered predictions should have no intercept term.")
     }

     # checks for feature.centers
     if (center.mean == 0 & any(feature.centers != 0)){
       stop("Uncentered predictions should have no feature centers.")
     }

     self$interpreter <- interpreter
     self$features <- features
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
#' @param ... Additional parameters to pass to predict
#' @return A one-column dataframe of the surrogate model's predictions
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
  if (any(!(names(object$grid) %in% names(newdata)))){
    stop("Given data is missing at least one feature.")
  }

  # two forms: centered and uncentered
  # if centered, then we need to center the current grid predictions
  # if uncentered, then we need not do anything
  preds <- data.frame(surrogate.preds = rep(object$intercept, nrow(newdata)))

  for (feature in object$interpreter$features[object$features]){

    if (object$snap.grid){
      # if snap.grid is T, then find the closest point in our existing grid points
      # grid points for this specific feature
      ref <- object$grid[[feature]]
      pred <- c()
      for (i in 1:length(newdata[,feature])){

        # find closest grid point for each i
        if (inherits(newdata[,feature], "factor")){

          # if we have seen the factor before
          if (newdata[i,feature] %in% levels(object$interpreter$grid.points[[feature]])){
            index <- which(newdata[i, feature] == levels(object$interpreter$grid.points[[feature]]))
            pred <- c(pred, ref[index,2])
          }
          else{
            # factor we have not seen before
            pred <-  c(pred, object$feature.centers[[feature]])
          }
        }
        else{
          index <- which.min(abs(ref[,1]-newdata[i,feature]))
          pred <- c(pred, ref[index,2]) # add grid point's value in PDP
        }
      }
    }
    else{
      # if snap.grid is FALSE, then we do not snap to the grid
      pred <- object$interpreter$pdp.1d[[feature]](newdata[,feature])
    }

    pred <- pred - object$feature.centers[[feature]] # subtract mean of grid points

    if (object$interpreter$feat.class[[feature]] != "factor"){
      preds <- preds + object$weights[[feature]]*pred
    }
    else{
      hold <- paste(feature, newdata[, feature], sep = "_")
      coeffs <- object$weights[hold]
      hold[is.na(coeffs)] <-  0 # nothing done if grid does not have this value
      preds <- preds + coeffs * pred
    }
  }
  return(preds)
}


#' @include predictor.R
#' @include interpret.R
#' @importFrom R6 R6Class
#' @importFrom stats predict
#' @import ggplot2
#' @import glmnet

#' @name build.grid
#' @title Build grid used for weights in distilled surrogate model
#' @description A dataframe storing the true predictions and the PDP predictions
#' @param object The Interpreter object
#' @param feat.ind The indices of the features in the Interpreter's features that we want
#'                 to include as PDP functions in the distilled model.
#' @return A dataframe used to find weights in regression (one-hot encoding for
#'         categorical features)
#' @note
#' This function is mainly used as a subroutine for the distill function. We include this
#' as a public function to allow users to create their own weights and surrogate functions
#' outside of our implemented method.
#' @export
build.grid = function(object, feat.ind = 1:length(object$features)){

  if (!(inherits(object, "Interpreter"))){
    stop("Object given is not of the interpreter class.")
  }

  # we fit to the training data (or subsample of it)
  data <- object$predictor$data[object$data.points, ]
  y <- predict(object$predictor, data[, -which(names(data) == object$predictor$y)])

  # create PDP curves for these features
  pdps <- data.frame(sentinel = rep(NA, nrow(data)))
  for (feature in object$features[feat.ind]){
    pdps <- cbind(pdps, object$pdp.1d[[feature]](data[,feature]))
  }
  pdps <- pdps[,-1]
  pdps <- data.frame(pdps)
  colnames(pdps) <- object$features[feat.ind]

  grid <- cbind(pdps, y)
  return(grid)
}

#' @name distill
#' @title Builds surrogate model from an interpreter object based on the univariate
#'   PDP functions of the original model.
#' @description Builds a surrogate model from the PDP functions
#' @param object The Interpreter object
#' @param center.mean Boolean value that determines whether to center each column
#'                    of predictions by their respective means. Default is TRUE
#' @param features The indices of the features in the Interpreter's features that we want
#'                 to include as PDP functions in the distilled model.
#' @param snap.grid Boolean function that determines whether the model recalculates
#'                  each value predicted or uses an approximation from previous
#'                  calculations. When this parameter is set to TRUE, we approximate
#'                  the predicted values with prevoius calculations. Default is TRUE.
#' @param cv Boolean that indicates whether we want to cross-validate our fitted coefficients
#'           with a regularizer. This should only be done when regularizing coefficients.
#' @param snap.train Boolean that determines whether we use the training data or the
#'                   equally spaced grid points. By default, this is true, which means
#'                   we snap to grid points as determined by the training data's marginal
#'                   distribution.
#' @param params.glmnet Optional list of parameters to pass to glmnet while fitting
#'                      PDP curves to resemble the original predictions. By specifying
#'                      parameters, one can do lasso or ridge regression.
#' @param params.cv.glmnet Optional list of parameters to pass to cv.glmnet while fitting
#'                         PDP curves to resemble the original predictions. By specifying
#'                         parameters, one can do lasso or ridge regression.
#' @return A surrogate class object that can be used for predictions
#' @note
#' For further details, please refer to the vignette for this method, which includes
#' usage examples.
#' @export
distill = function(object,
                   center.mean = TRUE,
                   features = 1:length(object$features),
                   cv = FALSE,
                   snap.grid = TRUE,
                   snap.train = TRUE,
                   params.glmnet  = list(),
                   params.cv.glmnet = list()
                   ){

  if (max(features) > length(object$features) || min(features) < 1) {
    stop("features must be indices of features contained in the original training data set")
  }

  if (length(object$features) < 2 || length(object$features) < length(features) ) {
    stop("features must be of length 2 or greater")
  }

  # get data for grid
  data <- build.grid(object, feat.ind = features)

  # get snap.grid if enabled
  if (snap.grid){
    if (snap.train){
      train_data <- object$predictor$data[object$data.points, ]
      grid <- list()
      for (feat in object$features[features]){
        feat.data <- data.frame(cbind(train_data[, feat], data[[feat]]))
        names(feat.data) <- c("feature", "PDP")
        grid <- append(grid, list(feat.data))
      }
      names(grid) <- object$features[features]
    }
    else{
      grid <- predict_PDP.1D.Plotter(object, features = object$features[features])
    }
  }
  else{
    grid <- NA
  }


  # if centered, then remove col means and store original mean of predictions
  if (center.mean){
    feature.centers <- colMeans(data)[-ncol(data)]
    center <- mean(data$preds)
    for (i in 1:ncol(data)){
      data[,i] <- data[,i]-mean(data[,i]) # subtract each column by the mean
    }
  } else{
    center <- 0
    feature.centers <- rep(0, ncol(data)-1)
    names(feature.centers) <- names(data)[-ncol(data)]
  }

  # Use one-hot encoding for glmnet
  ref <- object$predictor$data[object$data.points,]
  fit.data <- data.frame(sentinel = rep(NA, nrow(data)))
  pdpnames <- c()

  for (feature in object$features[features]){
    # continuous variables
    if (object$feat.class[[feature]]!="factor"){
      fit.data <- cbind(fit.data, data[, feature])
      pdpnames <- c(pdpnames, feature)
    }
    # For categorical variable
    else{
      one_hot.names <- c()
      one_hot.pdp <- rep(NA, nrow(data))

      # Create a column for each value (one-hot encoding)
      for (val in object$grid.points[[feature]]){
        hold <- rep(0, nrow(data))
        hold[which(ref[,feature] == val)] <-
          data[which(ref[,feature] == val), feature]
        one_hot.pdp <- cbind(one_hot.pdp, hold)
        one_hot.names <- c(one_hot.names,
                           paste(feature, val, sep = "_"))
      }

      fit.data <- cbind(fit.data, one_hot.pdp[,-1])
      pdpnames <- c(pdpnames, one_hot.names)
    }
  }
  fit.data <- fit.data[,-1]
  names(fit.data) <- pdpnames

  # fit based on cross-validated
  if (cv == FALSE){
    # build parameter list for fitting with glmnet
    params.glmnet$x <- as.matrix(fit.data)
    params.glmnet$y <- data$preds

    # if no other parameters were specified
    if (length(params.glmnet)==2){
      params.glmnet$family <- "gaussian"
      params.glmnet$alpha <- 1
      params.glmnet$lambda <- 0
      params.glmnet$intercept <- FALSE
      params.glmnet$lower.limits <- 0
    }

    # get coefficients for each
    fit.model <- do.call(glmnet::glmnet, args = c(params.glmnet))
  }
  else{
    params.cv.glmnet$x <- as.matrix(fit.data)
    params.cv.glmnet$y <- data$preds

    # if no other parameters were specified
    if (length(params.cv.glmnet) == 2){
      params.cv.glmnet$lower.limits <- 0
      params.cv.glmnet$intercept <- FALSE
      params.cv.glmnet$alpha <- 1
    }
    # get coefficients for each
    fit.model <- do.call(glmnet::cv.glmnet, args = c(params.cv.glmnet))
  }

  coeffs<- as.vector(coef(fit.model))[-1] # to remove intercept term 0
  names(coeffs) <- colnames(fit.data)

  return(Surrogate$new(interpreter = object,
                       features = features,
                       weights = coeffs,
                       intercept = center,
                       feature.centers = feature.centers,
                       center.mean = center.mean,
                       grid = grid,
                       snap.grid = snap.grid))
}

#' @include predictor.R
#' @include interpret.R
#' @include helper.R
#' @importFrom R6 R6Class
#' @importFrom stats predict
#' @import data.table
#' @import ggplot2
#' @import glmnet

#' @name build.grid
#' @title Build grid used for weights in distilled surrogate model
#' @description A dataframe storing the true predictions and the PDP predictions
#' @param object The Interpreter object
#' @param save Boolean for saving the results in the interpreter object. Default is TRUE.
#' @param fit.train Boolean for indicating whether we fit to the subsampled training
#'                  data or to all possible combinations of the grid.points. Default
#'                  is TRUE, which means that we use the subsampled training data.
#' @return A dataframe used to find weights in regression (one-hot encoding for
#'         categorical features)
#' @export
build.grid = function(object, save = T, fit.train = T){

  if (!(inherits(object, "Interpreter"))){
    stop("Object given is not of the interpreter class.")
  }

  if (!(all(is.na(object$saved[["build.grid"]])))){
    return(object$saved[["build.grid"]])
  }

  # fit.train means that we fit to the training data (or subsample of it)
  if (fit.train){
    data <- object$predictor$data[object$data.points,]
    y <- predict(object$predictor, data[, -which(names(data) == object$predictor$y)])

    # create PDP curves for these features
    pdps <- data.frame(sentinel = rep(NA, nrow(data)))
    for (feature in object$features){
      pdps <- cbind(pdps, forest_interpret$functions.1d[[feature]](data[,feature]))
    }
    pdps <- pdps[,-1]
    pdps <- data.frame(pdps)
    colnames(pdps) <- object$features
  }

  else{
    # y represents the true predictions
    covars <- expand.grid(object$grid.points) # gets all possible grid point combinations
    y <- predict(object$predictor, covars)

    # build grid of PDP functions (PDP predictions are in the same order as the grid points)
    ref <- predict_PDP.1D.Plotter(object)
    ref.values <- list()
    for (feature in names(ref)){
      ref.values[[feature]] <- ref[[feature]][,2]
    }
    pdps <- expand.grid(ref.values)
  }

  grid <- cbind(pdps, y)

  # save and return
  if (save){
    object$saved[["build.grid"]] <- grid
  }
  return(grid)
}

#' @name distill
#' @title Build surrogate model from distilled model
#' @description Builds a surrogate model from the PDP functions
#' @param object The Interpreter object
#' @param center.mean Boolean value that determines whether to center each column
#'                    of predictions by their respective means. Default is TRUE
#' @param snap.grid Boolean function that determines whether the model recalculates
#'                  each value predicted or uses an approximation from previous
#'                  calculations. Default is TRUE.
#' @param fit.train Fit to training data or fit to expand.grid. If true, we fit to
#'                  the training data. Default is TRUE.
#' @param params.glmnet Optional list of parameters to pass to glmnet while fitting
#'                      PDP curves to resemble the original predictions. By specifying
#'                      parameters, one can do lasso or ridge regression.
#' @return A surrogate class object that can be used for predictions
#' @export
distill = function(object, center.mean = T, snap.grid = T, fit.train = T,
                   params.glmnet  = list()){

  # get data for grid
  data <- build.grid(object, fit.train = fit.train, save = T)

  # if centered, then remove col means and store original mean of predictions
  if (center.mean){
    feature.centers <- colMeans(data)[-ncol(data)]
    center <- mean(data$preds)
    for (i in 1:ncol(data)){
      data[,i] <- data[,i]-mean(data[,i]) # subtract each column by the mean
    }
  }
  else{
    center <- 0
    feature.centers <- rep(0, ncol(data)-1)
    names(feature.centers) <- names(data)[-ncol(data)]
  }

  # Use one-hot encoding for glmnet
  ref <- object$predictor$data[object$data.points,]
  fit.data <- data.frame(sentinel = rep(NA, nrow(data)))
  pdpnames <- c()

  for (feature in object$features){
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

  # build parameter list for fitting with glmnet
  params.glmnet$x <- as.matrix(fit.data)
  params.glmnet$y <- data$preds

  # if no other parameters were specified
  if (length(params.glmnet)==2){
    params.glmnet$family <- "gaussian"
    params.glmnet$alpha <- 1
    params.glmnet$lambda <- 0
    params.glmnet$intercept <- F
    params.glmnet$lower.limits <- 0
  }

  # get coeffiicients for each
  fit.model <- do.call(glmnet::glmnet, args = c(params.glmnet))
  coeffs<- as.vector(coef(fit.model))[-1] # to remove intercept term 0
  names(coeffs) <- colnames(fit.data)

  return(Surrogate$new(interpreter = object,
                       weights = coeffs,
                       intercept = center,
                       feature.centers = feature.centers,
                       center.mean = center.mean,
                       grid = predict_PDP.1D.Plotter(object),
                       snap.grid = snap.grid))
}

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
#' @param include.2d Boolean value that determines whether to include the 2-d PDP
#'                   functions in the grid. Default is FALSE.
#' @param save Boolean for saving the results in the interpreter object. Default is TRUE.
#' @param fit.train Boolean for indicating whether we fit to the subsampled training
#'                  data or to all possible combinations of the grid.points. Default
#'                  is TRUE.
#' @return A dataframe used to find weights in regression
#' @export
build.grid = function(object, include.2d = F, save = T, fit.train = T){

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
    covars <- expand.grid(object$grid.points) # gets all possiblle grid point combinations
    y <- predict(object$predictor, covars)

    # build grid of PDP functions (PDP predictions are in the same order as the grid points)
    ref <- predict_PDP.1D.Plotter(object)
    ref.values <- list()
    for (feature in names(ref)){
      ref.values[[feature]] <- ref[[feature]][,2]
    }
    pdps <- expand.grid(ref.values)

    if (include.2d){
      stop("2-D implementation will be added later.")
    }
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
#' @param include.2d Boolean value that determines whether to include the 2-d PDP
#'                   functions in the grid. Default is FALSE.
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
distill = function(object, include.2d = F, center.mean = T, snap.grid = T, fit.train = T,
                   params.glmnet  = list()){

  # get data for grid
  data <- build.grid(object, include.2d = include.2d, fit.train = fit.train, save = T)

  # if centered, then remove col means and store original mean of predictions
  if (center.mean){
    center <- mean(data$preds)
    for (i in 1:ncol(data)){
      data[,i] <- data[,i]-mean(data[,i]) # subtract each column by its mean
    }
  }
  else{
    center <- 0
  }

  # build parameter list for fitting with glmnet
  params.glmnet$x <- as.matrix(data[, -which(names(data)=="preds"), drop=F])
  params.glmnet$y <- data$preds

  # if no other parameters were specified
  if (length(params.glmnet)==2){
    params.glmnet$family <- "gaussian"
    params.glmnet$alpha <- 1
    params.glmnet$lambda <- 0
    params.glmnet$intercept <- F
    params.glmnet$lower.limits <- rep(0, length(object$features))
  }

  # get coeffiicients for each
  fit.model <- do.call(glmnet::glmnet, args = c(params.glmnet))
  coeffs<- as.vector(coef(fit.model))[-1] # to remove intercept term 0
  names(coeffs) <- object$features


  return(Surrogate$new(interpreter = object,
                       weights = coeffs,
                       intercept = center,
                       center.mean = center.mean,
                       grid = predict_PDP.1D.Plotter(object),
                       snap.grid = snap.grid))
}

#' @include predictor.R
#' @importFrom R6 R6Class

#' @title The class for different interpretability methods.
#' @description Interpreter class description
#' @field feature Feature to be examined in the model
#' @field n.features Number of features the model was trained on
#' @field predictor The predictor object to use as a standardised wrapper for the model
#' @field grid.points The points at which to run the predictions.
#' @field center.at The place we center the predictions at.
#' @field group A categorical group variable in order to stratify the PDP predictions by.
#' @field data.points Data points to use for the predictions.
#' @examples
#' data <- MASS::Boston
#' set.seed(491)
#' test_ind <- sample(1:nrow(data), nrow(data)%/%5)
#' train_reg <- data[-test_ind,]
#' test_reg <- data[test_ind,]
#'
#'
#' data_class <- MASS::crabs
#' test_ind <- sample(1:nrow(data_class), nrow(data_class)%/%5)
#' train_class <- data_class[-test_ind,]
#' test_class <- data_class[test_ind,]
#'
#'
#' linreg <- lm(crim ~., data=train_reg)
#' linreg_predictor <- Predictor$new(model = linreg, data = train_reg, y="crim",
#'                                   predict = predict, task = "regression")
#' actual <- predict(linreg, test_reg)
#' method <- linreg_predictor$predict(test_reg)
#' sum(actual != method) # all the same values
#'
#' linreg_predictor$print()
#'
#' linreginterpret <- Interpreter$new(predictor = linreg_predictor,
#'                                    feature = "indus")
#'
#' linreginterpret$plot()
#' @export
Interpreter <- R6::R6Class("Interpreter",
  public = list(
    feature = NULL,
    n.features = NULL, # for future implementation of two variable pdp's
    predictor = NULL,
    grid.points = NULL,
    center.at = NULL,
    group = NULL,
    data.points = NULL,

    # initialize an interpreter object
    #' @param predictor The trained predictor object for the model that we want to
    #'  interpret.
    #' @param feature The name of the feature that we use for the interpretability model.
    #' @param center.at The location we should use to center the predictions.
    #' @param grid.size The grid size used for the PDP.
    #' @param grid.points Number of grid points used in the PDP.
    #' @param group A group variable to classify the observations and give more
    #'   granular predictions.
    #' @param samples The samples used for the PDP/ALE.
    #' @return An `Interpreter` object.
    #' @note
    #' PDP and ICE Implementation
    #' similar methods
    #' Needed fixes: set predictions as a one time calculation, allow for specific
    #' points instead of number of points, add in second variable interpretations,
    #' add grouping by another variable
    initialize = function(predictor, feature, center.at=NULL,
                          grid.size=20, grid.points=NULL, group = NULL, samples = NULL){

      # check to see if predictor is a valid predictor object
      if (!(inherits(predictor, "Predictor"))){
        stop("Predictor given is not of the Predictor class.")
      }

      # check to see if feature is a valid feature (only numeric)
      if (length(feature) > 1){
        if (method=="ale"){
          stop("ALE interpretation methods only support 1 variable.")
        } else {
          stop("Only up to two features are allowed. Currently working on two feature implementation!")
        }
      }
      if (any(!(feature %in% names(predictor$data)))){
        stop("Feature(s) is/are not in data.")
      }
      if (any(feature == predictor$y)){
        stop("Feature selected is the output. Please input a valid feature.")
      }
      if (!(inherits(predictor$data[,feature], "numeric"))){
        stop("Feature is non-numeric. Please give a numeric feature.")
      }
      if (length(unique(predictor$data[,feature]))<2){
        stop("There is only a single value that occurs for this feature.")
      }

      # check to see if valid grid.points
      if (is.null(grid.points)){
        grid.points <- seq(min(predictor$data[,feature]),
                           max(predictor$data[,feature]),
                           length.out = grid.size)
      } else {
        checkmate::assert_numeric(grid.points)
        grid.points <- unique(grid.points)
        if (length(grid.points) < 2){
          stop("Please give at least two distinct grid points.")
        }
      }

      # check to see if valid center.at input
      checkmate::assert_numeric(center.at, null.ok = TRUE)
      if (is.null(center.at)){
          center.at <- min(grid.points)
      } else {
        if (center.at > max(self$grid.points) | center.at < min(self$grid.points)){
          stop("Centered point is not within boundaries of grid points.")
        }
      }

      # check to see if valid group, only allows for one group at the moment
      if (!is.null(group)){
        # check that the variable is in the list of variables
        if (group == predictor$y ||
            !(group %in% names(predictor$data))){
          stop("Invalid variable given for groups.")
        } else {
          if (class(data[, group]) != "factor"){
            stop("The grouping must be done by a factor variable.")
          }
        }
      }

      # check to see if valid sample number
      data <- predictor$data
      if (is.null(samples)){
        # set samples to either batch size or whole dataset
        N <- predictor$batch.size
        N <- min(N, nrow(data))
        samples <- N
      } else {
        # get number of datapoints equal to samples
        checkmate::assert_numeric(samples)
        samples <- as.integer(samples)
        if (samples > nrow(samples) | samples < 1){
          stop("Invalid sample number.")
        }
      }
      # set data.points to be worked with (# of datapoints = sample number)
      data.points <- sample(1:nrow(data), samples)

      # initialize all variables belonging to this class
      self$feature <- feature
      self$n.features <- length(feature)
      self$predictor <- predictor
      self$grid.points <- grid.points
      self$center.at <- center.at
      self$group <- group
      self$data.points <- data.points
    },

    # can go back and change the scale of the gridpoints
    #' @param grid.points The grid points to be used.
    #' @export
    set.gridpoints = function(grid.points){
      checkmate::assert_numeric(grid.points)
      grid.points <- unique(grid.points)
      if (length(grid.points) < 2){
        stop("Please give at least two distinct grid points.")
      }
      self$grid.points <- grid.points
    },

    # change center
    #' @param center The value to center the predictions at
    #' @export
    set.center = function(center = NULL){
      if (is.null(center)){
        self$center.at <- min(self$grid.points)
      } else {
        checkmate::assert_numeric(center)
        if (center > max(self$grid.points) | center < min(self$grid.points)){
          stop("Centered point is not within boundaries of grid points.")
        }
        self$center.at <- center
      }
    }
  )
)

#' @name predict_grid.Interpretor
#' @title # prediction function for grid points
#' @description Gives predictions at each point on the grid.
#' @param object The Interpretor object to use.
#' @export
predict_grid.Interpretor = function(
  object
){
  # get the data for the selected samples
  data.points <- object$data.points
  data <- object$predictor$data[data.points, , drop=FALSE]
  # make a table for results
  results <- data.frame(sentinel = rep(0, length(data.points)))
  for (val in object$grid.points){
    newdata <- data
    newdata[, object$feature] <- val
    #return((object$predictor$predict(newdata)))
    results <- cbind.data.frame(results,
                                val = (object$predictor$predict(newdata))[,1])
  }
  results <- data.frame(results)
  results <- results[,-1, drop = FALSE]
  colnames(results) <- object$grid.points

  # subtract out the centered value
  newdata <- data
  newdata[,object$feature] <- object$center.at
  base <- object$predictor$predict(newdata)

  # normalize results
  results <- results - base[,1]
  names.row <- rownames(data)

  # return transpose (easier for plotting)
  results <- (t(results))
  colnames(results) <- names.row
  results <- cbind(feature = object$grid.points, results)
  rownames(results) <- NULL

  return(data.frame(results))
}

#' @name plot.Interpretor
#' @title PLotting method for Interpretor model
#' @description Plots either the PDP plots or ICE plots
#' @param object Interpretor object to make plots for.
#' @param method The type of plot to make. Can be one of `ice`,`pdp`,`pdp+ice`.
#' @export
plot.Interpretor = function(
  object,
  method = "pdp+ice"
){
  # grab the predictions dataframe
  df <- object$predict_grid()
  # df contains both pdp line and all ice lines
  pdp.line <- rowMeans(df[,-which(colnames(df)=="feature")])
  df <- cbind(df ,pdp = pdp.line)
  df <- setDT(data.frame(df))
  df.ice <- df[, -"pdp"]

  # for scaling
  min.val <- min(df[,-"feature"])
  max.val <- max(df[,-"feature"])

  melt.df <- melt(df, id.vars = "feature")
  melt.df.ice <- melt(df.ice, id.vars="feature")

  # check to make sure that the method given is valid
  if (!is.null(object$group)){
    stop("Will implement this feature later.")
  }
  if (!(method %in% c("pdp", "ice", "pdp+ice"))){
    stop("Method entered is not supported")
  }

  if (method=="ice"){
    plot.obj <- ggplot(data=melt.df.ice, aes(x=feature, y=value, group=variable)) +
      geom_line(color="grey")
  }

  if (method == "pdp"){
    plot.obj <- ggplot(data=melt.df[melt.df$variable=="pdp",], aes(x=feature,y=value)) +
      geom_line()
  }

  if (method == "pdp+ice"){
    melt.df.combined <- melt.df
    melt.df.combined$ispdp <- (melt.df$variable=="pdp")
    plot.obj <- ggplot(data=melt.df.combined, aes(x=feature, y=value,
                                                  group=variable, color=ispdp)) +
      geom_line() + scale_color_manual(values=c("grey", "red"))
  }

  return(plot.obj+ ylab(object$predictor$y) + xlab(object$feature) )
}











#' @include predictor.R
#' @importFrom R6 R6Class
#'
#'
#'
#'
#' @title Interpreter
#' @rdname Interpreter
#' @param feature The feature to use for interpretation
#' @param n.features The number of features in the model
#' @param predictor A function to run predictions for the model.
#' @param grid.points The number of grid points to use
#' @param center.at The location to center the predictions with
#' @param group A vector of group categories.
#' @param data.points the data points to use in the interpretor.
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
    #' @rdname initialize.Interpreter
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
    #' @title set.gridpoints.Interpreter
    #' @rdname set.gridpoints.Interpreter
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
    #' @title set.center.Interpreter
    #' @rdname set.center.Interpreter
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
    },

    # prediction function for grid points
    #' @title predict_grid.Interpreter
    #' @rdname predict_grid.Interpreter
    #' @export
    predict_grid = function(){
      # get the data for the selected samples
      data.points <- self$data.points
      data <- self$predictor$data[data.points, , drop=FALSE]
      # make a table for results
      results <- data.frame(sentinel = rep(0, length(data.points)))
      for (val in self$grid.points){
        newdata <- data
        newdata[, self$feature] <- val
        #return((self$predictor$predict(newdata)))
        results <- cbind.data.frame(results,
                                    val = (self$predictor$predict(newdata))[,1])
      }
      results <- data.frame(results)
      results <- results[,-1, drop = FALSE]
      colnames(results) <- self$grid.points

      # subtract out the centered value
      newdata <- data
      newdata[,self$feature] <- self$center.at
      base <- self$predictor$predict(newdata)

      # normalize results
      results <- results - base[,1]
      names.row <- rownames(data)

      # return transpose (easier for plotting)
      results <- (t(results))
      colnames(results) <- names.row
      results <- cbind(feature = self$grid.points, results)
      rownames(results) <- NULL

      return(data.frame(results))
    },

    #' @title plot.Interpreter
    #' @rdname plot.Interpreter
    #' @param method The type of plot to make. Can be one of `ice`,`pdp`,`pdp+ice`.
    #' @export
    plot = function(method = "pdp+ice"){
      # grab the predictions dataframe
      df <- self$predict_grid()
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
      if (!is.null(self$group)){
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

      return(plot.obj+ ylab(self$predictor$y) + xlab(self$feature) )
    }
  )
)

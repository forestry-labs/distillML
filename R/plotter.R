#' @include predictor.R
#' @include interpret.R
#' @include helper.R
#' @importFrom R6 R6Class
#' @import data.table
#' @import ggplot2

#' @title The class for interpretability plots
#'
Plotter <- R6::R6Class(
  "Plotter",
  public = list(
    interpreter = NULL,
    features = NULL,
    features.2d = NULL,
    center.at = NULL,
    grid.points = NULL,

    #initialize a plotter object
    # features is a simple vector of all features we want to analyze, 1d
    # features 2d is a matrix with two columns
    initialize = function(interpreter=NULL,
                          features=NULL,
                          features.2d = NULL,
                          grid.size = 20){
      # checks for interpreter class
      if (is.null(interpreter)){
        stop("Interpreter for plotting not given.")
      }
      if (!(inherits(interpreter, "Interpreter"))){
        stop("Interpreter given is not of the interpreter class.")
      }

      # checks for feature
      if (is.null(features) & is.null(features.2d)){
        stop("Please give either one or two dimensional features to plot.")
      }
      if (any(!(features %in% interpreter$features))) {
        stop("Feature(s) is/are not in the interpretability object.")
      }
      # checks for 2d features
      if (!(is.null(features.2d))){
        if (any(!(features %in% interpreter$features))){
          stop("Features are not in the interpretability object.")
        }
        # check that it has at least one continuous variable
        data <- interpreter$predictor$data[interpreter$data.points,]
        for (i in 1:nrow(features.2d)){
          if (class(data[,features.2d[i,1]]) != "numeric"  &
              class(data[,features.2d[i,2]]) != "numeric"){
            stop("At least one feature in each pair must be a numeric variable.")
          }
        }
      }
      # check to see if valid grid.size
      checkmate::assert_numeric(grid.size)
      grid.size <- as.integer(grid.size)
      if (grid.size < 2){
        stop("Please enter a valid grid size (>=2) to generate the grid.")
      }

      # generate grid.points
      total.features <- unique(c((as.vector(t(features.2d))), features))
      data <- interpreter$predictor$data[interpreter$data.points,]
      grid.points <- list()
      for (feature in total.features){
        if (class(data[,feature]) == "numeric"){
          temp.list <- list(seq(min(data[,feature]),max(data[,feature]),length.out = grid.size))
        }
        else{
          temp.list <- list(unique(data[,feature]))
        }
        grid.points <- append(grid.points, temp.list)
      }
      names(grid.points) <- total.features


      # center.at is a list of centers for each variable, initialized to the min
      center.at <- list()
      for (i in 1:length(total.features)){
        if (class(data[,total.features[i]]) %in% c("numeric", "integer")){
          temp.list <- list(min(grid.points[[i]]))
        }
        else{
          temp.list <- list(grid.points[[i]][1])
        }
        center.at <- append(center.at, temp.list)
      }
      names(center.at) <- total.features

      # intialize all variables
      self$interpreter <- interpreter
      self$features <- features
      self$features.2d <- features.2d
      self$center.at <- center.at
      self$grid.points <- grid.points
    }
  )
)

# method for setting center value for a specific feature
set.center.at = function(object, feature, value){
  if (!(inherits(object, "Plotter"))){
    stop("Plotter given is not of the plotter class.")
  }
  if (!(feature %in% object$features)){
    stop("Feature given is not in the feature list.")
  }
  # valid feature index
  index <- which(names(object$center.at) == feature)

  if (class(value) == "numeric"){
    if (class(value) != class(object$interpreter$predictor$data[,feature])){
      stop("Invalid value for the given feature.")
    }
  }
  if (class(value) %in% c("factor", "integer")){
    if (!(value %in% object$grid.points[[index]])){
      stop("Invalid value for the given feature.")
    }
  }
  object$center.at[[index]] <- value
}

# method for setting grid points for a specific feature
set.grid.points = function(object, feature, values){
  if (!(inherits(object, "Plotter"))){
    stop("Plotter given is not of the plotter class.")
  }
  if (!(feature %in% object$features)){
    stop("Feature given is not in the feature list.")
  }
  # valid feature index
  index <- which(names(object$grid.points) == feature)

  if (length(values) < 2){
    stop("Requires at least 2 distinct values.")
  }
  if (class(values) == "numeric"){
    if (class(values) != class(object$interpreter$predictor$data[,feature])){
      stop("Invalid value for the given feature.")
    }
  }
  if (class(values) %in% c("factor", "integer")){
    if (any(!(values %in% object$interpreter$predictor$data[,feature]))){
      stop("Invalid value for the given feature.")
    }
  }
  # check that center.at value is in the new set of values
  if (class(values) == "numeric"){
    if (object$center.at[[index]] > max(values) || object$center.at[[index]] < min(values)){
      stop("The value for centering is not included in the new values.")
    }
  }
  else{
    if (!(object$center.at[[index]] %in% values)){
      stop("The value for centering is not included in the new values.")
    }
  }
  object$grid.points[[index]] <- values
}


#' @name predict_grid.Interpretor
#' @title # prediction function for grid points
#' @description Gives predictions at each point on the grid.
#' @param object The Plotter object to use.
#'
#' Needed fixes: set predictions as a one time calculation,
#' add in second variable interpretations,
#' add grouping by another variable
#' @export
predict_grid.Plotter = function(object) {
  grid.predictions <- list()

  # get the data for the selected samples
  data <- object$interpreter$predictor$data[object$interpreter$data.points,]

  # make grid predictions for each variable listed
  for (feature in object$features){
    results <- data.frame(sentinel = rep(0, length(object$interpreter$data.points)))
    index <- which(names(object$grid.points) == feature)
    for (val in object$grid.points[[index]]) {
      newdata <- data
      newdata[, feature] <- val
      results <- cbind.data.frame(results,
                                  val = predict(object$interpreter$predictor, newdata)[, 1])
    }
    results <- data.frame(results)
    results <- results[, -1, drop = FALSE]
    colnames(results) <- object$grid.points[[index]]

    # subtract out the centered value
    newdata <- data
    newdata[, feature] <- object$center.at[[index]]
    base <- predict(object$interpreter$predictor, newdata)

    # normalize results
    results <- results - base[, 1]
    names.row <- rownames(data)

    # return transpose (easier for plotting)
    results <- (t(results))
    colnames(results) <- names.row
    results <- cbind(feature = object$grid.points[[index]], results)
    rownames(results) <- NULL

    grid.predictions <- append(grid.predictions, list(data.frame(results)))
  }
  names(grid.predictions) <- object$features
  return(grid.predictions) # returns a list of grid predictions for plotting
}

# helper function for plotter
class.definer = function(features.2d, data){
  # every feature that appears
  names.features <- unique(as.vector(t(features.2d)))
  classes <- c()
  for (feature in names.features){
    classes <- c(classes, class(data[,feature]))
  }
  names(classes) <- names.features
  return(classes)
}

#' @name plot.Interpretor
#' @title PLotting method for Interpretor model
#' @description Plots either the PDP plots or ICE plots
#' @param object Plotter object to make plots for.
#' @param method The type of plot to make. Can be one of `ice`,`pdp`,`pdp+ice`.
#' @export
plot.Plotter = function(object, method = "pdp+ice") {
  if (!(inherits(object, "Plotter"))){
    stop("Plotter given is not of the plotter class.")
  }
  # list of plots to be filled
  plots <- list()
  # for 1-D plots
  if (!(is.null(object$features))){
    # get object for plotting
    df_all <- predict_grid.Plotter(object)
    for (feature in object$features){
      index <- which(object$features == feature)
      df <- df_all[[index]]
      # df contains both pdp line and all ice lines
      pdp.line <- rowMeans(df[, -which(colnames(df) == "feature")])
      df <- cbind(df , pdp = pdp.line)
      df <- setDT(data.frame(df))
      df.ice <- df[,-"pdp"]

      # for scaling
      min.val <- min(df[, -"feature"])
      max.val <- max(df[, -"feature"])

      melt.df <- melt(df, id.vars = "feature")
      melt.df.ice <- melt(df.ice, id.vars = "feature")

      # check to make sure that the method given is valid
      if (!(method %in% c("pdp", "ice", "pdp+ice"))) {
        stop("Method entered is not supported")
      }

      if (method == "ice") {
        plot.obj <-
          ggplot(data = melt.df.ice, aes(x = feature, y = value, group = variable)) +
          geom_line(color = "grey")
      }

      if (method == "pdp") {
        plot.obj <-
          ggplot(data = melt.df[melt.df$variable == "pdp", ], aes(x = feature, y =
                                                                    value)) +
          geom_line()
      }

      if (method == "pdp+ice") {
        melt.df.combined <- melt.df
        melt.df.combined$ispdp <- (melt.df$variable == "pdp")
        plot.obj <-
          ggplot(data = melt.df.combined,
                 aes(
                   x = feature,
                   y = value,
                   group = variable,
                   color = ispdp
                 )) +
          geom_line() + scale_color_manual(values = c("grey", "red"))
      }
      plots <- append(plots, list(plot.obj + ylab(object$interpreter$predictor$y) + xlab(feature)))
    }
    names(plots) <- object$features
  }

  # for 2-D plots (some is redundant, quick fix)
  if (!(is.null(object$features.2d))){
    features.2d <- object$features.2d
    feature.classes <- class.definer(features.2d = features.2d,
                                     data = object$interpreter$predictor$data)
    # for the names in each function
    names.2d <- c()
    for (i in 1:nrow(features.2d)){
      # heatmap for 2 continuous features
      if (feature.classes[features.2d[i,1]] == "numeric" &&
          feature.classes[features.2d[i,2]]=="numeric"){
        feature.1 <- features.2d[i,1]
        feature.2 <- features.2d[i,2]

        vals.1 <- object$grid.points[[feature.1]]
        vals.2 <- object$grid.points[[feature.2]]

        # create a grid point of values
        values <- expand.grid(vals.1, vals.2)
        predictions <- c()
        for (j in 1:nrow(values)){
          prediction <- object$interpreter$functions.2d[[feature.1]][[feature.2]](values[j,1], values[j,2])
          predictions <- c(predictions, prediction)
        }
        values <- cbind(values, predictions)
        values <- data.frame(values)
        names(values) <- c("Feat.1", "Feat.2", "Val")

        # still need to relabel axis
        plot.obj <- ggplot(values, aes(x=Feat.1, y=Feat.2, fill = Val)) +
          geom_tile() + xlab(feature.1) + ylab(feature.2)
        plot.obj <- plot.obj + guides(fill=guide_legend(title = object$interpreter$predictor$y))
      }
      else {
        # find the categorical feature and continuous feature
        categorical <- NULL
        continuous <- NULL
        if (feature.classes[features.2d[i,1]] == "numeric"){
          continuous <- features.2d[i,1]
          categorical <- features.2d[i,2]
        }
        else {
          continuous <- features.2d[i,2]
          categorical <- features.2d[i,1]
        }

        # pull grid values
        vals.cont <- object$grid.points[[continuous]]
        vals.cat <- object$grid.points[[categorical]]

        # generate predictions for each level
        values <- expand.grid(vals.cont, vals.cat)
        predictions <- c()
        for (j in 1:nrow(values)){
          prediction <- object$interpreter$functions.2d[[continuous]][[categorical]](values[j,1], values[j,2])
          predictions <- c(predictions, prediction)
        }
        values <- cbind(values, predictions)
        values <- data.frame(values)
        names(values) <- c("Cont", "Cat", "Val")

        plot.obj <- ggplot(values, aes(x=Cont, y=Val, group=Cat, color=Cat)) +
          geom_line() + xlab(continuous) + ylab(object$interpreter$predictor$y)
        plot.obj <- plot.obj +  guides(color=guide_legend(title = categorical))
      }
      plots <- append(plots, list(plot.obj))
      names.2d <- c(names.2d, paste(features.2d[i,1], features.2d[i,2], sep = "."))
    }
    names(plots) <- c(object$features, names.2d)
  }
  return(plots)
}

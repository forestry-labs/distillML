#' @include predictor.R
#' @include interpret.R
#' @include helper.R
#' @importFrom R6 R6Class
#' @import data.table
#' @import ggplot2

#' @title Plotter class description
#' @description The class for interpretability plots and groups
#' @field interpreter The interpreter object to use to create the plots.
#' @field features The set of features to create 1-d plots for.
#' @field features.2d Pairs of features to create 2-d plots for.
#' @field center.at The value(s) to center the feature plots
#' @field grid.points A list of vectors containing the grid points to use for
#'                    the predictions.
#' @examples
#' library(interpret)
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
#' forest_plot <- Plotter$new(forest_interpret, features = c("Frontal Lobe"),
#'                           features.2d = data.frame(col1 = c("Frontal Lobe", "Frontal Lobe", "Frontal Lobe"),
#'                                                    col2 = c("Species", "Sex", "Rear Width")))
#' plots <- plot(forest_plot)
#' @export
Plotter <- R6::R6Class(
  "Plotter",
  public = list(
    interpreter = NULL,
    features = NULL,
    features.2d = NULL,
    center.at = NULL,
    grid.points = NULL,


    #' @description Initialize a plotter object
    #' @param interpreter Interpreter object to make plots for.
    #' @param features A vector of features used to make 1-d plots.
    #' @param features.2d A matrix or data frame containing two columns of
    #'  feature names to give pairs of features for the 2-d plots.
    #' @param grid.size The number of grid points to create for the pdp
    #'  functions / plots for each feature.
    #' @return A 'Plotter' object
    #' @note
    #' A wrapper to pass an interpreter object (see: interpret.R) for visualizing
    #' the interpretation methods used in the interpreter object. This creates
    #' plots for both 1-d and 2-d plots. For 2-d plots of one continuous and
    #' one categorical variable, we use linear plots with different shades representing
    #' the categorical variable. For 2-d plots of two continuous variables, there
    #' exists multiple options, including heatmap methods.
    #'
    #' The necessary variables are the interpreter object and either the vector/list of
    #' features for 1-d plots or the matrix / dataframe for 2-d plots.
    #'
    #' For the output, this model returns a plotter object that can then be plotted to
    #' get the visualizations of the data using the 'plot()' function.
    #' @export
    initialize = function(interpreter=NULL,
                          features=NULL,
                          features.2d = NULL,
                          grid.size = 30){
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


#' @name set.center.at
#' @title Sets the center value used for plotting
#' @description Method for setting center value for a specific feature
#' @param object The Plotter class to recenter
#' @param feature The feature to center
#' @param value The new center value to use for the feature.
#' @export
set.center.at = function(object,
                         feature,
                         value)
{
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
#' @name set.grid.points
#' @title Sets the center value used for plotting
#' @description Method for setting center value for a specific feature
#' @param object The Plotter class to recenter
#' @param feature The feature to center
#' @param values The set of new values to be used as the grid points for the selected feature
#' @export
set.grid.points = function(object,
                           feature,
                           values)
{
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


#' @name predict_grid.Plotter
#' @title Prediction Function for ICE Plots
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

# helper function for plotter (defines class for each feature)
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

#' @name plot.Plotter
#' @title PLotting method for Interpretor model
#' @description Plots either the PDP plots or ICE plots
#' @param object Plotter object to make plots for.
#' @param method The type of plot to make. Can be one of `ice`,`pdp`,`pdp+ice`.
#' @export
plot.Plotter = function(object,
                        method = "pdp+ice")
{
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
          geom_line() +
          scale_color_manual(labels = c("ICE", "PDP") ,values = c("grey", "red")) +
          guides(color=guide_legend(title = "Plot Type"))
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

#' @name localSurrogate
#' @title Given a plotter object with at least one pair of features,
#'   create a small surrogate model for the two features using the PDP function
#'   as the output and the two features as the independent variables.
#' @description Plots and returns a Rforestry object with a single tree explaining
#'   the PDP surface.
#' @param object Plotter object to make plots + surrogate for.
#' @param interact An indicator specifying if the surrogate model should also be
#'   given the interaction terms to create the surrogate models with.
#'   Default is FALSE.
#' @param params.forestry Optional list of parameters to pass to the surrogate
#'   model. Defaults to the standard Rforestry parameters with ntree = 1
#'   and maxDepth = 2.
#' @export
localSurrogate = function(object,
                          interact = FALSE,
                          params.forestry = list())
{

  if (!(inherits(object, "Plotter"))){
    stop("Plotter given is not of the plotter class.")
  }

  if ((is.null(object$features.2d))) {
    stop("Plotter object must have a set of 2d features to create a surrogate model")
  }

  # If no forestry params given, default to maxDepth = 2 and ntree = 1
  if (length(params.forestry) == 0) {
    params.forestry$maxDepth = 2
    params.forestry$ntree = 1
  }

  # list of plots to be filled
  plots <- list()
  surrogates <- list()

  features.2d <- object$features.2d
  feature.classes <- class.definer(features.2d = features.2d,
                                   data = object$interpreter$predictor$data)
  # for the names in each function
  names.2d <- c()
  for (i in 1:nrow(features.2d)){
    params.forestry.i <- params.forestry
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

      # Include the interaction term as a feature for the explanatory tree
      if (interact) {
        values$Interact <- values$Feat.1 * values$Feat.2
      }

      # Train the surrogate model
      params.forestry.i$y <- values$Val
      params.forestry.i$x <- values[,-which(names(values) == "Val")]
      surrogate_model <- do.call(Rforestry::forestry, args = c(params.forestry.i))
      surrogate_model <- Rforestry::make_savable(surrogate_model)
    } else {
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

      # When doing categorical + continuous interactions need to think
      # of best way to implement this

      params.forestry.i$y <- values$Val
      params.forestry.i$x <- values[,-which(names(values) == "Val")]
      # Train the surrogate model
      surrogate_model <- do.call(Rforestry::forestry, args = c(params.forestry.i))
      surrogate_model <- Rforestry::make_savable(surrogate_model)
    }
    plots <- append(plots, list(plot.obj))
    names.2d <- c(names.2d, paste(features.2d[i,1], features.2d[i,2], sep = "."))

    surrogates <- append(surrogates, surrogate_model)

  }
  names(plots) <- c(object$features, names.2d)
  names(surrogates) <- c(object$features, names.2d)

  return(list("plots" = plots, "models" = surrogates))
}

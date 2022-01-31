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
#' @field saved A list that saves the previous calculations for the 1-d ICE plots,
#'              1-d PDP plots, and 2-d PDP plots. This saves the uncentered calculations.
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
    saved = NULL,

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
    #' get the visualizations of the data using the 'plot()' function, grid predictions
    #' using different predict methods, and centered predictions.
    #'
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

      # Generate Empty Lists for Future Calculations
      ICE.list <- as.list(rep(NA, length(features)))
      names(ICE.list) <- features
      PDP.1D.list <- as.list(rep(NA, length(features)))
      names(PDP.1D.list) <- features

      PDP.2D.list <- as.list(rep(NA, nrow(features.2d)))
      all_names <- c(features.2d, sep = ", ")
      names(PDP.2D.list) <- do.call(paste, all_names)

      # intialize all variables
      self$interpreter <- interpreter
      self$features <- features
      self$features.2d <- features.2d
      self$center.at <- center.at
      self$grid.points <- grid.points
      self$saved <- list(ICE = ICE.list,
                         PDP.1D = PDP.1D.list,
                         PDP.2D = PDP.2D.list)
    }
  )
)

#' @name set.center.at
#' @title Sets the center value used for plotting
#' @description Method for setting center value for a specific feature
#' @param object The Plotter class to recenter
#' @param feature The feature to center
#' @param value The new center value to use for the feature.
#' @note
#' Unlike the grid predictions, the center.at values do not modify any of the
#' previous saved calculations. Therefore, it does not change or remove any of the
#' previously calculated, saved data.
#' @export
set.center.at = function(object,
                         feature,
                         value)
{
  if (!(inherits(object, "Plotter"))){
    stop("Plotter given is not of the plotter class.")
  }
  if (!(feature %in% unique(c((as.vector(t(object$features.2d))), object$features)))){
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
#' @note
#' Because the grid points determine what calculations are performed, changing the grid points
#' will remove any of the previously calculated values in the 'Plotter' object. For any 1-d ICE
#' or PDP plot, it will remove the previous calculations for the given feature. For any 2-d PDP
#' calcuations, it will remove plots that include the given feature as any of its features.
#' @export
set.grid.points = function(object,
                           feature,
                           values)
{
  if (!(inherits(object, "Plotter"))){
    stop("Plotter given is not of the plotter class.")
  }
  if (!(feature %in% unique(c((as.vector(t(object$features.2d))), object$features)))){
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

  # clear saved values because calculations have changed
  object$saved[["ICE"]][[feature]] <- NA
  object$saved[["PDP.1D"]][[feature]] <- NA

  index_rm <- which(rowSums(object$features.2d == feature) > 0)
  for (i in index_rm){
    object$saved[["PDP.2D"]][[i]] <- NA
  }
}

#' @name predict_ICE.Plotter
#' @title Prediction Function for ICE Plots
#' @description Gives predictions at each point on the grid.
#' @param object The Plotter object to use.
#' @param save A binary variable to determine whether the calculations should be
#'             saved in the plotter object
#' @return A list of dataframes for each feature. In each dataframe, the first
#'         column contains the grid values for the feature, and each subsequent
#'         column has a single observation with the modified feature set to that
#'         row's grid point.
#'
#' Needed fixes: add in second variable interpretations,
#' add grouping by another variable
#' @export
predict_ICE.Plotter = function(object, save = TRUE) {

  if (!(inherits(object, "Plotter"))){
    stop("Plotter given is not of the plotter class.")
  }

  # if all grid predictions are filled
  if (sum(is.na(object$saved$ICE)) == 0){
    return(object$saved$ICE)
  }

  grid.predictions <- list()
  needs.update <- names(which(is.na(object$saved$ICE)))

  # make grid predictions for each variable listed if not saved
  for (feature in object$features){
    results <- NULL
    if (feature %in% needs.update){
      results <- data.frame(sentinel = rep(0, length(object$interpreter$data.points)))
      index <- which(names(object$grid.points) == feature)
      for (val in object$grid.points[[index]]) {
        newdata <- object$interpreter$predictor$data[object$interpreter$data.points,]
        newdata[, feature] <- val
        results <- cbind.data.frame(results,
                                    val = predict(object$interpreter$predictor, newdata)[, 1])
      }
      results <- data.frame(results)
      results <- results[, -1, drop = FALSE]
      colnames(results) <- object$grid.points[[index]]

      # return transpose (easier for plotting)
      results <- (t(results))
      colnames(results) <- object$interpreter$data.points
      results <- cbind(feature = object$grid.points[[index]], results)
      rownames(results) <- NULL
    }
    else{
      results <- object$saved[["ICE"]][[feature]]
    }
    grid.predictions <- append(grid.predictions, list(data.frame(results)))
  }
  names(grid.predictions) <- object$features

  if (save == TRUE){
    object$saved[["ICE"]] <- grid.predictions
  }
  else{
    return(grid.predictions) # returns a list of grid predictions for plotting
  }

  return(object$saved[["ICE"]])
}

#' predict_PDP.1D.Plotter
#' @name predict_PDP.1D.Plotter
#' @title Prediction Function for PDP Plots
#' @description Gives prediction curve for all specified features in the
#'              plotter object
#' @param object The Plotter object to use.
#' @param save A binary variable to determine whether the calculations should be
#'             saved in the plotter object
#' @return A list of dataframes with the grid points and PDP prediction curves
#'         for each feature in object
#' @export
predict_PDP.1D.Plotter = function(object, save = TRUE){

  if (!(inherits(object, "Plotter"))){
    stop("Plotter given is not of the plotter class.")
  }

  # if all grid predictions are filled
  if (sum(is.na(object$saved$PDP.1D)) == 0){
    return(object$saved$PDP.1D)
  }

  # find all features that need updating
  needs.update <- names(which(is.na(object$saved$PDP.1D)))

  PDP.preds <- list()
  for (feat in object$features){
    results <- NULL
    if (feat %in% needs.update){
      feature <- object$grid.points[[feat]]
      PDP <- object$interpreter$functions.1d[[feat]](feature)
      results <- cbind(feature, PDP)
      results <- data.frame(results)
      colnames(results) <- c("feature", "PDP")
    }
    else{
      results <- object$saved[["PDP.1D"]][[feat]]
    }
    PDP.preds <- append(PDP.preds, list(results))
  }
    names(PDP.preds) <- object$features

  if (save == TRUE){
    object$saved[["PDP.1D"]] <- PDP.preds
  }
  else{
    return(PDP.preds)
  }

  return(object$saved[["PDP.1D"]])
}

#' predict_PDP.2D.Plotter
#' @name predict_PDP.2D.Plotter
#' @title Two Dimensional Prediction Curve for PDP Plots
#' @description Gives prediction surface for all specified feature pairs in the
#'              plotter object (features.2d)
#' @param object The Plotter object to use.
#' @param save A binary variable to determine whether the calculations should be
#'             saved in the plotter object
#' @return A list of dataframes for each pair of features.2d
#' @export
predict_PDP.2D.Plotter = function(object, save = TRUE){
  if (!(inherits(object, "Plotter"))){
    stop("Plotter given is not of the plotter class.")
  }

  # if all grid predictions are filled
  if (sum(is.na(object$saved$PDP.2D)) == 0){
    return(object$saved$PDP.2D)
  }

  # find all features that need updating
  needs.update <- which(is.na(object$saved$PDP.2D))

  PDP.2D.preds <- list()
  for (i in 1:nrow(object$features.2d)){
    if (i %in% needs.update){
      feat.1 <- object$features.2d[i,1]
      feat.2 <- object$features.2d[i,2]

      grid.values <- expand.grid(object$grid.points[[feat.1]],
                                 object$grid.points[[feat.2]])
      preds <- object$interpreter$functions.2d[[feat.1]][[feat.2]](grid.values)

      results <- cbind(grid.values, preds)
      results <- data.frame(results)
      colnames(results) <- c(feat.1, feat.2, "preds")
    }
    else{
      result <- object$saved[["PDP.2D"]][[i]]
    }
    PDP.2D.preds <- append(PDP.2D.preds, list(results))
    }
  all_names <- c(object$features.2d, sep = ", ")
  names(PDP.2D.preds) <- do.call(paste, all_names)

  if (save == TRUE){
    object$saved[["PDP.2D"]] <- PDP.2D.preds
  }
  else{
    return(PDP.2D.preds)
  }

  return(object$saved[["PDP.2D"]])
}



#' center.preds
#' @name center.preds
#' @title Centers the predicted values for 1-d ICE and PDP plots, 2-d PDP plots
#' @description Given the specified 'center.at' values of the plotter object, this
#'              function centers the speccified type of plot.
#' @param object The Plotter object to use
#' @param plot.type The type of plot which the user wants to center
#' @return centered dataframe/matrix of values for the given plot
#' @export
center.preds = function(object, plot.type){

  if (!(inherits(object, "Plotter"))){
    stop("Plotter given is not of the plotter class.")
  }

  if (!(plot.type %in% c("ICE", "PDP.1D", "PDP.2D"))){
    stop("Please give a valid plot type to center.")
  }

  if (plot.type == "ICE"){
    # For the ICE plots
    hold <- object$saved[["ICE"]]
    for (feature in names(hold)){
      # get the center value for this feature
      newdata <- object$interpreter$predictor$data[object$interpreter$data.points,]
      newdata[, feature] <- object$center.at[[feature]]
      base <- predict(object$interpreter$predictor, newdata)[,1]
      center_row <- c(0, base) # to not subtract the first value in each row (feature)

      # subtract this from each row of the data
      center_df <- rbind(center_row)[rep(1,nrow(hold[[feature]])),]
      hold[[feature]] <- (hold[[feature]] - center_df)
    }
    return(hold)
  }

  if (plot.type == "PDP.1D"){
    hold <- object$saved[["PDP.1D"]]
    for (feature in names(hold)){
      base <- object$interpreter$functions.1d[[feature]](object$center.at[[feature]])
      center_row <- c(0, base)
      center_df <- rbind(center_row)[rep(1,nrow(hold[[feature]])),]

      hold[[feature]] <- hold[[feature]] - center_df
    }
    return(hold)
  }

  else {
    hold <- object$saved[["PDP.2D"]]
    for (i in 1:length(hold)){
      feat.1 <- names(hold[[i]])[1]
      feat.2 <- names(hold[[i]])[2]

      center.1 <- object$center.at[[feat.1]]
      center.2 <- object$center.at[[feat.2]]
      dat <- data.frame(Var1=center.1, Var2 = center.2)
      base <- object$interpreter$functions.2d[[feat.1]][[feat.2]](dat)
      print(base)

      hold[[i]][,"preds"] <- hold[[i]][,"preds"] - base
    }
    return(hold)
  }

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
#' @return A list of plots with 1-d features and 2-d features. For 2-d features with
#'         one continuous and one categorical feature, the plot is a linear plot of the
#'         continuous feature with group colors representing the categorical feature.
#'         For two continuous features, the plot is a heatmap with the shade representing
#'         the value of the outcome.
#' @export
plot.Plotter = function(object,
                        method = "pdp+ice")
{
  if (!(inherits(object, "Plotter"))){
    stop("Plotter given is not of the plotter class.")
  }

  if (!(method %in% c("pdp", "ice", "pdp+ice"))) {
    stop("Method entered is not supported")
  }

  # list of plots to be filled
  plots <- list()
  # for 1-D plots
  if (!(is.null(object$features))){

    df_all <- predict_ICE.Plotter(object)
    df_all <- center.preds(object, plot.type = "ICE")

    for (feature in object$features){
      df <- df_all[[feature]]
      # df contains both pdp line and all ice lines
      pdp.line <- rowMeans(df[, -which(colnames(df) == "feature")]) # same as pdp functions
      df <- cbind(df , pdp = pdp.line)
      df <- setDT(data.frame(df))
      df.ice <- df[,-"pdp"]

      # for scaling
      min.val <- min(df[, -"feature"])
      max.val <- max(df[, -"feature"])

      melt.df <- melt(df, id.vars = "feature")
      melt.df.ice <- melt(df.ice, id.vars = "feature")

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

    # get all necessary values
    vals <- predict_PDP.2D.Plotter(object)
    vals <- center.preds(object, plot.type = "PDP.2D")

    # for the names in each function
    names.2d <- c()
    for (i in 1:nrow(features.2d)){
      # heatmap for 2 continuous features
      if (feature.classes[features.2d[i,1]] == "numeric" &&
          feature.classes[features.2d[i,2]]=="numeric"){

        values <- vals[[i]]
        names(values) <- c("Feat.1", "Feat.2", "Val")

        plot.obj <- ggplot(values, aes(x=Feat.1, y=Feat.2, fill = Val)) +
          geom_tile() + xlab(features.2d[i,1]) + ylab(features.2d[i,2])
        plot.obj <- plot.obj + guides(fill=guide_legend(title = object$interpreter$predictor$y))
      }
      else {
        # find the continuous feature among the two features
        continuous <- 2
        categorical <- 1
        if (feature.classes[features.2d[i,1]] == "numeric"){
          continuous <- 1
          categorical <- 2
        }
        values <- vals[[i]]
        if (continuous ==1){
          names(values) <- c("Cont", "Cat", "Val")
        }
        else{
          names(values) <- c("Cat", "Cont", "Val")
        }
        plot.obj <- ggplot(values, aes(x=Cont, y=Val, group=Cat, color=Cat)) +
          geom_line() + xlab(features.2d[i,continuous]) + ylab(object$interpreter$predictor$y)
        plot.obj <- plot.obj +  guides(color=guide_legend(title = features.2d[i,categorical]))
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
      predictions <- object$interpreter$functions.2d[[feature.1]][[feature.2]](values)
      #for (j in 1:nrow(values)){
      #  prediction <- object$interpreter$functions.2d[[feature.1]][[feature.2]](values[j,1], values[j,2])
      #  predictions <- c(predictions, prediction)
      #}
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
      predictions <- object$interpreter$functions.2d[[continuous]][[categorical]](values)
      #predictions <- c()
      #for (j in 1:nrow(values)){
      #  prediction <- object$interpreter$functions.2d[[continuous]][[categorical]](values[j,1], values[j,2])
      #  predictions <- c(predictions, prediction)
      #}
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

#' @include predictor.R
#' @include interpret.R
#' @importFrom R6 R6Class
#' @importFrom stats predict
#' @import ggplot2
#' @import dplyr
#' @importFrom data.table setDT melt :=
#' @importFrom gridExtra grid.arrange
#' @importFrom stats coef na.omit quantile sd
#' @importFrom utils head
#' @importFrom stats kmeans
#' @importFrom stats ksmooth


#' @name set.center.at
#' @title Sets a new center in the PDP and ICE plots made by an Interpreter
#' @description Method for setting center value for a specific feature
#' @param object The Interpreter class that we want to recenter the plots of.
#' @param feature The name of the feature to set grid points for.
#' @param value The new value to use for the plots of the specified feature to be centered at.
#'   Must match the type of the feature (a factor level or continuous value in the range
#'   of the specified feature).
#' @note
#' Unlike the grid predictions, the center.at values do not modify any of the
#' previous saved calculations. Therefore, it does not change or remove any of the
#' previously calculated, saved data. These center values are simply for the plots
#' made by the interpreter object, rather than the distilled model.
#' @export
set.center.at = function(object,
                         feature,
                         value)
{
  if (!(inherits(object, "Interpreter"))){
    stop("Object given is not of the interpreter class.")
  }
  if (!(feature %in% object$features)){
    stop("Feature given is not in the feature list.")
  }
  # valid feature index
  index <- which(names(object$center.at) == feature)

  if (inherits(value, c("numeric", "integer"))){
    if (class(value) != class(object$predictor$data[,feature])){
      stop("Invalid value for the given feature.")
    }
  }
  if (inherits(value, "factor")){
    if (!(value %in% object$grid.points[[index]])){
      stop("Invalid value for the given feature.")
    }
  }
  object$center.at[[index]] <- value
}

# method for setting grid points for a specific feature
#' @name set.grid.points
#' @title Sets grid points used for plotting PDP and ICE plots
#' @description Method for setting grid points for a specific feature plot
#' @param object The Interpreter class that we want to modify the grid points of.
#' @param feature The name of the feature to set grid points for.
#' @param values The set of new values to be used as the grid points for the selected feature.
#'   Must be a vector with entries in the range of the feature values in the training set
#'   and must match the type of the given feature (either a vector of factor levels
#'   or a vector of continuous feature values). Note that the center must be within
#'   the range of new grid points for continuous features.
#' @note
#' Because the grid points determine what calculations are performed for the
#' PDP/ICE functions, changing the grid points will remove any of the previously
#' calculated values in the 'Interpreter' object. For any 1-D ICE
#' or PDP plot, it will remove the previous calculations for the given feature. For any 2-D PDP
#' calcuations, it will remove plots that include the given feature as any of its features.
#' Note that these set grid points only apply to PDP and ICE plots, and ALE plots have their
#' own grid points determined by the distribution of the training data.
#' @export
set.grid.points = function(object,
                           feature,
                           values)
{
  if (!(inherits(object, "Interpreter"))){
    stop("Object given is not of the interpreter class.")
  }
  if (!(feature %in% unique(c((as.vector(t(object$features.2d))), object$features)))){
    stop("Feature given is not in the feature list.")
  }
  # valid feature index
  index <- which(names(object$grid.points) == feature)

  if (length(values) < 2){
    stop("Requires at least 2 distinct values.")
  }
  if (inherits(values,"numeric")){
    if (class(values) != class(object$predictor$data[,feature])){
      stop("Invalid value for the given feature.")
    }
  }
  if (inherits(values, c("factor", "integer"))){
    if (any(!(values %in% object$predictor$data[,feature]))){
      stop("Invalid value for the given feature.")
    }
  }
  # check that center.at value is in the new set of values
  if (!inherits(values, "factor")){
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
#' @param object The Interpeter object to use.
#' @param features A vector with the names of the features to predict ICE plots for
#' @param save A boolean indicator to indicate whether the calculations should be
#'             saved in the interpreter object or not. This can help reduce
#'             computation if the ICE functions are used many times, but requires
#'             additional memory to store the predictions. By default, this is TRUE.
#' @return A list of data frames, one for each feature. In each data frame, the first
#'         column contains the grid values for the feature, and each subsequent
#'         column has a single observation corresponding to the prediction of the model
#'         when with the given feature set to that grid point value.
#' @note
#' This method is meant to primarily be used to find the exact values for the ICE
#' curves plotted. Note that after the PDP curve is plotted, the returned
#' object of this function will be the saved predictions for plotting the curve, rather
#' than a recalculation of the values.
#'
#' @export
predict_ICE.Plotter = function(object,
                               features = NULL,
                               save = TRUE)
{

  if (!(inherits(object, "Interpreter"))){
    stop("Object given is not of the interpreter class.")
  }

  if (is.null(features)){
    features <- object$features
  }

  # if all grid predictions are filled
  if (sum(is.na(object$saved$ICE)) == 0){
    return(object$saved$ICE[features])
  }

  grid.predictions <- list()
  needs.update <- intersect(names(which(is.na(object$saved$ICE))), features)

  # make grid predictions for each variable listed if not saved
  for (feature in features){
    results <- NULL
    if (feature %in% needs.update){
      results <- data.frame(sentinel = rep(0, length(object$data.points)))
      index <- which(names(object$grid.points) == feature)
      for (val in object$grid.points[[index]]) {
        # Get subsampled data, remove y column, set feature
        newdata <- object$predictor$data[object$data.points,]
        newdata <- newdata[,-which(names(newdata)==object$predictor$y)]

        # necessary fix for factor variables
        if (inherits(val, "character")){
          newdata[,feature] <- factor(rep(val, nrow(newdata)),
                                      levels = levels(object$grid.points[[index]]))
        }
        else{
          newdata[, feature] <- val
        }

        results <- cbind.data.frame(results,
                                    val = predict(object$predictor, newdata)[, 1])
      }
      results <- data.frame(results)
      results <- results[, -1, drop = FALSE]
      colnames(results) <- object$grid.points[[index]]

      # return transpose (easier for plotting)
      results <- (t(results))
      colnames(results) <- object$data.points
      results <- cbind(feature = object$grid.points[[index]], results)
      rownames(results) <- NULL
    }
    else{
      results <- object$saved[["ICE"]][[feature]]
    }
    grid.predictions <- append(grid.predictions, list(data.frame(results)))
  }
  names(grid.predictions) <- features

  if (save == TRUE){
    object$saved[["ICE"]][features] <- grid.predictions
  }
  else{
    return(grid.predictions) # returns a list of grid predictions for plotting
  }

  return(object$saved[["ICE"]][features])
}

#' predict_PDP.1D.Plotter
#' @name predict_PDP.1D.Plotter
#' @title Prediction Function for PDP Plots
#' @description Gives prediction curve for all specified features in the
#'              plotter object
#' @param object The Interpreter object to plot PDP curves for.
#' @param features A vector with the names of the features to predict ICE plots for
#' @param save A boolean indicator to indicate whether the calculations should be
#'             saved in the interpreter object or not. This can help reduce
#'             computation if the PDP functions are used many times, but requires
#'             additional memory to store the predictions. By default, this is set
#'             to TRUE.
#' @return A list of data frames with the grid points and PDP prediction values
#'         for each feature in object
#' @note
#' This method is meant to primarily be used to find the exact values for the 1-D PDP
#' curves plotted. Note that after the PDP curve is plotted, the returned
#' object of this function will be the saved predictions for plotting the curve, rather
#' than a recalculation of the values.
#' @export
predict_PDP.1D.Plotter = function(object,
                                  features = NULL,
                                  save = TRUE)
{

  if (!(inherits(object, "Interpreter"))){
    stop("Objet given is not of the interpreter class.")
  }

  if (is.null(features)){
    features <- object$features
  }

  # if all grid predictions are filled
  if (sum(is.na(object$saved$PDP.1D)) == 0){
    return(object$saved$PDP.1D[features])
  }


  # find all features that need updating
  needs.update <- intersect(names(which(is.na(object$saved$PDP.1D))), features)

  PDP.preds <- list()
  for (feat in features){
    results <- NULL
    if (feat %in% needs.update){
      feature <- object$grid.points[[feat]]
      PDP <- object$pdp.1d[[feat]](feature)
      results <- cbind(feature, PDP)
      results <- data.frame(results)
      colnames(results) <- c("feature", "PDP")
    }
    else{
      results <- object$saved[["PDP.1D"]][[feat]]
    }
    PDP.preds <- append(PDP.preds, list(results))
  }
  names(PDP.preds) <- features

  if (save == TRUE){
    object$saved[["PDP.1D"]][features] <- PDP.preds
  }
  else{
    return(PDP.preds)
  }

  return(object$saved[["PDP.1D"]][features])
}

#' predict_PDP.2D.Plotter
#' @name predict_PDP.2D.Plotter
#' @title Two Dimensional Prediction Curve for PDP Plots
#' @description Gives prediction surface for all specified feature pairs in the
#'              interpreter object (features.2d)
#' @param object The Interpreter object to use.
#' @param feat.2d A 2-column dataframe or matrix that gives the first variable in
#'                in the first column, and the second variable in the next. The
#'                number of rows is equal to the number of 2-D PDPs one would like.
#' @param save A boolean indicator to indicate whether the calculations should be
#'             saved in the interpreter object or not. This can help reduce
#'             computation if the PDP functions are used many times, but requires
#'             additional memory to store the predictions. By default, this is set
#'             to TRUE.
#' @return A list of data frames for each pair of features.2d. Each data frame
#'         contains columns corresponding to the grid points for the two selected
#'         features and a column corresponding to the predictions of the model
#'         at the given combination of grid points.
#' @note
#' This method is meant to primarily be used to find the exact values for the 2-D PDP
#' curves or heatmap plotted. Note that after the PDP curve is plotted, the returned
#' object of this function will be the saved predictions for plotting the curve, rather
#' than a recalculation of the values.
#' @export
predict_PDP.2D.Plotter = function(object,
                                  feat.2d,
                                  save = TRUE)
{
  if (!(inherits(object, "Interpreter"))){
    stop("Object given is not of the interpreter class.")
  }

  PDP.2D.preds <- list()
  names.2d <- c()

  # See if we have all of the prediicted 2-D PDPs desired
  for (i in 1:nrow(feat.2d)){
    ordering <- order(as.character(feat.2d[i,]))
    features <- feat.2d[i, ][ordering] # order the features
    label <- paste(features[1], features[2], sep = ", ")

    # if the predictions for the feature are missing
    if (any(is.na(object$saved$PDP.2D[[label]]))){
      feat.1 <- as.character(features[1])
      feat.2 <- as.character(features[2])

      grid.values <- expand.grid(object$grid.points[[feat.1]],
                                 object$grid.points[[feat.2]])
      preds <- object$pdp.2d[[feat.1]][[feat.2]](grid.values)

      results <- cbind(grid.values, preds)
      results <- data.frame(results)
      colnames(results) <- c(feat.1, feat.2, "preds")

      # if save == TRUE
      if (save){
        object$saved[["PDP.2D"]][[label]] <- results
      }
    }
    else{
      results <- object$saved[["PDP.2D"]][[label]]
    }

    # append these results
    names.2d <- c(names.2d, label)
    PDP.2D.preds <- append(PDP.2D.preds, list(results))
  }

  names(PDP.2D.preds) <- names.2d
  return(PDP.2D.preds)
}



#' center.preds
#' @name center.preds
#' @title Centers the predicted values for 1-d ICE and PDP plots or 2-d PDP plots
#' @description Given the specified 'center.at' values of the Interpreter object, this
#'              function centers all of the plots in the Interpreter object
#'              of the specified type of plot.
#' @param object The Interpreter object to use
#' @param features A vector of names for the 1-D features we want to center
#' @param plot.type The type of plot that the user wants to center the predictions of.
#'        should be one of either "ICE", "PDP.1D", or "PDP.2D"
#' @param feats.2d  A 2-column dataframe or matrix that gives the first variable in
#'                in the first column, and the second variable in the next. The
#'                number of rows is equal to the number of 2-D PDPs one would like
#'                to center.
#' @return A list of centered data frame/matrix of values for the plot
#' @note
#' This function is mainly used to examine the exact values in the plot if the
#' plot is centered. Note that this function should only be called after calling
#' one of the various predict functions that matches the 'plot.type' parameter
#' with 'save' equal to TRUE.
#' @export
center.preds = function(object, features = NULL, plot.type, feats.2d = NULL){

  if (!(inherits(object, "Interpreter"))){
    stop("Object given is not of the interpreter class.")
  }

  if (!(plot.type %in% c("ICE", "PDP.1D", "PDP.2D"))){
    stop("Please give a valid plot type to center.")
  }

  if (is.null(features)){
    features <- object$features
  }

  if (plot.type == "ICE"){
    # For the ICE plots
    hold <- object$saved[["ICE"]]
    for (feature in features){
      # get the center value for this feature
      newdata <- object$predictor$data[object$data.points,]
      newdata[, feature] <- object$center.at[[feature]]
      base <- predict(object$predictor, newdata)[,1]
      center_row <- c(0, base) # to not subtract the first value in each row (feature)

        # subtract this from each row of the data
      center_df <- rbind(center_row)[rep(1,nrow(hold[[feature]])),]
      hold[[feature]] <- (hold[[feature]] - center_df)
    }
    return(hold)
  }

  if (plot.type == "PDP.1D"){
    hold <- object$saved[["PDP.1D"]]
    for (feature in features){
      base <- object$pdp.1d[[feature]](object$center.at[[feature]])
      center_row <- c(0, base)
      center_df <- rbind(center_row)[rep(1,nrow(hold[[feature]])),]

      hold[[feature]] <- hold[[feature]] - center_df
    }
    return(hold)
  }

  else {

    if (is.null(feats.2d)){
      stop("Please give the 2-D features needed.")
    }

    # get the uncentered data needed
    hold <- predict_PDP.2D.Plotter(object, feats.2d)

    for (i in 1:nrow(feats.2d)){
      feat.1 <- feats.2d[i,1]
      feat.2 <- feats.2d[i,2]

      center.1 <- object$center.at[[feat.1]]
      center.2 <- object$center.at[[feat.2]]
      dat <- data.frame(Var1=center.1, Var2 = center.2)
      base <- object$pdp.2d[[feat.1]][[feat.2]](dat)

      hold[[i]][,"preds"] <- hold[[i]][,"preds"] - base
    }
    return(hold)
  }

}

# Helper functions for the ALE plots ===========================================

# Local effect gives the local effect on the predictions of a model
# in the window around the set_value
#
# Parameters:
#     param variable_name - The variable we want perturb to calculate the local effect
#
#     param lower_limit - The lower limit of the variable we want to use
#
#     param upper_limit - The upper limit of the variable we want to use
#
#     param set_value - The value we want to perturb the variable around
#
#     param window_size - An optional parameter for the size of the window around
#                   the variable that we perturb and predict at
#
#     param training_data - The training data we use to make predictions
#
#     param predict_function - The prediction function we use to make predictions for the model
#
# Return:
#     A single value that is the mean local effect of the peturbation on the
#     predictions of the model.
#
local_effect <- function(variable_name,
                         lower_limit,
                         upper_limit,
                         set_value,
                         window_size,
                         training_data,
                         predict_function) {

  # Setup
  n <- nrow(training_data)
  selected_variable <- training_data %>% dplyr::pull(!!as.name(variable_name))

  # Calculating threshold for ALE neighborhood
  if(!missing(window_size)) {
    if(missing(set_value)) {
      set_value <- (upper_limit + lower_limit)/2
    }
    upper_set <- selected_variable[selected_variable >= set_value]
    lower_set <- selected_variable[selected_variable <= set_value]
    half_window_size <- ceiling(n * window_size/2)
    upper_limit <- quantile(upper_set, probs = min(half_window_size/length(upper_set), 1))
    lower_limit <- quantile(lower_set, probs = 1 - min(half_window_size/length(lower_set), 1))
  }

  # Subsetting to neighborhood
  neighborhood_training_data <- training_data %>%
    dplyr::filter(!!as.name(variable_name) <= upper_limit &
                    !!as.name(variable_name) >=  lower_limit)

  # Replacing values and predicting
  mutate_and_predict <- function(new_value) {
    neighborhood_training_data %>% dplyr::mutate(!!as.name(variable_name) := new_value) %>%
      predict_function()
  }

  # Computing single point ALE.
  (mutate_and_predict(upper_limit) - mutate_and_predict(lower_limit)) %>% mean
}

# This function calculates the accumulated local effects for the model over
# a set of grid points using the prediction function.
#
#
#
#
#
accumulated_local_effects <- function(predict_function,
                                      num_grid_points,
                                      variable_name,
                                      training_data,
                                      grid_points,
                                      center,
                                      window_size) {

  if(!missing(num_grid_points) && !missing(grid_points)) {
    stop("Only one of num_grid_points and grid_points can be specified")
  }
  if(missing(grid_points)) {
    grid_points  <- training_data %>% dplyr::pull(!!as.name(variable_name)) %>%
      quantile(probs = seq(0,1, length.out = num_grid_points + 1)) %>% unname %>% unique
  }
  if(center == "zero") {
    grid_point_closest_zero <- grid_points[which.min(abs(grid_points))]
    grid_points <- c(grid_points, - grid_point_closest_zero)
    grid_points <- sort(grid_points)
  }
  local_effects <- purrr::map2_dbl(head(grid_points, -1), grid_points[-1], local_effect,
                                   variable_name = variable_name,
                                   training_data = training_data, predict_function = predict_function, window_size = window_size)

  accumulated_local_effects <- cumsum(local_effects)
  midpoints <- (head(grid_points, -1) +  grid_points[-1])/2
  if(center == "mean") accumulated_local_effects <- accumulated_local_effects - mean(accumulated_local_effects)
  if(center == "zero") {
    center_point <- accumulated_local_effects[which(midpoints == 0)]
    accumulated_local_effects <- accumulated_local_effects - center_point
  }
  out <- tibble(x = midpoints, ale = accumulated_local_effects, variable = variable_name)
  out <- na.omit(out)
  return(out)
}

#' Constructs an ALE for a model.
#' @param predict_function a function taking a single tibble argument and returning the model
#' predictions corresponding to that tibble.
#' @param num_grid_points the number of grid_points at which to construct the ALE
#' @param training_data the training data used to fit the model
#' @param variable_names a vector of feature names in training data for which an ALE is required.
#' @param center one of "uncentered" meaning the plots are not centered, "mean"
#'   meaning the plots are centered at their mean and "zero" meaning the ALE
#'   passes through the origin. When using center == "zero" we recommend setting
#'   window_size because otherwise a smaller and possibly empty set will be used
#'   to compute the ALE at zero.
#' @param grid_points The grid points to use for the AlE calculation.
#' @param window_size the fraction of the data (between zero and one) used to compute each ALE point.
ale <- function(predict_function,
                num_grid_points,
                training_data,
                variable_names,
                center = "zero",
                grid_points,
                window_size
) {


  if(missing(variable_names))
    variable_names <- names(training_data)
  if (!center %in% c("uncentered", "mean", "zero"))
    stop('The "center" argument must be one of "uncentered", "mean", or "zero"')
  out_data <- purrr::map(.x = variable_names, .f = accumulated_local_effects,
                         predict_function = predict_function,
                         num_grid_points = num_grid_points,
                         training_data = training_data,
                         center = center,
                         grid_points = grid_points,
                         window_size = window_size) %>% bind_rows
  out_data
  return(list(ale = out_data, training_data = training_data %>% select(all_of(variable_names))))
}


#' Prediction function for the ALE plots
#' @param x An interpreter object
#' @param feature The feature to build the ALE for (must be continuous)
#' @param training_data The training data to use in order to build the ALE
#' @param save Boolean to save the ALE predictions
#' @return A tibble that contains the ALE predictions for the given values
#' @export
predict_ALE <- function(x, feature, training_data, save = TRUE){
  if (any(is.na(x$ale.grid[[feature]]))){
    # Create prediction function
    predict_function <- function(newdata) {
      x$predictor$prediction.function(x$predictor$model, newdata = newdata)
    }
    feat_ale = ale(predict_function,
                   num_grid_points = x$grid.size,
                   training_data = training_data,
                   variable_names = feature, center = "mean")
    if (save){
      x$ale.grid[[feature]] <- feat_ale
    }
  }
  else{
    feat_ale <- x$ale.grid[[feature]]
  }
  return(feat_ale)
}

#' @name plot-Interpreter
#' @rdname plot-Interpreter
#' @title Plotting method for Interpretor model
#' @description Plots the PDP, ALE, or ICE plots for an Interpreter object
#' @param x Interpreter object to generate plots from
#' @param method The type of plot that we want to generate. Must be one of "ice",
#' "pdp+ice", "pdp", or "ale"
#' @param features a vector of feature names that we want to produce 1-D plots for.
#' @param features.2d 2-D features that we want to produce plots for arguments.
#'                    A two-column dataframe of pairs of features to make local
#'                    surrogates for. Each row represents a pair of features,
#'                    with the names of features as the entries.If
#'                    the 'method' parameter is set to "ale", this argument should not
#'                    be used.
#' @param clusters A number of clusters to cluster the ICE predictions with.
#'                 If this is not NULL, one must use the method "ice".
#' @param clusterType An indicator specifying what method to use for the clustering.
#'    The possible options are "preds", and "gradient". If "preds" is used, the clusters
#'    will be determined by running K means on the predictions of the ICE functions.
#'    If the "gradient" option is used, the clusters will be determined by running K
#'    means on the numerical gradient of the predictions of the ICE functions. If
#'    this is not NULL, one must use the method "ice".
#' @param smooth A binary variable to determine whether to smoothen the plots of the
#'               PDP, ICE, or ALE curves for continuous variables.
#' @param smooth.bandwidth The bandwidth for the kernels. They are scaled such that
#'                       their quartiles are at 0.25 * bandwidth. By default, this
#'                       is set as the maximum difference between the minimum and
#'                       maximum of the grid points.
#' @param smooth.kernel The type of kernel to be used. Users can input either strings "box"
#'                    or "normal". The default is "normal".
#' @param smooth.npoints The number of points returned when using the kernel method. By
#'                       default, this is twice the number of grid points for that
#'                       feature.
#' @param ... Additiional parameters to pass to the plot function
#' @return A list of plots with 1-d features and 2-d features. For 2-d features with
#'         one continuous and one categorical feature, the plot is a linear plot of the
#'         continuous feature with group colors representing the categorical feature.
#'         For two continuous features, the plot is a heatmap with the shade representing
#'         the value of the outcome.
#' @export
plot.Interpreter = function(x,
                        method = "pdp+ice",
                        features = NULL,
                        features.2d = NULL,
                        clusters = NULL,
                        clusterType = "preds",
                        smooth = FALSE,
                        smooth.bandwidth = NULL,
                        smooth.kernel = "normal",
                        smooth.npoints = 2 * x$grid.size,
                        ...)
{

  if (!(inherits(x, "Interpreter"))){
    stop("x given is not of the interpreter class.")
  }

  if (!(method %in% c("pdp", "ice", "pdp+ice","ale"))) {
    stop("method entered is not supported")
  }

  if (!(clusterType %in% c("preds", "gradient"))) {
    stop("clusterType entered is not supported")
  }

  # Quash R CMD CHeck notes
  Feat.1 = Feat.2 = Val = Cont = Cat = value = grp = variable = ispdp = Cluster = NULL

  # list of plots to be filled
  plots <- list()

  if (method %in% c("pdp", "ice", "pdp+ice")) {
    # for 1-D plots
    if (!(is.null(x$features))){
      for (feature in features){
        hold_feat <- x$predictor$data[x$data.points, feature, drop=FALSE]
        names(hold_feat) <- c("feature")

        df_all <- predict_ICE.Plotter(x, features = features)

        # bar plot for factor variables
        if (x$feat.class[[feature]]=="factor"){
          # Process Data
          data.factor <- t(df_all[[feature]])[-1,]
          if (is.null(levels(x$predictor$data[[feature]]))){
            vals <- df_all[[feature]][,1]
          }
          else{
            vals <- levels(x$predictor$data[[feature]])[df_all[[feature]][,1]]
          }

          sds <- apply(data.factor, 2, sd)
          means <- apply(data.factor, 2, mean)
          mins <- means-sds
          maxs <- means+sds
          # get counts
          counts <- c()
          for (val in vals){
            counts <- c(counts, sum(x$predictor$data[x$data.points, feature] == val))
          }

          temp.data <- data.frame(vals, means, mins, maxs, counts)

          plot.obj <-
            ggplot(data = temp.data,
                   aes(x = vals, y = means)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            ylab(x$predictor$y) + xlab(feature) +
            geom_errorbar(aes(ymin = mins, ymax = maxs))
            theme_bw()

          # make frequency plot
          frequency <- ggplot(temp.data, aes(x=vals, y=counts)) +
            geom_bar(stat = "identity") + xlab(feature) + ylab("Counts")
        }
        # for continuous
        else{
          df_all <- center.preds(x, features = features, plot.type = "ICE")
          df <- df_all[[feature]]
          # df contains both pdp line and all ice lines
          pdps <- predict_PDP.1D.Plotter(x, features = features)
          pdp.line <- center.preds(x, features = features, plot.type = "PDP.1D")[[feature]][,2]
          df <- cbind(df , pdp = pdp.line)

          # smooth curves in dataframe
          if (smooth){
            if (is.null(smooth.bandwidth)){
              smooth.bandwidth <- rep(median(diff(df$feature))*5, ncol(df)-1)
            }
            else{
              smooth.bandwidth <- rep(smooth.bandwidth, ncol(df)-1)
            }

            hold <- ksmooth(x = df$feature,
                            y = df[, 2],
                            kernel = smooth.kernel,
                            bandwidth = smooth.bandwidth[1],
                            n.points = smooth.npoints)

            if (any(is.na(hold$y))){
              stop("The given kernel bandwidth returns NA values. Please specify a different bandwidth.")
            }

            new.df <- data.frame(hold$x, hold$y)

            for (col.index in 3:ncol(df)){
              hold <- ksmooth(x = df$feature,
                              y = df[, col.index],
                              kernel = smooth.kernel,
                              bandwidth = smooth.bandwidth[1],
                              n.points = smooth.npoints)
              new.df <- cbind(new.df, hold$y)
            }

            colnames(new.df) <- colnames(df)
            df <- new.df
          }



          df <- setDT(data.frame(df))
          df.ice <- df[,-"pdp"]

          # for scaling
          min.val <- min(df[, -"feature"])
          max.val <- max(df[, -"feature"])

          melt.df <- melt(df, id.vars = "feature")
          if (!is.null(clusters)) {

            if (clusterType == "gradient") {
              input = t(apply(df.ice[,-1], MARGIN = 2, FUN = function(x){return(diff(x))}))
            } else if (clusterType == "preds") {
              input = t(df.ice[,-1])
            }

            cluster.out <- kmeans(x = input, centers = clusters)
            cluster.map <- data.frame(Var = names(cluster.out$cluster),
                                      Cluster = unname(cluster.out$cluster))

            melt.df.ice <- melt(df.ice, id.vars = c("feature"))
            df.test <- left_join(x = melt.df.ice, y = cluster.map, by = c("variable" = "Var"))
            df.test$Cluster <- as.factor(df.test$Cluster)
          } else {
            melt.df.ice <- melt(df.ice, id.vars = "feature")
          }


          if (method == "ice") {
            # If we want to cluster the ice plots, we need to calculate the
            # cluster memberships for each observation
            if (!is.null(clusters)) {
              data.train <- rbind(df.test,
                                  data.frame(feature = hold_feat$feature,
                                             variable = NA,
                                             value = NA,
                                             Cluster = 0))
              plot.obj <-
                ggplot(data = data.train,
                       aes(x = feature,
                           y = value,
                           group = variable,
                           color = Cluster)) +
                geom_line(data = data.train %>% filter(!is.na(value)))+
                scale_color_viridis_d()+
                ylab(x$predictor$y) + xlab(feature) +
                theme_bw()+
                geom_rug(data = data.train %>% filter(is.na(value)), aes(x = feature), color = "black")
            } else {
              data.train <- rbind(melt.df.ice,
                                  data.frame(feature = hold_feat$feature,
                                             variable = NA,
                                             value = NA))

              plot.obj <-
                ggplot(data = data.train, aes(x = feature, y = value, group = variable)) +
                geom_line(data = data.train %>% filter(!is.na(value)), color = "grey")+
                ylab(x$predictor$y) + xlab(feature) +
                geom_rug(data = data.train %>% filter(is.na(value)), aes(x = feature), color = "black")+
                theme_bw()
            }
          }

          if (method == "pdp") {
            data.train <- rbind(melt.df[melt.df$variable == "pdp", ],
                                data.frame(feature = hold_feat$feature,
                                           variable = NA,
                                           value = NA))

            plot.obj <-
              ggplot(data = data.train, aes(x = feature, y = value)) +
              geom_line(data = data.train %>% filter(!is.na(value)))+
              ylab(x$predictor$y) + xlab(feature) +
              geom_rug(data = data.train %>% filter(is.na(value)), aes(x = feature), color = "black")+
              theme_bw()
          }

          if (method == "pdp+ice") {
            melt.df.combined <- melt.df
            melt.df.combined$ispdp <- (melt.df$variable == "pdp")

            data.train <- rbind(melt.df.combined,
                                data.frame(feature = hold_feat$feature,
                                           variable = NA,
                                           value = NA,
                                           ispdp = NA))

            plot.obj <-
              ggplot(data = data.train,
                     aes(
                       x = feature,
                       y = value,
                       group = variable,
                       color = ispdp
                     )) +
              geom_line(data = data.train %>% filter(!is.na(value))) +
              scale_color_manual(labels = c("ICE", "PDP") ,values = c("grey", "red")) +
              guides(color=guide_legend(title = "Plot Type"))+ ylab(x$predictor$y) + xlab(feature)+
              geom_rug(data = data.train %>% filter(is.na(value)), aes(x = feature), color = "black")+
              theme_bw()
          }
        }


        plots <- append(plots, list(plot.obj))
      }

      names(plots) <- features
    }

    # for 2-D plots
    if (!(is.null(features.2d))){ # if there are 2-D features given
      feature.classes <- x$feat.class

      # get all necessary values
      vals <- predict_PDP.2D.Plotter(x, feat.2d = features.2d)
      vals <- center.preds(x, plot.type = "PDP.2D", feats.2d = features.2d)

      # for the names in each function
      names.2d <- c()
      for (i in 1:nrow(features.2d)){
        # heatmap for 2 continuous features
        ordering <- order(as.character(features.2d[i,]))
        features.2d[i,] <- features.2d[i,][ordering]

        if (feature.classes[features.2d[i,1]] != "factor" &&
            feature.classes[features.2d[i,2]]!="factor"){

          values <- vals[[i]]
          names(values) <- c("Feat.1", "Feat.2", "Val")

          plot.obj <- ggplot(values, aes(x=Feat.1, y=Feat.2, fill = Val)) +
            geom_tile() + xlab(features.2d[i,1]) + ylab(features.2d[i,2])
          plot.obj <- plot.obj +
            guides(fill=guide_legend(title = x$predictor$y)) +
            theme_bw()
        }
        else {
          # find the continuous feature among the two features
          continuous <- 2
          categorical <- 1
          if (feature.classes[features.2d[i,1]] != "factor"){
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
            geom_line() + xlab(features.2d[i,continuous]) + ylab(x$predictor$y)
          plot.obj <- plot.obj +
            guides(color=guide_legend(title = features.2d[i,categorical])) +
            theme_bw()
        }
        plots <- append(plots, list(plot.obj))
        names.2d <- c(names.2d, paste(features.2d[i,1], features.2d[i,2], sep = "."))
      }
      #print(names.2d)
      names(plots) <- c(features, names.2d)
    }
    return(plots)

  } else if (method == "ale") {
    # Implement the ale plots for an interpreter class.

    # check that valid features have been given
    if (any(x$feat.class[features] == "factor")){
      stop("ALE methods do not support categorical variables.")
    }

    # Pull the training data
    training_data <- x$predictor$data[x$data.points, ] %>% dplyr::select(-x$predictor$y)

    for (feature in features){
      # Calculate the accumulated local effects using ale function
      feat_ale <- predict_ALE(x, feature, training_data, save = TRUE)

      # smoothen if desired
      if (smooth){
        temp.binsize <- max(feat_ale$training_data[, 1]) -
          min(feat_ale$training_data[, 1])

        if (is.null(smooth.bandwidth)){
          hold <- ksmooth(x = feat_ale$ale$x,
                          y = feat_ale$ale$ale,
                          kernel = smooth.kernel,
                          bandwidth = temp.binsize,
                          n.points = smooth.npoints)
        }

        else{
          hold <- ksmooth(x = feat_ale$ale$x,
                          y = feat_ale$ale$ale,
                          kernel = smooth.kernel,
                          bandwidth = smooth.bandwidth,
                          n.points = smooth.npoints)
        }

        if (any(is.na(hold$y))){
          stop("Bin size for the kernel method creates NA values. Please specify another bin size.")
        }

        to.remove <- nrow(feat_ale$ale)

        # add new rows and remove old rows
        feat_ale$ale <- rbind(feat_ale$ale,
                              data.frame(x = hold$x,
                                         ale = hold$y,
                                         variable = rep(feature, length(hold$x)))
                              )

        feat_ale$ale <- feat_ale$ale[(to.remove+1):nrow(feat_ale$ale), ]
      }

      # Turn output of ale into a plot
      rugplot_data <-
        feat_ale$training_data %>%
        tidyr::gather(key = "variable", value = "x") %>%
        mutate(ale = NA, grp = "rug")
      plot_data <-
        bind_rows(rugplot_data, feat_ale$ale %>% mutate(grp = "ale"))

      ggplot(plot_data) + geom_line(data = filter)
      plt <-
        ggplot(plot_data) +
        geom_line(data = plot_data %>% filter(grp == "ale"), aes(x = x, y = ale)) +
        geom_rug(data = plot_data %>% filter(grp == "rug"), aes(x = x)) +
        facet_wrap( ~ variable, scales = "free") + theme_bw() + xlab("")

      plots <- append(plots, list(plt))
    }
    names(plots) <- features

    return(plots)
  }
}



#' @name localSurrogate
#' @title Given a interpreter object with at least one pair of features,
#'   create a small surrogate model for the two features using the PDP function
#'   as the output and the two features as the independent variables.
#' @description Plots and returns a Rforestry object with a single tree explaining
#'   the PDP surface.
#' @param object Interpreter object to make plots + surrogate for.
#' @param features.2d A two-column dataframe of pairs of features to make local surrogates for.
#'                    Each row represents a pair of features, with the names of features as the
#'                    entries.
#' @param interact An indicator specifying if the surrogate model should also be
#'   given the interaction terms to create the surrogate models with.
#'   Default is FALSE.
#' @param params.forestry Optional list of parameters to pass to the surrogate
#'   model. Defaults to the standard Rforestry parameters with ntree = 1
#'   and maxDepth = 2.
#' @return A list of two distinct lists: one list contains the local surrogate models,
#'         and the other containing the 2-D PDP plots for the specified features.
#' @export
localSurrogate = function(object,
                          features.2d = NULL,
                          interact = FALSE,
                          params.forestry = list())
{

  if (!(inherits(object, "Interpreter"))){
    stop("Object given is not of the interpreter class.")
  }

  if ((is.null(features.2d))) {
    stop("The user must input a set of 2d features to create a surrogate model.")
  }

  # If no forestry params given, default to maxDepth = 2 and ntree = 1
  if (length(params.forestry) == 0) {
    params.forestry$maxDepth = 2
    params.forestry$ntree = 1
  }

  # quash R CMD Check notes
  Feat.1 = Feat.2 = Val = Cont = Cat = NULL

  # list of plots to be filled
  plots <- list()
  surrogates <- list()
  feature.classes <- object$feat.class


  # for the names in each function
  names.2d <- c()
  for (i in 1:nrow(features.2d)){
    params.forestry.i <- params.forestry
    # heatmap for 2 continuous features
    if (feature.classes[features.2d[i,1]] != "factor" &&
        feature.classes[features.2d[i,2]]!="factor"){
      feature.1 <- features.2d[i,1]
      feature.2 <- features.2d[i,2]
      vals.1 <- object$grid.points[[feature.1]]
      vals.2 <- object$grid.points[[feature.2]]
      # create a grid point of values
      values <- expand.grid(vals.1, vals.2)
      predictions <- object$pdp.2d[[feature.1]][[feature.2]](values)

      values <- cbind(values, predictions)
      values <- data.frame(values)
      names(values) <- c("Feat.1", "Feat.2", "Val")

      # still need to relabel axis
      plot.obj <- ggplot(values, aes(x=Feat.1, y=Feat.2, fill = Val)) +
        geom_tile() + xlab(feature.1) + ylab(feature.2)
      plot.obj <- plot.obj + guides(fill=guide_legend(title = object$predictor$y))+
        theme_bw()

      # Include the interaction term as a feature for the explanatory tree
      if (interact) {
        values$Interact <- values$Feat.1 * values$Feat.2
        names(values) <- c(paste(feature.1), paste(feature.2), "Val","Interact")
      } else {
        names(values) <- c(paste(feature.1), paste(feature.2), "Val")
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
      if (feature.classes[features.2d[i,1]] != "factor"){
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
      predictions <- object$pdp.2d[[continuous]][[categorical]](values)
      values <- cbind(values, predictions)
      values <- data.frame(values)
      names(values) <- c("Cont", "Cat", "Val")
      plot.obj <- ggplot(values, aes(x=Cont, y=Val, group=Cat, color=Cat)) +
        geom_line() + xlab(continuous) + ylab(object$predictor$y) + theme_bw()
      plot.obj <- plot.obj +  guides(color=guide_legend(title = categorical))

      # When doing categorical + continuous interactions need to think
      # of best way to implement interactions

      names(values) <- c(paste(continuous), paste(categorical), "Val")
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
  names(plots) <- c(names.2d)
  names(surrogates) <- c(names.2d)

  return(list("plots" = plots, "models" = surrogates))
}

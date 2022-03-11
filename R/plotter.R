#' @include predictor.R
#' @include interpret.R
#' @importFrom R6 R6Class
#' @importFrom stats predict
#' @import ggplot2
#' @import dplyr

#' @name set.method
#' @title Modify the Method Used for Interpretability
#' @description Sets a new method used for interpertability
#' @param object The Interpreter object
#' @param method The new method to generate the interpretability functions
#' @export
set.method = function(object, method){
  checkmate::assert_character(method)
  if (!(inherits(object, "Interpreter"))) {
    stop("Object given is not of the Interpreter class.")
  }
  if (!(method %in% c("pdp", "ale"))){
    stop("This method is not supported by the function.")
  }
  object[["method"]] <- method
}

#' @name set.center.at
#' @title Sets the center value used for plotting
#' @description Method for setting center value for a specific feature
#' @param object The Interpreter class to recenter
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
  if (!(inherits(object, "Interpreter"))){
    stop("Object given is not of the interpreter class.")
  }
  if (!(feature %in% object$features)){
    stop("Feature given is not in the feature list.")
  }
  # valid feature index
  index <- which(names(object$center.at) == feature)

  if (class(value) == "numeric"){
    if (class(value) != class(object$predictor$data[,feature])){
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
#' @param object The Interpreter class to recenter
#' @param feature The feature to center
#' @param values The set of new values to be used as the grid points for the selected feature
#' @note
#' Because the grid points determine what calculations are performed, changing the grid points
#' will remove any of the previously calculated values in the 'Interpreter' object. For any 1-d ICE
#' or PDP plot, it will remove the previous calculations for the given feature. For any 2-d PDP
#' calcuations, it will remove plots that include the given feature as any of its features.
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
  if (class(values) == "numeric"){
    if (class(values) != class(object$predictor$data[,feature])){
      stop("Invalid value for the given feature.")
    }
  }
  if (class(values) %in% c("factor", "integer")){
    if (any(!(values %in% object$predictor$data[,feature]))){
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

  object$saved[["build.grid"]] <- NA
}

#' @name predict_ICE.Plotter
#' @title Prediction Function for ICE Plots
#' @description Gives predictions at each point on the grid.
#' @param object The Interpeter object to use.
#' @param save A binary variable to determine whether the calculations should be
#'             saved in the interpreter object
#' @return A list of dataframes for each feature. In each dataframe, the first
#'         column contains the grid values for the feature, and each subsequent
#'         column has a single observation with the modified feature set to that
#'         row's grid point.
#'
#' Needed fixes: add in second variable interpretations,
#' add grouping by another variable
#' @export
predict_ICE.Plotter = function(object, save = TRUE) {

  if (!(inherits(object, "Interpreter"))){
    stop("Object given is not of the interpreter class.")
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
      results <- data.frame(sentinel = rep(0, length(object$data.points)))
      index <- which(names(object$grid.points) == feature)
      for (val in object$grid.points[[index]]) {
        # Get subsampled data, remove y column, set feature
        newdata <- object$predictor$data[object$data.points,]
        newdata <- newdata[,-which(names(newdata)==object$predictor$y)]

        # necessary fix for factor variables
        if (class(val) == "character"){
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
#' @param object The Interpreter object to use.
#' @param save A binary variable to determine whether the calculations should be
#'             saved in the interpreter object
#' @return A list of dataframes with the grid points and PDP prediction curves
#'         for each feature in object
#' @export
predict_PDP.1D.Plotter = function(object, save = TRUE){

  if (!(inherits(object, "Interpreter"))){
    stop("Objet given is not of the interpreter class.")
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
      PDP <- object$functions.1d[[feat]](feature)
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
#'              interpreter object (features.2d)
#' @param object The Interpreter object to use.
#' @param save A binary variable to determine whether the calculations should be
#'             saved in the interpreter object
#' @return A list of dataframes for each pair of features.2d
#' @export
predict_PDP.2D.Plotter = function(object, save = TRUE){
  if (!(inherits(object, "Interpreter"))){
    stop("Object given is not of the interpreter class.")
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
      preds <- object$functions.2d[[feat.1]][[feat.2]](grid.values)

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
#' @description Given the specified 'center.at' values of the interpreter object, this
#'              function centers the speccified type of plot.
#' @param object The Interpreter object to use
  #' @param plot.type The type of a which the user wants to center
#' @return centered dataframe/matrix of values for the given plot
#' @export
center.preds = function(object, plot.type){

  if (!(inherits(object, "Interpreter"))){
    stop("Object given is not of the interpreter class.")
  }

  if (!(plot.type %in% c("ICE", "PDP.1D", "PDP.2D"))){
    stop("Please give a valid plot type to center.")
  }

  if (plot.type == "ICE"){
    # For the ICE plots
    hold <- object$saved[["ICE"]]
    for (feature in names(hold)){
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
    for (feature in names(hold)){
      base <- object$functions.1d[[feature]](object$center.at[[feature]])
      center_row <- c(0, base)
      center_df <- rbind(center_row)[rep(1,nrow(hold[[feature]])),]

      hold[[feature]] <- hold[[feature]] - center_df
    }
    return(hold)
  }

  else {
    # NEEDS WORK ON CENTERING
    hold <- object$saved[["PDP.2D"]]
    for (i in 1:length(hold)){
      feat.1 <- names(hold[[i]])[1]
      feat.2 <- names(hold[[i]])[2]

      center.1 <- object$center.at[[feat.1]]
      center.2 <- object$center.at[[feat.2]]
      dat <- data.frame(Var1=center.1, Var2 = center.2)
      base <- object$functions.2d[[feat.1]][[feat.2]](dat)

      hold[[i]][,"preds"] <- hold[[i]][,"preds"] - base
    }
    return(hold)
  }

}

# Helper functions for the ALE plots ===========================================
local_effect <- function(variable_name, lower_limit, upper_limit,
                         set_value, window_size,
                         training_data, predict_function) {
  library(dplyr)
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

accumulated_local_effects <- function(predict_function, num_grid_points,
                                      variable_name, training_data,
                                      grid_points, center, window_size) {
  library(dplyr)
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
  local_effects <- map2_dbl(head(grid_points, -1), grid_points[-1], local_effect,
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
#' @param variable_names a character vector of column names in training_data for which an ALE is required.
#' @param center one of "uncentered" meaning the plots are not centered, "mean"
#'   meaning the plots are centered at their mean and "zero" meaning the ALE
#'   passes through the origin. When using center == "zero" we recommend setting
#'   window_size because otherwise a smaller and possibly empty set will be used
#'   to compute the ALE at zero.
#' @param window_size the fraction of the data (between zero and one) used to compute each ALE point.
ale <- function(predict_function,
                num_grid_points,
                training_data,
                variable_names,
                center = "zero",
                grid_points,
                window_size
                ) {
  library(purrr)

  if(missing(variable_names))
    variable_names <- names(training_data)
  if (!center %in% c("uncentered", "mean", "zero"))
    stop('The "center" argument must be one of "uncentered", "mean", or "zero"')
  out_data <- map(.x = variable_names, .f = accumulated_local_effects,
                  predict_function = predict_function,
                  num_grid_points = num_grid_points,
                  training_data = training_data,
                  center = center,
                  grid_points = grid_points,
                  window_size = window_size) %>% bind_rows
  out_data
  return(list(ale = out_data, training_data = training_data %>% select(all_of(variable_names))))
}

#' @name plot-Interpreter
#' @rdname plot-Interpreter
#' @title PLotting method for Interpretor model
#' @description Plots either the PDP plots or ICE plots
#' @param x Interpreter object to make plots for.
#' @param method A method for the plot. Must be one of "ice",
#' "pdp+ice", "pdp", or "ale"
#' @param features 1-D PDP/ICE/ALE curves to plot.
#' @param features.2d 2-D PDP curves to plot.
#' @param ... Additional arguments
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
                        ...)
{

  if (!(inherits(x, "Interpreter"))){
    stop("x given is not of the interpreter class.")
  }

  if (!(method %in% c("pdp", "ice", "pdp+ice","ale"))) {
    stop("Method entered is not supported")
  }
  library(dplyr)

  # Quash R CMD CHeck notes
  Feat.1 = Feat.2 = Val = Cont = Cat = value = variable = ispdp = NULL

  # list of plots to be filled
  plots <- list()

  if (method %in% c("pdp", "ice", "pdp+ice")) {
    # for 1-D plots
    if (!(is.null(x$features))){
      for (feature in features){
        df_all <- predict_ICE.Plotter(x)
        if (x$feat.class[[feature]]!="numeric"){
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
            theme_classic()

          # make frequency plot
          frequency <- ggplot(temp.data, aes(x=vals, y=counts)) +
            geom_bar(stat = "identity") + xlab(feature) + ylab("Counts")
        }
        else{
          df_all <- center.preds(x, plot.type = "ICE")
          df <- df_all[[feature]]
          # df contains both pdp line and all ice lines
          pdps <- predict_PDP.1D.Plotter(x)
          pdp.line <- center.preds(x, plot.type = "PDP.1D")[[feature]][,2]
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
              geom_line(color = "grey")+
              ylab(x$predictor$y) + xlab(feature) +
              theme_classic()
          }

          if (method == "pdp") {
            plot.obj <-
              ggplot(data = melt.df[melt.df$variable == "pdp", ], aes(x = feature, y =
                                                                        value)) +
              geom_line()+
              ylab(x$predictor$y) + xlab(feature) +
              theme_classic()
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
              guides(color=guide_legend(title = "Plot Type"))+
              theme_classic() + ylab(x$predictor$y) + xlab(feature)
          }
          # create a histogram for the distribution of the data
          hold_feat <- x$predictor$data[x$data.points, feature, drop=F]
          names(hold_feat) <- c("feature")
          frequency <- ggplot(hold_feat, aes(x = feature)) +
            geom_histogram(bins = (length(x$grid.points[[feature]]))) +
            xlab(feature)
        }


        plots <- append(plots, list(grid.arrange(plot.obj, frequency, heights = c(2,1))))
      }

      names(plots) <- features
    }

    # for 2-D plots
    if (!(is.null(features.2d))){
      feature.classes <- x$feat.class

      # get all necessary values
      vals <- predict_PDP.2D.Plotter(x)
      vals <- center.preds(x, plot.type = "PDP.2D")

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
          plot.obj <- plot.obj + guides(fill=guide_legend(title = x$predictor$y))
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
            geom_line() + xlab(features.2d[i,continuous]) + ylab(x$predictor$y)
          plot.obj <- plot.obj +  guides(color=guide_legend(title = features.2d[i,categorical]))
        }
        plots <- append(plots, list(plot.obj))
        names.2d <- c(names.2d, paste(features.2d[i,1], features.2d[i,2], sep = "."))
      }
      names(plots) <- c(x$features, names.2d)
    }
    return(plots)

  } else if (method == "ale") {
    # Implement the ale plots for an interpreter class.

    # Create prediction function
    predict_function <- function(newdata) {
      x$predictor$prediction.function(x$predictor$model, newdata = newdata)
    }

    # Pull the training data
    training_data <- x$predictor$data %>% dplyr::select(-x$predictor$y)

    for (feature in features){
      # Calculate the accumulated local effects using ale function
      feat_ale = ale(predict_function,
                     num_grid_points = 50,
                     training_data = training_data,
                     variable_names = feature, center = "mean")

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
        facet_wrap( ~ variable, scale = "free") + theme_bw() + xlab("")

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

  if (!(inherits(object, "Interpreter"))){
    stop("Object given is not of the interpreter class.")
  }

  if ((is.null(object$features.2d))) {
    stop("Interpreter object must have a set of 2d features to create a surrogate model")
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

  features.2d <- object$features.2d
  feature.classes <- object$feat.class
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
      predictions <- object$functions.2d[[feature.1]][[feature.2]](values)

      values <- cbind(values, predictions)
      values <- data.frame(values)
      names(values) <- c("Feat.1", "Feat.2", "Val")

      # still need to relabel axis
      plot.obj <- ggplot(values, aes(x=Feat.1, y=Feat.2, fill = Val)) +
        geom_tile() + xlab(feature.1) + ylab(feature.2)
      plot.obj <- plot.obj + guides(fill=guide_legend(title = object$predictor$y))

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
      predictions <- object$functions.2d[[continuous]][[categorical]](values)
      #predictions <- c()
      #for (j in 1:nrow(values)){
      #  prediction <- object$interpreter$functions.2d[[continuous]][[categorical]](values[j,1], values[j,2])
      #  predictions <- c(predictions, prediction)
      #}
      values <- cbind(values, predictions)
      values <- data.frame(values)
      names(values) <- c("Cont", "Cat", "Val")
      plot.obj <- ggplot(values, aes(x=Cont, y=Val, group=Cat, color=Cat)) +
        geom_line() + xlab(continuous) + ylab(object$predictor$y)
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
  names(plots) <- c(object$features, names.2d)
  names(surrogates) <- c(object$features, names.2d)

  return(list("plots" = plots, "models" = surrogates))
}

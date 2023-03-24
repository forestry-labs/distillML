#' @include predictor.R
#' @importFrom R6 R6Class
#' @importFrom stats median
#'
#' @title Interpreter class description
#' @description A wrapper class based on a predictor object for examining the
#' predictions of the model with respect to one or two features.
#' The two methods for interpreting a model based on one or two features are partial
#' dependence plots (PDP), which averages over the marginal distribution
#' of the predictions of the model, and accumulated local effects (ALE) functions
#' which averages over the conditional distribution of the predictions of the model.
#'
#' The only necessary argument is the Predictor object. The other arguments are
#' optional, but it may be useful to specify the number of samples or the specific
#' data points (data.points) if the training data is very large.
#' This can greatly reduce the time for computation.
#'
#' For the output, the model returns an interpreter object with two lists of
#' functions: one for interpreting a single feature's role in the black-box
#' model, and the other for intepreting a pair of features' role in the
#' black-box model. These interpretability functions are built for each
#' possible feature (or pair of features). Each of these functions return
#' a vector of averaged predictions equal in length to the number of
#' values (or number of rows) input into the function.
#'
#' @field predictor The Predictor object that contains the model that the user wants
#'        to query. This is the only parameter that is required to initialize an
#'        Interpreter object. All entries in the vector must
#'        match column names from the `data` parameter of the Predictor object.
#' @field features An optional list of single features that we want to create PDP functions for.
#' @field features.2d A two column data frame that contains pairs of names that we
#'        want to create 2D PDP functions for. All entries in the data frame must
#'        match column names from the `data` parameter of the Predictor object.
#' @field data.points A vector of indices of data points in the training
#'        data frame to be used as the observations for creating the PDP/ICE/ALE plots.
#'        When the training data is large, it can greatly reduce the required computation
#'        to pass only a downsampled subset of the training data to the pdp
#'        function construction. Alternatively, if one is only interested understanding
#'        the model predictions for a specific subgroup, the indices of the observations
#'        in the given subgroup can be passed here.
#' @field pdp.1d A List of functions giving single feature PDP interpretations of the model.
#' @field pdp.2d A List of functions giving two-feature PDP interpretations of the model
#' @field feat.class A vector that contains the class for each feature (categorical or continuous)
#' @field center.at The value(s) to center the feature plots at. A list of equal length
#'        to the length of the features.
#' @field grid.points A list of vectors containing the grid points to use for
#'                    the predictions for PDP and ICE plots. For ALE plots, we
#'                    use quantile-based methods that depend on the distribution of
#'                    the training data.
#' @field grid.size The number of grid points to plot for a continuous feature. This
#'                  parameter sets the number of grid points for PDP, ICE, and ALE plots.
#' @field saved A list that caches the previous calculations for the 1-D ICE plots,
#'              1-D PDP plots, 2-D PDP plots, and grid points for building the distilled model.
#'              This saves the uncentered calculations.
#' @field ale.grid A list that caches the saved predictions for the ALE plots
#' @examples
#' library(distillML)
#' library(Rforestry)
#' set.seed(491)
#' data <- iris
#'
#' test_ind <- sample(1:nrow(data), nrow(data)%/%5)
#' train_reg <- data[-test_ind,]
#' test_reg <- data[test_ind,]
#'
#'
#' forest <- forestry(x=data[,-1],
#'                    y=data[,1])
#'
#' forest_predictor <- Predictor$new(model = forest, data=train_reg,
#'                                   y="Sepal.Length", task = "regression")
#'
#'forest_interpret <- Interpreter$new(predictor = forest_predictor)
#'
#' @export
Interpreter <- R6::R6Class(
  "Interpreter",
  public = list(
    predictor = NULL,
    features = NULL,
    features.2d = NULL,
    data.points = NULL,
    pdp.1d = NULL,
    pdp.2d = NULL,
    feat.class = NULL,
    center.at = NULL,
    grid.points = NULL,
    grid.size = NULL,
    saved = NULL,
    ale.grid = NULL,

    # initialize an interpreter object
    #' @param predictor The Predictor object that contains the model that the user wants
    #'        to query. This is the only parameter that is required to initialize an
    #'        Interpreter object. All entries in the vector must
    #'        match column names from the `data` parameter of the Predictor object.
    #' @param samples The number of observations used for the interpretability
    #'                method. If no number is given, the default set is the
    #'                minimum between 1000 and the number of rows in the
    #'                training data set. Rows with missing values are excluded from
    #'                being sampled.
    #' @param data.points The indices of the data points used for the PDP/ALE. This
    #'                    overwrites the "samples" parameter above.
    #' @param grid.size The number of grid points used to create for the PDP, ICE, and ALE
    #'                  plots for each feature.
    #'
    #' @return An `Interpreter` object.
    #' @note
    #' The class that wraps a Predictor object for application of different
    #' interpretability methods. For usage examples, please refer to the README
    #' document.
    initialize = function(predictor = NULL,
                          samples = 1000,
                          data.points = NULL,
                          grid.size = 50) {
      # check to see if predictor is a valid predictor object
      if (is.null(predictor)) {
        stop("Predictor not given.")
      }
      if (!(inherits(predictor, "Predictor"))) {
        stop("Predictor given is not of the Predictor class.")
      }

      # determine valid features
      # remove y variable from consideration
      possible <-
        names(predictor$data)[names(predictor$data) != predictor$y]

      # find valid features in the data: more than 1 value
      possible <-
        names(which(apply(predictor$data[possible], 2,
                          function(x) length(unique(x))) >1))

      # one of the following: int, factor, numeric
      possible <-
        possible[which(sapply(predictor$data[possible], class) %in%
                         c("numeric", "factor", "integer"))]
      features <- possible

      # Define feature classes
      classes <- c()
      for (feature in features){
        classes <- c(classes, class(predictor$data[,(feature)]))
      }
      names(classes) <- features


      # check to see if valid sample number
      # if data.points is not specified
      data <- predictor$data
      valid.indices <- unname(which((rowSums(is.na(predictor$data))) == 0))
      if (is.null(data.points)) {
        checkmate::assert_numeric(samples) # check that sample is numeric
        # find data points with complete cases
        samples <- min(samples, length(valid.indices))
        if (samples > length(valid.indices) | samples < 1) {
          stop("Invalid sample number.")
        }
        samples <- as.integer(samples)
        # set data.points to be worked with (# of datapoints = sample number)
        data.points <- sample(valid.indices, samples)
      }
      # if data points were specified
      else {
        checkmate::assert_numeric(data.points)
        if (any(!(data.points %in% valid.indices))) {
          stop("The data points are not in the training data, or having missing values.")
        }
      }

      # create prediction functions for the features
      # for 1-dimensional functions
      pdp.function <- function(feature) {
        force(feature)
        pdp.function.2 <- function(val) {
          return.vals <- c()
          for (v in val){
            data <- predictor$data[data.points, ,drop = FALSE]
            if (classes[[feature]] == "factor"){
              data[, feature] <- factor(rep(v, nrow(data),
                                            levels = levels(predictor$data[, feature])))
            }
            else{
              data[, feature] <- v
            }
            return.vals <- c(return.vals, mean(predict(predictor, data)[, 1]))
          }
          return.vals
        }
        return(pdp.function.2)
      }

      functions.pdp <- list()
      for (feature in features) {
        temp.list <- list(pdp.function(feature = feature))
        functions.pdp <- append(functions.pdp, temp.list)
      }
      names(functions.pdp) <- features

      # for 2d features
      # requires a matrix/dataframe with two columns for predictions
      pdp.function.2d <- function(feature.1, feature.2){
        force(feature.1)
        force(feature.2)
        pdp.function.2d.2 <- function(values){
          results <- c()
          data <- predictor$data[data.points, , drop = FALSE]
          for (i in 1:nrow(values)){
            data[, feature.1] <- values[i,1]
            data[, feature.2] <- values[i,2]
            results <- c(results, mean(predict(predictor, data)[,1]))
          }
          results
        }
        return(pdp.function.2d.2)
      }
      functions.pdp.2d <- list()
      for (feature.1 in features){
        # list of functions for each
        temp.list <- list()
        for (feature.2 in features){
          if (feature.1 != feature.2){
            temp <- pdp.function.2d(feature.1 = feature.1, feature.2 = feature.2)
          }
          else {
            temp <- "Please use the one-dimensional functions."
          }
          temp.list <- append(temp.list, temp)
        }
        names(temp.list) <- features
        functions.pdp.2d <- append(functions.pdp.2d, list(temp.list))
      }
      names(functions.pdp.2d) <- features



      # check to see if valid grid.size
      checkmate::assert_numeric(grid.size)
      grid.size <- as.integer(grid.size)
      if (grid.size < 2){
        stop("Please enter a valid grid size (>=2) to generate the grid.")
      }

      # generate grid.points
      data <- predictor$data[data.points,]
      grid.points <- list()
      for (feature in features){
        if (!inherits(data[,feature], "factor") && length(unique(data[, feature])) > 2){
          temp.list <- list(seq(min(data[,feature]),max(data[,feature]),length.out = grid.size))
        }
        else{
          temp.list <- list(unique(data[,feature]))
        }
        grid.points <- append(grid.points, temp.list)
      }
      names(grid.points) <- features

      # center.at is a list of centers for each variable, initialized to the min
      center.at <- list()
      for (i in 1:length(features)){

        if (inherits(data[,features[i]], "factor")){
          temp.list <- list(grid.points[[i]][1])
        }
        else{
          temp.list <- list(min(grid.points[[i]]))
        }
        center.at <- append(center.at, temp.list)
      }
      names(center.at) <- features

      # Generate Empty Lists for Future Calculations
      ICE.list <- as.list(rep(NA, length(features)))
      names(ICE.list) <- features
      PDP.1D.list <- as.list(rep(NA, length(features)))
      names(PDP.1D.list) <- features

      # possible two dimensional features
      features.2d <- data.frame(feat.1 = 0, feat.2 =0)
      feat.ordered <- features[order(as.character(features))]
      for (i in 1:(length(feat.ordered)-1)){
        for (j in (i+1):length(feat.ordered)){
          row <- c(feat.ordered[i], feat.ordered[j])
          features.2d <- rbind(features.2d, row)
        }
      }
      features.2d <- features.2d[-1, ,drop = FALSE]

      # make ale.grid for continuous/integer features
      cont <- names(which(classes != "factor"))
      ale.grid <- as.list(rep(NA, length(cont)))
      names(ale.grid) <- cont


      PDP.2D.list <- as.list(rep(NA, nrow(features.2d)))
      all_names <- c(features.2d, sep = ", ")
      names(PDP.2D.list) <- do.call(paste, all_names)

      # initialize all variables belonging to this class
      self$features <- features
      self$features.2d <- features.2d
      self$predictor <- predictor
      self$data.points <- data.points
      self$pdp.1d <- functions.pdp
      self$pdp.2d <- functions.pdp.2d
      self$feat.class <- classes
      # added features from plotter object
      self$grid.points <- grid.points
      self$grid.size <- grid.size
      self$center.at <- center.at
      self$saved <- list(ICE = ICE.list,
                         PDP.1D = PDP.1D.list,
                         PDP.2D = PDP.2D.list)
      self$ale.grid <- ale.grid
    }
  )
)



#' @include predictor.R
#' @importFrom R6 R6Class

#' @title Interpreter class description
#' @description A wrapper class based on a predictor object for examining the
#' predictions of the model. Its predictions with respect to one or two features.
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
#'        want to create 2D pdp functions for. All entries in the data frame must
#'        match column names from the `data` parameter of the Predictor object.
#' @field data.points A vector of indices of data points in the training
#'        data frame to be used as the observations for creating the PDP/ALE functions.
#'        When the training data is large, it can greatly reduce the required computation
#'        to pass only a downsampled subset of the training data to the pdp
#'        function construction. Alternatively, if one is only interested understanding
#'        the model predictions for a specific subgroup, the indices of the observations
#'        in the given subgroup can be passed here.
#' @field functions.1d A List of functions giving single feature interpretations of the model.
#' @field functions.2d A List of functions giving two-feature interpretations of the model
#' @field feat.class A vector that contains the class for each feature (categorical or continuous)
#' @field method The chosen interpretability method for interpretation.
#'        By default, this method is set to partial dependence plots
#'        ("pdp"). This can either be set to "pdp" or "ale".
#' @field center.at The value(s) to center the feature plots at. A list of length
#'        equal to the length of the features.
#' @field grid.points A list of vectors containing the grid points to use for
#'                    the predictions.
#' @field saved A list that caches the previous calculations for the 1-d ICE plots,
#'              1-d PDP plots, 2-d PDP plots, and grid points for building the distillery model.
#'              This saves the uncentered calculations.
#' @examples
#' library(Distillery)
#' library(Rforestry)
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
#' @export
Interpreter <- R6::R6Class(
  "Interpreter",
  public = list(
    predictor = NULL,
    features = NULL,
    features.2d = NULL,
    data.points = NULL,
    functions.1d = NULL,
    functions.2d = NULL,
    feat.class = NULL,
    method = NULL,
    # new features added from plotter function
    center.at = NULL,
    grid.points = NULL,
    saved = NULL,
    # functions.ale = to come later

    # initialize an interpreter object
    #' @param predictor The Predictor object that contains the model that the user wants
    #'        to query. This is the only parameter that is required to initialize an
    #'        Interpreter object. All entries in the vector must
    #'        match column names from the `data` parameter of the Predictor object.
    #' @param samples The number of observations used for the interpretability
    #'                method. If no number is given, the default set is the
    #'                minimum between 1000 and the number of rows in the
    #'                training data set.
    #' @param data.points The indices of the data points used for the PDP/ALE. This
    #'                    overwrites the "samples" parameter above.
    #' @param method The chosen interpretability method for interpretation ("pdp" or "ale").
    #'               By default, this method is set to partial dependence plots
    #'               ("pdp"). This can either be set to "pdp" or "ale".
    #' @param grid.size The number of grid points to create for the pdp
    #'                  functions / plots for each feature.
    #'
    #' @return An `Interpreter` object.
    #' @note
    #' The class that wraps a Predictor object for application of different
    #' interpretability methods.
    initialize = function(predictor = NULL,
                          samples = 1000,
                          data.points = NULL,
                          method = "pdp",
                          # New parameters
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
        names(which(apply(predictor$data[, possible], 2,
                          function(x) length(unique(x))) >1))

      # one of the following: int, factor, numeric
      possible <-
        possible[which(sapply(predictor$data[, possible], class) %in%
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
      if (is.null(data.points)) {
        checkmate::assert_numeric(samples) # check that sample is numeric
        samples <- min(samples, nrow(data))
        if (samples > nrow(data) | samples < 1) {
          stop("Invalid sample number.")
        }
        samples <- as.integer(samples)
        # set data.points to be worked with (# of datapoints = sample number)
        data.points <- sample(1:nrow(data), samples)
      }
      # if data points were specified
      else {
        checkmate::assert_numeric(data.points)
        if (any(!(data.points %in% 1:nrow(data)))) {
          stop("The data points are not in the training data.")
        }
      }

      # check that method is supported
      checkmate::assert_character(method)
      if (!(method %in% c("pdp", "ale"))) {
        stop("This method is not supported by the function.")
      }

      # create prediction functions for the features
      if (method == "pdp") {
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
      }
      else {
        stop("Other methods have not been implemented yet for general functions.")
      }

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
        if (class(data[,feature]) == "numeric"){
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
        if (class(data[,features[i]]) %in% c("numeric", "integer")){
          temp.list <- list(min(grid.points[[i]]))
        }
        else{
          temp.list <- list(grid.points[[i]][1])
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
      feat.ordered <- features[order(features)]
      for (i in 1:(length(feat.ordered)-1)){
        for (j in (i+1):length(feat.ordered)){
          row <- c(feat.ordered[i], feat.ordered[j])
          features.2d <- rbind(features.2d, row)
        }
      }
      features.2d <- features.2d[-1, ,drop = F]

      PDP.2D.list <- as.list(rep(NA, nrow(features.2d)))
      all_names <- c(features.2d, sep = ", ")
      names(PDP.2D.list) <- do.call(paste, all_names)

      # initialize all variables belonging to this class
      self$features <- features
      self$features.2d <- features.2d
      self$predictor <- predictor
      self$data.points <- data.points
      self$functions.1d <- functions.pdp
      self$functions.2d <- functions.pdp.2d
      self$feat.class <- classes
      self$method <- method
      # added features from plotter object
      self$grid.points <- grid.points
      self$center.at <- center.at
      self$saved <- list(ICE = ICE.list,
                         PDP.1D = PDP.1D.list,
                         PDP.2D = PDP.2D.list,
                         build.grid = NA)
    }
  )
)



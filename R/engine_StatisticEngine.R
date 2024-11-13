# MARK: StatisticEngine
#' **StatisticEngine** R6 class and methods
#'
#' @description
#' The *StatisticEngine* R6 class is a framework for performing statistical analysis on data. Data can be added as a
#' character vector with a path to a `csv` file with variable names as first row and analyses names as first column
#' or a `data.frame` or `matrix` object.
#'
#' @template arg-headers
#' @template arg-settings-and-list
#' @template arg-results
#' @template arg-analyses
#' @template arg-interactive
#' @template arg-title
#' @template arg-labs
#' @template arg-showText
#' @template arg-showLegend
#'
#' @export
#'
StatisticEngine <- R6::R6Class("StatisticEngine",
  inherit = CoreEngine,

  # MARK: private fields
  # _ private fields -----
  private = list(
    .check_list_analyses_data_conformity = function(analyses) {
      if (length(analyses) > 0) {
        nvars <- vapply(analyses, function(x) ncol(x$data), 0)
        if (!all(nvars == nvars[1])) {
          warning("The number of variables in the analyses must be equal! Not done.")
          return(FALSE)
        }
        vars <- lapply(analyses, function(x) colnames(x$data))
        if (!all(vapply(vars, function(x) all(x == vars[[1]]), FALSE))) {
          warning("The variables in the analyses must be equal! Not done.")
          return(FALSE)
        }
      }
      TRUE
    }
  ),

  # MARK: active bindings
  # _ active bindings -----
  active = list(

    # MARK: data
    # __ data -----
    #' @field data Data object.
    data = function(value) {
      if (missing(value)) {
        return(self$analyses$data)
      }
      self$analyses$data <- value
      invisible(self)
    },

    # MARK: model
    # __ model -----
    #' @field model Statistic model.
    model = function(value) {
      if (missing(value)) {
        return(self$analyses$model)
      }
      self$analyses$model <- value
      invisible(self)
    },

    # MARK: quantification
    # __ quantification -----
    #' @field quantification Quantification results.
    quantification = function(value) {
      if (missing(value)) {
        return(self$analyses$quantification)
      }
      self$analyses$quantification <- value
      invisible(self)
    },

    # MARK: prediction_results
    # __ prediction_results -----
    #' @field prediction_results Prediction results.
    prediction_results = function() {
      res <- self$get_results("prediction")
      if (length(res) > 0) {
        return(res$prediction[["results"]])
      }
      NULL
    },

    # MARK: classification_results
    # __ classification_results -----
    #' @field classification_results Classification results.
    classification_results = function() {
      res <- self$get_results("classification")
      if (length(res) > 0) {
        return(res$classification[["results"]])
      }
      NULL
    }
  ),

  # MARK: public fields
  # _ public fields -----
  public = list(

    # MARK: initialize
    ## ___ initialize -----
    #' @description Creates an R6 class *StatisticEngine*. Child of *CoreEngine* R6 class.
    #'
    #' @param file Character of length one with the full path to the `sqlite`/`rds` save file of the engine.
    #' @param headers A `ProjectHeaders` S7 class object.
    #' @param analyses A `StatisticAnalyses` S7 class object or a `character vector` with full file path to `.csv`
    #' file with variable names as first row and analyses names as first column or a `data.frame` or `matrix` object.
    #' @param workflow A `Workflow` S7 class object.
    #'
    initialize = function(file = NULL, headers = NULL, workflow = NULL, analyses = NULL) {
      super$initialize(file, headers, workflow, analyses)
      invisible(self)
    },

    # MARK: get_overview
    ## ___ get_overview -----
    #' @description Gets an overview data.frame of all the analyses.
    get_overview = function() {
      self$analyses$info
    },

    # MARK: get_classes
    ## ___ get_classes -----
    #' @description Gets the class of each analysis.
    get_classes = function() {
      self$analyses$classes
    },

    # MARK: get_concentrations
    ## ___ get_concentrations -----
    #' @description Gets the concentration of each analysis.
    get_concentrations = function() {
      self$analyses$concentrations
    },

    # MARK: get_number_variables
    ## ___ get_number_variables -----
    #' @description Gets the number of variables.
    get_number_variables = function() {
      ncol(self$analyses$analyses)
    },

    # MARK: get_model_contributions
    ## ___ get_model_contributions -----
    #' @description Gets the model contributions.
    #'
    #' @param pcs Integer vector with the principle components.
    #'
    get_model_contributions = function(pcs = NULL) {
      m <- self$model

      if (is.null(m)) {
        warning("Model not found! Not done.")
        return(NULL)
      }

      dt <- m$rescont

      if (!is.null(pcs)) {
        checkmate::assert_integerish(pc)
        dt <- dt[pcs, , drop = FALSE]
      }

      if (is.null(dt)) {
        warning("Contributions not found! Not done.")
        return(NULL)
      }

      rownames(dt) <- self$get_analysis_names()

      dt
    },

    # MARK: add_analyses
    ## ___ add_analyses -----
    #' @description Adds analyses. Note that when adding new analyses, any existing results are removed. Note that the
    #' data must have the same number and names of variables (i.e., columns) as the model.
    #'
    #' @param analyses A `character vector` with full file path to `.csv` file with variable names as first row and
    #' analyses names as first column or a `data.frame` or `matrix` object.
    #'
    #' @return Invisible.
    #'
    add_analyses = function(analyses = NULL) {
      self$analyses <- add(self$analyses, analyses)
      invisible(self)
    },

    # MARK: add_classes
    ## ___ add_classes -----
    #' @description Adds classes to the analyses.
    #'
    #' @param classes A character vector with the classes.
    #'
    add_classes = function(classes) {
      if (!is.character(classes)) {
        warning("The classes must be a character vector! Not done.")
        return(invisible())
      }
      if (length(classes) != length(self$analyses)) {
        warning("The number of classes must be equal to the number of analyses! Not done.")
        return(invisible())
      }
      self$analyses$classes <- classes
      invisible(self)
    },

    # MARK: add_concentrations
    ## ___ add_concentrations -----
    #' @description Adds concentrations to the analyses.
    #'
    #' @param concentrations A numeric vector with the concentrations.
    #'
    add_concentrations = function(concentrations = NA_real_) {
      if (!is.numeric(concentrations)) {
        warning("The concentrations must be a numeric vector! Not done.")
        return(invisible())
      }
      if (length(concentrations) != length(self$analyses)) {
        warning("The number of concentrations must be equal to the number of analyses! Not done.")
        return(invisible())
      }
      self$analyses$concentrations <- concentrations
      invisible(self)
    },

    # MARK: remove_analyses
    ## ___ remove_analyses -----
    #' @description Removes analyses.
    #'
    #' @param analyses A character vector with the names or numeric vector with indices of the analyses to remove.
    #'
    #' @return Invisible.
    #'
    remove_analyses = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses <- remove(self$analyses, analyses)
      invisible(self)
    },

    # MARK: predict
    ## ___ predict -----
    #' @description Predicts the data using the model.
    #'
    #' @note Note that the model must be created before prediction and data must have the same number of variables as
    #' the model. Also, any pre-processing applied to the model data should be applied to the data before prediction.
    #' Note that only numeric values are accepted in data and the data column names are used as variable names
    #' and data row names are used as analyses names.
    #'
    #' @param data Data.frame, data-table or matrix with data.
    #'
    predict = function(data = NULL) {
      self$analyses <- predict(self$analyses, data)
      invisible(self)
    },

    # MARK: test
    ## ___ test -----
    #' @description Tests the model using the data.
    #'
    #' @note Note that the model must be created before testing and data must have the same number of variables as
    #' the model. Also, any pre-processing applied to the model data should be applied to the data before testing.
    #' Note that only numeric values are accepted in data and the data column names are used as variable names
    #' and data row names are used as analyses names.
    #'
    #' @param data Data.frame, data-table or matrix with data.
    #'
    test = function(data = NULL) {
      self$analyses <- test(self$analyses, data)
      invisible(self)
    },

    # MARK: plot_data
    ## ___ plot_data -----
    #' @description Plots the data.
    #'
    #' @param features A numeric vector with the features (columns of data matrix) to plot.
    #' @param transpose Logical, if TRUE the data is transposed (i.e., column names are used as legend).
    #'
    plot_data = function(analyses = NULL,
                         features = NULL,
                         transpose = FALSE,
                         interactive = TRUE,
                         xLab = NULL,
                         yLab = NULL,
                         title = NULL) {
      plot_data(self$analyses, analyses, features, transpose, interactive, xLab, yLab, title)
    },

    # MARK: plot_explained_variance
    ## ___ plot_explained_variance -----
    #' @description Plots the model explained cumulative variance.
    plot_explained_variance = function(interactive = TRUE,
                                       xLab = NULL,
                                       yLab = NULL,
                                       title = NULL) {
      plot_explained_variance(self$analyses, interactive, xLab, yLab, title)
    },

    # MARK: plot_scores
    ## ___ plot_scores -----
    #' @description Plots scores of the model.
    #'
    #' @param pcs A numeric vector (length 2) with the principle components to plot.
    #' @param colorGroups A factor character vector with the color groups for the scores.
    #'
    plot_scores = function(analyses = NULL,
                           interactive = TRUE,
                           pcs = 1:2,
                           title = NULL,
                           colorGroups = NULL,
                           showText = TRUE,
                           showLegend = TRUE) {
      plot_scores(self$analyses, analyses, interactive, pcs, title, colorGroups, showText, showLegend)
    },

    # MARK: plot_residuals
    ## ___ plot_residuals -----
    #' @description Plots residuals of the model.
    plot_residuals = function(analyses = NULL,
                              interactive = TRUE,
                              xLab = NULL,
                              yLab = NULL,
                              title = NULL) {
      plot_residuals(self$analyses, analyses, interactive, xLab, yLab, title)
    },

    # MARK: plot_loadings
    ## ___ plot_loadings -----
    #' @description Plots model loadings.
    #'
    #' @param pcs A vector with the principle components to plot.
    #' @param colorKey A character vector with the color key for the loading variables.
    #'
    plot_loadings = function(interactive = TRUE,
                             pcs = 1:2,
                             colorKey = NULL,
                             title = NULL,
                             showText = TRUE,
                             showLegend = TRUE) {
      plot_loadings(self$analyses, interactive, pcs, colorKey, title, showText, showLegend)
    },

    # MARK: plot_resolved_spectra
    ## ___ plot_resolved_spectra -----
    #' @description Plots model resolved spectra.
    #'
    #' @param pcs Integer vectors with the principle component to use for categorization.
    #' @param original Logical, if TRUE the original data is plotted.
    #'
    plot_resolved_spectra = function(interactive = TRUE,
                                     pcs = NULL,
                                     original = TRUE,
                                     title = NULL,
                                     showText = TRUE,
                                     showLegend = TRUE) {
      plot_resolved_spectra(self$analyses, interactive, pcs, original, title, showText, showLegend)
    },

    # MARK: plot_contributions
    ## ___ plot_contributions -----
    #' @description Plots model contributions.
    #'
    #' @param pcs Integer vectors with the principle component to use for categorization.
    #'
    plot_contributions = function(interactive = TRUE,
                                  pcs = NULL,
                                  title = NULL,
                                  showText = TRUE,
                                  showLegend = TRUE) {
      plot_contributions(self$analyses, interactive, pcs, title, showText, showLegend)
    },

    # MARK: plot_residual_distance
    ## ___ plot_residual_distance -----
    #' @description Plots the residual distance of the model.
    #'
    #' @param ... Additional arguments passed to the plotting function.
    #'
    plot_residual_distance = function(...) {
      plot_residual_distance(self$analyses, ...)
    }
  )
)

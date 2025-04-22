# MARK: StatisticEngine
# StatisticEngine -----
#' Statistical Analysis Engine
#'
#' @description
#' The *StatisticEngine* R6 class is a framework for performing statistical analysis on data.
#' Data can be added as a character vector with a path to a `csv` file with variable names as first
#' row and analyses names as first column or a `data.frame` or `matrix` object.
#'
#' @template arg-core-metadata
#' @template arg-core-workflow
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
  # private fields -----
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
  # active bindings -----
  active = list(

    # MARK: data
    ## data -----
    #' @field data Data object.
    data = function(value) {
      if (missing(value)) {
        return(self$Analyses$data)
      }
      self$Analyses$data <- value
      invisible(self)
    },

    # MARK: model
    ## model -----
    #' @field model Statistic model.
    model = function(value) {
      if (missing(value)) {
        return(self$Analyses$model)
      }
      self$Analyses$model <- value
      invisible(self)
    },

    # MARK: quantification
    ## quantification -----
    #' @field quantification Quantification results.
    quantification = function(value) {
      if (missing(value)) {
        return(self$Analyses$quantification)
      }
      self$Analyses$quantification <- value
      invisible(self)
    },

    # MARK: prediction_results
    ## prediction_results -----
    #' @field prediction_results Prediction results.
    prediction_results = function(value) {
      res <- self$get_results("prediction")
      if (length(res) > 0) {
        return(res$prediction[["results"]])
      }
      NULL
    },

    # MARK: classification_results
    ## classification_results -----
    #' @field classification_results Classification results.
    classification_results = function(value) {
      res <- self$get_results("classification")
      if (length(res) > 0) {
        return(res$classification[["results"]])
      }
      NULL
    }
  ),

  # MARK: public fields
  # public fields -----
  public = list(

    # MARK: initialize
    ## initialize -----
    #' @description Creates an R6 class *StatisticEngine*. Child of *CoreEngine* R6 class.
    #'
    #' @param analyses A `StatisticAnalyses` S7 class object or a `character vector` 
    #' with full file path to `.csv` file with variable names as first row and analyses
    #' names as first column or a `data.frame` or `matrix` object.
    #'
    initialize = function(metadata = NULL,
                          workflow = NULL,
                          analyses = NULL) {
      super$initialize(metadata, workflow, analyses)
      invisible(self)
    },

    # MARK: get_overview
    ## get_overview -----
    #' @description Gets an overview data.frame of all the analyses.
    get_overview = function() {
      self$Analyses$info
    },

    # MARK: get_classes
    ## get_classes -----
    #' @description Gets the class of each analysis.
    get_classes = function() {
      self$Analyses$classes
    },

    # MARK: get_concentrations
    ## get_concentrations -----
    #' @description Gets the concentration of each analysis.
    get_concentrations = function() {
      self$Analyses$concentrations
    },

    # MARK: get_number_variables
    ## get_number_variables -----
    #' @description Gets the number of variables.
    get_number_variables = function() {
      ncol(self$Analyses$analyses)
    },

    # MARK: get_model_contributions
    ## get_model_contributions -----
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
    ## add_analyses -----
    #' @description Adds analyses. Note that when adding new analyses, any existing results are removed. Note that the
    #' data must have the same number and names of variables (i.e., columns) as the model.
    #'
    #' @param analyses A `character vector` with full file path to `.csv` file with variable names as first row and
    #' analyses names as first column or a `data.frame` or `matrix` object.
    #'
    #' @return Invisible.
    #'
    add_analyses = function(analyses = NULL) {
      self$Analyses <- add(self$Analyses, analyses)
      invisible(self)
    },

    # MARK: add_classes
    ## add_classes -----
    #' @description Adds classes to the analyses.
    #'
    #' @param classes A character vector with the classes.
    #'
    add_classes = function(classes) {
      if (!is.character(classes)) {
        warning("The classes must be a character vector! Not done.")
        return(invisible())
      }
      if (length(classes) != length(self$Analyses)) {
        warning("The number of classes must be equal to the number of analyses! Not done.")
        return(invisible())
      }
      self$Analyses$classes <- classes
      invisible(self)
    },

    # MARK: add_concentrations
    ## add_concentrations -----
    #' @description Adds concentrations to the analyses.
    #'
    #' @param concentrations A numeric vector with the concentrations.
    #'
    add_concentrations = function(concentrations = NA_real_) {
      if (!is.numeric(concentrations)) {
        warning("The concentrations must be a numeric vector! Not done.")
        return(invisible())
      }
      if (length(concentrations) != length(self$Analyses)) {
        warning("The number of concentrations must be equal to the number of analyses! Not done.")
        return(invisible())
      }
      self$Analyses$concentrations <- concentrations
      invisible(self)
    },

    # MARK: remove_analyses
    ## remove_analyses -----
    #' @description Removes analyses.
    #'
    #' @param analyses A character vector with the names or numeric vector with indices of the analyses to remove.
    #'
    #' @return Invisible.
    #'
    remove_analyses = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses <- remove(self$Analyses, analyses)
      invisible(self)
    },

    # MARK: predict
    ## predict -----
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
      self$Analyses <- predict(self$Analyses, data)
      invisible(self)
    },

    # MARK: test
    ## test -----
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
      self$Analyses <- test(self$Analyses, data)
      invisible(self)
    },

    # MARK: plot_data
    ## plot_data -----
    #' @description Plots the data.
    #'
    #' @param features A numeric vector with the features (columns of data matrix) to plot.
    #' @param transpose Logical, if TRUE the data is transposed (i.e., column names are used as legend).
    #' @param colorGroups A factor character vector with the color groups for the data.
    #' @param xTickLabelsShow Logical, if TRUE the x-axis tick labels are shown.
    #'
    plot_data = function(analyses = NULL,
                         features = NULL,
                         transpose = FALSE,
                         interactive = TRUE,
                         xLab = NULL,
                         yLab = NULL,
                         title = NULL,
                         colorGroups = NULL,
                         xTickLabelsShow = TRUE) {
      plot_data(
        self$Analyses,
        analyses,
        features,
        transpose,
        interactive,
        xLab,
        yLab,
        title,
        colorGroups,
        xTickLabelsShow
      )
    },

    # MARK: plot_explained_variance
    ## plot_explained_variance -----
    #' @description Plots the model explained cumulative variance.
    plot_explained_variance = function(interactive = TRUE,
                                       xLab = NULL,
                                       yLab = NULL,
                                       title = NULL) {
      plot_explained_variance(self$Analyses, interactive, xLab, yLab, title)
    },

    # MARK: plot_scores
    ## plot_scores -----
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
      plot_scores(self$Analyses, analyses, interactive, pcs, title, colorGroups, showText, showLegend)
    },

    # MARK: plot_residuals
    ## plot_residuals -----
    #' @description Plots residuals of the model.
    plot_residuals = function(analyses = NULL,
                              interactive = TRUE,
                              xLab = NULL,
                              yLab = NULL,
                              title = NULL) {
      plot_residuals(self$Analyses, analyses, interactive, xLab, yLab, title)
    },

    # MARK: plot_loadings
    ## plot_loadings -----
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
      plot_loadings(self$Analyses, interactive, pcs, colorKey, title, showText, showLegend)
    },

    # MARK: plot_resolved_spectra
    ## plot_resolved_spectra -----
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
      plot_resolved_spectra(self$Analyses, interactive, pcs, original, title, showText, showLegend)
    },

    # MARK: plot_contributions
    ## plot_contributions -----
    #' @description Plots model contributions.
    #'
    #' @param pcs Integer vectors with the principle component to use for categorization.
    #'
    plot_contributions = function(interactive = TRUE,
                                  pcs = NULL,
                                  title = NULL,
                                  showText = TRUE,
                                  showLegend = TRUE) {
      plot_contributions(self$Analyses, interactive, pcs, title, showText, showLegend)
    },

    # MARK: plot_residual_distance
    ## plot_residual_distance -----
    #' @description Plots the residual distance of the model.
    #'
    #' @param ... Additional arguments passed to the plotting function.
    #'
    plot_residual_distance = function(...) {
      plot_residual_distance(self$Analyses, ...)
    }
  )
)

#' **StatisticEngine** R6 class and methods
#' 
#' @description
#' The *StatisticEngine* R6 class is a framework for performing statistical analysis on data.
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

  # _ active bindings -----
  active = list(
    
    #' @field data Matrix of the data, where rows represent analyses and columns variables.
    #'
    data = function() {
      if (self$has_results("data")) {
        res <- self$get_results("data")
        res
      } else {
        res <- lapply(self$get_analyses(), function(x) x$data)
        names <- self$get_analysis_names()
        res <- do.call(rbind, res)
        rownames(res) <- names
        res
      }
    },
    
    #' @field model Statistic model.
    #'
    model = function(value) {
      if (missing(value)) {
        res <- self$get_results("model")
        if (length(res) > 0) {
          res$model$data
        } else {
          NULL
        }
      } else {
        self$add_results(
          list(
            "model" = list(
              "data" = value,
              "software" = "mdatools",
              "version" = as.character(packageVersion("mdatools"))
            )
          )
        )
        invisible(self)
      }
    },
    
    #' @field prediction_results Prediction results from model.
    #'
    prediction_results = function() {  
      res <- self$get_results("prediction")
      if (length(res) > 0) return(res$prediction[["results"]])
      NULL
    },
    
    #' @field classification_results Classification results.
    #' 
    classification_results = function() {
      res <- self$get_results("classification")
      if (length(res) > 0) return(res$classification[["results"]])
      NULL
    }
  ),

  # _ public fields -----
  public = list(
    
    #' @description Creates an R6 class *StatisticEngine*. Child of *CoreEngine* R6 class.
    #'
    #' @param data Data.frame, data-table or matrix with data.
    #'
    initialize = function(data = NULL, headers = NULL, settings = NULL, analyses = NULL, results = NULL) {
      if (!is.null(analyses)) {
        if (is(analyses, "StatisticAnalysis")) analyses <- list(analyses)
        if (!all(vapply(analyses, function(x) is(x, "StatisticAnalysis"), FALSE))) {
          warning("The argument analyses must be a StatisticAnalysis object or a list of StatisticAnalysis objects! Not done.")
          analyses <- NULL
        }
        if (!private$.check_list_analyses_data_conformity(analyses)) analyses <- NULL
      }
      super$initialize(headers, settings, analyses, results)
      if (!is.null(data)) self$add_data(data)
      private$.register("created", "StatisticEngine", headers$name, paste(c(headers$author, headers$path), collapse = ", "))
    },
    
    ## ___ get -----
    
    #' @description Gets an overview data.frame of all the analyses.
    #'
    get_overview = function() {
      if (length(private$.analyses) > 0) {
        ov <- super$get_overview()
        ov$nvars <- vapply(private$.analyses, function(x) ncol(x$data), 0)
        ov$class <- vapply(private$.analyses, function(x) x$class, NA_character_)
        row.names(ov) <- seq_len(nrow(ov))
        ov
      } else {
       data.frame()
      }
    },
    
    #' @description Gets the class of each analysis.
    #' 
    get_classes = function() {
      if (length(private$.analyses) > 0) {
        classes <- vapply(private$.analyses, function(x) x$class, NA_character_)
        names(classes) <- self$get_analysis_names()
        classes
      } else {
        NULL
      }
    },
    
    #' @description Gets the concentration of each analysis.
    #' 
    get_concentrations = function() {
      if (length(private$.analyses) > 0) {
        concentrations <- vapply(private$.analyses, function(x) x$concentration, NA_real_)
        names(concentrations) <- self$get_analysis_names()
        concentrations
      } else {
        NULL
      }
    },
    
    #' @description Gets the number of variables.
    #' 
    get_number_variables = function() {
      if (length(private$.analyses) > 0) {
        nvars <- vapply(private$.analyses, function(x) ncol(x$data), 0)
        unique(nvars)
      } else {
        0
      }
    },
    
    #' @description Gets model explained variance.
    #'
    get_model_explained_variance = function() {
      m <- self$model
      if (is.null(m)) {
        warning("Model not found! Not done.")
        return(NULL)
      }
      var <- NULL
      switch(is(m),
        "pca" = {
          var <- m$res$cal$expvar
        },
        "mcr" = {
          var <- m$variance
        }
      )
      if (is.null(var)) {
        warning("Explained variance not found! Not done.")
        return(NULL)
      }
      var
    },
    
    #' @description Gets model scores.
    #'
    get_model_scores = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      m <- self$model
      if (is.null(m)) {
        warning("Model not found! Not done.")
        return(NULL)
      }
      dt <- m$res$cal$scores
      if (is.null(dt)) {
        warning("Scores not found! Not done.")
        return(NULL)
      }
      dt <- dt[analyses, , drop = FALSE]
      if (nrow(dt) == 0) {
        warning("Analyses not found! Not done.")
        return(NULL)
      }
      as.data.table(dt, keep.rownames = "analysis")
    },
    
    #' @description Gets model loadings.
    #' 
    get_model_loadings = function() {
      m <- self$model
      if (is.null(m)) {
        warning("Model not found! Not done.")
        return(NULL)
      }
      dt <- m$loadings
      if (is.null(dt)) {
        warning("Loadings not found! Not done.")
        return(NULL)
      }
      browser()
      as.data.table(dt)
    },
    
    #' @description Gets model residuals.
    #' 
    get_model_residuals = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      m <- self$model
      if (is.null(m)) {
        warning("Model not found! Not done.")
        return(NULL)
      }
      dt <- m$res$cal$residuals
      if (is.null(dt)) {
        warning("Residuals not found! Not done.")
        return(NULL)
      }
      dt <- dt[analyses, , drop = FALSE]
      if (nrow(dt) == 0) {
        warning("Analyses not found! Not done.")
        return(NULL)
      }
      dt
    },
    
    #' @description Gets model resolved spectra.
    #' 
    #' @param pcs Integer vector with the principle components.
    #' 
    get_model_resolved_spectra = function(pcs = NULL) {
      
      m <- self$model
      
      if (is.null(m)) {
        warning("Model not found! Not done.")
        return(NULL)
      }
      
      dt <- m$resspec
      
      if (is.null(dt)) {
        warning("Resolved spectra not found! Not done.")
        return(NULL)
      }
      
      if (!is.null(pcs)) {
        checkmate::assert_integerish(pc)
        dt <- dt[pcs, , drop = FALSE]
      }
      
      if (nrow(dt) == 0) {
        warning("PCs not found! Not done.")
        return(NULL)
      }
      
      dt
    },
    
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
    
    ## ___ add -----
    
    #' @description Adds analyses. Note that when adding new analyses, any existing results are removed.
    #'
    #' @param analyses A *StatisticAnalysis* S3 class object or a list with *StatisticAnalysis* S3 class objects as 
    #' elements (see `?StatisticAnalysis` for more information).
    #'
    #' @return Invisible.
    #'
    add_analyses = function(analyses = NULL) {
      analyses <- private$.validate_list_analyses(analyses, childClass = "StatisticAnalysis")
      if (!is.null(analyses)) {
        if (!private$.check_list_analyses_data_conformity(analyses)) return(invisible())
        n_analyses <- self$get_number_analyses()
        super$add_analyses(analyses)
        if (self$get_number_analyses() > n_analyses) self$remove_results()
      }
      invisible(self)
    },
    
    #' @description Adds data to the *StatisticEngine* object.
    #' 
    #' @note Note that only numeric values are accepted in data and the data column names are used as variable names 
    #' and data row names are used as analyses names. The data is internally converted to *StatisticAnalysis* objects.
    #' 
    #' @param data Data.frame, data-table or matrix with data.
    #'  
    add_data = function(data) {

      if (!is.data.frame(data) && !is(data, "data.table") && !is.matrix(data)) {
        warning("The data must be a data.frame, data.table or matrix! Not done.")
        return(invisible())
      }
      
      if (nrow(data) == 0) {
        warning("The data must not be empty! Not done.")
        return(invisible())
      }
     
      if (!all(vapply(data, is.numeric, FALSE))) {
        warning("The data must be numeric! Not done.")
        return(invisible())
      }
     
      if (is(data, "data.table") || is(data, "data.frame")) data <- as.matrix(data)
      
      names <- rownames(data)
      
      if (is.null(names)) names <- paste0("analysis_", seq_len(nrow(data)))
      
      analyses <- list()
      
      for (i in seq_len(nrow(data))) analyses[[i]] <- StatisticAnalysis(name = names[i], data = data[i, , drop = FALSE])
      
      self$add_analyses(analyses)
    },
    
    #' @description Adds classes to the analyses.
    #' 
    #' @param classes A character vector with the classes.
    #' 
    add_classes = function(classes) {
      
      if (!is.character(classes)) {
        warning("The classes must be a character vector! Not done.")
        return(invisible())
      }
      
      if (length(classes) != self$get_number_analyses()) {
        warning("The number of classes must be equal to the number of analyses! Not done.")
        return(invisible())
      }
      
      private$.analyses <- Map(function(x, y) {
        x$class <- y
        x
      }, private$.analyses, classes)
      
      private$.register("added", "analyses", "classes", paste(unique(classes), collapse = "; "))
      
      invisible(self)
    },
    
    #' @description Adds concentrations to the analyses.
    #' 
    #' @param concentrations A numeric vector with the concentrations.
    #' 
    add_concentrations = function(concentrations = NA_real_) {
      
      if (!is.numeric(concentrations)) {
        warning("The concentrations must be a numeric vector! Not done.")
        return(invisible())
      }
      
      if (length(concentrations) != self$get_number_analyses()) {
        warning("The number of concentrations must be equal to the number of analyses! Not done.")
        return(invisible())
      }
      
      private$.analyses <- Map(function(x, y) {
        x$concentration <- y
        x
      }, private$.analyses, concentrations)
      
      private$.register("added", "analyses", "concentrations", paste(unique(concentrations), collapse = "; "))
      
      invisible(self)
    },
    
    ## ___ processing -----
    
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
      
      if (!is.data.frame(data) && !is(data, "data.table") && !is.matrix(data)) {
        warning("The data must be a data.frame, data.table or matrix! Not done.")
        return(invisible())
      }
      
      if (nrow(data) == 0) {
        warning("The data must not be empty! Not done.")
        return(invisible())
      }
      
      if (!all(vapply(data, is.numeric, FALSE))) {
        warning("The data must be numeric! Not done.")
        return(invisible())
      }
      
      if (is(data, "data.table") || is(data, "data.frame")) data <- as.matrix(data)
      
      names <- rownames(data)
      
      if (is.null(names)) {
        names <- paste0("analysis_", seq_len(nrow(data)) + self$get_number_analyses())
        
      } else {
        
        if (any(names %in% self$get_analysis_names())) {
          warning("Some analysis names are already in the analyses! Not done.")
          return(invisible())
        }
      }
      
      if (is.null(self$model)) {
        warning("Model not found! Not done.")
        return(invisible())
      }
      
      if (ncol(data) != self$get_number_variables()) {
        warning("The number of variables in the data must be equal to the number of variables in the model! Not done.")
        return(invisible())
      }
      
      if (!requireNamespace("mdatools", quietly = TRUE)) {
        warning("Package mdatools not found but required! Not done.")
        return(invisible(self))
      }
      
      res <- do.call("predict", list(self$model, data))
      
      self$add_results(list("prediction" = list("results" = res, "data" = data)))
      
      message(paste0("\U2713 ", "Predicted results added!"))
      
      invisible(self)
    },
    
    #' @description Classifies the data using the classification labels of the analysis.
    #' 
    #' @note Note that the classification must be prepared before using the method `prepare_classification` and data 
    #' must have the same number of variables as the analyses in the engine.
    #' 
    #' @param data Data.frame, data-table or matrix with data.
    #' 
    classify = function(data = NULL) {
      
      if (!self$has_results("classification")) {
        warning("Classification instructions not found! Not done.")
        return(invisible())
      }
      
      classification <- self$get_results("classification")[[1]]
      
      train_data <- list()
      train_data[[classification$conditions$train_var]] <- self$data
      train_data[[classification$conditions$label_var]] <- factor(self$get_classes())
      train_data[[classification$conditions$test_var]] <- data
      
      res <- do.call(classification$func, c(train_data, classification$conditions$args))
      
      # check is res has attr prob
      if (!is.null(attr(res, "prob"))) {
        prob = round(attr(res, "prob"), digits = 1)
      } else {
       prob = NULL
      }
      
      res <- data.table::data.table(analysis = rownames(data), class = res)
      
      res$probability <- prob
      
      classification$data <- data
      
      classification$results <- res
      
      self$add_results(list("classification" = classification))
      
      message(paste0("\U2713 ", "Classification results added!"))
      
      invisible(self)
    },
    
    #' @description Evaluates the model for quantification.
    #' 
    #' @note Note that only the model MCR is valid for quantification and concentrations must be added to analyses.
    #' 
    quantify = function() {
      
      if (is.null(self$model)) {
        warning("Model not found! Not done.")
        return(invisible())
      }
      
      if (is(self$model) != "mcr") {
        warning("Model must be MCR for quantification! Not done.")
        return(invisible())
      }
      
      if (all(is.na(self$get_concentrations()))) {
        warning("Concentrations must be added to the analyses for quantification! Not done.")
        return(invisible())
      }
      
      concentrations <- self$get_concentrations()
      
      contributions <- self$get_model_contributions()
      
      res <- list()
      
      for (i in seq_len(ncol(contributions))) {
        
        c_i <- concentrations
        
        x <- c_i[!is.na(c_i)]
        
        y <- stat3$get_model_contributions()[, i][!is.na(c_i)]
        
        linear_model <- lm(x  ~  y)
        
        summary_linear_model <- summary(linear_model)
        
        r_squared <- summary_linear_model$r.squared
        
        to_quantify <- is.na(c_i)
        
        for (j in seq_len(length(c_i))) {
          
          if (to_quantify[j]) {
            mcr_val <- data.frame(y = contributions[, i][j])
            c_i[j] <- stats::predict(linear_model, newdata = mcr_val)
          }
        }
        
        res[[colnames(contributions)[i]]] <- list(
          "r_squared" = r_squared,
          "quantification" = data.table::data.table(
            "analysis" = rownames(contributions),
            "mcr_value" = contributions[, i],
            "concentration" = c_i
          )
        )
      }
      
      res
    },
    
    ## ___ plot -----
    
    #' @description Plots the raw data in analyses.
    #' 
    plot_data = function(analyses = NULL, interactive = TRUE, xLab = NULL, yLab = NULL, title = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      mat <- self$data
      
      mat <- mat[analyses, , drop = FALSE]
      
      if (nrow(mat) == 0) {
        warning("Analyses not found! Not done.")
        return(NULL)
      }
      
      if (is.null(xLab)) xLab <- "Variable Index"
      
      if (is.null(yLab)) yLab <- "Intensity"
      
      if (!interactive) {
        
        NULL
        
      } else {
        
        cl <- .get_colors(rownames(mat))
        
        fig <- plot_ly()
        
        xVal <- seq_len(ncol(mat))
        
        for (i in seq_len(nrow(mat))) {
          
          fig <- fig %>% add_trace(
            x = xVal,
            y = mat[i, ],
            type = "scatter", mode = "lines",
            line = list(width = 0.5, color = unname(cl[i])),
            name = names(cl)[i],
            legendgroup = names(cl)[i],
            showlegend = TRUE
          )
        }
        
        xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = xLab, titlefont = list(size = 12, color = "black"))
        yaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = yLab, titlefont = list(size = 12, color = "black"))
        
        fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
        
        fig
      }
    },
    
    #' @description Plots the model explained cumulative variance.
    #' 
    plot_model_explained_variance = function(interactive = TRUE, xLab = NULL, yLab = NULL, title = NULL) {
      
      variance <- self$get_model_explained_variance()
      
      if (is.null(variance)) {
        warning("Explained model or variance not found!")
        return(NULL)
      }
      
      variance <- cumsum(variance)
      
      if (is.null(xLab)) xLab <- "Principle Components"
      
      if (is.null(yLab)) yLab <- "Explained Variance (%)"
      
      if (!interactive) {
        plot(variance, type = "b", xlab = xLab, ylab = yLab, main = "Explained Variance")
        
      } else {
        
        fig <- plot_ly()
        
        fig <- fig %>% add_trace(
          x = seq_along(variance),
          y = variance,
          type = "scatter",
          mode = "lines+markers",
          line = list(width = 2),
          marker = list(size = 10),
          name = "Explained Variance",
          showlegend = TRUE
        )
        
        xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = xLab, titlefont = list(size = 12, color = "black"), dtick = 1)
        yaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = yLab, titlefont = list(size = 12, color = "black"))
        
        fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
        
        fig
      }
    },
    
    #' @description Plots scores of the model.
    #' 
    #' @param pcs A numeric vector (length 2) with the principle components to plot.
    #' @param colorGroups A factor character vector with the color groups for the scores.
    #' 
    plot_model_scores = function(analyses = NULL,
                                 interactive = TRUE,
                                 pcs = 1:2,
                                 title = NULL,
                                 colorGroups = NULL,
                                 showText = TRUE,
                                 showLegend = TRUE) {
      
      dt <- self$get_model_scores(analyses)
      
      if (is.null(dt)) return(NULL)
      
      if (length(pcs) > 2) {
        warning("The number of principle components cannot be larger than 2! Not done.")
        return(NULL)
      }
      
      if (any(pcs < 1) || any(pcs > ncol(dt))) {
        warning("The principle components must be in the range of the number of components in the model! Not done.")
        return(NULL)
      }
      
      var <- self$get_model_explained_variance()
      
      if (!interactive) {
        
        NULL
        
      } else {
        
        if (!is.null(colorGroups)) {
          
          if (length(colorGroups) != nrow(dt)) {
            warning("The color groups must have the same length as the number of analyses in the scores! Not done.")
            return(NULL)
          }
          
          colorGroups <- gsub(" ", "_", colorGroups)
          dt$var_name <- as.character(colorGroups)
          cl <- .get_colors(unique(colorGroups))
          
        } else {
          dt$var_name <- dt$analysis
          cl <- .get_colors(rownames(dt))
        }
        
        dt <- dt[order(dt$var_name), ]
        
        if (ncol(dt) == 1) {
          x_val = seq_len(nrow(dt))
          y_val = dt[[2]]
          xLab = "Analysis Index"
          if (!is.null(var)) {
            yLab = paste0("PC", pcs, "(", round(var[pcs], digits = 0) ,"%)")
          } else {
            yLab = paste0("PC", pcs)
          }
        } else {
          x_val = dt[[1 + pcs[1]]]
          y_val = dt[[1 + pcs[2]]]
          
          if (!is.null(var)) {
            xLab = paste0("PC", pcs[1], "(", round(var[pcs[1]], digits = 0) ,"%)")
            yLab = paste0("PC", pcs[2], "(", round(var[pcs[2]], digits = 0) ,"%)")
          } else {
            xLab = paste0("PC", pcs[1])
            yLab = paste0("PC", pcs[2])
          }
        }
        
        if (showText) {
          text <- paste0(dt$analysis, "\n", dt$var_name)
        } else {
          text <- NULL
        }
        
        fig <- plot_ly()
        
        fig <- fig %>% add_trace(
          x = x_val,
          y = y_val,
          type = "scatter",
          mode = "markers+text",
          name = dt$var_name,
          legendgroup = dt$var_name,
          marker = list(size = 10, color = cl[dt$var_name]),
          text = text,
          textfont = list(size = 14, color = cl[dt$var_name]),
          textposition = "top",
          showlegend = showLegend
        )
        
        xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = xLab, titlefont = list(size = 12, color = "black"))
        yaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = yLab, titlefont = list(size = 12, color = "black"))
        
        fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
        
        fig
      }
    },
    
    #' @description Plots residuals of the model.
    #' 
    plot_model_residuals = function(analyses = NULL,
                                    interactive = TRUE,
                                    xLab = NULL,
                                    yLab = NULL,
                                    title = NULL) {
      
      dt <- self$get_model_residuals(analyses)
      
      if (is.null(dt)) return(NULL)
      
      if (is.null(xLab)) xLab <- "Variable Index"
      
      if (is.null(yLab)) yLab <- "Intensity"
      
      if (!interactive) {
        
        NULL
        
      } else {
        
        cl <- .get_colors(rownames(dt))
        
        fig <- plot_ly()
        
        xVal <- seq_len(ncol(dt))
        
        for (i in seq_len(nrow(dt))) {
          
          fig <- fig %>% add_trace(
            x = xVal,
            y = dt[i, ],
            type = "scatter", mode = "lines",
            line = list(width = 0.5, color = unname(cl[i])),
            hoverinfo = "text",
            text = paste(
              "</br> analysis:  ", rownames(dt)[i],
              "</br> variable:  ", colnames(dt),
              "</br> intensity: ", "%{y}"
            ),
            name = names(cl)[i],
            legendgroup = names(cl)[i],
            showlegend = TRUE
          )
        }
        
        xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = xLab, titlefont = list(size = 12, color = "black"))
        yaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = yLab, titlefont = list(size = 12, color = "black"))
        
        fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
        
        fig
      }
    },
    
    #' @description Plots model loadings.
    #' 
    #' @param pcs A vector with the principle components to plot.
    #' @param colorKey A character vector with the color key for the loading variables.
    #' 
    plot_model_loadings = function(interactive = TRUE,
                                   pcs = 1:2,
                                   colorKey = NULL,
                                   title = NULL,
                                   showText = TRUE,
                                   showLegend = TRUE) {
      
      dt <- self$get_model_loadings()
      
      if (is.null(dt)) return(NULL)
      
      if (length(pcs) > 2) {
        warning("The number of principle components cannot be larger than 2! Not done.")
        return(NULL)
      }
      
      if (any(pcs < 1) || any(pcs > ncol(dt))) {
        warning("The principle components must be in the range of the number of components in the model! Not done.")
        return(NULL)
      }
      
      dt <- dt[, pcs, drop = FALSE]
      
      var <- self$get_model_explained_variance()
      
      if (!interactive) {
        
        NULL
        
      } else {
        
        if (!is.null(colorKey)) {
          
          if (length(colorKey) != nrow(dt)) {
            warning("The color key must have the same length as the number of variables in the loadings! Not done.")
            return(NULL)
          }
          
          cl <- .get_colors(colorKey)
          
        } else {
          cl <- .get_colors(1)
        }
        
        if (length(cl) == 1) showLegend <- FALSE
        
        fig <- plot_ly()
        
        if (ncol(dt) == 1) {
          x = seq_len(nrow(dt))
          y = dt[, 1]
          xLab = "Analysis Index"
          if (!is.null(var)) {
            yLab = paste0("PC", pcs, "(", round(var[pcs], digits = 0) ,"%)")
          } else {
            yLab = paste0("PC", pcs)
          }
          
        } else {
          x = dt[, 1]
          y = dt[, 2]
          
          if (!is.null(var)) {
            xLab = paste0("PC", pcs[1], "(", round(var[pcs[1]], digits = 0) ,"%)")
            yLab = paste0("PC", pcs[2], "(", round(var[pcs[2]], digits = 0) ,"%)")
          } else {
            xLab = paste0("PC", pcs[1])
            yLab = paste0("PC", pcs[2])
          }
        }
        
        if (showText) text <- rownames(dt) else text <- NULL
        
        fig <- fig %>% add_trace(
          x = x,
          y = y,
          type = "scatter",
          mode = "markers+text",
          color = names(cl),
          colors = cl,
          text = text,
          textfont = list(size = 14, color = cl),
          textposition = "top",
          marker = list(size = 10, color = cl),
          name = names(cl),
          legendgroup = names(cl),
          showlegend = showLegend
        )
        
        xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = xLab, titlefont = list(size = 12, color = "black"))
        yaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = yLab, titlefont = list(size = 12, color = "black"))
        
        fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
        
        fig
      }
    },
    
    #' @description Plots model resolved spectra.
    #' 
    #' @param pcs Integer vectors with the principle component to use for categorization.
    #' @param original Logical, if TRUE the original data is plotted.
    #' 
    plot_model_resolved_spectra = function(interactive = TRUE,
                                           pcs = NULL,
                                           original = TRUE,
                                           title = NULL,
                                           showText = TRUE,
                                           showLegend = TRUE) {
      
      dt <- self$get_model_resolved_spectra(pcs)
      
      if (is.null(dt)) return(NULL)
      
      if (original) {
        data <- t(self$data)
        dt <- cbind(data, dt)
        for (i in seq_len(ncol(dt))) dt[, i] <- dt[, i] / max(dt[, i])
      }
      
      if (!interactive) {
        
        NULL
        
      } else {
        
        cl <- .get_colors(colnames(dt))
        
        fig <- plot_ly()
        
        x = seq_len(nrow(dt))
        
        xLab = "Var Index"
        
        yLab = "Intensity"
        
        for (i in seq_len(length(cl))) {
          
          fig <- fig %>% add_trace(
            x = x,
            y = dt[, i],
            type = "scatter",
            mode = "markers+lines",
            line = list(size = 0.3, color = cl[i]),
            marker = list(size = 2, color = cl[i]),
            name = names(cl[i]),
            legendgroup = names(cl[i]),
            showlegend = TRUE
          )
        }
        
        xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = xLab, titlefont = list(size = 12, color = "black"))
        yaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = yLab, titlefont = list(size = 12, color = "black"))
        
        fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
        
        fig
      }
    },
    
    #' @description Plots model contributions.
    #' 
    #' @param pcs Integer vectors with the principle component to use for categorization.
    #' 
    plot_model_contributions = function(interactive = TRUE,
                                        pcs = NULL,
                                        title = NULL,
                                        showText = TRUE,
                                        showLegend = TRUE) {
      
      dt <- self$get_model_contributions(pcs)
      
      if (is.null(dt)) return(NULL)
      
      if (!interactive) {
        
        NULL
        
      } else {
        
        cl <- .get_colors(colnames(dt))
        
        fig <- plot_ly()
        
        x = rownames(dt)
        
        xLab = "Analysis Index"
        
        yLab = "Contribution"
        
        for (i in seq_len(length(cl))) {
          
          fig <- fig %>% add_trace(
            x = x,
            y = dt[, i],
            type = "scatter",
            mode = "markers+lines",
            line = list(size = 0.3, color = cl[i], dash = 'dash'),
            marker = list(size = 5, color = cl[i]),
            name = names(cl[i]),
            legendgroup = names(cl[i]),
            showlegend = TRUE
          )
        }
        
        xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = xLab, titlefont = list(size = 12, color = "black"))
        yaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = yLab, titlefont = list(size = 12, color = "black"))
        
        fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
        
        fig
      }
    },
    
    #' @description Plots scores of the model.
    #' 
    #' @param pc Integer (length 1) with the principle component to use for categorization.
    #' 
    plot_predicted_distances = function(pc = NULL,
                                        interactive = TRUE,
                                        title = NULL,
                                        showText = TRUE,
                                        showLegend = TRUE) {
      
      model <- self$model
      
      if (is.null(model)) {
        warning("Model not found! Not done.")
        return(NULL)
      }
      
      predicted <- self$prediction_results
      
      if (is.null(predicted)) {
        warning("Predicted results not found! Not done.")
        return(NULL)
      }
      
      if (!is.null(pc)) {
        
        if (length(pc) != 1) {
          warning("The principle component must be a single integer! Not done.")
          return(NULL)
        }
        
        if (pc < 1 || pc > model$ncomp) {
          warning("The principle component must be in the range of the number of components in the model! Not done.")
          return(NULL)
        }
        
      } else {
        pc <- model$ncomp.selected
      }
      
      if (!requireNamespace("mdatools", quietly = TRUE)) {
        warning("Package mdatools not found but required! Not done.")
        return(invisible(self))
      }
      
      predicted$categories <- mdatools::categorize(self$model, predicted, pc)
      
      Qlim <- model$Qlim
      
      T2lim <- model$T2lim
      
      res = list("model" = model$res$cal, "predicted" = predicted)
      
      lim_data <- mdatools::ldecomp.getLimitsCoordinates(Qlim, T2lim, ncomp = pc, norm = TRUE, log = FALSE)
      
      plot_data <- lapply(res, function(x) mdatools::plotResiduals(x, ncomp = pc, norm = TRUE, log = FALSE, show.plot = FALSE))
      
      cat <- list("model" = rep("model", nrow(plot_data[[1]])), "predicted" = predicted$categories)
      
      names(cat$model) <- rownames(plot_data$model)
      
      names(cat$predicted) <- rownames(plot_data$predicted)
      
      if (!interactive) {
        
        NULL
        
      } else {
        keys <- c(cat$model, as.character(cat$predicted))
        names(keys) <- c(names(cat$model), names(cat$predicted))
        
        plot_data_df <- lapply(plot_data, as.data.table)
        plot_data_df <- data.table::rbindlist(plot_data_df, idcol = "type")
        plot_data_df$type <- keys
        
        ukeys <- unique(keys)
        
        cl <- rep(toRGB("#737373"), length(ukeys))
        cl[ukeys %in% "outlier"] <- toRGB("darkred")
        cl[ukeys %in% "extreme"] <- toRGB("orange")
        cl[ukeys %in% "regular"] <- toRGB("#41AB5D")
        names(cl) <- ukeys
        
        fig <- plot_ly()
        
        fig <- fig %>% add_trace(
          x = lim_data[[1]][, 1],
          y = lim_data[[1]][, 2],
          type = "scatter",
          mode = "lines",
          line = list(width = 1.5, color = toRGB("orange"), dash = "dash"),
          name = "Extreme Limit",
          legendgroup = "Extreme",
          showlegend = showLegend
        )
        
        fig <- fig %>% add_trace(
          x = lim_data[[2]][, 1],
          y = lim_data[[2]][, 2],
          type = "scatter",
          mode = "lines",
          line = list(width = 1.5, color = toRGB("darkred")),
          name = "Outlier Limit",
          legendgroup = "Outlier Limit",
          showlegend = showLegend
        )
        
        for (i in ukeys) {
          
          sel <- keys %in% i
          
          if (showText) {
            text_label <- paste0(names(keys[sel]), "(", keys[sel], ")")
            
          } else {
            text_label <- NULL
          }
          
          fig <- fig %>% add_trace(
            x = plot_data_df[sel, 2][[1]],
            y = plot_data_df[sel, 3][[1]],
            type = "scatter",
            mode = "markers+text",
            text = text_label,
            textfont = list(size = 14, color = cl[i]),
            textposition = "top",
            marker = list(size = 10, color = cl[i]),
            name = i,
            legendgroup = i,
            showlegend = showLegend
          )
        }
        
        xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = "Score distance (h/h0)", titlefont = list(size = 12, color = "black"))
        yaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = "Orthogonal distance (q/q0)", titlefont = list(size = 12, color = "black"))
        
        fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
        
        fig
      }
    },
    
    ## ___ info -----
    
    ### ___ processing_function_calls -----
    
    #' @description A data.table with available data processing methods.
    #'
    processing_methods = function() {
      ps <- list()
      ps[["MakeModel"]] <- 1
      ps[["PrepareClassification"]] <- 1
      data.table(name = names(ps), max = unlist(ps))
    }
  )
)
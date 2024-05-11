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
      
      res <- lapply(self$get_analyses(), function(x) x$data)
      
      names <- self$get_analysis_names()
     
      res <- do.call(rbind, res)
      
      rownames(res) <- names
      
      res
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
    
    #' @field predicted Predicted results model.
    #'
    predicted = function(value) {
      
      if (missing(value)) {
        
        res <- self$get_results("predicted")
        
        if (length(res) > 0) {
          
          res$predicted
          
        } else {
          NULL
        }
      } else {
        
        self$add_results(
          list(
            "predicted" = list(
              "results" = value[["results"]],
              "data" = value[["data"]],
              "software" = "mdatools",
              "version" = as.character(packageVersion("mdatools"))
            )
          )
        )
        
        invisible(self)
      }
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
     
      private$.register(
        "created",
        "StatisticEngine",
        headers$name,
        "StreamFind",
        as.character(packageVersion("StreamFind")),
        paste(c(headers$author, headers$path), collapse = ", ")
      )
    },
    
    ## ___ get -----
    
    #' @description Gets an overview data.frame of all the analyses.
    #'
    get_overview = function() {
     
      if (length(private$.analyses) > 0) {
       
        ov <- super$get_overview()
        
        ov$nvars <- vapply(private$.analyses, function(x) ncol(x$data), 0)
        
        ov$class <- vapply(private$.analyses, function(x) paste(x$classes, collapse = ", "), NA_character_)
        
        row.names(ov) <- seq_len(nrow(ov))
        
        ov
       
      } else {
       data.frame()
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
      
      dt
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
      
      dt
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
    
    ## ___ processing -----
    
    #' @description Makes a Principle Component Analysis (PCA) model.
    #'
    #' @return Invisible.
    #'
    make_pca_model = function(settings) {
      
      # if (missing(settings)) settings <- Settings_make_pca_model_StreamFind()
      
      .dispatch_process_method("make_pca_model", settings, self, private)
      
      invisible(self)
    },
    
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
        return(invisible())
      }
      
      res <- do.call("predict", list(self$model, data))
      
      self$predicted <- list("results" = res, "data" = data)
      
      message(paste0("\U2713 ", "Predicted results added!"))
      
      invisible(self)
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
    
    #' @description Plots scores of the model.
    #' 
    #' @param pcs A vector with the principle components to plot.
    #' 
    plot_model_scores = function(analyses = NULL,
                                 interactive = TRUE,
                                 pcs = 1:2,
                                 title = NULL,
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
      
      dt <- dt[, pcs, drop = FALSE]
      
      if (!interactive) {
        
        NULL
        
      } else {
        
        cl <- .get_colors(rownames(dt))
        
        fig <- plot_ly()
        
        if (ncol(dt) == 1) {
          x = seq_len(nrow(dt))
          y = dt[, 1]
          xLab = "Analysis Index"
          yLab = paste0("PC", pcs)
          
        } else {
          x = dt[, 1]
          y = dt[, 2]
          xLab = paste0("PC", pcs[1])
          yLab = paste0("PC", pcs[2])
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
      
      if (!interactive) {
        
        NULL
        
      } else {
        
        if (!is.null(colorKey)) {
          
          if (length(colorKey) != ncol(dt)) {
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
          yLab = paste0("PC", pcs)
          
        } else {
          x = dt[, 1]
          y = dt[, 2]
          xLab = paste0("PC", pcs[1])
          yLab = paste0("PC", pcs[2])
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
      
      predicted <- self$predicted
      
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
        pc <- 1
      }
      
      predicted$results$categories <- mdatools::categorize(self$model, predicted$results, pc)
      
      Qlim <- model$Qlim
      
      T2lim <- model$T2lim
      
      res = list("model" = model$res$cal, "predicted" = predicted$results)
      
      lim_data <- ldecomp.getLimitsCoordinates(Qlim, T2lim, ncomp = pc, norm = TRUE, log = FALSE)
      
      plot_data <- lapply(res, function(x) plotResiduals(x, ncomp = pc, norm = TRUE, log = FALSE, show.plot = FALSE))
      
      cat <- list("model" = rep("model", nrow(plot_data[[1]])), "predicted" = predicted$results$categories)
      
      if (!interactive) {
        
        NULL
        
      } else {
        
        cl <- .get_colors(c(rep("model", nrow(plot_data[[1]])), rep("predicted", nrow(plot_data[[2]]))))
        
        cl <- split(cl, names(cl))
        
        # names(cl[[2]]) <- as.character(cat[[2]])
        
        cl[[2]][cat[[2]] == "outlier"] <- toRGB("darkred")
        cl[[2]][cat[[2]] == "extreme"] <- toRGB("orange")
        cl[[2]][cat[[2]] == "regular"] <- toRGB("#41AB5D")
        
        names(cl[[2]])[cat[[2]] == "outlier"] <- "outlier"
        names(cl[[2]])[cat[[2]] == "extreme"] <- "extreme"
        names(cl[[2]])[cat[[2]] == "regular"] <- "regular"
        
        fig <- plot_ly()
        
        for (i in seq_len(length(plot_data))) {
          
          if (showText) {
            text <- paste0(rownames(plot_data[[i]]), "(", as.character(cat[[i]]), ")")
            
          } else {
            text <- NULL
          }
          
          idx <- order(names(cl[[i]]))
          
          fig <- fig %>% add_trace(
            x = unname(plot_data[[i]][, 1])[idx],
            y = unname(plot_data[[i]][, 2])[idx],
            type = "scatter",
            mode = "markers+text",
            # color = names(cl[[i]]),
            # colors = cl[[i]],
            text = text,
            textfont = list(size = 14, color = cl[[i]][idx]),
            textposition = "top",
            marker = list(size = 10, color = cl[[i]][idx]),
            name = names(cl[[i]][idx]),
            legendgroup = names(cl[[i]][idx]),
            showlegend = showLegend
          )
        }
        
        fig <- fig %>% add_trace(
          x = lim_data[[1]][, 1],
          y = lim_data[[1]][, 2],
          type = "scatter",
          mode = "lines",
          line = list(width = 2, color = toRGB("orange")),
          name = "Extreme Limit",
          legendgroup = "Extreme",
          showlegend = showLegend
        )
        
        fig <- fig %>% add_trace(
          x = lim_data[[2]][, 1],
          y = lim_data[[2]][, 2],
          type = "scatter",
          mode = "lines",
          line = list(width = 2, color = toRGB("darkred")),
          name = "Outlier Limit",
          legendgroup = "Outlier Limit",
          showlegend = showLegend
        )
        
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
      data.table(
        name = c(
          "make_pca_model"
        ),
        max = c(
          1
        )
      )
    }
  )
)
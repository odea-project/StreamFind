
# StatisticModel -----

#' @export
#' @noRd
StatisticModel <- S7::new_class("StatisticModel", package = "StreamFind", parent = Results,
  
  properties = list(
    model = S7::new_property(S7::class_list, default = list()),
    test = S7::new_property(S7::class_list, default = list()),
    prediction = S7::new_property(S7::class_list, default = list())
  ),
  
  constructor = function(model = list()) {
    S7::new_object(
      Results(),
      name = "StatisticModel",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      model = model,
      test = list(),
      prediction = list()
    )
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_true(self@software == "StreamFind"),
      checkmate::test_list(self@model),
      checkmate::test_list(self@test),
      checkmate::test_list(self@prediction)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

# PCA -----

#' @export
#' @noRd
PCA <- S7::new_class("PCA", package = "StreamFind", parent = StatisticModel,

  properties = list(
    test = S7::new_property(S7::class_list, default = list(),
      getter = function(self) {
        if (length(self$model$res$test) == 0) return(NULL)
        self$model$res$test
      },
      setter = function(self, value) {
        if (length(self$model) > 0) self$model$res$test <- value
        self
      }
    )
  ),
  
  constructor = function(model = list()) {
    S7::new_object(
      StatisticModel(),
      name = "PCA",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      model = model,
      prediction = list()
    )
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_true(self@name == "PCA"),
      checkmate::test_true(self@software == "StreamFind")
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @noRd
S7::method(summary,  PCA) <- function(x) {
  summary(x$model)
}

#' @noRd
S7::method(plot,  PCA) <- function(x, ...) {
  plot(x$model, ...)
}

#' @noRd
S7::method(get_model_data,  PCA) <- function(x) {
  list(
    "ncomp" = x$model$ncomp.selected,
    "explained_variance" = x$model$calres$expvar,
    "scores" = x$model$calres$scores,
    "loadings" = x$model$loadings,
    "residuals" = x$model$calres$residuals
  )
}

#' @noRd
S7::method(predict,  PCA) <- function(x, data) {
  res <- stats::predict(x$model, data)
  x@prediction <- list("results" = res, "data" = data)
  x
}

#' @noRd
S7::method(test,  PCA) <- function(x, data) {
  res <- stats::predict(x$model, data)
  res$data <- data
  x$test <- res
  x
}

#' @export
#' @noRd
S7::method(plot_prediction, PCA) <- function(x, ...) {
  
  pc = NULL
  interactive = TRUE
  title = NULL
  showText = TRUE
  showLegend = TRUE
  
  dots <- list(...)
  
  if (length(dots) > 0) {
    if ("pc" %in% names(dots)) {
      pc <- dots$pc
    }
    if ("interactive" %in% names(dots)) {
      interactive <- dots$interactive
    }
    if ("title" %in% names(dots)) {
      title <- dots$title
    }
    if ("showText" %in% names(dots)) {
      showText <- dots$showText
    }
    if ("showLegend" %in% names(dots)) {
      showLegend <- dots$showLegend
    }
  }
  
  model <- x$model
  
  if (is.null(model)) {
    warning("Model not found! Not done.")
    return(NULL)
  }
  
  predicted <- x$prediction$results
  
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
  
  predicted$categories <- mdatools::categorize(model, predicted, pc)
  
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
}

# MCRPURE -----

#' @export
#' @noRd
MCRPURE <- S7::new_class("MCRPURE", package = "StreamFind", parent = StatisticModel,

  properties = list(
   
  ),
  
  constructor = function(model = list()) {
    S7::new_object(
      StatisticModel(), 
      name = "MCRPURE",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      model = model,
      prediction = list()
    )
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_true(self@name == "MCRPURE"),
      checkmate::test_true(self@software == "StreamFind")
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @noRd
S7::method(summary,  MCRPURE) <- function(x) {
  summary(x$model)
}

#' @noRd
S7::method(plot,  MCRPURE) <- function(x, ...) {
  plot(x$model, ...)
}

#' @noRd
S7::method(get_model_data,  MCRPURE) <- function(x) {
  list(
    "ncomp" = x$model$ncomp,
    "explained_variance" = unlist(x$model$variance[1, ]),
    "purity" = x$model$purityspec,
    "resolved" = x$model$resspec
  )
}

# KNN -----

#' @export
#' @noRd
KNN <- S7::new_class("KNN", package = "StreamFind", parent = StatisticModel,

  properties = list(
    
  ),
  
  constructor = function(model = list()) {
    S7::new_object(
      StatisticModel(), 
      name = "KNN",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      model = model,
      prediction = list()
    )
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_true(self@name == "KNN"),
      checkmate::test_true(self@software == "StreamFind")
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @noRd
S7::method(summary,  KNN) <- function(x) {
  NULL
}

#' @noRd
S7::method(plot,  KNN) <- function(x, ...) {
  NULL
}

#' @noRd
S7::method(get_model_data,  KNN) <- function(x) {
  list(
    "func" = x$model$func,
    "conditions" = x$model$conditions
  )
}

#' @noRd
S7::method(predict, KNN) <- function(x, data) {
  
  x$model$conditions$test <- data
  
  res <- do.call(x$model$func, c(
    list(
      "train" = x$model$conditions$train,
      "test" = data,
      "cl" = x$model$conditions$cl
    ),
    x$model$conditions$args
  ))
  
  # check is res has attribute prob
  if (!is.null(attr(res, "prob"))) {
    prob = round(attr(res, "prob"), digits = 1)
  } else {
    prob = NULL
  }
  
  prediction <- data.table::data.table(analysis = rownames(data), class = res)
  prediction$probability <- prob
  x@prediction <- list("results" = prediction, "data" = data)
  x
}

#' @export
#' @noRd
Quantification <- S7::new_class(
  name = "Quantification",
  package = "StreamFind",
  parent = Results,
  
  properties = list(
    compounds = S7::new_property(S7::class_character, default = character()),
    models = S7::new_property(S7::class_list, default = list()),
    quantities = S7::new_property(S7::class_list, default = list())
  ),
  
  constructor = function(compounds = character(), models = list(), quantities = list()) {
    S7::new_object(
      Results(), 
      name = "Quantification",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      compounds = compounds,
      models = models,
      quantities = quantities
    )
  },
  
  validator = function(self) {
    checkmate::assert_true(self@name == "Quantification")
    checkmate::assert_true(self@software == "StreamFind")
    checkmate::assert_character(self@compounds)
    checkmate::assert_list(self@models)
    checkmate::assert_list(self@quantities)
    NULL
  }
)

#' @export
#' @noRd
S7::method(show, Quantification) <- function(x) {
  cat("Quantification results\n")
  cat("Compounds: ", paste(x@compounds, collapse = ", "), "\n")
  cat("Models: ", length(x@models), "\n")
  cat("Quantities: ", length(x@quantities))
}

#' @export
#' @noRd
S7::method(plot, Quantification) <- function(x, onlyCalibration = TRUE) {
  
  if (length(x@quantities) == 0) {
    stop("No quantities to plot.")
  }
  
  fig <- plotly::plot_ly()
  
  for (i in seq_along(x@quantities)) {
    
    model <- x@models[[i]]
    quant <- x@quantities[[i]]
    
    if (nrow(quant) == 0) {
      next
    }
    
    name_calibration <- paste0("Calibration ", names(x@quantities[i]), " (", round(summary(model)[["r.squared"]], 4), ")")
    model_analyses <- names(model$fitted.values)
    model_sel <- x@quantities[[i]]$analysis %in% model_analyses
    model_concentration <- x@quantities[[i]]$concentration[model_sel]
    model_intensities <- x@quantities[[i]]$intensity[model_sel]
    
    if (!onlyCalibration) {
      quant_sel <- !x@quantities[[i]]$analysis %in% model_analyses
      quant_analyses <- x@quantities[[i]]$analysis[quant_sel]
      quant_concentration <- x@quantities[[i]]$concentration[quant_sel]
      quant_intensities <- x@quantities[[i]]$intensity[quant_sel]
      
      cols <- .get_colors(c(name_calibration, paste0(quant_analyses, " (", names(x@quantities[i]), ")")))
    } else {
      cols <- .get_colors(name_calibration)
    }
    
    fig <- fig %>% plotly::add_trace(
      x = model_concentration,
      y = model_intensities,
      name = name_calibration,
      legendgroup = name_calibration,
      type = "scatter",
      mode = "markers",
      marker = list(size = 10, color = "black", opacity = 0.5)
    )
    
    y_vals <- seq(min(model_intensities), max(model_intensities), length.out = 50)
    x_vals <- stats::predict(model, newdata = data.frame(y = y_vals))
    
    fig <- fig %>% plotly::add_trace(
      x = x_vals,
      y = y_vals,
      name = name_calibration,
      legendgroup = name_calibration,
      showlegend = FALSE,
      type = "scatter",
      mode = "lines",
      line = list(color = "black", width = 2, dash = "dash")
    )
    
    if (!onlyCalibration) {
      for (j in seq_along(quant_analyses)) {
        fig <- fig %>% plotly::add_trace(
          x = quant_concentration[j],
          y = quant_intensities[j],
          name = names(cols[j + 1]),
          type = "scatter",
          mode = "markers",
          marker = list(size = 10, color = cols[j + 1], opacity = 0.5)
        )
      }
    }
  }
  
  fig <- fig %>% plotly::layout(
    xaxis = list(title = "Concentration"),
    yaxis = list(title = "Intensity"),
    showlegend = TRUE
  )
  
  fig
}
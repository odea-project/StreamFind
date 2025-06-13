#' @title Method for Quantification using an MCR-ALS model
#' 
#' @description Quantify data using the mcrals statistical model and concentrations.
#' 
#' @param regression A character string indicating the type of regression to be used. Default is "linear". Possible values are "linear", "poly2" or "poly3".
#' @param concentrations Numeric of the same length as analyses with the concentrations to be used for quantification.
#' When concentration is not known or not available, use `NA_real_`.
#' 
#' @return A StatisticMethod_Quantify_mcrals object.
#'
#' @export
#' 
StatisticMethod_Quantify_mcrals <- S7::new_class(
  "StatisticMethod_Quantify_mcrals",
  parent = ProcessingStep,
  package = "StreamFind",
  constructor = function(regression = "linear", concentrations = NA_real_) {
    S7::new_object(
      ProcessingStep(
        data_type = "Statistic",
        method = "Quantify",
        required = "MakeModel",
        algorithm = "mcrals",
        parameters = list(
          regression = regression,
          concentrations = concentrations
        ),
        number_permitted = 1,
        version = as.character(packageVersion("StreamFind")),
        software = "mdatools",
        developer = "Sergey Kucheryavskiy",
        contact = "svk@bio.aau.dk",
        link = "https://github.com/svkucheryavski/mdatools",
        doi = "10.1016/j.chemolab.2020.103937"
      )
    )
  },
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "Statistic")
    checkmate::assert_choice(self@method, "Quantify")
    checkmate::assert_choice(self@algorithm, "mcrals")
    checkmate::assert_choice(self@required, "MakeModel")
    checkmate::assert_choice(self@number_permitted, 1)
    checkmate::assert_choice(self@parameters$regression, c("linear", "poly2", "poly3"))
    checkmate::assert_numeric(self@parameters$concentrations)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, StatisticMethod_Quantify_mcrals) <- function(x, engine = NULL) {
  
  if (!is(engine, "StatisticEngine")) {
    warning("Engine is not a StatisticEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$Analyses$has_model) {
    warning("No model available! Not done.")
    return(FALSE)
  }
  
  if (!is(engine$Analyses$model, "StreamFind::MCRALS")) {
    warning("Model is not StreamFind::MCRALS! Not done.")
    return(FALSE)
  }
  
  parameters <- x$parameters
  regression <- parameters$regression
  concentrations <- parameters$concentrations
  
  if (all(is.na(concentrations))) {
    if (length(engine$Analyses$concentrations) == 0) {
      warning("No concentrations available! Not done.")
      return(FALSE)
    } else {
      concentrations <- engine$Analyses$concentrations
    }
  } else if (length(concentrations) != length(engine$Analyses)) {
    warning("Concentrations length does not match the number of analyses! Not done.")
    return(FALSE)
  } else {
    engine$add_concentrations(concentrations)
  }
  
  model_data <- get_model_data(engine$model)
  
  res <- StreamFind::Quantification()
  
  for (i in seq_len(ncol(model_data$contribution) - 2)) {
    compound <- colnames(model_data$contribution)[i]
    c_i <- concentrations
    x <- c_i[!is.na(c_i)]
    y <- model_data$contribution[[i]][!is.na(c_i)]
    
    if (length(x) < 2 || length(y) < 2) {
      warning(paste0("Not enough data to fit a model for ", compound, "! Not done."))
      next
    }
    
    if (length(x) != length(y)) {
      warning(paste0("Length of x and y do not match for ", compound, "! Not done."))
      next
    }
    
    if (any(is.na(x)) || any(is.na(y))) {
      warning(paste0("NA values in x or y for ", compound, "! Not done."))
      next
    }
    
    if (any(x < 0) || any(y < 0)) {
      warning(paste0("Negative values in x or y for ", compound, "! Not done."))
      next
    }
    
    if (regression == "linear") {
      model <- lm(x ~ y)
    } else if (regression == "poly2") {
      model <- lm(x ~ poly(y, 2, raw = TRUE))
    } else if (regression == "poly3") {
      model <- lm(x ~ poly(y, 3, raw = TRUE))
    } else {
      stop("Unknown regression type!")
    }
    
    summary_model <- summary(model)
    r_squared <- summary_model$r.squared
    to_quantify <- is.na(c_i)
    for (j in seq_len(length(c_i))) {
      if (to_quantify[j]) {
        mcr_val <- data.frame(y = model_data$contribution[[i]][j])
        c_i[j] <- stats::predict(model, newdata = mcr_val)
      }
    }
    
    res@compounds <- c(res@compounds, compound)
    
    res@models[[compound]] <- model
    
    res@quantities[[compound]] <- data.table::data.table(
      "result" = model_data$contribution$result,
      "analysis" = model_data$contribution$analysis,
      "intensity" = model_data$contribution[[i]],
      "concentration" = c_i,
      "rsquared" = r_squared,
      "exp_variance" = round(model_data$explained_variance$expvar[i], 1)
    )
  }
  
  engine$quantification <- res
  message(paste0("\U2713 ", "Quantification results added!"))
  TRUE
}

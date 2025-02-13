
# ______________________________________________________________________________________________________________________
# mcrals -----
# ______________________________________________________________________________________________________________________

#' **StatisticMethod_Quantify_mcrals**
#'
#' @title StatisticMethod_Quantify_mcrals
#' 
#' @description Quantify data using the mcrals statistical model and concentrations.
#' 
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
  constructor = function(concentrations = NA_real_) {
    S7::new_object(
      ProcessingStep(
        engine = "Statistic",
        method = "Quantify",
        required = "MakeModel",
        algorithm = "mcrals",
        parameters = list(
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
    checkmate::assert_choice(self@engine, "Statistic")
    checkmate::assert_choice(self@method, "Quantify")
    checkmate::assert_choice(self@algorithm, "mcrals")
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
    linear_model <- lm(x  ~  y)
    summary_linear_model <- summary(linear_model)
    r_squared <- summary_linear_model$r.squared
    to_quantify <- is.na(c_i)
    for (j in seq_len(length(c_i))) {
      if (to_quantify[j]) {
        mcr_val <- data.frame(y = model_data$contribution[[i]][j])
        c_i[j] <- stats::predict(linear_model, newdata = mcr_val)
      }
    }
    
    res@compounds <- c(res@compounds, compound)
    
    res@models[[compound]] <- linear_model
    
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

#' MassSpecMethod_CorrectSpectraBaseline_baseline_als S7 class
#'
#' @description Performs baseline correction to spectra using the Asymmetric Least Squares (ALS) 
#' algorithm from the \pkg{baseline} package.
#' 
#' @param lambda Numeric (length 1) with the 2nd derivative constraint.
#' @param p Numeric (length 1) with the weighting of positive residuals.
#' @param maxit Integer (length 1) with the maximum number of iterations.
#'
#' @return A MassSpecMethod_CorrectSpectraBaseline_baseline_als object.
#'
#' @export
#'
MassSpecMethod_CorrectSpectraBaseline_baseline_als <- S7::new_class(
  name = "MassSpecMethod_CorrectSpectraBaseline_baseline_als",
  parent = S7::new_S3_class("ProcessingStep"),
  package = "StreamFind",
  
  constructor = function(lambda = 5, p = 0.05, maxit = 10) {
    
    S7::new_object(
      ProcessingStep(
        data_type = "MassSpec",
        method = "CorrectSpectraBaseline",
        required = "LoadSpectra",
        algorithm = "baseline_als",
        parameters = list(
          lambda = lambda,
          p = p,
          maxit = maxit
        ),
        number_permitted = Inf,
        version = as.character(packageVersion("StreamFind")),
        software = "baseline",
        developer = "Kristian Hovde Liland",
        contact = "kristian.liland@nmbu.no",
        link = "https://github.com/khliland/baseline/",
        doi = "10.1366/000370210792434350"
      )
    )
  },
  
  validator = function(self) {
    checkmate::test_choice(self@data_type, "MassSpec")
    checkmate::test_choice(self@method, "CorrectSpectraBaseline")
    checkmate::test_choice(self@algorithm, "baseline_als")
    checkmate::assert_number(self@parameters$lambda)
    checkmate::assert_number(self@parameters$p)
    checkmate::assert_integer(as.integer(self@parameters$maxit))
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_CorrectSpectraBaseline_baseline_als) <- function(x, engine = NULL) {
  
  if (!requireNamespace("baseline", quietly = TRUE)) {
    warning("Package baseline not found but required! Not done.")
    return(FALSE)
  }
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$Analyses$has_spectra) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  baseline_method <- "als"
  
  baseline_args <- list(
    lambda = x$parameters$lambda,
    p = x$parameters$p,
    maxit = x$parameters$maxit
  )
  
  spec_list <- engine$Spectra$spectra
  
  spec_list <- lapply(spec_list, function(x, baseline_method, baseline_args) {
    
    if (nrow(x) > 0) {
      
      if ("rt" %in% colnames(x)) {
        temp_x <- split(x, x$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          baseline_data <- .baseline_correction(z$intensity, baseline_method, baseline_args)
          z$baseline <- baseline_data$baseline
          z$raw <- z$intensity
          z$intensity <- baseline_data$corrected
          z
        })
        
        x <- data.table::rbindlist(temp_x)
        
      } else {
        baseline_data <- .baseline_correction(x$intensity, baseline_method, baseline_args)
        x$baseline <- baseline_data$baseline
        x$raw <- x$intensity
        x$intensity <- baseline_data$corrected
      }
    }
    
    x
    
  }, baseline_method = baseline_method, baseline_args = baseline_args)
  
  engine$Spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra beseline corrected!"))
  TRUE
}

#' MassSpecMethod_CorrectSpectraBaseline_airpls S7 class
#'
#' @description Performs baseline correction using adaptive iteratively reweighted Penalized Least
#' Squares (airPLS) based on the algorithm from Zhi-Min Zhang.
#' 
#' @param lambda Numeric (length 1) with the smoothing intensity. the higher the `lambda` the higher
#' the smoothing.
#' @param differences Integer (length 1) indicating the order of the difference of penalties
#' @param itermax Integer (length 1) with the maximum number of iterations.
#'
#' @return A MassSpecMethod_CorrectSpectraBaseline_airpls object.
#' 
#' @references
#' 
#' \insertRef{airpls01}{StreamFind}
#'
#' @export
#'
MassSpecMethod_CorrectSpectraBaseline_airpls <- S7::new_class(
  name = "MassSpecMethod_CorrectSpectraBaseline_airpls",
  parent = S7::new_S3_class("ProcessingStep"),
  package = "StreamFind",
  
  constructor = function(lambda = 10, differences = 1, itermax = 20) {
    S7::new_object(
      ProcessingStep(
        data_type = "MassSpec",
        method = "CorrectSpectraBaseline",
        required = "LoadSpectra",
        algorithm = "airpls",
        parameters = list(lambda = lambda, differences = differences, itermax = itermax),
        number_permitted = Inf,
        version = as.character(packageVersion("StreamFind")),
        software = "airPLS",
        developer = "Zhi-Min Zhang",
        contact = "zmzhang@csu.edu.cn",
        link = "https://github.com/zmzhang/airPLS",
        doi = "10.1039/b922045c"
      )
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "MassSpec")
    checkmate::assert_choice(self@method, "CorrectSpectraBaseline")
    checkmate::assert_choice(self@algorithm, "airpls")
    checkmate::assert_number(self@parameters$lambda)
    checkmate::assert_integer(as.integer(self@parameters$differences))
    checkmate::assert_integer(as.integer(self@parameters$itermax))
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_CorrectSpectraBaseline_airpls) <- function(x, engine = NULL) {
  
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    warning("Package Matrix not found but required! Not done.")
    return(FALSE)
  }
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_results_chromatograms()) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  lambda = x$parameters$lambda
  differences = x$parameters$differences
  itermax = x$parameters$itermax
  
  spec_list <- engine$Spectra$spectra
  
  spec_list <- lapply(spec_list, function(z, lambda, differences, itermax) {
    
    if (nrow(z) > 0) {
      
      if ("rt" %in% colnames(z)) {
        temp_x <- split(z, z$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          baseline_data <- .airPLS_by_zmzhang(z$intensity, lambda, differences, itermax)
          z$baseline <- baseline_data
          z$raw <- z$intensity
          baseline_data[baseline_data > z$intensity] <- z$intensity[baseline_data > z$intensity]
          z$intensity <- z$intensity - baseline_data
          z
        })
        
        z <- rbindlist(temp_x)
        
      } else {
        baseline_data <- .airPLS_by_zmzhang(z$intensity, lambda, differences, itermax)
        z$baseline <- baseline_data
        z$raw <- z$intensity
        baseline_data[baseline_data > z$intensity] <- z$intensity[baseline_data > z$intensity]
        z$intensity <- z$intensity - baseline_data
      }
    }
    
    z
    
  }, lambda = lambda, differences = differences, itermax = itermax)
  
  engine$Spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra beseline corrected!"))
  TRUE
}

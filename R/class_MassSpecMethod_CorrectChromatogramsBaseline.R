#' **MassSpecMethod_CorrectChromatogramsBaseline_baseline_als**
#'
#' @description Performs baseline correction to chromatograms using the Asymmetric Least Squares
#' (ALS) algorithm from the \pkg{baseline} package.
#' 
#' @param lambda Numeric (length 1) with the 2nd derivative constraint.
#' @param p Numeric (length 1) with the weighting of positive residuals.
#' @param maxit Integer (length 1) with the maximum number of iterations.
#'
#' @return A MassSpecMethod_CorrectChromatogramsBaseline_baseline_als object.
#'
#' @export
#'
MassSpecMethod_CorrectChromatogramsBaseline_baseline_als <- S7::new_class(
  name = "MassSpecMethod_CorrectChromatogramsBaseline_baseline_als",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(lambda = 5,
                         p = 0.05,
                         maxit = 10) {
    S7::new_object(
      ProcessingStep(
        engine = "MassSpec",
        method = "CorrectChromatogramsBaseline",
        required = "LoadChromatograms",
        algorithm = "baseline_als",
        parameters = list(
          lambda = as.numeric(lambda),
          p = as.numeric(p),
          maxit = as.numeric(maxit)
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
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "CorrectChromatogramsBaseline")
    checkmate::assert_choice(self@algorithm, "baseline_als")
    checkmate::assert_number(self@parameters$lambda)
    checkmate::assert_number(self@parameters$p)
    checkmate::assert_integer(as.integer(self@parameters$maxit))
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_CorrectChromatogramsBaseline_baseline_als) <- function(x, engine = NULL) {
  
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
  
  if (!engine$has_results_chromatograms()) {
    warning("No chromatograms results object available! Not done.")
    return(FALSE)
  }
  
  baseline_method <- "als"
  
  baseline_args <- list(
    lambda = x$parameters$lambda,
    p = x$parameters$p,
    maxit = x$parameters$maxit
  )
  
  chrom_list <- engine$Chromatograms$chromatograms
  
  chrom_list <- lapply(chrom_list, function(z, baseline_method, baseline_args) {
    
    if (nrow(z) > 0) {
      
      if ("id" %in% colnames(z)) {
        temp_x <- split(z, z$id)
        
        temp_x <- lapply(temp_x, function(z) {
          baseline_data <- .baseline_correction(z$intensity, baseline_method, baseline_args)
          z$baseline <- baseline_data$baseline
          z$raw <- z$intensity
          z$intensity <- baseline_data$corrected
          z
        })
        
        z <- data.table::rbindlist(temp_x)
        
      } else {
        baseline_data <- .baseline_correction(z$intensity, baseline_method, baseline_args)
        z$baseline <- baseline_data$baseline
        z$raw <- z$intensity
        z$intensity <- baseline_data$corrected
      }
    }
    
    z
    
  }, baseline_method = baseline_method, baseline_args = baseline_args)
  
  engine$Chromatograms$chromatograms <- chrom_list
  message(paste0("\U2713 ", "Chromatograms beseline corrected!"))
  TRUE
}

#' **MassSpecMethod_CorrectChromatogramsBaseline_airpls**
#'
#' @description Performs baseline correction using adaptive iteratively reweighted Penalized Least
#' Squares (airPLS) based on the algorithm from Zhi-Min Zhang.
#' 
#' @param lambda Numeric (length 1) with the smoothing intensity. the higher the `lambda` the
#' higher the smoothing.
#' @param differences Integer (length 1) indicating the order of the difference of penalties
#' @param itermax Integer (length 1) with the maximum number of iterations.
#'
#' @return A MassSpecMethod_CorrectChromatogramsBaseline_airpls object.
#' 
#' @references
#' 
#' \insertRef{airpls01}{StreamFind}
#'
#' @export
#'
MassSpecMethod_CorrectChromatogramsBaseline_airpls <- S7::new_class(
  name = "MassSpecMethod_CorrectChromatogramsBaseline_airpls",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(lambda = 10,
                         differences = 1,
                         itermax = 20) {
    S7::new_object(
      ProcessingStep(
        engine = "MassSpec",
        method = "CorrectChromatogramsBaseline",
        required = "LoadChromatograms",
        algorithm = "airpls",
        parameters = list(
          lambda = as.numeric(lambda),
          differences = as.numeric(differences),
          itermax = as.numeric(itermax)
        ),
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
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "CorrectChromatogramsBaseline")
    checkmate::assert_choice(self@algorithm, "airpls")
    checkmate::assert_number(self@parameters$lambda)
    checkmate::assert_integer(as.integer(self@parameters$differences))
    checkmate::assert_integer(as.integer(self@parameters$itermax))
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_CorrectChromatogramsBaseline_airpls) <- function(x, engine = NULL) {
  
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
    warning("No chromatograms results object available! Not done.")
    return(FALSE)
  }
  
  lambda = x$parameters$lambda
  differences = x$parameters$differences
  itermax = x$parameters$itermax
  
  chrom_list <- engine$Chromatograms$chromatograms
  
  chrom_list <- lapply(chrom_list, function(z, lambda, differences, itermax) {
    
    if (nrow(z) > 0) {
      
      if ("id" %in% colnames(z)) {
        temp_x <- split(z, z$id)
        
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
  
  engine$Chromatograms$chromatograms <- chrom_list
  message(paste0("\U2713 ", "Chromatograms beseline corrected!"))
  TRUE
}

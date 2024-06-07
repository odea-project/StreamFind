
#' @title .baseline_correction
#' 
#' @description Corrects the baseline from a vector using the baseline package.
#' 
#' @noRd
#' 
.baseline_correction <- function(vec, method, opts) {
  
  mat <- matrix(as.numeric(vec), nrow = 1)
  
  mat <- do.call(baseline::baseline, c(list(spectra = mat, method = method), opts))
  
  i_baseline <- as.numeric(mat@baseline)
  
  i_corrected <- as.numeric(mat@corrected)
  
  # i_baseline[i_baseline > vec] <- vec[i_baseline > vec]
  # 
  # i_baseline[i_baseline < 0] <- vec[i_baseline < 0]
  # 
  # i_corrected <- vec - i_baseline
  # 
  # i_corrected[i_corrected < 0] <- 0
  
  list("mat" = mat, "baseline" = i_baseline, "corrected" = i_corrected)
}

#' @title .s3_correct_spectra_baseline.Settings_correct_spectra_baseline_baseline
#'
#' @description Corrects baseline from spectra.
#'
#' @noRd
#'
.s3_correct_spectra_baseline.Settings_correct_spectra_baseline_baseline <- function(settings, self, private) {
  
  if (!requireNamespace("baseline", quietly = TRUE)) {
    warning("Package baseline not found but required! Not done.")
    return(FALSE)
  }
  
  baseline_method <- settings$parameters$method
  
  baseline_args <- settings$parameters$args
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
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
        
        x <- rbindlist(temp_x)
        
      } else {
        baseline_data <- .baseline_correction(x$intensity, baseline_method, baseline_args)
        x$baseline <- baseline_data$baseline
        x$raw <- x$intensity
        x$intensity <- baseline_data$corrected
      }
    }
    
    x
    
  }, baseline_method = baseline_method, baseline_args = baseline_args)
  
  self$spectra <- spec_list
  
  message(paste0("\U2713 ", "Spectra beseline corrected!"))
  
  TRUE
}

.WhittakerSmooth_by_zmzhang <- function(x, w, lambda, differences = 1) {
  x=matrix(x,nrow = 1, ncol=length(x))
  L=length(x)
  E=Matrix::spMatrix(L,L,i=seq(1,L),j=seq(1,L),rep(1,L))
  D=as(Matrix::diff(E,1,differences),"CsparseMatrix")
  W=as(Matrix::spMatrix(L,L,i=seq(1,L),j=seq(1,L),w),"CsparseMatrix")
  background=Matrix::solve((W+lambda*Matrix::t(D)%*%D),Matrix::t((w*x)));
  return(as.vector(background))
}

.airPLS_by_zmzhang <- function(x, lambda = 10, differences = 1, itermax = 20){
  x = as.vector(x)
  m = length(x)
  w = rep(1,m)
  control = 1
  i = 1
  while(control==1){
    z = .WhittakerSmooth_by_zmzhang(x,w,lambda,differences)
    d = x-z
    sum_smaller = abs(sum(d[d<0])) 
    if(sum_smaller<0.001*sum(abs(x))||i==itermax)
    {
      control = 0
    }
    w[d>=0] = 0
    w[d<0] = exp(i*abs(d[d<0])/sum_smaller)
    w[1] = exp(i*max(d[d<0])/sum_smaller)
    w[m] = exp(i*max(d[d<0])/sum_smaller)
    i=i+1
  }
  return(z)
}

#' @title .s3_correct_spectra_baseline.Settings_correct_spectra_baseline_airpls
#'
#' @description Corrects baseline from spectra using airPLS.
#'
#' @noRd
#'
.s3_correct_spectra_baseline.Settings_correct_spectra_baseline_airpls <- function(settings, self, private) {
  
  lambda = settings$parameters$lambda
  
  differences = settings$parameters$differences
  
  itermax = settings$parameters$itermax
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    warning("Package Matrix not found but required! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
  spec_list <- lapply(spec_list, function(x, lambda, differences, itermax) {
    
    if (nrow(x) > 0) {
      
      if ("rt" %in% colnames(x)) {
        temp_x <- split(x, x$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          baseline_data <- .airPLS_by_zmzhang(z$intensity, lambda, differences, itermax)
          z$baseline <- baseline_data
          z$raw <- z$intensity
          baseline_data[baseline_data > z$intensity] <- z$intensity[baseline_data > z$intensity]
          z$intensity <- z$intensity - baseline_data
          z
        })
        
        x <- rbindlist(temp_x)
        
      } else {
        baseline_data <- .airPLS_by_zmzhang(x$intensity, lambda, differences, itermax)
        x$baseline <- baseline_data
        x$raw <- x$intensity
        baseline_data[baseline_data > x$intensity] <- x$intensity[baseline_data > x$intensity]
        x$intensity <- x$intensity - baseline_data
      }
    }
    
    x
    
  }, lambda = lambda, differences = differences, itermax = itermax)
  
  self$spectra <- spec_list
  
  message(paste0("\U2713 ", "Spectra beseline corrected!"))
  
  TRUE
}

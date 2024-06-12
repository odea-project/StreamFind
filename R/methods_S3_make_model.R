
#' @title .s3_make_model.Settings_make_model_pca_mdatools
#'
#' @description Performs a Principle Component Analysis (PCA) on the data based on the \pkg{mdatools}.
#'
#' @noRd
#'
.s3_make_model.Settings_make_model_pca_mdatools <- function(settings, self, private) {
  
  if (!requireNamespace("mdatools", quietly = TRUE)) {
    warning("The package 'mdatools' is not available! Not done.")
    return(FALSE)
  }
  
  mat <- self$data
  
  ncomp = settings$parameters$ncomp
  
  if (is.null(ncomp)) ncomp = min(nrow(mat) - 1, ncol(mat), 20)
  
  center <- FALSE
  scale <- FALSE
  exclrows <- settings$parameters$exclrows
  exclcols <- settings$parameters$exclcols
  x.test <- settings$parameters$x.test
  method <- settings$parameters$method
  rand <- settings$parameters$rand
  lim.type <- settings$parameters$lim.type
  alpha <- settings$parameters$alpha
  gamma <- settings$parameters$gamma
  info <- settings$parameters$info
  
  m <- mdatools::pca(
    x = mat, ncomp = ncomp, center = center, scale = scale, exclrows = exclrows, exclcols = exclcols, x.test = x.test, 
    method = method, rand = rand, lim.type = lim.type, alpha = alpha, gamma = gamma, info = info
  )
  
  self$model <- m
  
  message(paste0("\U2713 ", "PCA model added!"))
  
  TRUE
}

#' @title .s3_make_model.Settings_make_model_mcrpure_mdatools
#'
#' @description Performs a Multivariate Curve Resolution (MCR) for purity evaluation on the data based on the \pkg{mdatools}.
#'
#' @noRd
#'
.s3_make_model.Settings_make_model_mcrpure_mdatools <- function(settings, self, private) {
  
  if (!requireNamespace("mdatools", quietly = TRUE)) {
    warning("The package 'mdatools' is not available! Not done.")
    return(FALSE)
  }
  
  mat <- self$data
  
  if (nrow(mat) < 2) {
    warning("The data matrix must have at least 2 rows! Not done.")
    return(FALSE)
  }
  
  ncomp = settings$parameters$ncomp
  
  if (is.null(ncomp)) ncomp = min(nrow(mat) - 1, ncol(mat), 20)
  
  purevars <- settings$parameters$purevars 
  offset <- settings$parameters$offset
  exclrows <- settings$parameters$exclrows
  exclcols <- settings$parameters$exclcols
  info <- settings$parameters$info
  
  m <- mdatools::mcrpure(
    x = mat,
    ncomp = ncomp,
    purevars = purevars,
    offset = offset,
    exclrows = exclrows,
    exclcols = exclcols,
    info = info
  )
  
  self$model <- m
  
  message(paste0("\U2713 ", "MCR purity model added!"))
  
  TRUE
}

#' @title .s3_make_model.Settings_make_model_mcrals_mdatools
#'
#' @description Performs a Multivariate Curve Resolution (MCR) using Alternating Least Squares (ALS) based on the 
#' \pkg{mdatools}.
#'
#' @noRd
#'
.s3_make_model.Settings_make_model_mcrals_mdatools <- function(settings, self, private) {
  
  if (!requireNamespace("mdatools", quietly = TRUE)) {
    warning("The package 'mdatools' is not available! Not done.")
    return(FALSE)
  }
  
  mat <- self$data
  
  if (nrow(mat) < 2) {
    warning("The data matrix must have at least 2 rows! Not done.")
    return(FALSE)
  }
  
  ncomp = settings$parameters$ncomp
  
  if (is.null(ncomp)) ncomp = min(nrow(mat) - 1, ncol(mat), 20)
  
  spec.ini <- matrix(runif(ncol(mat) * ncomp), ncol(mat), ncomp)
  cont.forced <- matrix(NA, nrow(mat), ncomp)
  spec.forced <- matrix(NA, ncol(mat), ncomp)
  
  if (settings$parameters$cont.solver == "mcrals.nnls") {
    cont.solver <- mdatools::mcrals.nnls
  } else if (settings$parameters$cont.solver == "mcrals.ols"){
    cont.solver <- mdatools::mcrals.ols
  }
  
  if (settings$parameters$spec.solver == "mcrals.nnls") {
    spec.solver <- mdatools::mcrals.nnls
  } else if (settings$parameters$spec.solver == "mcrals.ols"){
    spec.solver <- mdatools::mcrals.ols
  }
  
  cont.constraints <- settings$parameters$cont.constraints
  spec.constraints <- settings$parameters$spec.constraints
  exclrows <- settings$parameters$exclrows
  exclcols <- settings$parameters$exclcols
  verbose <- settings$parameters$verbose
  max.niter <- settings$parameters$max.niter
  tol <- settings$parameters$tol
  info <- settings$parameters$info
  
  m <- mdatools::mcrals(
    x = mat,
    ncomp = ncomp,
    cont.constraints = cont.constraints,
    spec.constraints = spec.constraints,
    spec.ini = spec.ini,
    cont.forced = cont.forced,
    spec.forced = spec.forced,
    cont.solver = cont.solver,
    spec.solver = spec.solver,
    exclrows = exclrows,
    exclcols = exclcols,
    verbose = verbose,
    max.niter = max.niter,
    tol = tol,
    info = info
  )
  
  self$model <- m
  
  message(paste0("\U2713 ", "MCR-ALS model added!"))
  
  TRUE
}

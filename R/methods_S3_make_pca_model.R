
#' @title .s3_make_pca_model.Settings_make_pca_model_mdatools
#'
#' @description Performs a Principle Component Analysis (PCA) on the data based on the \pkg{mdatools}.
#'
#' @noRd
#'
.s3_make_pca_model.Settings_make_pca_model_mdatools <- function(settings, self, private) {
  
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

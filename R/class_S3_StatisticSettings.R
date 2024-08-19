
# ______________________________________________________________________________________________________________________
# MakeModel -----
# ______________________________________________________________________________________________________________________

#' @title StatisticSettings_MakeModel_pca_mdatools
#'
#' @description Makes a Principle Component Analysis (PCA) model based on the R package \pkg{mdatools}.
#' 
#' @param ncomp Integer (length 1) with the number of components to be calculated.
#' @param exclrows Integer vector with the row indices to be excluded.
#' @param exclcols Integer vector with the column indices to be excluded.
#' @param x.test Matrix with the test data.
#' @param method Character (length 1) with the method to be used for PCA. Possible values are "svd" and "nipals".
#' @param rand Integer (length 1) with the random seed.
#' @param lim.type Character (length 1) with the type of limit for the PCA. Possible values are "jm" Jackson-Mudholkar 
#' approach, "chisq" based on chi-square distribution, "ddmoments" and "ddrobust" related to data driven method. It is
#' highly recommended to concult the original documentation in \link[mdatools]{pca} for more information.
#' @param alpha Numeric (length 1) with the alpha value for the PCA.
#' @param gamma Numeric (length 1) with the gamma value for the PCA.
#' @param info Character (length 1) with additional information.
#' 
#' @references
#' \insertRef{mdatools01}{StreamFind}
#'
#' @return A ProcessingSettings S3 class object with subclass StatisticSettings_MakeModel_pca_mdatools.
#'
#' @export
#'
StatisticSettings_MakeModel_pca_mdatools <- function(ncomp = NULL,
                                                     exclrows = NULL,
                                                     exclcols = NULL,
                                                     x.test = NULL,
                                                     method = "svd",
                                                     rand = NULL,
                                                     lim.type = "ddmoments",
                                                     alpha = 0.05,
                                                     gamma = 0.01,
                                                     info = "") {
  
  settings <- list(
    engine = "Statistic",
    call = "MakeModel",
    algorithm = "pca_mdatools",
    parameters = list(
      ncomp = ncomp,
      exclrows = exclrows,
      exclcols = exclcols,
      x.test = x.test,
      method = method,
      rand = rand,
      lim.type = lim.type,
      alpha = alpha,
      gamma = gamma,
      info = info
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "mdatools",
    developer = "Sergey Kucheryavskiy",
    contact = "svk@bio.aau.dk",
    link = "https://github.com/svkucheryavski/mdatools",
    doi = "10.1016/j.chemolab.2020.103937"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.StatisticSettings_MakeModel_pca_mdatools <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Statistic"),
    checkmate::test_choice(x$call, "MakeModel"),
    checkmate::test_choice(x$algorithm, "pca_mdatools"),
    checkmate::test_number(x$parameters$ncomp),
    checkmate::test_integer(x$parameters$exclrows),
    checkmate::test_integer(x$parameters$exclcols),
    checkmate::test_matrix(x$parameters$x.test),
    checkmate::test_choice(x$parameters$method, c("svd", "nipals")),
    checkmate::test_integer(x$parameters$rand),
    checkmate::test_choice(x$parameters$lim.type, c("jm", "chisq", "ddmoments", "ddrobust")),
    checkmate::test_number(x$parameters$alpha),
    checkmate::test_number(x$parameters$gamma),
    checkmate::test_character(x$parameters$info)
  )
}

#' @title StatisticSettings_MakeModel_mcrpure_mdatools
#'
#' @description Makes a Multivariate Curve Resolution (MCR) purity model based on the R package \pkg{mdatools}.
#' 
#' @param ncomp Integer (length 1) with the number of components to be calculated.
#' @param purevars Integer vector with the indices of the pure variables (optional).
#' @param offset Numeric (length 1) offset for correcting noise in computing maximum angles (should be value within 0 and 1).
#' @param exclrows Integer vector with the row indices to be excluded.
#' @param exclcols Integer vector with the column indices to be excluded.
#' @param info Character (length 1) with additional information.
#' 
#' @references
#' \insertRef{mdatools01}{StreamFind}
#' 
#' \insertRef{mdatools02}{StreamFind}
#'
#' @return A ProcessingSettings S3 class object with subclass StatisticSettings_MakeModel_mcrpure_mdatools.
#'
#' @export
#'
StatisticSettings_MakeModel_mcrpure_mdatools <- function(ncomp = NULL,
                                                         purevars = NULL,
                                                         offset = 0.05,
                                                         exclrows = NULL,
                                                         exclcols = NULL,
                                                         info = "") {
  
  settings <- list(
    engine = "Statistic",
    call = "MakeModel",
    algorithm = "mcrpure_mdatools",
    parameters = list(
      ncomp = ncomp,
      purevars = purevars,
      offset = offset,
      exclrows = exclrows,
      exclcols = exclcols,
      info = info
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "mdatools",
    developer = "Sergey Kucheryavskiy",
    contact = "svk@bio.aau.dk",
    link = "https://github.com/svkucheryavski/mdatools",
    doi = "10.1016/j.chemolab.2020.103937"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.StatisticSettings_MakeModel_mcrpure_mdatools <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Statistic"),
    checkmate::test_choice(x$call, "MakeModel"),
    checkmate::test_choice(x$algorithm, "mcrpure_mdatools"),
    checkmate::test_number(x$parameters$ncomp),
    checkmate::test_integer(x$parameters$purevars),
    checkmate::test_number(x$parameters$offset),
    checkmate::test_integer(x$parameters$exclrows),
    checkmate::test_integer(x$parameters$exclcols),
    checkmate::test_character(x$parameters$info)
  )
}

#' @title StatisticSettings_MakeModel_mcrals_mdatools
#'
#' @description Makes a Multivariate Curve Resolution (MCR) model using Alternating Least Squares (ALS) based on the 
#' R package \pkg{mdatools}.
#' 
#' @param ncomp Integer (length 1) with the number of components to be calculated.
#' @param cont.constraints List with constraints to be applied to contributions (see details in \link[mdatools]{mcrals}).
#' @param spec.constraints List with constraints to be applied to spectra (see details in \link[mdatools]{mcrals}).
#' @param cont.solver Character (length 1) with the name of the to solve the pure components contributions (see details 
#' in \link[mdatools]{mcrals}).
#' @param spec.solver Character (length 1) with the name of the function to solve the pure components spectra (see 
#' details in \link[mdatools]{mcrals}).
#' @param exclrows Integer vector with the row indices to be excluded.
#' @param exclcols Integer vector with the column indices to be excluded.
#' @param verbose Logical (length 1) indicating if the function should be verbose.
#' @param max.niter Integer (length 1) with the maximum number of iterations.
#' @param tol Numeric (length 1) with the tolerance for convergence.
#' @param info Character (length 1) with additional information.
#' 
#' @note The functionality to define the initial pure components spectra as well as forcing concentration and spectra 
#' values are not included in this integration of \link[mdatools]{mcrals}.
#' 
#' @references
#' \insertRef{mdatools01}{StreamFind}
#' 
#' \insertRef{mdatools02}{StreamFind}
#'
#' @return A ProcessingSettings S3 class object with subclass StatisticSettings_MakeModel_mcrals_mdatools.
#'
#' @export
#'
StatisticSettings_MakeModel_mcrals_mdatools <- function(ncomp = NULL,
                                                        cont.constraints = list(),
                                                        spec.constraints = list(),
                                                        cont.solver = "mcrals.nnls",
                                                        spec.solver = "mcrals.nnls",
                                                        exclrows = NULL,
                                                        exclcols = NULL,
                                                        verbose = FALSE,
                                                        max.niter = 100,
                                                        tol = 10^-6,
                                                        info = "") {
  
  settings <- list(
    engine = "Statistic",
    call = "MakeModel",
    algorithm = "mcrals_mdatools",
    parameters = list(
      ncomp = ncomp,
      cont.constraints = cont.constraints,
      spec.constraints = spec.constraints,
      cont.solver = cont.solver,
      spec.solver = spec.solver,
      exclrows = exclrows,
      exclcols = exclcols,
      verbose = verbose,
      max.niter = max.niter,
      tol = tol,
      info = info
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "mdatools",
    developer = "Sergey Kucheryavskiy",
    contact = "svk@bio.aau.dk",
    link = "https://github.com/svkucheryavski/mdatools",
    doi = "10.1016/j.chemolab.2020.103937"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.StatisticSettings_MakeModel_mcrals_mdatools <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Statistic"),
    checkmate::test_choice(x$call, "MakeModel"),
    checkmate::test_choice(x$algorithm, "mcrals_mdatools"),
    checkmate::test_number(x$parameters$ncomp),
    checkmate::test_list(x$parameters$cont.constraints),
    checkmate::test_list(x$parameters$spec.constraints),
    checkmate::test_choice(x$parameters$cont.solver, c("mcrals.nnls", "mcrals.ols")),
    checkmate::test_choice(x$parameters$spec.solver, c("mcrals.nnls", "mcrals.ols")),
    checkmate::test_integer(x$parameters$exclrows),
    checkmate::test_integer(x$parameters$exclcols),
    checkmate::test_logical(x$parameters$verbose, max.len = 1),
    checkmate::test_number(x$parameters$max.niter),
    checkmate::test_number(x$parameters$tol),
    checkmate::test_character(x$parameters$info)
  )
}





# ______________________________________________________________________________________________________________________
# PrepareClassification -----
# ______________________________________________________________________________________________________________________

#' @title StatisticSettings_PrepareClassification_knn
#' 
#' @description Prepares a classification model using the k-nearest neighbors (knn) algorithm from package \pkg{class}.
#' 
#' @param k Integer (length 1) with the number of neighbors to be used.
#' @param l Integer (length 1) with the minimum vote for definite decision, otherwise doubt.
#' (More precisely, less than k-l dissenting votes are allowed, even if k is increased by ties.)
#' 
#' @references
#' \insertRef{class01}{StreamFind}
#' 
#' @return A ProcessingSettings S3 class object with subclass StatisticSettings_PrepareClassification_knn.
#'
#' @export
#' 
StatisticSettings_PrepareClassification_knn <- function(k = 3, l = 0) {
  
  settings <- list(
    engine = "Statistic",
    call = "PrepareClassification",
    algorithm = "knn",
    parameters = list(
      k = k,
      l = l,
      prob = TRUE
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "class",
    developer = "Brian D. Ripley",
    contact = "ripley@stats.ox.ac.uk",
    link = "https://cran.r-project.org/web/packages/class/index.html",
    doi = "ISBN 0-387-95457-0"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#' 
validate.StatisticSettings_PrepareClassification_knn <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Statistic"),
    checkmate::test_choice(x$call, "PrepareClassification"),
    checkmate::test_choice(x$algorithm, "knn"),
    checkmate::test_number(x$parameters$k),
    checkmate::test_number(x$parameters$l)
  )
}





# ______________________________________________________________________________________________________________________
# PrepareData -----
# ______________________________________________________________________________________________________________________

#' @title StatisticSettings_PrepareData_autoscale
#' 
#' @description Auto scale and centers data using the \code{prep.autoscale} function from the \pkg{mdatools} package.
#' 
#' @param center Logical (length 1) indicating if the data should be centered.
#' @param scale Logical (length 1) indicating if the data should be scaled.
#' 
#' @return A ProcessingSettings S3 class object with subclass StatisticSettings_PrepareData_autoscale.
#'
#' @export
#' 
StatisticSettings_PrepareData_autoscale <- function(center = TRUE, scale = TRUE) {
  
  settings <- list(
    engine = "Statistic",
    call = "PrepareData",
    algorithm = "autoscale",
    parameters = list(
      center = center,
      scale = scale
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "mdatools",
    developer = "Sergey Kucheryavskiy",
    contact = "svk@bio.aau.dk",
    link = "https://github.com/svkucheryavski/mdatools",
    doi = "10.1016/j.chemolab.2020.103937"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#' 
validate.StatisticSettings_PrepareData_autoscale <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Statistic"),
    checkmate::test_choice(x$call, "PrepareData"),
    checkmate::test_choice(x$algorithm, "autoscale"),
    checkmate::test_logical(x$parameters$center, max.len = 1),
    checkmate::test_logical(x$parameters$scale, max.len = 1)
  )
}

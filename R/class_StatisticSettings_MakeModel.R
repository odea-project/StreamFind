
# ______________________________________________________________________________________________________________________
# pca_mdatools -----
# ______________________________________________________________________________________________________________________

#' **StatisticSettings_MakeModel_pca_mdatools**
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
#' @return A StatisticSettings_MakeModel_pca_mdatools object.
#'
#' @export
#'
StatisticSettings_MakeModel_pca_mdatools <- S7::new_class("StatisticSettings_MakeModel_pca_mdatools",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(ncomp = NULL,
                         exclrows = NULL,
                         exclcols = NULL,
                         x.test = NULL,
                         method = "svd",
                         rand = NULL,
                         lim.type = "ddmoments",
                         alpha = 0.05,
                         gamma = 0.01,
                         info = "") {
    
    S7::new_object(ProcessingSettings(
      engine = "Statistic",
      method = "MakeModel",
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
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "mdatools",
      developer = "Sergey Kucheryavskiy",
      contact = "svk@bio.aau.dk",
      link = "https://github.com/svkucheryavski/mdatools",
      doi = "10.1016/j.chemolab.2020.103937"
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "Statistic"),
      checkmate::test_choice(self@method, "MakeModel"),
      checkmate::test_choice(self@algorithm, "pca_mdatools"),
      checkmate::test_number(self@parameters$ncomp, null.ok = TRUE),
      checkmate::test_number(self@parameters$exclrows, null.ok = TRUE),
      checkmate::test_number(self@parameters$exclcols, null.ok = TRUE),
      checkmate::test_matrix(self@parameters$x.test, null.ok = TRUE),
      checkmate::test_choice(self@parameters$method, c("svd", "nipals")),
      checkmate::test_number(self@parameters$rand, null.ok = TRUE),
      checkmate::test_choice(self@parameters$lim.type, c("jm", "chisq", "ddmoments", "ddrobust")),
      checkmate::test_number(self@parameters$alpha),
      checkmate::test_number(self@parameters$gamma),
      checkmate::test_character(self@parameters$info)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, StatisticSettings_MakeModel_pca_mdatools) <- function(x, engine = NULL) {
  
  if (!requireNamespace("mdatools", quietly = TRUE)) {
    warning("The package 'mdatools' is not available! Not done.")
    return(FALSE)
  }
  
  if (!is(engine, "StatisticEngine")) {
    warning("Engine is not a StatisticEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  mat <- engine$data$data
  
  ncomp = x$parameters$ncomp
  if (is.null(ncomp)) ncomp = min(nrow(mat) - 1, ncol(mat), 20)
  center <- FALSE
  scale <- FALSE
  exclrows <- x$parameters$exclrows
  exclcols <- x$parameters$exclcols
  x.test <- x$parameters$x.test
  method <- x$parameters$method
  rand <- x$parameters$rand
  lim.type <- x$parameters$lim.type
  alpha <- x$parameters$alpha
  gamma <- x$parameters$gamma
  info <- x$parameters$info
  
  m <- mdatools::pca(
    x = mat, ncomp = ncomp, center = center, scale = scale, exclrows = exclrows, exclcols = exclcols, x.test = x.test, 
    method = method, rand = rand, lim.type = lim.type, alpha = alpha, gamma = gamma, info = info
  )
  
  engine$model <- StreamFind::PCA(model = m)
  message(paste0("\U2713 ", "PCA model added!"))
  TRUE
}

# ______________________________________________________________________________________________________________________
# mcrpure_mdatools -----
# ______________________________________________________________________________________________________________________

#' **StatisticSettings_MakeModel_mcrpure_mdatools**
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
#' @return A StatisticSettings_MakeModel_mcrpure_mdatools object.
#'
#' @export
#'
StatisticSettings_MakeModel_mcrpure_mdatools <- S7::new_class("StatisticSettings_MakeModel_mcrpure_mdatools",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(ncomp = NULL,
                         purevars = NULL,
                         offset = 0.05,
                         exclrows = NULL,
                         exclcols = NULL,
                         info = "") {
    
    S7::new_object(ProcessingSettings(
      engine = "Statistic",
      method = "MakeModel",
      algorithm = "mcrpure_mdatools",
      parameters = list(
        ncomp = ncomp,
        purevars = purevars,
        offset = offset,
        exclrows = exclrows,
        exclcols = exclcols,
        info = info
      ),
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "mdatools",
      developer = "Sergey Kucheryavskiy",
      contact = "svk@bio.aau.dk",
      link = "https://github.com/svkucheryavski/mdatools",
      doi = "10.1016/j.chemolab.2020.103937"
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "Statistic"),
      checkmate::test_choice(self@method, "MakeModel"),
      checkmate::test_choice(self@algorithm, "mcrpure_mdatools"),
      checkmate::test_number(self@parameters$ncomp, null.ok = TRUE),
      checkmate::test_number(self@parameters$purevars, null.ok = TRUE),
      checkmate::test_number(self@parameters$offset, null.ok = TRUE),
      checkmate::test_number(self@parameters$exclrows, null.ok = TRUE),
      checkmate::test_number(self@parameters$exclcols, null.ok = TRUE),
      checkmate::test_character(self@parameters$info)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, StatisticSettings_MakeModel_mcrpure_mdatools) <- function(x, engine = NULL) {
  
  if (!requireNamespace("mdatools", quietly = TRUE)) {
    warning("The package 'mdatools' is not available! Not done.")
    return(FALSE)
  }
  
  if (!is(engine, "StatisticEngine")) {
    warning("Engine is not a StatisticEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  mat <- engine$data$data
  
  if (nrow(mat) < 2) {
    warning("The data matrix must have at least 2 rows! Not done.")
    return(FALSE)
  }
  
  ncomp = x$parameters$ncomp
  if (is.null(ncomp)) ncomp = min(nrow(mat) - 1, ncol(mat), 20)
  purevars <- x$parameters$purevars 
  offset <- x$parameters$offset
  exclrows <- x$parameters$exclrows
  exclcols <- x$parameters$exclcols
  info <- x$parameters$info
  
  m <- mdatools::mcrpure(
    x = mat,
    ncomp = ncomp,
    purevars = purevars,
    offset = offset,
    exclrows = exclrows,
    exclcols = exclcols,
    info = info
  )
  
  engine$model <- StreamFind::MCRPURE(model = m)
  message(paste0("\U2713 ", "MCR purity model added!"))
  TRUE
}

# ______________________________________________________________________________________________________________________
# mcrals_mdatools -----
# ______________________________________________________________________________________________________________________

#' **StatisticSettings_MakeModel_mcrals_mdatools**
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
#' @return A StatisticSettings_MakeModel_mcrals_mdatools object.
#'
#' @export
#'
StatisticSettings_MakeModel_mcrals_mdatools <- S7::new_class("StatisticSettings_MakeModel_mcrals_mdatools",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(ncomp = NULL,
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
    
    S7::new_object(ProcessingSettings(
      engine = "Statistic",
      method = "MakeModel",
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
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "mdatools",
      developer = "Sergey Kucheryavskiy",
      contact = "svk@bio.aau.dk",
      link = "https://github.com/svkucheryavski/mdatools",
      doi = "10.1016/j.chemolab.2020.103937"
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "Statistic"),
      checkmate::test_choice(self@method, "MakeModel"),
      checkmate::test_choice(self@algorithm, "mcrals_mdatools"),
      checkmate::test_number(self@parameters$ncomp, null.ok = TRUE),
      checkmate::test_list(self@parameters$cont.constraints),
      checkmate::test_list(self@parameters$spec.constraints),
      checkmate::test_choice(self@parameters$cont.solver, c("mcrals.nnls", "mcrals.ols")),
      checkmate::test_choice(self@parameters$spec.solver, c("mcrals.nnls", "mcrals.ols")),
      checkmate::test_number(self@parameters$exclrows, null.ok = TRUE),
      checkmate::test_number(self@parameters$exclcols, null.ok = TRUE),
      checkmate::test_logical(self@parameters$verbose, max.len = 1),
      checkmate::test_number(self@parameters$max.niter),
      checkmate::test_number(self@parameters$tol),
      checkmate::test_character(self@parameters$info)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, StatisticSettings_MakeModel_mcrals_mdatools) <- function(x, engine = NULL) {
  
  if (!requireNamespace("mdatools", quietly = TRUE)) {
    warning("The package 'mdatools' is not available! Not done.")
    return(FALSE)
  }
  
  if (!is(engine, "StatisticEngine")) {
    warning("Engine is not a StatisticEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  mat <- engine$data$data
  
  ncomp = x$parameters$ncomp
  if (is.null(ncomp)) ncomp = min(nrow(mat) - 1, ncol(mat), 20)
  spec.ini <- matrix(runif(ncol(mat) * ncomp), ncol(mat), ncomp)
  cont.forced <- matrix(NA, nrow(mat), ncomp)
  spec.forced <- matrix(NA, ncol(mat), ncomp)
  
  if (x$parameters$cont.solver == "mcrals.nnls") {
    cont.solver <- mdatools::mcrals.nnls
  } else if (x$parameters$cont.solver == "mcrals.ols") {
    cont.solver <- mdatools::mcrals.ols
  }
  
  if (x$parameters$spec.solver == "mcrals.nnls") {
    spec.solver <- mdatools::mcrals.nnls
  } else if (x$parameters$spec.solver == "mcrals.ols") {
    spec.solver <- mdatools::mcrals.ols
  }
  
  cont.constraints <- x$parameters$cont.constraints
  spec.constraints <- x$parameters$spec.constraints
  exclrows <- x$parameters$exclrows
  exclcols <- x$parameters$exclcols
  verbose <- x$parameters$verbose
  max.niter <- x$parameters$max.niter
  tol <- x$parameters$tol
  info <- x$parameters$info
  
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
  
  engine$model <- StreamFind::StatisticModel(model = m)
  message(paste0("\U2713 ", "MCR-ALS model added!"))
  TRUE
}

# ______________________________________________________________________________________________________________________
# knn -----
# ______________________________________________________________________________________________________________________

#' **StatisticSettings_MakeModel_knn**
#'
#' @description Makes a classification model using the k-nearest neighbors (knn) algorithm from package \pkg{class}.
#' 
#' @param k Integer (length 1) with the number of neighbors to be used.
#' @param l Integer (length 1) with the minimum vote for definite decision, otherwise doubt.
#' (More precisely, less than k-l dissenting votes are allowed, even if k is increased by ties.)
#' 
#' @references
#' \insertRef{class01}{StreamFind}
#' 
#' @return A StatisticSettings_MakeModel_knn object.
#'
#' @export
#' 
StatisticSettings_MakeModel_knn <- S7::new_class("StatisticSettings_MakeModel_knn",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(k = 3, l = 0) {
   
    S7::new_object(ProcessingSettings(
      engine = "Statistic",
      method = "MakeModel",
      algorithm = "knn",
      parameters = list(
        k = k,
        l = l,
        prob = TRUE
      ),
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "class",
      developer = "Brian D. Ripley",
      contact = "ripley@stats.ox.ac.uk",
      link = "https://cran.r-project.org/web/packages/class/index.html",
      doi = "ISBN 0-387-95457-0"
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "Statistic"),
      checkmate::test_choice(self@method, "MakeModel"),
      checkmate::test_choice(self@algorithm, "knn"),
      checkmate::test_number(self@parameters$k),
      checkmate::test_number(self@parameters$l)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, StatisticSettings_MakeModel_knn) <- function(x, engine = NULL) {
  
  if (!requireNamespace("class", quietly = TRUE)) {
    warning("The package 'class' is not available! Not done.")
    return(FALSE)
  }
  
  if (!is(engine, "StatisticEngine")) {
    warning("Engine is not a StatisticEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  train = engine$analyses$analyses
  cl = engine$analyses$classes
  
  if (nrow(train) != length(cl)) {
    warning("The number of rows in the training data must be equal to the number of classes! Not done.")
    return(FALSE)
  }
  
  conditions <- list(
    train = train,
    cl = cl,
    args = x$parameters
  )
  
  func <- class::knn
  engine$model <- StreamFind::KNN(model = list("func" = func, "conditions" = conditions))
  message(paste0("\U2713 ", "KNN classification model added!"))
  invisible(TRUE)
}

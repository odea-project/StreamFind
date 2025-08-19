#' @title MassSpecMethod_SmoothChromatograms_movingaverage Class
#'
#' @description Smooths chromatograms using the moving average algorithm.
#'
#' @param windowSize Numeric (length 1) with the window size for the moving average.
#'
#' @return A MassSpecMethod_SmoothChromatograms_movingaverage object.
#'
#' @export
#'
MassSpecMethod_SmoothChromatograms_movingaverage <- function(windowSize = 5) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "SmoothChromatograms",
    required = "LoadChromatograms",
    algorithm = "movingaverage",
    parameters = list(windowSize = windowSize),
    number_permitted = Inf,
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecMethod_SmoothChromatograms_movingaverage object!")
  }
}

#' @export
#' @noRd
validate_object.MassSpecMethod_SmoothChromatograms_movingaverage <- function(
  x
) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "SmoothChromatograms")
  checkmate::assert_choice(x$algorithm, "movingaverage")
  checkmate::assert_number(x$parameters$windowSize)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_SmoothChromatograms_movingaverage <- function(
  x,
  engine = NULL
) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  if (is.null(engine$Results[["MassSpecResults_Chromatograms"]])) {
    warning("No chromatograms results object available! Not done.")
    return(FALSE)
  }
  windowSize <- x$parameters$windowSize
  chrom_obj <- engine$Results[["MassSpecResults_Chromatograms"]]
  chrom_list <- chrom_obj$chromatograms
  chrom_list <- lapply(
    chrom_list,
    function(x, windowSize) {
      if (nrow(x) > 0) {
        if ("id" %in% colnames(x)) {
          temp_x <- split(x, x$id)

          temp_x <- lapply(temp_x, function(z) {
            z$intensity <- .moving_average(z$intensity, windowSize = windowSize)
            z
          })

          x <- data.table::rbindlist(temp_x)
        } else {
          x$intensity <- .moving_average(x$intensity, windowSize = windowSize)
        }
      }

      x
    },
    windowSize = windowSize
  )
  chrom_obj$chromatograms <- chrom_list
  engine$Results <- chrom_obj
  message(paste0("\U2713 ", "Chromatograms smoothed!"))
  TRUE
}

#' @title MassSpecMethod_SmoothChromatograms_savgol Class
#'
#' @description Smooths chromatograms using the Savitzky-Golay algorithm from the \pkg{pracma}
#' package.
#'
#' @param fl Numeric (length 1) with the filter length (for instance fl = 51..151), has to be odd.
#' @param forder Numeric (length 1) with the order of the filter
#' (2 = quadratic filter, 4 = quartic).
#' @param dorder Numeric (length 1) with the order of the derivative
#' (0 = smoothing, 1 = first derivative, etc.).
#'
#' @return A MassSpecMethod_SmoothChromatograms_savgol object.
#'
#' @export
#'
MassSpecMethod_SmoothChromatograms_savgol <- function(
  fl = 11,
  forder = 4,
  dorder = 0
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "SmoothChromatograms",
    required = "LoadChromatograms",
    algorithm = "savgol",
    parameters = list(
      fl = fl,
      forder = forder,
      dorder = dorder
    ),
    number_permitted = Inf,
    version = as.character(packageVersion("StreamFind")),
    software = "pracma",
    developer = "Hans W. Borchers",
    contact = NA_character_,
    link = "https://cran.r-project.org/web/packages/pracma/index.html",
    doi = NA_character_
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecMethod_SmoothChromatograms_savgol object!")
  }
}

#' @export
#' @noRd
validate_object.MassSpecMethod_SmoothChromatograms_savgol <- function(self) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "SmoothChromatograms")
  checkmate::assert_choice(x$algorithm, "savgol")
  checkmate::assert_number(x$parameters$fl)
  checkmate::assert_number(x$parameters$forder)
  checkmate::assert_number(x$parameters$dorder)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_SmoothChromatograms_savgol <- function(
  x,
  engine = NULL
) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  if (is.null(engine$Results[["MassSpecResults_Chromatograms"]])) {
    warning("No chromatograms results object available! Not done.")
    return(FALSE)
  }
  if (!requireNamespace("pracma", quietly = TRUE)) {
    warning("Package pracma not found but required! Not done.")
    return(FALSE)
  }
  fl <- x$parameters$fl
  forder <- x$parameters$forder
  dorder <- x$parameters$dorder
  chrom_obj <- engine$Results[["MassSpecResults_Chromatograms"]]
  chrom_list <- chrom_obj$chromatograms
  chrom_list <- lapply(
    chrom_list,
    function(x, fl, forder, dorder) {
      if (nrow(x) > 0) {
        if ("id" %in% colnames(x)) {
          temp_x <- split(x, x$id)

          temp_x <- lapply(temp_x, function(z) {
            z$intensity <- pracma::savgol(
              z$intensity,
              fl = fl,
              forder = forder,
              dorder = dorder
            )
            z
          })

          x <- data.table::rbindlist(temp_x)
        } else {
          x$intensity <- pracma::savgol(
            x$intensity,
            fl = fl,
            forder = forder,
            dorder = dorder
          )
        }
      }
      x
    },
    fl = fl,
    forder = forder,
    dorder = dorder
  )
  chrom_obj$hromatograms <- chrom_list
  engine$Results <- chrom_obj
  message(paste0("\U2713 ", "Chromatograms smoothed!"))
  TRUE
}

# MARK: RamanMethod_NormalizeSpectra_minmax
#' @title RamanMethod_NormalizeSpectra_minmax Class
#'
#' @description Normalizes spectra using the min-max algorithm.
#'
#' @return A RamanMethod_NormalizeSpectra_minmax object.
#'
#' @export
#'
RamanMethod_NormalizeSpectra_minmax <- function() {
  x <- ProcessingStep(
    type = "Raman",
    method = "NormalizeSpectra",
    algorithm = "minmax",
    parameters = list(),
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
    stop("Invalid RamanMethod_NormalizeSpectra_minmax object!")
  }
}

#' @describeIn RamanMethod_NormalizeSpectra_minmax Validate the RamanMethod_NormalizeSpectra_minmax object, returning NULL if valid.
#' @param A RamanMethod_NormalizeSpectra_minmax object.
#' @export
#' 
validate_object.RamanMethod_NormalizeSpectra_minmax <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "NormalizeSpectra")
  checkmate::assert_choice(x$algorithm, "minmax")
  NextMethod()
  NULL
}


#' @export
#' @noRd
run.RamanMethod_NormalizeSpectra_minmax <- function(x, engine = NULL) {
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
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

  spec_list <- engine$Spectra$spectra

  spec_list <- lapply(spec_list, function(z) {
    if (nrow(z) > 0) {
      if (("rt" %in% colnames(z)) && ("shift" %in% colnames(z))) {
        temp_x <- split(z, z$rt)

        temp_x <- lapply(temp_x, function(z) {
          max_int <- max(z$intensity)
          min_int <- min(z$intensity)
          z$intensity <- (z$intensity - min_int) /
            (max_int - min_int + abs(min_int))
          z
        })

        z <- data.table::rbindlist(temp_x)
      } else {
        max_int <- max(z$intensity)
        min_int <- min(z$intensity)
        z$intensity <- (z$intensity - min_int) / (max_int - min_int)
      }
    }

    z
  })

  engine$Spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra normalized!"))
  invisible(TRUE)
}

# MARK: RamanMethod_NormalizeSpectra_snv
#' @title RamanMethod_NormalizeSpectra_snv Class
#'
#' @description Normalizes spectra using the Standard Normal Variate (SNV) algorithm.
#'
#' @param liftTozero Logical (length 1) indicating if the spectra should be lifted to zero.
#'
#' @return A RamanMethod_NormalizeSpectra_snv object.
#'
#' @export
#'
RamanMethod_NormalizeSpectra_snv <- function(liftTozero = FALSE) {
  x <- ProcessingStep(
    type = "Raman",
    method = "NormalizeSpectra",
    algorithm = "snv",
    parameters = list(liftTozero = liftTozero),
    number_permitted = Inf,
    version = as.character(packageVersion("StreamFind")),
    software = NA_character_,
    developer = "J\u00FCrgen Schram",
    contact = "schram@hsnr.de",
    link = NA_character_,
    doi = "10.1016/j.trac.2018.12.004"
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid RamanMethod_NormalizeSpectra_snv object!")
  }
}

#' @describeIn RamanMethod_NormalizeSpectra_snv Validate the RamanMethod_NormalizeSpectra_snv object, returning NULL if valid.
#' @param A RamanMethod_NormalizeSpectra_snv object.
#' @export
#'
validate_object.RamanMethod_NormalizeSpectra_snv = function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "NormalizeSpectra")
  checkmate::assert_choice(x$algorithm, "snv")
  checkmate::assert_logical(x$parameters$liftTozero, max.len = 1)
  NextMethod()
  NULL
}


#' @export
#' @noRd
run.RamanMethod_NormalizeSpectra_snv <- function(x, engine = NULL) {
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
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

  liftTozero <- x$parameters$liftTozero

  spec_list <- engine$Spectra$spectra

  spec_list <- lapply(spec_list, function(z) {
    if (nrow(z) > 0) {
      if (("rt" %in% colnames(z)) && ("shift" %in% colnames(z))) {
        temp_x <- split(z, z$rt)

        temp_x <- lapply(temp_x, function(z) {
          mean_int <- mean(z$intensity)
          sd_int <- sd(z$intensity)
          z$intensity <- (z$intensity - mean_int) / sd_int
          if (liftTozero) {
            z$intensity <- z$intensity + abs(min(z$intensity))
          }
          z
        })

        z <- data.table::rbindlist(temp_x)
      } else {
        mean_int <- mean(z$intensity)
        sd_int <- sd(z$intensity)
        z$intensity <- (z$intensity - mean_int) / sd_int
        if (liftTozero) z$intensity <- z$intensity + abs(min(z$intensity))
      }
    }

    z
  })

  x$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra normalized!"))
  invisible(TRUE)
}

# MARK: RamanMethod_NormalizeSpectra_scale
#' @title RamanMethod_NormalizeSpectra_scale Class
#'
#' @description Normalizes spectra using scaling based on the standard deviation.
#'
#' @return A RamanMethod_NormalizeSpectra_scale object.
#'
#' @export
#'
RamanMethod_NormalizeSpectra_scale <- function() {
  x <- ProcessingStep(
    type = "Raman",
    method = "NormalizeSpectra",
    algorithm = "scale",
    parameters = list(),
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
    stop("Invalid RamanMethod_NormalizeSpectra_scale object!")
  }
}

#' @describeIn RamanMethod_NormalizeSpectra_scale Validate the RamanMethod_NormalizeSpectra_scale object, returning NULL if valid.
#' @param A RamanMethod_NormalizeSpectra_scale object.
#' @export
#'
validate_object.RamanMethod_NormalizeSpectra_scale <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "NormalizeSpectra")
  checkmate::assert_choice(x$algorithm, "scale")
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.RamanMethod_NormalizeSpectra_scale <- function(x, engine = NULL) {
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
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

  spec_list <- engine$Spectra$spectra

  spec_list <- lapply(spec_list, function(z) {
    if (nrow(z) > 0) {
      if (("rt" %in% colnames(z)) && ("shift" %in% colnames(z))) {
        temp_x <- split(z, z$rt)

        temp_x <- lapply(temp_x, function(z) {
          sd_int <- sd(z$intensity)
          z$intensity <- z$intensity / sd_int
          z
        })

        z <- data.table::rbindlist(temp_x)
      } else {
        sd_int <- sd(z$intensity)
        z$intensity <- z$intensity / sd_int
      }
    }

    z
  })

  engine$Spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra normalized!"))
  invisible(TRUE)
}

# MARK: RamanMethod_NormalizeSpectra_blockweight
#' @title RamanMethod_NormalizeSpectra_blockweight Class
#'
#' @description Normalizes spectra using block weighting for downstream data evaluation.
#'
#' @return A RamanMethod_NormalizeSpectra_blockweight object.
#'
#' @export
#'
RamanMethod_NormalizeSpectra_blockweight <- function() {
  x <- ProcessingStep(
    type = "Raman",
    method = "NormalizeSpectra",
    algorithm = "blockweight",
    parameters = list(),
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
    stop("Invalid RamanMethod_NormalizeSpectra_blockweight object!")
  }
}

#' @describeIn RamanMethod_NormalizeSpectra_blockweight Validate the RamanMethod_NormalizeSpectra_blockweight object, returning NULL if valid.
#' @param A RamanMethod_NormalizeSpectra_blockweight object.
#' @export
#' 
validate_object.RamanMethod_NormalizeSpectra_blockweight <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "NormalizeSpectra")
  checkmate::assert_choice(x$algorithm, "blockweight")
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.RamanMethod_NormalizeSpectra_blockweight <- function(x, engine = NULL) {
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
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

  spec_list <- engine$Spectra$spectra

  spec_list <- lapply(spec_list, function(z) {
    if (nrow(z) > 0) {
      if (("rt" %in% colnames(z)) && ("shift" %in% colnames(z))) {
        temp_x <- split(z, z$rt)

        temp_x <- lapply(temp_x, function(z) {
          z$intensity <- z$intensity / sqrt(length(z$intensity))
          z
        })

        z <- data.table::rbindlist(temp_x)
      } else {
        z$intensity <- z$intensity / sqrt(length(z$intensity))
      }
    }

    z
  })

  engine$Spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra normalized!"))
  invisible(TRUE)
}

# MARK: RamanMethod_NormalizeSpectra_meancenter
#' @title RamanMethod_NormalizeSpectra_meancenter Class
#'
#' @description Normalizes spectra using mean centering.
#'
#' @return A RamanMethod_NormalizeSpectra_meancenter object.
#'
#' @export
#'
RamanMethod_NormalizeSpectra_meancenter <- function() {
  x <- ProcessingStep(
    type = "Raman",
    method = "NormalizeSpectra",
    algorithm = "meancenter",
    parameters = list(),
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
    stop("Invalid RamanMethod_NormalizeSpectra_meancenter object!")
  }
}

#' @describeIn RamanMethod_NormalizeSpectra_meancenter Validate the RamanMethod_NormalizeSpectra_meancenter object, returning NULL if valid.
#' @param A RamanMethod_NormalizeSpectra_meancenter object.
#' @export
#'
validate_object.RamanMethod_NormalizeSpectra_meancenter <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "NormalizeSpectra")
  checkmate::assert_choice(x$algorithm, "meancenter")
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.RamanMethod_NormalizeSpectra_meancenter <- function(x, engine = NULL) {
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
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

  spec_list <- engine$Spectra$spectra

  spec_list <- lapply(spec_list, function(z) {
    if (nrow(z) > 0) {
      if (("rt" %in% colnames(z)) && ("shift" %in% colnames(z))) {
        temp_x <- split(z, z$rt)

        temp_x <- lapply(temp_x, function(z) {
          mean_int <- mean(z$intensity)
          z$intensity <- z$intensity - mean_int
          z
        })

        z <- data.table::rbindlist(temp_x)
      } else {
        mean_int <- mean(z$intensity)
        z$intensity <- z$intensity / mean_int
      }
    }

    z
  })

  engine$Spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra normalized!"))
  invisible(TRUE)
}

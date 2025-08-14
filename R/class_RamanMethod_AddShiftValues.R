#' @title RamanMethod_AddShiftValues_native Class
#'
#' @description Adds custom shift values to the Raman spectra or copies from one or more analyses to
#' the other analyses. When more than one analyses are selected, the shift values are copied from
#' the selected but them averaged before being added to the other analyses. If the column "rt" is
#' present in the spectra, the shift values are added to each retention time.
#'
#' @param mode A character string. The mode to be used, possible values are `analyses` or `custom`
#' for copying from analyses or adding custom shift values, respectively.
#' @param index An integer vector with the index/es of the analysis/analyses to copy the shift
#' values from.
#' @param shifts A numeric vector with the shift values to be added to the spectra.
#'
#' @return A RamanMethod_AddShiftValues_native object.
#'
#' @export
#'
RamanMethod_AddShiftValues_native <- function(
  mode = "analyses",
  index = NA_integer_,
  shifts = NA_real_
) {
  x <- ProcessingStep(
    type = "Raman",
    method = "AddShiftValues",
    algorithm = "native",
    parameters = list(
      mode = mode,
      index = index,
      shifts = shifts
    ),
    number_permitted = 1,
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
    stop("Invalid RamanMethod_AddShiftValues_native object!")
  }
}

#' @describeIn RamanMethod_AddShiftValues_native Validate the RamanMethod_AddShiftValues_native object, returning NULL if valid.
#' @param A RamanMethod_AddShiftValues_native object.
#' @export
#'
validate_object.RamanMethod_AddShiftValues_native <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "AddShiftValues")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_choice(x$parameters$mode, c("analyses", "custom"))
  checkmate::assert_integer(as.integer(x$parameters$index))
  checkmate::assert_numeric(x$parameters$shifts)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.RamanMethod_AddShiftValues_native <- function(x, engine = NULL) {
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

  algo_mode <- x$parameters$mode
  shifts <- x$parameters$shifts
  index <- as.integer(x$parameters$index)

  spec_list <- engine$Spectra$spectra

  if (algo_mode %in% "analyses") {
    if (is.na(index)) {
      index <- seq_len(length(spec_list))
    }

    shifts_list <- spec_list[index]

    if (length(spec_list) == 0) {
      warning("No analyses selected! Not done.")
      return(FALSE)
    }

    shifts_list <- lapply(shifts_list, function(z) {
      if (nrow(z) == 0) {
        return(NULL)
      }
      if ("rt" %in% colnames(z)) {
        z <- split(z, by = "rt")
        zs <- z[[1]]$shift
        for (i in 2:length(z)) {
          zs <- zs + z[[i]]$shift
        }
        zs <- zs / length(z)
        return(zs)
      } else {
        return(z$shift)
      }
    })

    shifts <- shifts_list[[1]]

    if (is.null(shifts)) {
      warning("No shift values found! Not done.")
      return(FALSE)
    }

    if (length(shifts_list) > 1) {
      for (i in 2:length(shifts_list)) {
        if (!is.null(shifts_list[[i]])) shifts <- shifts + shifts_list[[i]]
      }
      shifts <- shifts / length(shifts_list)
    }
  } else {
    if (all(is.na(shifts))) {
      warning("No shift values found! Not done.")
      return(FALSE)
    }
  }

  shifts <- round(shifts, digits = 0)

  spec_list <- lapply(
    spec_list,
    function(z, shifts) {
      if (nrow(z) == 0) {
        return(data.table::data.table())
      }
      if ("rt" %in% colnames(z)) {
        z <- split(z, by = "rt")
        z <- lapply(z, function(y) {
          y$shift <- shifts
          return(y)
        })
        z <- data.table::rbindlist(z)
      } else {
        z$shift <- shifts
      }
      z
    },
    shifts = shifts
  )

  engine$Spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Shift values added!"))
  TRUE
}

#' @title RamanMethod_RemoveCosmicRays_native
#'
#' @description Removes cosmic rays from Raman spectra.
#'
#' @return A RamanMethod_RemoveCosmicRays_native object.
#'
#' @export
#'
RamanMethod_RemoveCosmicRays_native <- function() {
  x <- ProcessingStep(
    type = "Raman",
    method = "RemoveCosmicRays",
    algorithm = "native",
    parameters = list(),
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
    stop("Invalid RamanMethod_RemoveCosmicRays_native object!")
  }
}

#' @describeIn RamanMethod_RemoveCosmicRays_native Validate the RamanMethod_RemoveCosmicRays_native object, returning NULL if valid.
#' @param x A RamanMethod_RemoveCosmicRays_native object.
#' @export
#'
validate_object.RamanMethod_RemoveCosmicRays_native <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "RemoveCosmicRays")
  checkmate::assert_choice(x$algorithm, "native")
  NextMethod()
  NULL
}


#' @export
#' @noRd
run.RamanMethod_RemoveCosmicRays_native <- function(x, engine = NULL) {
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

  intensity <- NULL
  . <- NULL

  number_cosmic_rays <- 0

  spec_list <- lapply(spec_list, function(z) {
    if (nrow(z) == 0) {
      return(data.table::data.table())
    }

    res <- data.table::copy(z)

    if ("rt" %in% colnames(res)) {
      res <- split(res, by = "rt")

      res <- lapply(res, function(y) {
        if (nrow(y) == 0) {
          return(data.table::data.table())
        }

        distance_to_prev <- c(NA_real_, diff(y$intensity))
        distance_to_next <- c(diff(y$intensity), NA_real_)
        distance_total <- abs(distance_to_prev) + abs(distance_to_prev)
        distance_total[is.na(distance_total)] <- 0

        if (any(distance_total > 2000)) {
          browser()

          plot(
            y$intensity,
            type = "l",
            ylim = c(min(y$intensity), max(y$intensity))
          )
          lines(distance_total, col = "blue")
          lines(
            rep(mean(distance_total, na.rm = TRUE), length(distance_to_prev)),
            col = "green"
          )
          lines(
            rep(
              mean(distance_total, na.rm = TRUE) +
                sd(distance_total, na.rm = TRUE),
              length(distance_to_prev)
            ),
            col = "red"
          )
        }
      })
    } else if ("id" %in% colnames(res)) {} else {}

    res
  })

  # engine$Spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Cosmic rays removed!"))
  TRUE
}

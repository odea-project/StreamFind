#' @title RamanMethod_SubtractScansSection_native Class
#'
#' @description Subtracts the spectra of a section of the scans to all other spectra.
#'
#' @param sectionWindow Numeric (length 2) with the window for the sectioning.
#'
#' @return A RamanMethod_SubtractScansSection_native object.
#'
#' @export
#'
RamanMethod_SubtractScansSection_native <- function(
  sectionWindow = c(10, 200)
) {
  x <- ProcessingStep(
    type = "Raman",
    method = "SubtractScansSection",
    algorithm = "native",
    parameters = list(
      sectionWindow = sectionWindow
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
    stop("Invalid RamanMethod_SubtractScansSection_native object!")
  }
}

#' @describeIn RamanMethod_SubtractScansSection_native Validate the RamanMethod_SubtractScansSection_native object, returning NULL if valid.
#' @param x A RamanMethod_SubtractScansSection_native object.
#' @export
#'
validate_object.RamanMethod_SubtractScansSection_native <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "SubtractScansSection")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_numeric(x$parameters$sectionWindow, len = 2)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.RamanMethod_SubtractScansSection_native <- function(x, engine = NULL) {
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  if (is.null(engine$Results[["RamanResults_Spectra"]])) {
    engine$Results <- RamanResults_Spectra(
      lapply(engine$Analyses$analyses, function(a) a$spectra)
    )
  }
  spec_obj <- engine$Results[["RamanResults_Spectra"]]
  sectionWindow = x$parameters$sectionWindow
  spec_list <- spec_obj$spectra
  intensity <- NULL
  . <- NULL
  spec_cut <- lapply(spec_list, function(z) {
    if (nrow(z) == 0) {
      return(data.table::data.table())
    }
    if (!"rt" %in% colnames(z)) {
      warning("No 'rt' column in the spectra for finding scans session!")
      return(data.table::data.table())
    }

    res <- data.table::copy(z)

    if (length(sectionWindow) == 2 && is.numeric(sectionWindow)) {
      sectionWindow <- sort(sectionWindow)
      cutSec <- res[res$rt >= sectionWindow[1] & res$rt <= sectionWindow[2], ]
      if (nrow(cutSec) > 0) {
        cutSec <- cutSec[, .(intensity = mean(intensity)), by = "shift"]
        res <- res[res$rt < sectionWindow[1] | res$rt > sectionWindow[2], ]
        res_list <- split(res, res$rt)
        res_list <- lapply(
          res_list,
          function(z, cutSec) {
            z$intensity <- z$intensity - cutSec$intensity
            z
          },
          cutSec = cutSec
        )
        res <- data.table::rbindlist(res_list)
      }
    }

    res
  })
  spec_obj$spectra <- spec_cut
  engine$Results <- spec_obj
  message(paste0("\U2713 ", "Scans section subtracted!"))
  TRUE
}

#' @title RamanMethod_FilterSpectra_native Class
#'
#' @description Various filter options for Raman spectra.
#'
#' @param onlyChromPeaksSpectra Logical (length 1) for keeping only spectra with chromatographic
#' peaks.
#' @param onlyTopChromPeaksSpectra Numeric (length 1) with the number of top spectra to keep for
#' each chromatographic peak.
#'
#' @return A RamanMethod_FilterSpectra_native object.
#'
#' @export
#'
RamanMethod_FilterSpectra_native <- function(
  onlyChromPeaksSpectra = FALSE,
  onlyTopChromPeaksSpectra = 0
) {
  x <- ProcessingStep(
    type = "Raman",
    method = "FilterSpectra",
    algorithm = "native",
    parameters = list(
      onlyChromPeaksSpectra = onlyChromPeaksSpectra,
      onlyTopChromPeaksSpectra = onlyTopChromPeaksSpectra
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
    stop("Invalid RamanMethod_FilterSpectra_native object!")
  }
}

#' @describeIn RamanMethod_FilterSpectra_native Validate the RamanMethod_FilterSpectra_native object, returning NULL if valid.
#' @param x A RamanMethod_FilterSpectra_native object.
#' @export
#'
validate_object.RamanMethod_FilterSpectra_native <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "FilterSpectra")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_logical(x$parameters$onlyChromPeaksSpectra, max.len = 1)
  checkmate::assert_number(x$parameters$onlyTopChromPeaksSpectra)
  NextMethod()
  NULL
}


#' @export
#' @noRd
run.RamanMethod_FilterSpectra_native <- function(x, engine = NULL) {
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

  if (x$parameters$onlyChromPeaksSpectra) {
    if (length(spec_obj$chrom_peaks) == 0) {
      warning("No chromatographic peaks found! Not done.")
      return(FALSE)
    }
    spec_list <- spec_obj$spectra
    chrom_peaks <- spec_obj$chrom_peaks
    if (!identical(names(chrom_peaks), names(spec_list))) {
      warning("Chromatograms and spectra do not match! Not done.")
      return(FALSE)
    }
    sub_spectra <- Map(
      function(z, y) {
        z$id <- NA_character_
        if ("group" %in% colnames(y)) {
          z$group <- NA_character_
        }
        for (i in seq_len(nrow(y))) {
          sel <- z$rt >= y$rtmin[i] & z$rt <= y$rtmax[i]
          z$id[sel] <- paste0(y$peak[i], "_", round(y$rt[i], digits = 0))
          if ("group" %in% colnames(y)) {
            z$group[sel] <- y$group[i]
          }
        }
        z <- z[!is.na(z$id), ]
        z
      },
      spec_list,
      chrom_peaks
    )
    spec_obj$spectra <- sub_spectra
    message(paste0("\U2713 ", "Only spectra from chromatographic peaks kept!"))
  }

  topSpectra <- x$parameters$onlyTopChromPeaksSpectra
  if (topSpectra > 0) {
    if (length(spec_obj$chrom_peaks) == 0) {
      warning("No chromatographic peaks found! Not done.")
      return(FALSE)
    }
    spec_list <- spec_obj$spectra
    chrom_peaks <- spec_obj$chrom_peaks
    if (!identical(names(chrom_peaks), names(spec_list))) {
      warning("Chromatograms and spectra do not match! Not done.")
      return(FALSE)
    }
    sub_spectra <- Map(
      function(z, y) {
        intensity <- NULL
        z$id <- NA_character_
        if ("group" %in% colnames(y)) {
          z$group <- NA_character_
        }
        for (i in seq_len(nrow(y))) {
          sel <- z$rt >= y$rtmin[i] & z$rt <= y$rtmax[i]
          temp <- z[sel, ]
          temp <- temp[, .(intensity = sum(intensity)), by = "rt"]
          data.table::setorder(temp, -intensity)
          top_rts <- temp$rt[seq_len(min(topSpectra, nrow(temp)))]
          sel <- z$rt %in% top_rts
          z$id[sel] <- paste0(y$peak[i], "_", round(y$rt[i], digits = 0))
          if ("group" %in% colnames(y)) {
            z$group[sel] <- y$group[i]
          }
        }
        z <- z[!is.na(z$id), ]
        z
      },
      spec_list,
      chrom_peaks
    )
    spec_obj$spectra <- sub_spectra
    message(paste0("\U2713 ", "Only spectra from chromatographic peaks kept!"))
  }
  engine$Results <- spec_obj
  invisible(TRUE)
}

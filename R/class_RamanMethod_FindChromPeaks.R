#' @title RamanMethod_FindChromPeaks_LocalMaxima Class
#'
#' @description Finds peak maxima in the chromatographic dimension of multiple Raman spectra.
#'
#' @param minWidth Numeric (length 1) with the minimum width of a peak.
#' @param maxWidth Numeric (length 1) with the maximum width of a peak.
#' @param minHeight Numeric (length 1) with the minimum height of a peak.
#'
#' @return A RamanMethod_FindChromPeaks_LocalMaxima object.
#'
#' @export
#'
RamanMethod_FindChromPeaks_LocalMaxima <- function(
  minWidth = 0,
  maxWidth = 0,
  minHeight = 0
) {
  x <- ProcessingStep(
    type = "Raman",
    method = "FindChromPeaks",
    required = NA_character_,
    algorithm = "LocalMaxima",
    parameters = list(
      minWidth = as.numeric(minWidth),
      maxWidth = as.numeric(maxWidth),
      minHeight = as.numeric(minHeight)
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
    stop("Invalid RamanMethod_FindChromPeaks_LocalMaxima object!")
  }
}

#' @export
#' @noRd
validate_object.RamanMethod_FindChromPeaks_LocalMaxima <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "FindChromPeaks")
  checkmate::assert_choice(x$algorithm, "LocalMaxima")
  checkmate::assert_number(x$parameters$minWidth)
  checkmate::assert_number(x$parameters$maxWidth)
  checkmate::assert_number(x$parameters$minHeight)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.RamanMethod_FindChromPeaks_LocalMaxima <- function(x, engine = NULL) {
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
  if (!any(vapply(spec_obj$spectra, function(s) "rt" %in% colnames(s), logical(1)))) {
    warning("No chromatograms available! Not done.")
    return(FALSE)
  }
  parameters <- x$parameters
  spectra <- spec_obj$spectra
  chrom_peaks <- lapply(spectra, function(s) {
    if (nrow(s) == 0) {
      return(data.table::data.table())
    }
    intensity <- NULL
    s <- s[, .(intensity = sum(intensity)), by = "rt"]
    find_relevant_peaks <- function(x, y, min_width, max_width, min_height) {
      peaks <- numeric(0)
      peaks_left <- numeric(0)
      peaks_right <- numeric(0)
      widths <- numeric(0)
      heights_left <- numeric(0)
      heights_right <- numeric(0)
      peak_bases <- numeric(0)
      peak_areas <- numeric(0)
      is_peak <- function(i, y) {
        y[i] > y[i - 1] && y[i] > y[i + 1]
      }
      i <- 2
      while (i < length(y) - 1) {
        if (is_peak(i, y)) {
          left <- i
          while (
            left > 1 &&
              y[left - 1] < y[i] &&
              x[i] - x[left] <= max_width / 1.5
          ) {
            left <- left - 1
          }

          right <- i
          while (
            right < length(y) &&
              y[right + 1] < y[i] &&
              x[right] - x[i] <= max_width / 1.5
          ) {
            right <- right + 1
          }

          peak_width <- x[right] - x[left]
          peak_left <- x[left]
          peak_right <- x[right]
          peak_height_right <- y[i] - y[right]
          peak_height_left <- y[i] - y[left]

          if (
            peak_height_right >= min_height &&
              peak_height_left >= min_height &&
              peak_width >= min_width
          ) {
            which_max <- which.max(y[left:right]) + left - 1

            peaks <- c(peaks, which_max)
            peaks_left <- c(peaks_left, peak_left)
            peaks_right <- c(peaks_right, peak_right)
            widths <- c(widths, peak_width)
            heights_left <- c(heights_left, peak_height_left)
            heights_right <- c(heights_right, peak_height_right)
            peak_bases <- c(peak_bases, min(left, right))
            peak_areas <- c(
              peak_areas,
              .integrate_peak_area(x, y, x[left], x[right])
            )

            i <- right
          }
        }
        i <- i + 1
      }
      return(data.table::data.table(
        peak = seq_along(peaks),
        rt = x[peaks],
        rtmin = peaks_left,
        rtmax = peaks_right,
        intensity = y[peaks],
        width = widths,
        height_left = heights_left,
        height_right = heights_right,
        area = peak_areas,
        sn = y[peaks] / y[peak_bases]
      ))
    }

    peak_results <- find_relevant_peaks(
      s$rt,
      s$intensity,
      min_width = parameters$minWidth,
      max_width = parameters$maxWidth,
      min_height = parameters$minHeight
    )
    if (FALSE) {
      plot(
        s$rt,
        s$intensity,
        type = "l",
        main = "Chromatogram with Relevant Peaks"
      )
      points(peak_results$rt, peak_results$intensity, col = "red", pch = 19)
      points(
        peak_results$rtmin,
        peak_results$intensity,
        col = "green",
        pch = 19
      )
      points(
        peak_results$rtmax,
        peak_results$intensity,
        col = "green",
        pch = 19
      )
    }
    return(peak_results)
  })

  names(chrom_peaks) <- names(spectra)
  spec_obj$chrom_peaks <- chrom_peaks
  engine$Results <- spec_obj
  message(paste0("\U2713 ", "Chromatograms peaks found and added!"))
  TRUE
}

#' @title RamanMethod_FindChromPeaks_pracma Class
#'
#' @description Integrates chromatograms using the function `findpeaks` from the package
#' \pkg{pracma} with natively added peak exclusion and evaluation steps.
#'
#' @param merge Logical (length 1) indicating if the nearby peaks should be merged.
#' @param closeByThreshold Numeric (length 1) with the maximum distance between peaks to be merged.
#' @param minPeakHeight Numeric (length 1) with the minimum peak height to be considered.
#' @param minPeakDistance Numeric (length 1) with the minimum distance between peaks.
#' @param minPeakWidth Numeric (length 1) with the minimum peak width.
#' @param maxPeakWidth Numeric (length 1) with the maximum peak width.
#' @param minSN Numeric (length 1) with the minimum signal-to-noise ratio.
#'
#' @return A RamanMethod_FindChromPeaks_pracma object.
#'
#' @export
#'
RamanMethod_FindChromPeaks_pracma <- function(
  merge = TRUE,
  closeByThreshold = 45,
  minPeakHeight = 0,
  minPeakDistance = 10,
  minPeakWidth = 5,
  maxPeakWidth = 120,
  minSN = 10
) {
  x <- ProcessingStep(
    type = "Raman",
    method = "FindChromPeaks",
    required = NA_character_,
    algorithm = "pracma",
    parameters = list(
      merge = merge,
      closeByThreshold = closeByThreshold,
      minPeakHeight = minPeakHeight,
      minPeakDistance = minPeakDistance,
      minPeakWidth = minPeakWidth,
      maxPeakWidth = maxPeakWidth,
      minSN = minSN
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
    stop("Invalid RamanMethod_FindChromPeaks_pracma object!")
  }
}

#' @export
#' @noRd
validate_object.RamanMethod_FindChromPeaks_pracma = function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "FindChromPeaks")
  checkmate::assert_choice(x$algorithm, "pracma")
  checkmate::assert_logical(x$parameters$merge, max.len = 1)
  checkmate::assert_number(x$parameters$closeByThreshold)
  checkmate::assert_number(x$parameters$minPeakHeight)
  checkmate::assert_number(x$parameters$minPeakDistance)
  checkmate::assert_number(x$parameters$minPeakWidth)
  checkmate::assert_number(x$parameters$maxPeakWidth)
  checkmate::assert_number(x$parameters$minSN)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.RamanMethod_FindChromPeaks_pracma <- function(x, engine = NULL) {
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
  if (!any(vapply(spec_obj$spectra, function(s) "rt" %in% colnames(s), logical(1)))) {
    warning("No chromatograms available! Not done.")
    return(FALSE)
  }
  parameters <- x$parameters
  spectra <- spec_obj$spectra
  chrom_peaks <- lapply(spectra, function(s) {
    if (nrow(s) == 0) {
      return(data.table::data.table())
    }
    if (!"rt" %in% colnames(s)) {
      return(data.table::data.table())
    }
    intenisty <- NULL
    s <- s[, .(intensity = sum(intensity)), by = "rt"]
    pks <- .find_peaks(
      s,
      "rt",
      parameters$merge,
      parameters$closeByThreshold,
      parameters$minPeakHeight,
      parameters$minPeakDistance,
      parameters$maxPeakWidth,
      parameters$minPeakWidth,
      parameters$minSN
    )
    if (nrow(pks) == 0) {
      return(data.table::data.table())
    }
    pks$idx <- NULL
    data.table::setnames(
      pks,
      c("xVal", "min", "max"),
      c("rt", "rtmin", "rtmax"),
      skip_absent = TRUE
    )
    data.table::setcolorder(pks, c("peak"))
    pks
  })
  names(chrom_peaks) <- names(spectra)
  spec_obj$chrom_peaks <- chrom_peaks
  engine$Results <- spec_obj
  message(paste0("\U2713 ", "Chromatograms peaks found and added!"))
  TRUE
}

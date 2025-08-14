#' @title RamanMethod_GroupPeaks_native Class
#'
#' @description Groups peaks across analyses.
#'
#' @param fillMissingPeaks Logical (length 1) for filling missing peaks.
#'
#' @return A RamanMethod_GroupPeaks_native object.
#'
#' @export
#'
RamanMethod_GroupPeaks_native <- function(fillMissingPeaks = TRUE) {
  x <- ProcessingStep(
    type = "Raman",
    method = "GroupPeaks",
    algorithm = "native",
    parameters = list(
      fillMissingPeaks = fillMissingPeaks
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
    stop("Invalid RamanMethod_GroupPeaks_native object!")
  }
}

#' @describeIn RamanMethod_GroupPeaks_native Validate the RamanMethod_GroupPeaks_native object, returning NULL if valid.
#' @param x A RamanMethod_GroupPeaks_native object.
#' @export
#'
validate_object.RamanMethod_GroupPeaks_native <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "GroupPeaks")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_logical(x$parameters$fillMissingPeaks, max.len = 1)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.RamanMethod_GroupPeaks_native <- function(x, engine = NULL) {
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

  if (!engine$Spectra$has_chrom_peaks) {
    warning("No chromatographic peaks found! Not done.")
    return(FALSE)
  }

  fillMissingPeaks <- x$parameters$fillMissingPeaks

  chrom_peaks <- engine$Spectra$chrom_peaks

  spec_list <- engine$Spectra$spectra

  analyses_names <- names(chrom_peaks)

  chrom_peaks <- rbindlist(chrom_peaks, idcol = "analysis")

  peak_numbers <- unique(chrom_peaks$peak)

  number_peaks <- length(peak_numbers)

  chrom_peaks$group <- NA_character_

  group_number <- 1

  filled_peaks <- 0

  for (p in seq_len(nrow(chrom_peaks))) {
    if (!is.na(chrom_peaks$group[p])) {
      next
    }
    rt_range <- c(chrom_peaks$rtmin[p], chrom_peaks$rtmax[p])
    sel_peaks <- chrom_peaks$rtmax >= rt_range[1] &
      chrom_peaks$rtmin <= rt_range[2]
    temp <- chrom_peaks[sel_peaks, ]
    group_name <- paste0(
      "G",
      group_number,
      "_T",
      round(mean(temp$rt), digits = 0)
    )
    chrom_peaks$group[sel_peaks] <- group_name

    if (fillMissingPeaks && any(!analyses_names %in% temp$analysis)) {
      temp_rtmax <- max(temp$rtmax)
      temp_rtmin <- min(temp$rtmin)
      for (a in analyses_names[!analyses_names %in% temp$analysis]) {
        if (!a %in% temp$analysis) {
          filled_peaks <- filled_peaks + 1
          spec_a <- spec_list[[a]]
          spec_a <- spec_a[spec_a$rt >= temp_rtmin & spec_a$rt <= temp_rtmax, ]
          intensity <- NULL
          spec_a <- spec_a[, .(intensity = sum(intensity)), by = rt]
          maxint <- which.max(spec_a$intensity)
          height_left <- spec_a$intensity[1]
          height_right <- spec_a$intensity[nrow(spec_a)]
          noise <- mean(c(height_left, height_right))
          if (noise < 0) {
            sn <- (spec_a$intensity[maxint] + abs(noise)) /
              (noise + 2 * abs(noise))
          } else {
            sn <- spec_a$intensity[maxint] / noise
          }

          area <- .integrate_peak_area(
            spec_a$rt,
            spec_a$intensity,
            temp_rtmin,
            temp_rtmax
          )

          new_chrom_peaks <- data.table::data.table(
            analysis = a,
            peak = paste0("f_", filled_peaks),
            rt = spec_a$rt[maxint],
            rtmin = temp_rtmin,
            rtmax = temp_rtmax,
            intensity = spec_a$intensity[maxint],
            width = temp_rtmax - temp_rtmin,
            height_left = height_left,
            height_right = height_right,
            area = area,
            sn = round(sn, digits = 1),
            group = group_name
          )

          new_chrom_peaks <- new_chrom_peaks[,
            colnames(chrom_peaks),
            with = FALSE
          ]

          chrom_peaks <- rbind(
            chrom_peaks,
            new_chrom_peaks,
            fill = TRUE
          )
        }
      }
    }
    group_number <- group_number + 1
  }

  setorder(chrom_peaks, analysis, rt)

  split_analysis_vec <- chrom_peaks$analysis
  chrom_peaks$analysis <- NULL
  chrom_peaks <- split(chrom_peaks, split_analysis_vec)

  engine$Spectra$chrom_peaks <- chrom_peaks
  message(paste0(
    "\U2713 ",
    "Chromatographic peaks grouped into ",
    group_number,
    " !"
  ))
  TRUE
}

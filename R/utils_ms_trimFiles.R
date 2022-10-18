
#' @title trimSpectraMZR
#'
#' @description Trim \emph{mzML} or \emph{mzXML} files based on time
#' (in seconds) and  \emph{m/z} (in Da) ranges using the \pkg{mzR} package.
#'
#' @param files A character vector with the full path of the files to trim.
#' @param rtr A numeric vector of length 2 with the maximum and minimum
#' time range to trim the files, in seconds.
#' @param MS1 Set to TRUE for trimming MS level one data.
#' @param MS2 Set to TRUE for trimming MS level two data.
#' @param mzr A numeric vector of length 2 with the maximum and minimum
#' \emph{m/z} range to trim the files, in Da.
#' @param mzr_ms2 A numeric vector of length 2 with the maximum and minimum
#' \emph{m/z} range to trim MS2 data files, in Da. when NULL and MS2 is TRUE
#' the \code{mzr} argument is used instead.
#' @param intensityThreshold A numeric vector of length one with the minimum
#' intensity threshold. Traces below the given intensity threshold are removed.
#' If a length two vector is given, the first value if applied for MS1 data
#' and the second to MS2 data.
#' @param copyMetadata Set to TRUE to copy metadata from the original file.
#' @param path A character string with the path to save the files.
#' When \code{NULL}, the file directory is used instead.
#' @param prefix A character string with the name prefix for the new trimmed file.
#'
#' @return Saves the trimmed \emph{mzML} or \emph{mzXML} files
#' in a given path, adding a pre-defined prefix to the file name.
#'
#' @export
#'
#' @references
#' \insertRef{mzr01}{streamFind}
#'
#' \insertRef{mzr02}{streamFind}
#'
#' \insertRef{mzr03}{streamFind}
#'
#' \insertRef{mzr04}{streamFind}
#'
trimSpectraFilesMZR <- function(files, MS1 = TRUE, MS2 = TRUE,
                                rtr = NULL, mzr = NULL, mzr_ms2 = NULL,
                                intensityThreshold = NULL, copyMetadata = TRUE,
                                path = NULL, prefix = "trim_") {

  requireNamespace("mzR")

  if (!is.null(rtr) & !is.numeric(rtr)) {
    return(warning("rtr must be numeric!"))
  }

  if (!is.null(rtr) & length(rtr) != 2) {
    return(warning("rtr not valid! A vector with
            length 2 is expected for time range, in seconds."))
  }

  if (!is.null(mzr) & !is.numeric(mzr)) {
    return(warning("mzr must be numeric!"))
  }

  if (!is.null(mzr) & length(mzr) != 2) {
    return(warning("mzr not valid! A numeric vector with
            length 2 is expected for m/z range, in Da."))
  }

  if (!is.null(mzr_ms2)) {

    if (!is.numeric(mzr_ms2)) {
      return(warning("mzr_ms2 must be numeric!"))
    }

    if (length(mzr_ms2) != 2) {
      return(warning("mzr_ms not valid! A numeric vector with
            length 2 is expected for m/z range, in Da."))
    }
  }

  if (MS2 & is.null(mzr_ms2)) mzr_ms2 <- mzr

  if (!is.null(intensityThreshold)) {

    if (!is.numeric(intensityThreshold)) {
      return(warning("Intensity threshold must be numeric!"))
    }

    if (length(intensityThreshold) > 2) {
      return(warning("Intensity threshold must be maximum of length 2!"))
    }

    if (length(intensityThreshold) == 2) {
      intThresMS1 <- intensityThreshold[1]
      intThresMS2 <- intensityThreshold[2]
    } else {
      intThresMS1 <- intensityThreshold[1]
      intThresMS2 <- intensityThreshold[1]
    }
  }



  cat("Trimming files... \n")
  pb <- txtProgressBar(
    min = 0,
    max = length(files),
    style = 3,
    width = 50,
    char = "+"
  )

  for (f in files) {

    if (!file_ext(f) %in% c("mzML", "mzXML")) {
      warning(paste0("File ", basename(f), " not as mzML or mzXML. Skipped!"))
      next
    }

    msf <- mzR::openMSfile(f)
    hd <- mzR::header(msf)

    if (!is.null(rtr)) {
      hd2 <- hd[hd$retentionTime >= rtr[1] & hd$retentionTime <= rtr[2], ]
    }

    if (!is.null(mzr) | !is.null(mzr_ms2)) {

      spec <- mzR::peaks(msf, scans = hd2$seqNum)

      for (s in seq_len(length(spec))) {
        altered <- FALSE

        if (!is.matrix(spec[[s]])) {
          spec[[s]] <- matrix(spec[[s]],
                              ncol = 2,
                              dimnames = list(1, c("mz", "intensity")))
        }

        if (MS1 & hd2$msLevel[s] == 1 & !is.null(mzr)) {
          altered <- TRUE
          spec[[s]] <- spec[[s]][spec[[s]][, 1] >= mzr[1] &
                                   spec[[s]][, 1] <= mzr[2], ]

          if (!is.matrix(spec[[s]])) {
            spec[[s]] <- matrix(spec[[s]], ncol = 2,
                                dimnames = list(1, c("mz", "intensity")))
          }

          if (!is.null(intensityThreshold)) {
            spec[[s]] <- spec[[s]][spec[[s]][, 2] >= intThresMS1, ]
          }
        }

        if (MS2 & hd2$msLevel[s] == 2 & !is.null(mzr_ms2)) {
          altered <- TRUE

          spec[[s]] <- spec[[s]][spec[[s]][, 1] >= mzr_ms2[1] &
                                   spec[[s]][, 1] <= mzr_ms2[2], ]

          if (!is.matrix(spec[[s]])) {
            spec[[s]] <- matrix(spec[[s]], ncol = 2,
                                dimnames = list(1, c("mz", "intensity")))
          }

          if (!is.null(intensityThreshold)) {
            spec[[s]] <- spec[[s]][spec[[s]][, 2] >= intThresMS2, ]
          }
        }

        # if altered update hd2
        if (altered) {

          if (!is.matrix(spec[[s]])) {
            spec[[s]] <- matrix(spec[[s]], ncol = 2,
                                dimnames = list(1, c("mz", "intensity")))
          }

          hd2$peaksCount[s] <- nrow(spec[[s]])
          hd2$totIonCurrent[s] <- sum(spec[[s]][, 2])

          if (nrow(spec[[s]]) > 0) {
            hd2$basePeakMZ[s] <-
              spec[[s]][spec[[s]][, 2] == max(spec[[s]][, 2]), 1][1] # when duplicated get the first
            hd2$basePeakIntensity[s] <-
              spec[[s]][spec[[s]][, 2] == max(spec[[s]][, 2]), 2][1]
            hd2$lowMZ[s] <- min(spec[[s]][, 1])[1]
            hd2$highMZ[s] <- max(spec[[s]][, 1])[1]
            hd2$scanWindowLowerLimit[s] <- min(spec[[s]][, 1])[1]
            hd2$scanWindowUpperLimit[s] <- max(spec[[s]][, 1])[1]

          } else {
            hd2$basePeakMZ[s] <- 0
            hd2$basePeakIntensity[s] <- 0
            hd2$lowMZ[s] <- 0
            hd2$highMZ[s] <- 0
            hd2$scanWindowLowerLimit[s] <- 0
            hd2$scanWindowUpperLimit[s] <- 0
          }
        }
      }
    }

    hd2$seqNum <- seq_len(nrow(hd2))
    rownames(hd2) <- seq_len(nrow(hd2))
    mzR::close(msf)


    format_tag <- "mzml"
    if (grepl("XML", f)) {
      format_tag <- "mzxml"
    }

    savePath <- dirname(f)
    if (!is.null(path)) savePath <- path

    if (copyMetadata) {
      mzR::copyWriteMSData(
        object = spec,
        file = paste0(savePath, "/", prefix, basename(f)),
        original_file = f,
        header = hd2,
        outformat = format_tag,
        software_processing = unlist(
          c("mzR", paste0(packageVersion("mzR")), "MS:-1", "Trimmed spectra"))
      )
    } else {
      mzR::writeMSData(
        object = spec,
        file = paste0(savePath, "/", prefix, basename(f)),
        header = hd2,
        outformat = format_tag,
        software_processing = unlist(
          c("mzR", paste0(packageVersion("mzR")), "MS:-1", "Trimmed spectra"))
      )
    }
    setTxtProgressBar(pb, which(f == files))
  }
  cat(" Done! \n")
  close(pb)
}

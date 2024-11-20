
#' check_compatible_ms_formats_for_conversion
#'
#' @description List of possible vendor formats to be converted to
#' `mzML` or `mzXML`.
#'
#' @references
#' \insertRef{proteo01}{StreamFind}
#'
#' \insertRef{proteo02}{StreamFind}
#'
#' @return A `data.frame` with the formats compatible for conversion.
#'
#' @noRd
#'
check_compatible_ms_formats_for_conversion <- function() {
  df_formats <- data.frame(
    type = c(rep("ms", 11)),
    vendor = c(
      "agilent", rep("bruker", 5), "sciex", "shimadzu",
      "thermo scientific", rep("waters", 2)
    ),
    format = c(
      ".d", ".d", ".yep", ".baf", ".fid", ".tdf",
      ".wiff", ".lcd", ".RAW", ".RAW", "UNIFI"
    )
  )

  return(df_formats)
}

#' Function to convert vendor MS files to *mzML* or *mzXML*
#'
#' @description Function to convert vendor MS files to *.mzML* or *.mzXML*
#' using the command line interface of
#' \href{https://proteowizard.sourceforge.io/download.html}{MSConvert}
#' from \href{https://proteowizard.sourceforge.io/}{ProteoWizard}.
#'
#' @param files A vector with file full path/s for conversion.
#' @param outputFormat A character string with the format of the output file.
#' Possible values are "mzML" and "mzXML".
#' @param outputPath The directory to place the output file. When \code{NULL}
#' the directory of the original file is used.
#' @param optList A list of further options to be passed to the respective
#' conversion tool. See documentation in
#' \url{https://proteowizard.sourceforge.io/tools/msconvert.html}
#' for possible commands.
#'
#' @note When giving paths make sure that single backslash is changed to
#' double backslash or to single forward slash.
#'
#' @references
#'
#' \insertRef{proteo01}{StreamFind}
#'
#' \insertRef{proteo02}{StreamFind}
#'
#' @export
#'
#' @examples
#' ## The full file path
#' #> file = E:/ms_file.d
#'
#' ## Options to centroid MS1 and MS2 data in file
#' #> optList <- list(filter = "peakPicking vendor msLevel=1-2")
#'
#' ## Convert to mzML
#' #> convert_ms_files(file, outfile = "mzML", optList = optList)
#'
#'
convert_ms_files <- function(files = NULL, outputFormat = "mzML", outputPath = NULL, optList = NULL) {
  # files = "E:\\02_QC_pos-r001.d"

  files <- gsub("\\\\", "/", files)

  # check if files are compatible
  files_check <- lapply(files, function(x, comfor) {
    temp <- comfor[grepl(paste0(".", tools::file_ext(x)), comfor$format, fixed = TRUE), ]
    temp$file <- x
    return(data.table::copy(temp))
  }, comfor = check_compatible_ms_formats_for_conversion())

  files_check <- data.table::rbindlist(files_check)

  files_check <- unique(files_check, by = c("format", "file"))

  if (nrow(files_check) > 0) {
    files_ms <- files_check$file

    outputFormats_pwiz <- c(
      "mzML", "mzXML", "mz5", "mgf", "text",
      "ms1", "cms1", "ms2", "cms2"
    )

    if (length(outputFormat) != 1 & !outputFormat %in% outputFormats_pwiz) {
      warning("Output file format must be mzML or mzXML")
      return(NULL)
    }

    # check if MSConvert is installed and configured
    version <- system("msconvert --help", intern = TRUE)
    if (!(TRUE %in% grepl("ProteoWizard release:", version))) {
      warning("msConvert from ProteoWizard not found! Make sure it is installed and added to the system PATH.")
      return()
    }

    version <- version[grepl("ProteoWizard release:", version)]
    version <- sub(".*: ", "", version)
    version <- sub(" \\(.*", "", version)
    version <- gsub("\\.", "", version)

    lapply(files_ms, function(f) {
      cmd_tx <- paste0("msconvert \"", f, "\"")

      if (!is.null(outputPath)) {
        cmd_tx <- paste0(cmd_tx, " -o \"", outputPath, "\"")
      } else {
        cmd_tx <- paste0(cmd_tx, " -o \"", dirname(f), "\"")
      }

      cmd_tx <- paste0(cmd_tx, " --", outputFormat)

      # if (!is.null(optList)) {
      #   for (i in seq_len(length(optList))) {
      #     cmd_tx <- paste0(cmd_tx, " --", names(optList[i]))
      #     cmd_tx <- paste0(cmd_tx, ' "', optList[[i]], '"')
      #   }
      # }
      
      if (!is.null(optList)) {
        for (i in seq_len(length(optList))) {
          cmd_tx <- paste0(cmd_tx, " --", names(optList[i]), " \"", optList[[i]], "\"")
        }
      }

      system(cmd_tx, intern = FALSE)
    })

    cat("Files converted! \n")
  }
}

#' Function to trim spectra in *mzML* or *mzXML* files
#'
#' @description Trim spectra in \emph{mzML} or \emph{mzXML} files based on time
#' (in seconds) and  \emph{m/z} (in Da) ranges, using the \pkg{mzR} package.
#'
#' @param files A character vector with the full path of the file/s to trim.
#' @param MS1 Set to TRUE for trimming MS level 1 data.
#' @param MS2 Set to TRUE for trimming MS level 2 data.
#' @param rtWindow A numeric vector of length 2 with the minimum and maximum
#' time range to trim the files, in seconds.
#' @param mzWindow A numeric vector of length 2 with the minimum and maximum
#' \emph{m/z} range to trim the files, in Da.
#' @param mzWindow_ms2 A numeric vector of length 2 with the minimum and maximum
#' \emph{m/z} range to trim MS2 data files, in Da. when NULL and MS2 is TRUE
#' the \code{mzWindow} argument is used instead.
#' @param intensityThreshold A numeric vector of length one with the minimum
#' intensity threshold. Traces below the given intensity threshold are removed.
#' If a length two vector is given, the first value if applied for MS1 data
#' and the second to MS2 data.
#' @param copyMetadata Set to TRUE to copy metadata from the original file.
#' @param path A character string with the path to save the files.
#' When \code{NULL}, the file directory is used instead.
#' @param prefix A character string with the name prefix for the new
#' trimmed file.
#'
#' @note When giving paths make sure that single backslash is changed to
#' double backslash of to single forward slash.
#'
#' @return Saves the trimmed \emph{mzML} or \emph{mzXML} files
#' in a given path, adding a pre-defined prefix to the file name.
#'
#' @export
#'
#' @references
#' \insertRef{mzr01}{StreamFind}
#'
#' \insertRef{mzr02}{StreamFind}
#'
#' \insertRef{mzr03}{StreamFind}
#'
#' \insertRef{mzr04}{StreamFind}
#'
trim_ms_files_spectra <- function(files, MS1 = TRUE, MS2 = TRUE,
                                  rtWindow = NULL, mzWindow = NULL,
                                  mzWindow_ms2 = NULL,
                                  intensityThreshold = NULL, copyMetadata = TRUE,
                                  path = NULL, prefix = "trim_") {
  
  if (!requireNamespace("mzR", quietly = TRUE)) return(warning("mzR package not found!"))

  if (!is.null(rtWindow) & !is.numeric(rtWindow)) return(warning("rtWindow must be numeric!"))

  if (!is.null(rtWindow) & length(rtWindow) != 2) {
    return(warning("rtWindow not valid! A vector with length 2 is expected for time range, in seconds."))
  }

  if (!is.null(mzWindow) & !is.numeric(mzWindow)) return(warning("mzWindow must be numeric!"))

  if (!is.null(mzWindow) & length(mzWindow) != 2) {
    return(warning("mzWindow not valid! A numeric vector with length 2 is expected for m/z range, in Da."))
  }

  if (!is.null(mzWindow_ms2)) {
    if (!is.numeric(mzWindow_ms2)) {
      return(warning("mzWindow_ms2 must be numeric!"))
    }

    if (length(mzWindow_ms2) != 2) {
      return(warning("mzWindow_ms not valid! A numeric vector with length 2 is expected for m/z range, in Da."))
    }
  }

  if (MS2 & is.null(mzWindow_ms2)) mzWindow_ms2 <- mzWindow

  if (!is.null(intensityThreshold)) {
    if (!is.numeric(intensityThreshold)) {
      return(warning("Intensity threshold must be numeric!"))
    }

    if (length(intensityThreshold) > 2) return(warning("Intensity threshold must be maximum of length 2!"))

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

    if (!is.null(rtWindow)) {
      hd2 <- hd[hd$retentionTime >= rtWindow[1] &
        hd$retentionTime <= rtWindow[2], ]
    }

    if (!is.null(mzWindow) | !is.null(mzWindow_ms2)) {
      spec <- mzR::peaks(msf, scans = hd2$seqNum)

      for (s in seq_len(length(spec))) {
        altered <- FALSE

        if (!is.matrix(spec[[s]])) {
          spec[[s]] <- matrix(spec[[s]],
            ncol = 2,
            dimnames = list(1, c("mz", "intensity"))
          )
        }

        if (MS1 & hd2$msLevel[s] == 1 & !is.null(mzWindow)) {
          altered <- TRUE
          spec[[s]] <- spec[[s]][spec[[s]][, 1] >= mzWindow[1] &
            spec[[s]][, 1] <= mzWindow[2], ]

          if (!is.matrix(spec[[s]])) {
            spec[[s]] <- matrix(spec[[s]],
              ncol = 2,
              dimnames = list(1, c("mz", "intensity"))
            )
          }

          if (!is.null(intensityThreshold)) {
            spec[[s]] <- spec[[s]][spec[[s]][, 2] >= intThresMS1, ]
          }
        }

        if (MS2 & hd2$msLevel[s] == 2 & !is.null(mzWindow_ms2)) {
          altered <- TRUE

          spec[[s]] <- spec[[s]][spec[[s]][, 1] >= mzWindow_ms2[1] &
            spec[[s]][, 1] <= mzWindow_ms2[2], ]

          if (!is.matrix(spec[[s]])) {
            spec[[s]] <- matrix(spec[[s]],
              ncol = 2,
              dimnames = list(1, c("mz", "intensity"))
            )
          }

          if (!is.null(intensityThreshold)) {
            spec[[s]] <- spec[[s]][spec[[s]][, 2] >= intThresMS2, ]
          }
        }

        # if altered update hd2
        if (altered) {
          if (!is.matrix(spec[[s]])) {
            spec[[s]] <- matrix(spec[[s]],
              ncol = 2,
              dimnames = list(1, c("mz", "intensity"))
            )
          }

          hd2$peaksCount[s] <- nrow(spec[[s]])
          hd2$totIonCurrent[s] <- sum(spec[[s]][, 2])

          if (nrow(spec[[s]]) > 0) {
            hd2$basePeakMZ[s] <-
              spec[[s]][spec[[s]][, 2] == max(spec[[s]][, 2]), 1][1]
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
          c("mzR", paste0(packageVersion("mzR")), "MS:-1", "Trimmed spectra")
        )
      )
    } else {
      mzR::writeMSData(
        object = spec,
        file = paste0(savePath, "/", prefix, basename(f)),
        header = hd2,
        outformat = format_tag,
        software_processing = unlist(
          c("mzR", paste0(packageVersion("mzR")), "MS:-1", "Trimmed spectra")
        )
      )
    }
    setTxtProgressBar(pb, which(f == files))
  }
  cat(" Done! \n")
  close(pb)
}

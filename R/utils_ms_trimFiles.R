

#' @title trimSpectraMZR
#'
#' @description Trim \emph{mzML} or \emph{mzXML} files based on time (in seconds)
#' and  \emph{m/z} (in Da) ranges using the \pkg{mzR} package.
#'
#' @param files A character vector with the full path of the files to trim.
#' @param rtr A numeric vector of length 2 with the maximum and minimum
#' time range to trim the files, in seconds.
#' @param mzr A numeric vector of length 2 with the maximum and minimum
#' \emph{m/z} range to trim the files, in Da.
#' @param onlyMS1 Set to TRUE for trimming only MS level one data.
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
#' @importFrom mzR openMSfile header peaks close copyWriteMSData writeMSData
#'
trimSpectraFilesMZR <- function(files, rtr = NULL, mzr = NULL, onlyMS1 = TRUE,
                                copyMetadata = TRUE, path = NULL, prefix = "trim_") {

  if (length(rtr) != 2 & !is.numeric(rtr)) {
    return(warning("rtr not valid! A numeric vector with
            length 2 is expected for time range, in seconds."))
  }

  if (length(mzr) != 2 & !is.numeric(mzr)) {
    return(warning("mzr not valid! A numeric vector with
            length 2 is expected for m/z range, in Da."))
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

    if (!tools::file_ext(f) %in% c("mzML", "mzXML")) {
      warning(paste0("File ", basename(f), " not as mzML or mzXML. Skipped!"))
      next
    }

    msf <- mzR::openMSfile(f)
    hd <- mzR::header(msf)

    if (!is.null(rtr)) {
      hd2 <- hd[hd$retentionTime >= rtr[1] & hd$retentionTime <= rtr[2], ]
    }

    if (!is.null(mzr)) {
      spec <- mzR::peaks(msf, scans = hd2$seqNum)
      for (s in seq_len(length(spec))) {
        if (onlyMS1 & hd2$msLevel[s] > 1) next
        spec[[s]] <- spec[[s]][spec[[s]][, 1] >= mzr[1] & spec[[s]][, 1] <= mzr[2], ]
      }
    }

    hd2$seqNum <- seq_len(nrow(hd2))
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
        software_processing = unlist(c("mzR", paste0(packageVersion("mzR")), "MS:-1", "Trimmed spectra"))
      )
    } else {
      mzR::writeMSData(
        object = spec,
        file = paste0(savePath, "/", prefix, basename(f)),
        header = hd2,
        outformat = format_tag,
        software_processing = unlist(c("mzR", paste0(packageVersion("mzR")), "MS:-1", "Trimmed spectra"))
      )
    }
    setTxtProgressBar(pb, which(f == files))
  }
  cat(" Done! \n")
  close(pb)
}

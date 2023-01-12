
get_ms_chromatograms_from_file_ext <- function(file, minIntensity = 0) {

  inval <- FALSE

  if (!file.exists(file)) inval <- TRUE

  ms_file_formats <- ".mzML|.mzXML"
  check_file_format <- grepl(ms_file_formats, file)

  if (!check_file_format) inval <- TRUE

  if (inval) {
    warning("File format not valid!")
    return(data.frame())
  }

  if (FALSE %in% requireNamespace("mzR", quietly = TRUE)) {
    warning("mzR package not found!")
    return(data.frame())
  }



  zF <- mzR::openMSfile(file, backend = "pwiz")

  cH <- as.data.table(suppressWarnings(mzR::chromatogramHeader(zF)))

  if (nrow(cH) > 0) {

    cH$polarity <- as.character(cH$polarity)
    cH[polarity == 1, polarity := "positive"]
    cH[polarity == 0, polarity := "negative"]
    cH[polarity == -1, polarity := NA_character_]

    cC <- mzR::chromatograms(zF, cH$chromatogramIndex)

    if (!is.data.frame(cC)) {

      names(cC) <- as.character(cH$chromatogramIndex)
      cC <- lapply(cC, function(x) {
        x <- as.data.frame(x)
        colnames(x) <- c("rt", "intensity")
        if (max(x$rt) < 60) x$rt <- x$rt * 60
        return(x)
      })

      cC <- rbindlist(cC, idcol = "index", fill = TRUE)
      cC$index <- as.numeric(cC$index)

      cH_b <- data.table(
        "index" = as.numeric(cH$chromatogramIndex),
        "id" = cH$chromatogramId,
        "polarity" = cH$polarity,
        "preMZ" = cH$precursorIsolationWindowTargetMZ,
        "mz" = cH$productIsolationWindowTargetMZ
      )

      chroms_data <- cH_b[cC, on = "index"]

    } else {

      colnames(cC) <- c("rt", "intensity")
      if (max(cC$rt) < 60) cC$rt <- cC$rt * 60

      chroms_data <- data.table(
        "index" = cH$chromatogramIndex,
        "id" = cH$chromatogramId,
        "polarity" = cH$polarity,
        "preMZ" = cH$precursorIsolationWindowTargetMZ,
        "mz" = cH$productIsolationWindowTargetMZ,
        "rt" = cC$rt,
        "intensity" = cC$intensity
      )

    }

    chroms_data <- chroms_data[chroms_data$intensity > minIntensity, ]

    return(chroms_data)

  }

  return(data.frame())
}

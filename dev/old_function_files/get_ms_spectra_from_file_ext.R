
get_ms_spectra_from_file_ext <- function(file,
                                         levels = NULL,
                                         rtr = NULL,
                                         preMZrange = NULL,
                                         minIntensityMS1 = 0,
                                         minIntensityMS2 = 0) {

  inval <- FALSE

  if (!file.exists(file)) inval <- TRUE

  ms_file_formats <- ".mzML|.mzXML"
  check_file_format <- grepl(ms_file_formats, file)

  if (!check_file_format) inval <- TRUE

  if (inval) {
    warning("File format not valid!")
    return(data.table::data.table())
  }

  if (FALSE %in% requireNamespace("mzR", quietly = TRUE)) {
    warning("mzR package not found!")
    return(data.table::data.table())
  }



  zF <- mzR::openMSfile(file, backend = "pwiz")

  zH <- mzR::header(zF)

  if (nrow(zH) > 0) {

    if (max(zH$retentionTime) < 60) zH$retentionTime <- zH$retentionTime * 60

    if (!is.null(levels)) zH <- zH[zH$msLevel %in% levels, ]

    temp_checkOverlapRanges <- function(vals, ranges) {
      return(rowSums(mapply(function(a, b) data.table::between(vals, a, b),
                            ranges$min, ranges$max)) > 0)
    }

    if (!is.null(rtr)) {
      zH <- zH[temp_checkOverlapRanges(zH$retentionTime, rtr), ]
    }

    if(!is.null(preMZrange)) {
      zH <- zH[temp_checkOverlapRanges(zH$precursorMZ, preMZrange), ]
    }

    zD <- mzR::peaks(zF, scans = zH$seqNum)

    mat_idx <- rep(zH$seqNum, sapply(zD, nrow))
    zD <- data.table::as.data.table(do.call(rbind, zD))
    zD$index <- mat_idx

    if (TRUE %in% (unique(zH$msLevel) == 2)) {

      zH_b <- data.table::data.table(
        "index" = zH$seqNum,
        "scan" = zH$acquisitionNum,
        "level" = zH$msLevel,
        "ce" = zH$collisionEnergy,
        "preScan" = zH$precursorScanNum,
        "preMZ" = zH$precursorMZ,
        "rt" = zH$retentionTime
      )

    } else {

      zH_b <- data.table::data.table(
        "index" = zH$seqNum,
        "scan" = zH$acquisitionNum,
        "level" = zH$msLevel,
        "rt" = zH$retentionTime
      )

    }

    if (!all(is.na(zH$ionMobilityDriftTime))) {
      rt_unique <- unique(zH_b$rt)
      frame_numbers <- seq_len(length(rt_unique))
      if ("preMZ" %in% colnames(zH_b)) zH_b$preMZ <- NA_real_
      zH_b$frame <- factor(zH_b$rt, levels = rt_unique, labels = frame_numbers)
      zH_b$driftTime <- zH$ionMobilityDriftTime
    }

    zH_n <- data.table::merge.data.table(zH_b, zD, by = "index")

    zH_n <- zH_n[!(zH_n$intensity <= minIntensityMS1 & zH_n$level == 1), ]
    zH_n <- zH_n[!(zH_n$intensity <= minIntensityMS2 & zH_n$level == 2), ]

    if (exists("zF")) suppressWarnings(mzR::close(zF))

    return(zH_n)

  }

  return(data.table::data.table())
}

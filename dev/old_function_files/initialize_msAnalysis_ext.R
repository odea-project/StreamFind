
initialize_msAnalysis_ext <- function(file) {

  inval <- FALSE

  if (!file.exists(file)) inval <- TRUE

  ms_file_formats <- ".mzML|.mzXML"

  if (FALSE %in% grepl(ms_file_formats, file)) inval <- TRUE

  if (inval) {
    warning("File format not valid!")
    return(list())
  }

  if (FALSE %in% requireNamespace("mzR", quietly = TRUE)) {
    warning("mzR package not found!")
    return(list())
  }



  zF <- mzR::openMSfile(file, backend = "pwiz")
  zH <- suppressWarnings(mzR::header(zF))
  cH <- suppressWarnings(mzR::chromatogramHeader(zF))
  instrument_info <- mzR::instrumentInfo(zF)
  run_info <- suppressWarnings(mzR::runInfo(zF))



  if (1 %in% zH$polarity) {
    polarity_pos <- "positive"
  } else polarity_pos <- NULL

  if (0 %in% zH$polarity) {
    polarity_neg <- "negative"
  } else polarity_neg <- NULL

  if (nrow(cH) > 0 & ("polarity" %in% colnames(cH))) {

    if (1 %in% cH$polarity) {
      polarity_pos_chroms <- "positive"
    } else polarity_pos_chroms <- NULL

    if (0 %in% cH$polarity) {
      polarity_neg_chroms <- "negative"
    } else polarity_neg_chroms <- NULL

  } else {
    polarity_neg_chroms <- NULL
    polarity_pos_chroms <- NULL
  }

  polarities <- unique(
    c(polarity_pos, polarity_neg, polarity_neg_chroms, polarity_pos_chroms)
  )

  if (is.null(polarities)) polarities <- NA_character_



  spectra_number <- run_info$scanCount

  if (TRUE %in% zH$centroided) {
    spectra_mode <- "centroid"
  } else if (FALSE %in% zH$centroided) {
    spectra_mode <- "profile"
  } else {
    spectra_mode <- NA_character_
  }

  if (!all(is.na(zH$ionMobilityDriftTime))) {
    ion_mobility <- TRUE
  } else {
    ion_mobility <- FALSE
  }

  if (grepl(".mzML", file)) {
    chromatograms_number <- mzR::nChrom(zF)
  } else {
    chromatograms_number <- 0
  }



  if (spectra_number == 0 & chromatograms_number > 0) {

    if (TRUE %in% grepl("SRM", cH$chromatogramId)) {
      data_type <- "SRM"
    }

    tic <- cH[cH$chromatogramId %in% "TIC", ]
    if (nrow(tic) > 0) {
      tic <- mzR::chromatograms(zF, tic$chromatogramIndex)
      colnames(tic) <- c("rt", "intensity")
      if (max(tic$rt) < 60) tic$rt <- tic$rt * 60
    } else tic <- data.frame()

    bpc <- cH[cH$chromatogramId %in% "BPC", ]

    if (nrow(bpc) > 0) {
      bpc <- mzR::chromatograms(zF, bpc$chromatogramIndex)
      colnames(bpc) <- c("rt", "intensity")
      if (max(bpc$rt) < 60) bpc$rt <- bpc$rt * 60
    } else bpc <- data.frame()

  } else if (spectra_number > 0) {

    if (2 %in% run_info$msLevels) {
      data_type <- "MS/MS"
    } else {
      data_type <- "MS"
    }

    if (max(zH$retentionTime) < 60) zH$retentionTime <- zH$retentionTime * 60

    zH_ms1 <- zH[zH$msLevel == 1, ]

    tic <- data.frame(
      "rt" = zH_ms1$retentionTime,
      "intensity" = zH_ms1$totIonCurrent
    )

    bpc <- data.frame(
      "rt" = zH_ms1$retentionTime,
      "mz" = zH_ms1$basePeakMZ,
      "intensity" = zH_ms1$basePeakIntensity
    )

  } else {

    data_type <- NA_character_
  }



  if (is.infinite(run_info$lowMz)) run_info$lowMz <- NA_real_
  if (is.infinite(run_info$highMz)) run_info$highMz <- NA_real_
  if (is.infinite(run_info$dStartTime)) run_info$dStartTime <- min(tic$rt)
  if (is.infinite(run_info$dEndTime)) run_info$dEndTime <- max(tic$rt)
  if (data_type %in% "SRM") run_info$msLevels <- NA_integer_



  acquisition_info <- list(
    "time_stamp" = run_info$startTimeStamp,
    "spectra_number" = spectra_number,
    "spectra_mode" = spectra_mode,
    "spectra_levels" = run_info$msLevels,
    "mz_low" = run_info$lowMz,
    "mz_high" = run_info$highMz,
    "rt_start" = run_info$dStartTime,
    "rt_end" = run_info$dEndTime,
    "polarity" = polarities,
    "chromatograms_number" = chromatograms_number,
    "ion_mobility" = ion_mobility
  )



  suppressWarnings(mzR::close(zF))



  analysis <- list(
    name = gsub(".mzML|.mzXML", "", basename(file)),
    file = file,
    type = data_type,
    instrument = instrument_info,
    acquisition = acquisition_info,
    tic = tic,
    bpc = bpc,
    spectra = data.frame(),
    chromatograms = data.frame(),
    settings = list(),
    peaks = data.frame(),
    metadata = list()
  )

  return(analysis)
}

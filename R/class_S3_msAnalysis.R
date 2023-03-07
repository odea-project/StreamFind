#' **msAnalysis** S3 class constructor and methods
#'
#' @description
#' Creates an msAnalysis S3 class object.
#'
#' @param name *mzML* or *mzXML* file name without extension.
#' @param replicate Character with length one, representing the analysis
#' replicate group name.
#' @param blank Character with length one, representing the associated blank
#' replicate group name.
#' @param file *mzML* or *mzXML* full file path (with extension).
#' @param type Character with length one. Possible values as "MS" for only MS1
#' spectral data, "MS/MS" for tandem spectral data (i.e., MS1 and MS2) or "SRM"
#' for selected reaction monitoring data (i.e., no spectra only chromatograms).
#' @param instrument List with metadata from the instrument used to acquire the
#' data (content is highly vendor dependent).
#' @param time_stamp Character with length one, representing the start time and
#' date of the data acquisition.
#' @param spectra_number Integer with the number of spectra in the file.
#' @param spectra_mode Character with length one. Possible values are *centroid*
#' , *profile* or `NA` for centroided data, data in profile mode or mode not
#' defined or absence spectra, respectively.
#' @param spectra_levels Integer vector with the number of levels in the
#' spectra (i.e., 1 or 1 and 2).
#' @param mz_low Numeric length one with the lowest \emph{m/z} value detected
#' in the spectra, in Da.
#' @param mz_high Numeric length one with the highest \emph{m/z} value detected
#' in the spectra, in Da.
#' @param rt_start Numeric length one with the run start time, in seconds.
#' @param rt_end Numeric length one with the run end time, in seconds.
#' @param polarity Character length one. Possible values are *positive*,
#' *negative* or *both* (the latter refers to polarity switching acquisition).
#' @param chromatograms_number Integer with the number of chromatograms in the
#' file.
#' @param ion_mobility Logical length one for presence or absence of drift
#' separation from ion mobility.
#' @param tic data.table with the total ion chromatogram.
#' @param bpc data.table with the base peak chromatogram.
#' @param spectra data.table with the raw spectra (only present if loaded).
#' @param chromatograms data.table with the raw chromatograms (only present
#' if loaded).
#' @param features data.table with the features from data processing.
#' @param metadata List with flexible storage for experimental metadata
#' (e.g., concentration, location, etc.).
#'
#' @return An msAnalysis S3 class object.
#'
#' @export
#'
msAnalysis <- function(name = NA_character_,
                       replicate = NA_character_,
                       blank = NA_character_,
                       file = NA_character_,
                       type = NA_character_,
                       instrument = list(),
                       time_stamp = NA_character_,
                       spectra_number = NA_integer_,
                       spectra_mode = NA_character_,
                       spectra_levels = NA_integer_,
                       mz_low = NA_real_,
                       mz_high = NA_real_,
                       rt_start = NA_real_,
                       rt_end = NA_real_,
                       polarity = NA_character_,
                       chromatograms_number = NA_integer_,
                       ion_mobility = NA,
                       tic = data.table(),
                       bpc = data.table(),
                       spectra = data.table(),
                       chromatograms = data.table(),
                       features = data.table(),
                       metadata = list()) {

  if (is.data.frame(features)) {
    if ("ms1" %in% colnames(features)) {
      features$ms1 <- lapply(features$ms1, as.data.table)
    }

    if ("ms2" %in% colnames(features)) {
      features$ms2 <- lapply(features$ms2, as.data.table)
    }
  }

  if (is.list(features)) {
    if ("ms1" %in% names(features)) {
      features$ms1 <- lapply(features$ms1, as.data.table)
    }

    if ("ms2" %in% names(features)) {
      features$ms2 <- lapply(features$ms2, as.data.table)
    }
  }

  x <- list(
    "name" = name,
    "replicate" = replicate,
    "blank" = blank,
    "file" = file,
    "type" = type,
    "instrument" = instrument,
    "time_stamp" = time_stamp,
    "spectra_number" = spectra_number,
    "spectra_mode" = spectra_mode,
    "spectra_levels" = spectra_levels,
    "mz_low" = mz_low,
    "mz_high" = mz_high,
    "rt_start" = rt_start,
    "rt_end" = rt_end,
    "polarity" = polarity,
    "chromatograms_number" = chromatograms_number,
    "ion_mobility" = ion_mobility,
    "tic" = tic,
    "bpc" = bpc,
    "spectra" = spectra,
    "chromatograms" = chromatograms,
    "features" = features,
    "metadata" = metadata
  )

  x$name <- as.character(x$name)
  x$replicate <- as.character(x$replicate)
  x$blank <- as.character(x$blank)
  if (is.na(x$blank)) x$blank <- NA_character_
  x$file <- as.character(x$file)
  x$type <- as.character(x$type)
  x$time_stamp <- as.character(x$time_stamp)
  x$spectra_number <- as.integer(x$spectra_number)
  x$spectra_mode <- as.character(x$spectra_mode)
  x$spectra_levels <- as.integer(x$spectra_levels)
  x$mz_low <- as.numeric(x$mz_low)
  x$mz_high <- as.numeric(x$mz_high)
  x$rt_start <- as.numeric(x$rt_start)
  x$rt_end <- as.numeric(x$rt_end)
  x$polarity <- as.character(x$polarity)
  x$chromatograms_number <- as.integer(x$chromatograms_number)
  x$ion_mobility <- as.logical(x$ion_mobility)
  x$tic <- as.data.table(x$tic)
  x$bpc <- as.data.table(x$bpc)
  x$spectra <- as.data.table(x$spectra)
  x$chromatograms <- as.data.table(x$chromatograms)
  x$features <- as.data.table(x$features)

  if (validate.msAnalysis(x)) {
    structure(x, class = "msAnalysis")
  } else {
    NULL
  }
}

#' @describeIn msAnalysis
#' Validates an msAnalysis S3 class object, returning a logical value of length
#' one.
#'
#' @param msAnalysis A msAnalysis S3 class object.
#'
#' @export
#'
validate.msAnalysis <- function(msAnalysis = NULL) {
  valid <- FALSE
  name <- NA_character_

  if (is.list(msAnalysis)) {
    valid <- TRUE

    if (length(msAnalysis$name) != 1 & !is.character(msAnalysis$name)) {
      warning("Analysis name not conform!")
      valid <- FALSE
    } else {
      name <- msAnalysis$name
    }

    if (length(msAnalysis$replicate) != 1 &
        !is.character(msAnalysis$replicate)) {
      warning("Analysis replicate name not conform!")
      valid <- FALSE
    }

    if (length(msAnalysis$blank) != 1 & !is.character(msAnalysis$blank)) {
      warning("Analysis blank name not conform!")
      valid <- FALSE
    }

    if (length(msAnalysis$file) != 1 & !is.character(msAnalysis$file)) {
      warning("Analysis file path entry not conform!")
      valid <- FALSE
    } else if (!file.exists(msAnalysis$file)) {
      warning(paste0(
        msAnalysis$file,
        " does not exist! Update file paths with msData$update_files() method"
      ))
      valid <- FALSE
    }

    if (length(msAnalysis$type) != 1) {
      warning("Analysis type entry not conform!")
      valid <- FALSE
    } else if (!(msAnalysis$type %in% c("MS", "MS/MS", "SRM"))) {
      warning("Analysis type must be 'MS', 'MS/MS' or 'SRM'!")
      valid <- FALSE
    }

    if (!is.integer(msAnalysis$spectra_number) &&
        length(msAnalysis$spectra_number) != 1) {
      warning("Analysis spectra_numebr entry not conform!")
      valid <- FALSE
    }

    if (!is.integer(msAnalysis$chromatograms_number) &&
        length(msAnalysis$chromatograms_number) != 1) {
      warning("Analysis chromatograms_number entry not conform!")
      valid <- FALSE
    }

    if (!is.character(msAnalysis$spectra_mode) &
        length(msAnalysis$spectra_mode) != 1) {
      warning("Analysis spectra_mode entry not conform!")
      valid <- FALSE
    }

    if (!is.integer(msAnalysis$spectra_levels)) {
      warning("Analysis spectra_levels entry not conform!")
      valid <- FALSE
    }

    if (length(msAnalysis$mz_low) != 1 & !is.numeric(msAnalysis$mz_low)) {
      warning("Analysis mz_low entry not conform!")
      valid <- FALSE
    }

    if (length(msAnalysis$mz_high) != 1 & !is.numeric(msAnalysis$mz_high)) {
      warning("Analysis mz_high entry not conform!")
      valid <- FALSE
    }

    if (length(msAnalysis$rt_start) != 1 & !is.numeric(msAnalysis$rt_start)) {
      warning("Analysis rt_start entry not conform!")
      valid <- FALSE
    }

    if (length(msAnalysis$rt_end) != 1 & !is.numeric(msAnalysis$rt_end)) {
      warning("Analysis rt_end entry not conform!")
      valid <- FALSE
    }

    if (!is.character(msAnalysis$polarity)) {
      warning("Analysis polarity entry not conform!")
      valid <- FALSE
    } else if (FALSE %in%
        (msAnalysis$polarity %in% c("positive", "negative", NA_character_))) {
      warning("Analysis polarity entry not conform!")
      valid <- FALSE
    }

    if (length(msAnalysis$ion_mobility) != 1 &
        !is.logical(msAnalysis$ion_mobility)) {
      warning("Analysis ion_mobility entry not conform!")
      valid <- FALSE
    }

    if (!is.data.frame(msAnalysis$tic)) {
      warning("Analysis tic entry not conform!")
      valid <- FALSE
    } else if (FALSE %in%
        (c("rt", "intensity") %in% colnames(msAnalysis$tic))) {
      warning("Analysis tic data.table must have columns rt and intensity!")
      valid <- FALSE
    }

    if (!is.data.frame(msAnalysis$bpc)) {
      warning("Analysis bpc entry not conform!")
      valid <- FALSE
    } else if (FALSE %in%
        (c("rt", "mz", "intensity") %in% colnames(msAnalysis$bpc))) {
      warning("Analysis bpc data.table must have columns mz, rt and intensity!")
      valid <- FALSE
    }

    if (!is.data.frame(msAnalysis$spectra)) {
      warning("Analysis spectra entry not conform!")
      valid <- FALSE
    }

    if (!is.data.frame(msAnalysis$chromatograms)) {
      warning("Analysis chromatograms entry not conform!")
      valid <- FALSE
    }

    if (!is.data.frame(msAnalysis$features)) {
      warning("Analysis features entry not conform!")
      valid <- FALSE
    }

    if (!is.list(msAnalysis$metadata)) {
      warning("Analysis netadata entry not conform!")
      valid <- FALSE
    }
  }

  if (!valid) warning("Issue/s found with analysis ", msAnalysis$name, "!")

  valid
}

#' @describeIn msAnalysis
#' Converts the argument in an msAnalysis S3 class object.
#'
#' @param value A list to be checked and/or converted to msAnalysis S3 class.
#'
#' @export
as.msAnalysis <- function(value) {
  if (length(value) == 1 & is.list(value)) value <- value[[1]]
  do.call(msAnalysis, value)
}

#' @describeIn msAnalysis
#' Parses information from *mzML* or *mzXML* file/s and returns a list with
#' msAnalysis S3 class object/s. On error, returns \code{NULL}.
#'
#' @param files A character vector with *mzML* or *mzXML* full file path/s.
#' Alternatively, a data.frame with the column/s file, replicate and blank
#' with the full file path/s, the replicate group name/s (string) and the
#' associated blank replicate group name (string).
#' @template arg-runParallel
#'
#' @export
parse.msAnalysis <- function(files = NULL, runParallel = FALSE) {
  if (is.data.frame(files)) {
    if ("file" %in% colnames(files)) {
      if ("replicate" %in% colnames(files)) {
        replicates <- as.character(files$replicate)
      } else {
        replicates <- rep(NA_character_, nrow(files))
      }

      if ("blank" %in% colnames(files)) {
        blanks <- as.character(files$blank)
      } else {
        blanks <- NULL
      }

      files <- files$file
    } else {
      files <- ""
    }
  } else {
    replicates <- rep(NA_character_, length(files))
    blanks <- NULL
  }

  possible_ms_file_formats <- ".mzML|.mzXML"

  valid_files <- vapply(files,
                        FUN.VALUE = FALSE,
                        function(x, possible_ms_file_formats) {
                          if (!file.exists(x)) {
                            return(FALSE)
                          }
                          if (FALSE %in% grepl(possible_ms_file_formats, x)) {
                            return(FALSE)
                          }
                          TRUE
                        }, possible_ms_file_formats = possible_ms_file_formats
  )

  if (!all(valid_files)) {
    warning("File/s not valid!")
    return(NULL)
  }

  cached_analyses <- FALSE

  if (requireNamespace("patRoon")) {
    hash <- patRoon:::makeHash(files)
    analyses <- patRoon:::loadCacheData("parsed_ms_analyses", hash)

    if (!is.null(analyses)) {
      if (all(vapply(analyses, validate.msAnalysis, FALSE))) {
        cached_analyses <- TRUE
      }
    }

  } else {
    hash <- NULL
    analyses <- NULL
  }

  if (runParallel & length(files) > 1 & !cached_analyses) {
    workers <- parallel::detectCores() - 1
    if (length(files) < workers) workers <- length(files)
    par_type <- "PSOCK"
    if (parallelly::supportsMulticore()) par_type <- "FORK"
    cl <- parallel::makeCluster(workers, type = par_type)
    doParallel::registerDoParallel(cl)
    # on.exit(parallel::stopCluster(cl))
  } else {
    registerDoSEQ()
  }

  i = NULL

  # with mzR -----
  if (requireNamespace("mzR") & !cached_analyses) {
    analyses <- foreach(i = files, .packages = "mzR") %dopar% {
      file_link <- mzR::openMSfile(i, backend = "pwiz")
      sH <- suppressWarnings(mzR::header(file_link))
      cH <- suppressWarnings(mzR::chromatogramHeader(file_link))
      instrument <- mzR::instrumentInfo(file_link)
      run <- suppressWarnings(mzR::runInfo(file_link))

      polarities <- NULL
      if (1 %in% sH$polarity) polarities <- c(polarities, "positive")
      if (0 %in% sH$polarity) polarities <- c(polarities, "negative")
      if (nrow(cH) > 0 & ("polarity" %in% colnames(cH))) {
        if (1 %in% cH$polarity) polarities <- c(polarities, "positive")
        if (0 %in% cH$polarity) polarities <- c(polarities, "negative")
      }
      if (is.null(polarities)) polarities <- NA_character_

      spectra_number <- run$scanCount
      spectra_mode <- NA_character_
      if (TRUE %in% sH$centroided) spectra_mode <- "centroid"
      if (FALSE %in% sH$centroided) spectra_mode <- "profile"

      ion_mobility <- FALSE
      if (!all(is.na(sH$ionMobilityDriftTime))) ion_mobility <- TRUE

      chromatograms_number <- 0
      if (grepl(".mzML", i)) {
        chromatograms_number <- mzR::nChrom(file_link)
      }

      if (spectra_number == 0 & chromatograms_number > 0) {
        if (TRUE %in% grepl("SRM", cH$chromatogramId)) data_type <- "SRM"

        tic <- cH[cH$chromatogramId %in% "TIC", ]
        if (nrow(tic) > 0) {
          tic <- mzR::chromatograms(file_link, tic$chromatogramIndex)
          colnames(tic) <- c("rt", "intensity")
          if (max(tic$rt) < 60) tic$rt <- tic$rt * 60
        } else {
          tic <- data.frame("rt" = numeric(), "intensity" = numeric())
        }

        bpc <- cH[cH$chromatogramId %in% "BPC", ]
        if (nrow(bpc) > 0) {
          bpc <- mzR::chromatograms(file_link, bpc$chromatogramIndex)
          if (!"mz" %in% colnames(bpc)) {
            bpc$mz <- NA
            colnames(bpc) <- c("rt", "intensity", "mz")
          } else {
            colnames(bpc) <- c("rt", "mz", "intensity")
          }

          if (max(bpc$rt) < 60) bpc$rt <- bpc$rt * 60
        } else {
          bpc <- data.frame(
            "rt" = numeric(),
            "mz" = numeric(),
            "intensity" = numeric()
          )
        }
      } else if (spectra_number > 0) {
        if (2 %in% run$msLevels) {
          data_type <- "MS/MS"
        } else {
          data_type <- "MS"
        }

        if (max(sH$retentionTime) < 60) {
          sH$retentionTime <- sH$retentionTime * 60
        }

        sH_ms1 <- sH[sH$msLevel == 1, ]

        tic <- data.frame(
          "rt" = sH_ms1$retentionTime,
          "intensity" = sH_ms1$totIonCurrent
        )

        bpc <- data.frame(
          "rt" = sH_ms1$retentionTime,
          "mz" = sH_ms1$basePeakMZ,
          "intensity" = sH_ms1$basePeakIntensity
        )
      } else {
        tic <- data.frame("rt" = numeric(),"intensity" = numeric())
        bpc <- data.frame(
          "rt" = numeric(),
          "mz" = numeric(),
          "intensity" = numeric()
        )
        data_type <- NA_character_
      }

      if (is.infinite(run$lowMz)) run$lowMz <- NA_real_
      if (is.infinite(run$highMz)) run$highMz <- NA_real_
      if (is.infinite(run$dStartTime)) run$dStartTime <- min(tic$rt)
      if (is.infinite(run$dEndTime)) run$dEndTime <- max(tic$rt)
      if (data_type %in% "SRM") run$msLevels <- NA_integer_

      analysis <- list(
        "name" = gsub(".mzML|.mzXML", "", basename(i)),
        "replicate" = NA_character_,
        "blank" = NA_character_,
        "file" = i,
        "type" = data_type,
        "instrument" = instrument,
        "time_stamp" = run$startTimeStamp,
        "spectra_number" = as.integer(spectra_number),
        "spectra_mode" = spectra_mode,
        "spectra_levels" = as.integer(run$msLevels),
        "mz_low" = as.numeric(run$lowMz),
        "mz_high" = as.numeric(run$highMz),
        "rt_start" = as.numeric(run$dStartTime),
        "rt_end" = as.numeric(run$dEndTime),
        "polarity" = polarities,
        "chromatograms_number" = as.integer(chromatograms_number),
        "ion_mobility" = ion_mobility,
        "tic" = tic,
        "bpc" = bpc,
        "spectra" = data.frame(),
        "chromatograms" = data.frame(),
        "features" = data.frame(),
        "metadata" = list()
      )

      suppressWarnings(mzR::close(file_link))
      analysis
    }

    ## with xml2 -----
  } else if (requireNamespace("xml2") & !cached_analyses) {
    analyses <- foreach(i = files, .packages = "xml2") %dopar% {

      xml_data <- read_xml(i)

      ### for mzML -----
      if (grepl("mzML", i)) {

        data_type <- NA_character_

        inst_x <- "//d1:referenceableParamGroup/d1:cvParam"
        inst_n <- xml_find_first(xml_data, xpath = inst_x)
        if (length(inst_n) > 0) {
          inst_val <- xml_attr(inst_n, "name")
        } else {
          inst_val <- NA_character_
        }
        config_x <- "//d1:componentList/child::node()"
        config_n <- xml_find_all(xml_data, xpath = config_x)
        if (length(config_n) > 0) {
          config_names <- xml_name(config_n)
          config_names_n <- xml_find_first(config_n, "d1:cvParam")
          config_vals <- xml_attr(config_names_n, "name")
        } else {
          config_names <- NA_character_
          config_vals <- NA_character_
        }
        config <- data.frame(name = config_names, value = config_vals)
        config <- split(config, config$name)
        config <- lapply(config, function(x) x$value)
        instrument <- c(list("vendor" = inst_val), config)

        time_n <- xml_find_first(xml_data, xpath = "//d1:run")
        time_val <- xml_attr(time_n, "startTimeStamp")
        if (!is.na(time_val)) {
          time_stamp <- as.POSIXct(strptime(time_val, "%Y-%m-%dT%H:%M:%SZ"))
        } else {
          time_stamp <- as.POSIXct(NA)
        }

        ms_level_x <- '//d1:spectrum/d1:cvParam[@name="ms level"]'
        ms_level_n <- xml_find_all(xml_data, xpath = ms_level_x)
        spectra_number <- length(ms_level_n)
        if (length(ms_level_n) > 0) {
          ms_levels <- sort(as.integer(unique(xml_attr(ms_level_n, "value"))))
        } else {
          ms_levels <- NA_integer_
        }

        mz_low_x <- '//d1:spectrum/d1:cvParam[@name="lowest observed m/z"]'
        mz_low_n <- xml_find_all(xml_data, xpath = mz_low_x)
        if (length(mz_low_n) > 0) {
          mz_low <- min(as.numeric(xml_attr(mz_low_n, "value")))
        } else {
          mz_low <- NA_real_
        }

        mz_high_x <- '//d1:spectrum/d1:cvParam[@name="highest observed m/z"]'
        mz_high_n <- xml_find_all(xml_data, xpath = mz_high_x)
        if (length(mz_high_n) > 0) {
          mz_high <- max(as.numeric(xml_attr(mz_high_n, "value")))
        } else {
          mz_high <- NA_real_
        }

        centroided_x <- '//d1:spectrum/d1:cvParam[@accession="MS:1000127"]'
        centroided_n <- xml_find_all(xml_data, xpath = centroided_x)
        if (length(centroided_n) > 0) {
          spectra_mode <- "centroid"
        } else {
          profile_x <- '//d1:spectrum/d1:cvParam[@accession="MS:1000128"]'
          profile_n <- xml_find_all(xml_data, xpath = profile_x)
          if (length(profile_n) > 0) {
            spectra_mode <- "profile"
          } else {
            spectra_mode <- NA_character_
          }
        }

        polarity_pos <- '//d1:spectrum/d1:cvParam[@accession="MS:1000130"]'
        polarity_pos <- xml_find_all(xml_data, polarity_pos)

        polarity_neg <- '//d1:spectrum/d1:cvParam[@accession="MS:1000129"]'
        polarity_neg <- xml_find_all(xml_data, polarity_neg)

        if (length(polarity_pos) > 0 | length(polarity_neg) > 0) {
          polarities <- c(
            unique(gsub(" scan", "", xml_attr(polarity_pos, "name"))),
            unique(gsub(" scan", "", xml_attr(polarity_neg, "name")))
          )
        } else {
          polarities <- NA_character_
        }

        rt_x <- '//d1:spectrum/d1:scanList/d1:scan/d1:cvParam[@name="scan start time"]'
        rt <- xml_find_all(xml_data, xpath = rt_x)
        unit <- unique(xml_attr(rt, "unitName"))
        rt <- as.numeric(xml_attr(rt, "value"))
        if ("minute" %in% unit) rt = rt * 60

        if (length(rt) > 0) {
          rt_start <- min(rt)
          rt_end <- max(rt)
        } else {
          rt_start <- NA_real_
          rt_end <- NA_real_
        }

        chrom_x <- '//d1:chromatogram'
        chrom_n <- xml_find_all(xml_data, chrom_x)
        chromatograms_number <- length(chrom_n)

        tic <- data.frame("rt" = numeric(), "intensity" = numeric())
        bpc <- data.frame("rt" = numeric(), "mz" = numeric(), "intensity" = numeric())

        if (spectra_number == 0 & chromatograms_number > 0) {

          if (length(chrom_n) == 1) chrom_n <- list(chrom_n)
          id <- unlist(lapply(chrom_n, function(x) xml_attr(x, "id")))
          index <- unlist(lapply(chrom_n, function(x) xml_attr(x, "index")))
          index <- as.numeric(index)
          if (0 %in% index) index = index + 1
          index <- index[id %in% c("TIC", "BPC")]

          if (TRUE %in% grepl("SRM", id)) data_type <- "SRM"

          if (length(index) > 0) {
            chrom_data <- lapply(chrom_n[index], function(x) {
              arrays <- 'd1:binaryDataArrayList/d1:binaryDataArray'
              arrays <- xml_find_all(x, xpath = arrays)
              arrays <- lapply(arrays, function(z) {
                precision_23 <- 'd1:cvParam[@accession="MS:1000523"]'
                precision_22 <- 'd1:cvParam[@accession="MS:1000522"]'
                precision <- xml_find_all(z, xpath = precision_23)
                if (length(precision) == 0) {
                  precision <- xml_find_all(z, xpath = precision_22)
                }
                precision <- xml_attr(precision, "name")
                precision <- sub(precision, pattern = "-bit float", replacement = "")
                precision <- sub(precision, pattern = "-bit integer", replacement = "")
                precision <- as.numeric(precision)/8
                comp <- 'd1:cvParam[@accession="MS:1000574"]'
                comp <- xml_find_all(z, xpath = comp)
                comp <- xml_attr(comp, "name")
                comp <- switch(comp,
                               `zlib` = "gzip",
                               `zlib compression` = "gzip",
                               `no compression` = "none",
                               `none` = "none"
                )
                val <- 'd1:binary'
                val <- xml_find_all(z, xpath = val)
                val <- xml_text(val)
                val <- base64enc::base64decode(val)
                val <- as.raw(val)
                val <- memDecompress(val, type = comp)
                val <- readBin(
                  val, what = "double",
                  n = length(val)/precision, size = precision
                )
                time_unit <- 'd1:cvParam[@name="time array"]'
                time_unit <- xml_find_all(z, xpath = time_unit)
                if (length(time_unit) > 0) {
                  unit <- xml_attr(time_unit, "unitName")
                  if ("minute" %in% unit) val <- val * 60
                }
                val
              })
              rt <- arrays[[1]]
              intensity <- arrays[[2]]
              out_df <- data.table("rt" = rt, "intensity" = intensity)
              out_df
            })
            id <- id[id %in% c("TIC", "BPC")]
            names(chrom_data) <- id
            if ("TIC" %in% id) tic <- chrom_data[["TIC"]]
            if ("BPC" %in% id) tic <- chrom_data[["BPC"]]
          }

        } else if (spectra_number > 0) {
          if (2 %in% ms_levels) {
            data_type <- "MS/MS"
          } else {
            data_type <- "MS"
          }

          ms1_nodes <- '//d1:spectrum[d1:cvParam[@name="ms level" and @value="1"]]'
          ms1_nodes <- xml_find_all(xml_data, ms1_nodes)
          if (length(ms1_nodes) > 0) {
            ms1_rt <- 'd1:scanList/d1:scan/d1:cvParam[@name="scan start time"]'
            ms1_rt <- xml_find_all(ms1_nodes, xpath = ms1_rt)
            unit <- unique(xml_attr(ms1_rt, "unitName"))
            ms1_rt <- as.numeric(xml_attr(ms1_rt, "value"))
            if ("minute" %in% unit) ms1_rt = ms1_rt * 60

            tic <- data.frame(
              "rt" = ms1_rt,
              "intensity" = as.numeric(xml_attr(xml_find_all(ms1_nodes,
                xpath = 'd1:cvParam[@name="total ion current"]'), "value")
              )
            )

            bpc <- data.frame(
              "rt" = ms1_rt,
              "mz" = as.numeric(xml_attr(xml_find_all(ms1_nodes,
                xpath = 'd1:cvParam[@name="base peak m/z"]'), "value")
              ),
              "intensity" = as.numeric(xml_attr(xml_find_all(ms1_nodes,
                xpath = 'd1:cvParam[@name="base peak intensity"]'), "value")
              )
            )
          }
        }

        analysis <- list(
          "name" = gsub(".mzML|.mzXML", "", basename(i)),
          "replicate" = NA_character_,
          "blank" = NA_character_,
          "file" = i,
          "type" = data_type,
          "instrument" = instrument,
          "time_stamp" = time_stamp,
          "spectra_number" = as.integer(spectra_number),
          "spectra_mode" = spectra_mode,
          "spectra_levels" = as.integer(ms_levels),
          "mz_low" = as.numeric(mz_low),
          "mz_high" = as.numeric(mz_high),
          "rt_start" = as.numeric(rt_start),
          "rt_end" = as.numeric(rt_end),
          "polarity" = polarities,
          "chromatograms_number" = as.integer(chromatograms_number),
          "ion_mobility" = FALSE,
          "tic" = tic,
          "bpc" = bpc,
          "spectra" = data.frame(),
          "chromatograms" = data.frame(),
          "features" = data.frame(),
          "metadata" = list()
        )

        ### for mzXML ----
      } else if (grepl("mzXML", i)) {

        data_type <- NA_character_
        tic <- data.frame("rt" = numeric(), "intensity" = numeric())
        bpc <- data.frame("rt" = numeric(), "mz" = numeric(), "intensity" = numeric())
        time_stamp <- NA_character_

        inst_x <- "//d1:msInstrument/child::node()[starts-with(name(), 'ms')]"
        inst_n <- xml_find_all(xml_data, xpath = inst_x)
        if (length(inst_n) > 0) {
          inst_names <- xml_attr(inst_n, "category")
          inst_vals <- xml_attr(inst_n, "value")
        } else {
          inst_names <- "instrument_data"
          inst_vals <- NA_character_
        }
        instrument <- data.frame(name = inst_names, value = inst_vals)
        instrument <- split(instrument, instrument$name)
        instrument <- lapply(instrument, function(x) x$value)

        scan_n <- xml_find_all(xml_data, xpath = "//d1:scan")
        spectra_number <- length(scan_n)

        if (spectra_number > 0) {

          centroided <- as.integer(unique(xml_attr(scan_n, "centroided")))
          if (1 %in% centroided) {
            spectra_mode <- "centroid"
          } else if (0 %in% centroided) {
            spectra_mode <- "profile"
          } else {
            spectra_mode <- NA_character_
          }

          ms_levels <- as.integer(sort(unique(xml_attr(scan_n, "msLevel"))))
          mz_low <- min(as.numeric(xml_attr(scan_n, "lowMz")))

          mz_high <- max(as.numeric(xml_attr(scan_n, "highMz")))

          rt <- xml_attr(scan_n, "retentionTime")
          unit <- unique(gsub(".*[0-9]", "", rt))
          rt <- gsub("[^0-9.-]", "", rt)
          rt <- as.numeric(rt)
          if (!"S" %in% unit) rt <- rt * 60
          rt_start <- min(rt)
          rt_end <- max(rt)

          polarities <- unique(xml_attr(scan_n, "polarity"))
          polarities[polarities %in% "+"] <- "positive"
          polarities[polarities %in% "-"] <- "negative"

          if (2 %in% ms_levels) {
            data_type <- "MS/MS"
          } else {
            data_type <- "MS"
          }

          ms1_nodes <- xml_find_all(xml_data, xpath = '//d1:scan[@msLevel="1"]')
          rt_chrom <- xml_attr(ms1_nodes, "retentionTime")
          rt_chrom <- as.numeric(gsub(pattern = "PT|S", replacement = "", rt_chrom))
          if (!"S" %in% unit) rt_chrom <- rt_chrom * 60

          tic <- data.frame(
            "rt" = rt_chrom,
            "intensity" = as.numeric(xml_attr(ms1_nodes, "totIonCurrent"))
          )

          bpc <- data.frame(
            "rt" = rt_chrom,
            "mz" = as.numeric(xml_attr(ms1_nodes, "basePeakMz")),
            "intensity" = as.numeric(xml_attr(ms1_nodes, "basePeakIntensity"))
          )

        } else {
          spectra_mode <- NA_character_
          ms_levels <- NA_integer_
          mz_low <- NA_real_
          mz_high <- NA_real_
          rt_start <- NA_real_
          rt_end <- NA_real_
          polarities <- NA_character_
        }

        analysis <- list(
          "name" = gsub(".mzML|.mzXML", "", basename(i)),
          "replicate" = NA_character_,
          "blank" = NA_character_,
          "file" = i,
          "type" = data_type,
          "instrument" = instrument,
          "time_stamp" = time_stamp,
          "spectra_number" = as.integer(spectra_number),
          "spectra_mode" = spectra_mode,
          "spectra_levels" = as.integer(ms_levels),
          "mz_low" = as.numeric(mz_low),
          "mz_high" = as.numeric(mz_high),
          "rt_start" = as.numeric(rt_start),
          "rt_end" = as.numeric(rt_end),
          "polarity" = polarities,
          "chromatograms_number" = 0,
          "ion_mobility" = FALSE,
          "tic" = tic,
          "bpc" = bpc,
          "spectra" = data.frame(),
          "chromatograms" = data.frame(),
          "features" = data.frame(),
          "metadata" = list()
        )
      } else {
        stop("File format must be either mzML or mzXML!")
      }
      analysis
    }
  } else if (!cached_analyses) {
    warning("Both mzR and xml2 R packages are not installed or available!")
    return(NULL)

  } else {
    message("\U2713 Analyses loaded from cache!")
  }

  if (!cached_analyses & !is.null(hash)) {
    if (!is.null(analyses)) {
      if (all(vapply(analyses, validate.msAnalysis, FALSE))) {
        patRoon:::saveCacheData("parsed_ms_analyses", analyses, hash)
      }
    }
  }

  if (runParallel) parallel::stopCluster(cl)

  # finishing ----

  if (!is.null(analyses)) {
    if (all(is.na(replicates))) {
      replicates <- vapply(analyses, function(x) x$name, "")
      replicates <- gsub("-", "_", replicates)
      replicates <- sub("_[^_]+$", "", replicates)
    }

    analyses <- Map(
      function(x, y) {
        x$replicate <- y
        x
      },
      analyses, replicates
    )

    if (!is.null(blanks) & length(blanks) == length(analyses)) {
      if (all(blanks %in% replicates)) {
        analyses <- Map(
          function(x, y) {
            x$blank <- y
            x
          },
          analyses, blanks
        )
      }
    }

    analyses <- lapply(analyses, function(x) {
      x$tic <- as.data.table(x$tic)
      x$bpc <- as.data.table(x$bpc)
      x$spectra <- as.data.table(x$spectra)
      x$chromatograms <- as.data.table(x$chromatograms)
      x$features <- as.data.table(x$features)
      x
    })

    names(analyses) <- vapply(analyses, function(x) x$name, "")
    analyses <- analyses[order(names(analyses))]
  }

  # end -----
  analyses <- lapply(analyses, function(x) as.msAnalysis(x))

  analyses
}

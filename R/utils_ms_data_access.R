

#' @title msAnalysis_loadMetadata
#'
#' @description Creates an \linkS4class{msAnalysis} for each mzML/mzXML file
#' in a given data.frame with columns "file", "replicate" and "blank".
#' The function uses code adapted from the \pkg{RaMS} R package to obtain
#' file metadata using the \pkg{xml2} R package in the back-end.
#'
#' @noRd
msAnalysis_loadMetadata <- function(file_df) {

  inval <- TRUE

  if (testClass(file_df, c("data.table"))) {
    n_files <- nrow(file_df)
    if (n_files > 0) inval <- FALSE
  }

  if (inval) return(new("msAnalysis"))

  file_df <- split(file_df, factor(file_df$file, levels = file_df$file))

  if (length(file_df) > 1) {

    handlers(handler_progress(format="[:bar] :percent :eta :message"))

    workers <- length(availableWorkers()) - 1

    plan("multisession", workers = workers)

    with_progress({

      p <- progressor(along = file_df)

      analyses <- future_lapply(file_df, function(x) {

        ana <- list()

        fl <- x$file

        if (grepl(".mzML", fl)) meta <- streamFind:::mzML_loadMetadata(fl)
        if (grepl(".mzXML", fl)) meta <- streamFind:::mzXML_loadMetadata(fl)

        ana$name <- gsub(".mzML|.mzXML", "", basename(fl))

        ana$file <- fl

        ana$metadata <- meta

        p()

        return(ana)

      }, future.chunk.size = 1)
    })

    plan("sequential")

  } else {

    analyses <- lapply(file_df, function(x) {

      ana <- list()

      fl <- x$file

      if (grepl(".mzML", fl)) meta <- mzML_loadMetadata(fl)
      if (grepl(".mzXML", fl)) meta <- mzXML_loadMetadata(fl)

      ana$name <- gsub(".mzML|.mzXML", "", basename(fl))
      ana$file <- fl

      ana$metadata <- meta

      return(ana)

    })

  }

  analyses <- lapply(analyses, function(x) {

    ana <- new("msAnalysis",
      name = x$name,
      file = x$file,
      metadata = x$metadata
    )

    return(ana)
  })

  names(analyses) <- sapply(analyses, FUN = function(x) analysisNames(x))

  analyses <- analyses[sort(names(analyses), decreasing = FALSE)]

  cat(" Done! \n")

  return(analyses)
}



#' @title msAnalysis_loadSpectraInfo
#'
#' @description Creates a \linkS4class{data.table} for a mzML/mzXML file
#' with the basic info for the spectra.
#'
#' @noRd
msAnalysis_loadSpectraInfo <- function(fl, rtr = NULL, levels = NULL) {

  if (grepl(".mzML", fl))
    spec_info <- mzML_loadSpectraInfo(fl, rtr = rtr, levels = levels)

  if (grepl(".mzXML", fl))
    spec_info <- mzXML_loadSpectraInfo(fl, rtr = rtr, levels = levels)

  return(spec_info)
}



#' @title msAnalysis_loadRawData
#'
#' @description Creates a \linkS4class{data.table} for a mzML/mzXML file
#' with the basic info for the spectra.
#'
#' @return A \linkS4class{data.table}.
#'
#' @noRd
msAnalysis_loadRawData <- function(fl,
                                   spectra = TRUE,
                                   levels = c(1, 2), rtr = NULL,
                                   minIntensityMS1 = 0,
                                   minIntensityMS2 = 0,
                                   chroms = TRUE) {

  if (grepl(".mzML", fl))
    dl_out <- mzML_loadRawData(fl,
      spectra = spectra, levels = levels, rtr = rtr,
      minIntensityMS1 = minIntensityMS1, minIntensityMS2 = minIntensityMS2,
      chroms = chroms)

  if (grepl(".mzXML", fl))
    dl_out <- mzXML_loadRawData(fl,
      levels = levels, rtr = rtr,
      minIntensityMS1 = minIntensityMS1, minIntensityMS2 = minIntensityMS2
    )

  return(dl_out)
}



### parse mzML - xml2 ----------------------------------------------------------

#' @title mzML_loadMetadata
#'
#' @description Code adapted from package \pkg{RaMS} developed by
#' William Kumler. See more details in:
#' \href{https://github.com/wkumler/RaMS}.
#'
#' @param fl A full path to an mzML file.
#'
#' @noRd
mzML_loadMetadata <- function(fl) {

  library(xml2)

  xml_data <- read_xml(fl)

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

  time_n <- xml_find_first(xml_data, xpath = "//d1:run")
  time_val <- xml_attr(time_n, "startTimeStamp")
  if (!is.na(time_val)) {
    time_stamp <- as.POSIXct(strptime(time_val, "%Y-%m-%dT%H:%M:%SZ"))
  } else {
    time_stamp <- as.POSIXct(NA)
  }

  ms_level_x <- '//d1:spectrum/d1:cvParam[@name="ms level"]'
  ms_level_n <- xml_find_all(xml_data, xpath = ms_level_x)
  number_spectra <- length(ms_level_n)
  if (length(ms_level_n) > 0) {
    ms_levels <- as.integer(unique(xml_attr(ms_level_n, "value")))
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
    spectrum_mode <- "centroid"
  } else {
    profile_x <- '//d1:spectrum/d1:cvParam[@accession="MS:1000128"]'
    profile_n <- xml_find_all(xml_data, xpath = profile_x)
    if (length(profile_n) > 0) {
      spectrum_mode <- "profile"
    } else {
      spectrum_mode <- NA_character_
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
  number_chromatograms <- length(chrom_n)

  meta1 <- list("inst_data" = inst_val)

  meta2 <- list(
    "time_stamp" = time_stamp,
    "number_spectra" = number_spectra,
    "spectrum_mode" = spectrum_mode,
    "ms_levels" = ms_levels,
    "mz_low" = mz_low,
    "mz_high" = mz_high,
    "rt_start" = rt_start,
    "rt_end" = rt_end,
    "polarity" = polarities,
    "number_chromatograms" = number_chromatograms
  )

  meta <- c(meta1, config, meta2)

  return(meta)
}



#' @title mzML_loadSpectraInfo
#'
#' @description Loads information for each spectrum in an mzML file.
#' Code adapted from package \pkg{RaMS} developed by
#' William Kumler. See more details in:
#' \href{https://github.com/wkumler/RaMS}.
#'
#' @param fl A full path to an mzML file.
#' @param rtr ...
#' @param levels ...
#'
#' @return A \linkS4class{data.table} with columns "index", "scan",
#' "level" (for MS levels) and "rt". When MS levels above 2 are present,
#' columns "ce" (collision energy), "preScan" and "preMZ" are added
#' to identify the precursor.
#'
#' @noRd
mzML_loadSpectraInfo <- function(xml_data, rtr = NULL, levels = NULL, preMZrange = NULL) {

  library(xml2)

  spectra_x <- '//d1:spectrum'
  spectra_n <- xml_find_all(xml_data, xpath = spectra_x)

  if (length(spectra_n) > 0) {

    level_x <- 'd1:cvParam[@name="ms level"]'
    level <- xml_find_all(spectra_n, xpath = level_x)
    level <- as.integer(xml_attr(level, "value"))

    if (!is.null(levels)) {
      check_level <- level %in% levels
      level <- level[check_level]
      spectra_n <- spectra_n[check_level]
    }

    rt_x <- 'd1:scanList/d1:scan/d1:cvParam[@name="scan start time"]'
    rt <- xml_find_all(spectra_n, xpath = rt_x)
    unit <- unique(xml_attr(rt, "unitName"))
    rt <- as.numeric(xml_attr(rt, "value"))

    if ("minute" %in% unit) rt = rt * 60

    if (!is.null(rtr) & length(rtr) == 2 & is.numeric(rtr)) {
      rtr <- sort(rtr)
      check_rt <- rt >= rtr[1] & rt <= rtr[2]
      rt <- rt[check_rt]
      level <- level[check_rt]
      spectra_n <- spectra_n[check_rt]
    }

    index <- as.numeric(xml_attr(spectra_n, "index"))
    scan <- as.numeric(gsub("\\D", "", xml_attr(spectra_n, "id")))

    level_unique <- sort(unique(level))

    if (TRUE %in% (level_unique >= 2)) {

      total_scans <- length(scan)
      ce <- rep(NA_real_, total_scans)
      preScan <- rep(NA_real_, total_scans)
      preMZ <- rep(NA_real_, total_scans)

      for (i in level_unique[!level_unique %in% 1]) {

        msn_x <- paste0('//d1:spectrum[d1:cvParam[@name="ms level" and @value="', i, '"]]')
        msn_n <- xml_find_all(xml_data, xpath = msn_x)

        msn_scan <- as.numeric(gsub("\\D", "", xml_attr(msn_n, "id")))

        preScan_x <- paste0('d1:precursorList/d1:precursor')
        preScan_v <- xml_find_all(msn_n, preScan_x)
        preScan_v <- xml_attr(preScan_v, "spectrumRef")
        preScan_v <- as.numeric(gsub("\\D", "", preScan_v))
        preScan[scan %in% msn_scan] <- preScan_v[msn_scan %in% scan]

        ce_x <- paste0('d1:precursorList/d1:precursor/d1:activation',
                           '/d1:cvParam[@name="collision energy"]')
        ce_v <- xml_find_all(msn_n, ce_x)
        ce_v <- as.integer(xml_attr(ce_v, "value"))
        ce[scan %in% msn_scan] <- ce_v[msn_scan %in% scan]

        preMZ_x <- paste0('d1:precursorList/d1:precursor/d1:selectedIonList',
                          '/d1:selectedIon/d1:cvParam[@name="selected ion m/z"]')
        preMZ_v <- xml_find_all(msn_n, preMZ_x)
        preMZ_v <- as.numeric(xml_attr(preMZ_v, "value"))
        preMZ[scan %in% msn_scan] <- preMZ_v[msn_scan %in% scan]
      }

      if (!is.null(preMZrange) & length(preMZrange) == 2 & is.numeric(preMZrange)) {
        preMZrange <- sort(preMZrange)
        check_preMZ <- preMZ >= preMZrange[1] & preMZ <= preMZrange[2]
        index <- index[check_preMZ]
        scan <- scan[check_preMZ]
        level <- level[check_preMZ]
        ce <- ce[check_preMZ]
        preScan <- preScan[check_preMZ]
        preMZ <- preMZ[check_preMZ]
        rt <- rt[check_preMZ]
      }

      df_a <- data.table(
        "index" = index,
        "scan" = scan,
        "level" = level,
        "ce" = ce,
        "preScan" = preScan,
        "preMZ" = preMZ,
        "rt" = rt
      )

    } else {

      df_a <- data.table(
        "index" = index,
        "scan" = scan,
        "level" = level,
        "rt" = rt
      )
    }

  } else {

    df_a <- data.table(
      "index" = numeric(),
      "scan" = numeric(),
      "level" = numeric(),
      "rt" = numeric()
    )
  }

  return(df_a)
}



#' @title mzML_loadRawData
#'
#' @description Loads spectra and chromatograms for an mzML file.
#' Code adapted from package \pkg{RaMS} developed by
#' William Kumler. See more details in: \href{https://github.com/wkumler/RaMS}.
#'
#' @param fl An mzML file.
#' @param spectra Logical, set to TRUE for parsing spectra.
#' @param levels A integer vector with the MS levels.
#' @param rtr A numeric vector of length two defining the time range.
#' @param minIntensityMS1 A numeric value with the minimum intensity
#' for MS1 data.
#' @param minIntensityMS2 A numeric value with the minimum intensity
#' for MS2 data.
#' @param chroms  Logical, set to TRUE for parsing chromatograms.
#'
#' @return A \linkS4class{data.table} with columns "index", "scan",
#' "level" (for MS levels) and "rt". When MS levels above 2 are present,
#' columns "ce" (collision energy), "preScan" and "preMZ" are added
#' to identify the precursor.
#'
#' @noRd
mzML_loadRawData <- function(fl, spectra = TRUE, TIC = TRUE, BPC = TRUE,
                             chroms = TRUE, levels = c(1, 2),
                             rtr = NULL, preMZrange = NULL,
                             minIntensityMS1 = 0, minIntensityMS2 = 0) {

  library(xml2)

  dl <- list()

  dl[["chroms"]] <- list()

  dl[["spectra"]] <- list()

  xml_data <- read_xml(fl)


  ### TIC and BPC ------
  if (TIC | BPC) {

    ms1_nodes <- '//d1:spectrum[d1:cvParam[@name="ms level" and @value="1"]]'
    ms1_nodes <- xml_find_all(xml_data, ms1_nodes)

    if (length(ms1_nodes) > 0) {

      ms1_rt <- 'd1:scanList/d1:scan/d1:cvParam[@name="scan start time"]'
      ms1_rt <- xml_find_all(ms1_nodes, xpath = ms1_rt)
      unit <- unique(xml_attr(ms1_rt, "unitName"))
      ms1_rt <- as.numeric(xml_attr(ms1_rt, "value"))
      if ("minute" %in% unit) ms1_rt = ms1_rt * 60

      if (TIC) {

        tic <- data.table(
          id = "TIC",
          rt = ms1_rt,
          intensity = as.numeric(xml_attr(xml_find_all(
            ms1_nodes,
            xpath = 'd1:cvParam[@name="total ion current"]'), "value"))
        )

        if (!is.null(rtr)) tic <- tic[rt >= rtr[1] & rt <= rtr[2], ]

        tic <- tic[intensity >= minIntensityMS1, ]

        dl[["chroms"]][["TIC"]] <- tic

      }

      if (BPC) {

        bpc <- data.table(
          id = "BPC",
          rt = ms1_rt,
          mz = as.numeric(xml_attr(xml_find_all(
            ms1_nodes,
            xpath = 'd1:cvParam[@name="base peak m/z"]'), "value")),
          intensity = as.numeric(xml_attr(xml_find_all(
            ms1_nodes,
            xpath = 'd1:cvParam[@name="base peak intensity"]'), "value"))
        )

        if (!is.null(rtr)) bpc <- bpc[rt >= rtr[1] & rt <= rtr[2], ]

        bpc <- bpc[intensity >= minIntensityMS1, ]

        dl[["chroms"]][["BPC"]] <- bpc

      }
    }
  }


  ### chroms ------
  if (chroms) {

    chroms_x <- '//d1:chromatogram'
    chroms_n <- xml_find_all(xml_data, xpath = chroms_x)

    if (length(chroms_n) == 1) chroms_n <- list(chroms_n)

    chroms_data <- lapply(chroms_n, function(x) {

      idx <- as.numeric(xml_attr(x, "index"))

      id <- xml_attr(x, "id")

      polarity_pos <- 'd1:cvParam[@accession="MS:1000130"]'
      polarity_pos <- xml_child(x, polarity_pos)

      polarity_neg <- 'd1:cvParam[@accession="MS:1000129"]'
      polarity_neg <- xml_child(x, polarity_neg)

      if (length(polarity_pos) > 0 | length(polarity_neg) > 0) {

        polarity <- c(
          unique(gsub(" scan", "", xml_attr(polarity_pos, "name"))),
          unique(gsub(" scan", "", xml_attr(polarity_neg, "name")))
        )

        polarity <- polarity[!is.na(polarity)]

      } else {
        polarity <- NA_character_
      }

      preMZ_xpath <- 'd1:precursor//d1:cvParam[@name="isolation window target m/z"]'
      preMZ <- as.numeric(xml_attr(xml_child(x, preMZ_xpath), "value"))

      product_mz_xpath <- 'd1:product//d1:cvParam[@name="isolation window target m/z"]'
      product_mz <- as.numeric(xml_attr(xml_child(x, product_mz_xpath), "value"))

      arrays <- 'd1:binaryDataArrayList/d1:binaryDataArray'
      arrays <- xml_find_all(x, xpath = arrays)

      arrays <- lapply(arrays[1:2], function(z) {

        precision <- 'd1:cvParam[@accession="MS:1000523"]'
        precision <- xml_find_all(z, xpath = precision)
        precision <- xml_attr(precision, "name")
        precision <- sub(precision, pattern = "-bit float", replacement = "")
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
        val <- readBin(val, what = "double",
          n = length(val)/precision, size = precision
        )

        time_unit <- 'd1:cvParam[@name="time array"]'
        time_unit <- xml_find_all(z, xpath = time_unit)
        if (length(time_unit) > 0) {
          unit <- xml_attr(time_unit, "unitName")
          if ("minute" %in% unit) val <- val * 60
        }

        return(val)
      })

      rt <- arrays[[1]]
      intensity <- arrays[[2]]

      out_df <- data.table(
        "index" = idx,
        "id" = id,
        "polarity" = polarity,
        "preMZ" = preMZ,
        "mz" = product_mz,
        "rt" = rt,
        "intensity" = intensity
      )

      return(out_df)
    })

    if (length(chroms_data) > 0) {

      chroms_data <- rbindlist(chroms_data, fill = TRUE)

      if ("TIC" %in% names(dl[["chroms"]])) {
        chroms_data <- chroms_data[!id %in% "TIC", ]
      }

      chroms_data <- chroms_data[intensity >= minIntensityMS1, ]
      dl[["chroms"]][["other_chroms"]] <- chroms_data

    }
  }


  if (TIC | BPC | chroms) {
    dl[["chroms"]] <-  rbindlist(dl[["chroms"]], fill = TRUE)
  }


  ### spectra ------
  if (spectra) {

    spectra_x <- '//d1:spectrum'
    spectra_n <- xml_find_all(xml_data, xpath = spectra_x)

    if (length(spectra_n) > 0) {

      df_a <- mzML_loadSpectraInfo(xml_data, rtr, levels, preMZrange)

      index <- as.numeric(xml_attr(spectra_n, "index"))
      index_check <- index %in% df_a$index
      index <- index[index_check]
      spectra_n <- spectra_n[index_check]

      comp_x <- paste0('//d1:cvParam[@accession="MS:1000574"]|',
                      '//d1:cvParam[@accession="MS:1000576"]')
      comp_n <- xml_find_first(spectra_n[[1]], comp_x)
      comp <- xml_attr(comp_n, "name")
      comp <- switch(comp,
        `zlib` = "gzip",
        `zlib compression` = "gzip",
        `no compression` = "none",
        `none` = "none"
      )

      mz_prs <- '//d1:cvParam[@accession="MS:1000523"]'
      mz_prs <- xml_find_first(spectra_n[[1]], xpath = mz_prs)
      mz_prs <- xml_attr(mz_prs, "name")
      mz_prs <- sub(mz_prs, pattern = "-bit float", replacement = "")
      mz_prs <- as.numeric(mz_prs)/8

      int_prs <- '//d1:cvParam[@accession="MS:1000521"]'
      int_prs <- xml_find_first(spectra_n[[1]], xpath = int_prs)
      int_prs <- xml_attr(int_prs, "name")
      int_prs <- sub(int_prs, pattern = "-bit float", replacement = "")
      int_prs <- as.numeric(int_prs)/8

      if (is.na(int_prs)) int_prs <- mz_prs
      if (is.na(mz_prs)) mz_prs <- int_prs

      mz_x <- 'd1:binaryDataArrayList/d1:binaryDataArray[1]/d1:binary'
      mz <- xml_text(xml_find_all(spectra_n, mz_x))
      mz <- lapply(mz, function(z, comp, mz_prs) {
        if (!nchar(z)) return(numeric(0))
        temp <- base64enc::base64decode(z)
        temp <- as.raw(temp)
        temp <- memDecompress(temp, type = comp)
        temp <- readBin(temp, what = "double",
                        n = length(temp)/mz_prs, size = mz_prs)
      }, comp = comp, mz_prs = mz_prs)

      intensity_x <- 'd1:binaryDataArrayList/d1:binaryDataArray[2]/d1:binary'
      intensity <- xml_text(xml_find_all(spectra_n, intensity_x))
      intensity <- lapply(intensity, function(z, comp, int_prs) {
        if (!nchar(z)) return(numeric(0))
        temp <- base64enc::base64decode(z)
        temp <- as.raw(temp)
        temp <- memDecompress(temp, type = comp)
        temp <- readBin(temp, what = "double",
                        n = length(temp)/int_prs, size = int_prs)
      }, comp = comp, int_prs = int_prs)

      df_b <- data.table(
        "index" = rep(index, sapply(mz, length)),
        "mz" = as.numeric(unlist(mz)),
        "intensity" = as.numeric(unlist(intensity))
      )

      df_out <- right_join(df_a, df_b, by = "index")

      df_out <- df_out[!(intensity <= minIntensityMS1 & level == 1), ]
      df_out <- df_out[!(intensity <= minIntensityMS2 & level == 2), ]

      dl[["spectra"]] <- df_out
    }
  }


  return(dl)
}



### parse mzXML - xml2 ---------------------------------------------------------

#' @title mzXML_loadMetadata
#'
#' @description Code adapted from package \pkg{RaMS} developed by
#' William Kumler. See more details in:
#' \href{https://github.com/wkumler/RaMS}.
#'
#' @param fl A full path to an mzXML file.
#'
#' @noRd
mzXML_loadMetadata <- function(fl){

  library(xml2)

  xml_data <- read_xml(fl)

  inst_x <- "//d1:msInstrument/child::node()[starts-with(name(), 'ms')]"
  inst_n <- xml_find_all(xml_data, xpath = inst_x)
  if (length(inst_n) > 0) {
    inst_names <- xml_attr(inst_n, "category")
    inst_vals <- xml_attr(inst_n, "value")
  } else {
    inst_names <- "instrument_data"
    inst_vals <- NA_character_
  }

  config <- data.frame(name = inst_names, value = inst_vals)
  config <- split(config, config$name)
  config <- lapply(config, function(x) x$value)

  scan_n <- xml_find_all(xml_data, xpath = "//d1:scan")

  number_spectra <- length(scan_n)

  if (length(scan_n) > 0) {

  centroided <- as.integer(unique(xml_attr(scan_n, "centroided")))
  if (1 %in% centroided) {
    spectrum_mode <- "centroid"
  } else if (0 %in% centroided) {
    spectrum_mode <- "profile"
  } else {
    spectrum_mode <- NA_character_
  }

  ms_levels <- as.integer(unique(xml_attr(scan_n, "msLevel")))
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

  } else {

    number_spectra <- NA_integer_
    spectrum_mode <- NA_character_
    ms_levels <- NA_integer_
    mz_low <- NA_real_
    mz_high <- NA_real_
    rt_start <- NA_real_
    rt_end <- NA_real_
    polarities <- NA_character_
  }

  meta <- list(
    "number_spectra" = number_spectra,
    "spectrum_mode" = spectrum_mode,
    "ms_levels" = ms_levels,
    "mz_low" = mz_low,
    "mz_high" = mz_high,
    "rt_start" = rt_start,
    "rt_end" = rt_end,
    "polarity" = polarities,
    "number_chromatograms" = 0
  )

  meta <- c(config, meta)

  return(meta)
}



#' @title mzXML_loadSpectraInfo
#'
#' @description Loads information for each spectrum in an mzXML file.
#' Code adapted from package \pkg{RaMS} developed by
#' William Kumler. See more details in:
#' \href{https://github.com/wkumler/RaMS}.
#'
#' @param fl A full path to an mzXML file.
#' @param rtr A numeric vector of length two with the time range
#' (i.e., min and max values).
#' @param levels A integer vector with the MS levels.
#'
#' @return A \linkS4class{data.table} with columns "index", "scan",
#' "level" (for MS levels) and "rt". When MS levels above 2 are present,
#' columns "ce" (collision energy), "preScan" and "preMZ" are added
#' to identify the precursor.
#'
#' @noRd
mzXML_loadSpectraInfo <- function(fl, rtr = NULL, levels = NULL, preMZrange = NULL) {

  library(xml2)

  xml_data <- read_xml(fl)

  # TODO xcms error when removing empty septra, due to different scan numbers
  #scan_n <- xml_find_all(xml_data, xpath = "//d1:scan[@peaksCount>0]")

  scan_n <- xml_find_all(xml_data, xpath = "//d1:scan")

  if (length(scan_n) > 0) {

    level <- as.integer(xml_attr(scan_n, "msLevel"))

    if (!is.null(levels)) {
      check_level <- level %in% levels
      level <- level[check_level]
      scan_n <- scan_n[check_level]
    }

    rt <- xml_attr(scan_n, "retentionTime")
    rt <- gsub("[^0-9.-]", "", rt)
    rt <- as.numeric(rt)

    if (max(rt) < 70) rt <- rt * 60

    if (!is.null(rtr) & length(rtr) == 2 & is.numeric(rtr)) {
      rtr <- sort(rtr)
      check_rt <- rt >= rtr[1] & rt <= rtr[2]
      rt <- rt[check_rt]
      level <- level[check_rt]
      scan_n <- scan_n[check_rt]
    }

    scan <- as.numeric(xml_attr(scan_n, "num"))

    level_unique <- sort(unique(level))

    if (TRUE %in% (level_unique >= 2)) {

      total_scans <- length(scan)
      ce <- rep(NA_real_, total_scans)
      preMZ <- rep(NA_real_, total_scans)


      for (i in level_unique[!level_unique %in% 1]) {

        #msn_x <- paste0('//d1:scan[@msLevel="', i, '" and @peaksCount>0]')
        msn_x <- paste0('//d1:scan[@msLevel="', i, '"]')
        msn_n <- xml_find_all(xml_data, xpath = msn_x)

        msn_scan <- as.numeric(xml_attr(msn_n, "num"))
        check_msn_scan <- msn_scan %in% scan
        msn_scan <- msn_scan[check_msn_scan]
        msn_n <- msn_n[check_msn_scan]
        msn_ce <- as.numeric(xml_attr(msn_n, "collisionEnergy"))
        ce[scan %in% msn_scan] <- msn_ce

        msn_pre_n <- xml_find_all(msn_n, xpath = "d1:precursorMz")
        msn_preMz <- as.numeric(xml_text(msn_pre_n))
        preMZ[scan %in% msn_scan] <- msn_preMz

      }

      if (!is.null(preMZrange) & length(preMZrange) == 2 & is.numeric(preMZrange)) {

        preMZrange <- sort(preMZrange)
        check_preMZ <- preMZ >= preMZrange[1] & preMZ <= preMZrange[2]

        scan <- scan[check_preMZ]
        level <- level[check_preMZ]
        ce <- ce[check_preMZ]
        preMZ <- preMZ[check_preMZ]
        rt <- rt[check_preMZ]

      }

      df_a <- data.table(
        "scan" = scan,
        "level" = level,
        "ce" = ce,
        "preMZ" = preMZ,
        "rt" = rt
      )

    } else {

      df_a <- data.table(
        "scan" = scan,
        "level" = level,
        "rt" = rt
      )
    }
  } else {
    df_a <- data.table(
      "index" = numeric(),
      "scan" = numeric(),
      "level" = numeric(),
      "rt" = numeric()
    )
  }

  return(df_a)
}



#' @title mzXML_loadRawData
#'
#' @description Loads spectra and chromatograms for an mzXML file.
#' Code adapted from package \pkg{RaMS} developed by
#' William Kumler. See more details in: \href{https://github.com/wkumler/RaMS}.
#'
#' @param fl A full path to an mzXML file.
#' @param levels A integer vector with the MS levels.
#' @param rtr A numeric vector of length two defining the time range.
#' @param minIntensityMS1 A numeric value with the minimum intensity
#' for MS1 data.
#' @param minIntensityMS2 A numeric value with the minimum intensity
#' for MS2 data.
#'
#' @return A list with a \linkS4class{data.table} named "spectra" with columns
#' "index", "scan", "level" (for MS levels), "rt", "mz" and "intensity".
#' When MS levels above 2 are present, columns "ce" (collision energy),
#' "preScan" and "preMZ" are added to identify the precursor.
#'
#' @noRd
mzXML_loadRawData <- function(fl, spectra = TRUE, TIC = TRUE, BPC = TRUE,
                              levels = c(1, 2), rtr = NULL, preMZrange = NULL,
                              minIntensityMS1 = 0, minIntensityMS2 = 0) {

  library(xml2)

  dl <- list()

  xml_data <- read_xml(fl)

  #scan_n <- xml_find_all(xml_data, xpath = "//d1:scan[@peaksCount>0]")

  scan_n <- xml_find_all(xml_data, xpath = "//d1:scan")

  if (length(scan_n) > 0) {

    if (!is.null(rtr)) rtr <- sort(rtr)

    if (TIC | BPC) {

      dl[["chroms"]] <- list()

      chrom_nodes <- xml_find_all(xml_data, xpath = '//d1:scan[@msLevel="1"]')
      rt_chrom <- xml_attr(chrom_nodes, "retentionTime")
      rt_chrom <- as.numeric(gsub(pattern = "PT|S", replacement = "", rt_chrom))

      if (TIC) {

        tic <- data.table(
          id = "TIC",
          rt = rt_chrom,
          intensity = as.numeric(xml_attr(chrom_nodes, "totIonCurrent"))
        )

        if (!is.null(rtr)) tic <- tic[rt >= rtr[1] & rt <= rtr[2], ]

        tic <- tic[intensity >= minIntensityMS1, ]

        dl[["chroms"]][["TIC"]] <- tic

      }

      if (BPC) {

        bpc <- data.table(
          id = "BPC",
          rt = rt_chrom,
          mz = as.numeric(xml_attr(chrom_nodes, "basePeakMz")),
          intensity = as.numeric(xml_attr(chrom_nodes, "basePeakIntensity"))
        )

        if (!is.null(rtr)) bpc <- bpc[rt >= rtr[1] & rt <= rtr[2], ]

        bpc <- bpc[intensity >= minIntensityMS1, ]

        dl[["chroms"]][["BPC"]] <- bpc

      }

      dl[["chroms"]] <-  rbindlist(dl[["chroms"]], fill = TRUE)

    }

    if (spectra) {

      df_a <- mzXML_loadSpectraInfo(fl, rtr, levels, preMZrange)
      scan <- as.numeric(xml_attr(scan_n, "num"))
      scan_check <- scan %in% df_a$scan
      scan <- scan[scan_check]
      scan_n <- scan_n[scan_check]

      enc_n <- xml_find_first(xml_data, '//d1:peaks')

      comp <- xml_attr(enc_n, "compressionType")
      comp <- switch(comp,
        `zlib` = "gzip",
        `zlib compression` = "gzip",
        `no compression` = "none",
        `none` = "none"
      )

      if (is.null(comp)) comp <- "none"

      prs <- xml_attr(enc_n, "precision")
      prs <- as.numeric(prs)/8

      byte_order <- xml_attr(enc_n, "byteOrder")
      endi_enc <- switch(byte_order, `network` = "big")

      peak_n <- xml_text(xml_find_all(scan_n, xpath = "d1:peaks"))
      vals <- lapply(peak_n, function(z, comp, prs, endi_enc = endi_enc){
        if (!nchar(z)) return(matrix(ncol = 2, nrow = 0))
        temp <- base64enc::base64decode(z)
        temp <- as.raw(temp)
        temp <- memDecompress(temp, type = comp)
        temp <- readBin(temp, what = "numeric",
          n = length(temp)/prs, size = prs, endian = endi_enc
        )
        temp <- matrix(temp, ncol = 2, byrow = TRUE)
        return(temp)
      }, comp = comp, prs = prs, endi_enc = endi_enc)

      # df_b <- mapply(cbind, scan, vals, SIMPLIFY = FALSE)
      # df_b <- lapply(df_b, as.data.frame)
      # df_b <- rbindlist(df_b)
      # colnames(df_b) <- c("scan", "mz", "intensity")

      names(vals) <- scan
      df_b <- lapply(vals, as.data.frame)
      df_b <- rbindlist(df_b, idcol = "scan")
      colnames(df_b) <- c("scan", "mz", "intensity")
      df_b$scan <- as.numeric(df_b$scan)

      df_out <- right_join(df_a, df_b, by = "scan")

      df_out <- df_out[!(intensity <= minIntensityMS1 & level == 1), ]
      df_out <- df_out[!(intensity <= minIntensityMS2 & level == 2), ]

      dl[["spectra"]] <- df_out
    }

  } else if (spectra) {

    dl[["spectra"]] <- data.table(
      "scan" = numeric(),
      "level" = numeric(),
      "rt" = numeric(),
      "mz" = numeric(),
      "intensity" = numeric()
    )

  }

  return(dl)
}



### parse with mzR ------------------------------------------------------------

#' @title loadMetadataMZR
#'
#' @description Parsing metadata from MS files using the package \pkg{mzR}.
#'
#' @param fl A full path to an mzML file.
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
#' @noRd
loadMetadataMZR <- function(fl) {

  if(FALSE %in% requireNamespace("mzR", quietly = TRUE)) {
    warning("Package mzR not detected.")
    return(list())
  }

  zF <- mzR::openMSfile(fl, backend = "pwiz")

  meta1 <- mzR::instrumentInfo(zF)

  zH <- mzR::header(zF)

  run_info <- suppressWarnings(mzR::runInfo(zF))

  if (is.infinite(run_info$lowMz)) run_info$lowMz <- NA_real_
  if (is.infinite(run_info$highMz)) run_info$highMz <- NA_real_
  if (is.infinite(run_info$dStartTime)) run_info$dStartTime <- NA_real_
  if (is.infinite(run_info$dEndTime)) run_info$dEndTime <- NA_real_

  if (1 %in% zH$polarity) {
    polarity_pos <- "positive"
  } else polarity_pos <- NULL

  if (0 %in% zH$polarity) {
    polarity_neg <- "negative"
  } else polarity_neg <- NULL


  if (length(polarity_pos) > 0 | length(polarity_neg) > 0) {
    polarities <- c(polarity_pos, polarity_neg)
  } else {
    polarities <- NA_character_
  }

  if (TRUE %in% zH$centroided) {
    spectrum_mode <- "centroid"
  } else if (FALSE %in% zH$centroided) {
    spectrum_mode <- "profile"
  } else {
    spectrum_mode <- NA_character_
  }

  if (!all(is.na(zH$ionMobilityDriftTime))) {
    ion_mobility <- TRUE
  } else {
    ion_mobility <- FALSE
  }

  if (grepl(".mzML", fl)) {
    number_chromatograms <- mzR::nChrom(zF)
  } else {
    number_chromatograms <- 0
  }

  meta2 <- list(
    "time_stamp" = run_info$startTimeStamp,
    "number_spectra" = run_info$scanCount,
    "spectrum_mode" = spectrum_mode,
    "ms_levels" = run_info$msLevels,
    "mz_low" = run_info$lowMz,
    "mz_high" = run_info$highMz,
    "rt_start" = run_info$dStartTime,
    "rt_end" = run_info$dEndTime,
    "polarity" = polarities,
    "number_chromatograms" = number_chromatograms,
    "ion_mobility" = ion_mobility
  )

  meta <- c(meta1, meta2)

  suppressWarnings(mzR::close(zF))

  return(meta)
}



#' @title loadRawDataMZR
#'
#' @description Parsing raw data from MS files using the package \pkg{mzR}.
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
#' @noRd
loadRawDataMZR <- function(fl, spectra = TRUE, TIC = TRUE, BPC = TRUE,
                           chroms = TRUE, levels = c(1, 2),
                           rtr = NULL, preMZrange = NULL,
                           minIntensityMS1 = 0, minIntensityMS2 = 0) {

  if(FALSE %in% requireNamespace("mzR", quietly = TRUE)) {
    warning("Package mzR not detected.")
    return(list())
  }

  dl <- list()

  dl[["chroms"]] <- list()

  dl[["spectra"]] <- list()

  zF <- mzR::openMSfile(fl, backend = "pwiz")

  if (spectra | TIC | BPC) zH <- mzR::header(zF)

  if (nrow(zH) > 0) {

    if (TIC) {

      tic <- data.table::data.table(
        id = "TIC",
        rt = zH$retentionTime,
        intensity = zH$totIonCurrent
      )

      if (!is.null(rtr)) tic <- tic[rt >= rtr[1] & rt <= rtr[2], ]

      tic <- tic[intensity >= minIntensityMS1, ]

      dl[["chroms"]][["TIC"]] <- tic

    }

    if (BPC) {

      bpc <- data.table::data.table(
        id = "BPC",
        rt = zH$retentionTime,
        mz = zH$basePeakMZ,
        intensity = zH$basePeakIntensity
      )

      if (!is.null(rtr)) bpc <- bpc[rt >= rtr[1] & rt <= rtr[2], ]

      bpc <- bpc[intensity >= minIntensityMS1, ]

      dl[["chroms"]][["BPC"]] <- bpc

    }
  }

  if (chroms) {

    cH <- data.table::as.data.table(suppressWarnings(mzR::chromatogramHeader(zF)))

    if (nrow(cH) > 0) {

      cH$polarity <- as.character(cH$polarity)
      cH <- cH[polarity == 1, polarity := "positive"]
      cH <- cH[polarity == 0, polarity := "negative"]
      cH <- cH[polarity == -1, polarity := NA_character_]

      cC <- mzR::chromatograms(zF, cH$chromatogramIndex)

      if (!is.data.frame(cC)) {

        names(cC) <- as.character(cH$chromatogramIndex)
        cC <- lapply(cC, function(x) {
          x <- as.data.frame(x)
          colnames(x) <- c("rt", "intensity")
          return(x)
        })
        cC <- data.table::rbindlist(cC, idcol = "index", fill = TRUE)
        cC$index <- as.numeric(cC$index)
        cH_b <- data.table(
          index = cH$chromatogramIndex,
          id = cH$chromatogramId,
          polarity = cH$polarity,
          preMZ = cH$precursorIsolationWindowTargetMZ,
          mz = cH$productIsolationWindowTargetMZ
        )

        chroms_data <- dplyr::inner_join(cH_b, cC, by = "index")

      } else {

        chroms_data <- data.table(
          index = cH$chromatogramIndex,
          id = cH$chromatogramId,
          polarity = cH$polarity,
          preMZ = cH$precursorIsolationWindowTargetMZ,
          mz = cH$productIsolationWindowTargetMZ,
          rt = cC$rt,
          intensity = cC$intensity
        )

      }

      chroms_data <- chroms_data[intensity > minIntensityMS1, ]

      if (nrow(chroms_data) > 0) {

        if ("TIC" %in% names(dl[["chroms"]])) {
          chroms_data <- chroms_data[!id %in% "TIC", ]
        }

        dl[["chroms"]][["other_chroms"]] <- chroms_data
      }
    }
  }

  if (TIC | BPC | chroms) {
    dl[["chroms"]] <-  rbindlist(dl[["chroms"]], fill = TRUE)
  }

  if (spectra) {

    if (nrow(zH) > 0) {

      if (!is.null(rtr) & length(rtr) == 2) {
        rtr <- sort(rtr)
        zH <- zH[zH$retentionTime >= rtr[1] & zH$retentionTime <= rtr[2], ]
      }

      if (!is.null(levels)) zH <- zH[zH$msLevel %in% levels, ]

      if(!is.null(preMZrange) & length(preMZrange) == 2 & is.numeric(preMZrange)) {
        preMZrange <- sort(preMZrange)
        zH <- zH[zH$precursorMZ >= preMZrange[1] & zH$precursorMZ <= preMZrange[2], ]
      }

      if (nrow(zH) > 0) {

        zD <- mzR::peaks(zF, scans = zH$seqNum)
        zD <- lapply(zD, as.data.table)
        names(zD) <- zH$seqNum
        zD <- data.table::rbindlist(zD, idcol = "index")
        zD$index <- as.numeric(zD$index)

        if (TRUE %in% (unique(zH$msLevel) == 2)) {

          zH_b <- data.table(
            index = zH$seqNum,
            scan = zH$acquisitionNum,
            level = zH$msLevel,
            ce = zH$collisionEnergy,
            preScan = zH$precursorScanNum,
            preMZ = zH$precursorMZ,
            rt = zH$retentionTime
          )

        } else {

          zH_b <- data.table(
            index = zH$seqNum,
            scan = zH$acquisitionNum,
            level = zH$msLevel,
            rt = zH$retentionTime
          )

        }

        if (!all(is.na(zH$ionMobilityDriftTime))) {
          zH_b$drifTime <- zH$ionMobilityDriftTime
        }

        zH_n <- dplyr::inner_join(zH_b, zD, by = "index")

        zH_n <- zH_n[!(intensity <= minIntensityMS1 & level == 1), ]
        zH_n <- zH_n[!(intensity <= minIntensityMS2 & level == 2), ]

        dl[["spectra"]] <- zH_n

      }
    }
  }

  suppressWarnings(mzR::close(zF))

  return(dl)
}

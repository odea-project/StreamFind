

### //// validity -------------------------------------------------------------

msAnalysis_validity <- function(object) {

  valid <- TRUE

  if (nrow(object@peaks) > 0) {
    must_have_names <- c("id", "index", "rt", "mz", "intensity", "area",
                         "drt", "rtmin", "rtmax", "dppm", "mzmin", "mzmax",
                         "adduct", "mass", "filtered", "filter")
    valid <- !FALSE %in% (must_have_names %in% colnames(object@peaks))
  }

  return(valid)
}



### //// msAnalysis -----------------------------------------------------------

#' msAnalysis-class
#'
#' @description An S4 class representing an MS analysis/file within the
#' \pkg{streamFind} package. The \code{msAnalysis} is used to store and
#' manage MS data and the respective methods can be used for inspection,
#' processing and evaluation of the data.
#'
#' @slot name A character string with the MS file name without the extension.
#' @slot file A character string with the complete path of the MS file.
#' @slot metadata A list with information queried from the MS file and other
#' analysis metadata.
#' @slot spectra A \linkS4class{data.table} with spectra parsed from
#' the MS file.
#' @slot chromatograms A \linkS4class{data.table} with chromatograms parsed
#' from the MS file.
#' @slot settings A list with \linkS4class{settings} objects used for
#' data processing, such as peak picking.
#' @slot peaks A \linkS4class{data.table} with peak details,
#' containing the following mandatory columns:
#' \enumerate{
#'  \item \strong{id}: character, the identifier of the peak;
#'  \item \strong{index}: numeric, the index of the peak;
#'  \item \strong{rt}: numeric, the retention time (in seconds);
#'  \item \strong{mz}: numeric, the calculated \emph{m/z} value;
#'  \item \strong{intensity}: numeric, the intensity or height (in counts);
#'  \item \strong{area}: numeric, the area of the integrated peak;
#'  \item \strong{drt}: numeric, the peak width (in seconds);
#'  \item \strong{rtmin}: numeric, the minimum retention time (in seconds);
#'  \item \strong{rtmax}: numeric, the maximum retention time, (in seconds);
#'  \item \strong{dppm}: numeric, the mass width of the peak (in ppm);
#'  \item \strong{mzmin}: numeric, the minimum \emph{m/z} value;
#'  \item \strong{mzmax}: numeric, the maximum \emph{m/z} value;
#'  \item \strong{adduct}: character, the annotated adduct ion of the peak;
#'  \item \strong{mass}: numeric, the neutral mass of the peak;
#'  \item \strong{isotope}: numeric, the annotated isotopic number;
#'  \item \strong{monoIsotope}: character, the id of the mono isotopic ion;
#'  \item \strong{feature}: character, the identifier of the features (i.e.,
#'  group of peaks) after grouping of corresponding peaks across analyses;
#'  \item \strong{...} other optional columns added from various
#'  functions/algorithms.
#' }
#'
#' @export
#'
#' @md
setClass("msAnalysis",
  slots = c(
    name = "character",
    file = "character",
    metadata = "list",
    spectra = "data.table",
    chromatograms = "data.table",
    settings = "list",
    peaks = "data.table"
  ),
  prototype = list(
    name = NA_character_,
    file = NA_character_,
    metadata = list(),
    spectra = data.table(),
    chromatograms = data.table(),
    settings = list(),
    peaks = data.table()
  ),
  validity = msAnalysis_validity
)



### //// S4 methods -----------------------------------------------------------

#### _initialize_ -------------------------------------------------------------

#' @describeIn msAnalysis creates an \linkS4class{msAnalysis} from a file.
#'
#' @export
#'
setMethod("initialize", "msAnalysis", function(.Object, file = NA_character_) {

  .Object <- callNextMethod()

  inval <- FALSE

  if (!file.exists(file)) inval <- TRUE

  ms_file_formats <- ".mzML|.mzXML"
  check_file_format <- grepl(ms_file_formats, file)

  if (!check_file_format) inval <- TRUE

  if (inval) return(.Object)

  if (TRUE %in% requireNamespace("mzR", quietly = TRUE)) {

    #meta <- loadMetadataMZR(file)

    zF <- mzR::openMSfile(file, backend = "pwiz")

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

    if (grepl(".mzML", file)) {
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

  } else {

    if (grepl(".mzML", file)) meta <- mzML_loadMetadata(file)
    if (grepl(".mzXML", file)) meta <- mzXML_loadMetadata(file)

  }

  analysis_name <- gsub(".mzML|.mzXML", "", basename(file))

  analysis <- .Object

  analysis@name <- analysis_name
  analysis@file <- file
  analysis@metadata <- meta

  return(analysis)
})



#### _show_ -------------------------------------------------------------------

#' @describeIn msAnalysis shows the details of an \linkS4class{msAnalysis}.
#'
#' @param object An \linkS4class{msAnalysis} object.
#'
#' @export
#'
setMethod("show", "msAnalysis", function(object) {

  cat(
    "  Class          ", is(object), "\n",
    "  Name           ", object@name, "\n",
    "  Polarity       ", paste(object@metadata$polarity, collapse = ", "), "\n",
    "  File           ", object@file, "\n",
    "  Levels         ", paste(object@metadata$ms_levels, collapse = ", "), " \n",
    "  Spectrum mode  ", object@metadata$spectrum_mode, "\n",
    ifelse(TRUE %in% object@metadata[["ion_mobility"]],
    "  Ion mobility   TRUE\n", ""),
    "  Spectra        ", object@metadata$number_spectra, "\n",
    "  Chromatograms  ", object@metadata$number_chromatograms, "\n",
    "  Loaded traces  ", nrow(object@spectra), "\n",
    "  Loaded chroms  ", length(unique(object@chromatograms$id)), "\n",
    "  Picked peaks   ", nrow(object@peaks), "\n",
    "  Settings: \n",
    sep = ""
  )
  if (length(object@settings) > 0) {
    for (i in seq_len(length(object@settings))) {
      cat(
        "     ",
        names(object@settings)[i], ": ",
        object@settings[[i]]@algorithm,  "\n", sep = ""
      )
    }
  } else {
    cat("     n.a.", "\n", sep = "")
  }
})



#### name and path ______ -----------------------------------------------------

##### analysisName ------------------------------------------------------------

#' @describeIn msAnalysis getter for analysis name.
#'
#' @export
#'
#' @aliases analysisName,msAnalysis,msAnalysis-method
#'
setMethod("analysisName", "msAnalysis", function(object) {
  return(object@name)
})



##### analysisNames -----------------------------------------------------------

#' @describeIn msAnalysis getter for analysis name.
#'
#' @export
#'
#' @aliases analysisNames,msAnalysis,msAnalysis-method
#'
setMethod("analysisNames", "msAnalysis", function(object) {
  return(analysisName(object))
})



##### filePath ----------------------------------------------------------------

#' @describeIn msAnalysis getter for analysis file path.
#'
#' @export
#'
#' @aliases filePath,msAnalysis,msAnalysis-method
#'
setMethod("filePath", "msAnalysis", function(object) {
  return(object@file)
})



##### filePaths ---------------------------------------------------------------

#' @describeIn msAnalysis getter for analysis file path.
#'
#' @export
#'
#' @aliases filePaths,msAnalysis,msAnalysis-method
#'
setMethod("filePaths", "msAnalysis", function(object) {
  return(filePath(object))
})



##### analysisInfo ------------------------------------------------------------

#' @describeIn msAnalysis getter for analysis info as \link{data.frame} with
#' four columns: path, analysis, group and blank. The \link{data.frame}
#' can be used as analysisInfo in \pkg{patRoon}.
#'
#' @param obj A \linkS4class{msAnalysis} object.
#'
#' @export
#'
#' @aliases analysisInfo,msAnalysis,msAnalysis-method
#'
setMethod("analysisInfo", "msAnalysis", function(obj) {
  return(data.frame(
    "path" = dirname(obj@file),
    "analysis" = obj@name,
    "group" = obj@name,
    "blank" = "",
    "class" = is(obj)
  ))
})



##### analysisTable -----------------------------------------------------------

#' @describeIn msAnalysis getter for a \linkS4class{data.table} with
#' four columns, containing the analysis file, name, replicate, blank and class.
#'
#' @export
#'
#' @aliases analysisTable,msAnalysis,msAnalysis-method
#'
setMethod("analysisTable", "msAnalysis", function(object) {
  return(data.table(
    "file" = object@file,
    "analysis" = object@name,
    "replicate" = object@name,
    "blank" = NA_character_,
    "class" = is(object)
  ))
})



#### metadata ____________ ----------------------------------------------------

##### getMetadataNames --------------------------------------------------------

#' @describeIn msAnalysis getter for the names of metadata in the analysis.
#'
#' @export
#'
#' @aliases getMetadataNames,msAnalysis,msAnalysis-method
#'
setMethod("getMetadataNames", "msAnalysis", function(object) {
  return(names(object@metadata))
})



##### getMetadata -------------------------------------------------------------

#' @describeIn msAnalysis getter for metadata entries in the analysis.
#' Returns a list of metadata entries as defined by \code{which}. The argument
#' which is a string or character vector defining the name/s of the desired
#' metadata. When \code{which} is \code{NULL}, all entries are returned as list.
#'
#' @param which A character vector with the entry name/s.
#'
#' @export
#'
#' @aliases getMetadata,msAnalysis,msAnalysis-method
#'
setMethod("getMetadata", "msAnalysis", function(object, which = NULL) {

  if (!is.null(which)) {
    mtd_a <- object@metadata[which]
  } else {
    mtd_a <- object@metadata
  }

  return(mtd_a)
})



##### addMetadata -------------------------------------------------------------

#' @describeIn msAnalysis setter for analysis metadata. When overwrite is
#' set to \code{TRUE}, metadata entries with the same name are overwritten.
#' Note, metadata entries parsed from the MS file cannot be overwritten.
#'
#' @param metadata A named vector with metadata entries or a one row
#' \linkS4class{data.frame} or \linkS4class{data.table} with metadata
#' added as columns.
#' @param overwrite Logical, set to \code{TRUE} to overwrite.
#'
#' @export
#'
#' @aliases addMetadata,msAnalysis,msAnalysis-method
#'
setMethod("addMetadata", "msAnalysis", function(object,
                                                metadata = NULL,
                                                overwrite = FALSE) {

  const_names <- c(
    "inst_data", "analyzer", "detector", "source", "time_stamp",
    "msDetector", "msIonisation", "msManufacturer", "msMassAnalyzer", "msModel",
    "number_spectra", "spectrum_mode", "ms_levels", "mz_low", "mz_high",
    "rt_start", "rt_end", "polarity", "number_chromatograms")

  if (is.data.frame(metadata)) {

    name_is_already_there <- colnames(metadata) %in% getMetadataNames(object)
    name_in_const_names <- colnames(metadata) %in% const_names
    metadata <- metadata[1, ]

  } else if (is.vector(metadata)) {

    if (is.null(names(metadata))) {
      warning("Metadata must be a named vector!")
      return(object)
    }

    name_is_already_there <- names(metadata) %in% getMetadataNames(object)
    name_in_const_names <- names(metadata) %in% const_names

  }


  if (exists("name_is_already_there")) {

    if (TRUE %in% name_is_already_there & !overwrite) {
      warning("Metadata name/s already exist/s!")
      return(object)
    }

    if (TRUE %in% name_in_const_names) {
      warning("Metadata entries from raw MS file cannot be overwritten!")
      return(object)
    }

    if (TRUE %in% name_is_already_there) {

      metadata <- as.list(metadata)

      object@metadata[names(object@metadata) %in%
                        names(metadata)] <- metadata[name_is_already_there]

      object@metadata <- c(object@metadata, metadata[!name_is_already_there])

      return(object)

    } else {

      metadata <- as.list(metadata)
      object@metadata <- c(object@metadata, metadata)
      return(object)

    }
  }

  return(object)
})



##### polarity ----------------------------------------------------------------

#' @describeIn msAnalysis getter for analysis polarity.
#'
#' @export
#'
#' @aliases polarity,msAnalysis,msAnalysis-method
#'
setMethod("polarity", "msAnalysis", function(object) {
  mt <- getMetadata(object, which = "polarity")
  return(unlist(mt))
})



##### polarities --------------------------------------------------------------

#' @describeIn msAnalysis getter for analysis polarity.
#'
#' @export
#'
#' @aliases polarities,msAnalysis,msAnalysis-method
#'
setMethod("polarities", "msAnalysis", function(object) {
  return(polarity(object))
})



#### raw data _____________ ---------------------------------------------------

##### loadSpectraInfo ---------------------------------------------------------

#' @describeIn msAnalysis adds basic raw spectra information (i.e., scan number,
#'  ms level and retention time of each spectrum in the MS analysis) to
#'  the slot \code{spectra}. If the levels are higher than one, as the case
#'  of MS/MS data, the collision energy and both precursor \emph{m/z} are
#'  also returned.
#'
#' @export
#'
#' @aliases loadSpectraInfo,msAnalysis,msAnalysis-method
#'
setMethod("loadSpectraInfo", "msAnalysis", function(object) {

  file <- filePath(object)

  xml_data <- xml2::read_xml(file)

  if (grepl(".mzML", file))
    spec_info <- mzML_loadSpectraInfo(xml_data)

  if (grepl(".mzXML", file))
    spec_info <- mzXML_loadSpectraInfo(xml_data)

  object@spectra <- spec_info

  return(object)
})



##### getRawData --------------------------------------------------------------

#' @describeIn msAnalysis gets (when available) raw spectra and chromatograms
#' from the raw file of an \linkS4class{msAnalysis} object.
#'
#' @param spectra Logical, set to \code{TRUE} for parsing spectra.
#' @param TIC Logical, set to \code{TRUE} for parsing TIC from xml headings.
#' @param BPC Logical, set to \code{TRUE} for parsing BPC from xml headings.
#' @param chroms Logical, set to \code{TRUE} for parsing chromatograms.
#' @param levels A numeric vector with the MS levels for parsing spectra.
#' @param rtr A numeric vector of length two with the time range (in seconds)
#' for parsing spectra, TIC and/or BPC.
#' @param preMZrange A numeric vector of length two with the \code{m/z} range
#' of precursor ions for collecting spectra with level higher than 1.
#' @param minIntensityMS1 Numeric value on length one with the
#' minimum intensity of MS1 level traces.
#' @param minIntensityMS2 Numeric value on length one with the
#' minimum intensity of MS2 level traces.
#'
#' @export
#'
#' @aliases getRawData,msAnalysis,msAnalysis-method
#'
setMethod("getRawData", "msAnalysis", function(object,
                                               spectra = TRUE,
                                               TIC = TRUE,
                                               BPC = TRUE,
                                               chroms = TRUE,
                                               levels = NULL,
                                               rtr = NULL,
                                               preMZrange = NULL,
                                               minIntensityMS1 = 0,
                                               minIntensityMS2 = 0) {
  inval <- FALSE

  list_out <- list()

  file <- filePath(object)

  if (!file.exists(file)) {
    warning(paste0("File ", file ," not found!"))
    inval <- TRUE
  }

  # TODO add more validation tests for arguments

  if (inval) {
    list_out[["chroms"]] <- data.table()
    list_out[["spectra"]] <- data.table()
    return(list_out)
  }

  if (is.null(levels)) levels <- getMetadata(object, "ms_levels")$ms_levels

  if (TRUE %in% requireNamespace("mzR", quietly = TRUE)) {

    list_out[["chroms"]] <- list()

    list_out[["spectra"]] <- list()

    zF <- mzR::openMSfile(file, backend = "pwiz")

    if (spectra | TIC | BPC) {

      zH <- mzR::header(zF)

      if (nrow(zH) > 0) {

        if (max(zH$retentionTime) < 60) zH$retentionTime <- zH$retentionTime * 60

        if (!is.null(levels)) zH <- zH[zH$msLevel %in% levels, ]

        if (!is.null(rtr)) {
          zH <- zH[streamFind::checkOverlapRanges(zH$retentionTime, rtr), ]
        }

        if(!is.null(preMZrange)) {
          zH <- zH[streamFind::checkOverlapRanges(zH$precursorMZ, preMZrange), ]
        }

        zH_ms1 <- zH[zH$msLevel == 1, ]

        if (TIC) {

          tic <- data.table(
            "id" = "TIC",
            "rt" = zH_ms1$retentionTime,
            "intensity" = zH_ms1$totIonCurrent
          )

          tic <- tic[tic$intensity >= minIntensityMS1, ]

          if (max(tic$intensity) > 0 & !(TRUE %in% duplicated(tic$rt))) {
            list_out[["chroms"]][["TIC"]] <- tic
          }
        }

        if (BPC) {

          bpc <- data.table(
            "id" = "BPC",
            "rt" = zH_ms1$retentionTime,
            "mz" = zH_ms1$basePeakMZ,
            "intensity" = zH_ms1$basePeakIntensity
          )

          bpc <- bpc[bpc$intensity >= minIntensityMS1, ]

          if (max(bpc$intensity) > 0 & !(TRUE %in% duplicated(bpc$rt))) {
            list_out[["chroms"]][["BPC"]] <- bpc
          }

        }
      }
    }

    if (chroms) {

      cH <- as.data.table(suppressWarnings(mzR::chromatogramHeader(zF)))

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

        chroms_data <- chroms_data[chroms_data$intensity > minIntensityMS1, ]

        if (nrow(chroms_data) > 0) {

          if ("TIC" %in% names(list_out[["chroms"]])) {
            chroms_data <- chroms_data[!chroms_data$id %in% "TIC", ]
          }

          list_out[["chroms"]][["other_chroms"]] <- chroms_data
        }
      }
    }

    if (TIC | BPC | chroms) {
      list_out[["chroms"]] <-  rbindlist(list_out[["chroms"]], fill = TRUE)
    }

    if (spectra) {

      if (nrow(zH) > 0) {

        if (nrow(zH) > 0) {

          zD <- mzR::peaks(zF, scans = zH$seqNum)

          mat_idx <- rep(zH$seqNum, sapply(zD, nrow))
          zD <- as.data.table(do.call(rbind, zD))
          zD$index <- mat_idx

          if (TRUE %in% (unique(zH$msLevel) == 2)) {

            zH_b <- data.table(
              "index" = zH$seqNum,
              "scan" = zH$acquisitionNum,
              "level" = zH$msLevel,
              "ce" = zH$collisionEnergy,
              "preScan" = zH$precursorScanNum,
              "preMZ" = zH$precursorMZ,
              "rt" = zH$retentionTime
            )

          } else {

            zH_b <- data.table(
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

          zH_n <- zH_b[zD, on = "index"]

          zH_n <- zH_n[!(zH_n$intensity <= minIntensityMS1 & zH_n$level == 1), ]
          zH_n <- zH_n[!(zH_n$intensity <= minIntensityMS2 & zH_n$level == 2), ]

          list_out[["spectra"]] <- zH_n

        }
      }
    }

  } else {

    if (grepl(".mzML", file)) {
      list_out <- mzML_loadRawData(file,
        spectra = spectra, TIC = TIC, BPC = BPC, chroms = chroms,
        levels = levels, rtr = rtr, preMZrange = preMZrange,
        minIntensityMS1 = minIntensityMS1, minIntensityMS2 = minIntensityMS2)
    }

    if (grepl(".mzXML", file)) {
      list_out <- mzXML_loadRawData(file,
        spectra = spectra, TIC = TIC, BPC = TIC,
        levels = levels, rtr = rtr, preMZrange = preMZrange,
        minIntensityMS1 = minIntensityMS1, minIntensityMS2 = minIntensityMS2)
    }

  }

  if (length(list_out$spectra) == 0) {
    list_out$spectra <- data.table()
  }

  if (length(list_out$chroms) == 0) {
    list_out$chroms <- data.table()
  }

  if (exists("zF")) suppressWarnings(mzR::close(zF))

  return(list_out)
})



##### loadRawData -------------------------------------------------------------

#' @describeIn msAnalysis adds (when available) raw spectra and chromatograms
#' to the respective slots of an \linkS4class{msAnalysis} object.
#'
#' @export
#'
#' @aliases loadRawData,msAnalysis,msAnalysis-method
#'
setMethod("loadRawData", "msAnalysis", function(object) {

  list_out <- getRawData(object)

  object@spectra <- copy(list_out$spectra)
  object@chromatograms <- copy(list_out$chroms)

  return(object)
})



##### hasLoadedSpectra --------------------------------------------------------

#' @describeIn msAnalysis checks if the \linkS4class{msAnalysis} has loaded
#' spectra.
#'
#' @export
#'
#' @aliases hasLoadedSpectra,msAnalysis,msAnalysis-method
#'
setMethod("hasLoadedSpectra", "msAnalysis", function(object) {
  return(nrow(object@spectra) > 0 &&
           "intensity" %in% colnames(object@spectra))
})



##### hasLoadedChromatograms --------------------------------------------------

#' @describeIn msAnalysis checks if the \linkS4class{msAnalysis} has loaded
#' chromatograms.
#'
#' @export
#'
#' @aliases hasLoadedChromatograms,msAnalysis,msAnalysis-method
#'
setMethod("hasLoadedChromatograms", "msAnalysis", function(object) {
  return(nrow(object@chromatograms) > 0 &&
           "id" %in% colnames(object@chromatograms))
})



##### spectra -----------------------------------------------------------------

#' @describeIn msAnalysis getter for slot spectra in the
#' \linkS4class{msAnalysis}.
#'
#' @export
#'
#' @aliases spectra,msAnalysis,msAnalysis-method
#'
setMethod("spectra", "msAnalysis", function(object) {
  return(object@spectra)
})



##### plotSpectra -------------------------------------------------------------

#' @describeIn msAnalysis plots spectra in the \linkS4class{msAnalysis} in 3D.
#' The arguments \code{mz}, \code{ppm}, \code{rt}
#' and \code{sec} are used to construct the targets.
#' See ?\link{makeTargets} for more information.
#'
#' @export
#'
#' @aliases plotSpectra,msAnalysis,msAnalysis-method
#'
setMethod("plotSpectra", "msAnalysis", function(object,
                                                mz = NULL, rt = NULL,
                                                ppm = 20, sec = 60) {

  if (!hasLoadedSpectra(object)) {
    warning("Spectra not found, load raw spectra
            first with loadRawData() method.")
    return(NULL)
  }

  targets <- makeTargets(mz, rt, ppm, sec)

  spec_dt <- spectra(object)

  if (TRUE %in% c((targets$mzmax > 0), (targets$rtmax > 0))) {

      if (0 %in% targets$mzmax) targets$mzmax <- max(spec_dt$mz)
      if (0 %in% targets$rtmax) targets$rtmax <- max(spec_dt$rt)

      spec_dt <- spec_dt[mz >= min(targets$mzmin) & mz <= max(targets$mzmax) &
                         rt >= min(targets$rtmin) & rt <= max(targets$rtmax), ]
  }


  spec_dt$level <- factor(spec_dt$level)

  spec_dt_2 <- copy(spec_dt)
  spec_dt_2$rtmz <- paste(spec_dt_2$mz, spec_dt_2$rt)
  spec_dt_2_temp <- copy(spec_dt)
  spec_dt_2_temp$intensity <- 0
  spec_dt_2_temp$rtmz <- paste(spec_dt_2$mz, spec_dt_2$rt)
  spec_dt_2 <- rbind(spec_dt_2, spec_dt_2_temp)


  fig <- plotly::plot_ly(spec_dt_2, x = ~rt, y = ~mz, z = ~intensity) %>%
    plotly::group_by(~rtmz) %>%
    plotly::add_lines(color = ~level, colors = c("#BF382A", "#0C4B8E"))

  fig <- fig %>% plotly::layout(scene = list(
    xaxis = list(title = "Retention time (seconds)"),
    yaxis = list(title = "<i>m/z<i>"),
    zaxis = list(title = "Intensity (counts)")))

  return(fig)
})



##### chromatograms -----------------------------------------------------------

#' @describeIn msAnalysis getter for the chromatograms slot.
#'
#' @export
#'
#' @aliases chromatograms,msAnalysis,msAnalysis-method
#'
setMethod("chromatograms", "msAnalysis", function(object) {
  return(object@chromatograms)
})



##### plotChromatograms -------------------------------------------------------

#' @describeIn msAnalysis plots chromatograms in the \linkS4class{msAnalysis}.
#'
#' @export
#'
#' @param index A numeric vector with the index(s).
#'
#' @aliases plotChromatograms,msAnalysis,msAnalysis-method
#'
setMethod("plotChromatograms", "msAnalysis", function(object,
                                                      index = NULL,
                                                      id = NULL,
                                                      interactive = FALSE) {

  chroms <- chromatograms(object)
  chroms$analysis <- analysisNames(object)

  if (!is.null(index) & nrow(chroms) > 0) {
    idx <- index
    chroms <- chroms[chroms$index %in% idx, ]
  }

  if (nrow(chroms) == 0) {
    warning("Chromatogram/s not found, load chromatograms
            first with loadRawData() method.")
    return(NULL)
  }

  chroms <- chroms[, c("analysis", "id", "rt", "intensity")]

  chroms$var <- chroms$id

  if (length(unique(chroms$id)) == 1) {
    title <- unique(chroms$id)
  } else {
    title <- NULL
  }

  plotEICs(chroms, title = title, interactive = interactive)

})


# TODO make method to clear loaded raw data


##### EICs --------------------------------------------------------------------

#' @describeIn msAnalysis get extracted ion chromatograms (EICs)
#' for specified \emph{m/z} (Da) and retention time (seconds) targets.
#' The arguments \code{mz}, \code{ppm}, \code{rt}
#' and \code{sec} are used to construct the targets.
#' See ?\link{makeTargets} for more information.
#'
#' @template args-makeTargets
#'
#' @export
#'
#' @aliases EICs,msAnalysis,msAnalysis-method
#'
setMethod("EICs", "msAnalysis", function(object,
                                         mz = NULL, rt = NULL,
                                         ppm = 20, sec = 60, id = NULL) {

  targets <- makeTargets(mz, rt, ppm, sec, id)

  rtr <- targets[, c("rtmin", "rtmax")]
  colnames(rtr) <- c("min", "max")
  if ((nrow(rtr) == 1) & TRUE %in% (rtr$max == 0)) rtr <- NULL

  if (hasLoadedSpectra(object)) {

    spec <- spectra(object)
    spec <- spec[spec$level == 1, ]

    # if (!is.null(rtr)) {
    #   spec <- spec[checkOverlapRanges(spec$rt, rtr), ]
    # }

  } else {

    spec <- getRawData(object,
      TIC = FALSE, BPC = FALSE, chroms = FALSE,
      levels = 1, rtr = rtr)[["spectra"]]

  }

  if (nrow(spec) > 0) {

    spec <- spec[, c("scan", "level", "rt", "mz", "intensity")]

    eics <- rcpp_ms_make_eics_for_msAnalysis(spec, targets)

    eics <- as.data.table(eics)

    intensity <- NULL

    if (nrow(eics) > 0) {
      eics <- eics[, `:=`(intensity = sum(intensity)), by = c("id", "rt")][]
    }

  } else {

    eics <- data.table()

  }

  return(eics)
})



##### plotEICs ----------------------------------------------------------------

#' @describeIn msAnalysis Plots extracted ion chromatograms (EICs).
#' The arguments for data collection are the same as the \link{EICs} method.
#' A \linkS4class{data.table} as produced by \link{EICs} can be used instead.
#' The \code{legendNames} is a character vector with the same length as
#' targets for plotting and is used as legend of the plot.
#' Note, the plot legends the data by target.
#'
#' @param legendNames A character vector with the same length and order
#' as the number and order of targets to be used as plot legend.
#' @param title A character string with the plot title.
#' @param interactive Logical value, set to \code{TRUE} to use
#' the \pkg{plotly} instead of \pkg{base}. The default is \code{FALSE}.
#'
#' @export
#'
#' @aliases plotEICs,msAnalysis,msAnalysis-method
#'
setMethod("plotEICs", "msAnalysis", function(object,
                                             mz = NULL, rt = NULL,
                                             ppm = 20, sec = 30, id = NULL,
                                             legendNames = NULL,
                                             title = NULL,
                                             interactive = FALSE) {

  eics <- EICs(object, mz, rt, ppm, sec, id)

  return(
    plotEICs(eics,
      analyses = NULL,
      colorBy = "targets",
      legendNames, title, interactive
    )
  )
})



##### BPC ---------------------------------------------------------------------

#' @describeIn msAnalysis extracts the base peak chromatogram (BPC)
#' of the analysis.
#'
#' @export
#'
#' @aliases BPC,msAnalysis,msAnalysis-method
#'
setMethod("BPC", "msAnalysis", function(object) {

  bpc <- chromatograms(object)

  if (nrow(bpc) > 0) bpc <- bpc[id %in% "BPC", ]

  if (nrow(bpc) == 0) {

    bpc <- getRawData(object, spectra = FALSE,
                      TIC = FALSE, BPC = TRUE,
                      chroms = TRUE)[["chroms"]]

    bpc <- bpc[id %in% "BPC", ]

  }

  if (nrow(bpc) == 0) {

    targets <- makeTargets()
    targets$id <- "BPC"
    targets$rtmin <- getMetadata(object, which = "rt_start")$rt_start
    targets$rtmax <- getMetadata(object, which = "rt_end")$rt_end
    targets$mzmin <- getMetadata(object, which = "mz_low")$mz_low
    targets$mzmax <- getMetadata(object, which = "mz_high")$mz_high

    bpc <- EICs(object, mz = targets)

    intensity <- NULL

    bpc[bpc[, rank(intensity, ties.method = "max") != .N, by = rt]$V1, intensity := NA]
    bpc <- bpc[!is.na(intensity), ]

  } else {

    bpc <- bpc[, c("id", "rt", "mz", "intensity")]

  }

  return(bpc)
})



##### BPCs --------------------------------------------------------------------

#' @describeIn msAnalysis extracts the base peak chromatogram (BPC)
#' of the analysis.
#'
#' @export
#'
#' @aliases BPCs,msAnalysis,msAnalysis-method
#'
setMethod("BPCs", "msAnalysis", function(object) {
  return(BPC(object))
})



##### plotBPC -----------------------------------------------------------------

#' @describeIn msAnalysis plots the base peak chromatogram (BPC) in the analysis.
#'
#' @export
#'
#' @aliases plotBPC,msAnalysis,msAnalysis-method
#'
setMethod("plotBPC", "msAnalysis", function(object,
                                            title = NULL,
                                            interactive = FALSE) {

  bpc <- BPC(object)

  bpc$analysis <- analysisName(object)
  setcolorder(bpc, c("analysis", "id", "rt", "intensity"))

  return(
    plotTICs(bpc,
             analyses = NULL,
             colorBy = "analyses",
             title = title,
             interactive = interactive)
  )
})



##### plotBPCs ----------------------------------------------------------------

#' @describeIn msAnalysis plots the base peak chromatogram (BPC) in the analysis.
#'
#' @export
#'
#' @aliases plotBPCs,msAnalysis,msAnalysis-method
#'
setMethod("plotBPCs", "msAnalysis", function(object,
                                             title = NULL,
                                             interactive = FALSE) {
  return(plotBPC(object, title, interactive))
})



##### TIC ---------------------------------------------------------------------

#' @describeIn msAnalysis extracts the total ion chromatogram (TIC)
#' of the analysis.
#'
#' @export
#'
#' @aliases TIC,msAnalysis,msAnalysis-method
#'
setMethod("TIC", "msAnalysis", function(object) {

  tic <- chromatograms(object)

  if (nrow(tic) > 0) tic <- tic[id %in% "TIC", ]

  if (nrow(tic) == 0) {

    tic <- getRawData(object, spectra = FALSE,
                      TIC = TRUE, BPC = FALSE,
                      chroms = TRUE)[["chroms"]]

    tic <- tic[id %in% "TIC", ]

  }

  if (nrow(tic) == 0) {

    targets <- makeTargets()
    targets$id <- "TIC"
    targets$rtmin <- getMetadata(object, which = "rt_start")$rt_start
    targets$rtmax <- getMetadata(object, which = "rt_end")$rt_end
    targets$mzmin <- getMetadata(object, which = "mz_low")$mz_low
    targets$mzmax <- getMetadata(object, which = "mz_high")$mz_high

    tic <- EICs(object, mz = targets)
    tic <- tic[, .(id = unique(id), intensity = sum(intensity)), by = "rt"]

  } else {

    tic <- tic[, .(id, rt, intensity)]

  }

  return(tic)
})



##### TICs --------------------------------------------------------------------

#' @describeIn msAnalysis extracts the total ion chromatogram (TIC)
#' of the analysis.
#'
#' @export
#'
#' @aliases TICs,msAnalysis,msAnalysis-method
#'
setMethod("TICs", "msAnalysis", function(object) {
  return(TIC(object))
})



##### plotTIC -----------------------------------------------------------------

#' @describeIn msAnalysis plots the total ion chromatogram (TIC) in the analysis.
#'
#' @export
#'
#' @aliases plotTIC,msAnalysis,msAnalysis-method
#'
setMethod("plotTIC", "msAnalysis", function(object,
                                            title = NULL,
                                            interactive = FALSE) {

  tic <- TIC(object)

  tic$analysis <- analysisName(object)
  setcolorder(tic, c("analysis", "id", "rt", "intensity"))

  return(
    plotTICs(tic,
             analyses = NULL,
             colorBy = "analyses",
             title = title,
             interactive = interactive)
  )
})



##### plotTICs ----------------------------------------------------------------

#' @describeIn msAnalysis plots the total ion chromatogram (TIC) in the analysis.
#'
#' @export
#'
#' @aliases plotTICs,msAnalysis,msAnalysis-method
#'
setMethod("plotTICs", "msAnalysis", function(object,
                                             title = NULL,
                                             interactive = FALSE) {
  return(plotTIC(object, title, interactive))
})



##### XICs --------------------------------------------------------------------

#' @describeIn msAnalysis gets three dimensional (\emph{m/z}, time and intensity)
#' extracted ion chromatograms (XICs) for specified \emph{m/z} and retention
#' time pair targets in analysis. The arguments \code{mz}, \code{ppm},
#' \code{rt}, \code{sec} and \code{id} are used to construct the targets.
#' See ?\link{makeTargets} for more information.
#'
#' @export
#'
#' @aliases XICs,msAnalysis,msAnalysis-method
#'
setMethod("XICs", "msAnalysis", function(object,
                                         mz = NULL, rt = NULL,
                                         ppm = 20, sec = 60, id = NULL) {

  targets <- makeTargets(mz, rt, ppm, sec, id)

  rtr <- targets[, .(rtmin, rtmax)]
  colnames(rtr) <- c("min", "max")
  if ((nrow(rtr) == 1) & TRUE %in% (rtr$max == 0)) rtr = NULL

  if (hasLoadedSpectra(object)) {

    spec <- spectra(object)
    spec <- spec[level == 1, ]

  } else {

    spec <- getRawData(object,
                       TIC = FALSE, BPC = FALSE, chroms = FALSE,
                       levels = 1, rtr = rtr)[["spectra"]]

  }

  if (nrow(spec) > 0) {

    spec <- spec[, .(scan, level, rt, mz, intensity)]

    xics <- rcpp_ms_make_eics_for_msAnalysis(spec, targets)

    xics <- as.data.table(xics)

  } else {

    xics <- data.table()

  }

  return(xics)
})



##### plotXICs ----------------------------------------------------------------

#' @describeIn msAnalysis plots three dimensional (\emph{m/z}, time and intensity)
#' extracted ion chromatograms (XICs) for specified \emph{m/z} and retention
#' time pair targets in analyses of an \linkS4class{msAnalysis} object.
#' The arguments \code{mz}, \code{ppm}, \code{rt}, \code{sec} and \code{id} are
#' used to construct the targets. See ?\link{makeTargets} for more information.
#' When \code{plotTargetMark} is \code{TRUE} a target is plotted representing
#' the deviations as defined by the arguments \code{ppmMark} and \code{secMark}
#' in ppm and seconds, respectively. When ranges were given to build the XIC,
#' exact \emph{m/z} and time targets can be specified with the argument
#' \code{targetsMark}. \code{targetsMark} should be a two column table named
#' mz and rt with exact \emph{m/z} and time targets. Note that the number of
#' rows should be the same as the number of target in the XIC. The number of
#' rows to plot multiple targets can be defined by the argument  \code{numberRows}.
#'
#' @template args_plots_xics
#'
#' @export
#'
#' @aliases plotXICs,msAnalysis,msAnalysis-method
#'
setMethod("plotXICs", "msAnalysis", function(object,
                                             mz = NULL, ppm = 20,
                                             rt = NULL, sec = 60, id = NULL,
                                             legendNames = NULL,
                                             plotTargetMark = TRUE,
                                             targetsMark = NULL,
                                             ppmMark = 5,
                                             secMark = 10,
                                             numberRows = 1) {

  xic <- XICs(object, mz, rt, ppm, sec, id)

  if (is.null(targetsMark)) {
    targetsMark <- makeTargets(mz, rt, ppm, sec, id)
    targetsMark <- targetsMark[id %in% xic$id, ]
  }

  plot <- plotXICs(xic,
    legendNames = legendNames,
    plotTargetMark = plotTargetMark,
    targetsMark = targetsMark[, .(id, mz, rt)],
    ppmMark = ppmMark,
    secMark = secMark,
    numberRows = numberRows
  )

  return(plot)
})



##### MS2s --------------------------------------------------------------------

#' @describeIn msAnalysis gets MS2 data for specified \emph{m/z} and
#' retention time (seconds) targets from the analysis.
#'
#' @param mzClust A numeric value defining the \emph{m/z} cutoff (in Da) to
#' cluster mass traces from different scans.
#' @param isolationWindow A numeric value defining the isolation window (in Da)
#' applied to isolate the MS1 precursor.
#'
#' @export
#'
#' @aliases MS2s,msAnalysis,msAnalysis-method
#'
setMethod("MS2s", "msAnalysis", function(object,
                                         mzClust = 0.01,
                                         isolationWindow = 1,
                                         mz = NULL, rt = NULL,
                                         ppm = 20, sec = 60, id = NULL) {

  targets <- makeTargets(mz, rt, ppm, sec, id)

  targets$mzmin <- targets$mzmin - (isolationWindow/2)
  targets$mzmax <- targets$mzmax + (isolationWindow/2)

  rtr <- targets[, .(rtmin, rtmax)]
  colnames(rtr) <- c("min", "max")
  rtr$min <- rtr$min * 0.95
  rtr$max <- rtr$max * 1.05
  if ((nrow(rtr) == 1) & TRUE %in% (rtr$max == 0)) rtr = NULL

  preMZrange <- targets[, .(mzmin, mzmax)]
  colnames(preMZrange) <- c("min", "max")
  preMZrange$min <- preMZrange$min * 0.95
  preMZrange$max <- preMZrange$max * 1.05
  if ((nrow(preMZrange) == 1) & TRUE %in% (preMZrange$max == 0)) rtr = NULL

  if (hasLoadedSpectra(object)) {

    spec <- spectra(object)
    spec <- spec[level == 2, ]

  } else {

    spec <- getRawData(object,
                       TIC = FALSE, BPC = FALSE, chroms = FALSE,
                       levels = 2, rtr = rtr,
                       preMZrange = preMZrange)[["spectra"]]

  }

  if (nrow(spec) > 0) {

    ms2 <- rcpp_ms_extract_ms2_for_msAnalysis(spec, targets,
                                              mzClust, verbose = FALSE)

    ms2 <- rbindlist(ms2, fill = TRUE)

  }

  return(ms2)
})



##### plotMS2s ----------------------------------------------------------------

#' @describeIn msAnalysis plots MS2 data for specified \emph{m/z} and
#' retention time (seconds) targets in the analysis.
#'
#' @export
#'
#' @aliases plotMS2s,msAnalysis,msAnalysis-method
#'
setMethod("plotMS2s", "msAnalysis", function(object,
                                             mzClust = 0.01,
                                             isolationWindow = 1.3,
                                             mz = NULL, rt = NULL,
                                             ppm = 20,  sec = 60, id = NULL,
                                             legendNames = NULL,
                                             title = NULL,
                                             interactive = FALSE) {

  ms2 <- MS2s(object, mzClust, isolationWindow, mz, rt, ppm, sec, id)

  if (nrow(ms2) < 1) return(cat("Data was not found for any of the targets!"))

  return(
    plotMS2s(ms2, legendNames = legendNames, title = title,
             colorBy = "targets", interactive = interactive)
  )
})



#### settings ______________---------------------------------------------------

##### addSettings -------------------------------------------------------------

#' @describeIn msAnalysis adds processing settings to the analysis.
#'
#' @export
#'
#' @aliases addSettings,msAnalysis,msAnalysis-method
#'
setMethod("addSettings", "msAnalysis", function(object, settings) {

  valid <- testClass(settings, "settings")

  if (!valid) {
    warning("Settings class not correct, returning original object!")
    return(object)
  }

  object@settings[[settings@call]] <- settings

  return(object)
})



##### getSettingsNames --------------------------------------------------------

#' @describeIn msAnalysis gets the call names of processing settings
#' in the analysis.
#'
#' @export
#'
#' @aliases getSettingsNames,msAnalysis,msAnalysis-method
#'
setMethod("getSettingsNames", "msAnalysis", function(object) {
  return(names(object@settings))
})



##### getSettings -------------------------------------------------------------

#' @describeIn msAnalysis gets processing settings in the analysis.
#'
#' @param call The call name of the settings to retrieve.
#'
#' @export
#'
#' @aliases getSettings,msAnalysis,msAnalysis-method
#'
setMethod("getSettings", "msAnalysis", function(object, call = NULL) {

  if (is.null(call)) {
    param <- list(object@settings)
  } else {
    param <- list(object@settings[[call]])
  }

  names(param) <- analysisNames(object)

  return(param)
})



#### peak methods _______------------------------------------------------------

##### hasPeaks ----------------------------------------------------------------

#' @describeIn msAnalysis checks if the \linkS4class{msAnalysis} has peaks.
#'
#' @export
#'
#' @aliases hasPeaks,msAnalysis,msAnalysis-method
#'
setMethod("hasPeaks", "msAnalysis", function(object) {
  return(nrow(object@peaks) > 0)
})



##### peaks -------------------------------------------------------------------

#' @describeIn msAnalysis gets chromatographic peaks from the analysis.
#' The arguments \code{targetID}, \code{mass}, \code{mz} and \code{rt} can
#' be used to select specific peaks. The \emph{id} of peaks and/or features
#' can be given in the \code{targetsID} argument to select the respective peaks.
#' When the \code{filtered} argument is set to \code{TRUE}, filtered peaks are
#' also returned.
#'
#' @param mass As the argument \code{mz} but specifying the neutral mass
#' not the mass-to-charge ratio (\emph{m/z}) for building targets.
#' @template args-single-targetsID
#' @template args-single-filtered
#'
#' @export
#'
#' @aliases peaks,msAnalysis,msAnalysis-method
#'
setMethod("peaks", "msAnalysis", function(object,
                                          targetsID = NULL,
                                          mass = NULL,
                                          mz = NULL, rt = NULL,
                                          ppm = 20, sec = 30,
                                          filtered = TRUE) {

  if (!filtered) {
    pks <- object@peaks[!object@peaks$filtered, ]
  } else {
    pks <- object@peaks
  }

  if (!is.null(targetsID) & "feature" %in% colnames(pks)) {
    pks <- pks[id %in% targetsID | feature %in% targetsID, ]
    return(pks)
  } else if (!is.null(targetsID)) {
    pks <- pks[id %in% targetsID, ]
    return(pks)
  }

  if (!is.null(mass)) {

    if (is.data.frame(mass)) {
      colnames(mass) <- gsub("mass", "mz", colnames(mass))
      colnames(mass) <- gsub("neutralMass", "mz", colnames(mass))
    }

    targets <- makeTargets(mass, rt, ppm, sec)

    sel <- rep(FALSE, nrow(pks))
    for (i in seq_len(nrow(targets))) {
      sel[between(pks$mass, targets$mzmin[i], targets$mzmax[i]) &
            between(pks$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE
    }

    return(pks[sel])
  }

  if (!is.null(mz)) {

    targets <- makeTargets(mz, rt, ppm, sec)

    sel <- rep(FALSE, nrow(pks))
    for (i in seq_len(nrow(targets))) {
      sel[between(pks$mz, targets$mzmin[i], targets$mzmax[i]) &
            between(pks$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE
    }

    return(pks[sel])
  }

  return(pks)
})



##### peakEICs ----------------------------------------------------------------

#' @describeIn msAnalysis getter for peak EICs in the analysis object.
#' The arguments \code{targetID}, \code{mass}, \code{mz} and \code{rt} can
#' be used to select specific peaks. The \emph{id} of peaks and/or features
#' can be given in the \code{targetsID} argument to select the respective peaks.
#'
#' @export
#'
#' @aliases peakEICs,msAnalysis,msAnalysis-method
#'
setMethod("peakEICs", "msAnalysis", function(object,
                                             targetsID = NULL,
                                             mass = NULL,
                                             mz = NULL, rt = NULL,
                                             ppm = 20, sec = 30,
                                             rtExpand = 120,
                                             mzExpand = 0.005,
                                             filtered = TRUE) {

  peaks <- peaks(object, targetsID, mass, mz, rt, ppm, sec, filtered)

  pks_tars <- copy(peaks[, .(id, mz, rt, mzmin, mzmax, rtmin, rtmax)])
  pks_tars$rtmin <- pks_tars$rtmin - rtExpand
  pks_tars$rtmax <- pks_tars$rtmax + rtExpand
  pks_tars$mzmin <- pks_tars$mzmin - mzExpand
  pks_tars$mzmax <- pks_tars$mzmax + mzExpand

  eic <- EICs(object, mz = pks_tars)

  return(eic)
})



##### plotPeaks ---------------------------------------------------------------

#' @describeIn msAnalysis plots chromatographic peaks in the analysis.
#' The arguments \code{targetID}, \code{mass}, \code{mz} and \code{rt} can
#' be used to select specific peaks. The \emph{id} of peaks and/or features
#' can be given in the \code{targetsID} argument to select the respective peaks.
#' The \code{legendNames} is a character vector with the same length as targets
#' for plotting and is used as legend for the plot.
#'
#' @export
#'
#' @aliases plotPeaks,msAnalysis,msAnalysis-method
#'
setMethod("plotPeaks", "msAnalysis", function(object,
                                              targetsID = NULL,
                                              mass = NULL,
                                              mz = NULL, rt = NULL,
                                              ppm = 20, sec = 30,
                                              rtExpand = 120,
                                              mzExpand = 0.005,
                                              filtered = TRUE,
                                              legendNames = NULL,
                                              title = NULL,
                                              interactive = FALSE) {

  colorBy = "targets"

  peaks <- peaks(object, targetsID, mass, mz, rt, ppm, sec, filtered)

  pks_tars <- copy(peaks[, .(id, mz, rt, mzmin, mzmax, rtmin, rtmax)])
  pks_tars$rtmin <- pks_tars$rtmin - rtExpand
  pks_tars$rtmax <- pks_tars$rtmax + rtExpand
  pks_tars$mzmin <- pks_tars$mzmin - mzExpand
  pks_tars$mzmax <- pks_tars$mzmax + mzExpand

  eic <- EICs(object, mz = pks_tars)

  return(
    plotPeaks(eic, peaks, analyses = NULL, colorBy = "targets",
      legendNames = legendNames,
      title = title,
      interactive = interactive
    )
  )
})



##### mapPeaks ----------------------------------------------------------------

#' @describeIn msAnalysis maps chromatographic peaks in the analysis.
#' The \code{legendNames} is a character vector with the same length as targets
#' and is used as legend for the plot.
#'
#' @param xlim A length one or two numeric vector for
#' setting the \emph{x} limits of a plot.
#' @param ylim A length one or two numeric vector for
#' setting the \emph{y} limits of a plot.
#'
#' @export
#'
#' @aliases mapPeaks,msAnalysis,msAnalysis-method
#'
setMethod("mapPeaks", "msAnalysis", function(object,
                                             targetsID = NULL,
                                             mass = NULL,
                                             mz = NULL, rt = NULL,
                                             ppm = 20, sec = 30,
                                             filtered = TRUE,
                                             legendNames = NULL,
                                             xlim = 30,
                                             ylim = 0.05,
                                             title = NULL,
                                             showLegend = TRUE,
                                             interactive = FALSE) {

  colorBy = "targets"

  peaks <- peaks(
    object,
    targetsID,
    mass,
    mz, rt,
    ppm, sec,
    filtered
  )

  if (nrow(peaks) < 1) return(cat("Requested peaks were not found!"))

  if (!is.null(legendNames) &
                    length(legendNames) == length(unique(peaks$id))) {

    leg <- legendNames
    names(leg) <- unique(peaks$id)
    varkey <- sapply(peaks$id, function(x) leg[x])

  } else {

    varkey <- peaks$id

  }

  peaks[, var := varkey][]

  if (!interactive) {

    return(
      mapPeaksStatic(peaks, xlim, ylim, title,
                     showLegend = showLegend)
    )

  } else {

    plot <- mapPeaksInteractive(peaks, xlim, ylim, title)

    return(plot)
  }

})



#### _sub-setting peaks_ ------------------------------------------------------

#' @describeIn msAnalysis subset on peaks, using peak index or id.
#'
#' @param x A \linkS4class{msAnalysis} object.
#' @param i The indice/s or name/s of the peaks to keep in the \code{x} object.
#' @param j Not applicable to \linkS4class{msAnalysis}.
#' @param drop Not applicable to \linkS4class{msAnalysis}.
#'
#' @export
#'
setMethod("[", c("msAnalysis", "ANY", "missing", "missing"), function(x, i, ...) {

  if (!missing(i)) {

    if (!is.character(i)) {

      pname <- peaks(x)[i, ]
      pname <- pname$id

    } else {

      if (FALSE %in% (i %in% peaks(x)$id)) {
        warning("Given peak name/s not found in the object.")
        return(x)
      }

      pname <- i
    }

    x@peaks <- x@peaks[id %in% pname, ]

  }

  return(x)
})



#### _hasAdjustedRetentionTime_ -----------------------------------------------

#' @describeIn msAnalysis getter for presence of adjusted retention time
#' in the \linkS4class{msAnalysis}.
#'
#' @export
#'
#' @aliases hasAdjustedRetentionTime,msAnalysis,msAnalysis-method
#'
setMethod("hasAdjustedRetentionTime", "msAnalysis", function(object) {
  return("rtAdjusted" %in% colnames(object@spectra))
})



### _as.features_ -------------------------------------------------------------

#' @describeIn msAnalysis converts the \linkS4class{msAnalysis}
#' to a \linkS4class{features} object from the package \pkg{patRoon}.
#'
#' @export
#'
#' @aliases as.features,msAnalysis,msAnalysis-method
#'
setMethod("as.features", "msAnalysis", function(object) {

  requireNamespace("patRoon")

  anaInfo <- analysisInfo(object)

  feat <- peaks(object)

  if ("filtered" %in% colnames(feat)) feat <- feat[!feat$filtered, ]

  if (nrow(feat) == 0) {
    warning("Peaks not found to create a patRoon's features S4 object!")
    return(NULL)
  }

  setnames(feat,
           c("id", "rt", "rtmin", "rtmax", "feature"),
           c("ID", "ret", "retmin", "retmax", "group"),
           skip_absent = TRUE
  )

  feat <- select(feat,
            ID, ret, mz, area, intensity, retmin, retmax, mzmin, mzmax,
            everything()
  )

  if (length(unique(polarities(object))) > 1 | "both" %in% polarities(object)) {

    feat$mzmin <- feat$mass - (feat$mz - feat$mzmin)
    feat$mzmax <- feat$mass + (feat$mzmax - feat$mz)
    feat$mz <- feat$mass
    feat$mass <- NULL

    feat <- list(feat)
    names(feat) <- analysisName(object)

    feat_obj <- new("featuresSet",
      features = feat, analysisInfo = anaInfo, algorithm = "openms-set")

  } else {

    feat <- list(feat)
    names(feat) <- analysisName(object)

    feat_obj <- new("featuresOpenMS", features = feat, analysisInfo = anaInfo)

  }

  return(feat_obj)
})

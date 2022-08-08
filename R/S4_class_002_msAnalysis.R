
#### validity ------------------------------------------------------------------

msAnalysis_validity <- function(object) {

  valid <- TRUE

  if (nrow(object@peaks) > 0) {
    must_have_names <- c("id", "rt", "mz", "intensity", "area", "drt",
                         "rtmin", "rtmax", "dppm", "mzmin", "mzmax")
    valid <- !FALSE %in% must_have_names %in% colnames(object@peaks)
  }

  return(valid)
}



### msAnalysis -----------------------------------------------------------------

#' msAnalysis-class
#'
#' @description An S4 class representing an MS sample/file within the
#' \pkg{streamFind} package. The \code{msAnalysis} is used to store and
#' manage MS data and the respective methods can be used for inspection,
#' processing and evaluation.
#'
#' @template slot-msAnalysis
#'
#' @references
#' \insertRef{patroon01}{streamFind}
#'
#' \insertRef{proteo01}{streamFind}
#'
#' \insertRef{proteo02}{streamFind}
#'
#' \insertRef{mzr01}{streamFind}
#'
#' \insertRef{mzr02}{streamFind}
#'
#' \insertRef{mzr03}{streamFind}
#'
#' \insertRef{mzr04}{streamFind}
#'
#' @export
#'
#' @importFrom data.table data.table
#'
#' @md
setClass("msAnalysis",
  slots = c(
    analysis = "character",
    file = "character",
    replicate = "character",
    blank = "character",
    metadata = "list",
    spectra = "data.table",
    chromatograms = "data.table",
    parameters = "list",
    peaks = "data.table"
  ),
  prototype = list(
    analysis = NA_character_,
    file = NA_character_,
    replicate = NA_character_,
    blank = NA_character_,
    metadata = list(),
    spectra = data.table(),
    chromatograms = data.table(),
    parameters = list(),
    peaks = data.table()
  ),
  validity = msAnalysis_validity
)



### S4 methods -----------------------------------------------------------------

#### show ----------------------------------------------------------------

#' @describeIn msAnalysis shows the details of an \linkS4class{msAnalysis}.
#'
#' @param object An \linkS4class{msAnalysis} object.
#'
#' @export
#'
setMethod("show", "msAnalysis", function(object) {

  cat(
    "  Class         ", is(object), "\n",
    "  Name          ", object@analysis, "\n",
    "  Replicate     ", object@replicate, "\n",
    "  Blank         ", object@blank, "\n",
    "  Polarity      ", object@metadata$polarity, "\n",
    "  File          ", object@file, "\n",
    "  Levels        ", object@metadata$ms_levels, " \n",
    "  Centroided    ", object@metadata$centroided, "\n",
    "  Spectra       ", object@metadata$number_spectra, "\n",
    "  Chromatograms ", object@metadata$number_chromatograms, "\n",
    "  Peaks         ", nrow(object@peaks), "\n",
    "  Parameters: \n",
    sep = ""
  )
  if (length(object@parameters) > 0) {
    for (i in seq_len(length(object@parameters))) {
      cat(
        "     ",
        names(object@parameters)[i], ": ",
        object@parameters[[i]]@algorithm,  "\n", sep = ""
      )
    }
  } else {
    cat("     n.a.", "\n", sep = "")
  }
})



#### filePaths -----------------------------------------------------------------

#' @describeIn msAnalysis getter for analysis file path.
#'
#' @export
#'
#' @aliases filePaths,msAnalysis,msAnalysis-method
#'
setMethod("filePaths", "msAnalysis", function(object) {
  fl <- object@file
  names(fl) <- analysisNames(object)
  return(fl)
})

#### analysisInfo --------------------------------------------------------------

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
    "analysis" = obj@analysis,
    "group" = obj@replicate,
    "blank" = obj@blank,
    "class" = is(obj))
  )
})

#### analysisTable -------------------------------------------------------------

#' @describeIn msAnalysis getter for analysis table as \link{data.table} with
#' four columns: file, analysis, replicate and blank.
#'
#' @export
#'
#' @importFrom data.table data.table
#'
#' @aliases analysisTable,msAnalysis,msAnalysis-method
#'
setMethod("analysisTable", "msAnalysis", function(object) {
  return(data.table(
    "file" = object@file,
    "analysis" = object@analysis,
    "replicate" = object@replicate,
    "blank" = object@blank)
  )
})

#### analysisNames -------------------------------------------------------------

#' @describeIn msAnalysis getter for analysis name.
#'
#' @export
#'
#' @aliases analysisNames,msAnalysis,msAnalysis-method
#'
setMethod("analysisNames", "msAnalysis", function(object) {
  ana <- object@analysis
  names(ana) <- ana
  return(ana)

})

#### replicateNames ------------------------------------------------------------

#' @describeIn msAnalysis getter for the replicate name.
#'
#' @export
#'
#' @aliases replicateNames,msAnalysis,msAnalysis-method
#'
setMethod("replicateNames", "msAnalysis", function(object) {
  rep <- object@replicate
  names(rep) <- analysisNames(object)
  return(rep)
})

#### replicateNames<- ----------------------------------------------------------

#' @describeIn msAnalysis setter for analysis replicate name.
#'  The \code{value} is a character string with analysis replicate name.
#'
#' @param value A character string applicable to the respective method.
#'
#' @export
#'
#' @aliases replicateNames<-,msAnalysis,msAnalysis-method
#'
setMethod("replicateNames<-",
          signature("msAnalysis", "ANY"), function(object, value) {

  if (length(value) != 1) {
    warning("Length of value must be one.")
    return(object)
  }
  object@replicate <- unname(value)
  return(object)
})

#### blankReplicateNames -------------------------------------------------------

#' @describeIn msAnalysis getter for associated blank replicate name.
#'
#' @export
#'
#' @aliases blankReplicateNames,msAnalysis,msAnalysis-method
#'
setMethod("blankReplicateNames", "msAnalysis", function(object) {
  blk <- object@blank
  names(blk) <- analysisNames(object)
  return(blk)
})

#### blankReplicateNames<- -----------------------------------------------------

#' @describeIn msAnalysis setter for associated blank replicate.
#' The \code{value} is a character string with associated blank replicate name.
#'
#' @export
#'
#' @aliases blankReplicateNames<-,msAnalysis,msAnalysis-method
#'
setMethod("blankReplicateNames<-",
          signature("msAnalysis", "ANY"), function(object, value) {

  if (length(value) != 1) {
    warning("Length of value must be one.")
    return(object)
  }
  object@blank <- unname(value)
  return(object)
})

#### metadata ------------------------------------------------------------------

#' @describeIn msAnalysis getter for analysis metadata.
#'  Returns a \linkS4class{data.table} with a column per metadata entry.
#'
#' @template args-single-which-entry
#'
#' @export
#'
#' @importMethodsFrom S4Vectors metadata
#' @importFrom data.table as.data.table
#'
#' @aliases metadata,msAnalysis,msAnalysis-method
#'
setMethod("metadata", "msAnalysis", function(x, which = NULL) {

  if (!is.null(which)) {
    mtd_a <- c(list(analysis = x@analysis), x@metadata[which])
  } else {
    mtd_a <- c(list(analysis = x@analysis), x@metadata)
  }

  if ("msLevels" %in% names(mtd_a)) {
    mtd_a$msLevels <- paste(sort(mtd_a$msLevels), collapse = "; ")
  }

  mtd_a <- as.data.table(mtd_a)

  return(mtd_a)
})

#### addMetadata ---------------------------------------------------------------

#' @describeIn msAnalysis setter for analysis metadata.
#'
#' @param metadata A named vector with metadata entries or a one row
#' \code{data.frame} or \code{data.table} with metadata added as columns.
#' @param overwrite Logical, set to \code{TRUE} to overwrite.
#'
#' @export
#'
#' @importMethodsFrom S4Vectors metadata
#' @importFrom data.table is.data.table
#'
#' @aliases addMetadata,msAnalysis,msAnalysis-method
#'
setMethod("addMetadata", "msAnalysis", function(object,
                                                metadata = NULL,
                                                overwrite = FALSE) {

  if (is.data.frame(metadata) | is.data.table(metadata)) {
    name_is_already_there <- colnames(metadata) %in% names(object@metadata)
    metadata <- metadata[1, ] #only takes the first row

  } else if (is.vector(metadata)) {
    if (is.null(names(metadata))) {
      warning("Metadata must be a named vector named!")
      return(object)
    }
    name_is_already_there <- names(metadata) %in% names(object@metadata)
  }

  if (exists("name_is_already_there")) {
    if (TRUE %in% name_is_already_there & !overwrite) {
      warning("Metadata name/s already exist/s!")
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

#### polarities ----------------------------------------------------------------

#' @describeIn msAnalysis getter for analyses polarity.
#'
#' @export
#'
#' @aliases polarities,msAnalysis,msAnalysis-method
#'
setMethod("polarities", "msAnalysis", function(object) {
  mt <- metadata(object, which = "polarity")
  mt_v <- mt$polarity
  names(mt_v) <- mt$analysis
  return(mt_v)
})

### loadRawData ----------------------------------------------------------------

#' @describeIn msAnalysis adds raw spectra and chromatograms to the respective
#' slots of an \linkS4class{msAnalysis} object.
#'
#' @param minIntensityMS1 Numeric value on length one with the
#' minimum intensity of MS1 level traces.
#' @param minIntensityMS2 Numeric value on length one with the
#' minimum intensity of MS2 level traces.
#'
#' @export
#'
#' @importFrom data.table copy
#'
#' @aliases loadRawData,msAnalysis,msAnalysis-method
#'
setMethod("loadRawData", "msAnalysis", function(object,
                                                minIntensityMS1 = 0,
                                                minIntensityMS2 = 0) {

  lvs <- metadata(object, "msLevels")$msLevels
  lvs <- as.numeric(strsplit(lvs, "; ", fixed = TRUE)[[1]])

  rd_list <- loadRawDataMZR(filePaths(object),
                            spectra = TRUE,
                            level = lvs,
                            rtr = NULL,
                            minIntensityMS1 = minIntensityMS1,
                            minIntensityMS2 = minIntensityMS2,
                            chroms = TRUE,
                            chromsID = NULL,
                            ifChromNoSpectra = FALSE)

  if ("spectra" %in% names(rd_list)) object@spectra <- copy(rd_list$spectra)
  if ("chroms" %in% names(rd_list)) object@chromatograms <- copy(rd_list$chroms)

  return(object)
})

### hasLoadedSpectra -----------------------------------------------------------

#' @describeIn msAnalysis checks if the \linkS4class{msAnalysis} has loaded
#' raw spectra.
#'
#' @export
#'
#' @aliases hasLoadedSpectra,msAnalysis,msAnalysis-method
#'
setMethod("hasLoadedSpectra", "msAnalysis", function(object) {

  return(nrow(object@spectra) > 0 && "intensity" %in% colnames(object@spectra))
})

### hasLoadedChromatograms -----------------------------------------------------

#' @describeIn msAnalysis checks if the \linkS4class{msAnalysis} has loaded raw
#' chromatograms.
#'
#' @export
#'
#' @aliases hasLoadedChromatograms,msAnalysis,msAnalysis-method
#'
setMethod("hasLoadedChromatograms", "msAnalysis", function(object) {

  return(nrow(object@chromatograms) > 0)
})

### spectra --------------------------------------------------------------------

#' @describeIn msAnalysis getter for slot spectra in the
#' \linkS4class{msAnalysis}.
#'
#' @export
#'
#' @importMethodsFrom ProtGenerics spectra
#'
#' @aliases spectra,msAnalysis,msAnalysis-method
#'
setMethod("spectra", "msAnalysis", function(object) {

  return(object@spectra)
})

### EICs -----------------------------------------------------------------------

#' @describeIn msAnalysis get extracted ion chromatograms (EICs)
#' for specified \emph{m/z} (Da) and retention time (seconds) targets.
#' The arguments \code{mz}, \code{ppm}, \code{rt}
#' and \code{sec} are used to construct the targets.
#' See ?\link{makeTargets} for more information.
#'
#' @template args-makeTargets
#'
#' @importFrom data.table rbindlist as.data.table
#'
#' @export
#'
#' @aliases EICs,msAnalysis,msAnalysis-method
#'
setMethod("EICs", "msAnalysis", function(object,
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 60, id = NULL) {

  targets <- makeTargets(mz, rt, ppm, sec, id)

  rtr <- c(min(targets$rtmin), max(targets$rtmax))
  if (rtr[2] == 0) rtr = NULL

  if (!hasLoadedSpectra(object)) {
    spec <- loadRawDataMZR(file = filePaths(object),
                           chroms = FALSE, level = 1, rtr = rtr)
    spec <- spec$spectra
  } else {
    spec <- spectra(object)
    spec <- spec[lv == 1, ]
    spec <- spec[, .(index, scan, lv, rt, mz, intensity)]
  }

  spec <- list(as.data.frame(spec))
  names(spec) <- analysisNames(object)

  targets$analysis <- analysisNames(object)
  targets$replicate <- replicateNames(object)

  eics <- rcpp_extract_eics(spec = spec, targets = targets)

  eics <- rbindlist(eics)

  eics <- eics[, .(intensity = sum(intensity)),
               by = c("analysis", "replicate","id", "rt")]

  return(eics)
})

### plotEICs -------------------------------------------------------------------

#' @describeIn msAnalysis A method for plotting extracted ion chromatograms
#' (EICs) of data in the analysis file.
#' The arguments for data collection are the same as the \link{EICs} method.
#' A \linkS4class{data.table} can be used instead.
#' The \code{legendNames} is a character vector with the same length as
#' targets for plotting and can be used to legend the plot.
#' Note, the plot legends the data by target.
#'
#' @param legendNames A character vector with the same length and order
#' as the number and order of targets to be used as plot legend.
#' @param title a character string to the define a title.
#' @param interactive Logical value, set to \code{TRUE} to use
#' the \pkg{plotly} instead of \pkg{base}. The default is \code{FALSE}.
#'
#' @export
#'
#' @aliases plotEICs,msAnalysis,msAnalysis-method
#'
setMethod("plotEICs", "msAnalysis", function(object,
                                             mz = NULL, ppm = 20,
                                             rt = NULL, sec = 30, id = NULL,
                                             legendNames = NULL,
                                             title = NULL,
                                             interactive = FALSE) {

  eic <- EICs(object, mz, ppm, rt, sec, id)

  return(
    plotEICs(eic,
      analyses = NULL,
      colorBy = "targets",
      legendNames, title, interactive
    )
  )
})

### TICs -----------------------------------------------------------------------

#' @describeIn msAnalysis extracts the total ion chromatograms (TICs)
#' of analysis.
#'
#' @export
#'
#' @importFrom data.table `:=` setcolorder
#'
#' @aliases TICs,msAnalysis,msAnalysis-method
#'
setMethod("TICs", "msAnalysis", function(object) {

  if (!hasLoadedChromatograms(object)) {
    tic <- loadRawDataMZR(
      filePaths(object),
      spectra = FALSE, chroms = TRUE,
      chromsID = "TIC")[["chroms"]]
  } else {
    tic <- object@chromatograms[id %in% "TIC", ]
  }

  if (nrow(tic) < 1) {

    targets <- makeTargets()
    targets$id <- "TIC"
    targets[rtmin == 0, rtmin := metadata(object, which = "dStartTime")$dStartTime]
    targets[rtmax == 0, rtmax := metadata(object, which = "dEndTime")$dEndTime]
    targets[mzmin == 0, mzmin := metadata(object, which = "lowMz")$lowMz]
    targets[mzmax == 0, mzmax := metadata(object, which = "highMz")$highMz]

    tic <- EICs(object, mz = targets)
    tic <- tic[, .(id = unique(id), intensity = sum(intensity)), by = "rt"]

  } else {
    tic <- tic[, .(id, rt, intensity)]
  }

  tic[, `:=`(analysis = analysisNames(object), replicate = replicateNames(object))]
  setcolorder(tic, c("analysis", "replicate", "id", "rt", "intensity"))

  if (max(tic$rt) < 120) tic[, rt := rt * 60]
  tic <- tic[intensity > 0, ]

  return(tic)
})

### plotTICs -------------------------------------------------------------------

#' @describeIn msAnalysis plots the total ion chromatogram (TIC) in the analysis.
#'
#' @export
#'
#' @aliases plotTICs,msAnalysis,msAnalysis-method
#'
setMethod("plotTICs", "msAnalysis", function(object,
                                             title = NULL,
                                             interactive = FALSE) {

  tics <- TICs(object)

  return(
    plotTICs(tics,
      analyses = NULL,
      colorBy = "analyses",
      title = title,
      interactive = interactive
    )
  )
})

### XICs -----------------------------------------------------------------------

#' @describeIn msAnalysis get three dimensional (\emph{m/z}, time and intensity)
#' extracted ion chromatograms (XICs) for specified \emph{m/z} and retention
#' time pair targets in analysis. The arguments \code{mz}, \code{ppm},
#' \code{rt}, \code{sec} and \code{id} are used to construct the targets.
#' See ?\link{makeTargets} for more information.
#'
#' @export
#'
#' @importFrom data.table rbindlist
#'
#' @aliases XICs,msAnalysis,msAnalysis-method
#'
setMethod("XICs", "msAnalysis", function(object,
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 60, id = NULL) {

  targets <- makeTargets(mz, rt, ppm, sec, id)

  rtr <- c(min(targets$rtmin) * 0.8, max(targets$rtmax) * 1.2)
  if (rtr[2] == 0) rtr = NULL

  if (!hasLoadedSpectra(object)) {
    spec <- loadRawDataMZR(file = filePaths(object),
                           chroms = FALSE, level = 1, rtr = rtr)
    spec <- spec$spectra
  } else {
    spec <- spectra(object)
    spec <- spec[lv == 1, ]
    spec <- spec[, .(index, scan, lv, rt, mz, intensity)]
  }

  spec <- list(as.data.frame(spec))
  names(spec) <- analysisNames(object)

  targets$analysis <- analysisNames(object)

  xic <- rcpp_extract_xics(spec = spec, targets = targets)

  xic <- rbindlist(xic)

  return(xic)
})


### plotXICs -------------------------------------------------------------------

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
#' rows to plot multiple targets can be defined by the \code{numberRows}
#' argument.
#'
#' @template args_plots_xics
#'
#' @export
#'
#' @importFrom data.table is.data.table
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

  xic <- XICs(object, mz, ppm, rt, sec, id)

  plot <- plotXICs(xic,
   legendNames = legendNames,
   plotTargetMark = plotTargetMark,
   targetsMark = targetsMark,
   ppmMark = ppmMark,
   secMark = secMark,
   numberRows = numberRows
  )

  return(plot)
})

### MS2s -----------------------------------------------------------------

#' @describeIn msAnalysis get MS2 data for specified \emph{m/z} and retention time (seconds) targets.
#' The \code{clusteringUnit} defines the method used for clustering.
#' Possible values are \emph{euclidean} (the default) or \emph{distance}.
#' The mass (in Da) and time (in seconds) isolation windows to screen for the respective precursors
#' are defined with the arguments \code{isolationMassWindow} and \code{isolationTimeWindow}, respectively.
#' The \code{clusteringUnit} and \code{clusteringWindow} define
#' the mass deviation unit and deviation to cluster mass traces from different spectra, respectively.
#' For the \code{clusteringUnit}, possible values are \emph{mz} (the default) or \emph{ppm}.
#' The \code{minIntensityPre} and \code{minIntensityPost}
#' define the minimum intensity for mass traces before and after clustering, respectively.
#' Set \code{mergeVoltages} to \code{TRUE} for merging spectra acquired with different collision energies.
#' The \code{mergeBy} argument is used to merge spectra by "samples" or "replicateNames".
#' When \code{NULL}, MS2 is given per target and per sample.
#'
#' @template args-single-settings
#'
#' @export
#'
#' @aliases MS2s,msAnalysis,msAnalysis-method
#'
setMethod("MS2s", "msAnalysis", function(object = NULL,
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 60, id = NULL,
                                         settings = NULL) {

  level <- 2
  return(
    extractMSn(object, analyses = NULL, level, mz, ppm, rt, sec, id, settings)
  )
})

### plotMS2s -------------------------------------------------------------

#' @describeIn msAnalysis plots MS2 data for specified \emph{m/z} and retention time (seconds) targets
#' in analyses of an \linkS4class{msData} object. The \code{clusteringUnit} defines the method used for clustering.
#' Possible values are \emph{euclidean} (the default) or \emph{distance}.
#' The mass (in Da) and time (in seconds) isolation windows to screen for the respective precursors
#' are defined with the arguments \code{isolationMassWindow} and \code{isolationTimeWindow}, respectively.
#' The \code{clusteringUnit} and \code{clusteringWindow} define
#' the mass deviation unit and deviation to cluster mass traces from different spectra, respectively.
#' For the \code{clusteringUnit}, possible values are \emph{mz} (the default) or \emph{ppm}.
#' The \code{minIntensityPre} and \code{minIntensityPost}
#' define the minimum intensity for mass traces before and after clustering, respectively.
#' Set \code{mergeVoltages} to \code{TRUE} for merging spectra acquired with different collision energies.
#' The \code{mergeBy} argument is used to merge spectra by "analyses" or "replicates".
#' When \code{NULL}, MS2 is given per target and per sample. The possible values for the
#' \code{colorBy} argument are "targets" and "voltages" to color by
#' each target, sample, replicate or collision energy, respectively.
#'
#' @param colorBy A string defining the color method for plotting.
#'
#' @export
#'
#' @aliases plotMS2s,msAnalysis,msAnalysis-method
#'
setMethod("plotMS2s", "msAnalysis", function(object = NULL,
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 60, id = NULL,
                                         settings = NULL,
                                         legendNames = NULL,
                                         title = NULL,
                                         colorBy = "targets",
                                         interactive = FALSE) {

  level <- 2
  ms2 <- extractMSn(object, analyses = NULL, level, mz, ppm, rt, sec, id, settings)
  if (nrow(ms2) < 1) return(cat("Data was not found for any of the targets!"))
  return(
    plotMS2s(ms2, legendNames = legendNames, title = title,
             colorBy = colorBy, interactive = interactive
    )
  )
})

### hasAdjustedRetentionTime ---------------------------------------------

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

### addParameters ----------------------------------------------------------

#' @describeIn msAnalysis adds processing parameters to the analysis.
#'
#' @export
#'
#' @importFrom checkmate testClass
#'
#' @aliases addParameters,msAnalysis,msAnalysis-method
#'
setMethod("addParameters", "msAnalysis", function(object,
                                                  settings) {

  valid <- testClass(settings, "settings")

  if (!valid) {
    warning("Arguments not correct, returning original object!")
    return(object)
  }

  object@parameters[[settings@call]] <- settings

  return(object)
})


### getParameters ----------------------------------------------------------

#' @describeIn msAnalysis gets processing parameters in the analysis.
#'
#' @param call The call name of the settings to retrieve.
#'
#' @export
#'
#' @aliases getParameters,msAnalysis,msAnalysis-method
#'
setMethod("getParameters", "msAnalysis", function(object, call = NULL) {

  if (is.null(call)) {
    param <- list(object@parameters)
  } else {
    param <- list(object@parameters[[call]])
  }

  names(param) <- analysisNames(object)

  return(param)
})

### hasPeaks -----------------------------------------------------

#' @describeIn msAnalysis check if the \linkS4class{msAnalysis} has peaks.
#'
#' @export
#'
#' @aliases hasPeaks,msAnalysis,msAnalysis-method
#'
setMethod("hasPeaks", "msAnalysis", function(object) {

  return(nrow(object@peaks) > 0)
})

### peaks ----------------------------------------------------------------

#' @describeIn msAnalysis getter for chromatographic peaks.
#' The arguments \code{targetID} and \code{mz}/\code{rt} can be used
#' to select specific peaks. The \emph{id} of peaks and/or features can be
#' given in the \code{targetsID} argument to select the respective peaks.
#'
#' @template args-single-targetsID
#' @template args-single-filtered
#'
#' @export
#'
#' @importFrom dplyr between
#'
#' @aliases peaks,msAnalysis,msAnalysis-method
#'
setMethod("peaks", "msAnalysis", function(object,
                                          targetsID = NULL,
                                          mz = NULL, ppm = 20,
                                          rt = NULL, sec = 60,
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


### plotPeaks ------------------------------------------------------------------------------------------------

#' @describeIn msAnalysis a method for plotting chromatographic peaks
#' in an \linkS4class{msAnalysis} object.
#' The arguments \code{targetID} and \code{mz}/\code{rt} can be used
#' to select specific peaks. The \emph{id} of peaks and/or features can be
#' given in the \code{targetsID} argument to select the respective peaks.
#' The \code{legendNames} is a character vector with the same length as targets for plotting and
#' can be used to legend the plot.
#'
#' @export
#'
#' @importMethodsFrom xcms plotPeaks
#' @importFrom data.table rbindlist copy
#'
#' @aliases plotPeaks,msAnalysis,msAnalysis-method
#'
setMethod("plotPeaks", "msAnalysis", function(object,
                                              targetsID = NULL,
                                              mz = NULL, ppm = 20,
                                              rt = NULL, sec = 30,
                                              filtered = TRUE,
                                              legendNames = NULL,
                                              title = NULL,
                                              interactive = FALSE) {

  colorBy = "targets"

  peaks <- peaks(object, targetsID, mz, ppm, rt, sec, filtered)

  pks_tars <- copy(peaks[, .(id, mz, rt, mzmin, mzmax, rtmin, rtmax)])
  pks_tars$rtmin <- min(pks_tars$rtmin) - 60
  pks_tars$rtmax <- max(pks_tars$rtmax) + 60

  eic <- EICs(object, mz = pks_tars)

  return(
    plotPeaks(eic, peaks, analyses = NULL, colorBy = "targets",
      legendNames = legendNames,
      title = title,
      interactive = interactive
    )
  )
})

### mapPeaks ------------------------------------------------------------------------------------------------

#' @describeIn msAnalysis a method for mapping peaks mass and time space.
#' The \code{legendNames} is a character vector with the same length as targets for plotting and
#' can be used to legend the plot.
#'
#' @param xlim A length one or two numeric vector for setting the \emph{x} limits of a plot.
#' @param ylim A length one or two numeric vector for setting the \emph{y} limits of a plot.
#'
#' @export
#'
#' @aliases mapPeaks,msAnalysis,msAnalysis-method
#'
setMethod("mapPeaks", "msAnalysis", function(object,
                                             targetsID = NULL,
                                             mz = NULL, ppm = 20,
                                             rt = NULL, sec = 30,
                                             filtered = TRUE,
                                             legendNames = NULL,
                                             xlim = 30,
                                             ylim = 0.05,
                                             title = NULL) {

  colorBy = "targets"

  peaks <- peaks(
    object,
    targetsID,
    mz, ppm,
    rt, sec,
    filtered
  )

  if (nrow(pks) < 1) return(cat("Requested peaks were not found!"))

  if (!is.null(legendNames) & length(legendNames) == length(unique(peaks$id))) {
    leg <- legendNames
    names(leg) <- unique(pks$id)
    varkey <- sapply(pks$id, function(x) leg[x])
  } else {
    leg <- paste0(peaks$id, " - ", round(pks$mz, digits = 4), "/", round(peaks$rt, digits = 0))
    names(leg) <- peaks$id
    varkey <- sapply(peaks$id, function(x) leg[names(leg) == x])
  }

  peaks[, var := varkey][]

  plot <- mapPeaksInteractive(peaks, xlim, ylim, title)

  return(plot)
})

#### [ sub-setting peaks ----------------------------------------------

#' @describeIn msAnalysis subset on peaks, using peak index or name.
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

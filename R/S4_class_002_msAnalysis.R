

#### validity ------------------------------------------------------------

msAnalysis_validity <- function(object) {

  valid <- TRUE

  if (nrow(object@peaks) > 0) {
    must_have_names <- c("id", "rt", "mz", "intensity", "area", "drt", "rtmin", "rtmax", "dppm", "mzmin", "mzmax")
    valid <- !FALSE %in% must_have_names %in% colnames(object@peaks)
  }

  return(valid)
}


### msAnalysis ----------------------------------------------------------------------------------------------

#' msAnalysis-class
#'
#' @description An S4 class representing an MS sample/file within the \pkg{streamFind} package.
#' The \code{msAnalysis} is used to store and manage MS data and the respective methods can be used
#' for inspection, processing and evaluation.
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
    spectra = data.table::data.table(),
    chromatograms = data.table::data.table(),
    parameters = list(),
    peaks = data.table::data.table()
  ),
  validity = msAnalysis_validity
)


### S4 methods ----------------------------------------------------------------------------------------------

#### initialize ----------------------------------------------------------

#' @describeIn msAnalysis initializes an \linkS4class{msAnalysis} object.
#'
#' @usage ## S4 method to initialize an "msAnalysis" object \cr
#' \code{new("msAnalysis", file)}
#'
#' @param file A character string with the complete file path.
#' @param replicate optional character string for analysis replicate name
#' @param blank optional character string for associated blank replicate name
#'
#' @importFrom data.table as.data.table
#' @importFrom stringr str_replace
#' @importFrom mzR openMSfile header runInfo chromatogramHeader
#'
#' @export
#'
setMethod("initialize", "msAnalysis", function(.Object, ...) {

  .Object <- callNextMethod()

  if (is.na(.Object@file)) {
    warning("A file was not given to create the msAnalysis object!")
    return(.Object)

  } else {

    fl <- .Object@file
    msf <- mzR::openMSfile(fl, backend = "pwiz")

    hd <- data.table::as.data.table(mzR::header(msf))
    rInfo <- mzR::runInfo(msf)
    acInfo <- mzR::instrumentInfo(msf)

    .Object@analysis <- gsub(".mzML|.mzXML", "", basename(fl))

    if (is.na(.Object@replicate)) {
      .Object@replicate <- .Object@analysis
      .Object@replicate <- stringr::str_replace(.Object@replicate, "-", "_")
      .Object@replicate <- sub("_[^_]+$", "", .Object@replicate)
    }

    if (nrow(hd) > 0) {
      polarity <- unique(hd$polarity)
      if (length(polarity) == 1) {
        if (polarity == 1) .Object@metadata$polarity <- "positive"
        if (polarity == 0) .Object@metadata$polarity <- "negative"
      } else if (0 %in% polarity | 1 %in% polarity) {
        .Object@metadata$polarity <- "both"
      } else {
        .Object@metadata$polarity <- NA_character_
      }
      .Object@metadata$centroided <- TRUE %in% hd$centroided
      .Object@metadata <- c(.Object@metadata, rInfo, acInfo)
    }

    suppressWarnings(mzR::close(msf))

    return(.Object)
  }

})


#### show ----------------------------------------------------------------

#' @describeIn msAnalysis prints the details of an \linkS4class{msAnalysis} object.
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
    "  Levels        ", object@metadata$msLevels, " \n",
    "  Centroided    ", object@metadata$centroided, "\n",
    "  Spectra       ", nrow(object@spectra), "\n",
    "  Chromatograms ", nrow(object@chromatograms), "\n",
    "  Peaks         ", nrow(object@peaks), "\n",
    "  Parameters: \n",
    sep = ""
  )
  if (length(object@parameters) > 0) {
    for (i in seq_len(length(object@parameters))) {
      cat("     ", names(object@parameters)[i], ": ", object@parameters[[i]]@algorithm,  "\n", sep = "")
    }
  } else {
    cat("     n.a.", "\n", sep = "")
  }

})

#### files -----------------------------------------------------------

#' @describeIn msAnalysis getter for analysis file path.
#'
#' @export
#'
#' @aliases files,msAnalysis,msAnalysis-method
#'
setMethod("files", "msAnalysis", function(object) {
  fl <- object@file
  names(fl) <- analyses(object)
  return(fl)
})

#### analysisInfo -------------------------------------------------------

#' @describeIn msAnalysis getter for analysis info as \link{data.frame} with
#' four columns: path, analysis, group and blank. The \link{data.frame}
#' can be used as analysisInfo in \pkg{patRoon}.
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

#### analysisTable -------------------------------------------------------

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

#### analyses ------------------------------------------------------------

#' @describeIn msAnalysis getter for analysis name.
#'
#' @export
#'
#' @importMethodsFrom patRoon analyses
#'
#' @aliases analyses,msAnalysis,msAnalysis-method
#'
setMethod("analyses", "msAnalysis", function(obj) {
  ana <- obj@analysis
  names(ana) <- ana
  return(ana)

})

#### replicates ----------------------------------------------------------

#' @describeIn msAnalysis getter for the replicate name.
#'
#' @export
#'
#' @aliases replicates,msAnalysis,msAnalysis-method
#'
setMethod("replicates", "msAnalysis", function(object) {
  rep <- object@replicate
  names(rep) <- analyses(object)
  return(rep)
})

#### replicates<- --------------------------------------------------------

#' @describeIn msAnalysis setter for analysis replicate name.
#'  The \code{value} is a character string with analysis replicate name.
#'
#' @param value A character string applicable to the respective method.
#'
#' @export
#'
#' @aliases replicates<-,msAnalysis,msAnalysis-method
#'
setMethod("replicates<-", signature("msAnalysis", "ANY"), function(object, value) {

  if (length(value) != 1) {
    warning("Length of value must be one.")
    return(object)
  }
  object@replicate <- unname(value)
  return(object)
})

#### blanks --------------------------------------------------------------

#' @describeIn msAnalysis getter for associated blank replicate name.
#'
#' @export
#'
#' @aliases blanks,msAnalysis,msAnalysis-method
#'
setMethod("blanks", "msAnalysis", function(object) {
  blk <- object@blank
  names(blk) <- analyses(object)
  return(blk)
})

#### blanks<- ------------------------------------------------------------

#' @describeIn msAnalysis setter for associated blank replicate.
#' The \code{value} is a character string with associated blank replicate name.
#'
#' @param value A character string applicable to the respective method.
#'
#' @export
#'
#' @aliases blanks<-,msAnalysis,msAnalysis-method
#'
setMethod("blanks<-", signature("msAnalysis", "ANY"), function(object, value) {

  if (length(value) != 1) {
    warning("Length of value must be one.")
    return(object)
  }
  object@blank <- unname(value)
  return(object)
})

#### metadata ---------------------------------------------------------

#' @describeIn msAnalysis getter for analysis metadata.
#'  Returns a \link[data.table]{date.table} with a column per metadata entry.
#'
#' @template args-single-which-entry
#'
#' @export
#'
#' @importFrom data.table rbindlist
#'
#' @aliases metadata,msAnalysis,msAnalysis-method
#'
setMethod("metadata", "msAnalysis", function(object, which = NULL) {

  if (!is.null(which)) {
    mtd_a <- c(list(analysis = object@analysis), object@metadata[which])
  } else {
    mtd_a <- c(list(analysis = object@analysis), object@metadata)
  }

  if ("msLevels" %in% names(mtd_a)) mtd_a$msLevels <- paste(sort(mtd_a$msLevels), collapse = "; ")

  mtd_a <- data.table::as.data.table(mtd_a)

  return(mtd_a)
})

#### polarities ----------------------------------------------------------

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


### EICs -----------------------------------------------------------------

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
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 60, id = NULL) {

  return(extractEICs(object, analyses = NULL, mz, ppm, rt, sec, id))
})


### plotEICs -------------------------------------------------------------

#' @describeIn msAnalysis A method for plotting extracted ion chromatograms (EICs)
#' of data in the analysis file.
#' The arguments for data collection are the same as the \link{EICs} method.
#' A \link[data.table]{date.table} can be used instead.
#' The \code{legendNames} is a character vector with the same length as targets for plotting and
#' can be used to legend the plot. Note, the plot legends the data by target.
#'
#' @template args-makeTargets
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

  eic <- extractEICs(object, analyses, mz, rt, ppm, id)

  return(plotEICs(eic, analyses = NULL, colorBy = "targets", legendNames, title, interactive))
})


### TICs -----------------------------------------------------------------

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

  file <- files(object)

  tic <- loadRawDataMZR(file, spectra = FALSE, chroms = TRUE, chromsID = "TIC")[[1]]

  if (nrow(tic) < 1) {
    tic <- extractEICs(object, analyses = NULL, mz = NULL, rt = NULL)
    tic <- tic[, .(id, rt, intensity)]
  } else {
    tic <- tic[, .(id, rt, intensity)]
  }
  tic[, `:=`(analysis = analyses(object), replicate = replicates(object))]
  setcolorder(tic, c("analysis", "replicate", "id", "rt", "intensity"))
  if (max(tic$rt) < 120) tic[, rt := rt * 60]
  tic <- tic[intensity > 0, ]

  return(tic)
})


### plotTICs -------------------------------------------------------------

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

  return(plotTICs(tics, analyses = NULL, colorBy = "analyses", title = title, interactive = interactive))
})


### XICs -----------------------------------------------------------------

#' @describeIn msAnalysis get three dimensional (\emph{m/z}, time and intensity)
#' extracted ion chromatograms (XICs) for specified \emph{m/z} and retention time pair targets
#' in analysis. The arguments \code{mz}, \code{ppm}, \code{rt}, \code{sec} and \code{id}
#' are used to construct the targets. See ?\link{makeTargets} for more information.
#'
#' @export
#'
#' @aliases XICs,msAnalysis,msAnalysis-method
#'
setMethod("XICs", "msAnalysis", function(object,
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 60, id = NULL) {

  xic <- extractXICs(object, analyses = NULL, mz, ppm, rt, sec, id)

  return(xic)
})


### plotXICs -------------------------------------------------------------

#' @describeIn msAnalysis plots three dimensional (\emph{m/z}, time and intensity)
#' extracted ion chromatograms (XICs) for specified \emph{m/z} and retention time pair targets
#' in analyses of an \linkS4class{msAnalysis} object. The arguments \code{mz}, \code{ppm}, \code{rt},
#' \code{sec} and \code{id} are used to construct the targets. See ?\link{makeTargets} for more information.
#' When \code{plotTargetMark} is \code{TRUE} a target is plotted representing the deviations as defined
#' by the arguments \code{ppmMark} and \code{secMark} in ppm and seconds, respectively.
#' When ranges were given to build the XIC, exact \emph{m/z} and time targets can be specified with
#' the argument \code{targetsMark}. \code{targetsMark} should be a two column table named mz and rt with
#' exact \emph{m/z} and time targets. Note that the number of rows should be the same as the number of target
#' in the XIC. The number of rows to plot multiple targets can be defined by the \code{numberRows} argument.
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

  xic <- extractXICs(object, analyses, mz, ppm, rt, sec, id)

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
#' The \code{mergeBy} argument is used to merge spectra by "samples" or "replicates".
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
  return(extractMSn(object, analyses = NULL, level, mz, ppm, rt, sec, id, settings))
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
#' The \code{mergeBy} argument is used to merge spectra by "samples" or "replicates".
#' When \code{NULL}, MS2 is given per target and per sample. The possible values for the
#' \code{colorBy} argument are "targets" and "voltages" to color by
#' each target, sample, replicate or collision energy, respectively.
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

### loadRawData ----------------------------------------------------------

#' @describeIn msAnalysis adds raw spectra and chromatograms to the respective
#' slots of an \linkS4class{msAnalysis} object.
#'
#' @export
#'
#' @aliases loadRawData,msAnalysis,msAnalysis-method
#'
setMethod("loadRawData", "msAnalysis", function(object) {

  lvs <- metadata(object, "msLevels")$msLevels
  lvs <- as.numeric(strsplit(lvs, "; ", fixed = TRUE)[[1]])

  rd_list <- loadRawDataMZR(files(object),
                            spectra = TRUE,
                            level = lvs,
                            rtr = NULL,
                            chroms = TRUE,
                            chromsID = NULL,
                            ifChromNoSpectra = FALSE)

  if ("spectra" %in% names(rd_list)) object@spectra <- rd_list$spectra
  if ("chroms" %in% names(rd_list)) object@chromatograms <- rd_list$chroms

  return(object)
})

### spectra ----------------------------------------------------------

#' @describeIn msAnalysis getter for slot spectra in the \linkS4class{msAnalysis}.
#'
#' @export
#'
#' @aliases spectra,msAnalysis,msAnalysis-method
#'
setMethod("spectra", "msAnalysis", function(obj) {

  return(obj@spectra)
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
#' @aliases addParameters,msAnalysis,msAnalysis-method
#'
setMethod("addParameters", "msAnalysis", function(object,
                                                  settings) {

  valid <- checkmate::testClass(settings, "settings")

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

  names(param) <- analyses(object)

  return(param)
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

  pks <- peaks(object, targetsID, mz, ppm, rt, sec, filtered)

  pks_tars <- copy(pks[, .(id, mz, rt, mzmin, mzmax, rtmin, rtmax)])
  pks_tars$rtmin <- min(pks_tars$rtmin) - 60
  pks_tars$rtmax <- max(pks_tars$rtmax) + 60

  eic <- EICs(object, mz = pks_tars)

  return(
    plotPeaks(eic, pks, analyses = NULL, colorBy = "targets",
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

  pks <- peaks(
    object,
    targetsID,
    mz, ppm,
    rt, sec,
    filtered
  )

  if (nrow(pks) < 1) return(cat("Requested peaks were not found!"))

  if (!is.null(legendNames) & length(legendNames) == length(unique(pks$id))) {
    leg <- legendNames
    names(leg) <- unique(pks$id)
    varkey <- sapply(pks$id, function(x) leg[x])
  } else {
    leg <- paste0(pks$id, " - ", round(pks$mz, digits = 4), "/", round(pks$rt, digits = 0))
    names(leg) <- pks$id
    varkey <- sapply(pks$id, function(x) leg[names(leg) == x])
  }

  pks[, var := varkey][]

  plot <- mapPeaksInteractive(pks, xlim, ylim, title)

  return(plot)
})

#### [ sub-setting peaks ----------------------------------------------

#' @describeIn msAnalysis subset on peaks, using peak index or name.
#'
#' @param i The indice/s or name/s of the peaks to keep in the \code{x} object.
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

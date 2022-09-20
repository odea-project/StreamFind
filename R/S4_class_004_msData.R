

#### validity ------------------------------------------------------------

msData_validity <- function(object) {

  valid <- TRUE

  if (!all(sapply(object@analyses, is) %in% "msAnalysis")) {
    warning("Analysis objects must be as msAnalysis S4 class objects!")
    valid <- FALSE
  }

  #validates each msAnalysis, if they have peaks data.table
  if (!all(sapply(object@analyses, function(x) validObject(x)))) {
    valid <- FALSE
  }

  blks <- na.omit(blankReplicateNames(object))
  if (length(blks) > 0) {
    if (FALSE %in% (blks %in% replicateNames(object))) {
      warning("Blank replicates not present in analyses set!")
      valid <- FALSE
    }
  }

  # TODO validation for features classes, including the analysis/replicates and blanks

  return(valid)
}

### msData ------------------------------------------------------------------------------------------------

#' @title msData
#'
#' @description An S4 class object to store and manage processing of files with
#' MS data. The \code{msData} object inherits the \linkS4class{streamSet}
#' structure with type defined as \emph{ms}.
#'
#' @template slot-streamSet
#' @template slot-msData
#'
#' @export
#'
#' @md
setClass("msData",
  slots = c(
    features = "msFeatures"
  ),
  contains = "streamSet",
  prototype = list(
    features = new("msFeatures")
  ),
  validity = msData_validity
)

### S4 methods -----------------------------------------------------------------

#### show ----------------------------------------------------------------

#' @describeIn msData prints the details of an \linkS4class{msData} object.
#'
#' @template args-single-object-msData
#'
#' @export
#'
setMethod("show", "msData", function(object) {

  cat(
    "  Class         ", paste(is(object), collapse = "; "), "\n",
    "  Title         ", object@title, "\n",
    "  Date          ", as.character(object@date), "\n",
    "  Path          ", object@path, "\n",
    sep = ""
  )
  if (length(object@analyses) > 0) {
    tb <- data.table(
      analysis = analysisNames(object),
      replicate = sapply(object@analyses, function(x) x@replicate),
      blank = sapply(object@analyses, function(x) x@blank),
      class = sapply(object@analyses, function(x) is(x))
    )

    tb$traces <- sapply(object@analyses, function(x) nrow(x@spectra))
    tb$peaks <- sapply(object@analyses, function(x) nrow(x@peaks))

    if (nrow(object@features@metadata) > 0) {
      tb$features <- apply(
        object@features@intensity[, .SD, .SDcols = analysisNames(object)], 2,
        function(x) length(x[x > 0])
      )
    } else {
      tb$features <- 0
    }

    print(tb)

  } else {
    cat("     n.a.", "\n", sep = "")
  }

})

#### analysisInfo -------------------------------------------------------

#' @describeIn msData getter for analysis info as \link{data.frame} with
#' four columns: path, analysis, group and blank. The \link{data.frame}
#' can be used as analysisInfo in \pkg{patRoon}.
#'
#' @param obj A \linkS4class{msData} object.
#'
#' @export
#'
#' @aliases analysisInfo,msData,msData-method
#'
setMethod("analysisInfo", "msData", function(obj) {
  temp <- data.frame(
    "path" = sapply(obj@analyses, function(x) dirname(x@file)),
    "analysis" = sapply(obj@analyses, function(x) x@analysis),
    "group" = sapply(obj@analyses, function(x) x@replicate),
    "blank" = sapply(obj@analyses, function(x) x@blank),
    "class" = sapply(obj@analyses, function(x) is(x)),
    "file" = sapply(obj@analyses, function(x) x@file))

  rownames(temp) <- seq_len(nrow(temp))
  return(temp)
})

#### analysisTable -------------------------------------------------------

#' @describeIn msData getter for analysis table as \link{data.table} with
#' four columns: file, analysis, replicate and blank.
#'
#' @export
#'
#' @aliases analysisTable,msData,msData-method
#'
setMethod("analysisTable", "msData", function(object) {
  temp <- data.table(
    "file" = sapply(object@analyses, function(x) x@file),
    "analysis" = sapply(object@analyses, function(x) x@analysis),
    "replicate" = sapply(object@analyses, function(x) x@replicate),
    "blank" = sapply(object@analyses, function(x) x@blank)
  )
  rownames(temp) <- seq_len(nrow(temp))
  return(temp)
})

#### filePaths -----------------------------------------------------------

#' @describeIn msData getter for analysis file paths.
#'
#' @export
#'
#' @aliases filePaths,msData,msData-method
#'
setMethod("filePaths", "msData", function(object) sapply(object@analyses, function(x) x@file))

#### analysisNames ------------------------------------------------------------

#' @describeIn msData getter for analysis names.
#'
#' @export
#'
#' @aliases analysisNames,msData,msData-method
#'
setMethod("analysisNames", "msData", function(object) sapply(object@analyses, function(x) x@analysis))

#### replicateNames ----------------------------------------------------------

#' @describeIn msData getter for replicate names.
#'
#' @export
#'
#' @aliases replicateNames,msData,msData-method
#'
setMethod("replicateNames", "msData", function(object) sapply(object@analyses, function(x) x@replicate))

#### replicateNames<- --------------------------------------------------------

#' @describeIn msData setter for analysis replicate names.
#' The \code{value} is a character vector with the same length as
#' the number of analyses in the \code{object},
#' containing analysis replicate name for each analysis.
#'
#' @param value A character vector applicable to the respective method.
#'
#' @export
#'
#' @aliases replicateNames<-,msData,msData-method
#'
setMethod("replicateNames<-", signature("msData", "ANY"), function(object, value) {

  ana <- analysisNames(object)
  if (length(value) != length(ana)) {
    warning("Length of value does not match the number of analyses.")
    return(object)
  }

  names(value) <- ana
  for (a in ana) object@analyses[[a]]@replicate <- unname(value[a])
  return(object)
})

#### blankReplicateNames --------------------------------------------------------------

#' @describeIn msData getter for blank names.
#'
#' @export
#'
#' @aliases blankReplicateNames,msData,msData-method
#'
setMethod("blankReplicateNames", "msData", function(object) sapply(object@analyses, function(x) x@blank))

#### blankReplicateNames<- ------------------------------------------------------------

#' @describeIn msData setter for associated blank replicate for each analyses.
#' The \code{value} is a character vector with the same length as
#' the number of analyses in the \code{object},
#' containing the associated blank replicate name of each analysis.
#'
#' @param value A character vector applicable to the respective method.
#'
#' @export
#'
#' @aliases blankReplicateNames<-,msData,msData-method
#'
setMethod("blankReplicateNames<-", signature("msData", "ANY"), function(object, value) {

  ana <- analysisNames(object)
  if (length(value) != length(ana)) {
    warning("Length of value does not match the number of analyses.")
    return(object)
  }

  names(value) <- ana
  for (a in ana) object@analyses[[a]]@blank <- unname(value[a])
  return(object)
})

#### getMetadata ---------------------------------------------------------------

#' @describeIn msData getter for analyses metadata.
#' Returns a nested list for each analysis (as defined by the \code{analyses})
#' with the list of metadata entries as defined by the \code{which} argument.
#' When \code{which} is \code{NULL}, all entries are returned.
#'
#' @template args-single-which-entry
#'
#' @export
#'
#' @aliases getMetadata,msData,msData-method
#'
setMethod("getMetadata", "msData", function(object, analyses = NULL, which = NULL) {

  if (!is.null(analyses)) object <- object[analyses]

  mtd <- lapply(object@analyses, function(z, which) {
    return(getMetadata(z, which))
  }, which = which)

  return(mtd)
})

#### addMetadata ---------------------------------------------------------

#' @describeIn msData setter for analyses metadata.
#'
#' @param metadata A list with a named vector of metadata for each analyses
#' in the \linkS4class{msData} object or a \code{data.frame} or
#' \code{data.table} with metadata added as columns and with the number of
#' row equal to the number of analyses in the \linkS4class{msData} object.
#' @param overwrite Logical, set to \code{TRUE} to overwrite.
#'
#' @export
#'
#' @aliases addMetadata,msData,msData-method
#'
setMethod("addMetadata", "msData", function(object,
                                            metadata = NULL,
                                            overwrite = FALSE) {

  if (is.data.frame(metadata) | is.data.table(metadata)) {

    if (!"analysis" %in% colnames(metadata)) metdata$analysis <- analysisNames(object)

    name_is_already_there <- lapply(metadata$analysis, function(x, metadata, object) {
      names(metadata[metadata$analysis %in% x, !colnames(metadata) %in% "analysis"]) %in% names(object@analyses[[x]]@metadata)
    }, metadata = metadata, object = object)
    names(name_is_already_there) <- metadata$analysis

    metadata <- split(metadata, metadata$analysis)
    metadata <- lapply(metadata, function(x) return(x[, !colnames(x) %in% "analysis"]))

  } else if (is.list(metadata)) {

    if (length(metadata) != length(object@analyses)) {
      warning("Metadata list must be the same length as the number of analyses!")
      return(object)
    }

    if (all(sapply(metadata, function(x) is.null(names(x))))) {
      warning("Metadata must be a named vector named!")
      return(object)
    }

    if (TRUE %in% is.null(names(metadata))) names(metadata) <- analysisNames(object)

    name_is_already_there <- lapply(names(metadata), function(x, metadata, object) {
      names(metadata[[x]]) %in% names(object@analyses[[x]]@metadata)
    }, metadata = metadata, object = object)
    names(name_is_already_there) <- names(metadata)

  }

  if (exists("name_is_already_there")) {

    if (TRUE %in% unlist(name_is_already_there) & !overwrite) {
      warning("Metadata name/s already exist/s and overwrite is not allowed!")
      return(object)
    }

    for (ana in names(metadata)) {
      if (TRUE %in% name_is_already_there[[ana]]) {
        mtd <- as.list(metadata[[ana]])
        object@analyses[[ana]]@metadata[names(object@analyses[[ana]]@metadata) %in% names(mtd)] <- mtd[name_is_already_there[[ana]]]
        object@analyses[[ana]]@metadata <- c(object@analyses[[ana]]@metadata, mtd[!name_is_already_there[[ana]]])
      } else {
        mtd <- as.list(metadata[[ana]])
        object@analyses[[ana]]@metadata <- c(object@analyses[[ana]]@metadata, mtd)
      }
    }
    return(object)
  }

  return(object)
})

#### addAnalyses ---------------------------------------------------------

#' @describeIn msData adds \linkS4class{msAnalysis} objects to the existing
#' \linkS4class{msData}.
#'
#' @param analysisList A list of \linkS4class{msAnalysis} objects.
#'
#' @export
#'
#' @aliases addAnalyses,msData,msData-method
#'
setMethod("addAnalyses", "msData", function(object, analysisList = NULL) {

  if (!is.null(analysisList)) {

    cls <- NA_character_

    if (length(analysisList) == 1) {
      cls <- class(analysisList)
    } else {
      cls <- sapply(analysisList, function(x) class(x))
    }

    if (all(cls %in% "msAnalysis")) {

      if (!is.list(analysisList)) {
        analysisList <- list(analysisList)
        names(analysisList) <- analysisNames(analysisList)
      } else {
        names(analysisList) <- sapply(analysisList, function(x) analysisNames(x))
      }

      #check name
      if (TRUE %in% (names(analysisList) %in% analysisNames(object))) {
        warning("Analysis name/s to add is already in msData!")
        return(object)
      }

      object@analyses <- c(object@analyses, analysisList)
      object@analyses <-  object@analyses[order(names(object@analyses))]

      object@features <- new("msFeatures")

      object@analyses <- lapply(object@analyses, function(x) {
        if (nrow(x@peaks) > 0) {
          x@peaks$feature <- NA_character_
        }
        return(x)
      })

      return(object)
    }
  }

  warning("No msAnalysis object given to add!")
  return(object)
})

#### getAnalyses ---------------------------------------------------------

#' @describeIn msData getter for analysis names.
#'
#' @template args-single-analyses
#'
#' @export
#'
#' @aliases getAnalyses,msData,msData-method
#'
setMethod("getAnalyses", "msData", function(object, analyses = NULL) {

  if (!is.null(analyses)) {
    if (is.character(analyses)) {
      analyses <- which(analyses == analysisNames(object))
    }
    if (length(analyses) > 0 && is.numeric(analyses)) {
      if (length(analyses) == 1) return(object@analyses[[analyses]])
      return(object@analyses[analyses])
    }
  }
  warning("Analysis not specified or not found in object!")
  return(list())
})

#### [ sub-setting analyses ----------------------------------------------

#' @describeIn msData subset on analyses, using index or analyses name.
#'
#' @template args-single-i-subsetting
#'
#' @export
#'
setMethod("[", c("msData", "ANY", "missing", "missing"), function(x, i, ...) {

  if (!missing(i)) {

    x <- callNextMethod()

    x@features <- x@features[i]

    #when has features, updates feature column in peaks slot of each analysis
    if (nrow(x@features@metadata) > 0) {
      f_id <- features(x)[["id"]]
      x@analyses <- lapply(x@analyses, function(z, f_id) {
        temp <- z@peaks
        temp[!feature %in% f_id, `:=`(feature = NA_character_, filtered = TRUE, filter = "grouping")]
        z@peaks <- copy(temp)
        return(z)
      }, f_id = f_id)
    }
    return(x)
  }

  return(x)
})

#### polarities ----------------------------------------------------------

#' @describeIn msData getter for analyses polarity.
#'
#' @export
#'
#' @aliases polarities,msData,msData-method
#'
setMethod("polarities", "msData", function(object) {
  mt <- getMetadata(object, which = "polarity")
  mt_v <- unlist(lapply(mt, function(x) x$polarity))
  return(mt_v)
})

### EICs -----------------------------------------------------------------

#' @describeIn msData get extracted ion chromatograms (EICs)
#' for specified \emph{m/z} (Da) and retention time (seconds) targets
#' in given analyses. The arguments \code{mz}, \code{ppm}, \code{rt}
#' and \code{sec} are used to construct the targets.
#' See ?\link{makeTargets} for more information.
#'
#' @template args-makeTargets
#'
#' @export
#'
#' @aliases EICs,msData,msData-method
#'
setMethod("EICs", "msData", function(object,
                                     analyses = NULL,
                                     mz = NULL, ppm = 20,
                                     rt = NULL, sec = 60, id = NULL) {

  analyses <- checkAnalysesArgument(object, analyses)

  targets <- makeTargets(mz, rt, ppm, sec, id)

  rtr <- c(min(targets$rtmin), max(targets$rtmax))
  if (rtr[2] == 0) rtr = NULL

  spec <- lapply(object@analyses[analyses], function(x, rtr) {

    if (!hasLoadedSpectra(x)) {
      temp <- msAnalysis_loadRawData(
        fl = filePaths(x),
        chroms = FALSE, levels = 1, rtr = rtr
      )
      temp <- temp$spectra
    } else {
      temp <- spectra(x)
      temp <- temp[lv == 1, ]
      temp <- temp[, .(index, scan, lv, rt, mz, intensity)]
    }
    return(temp)
  }, rtr = rtr)

  names(spec) <- analyses

  targets <- full_join(
    targets,
    data.table(
      analysis = analyses,
      replicate = replicateNames(object)[analyses]
    ),
    by = character()
  )

  # TODO add in cpp a check for returning an empty table when no traces are extracted

  eics <- rcpp_extract_eics(spec = spec, targets = targets)

  eics <- rbindlist(eics)

  eics <- eics[, .(intensity = sum(intensity)), by = c("analysis", "replicate", "id", "rt")]

  return(eics)
})

### plotEICs -------------------------------------------------------------

#' @describeIn msData a method for plotting extracted ion chromatograms (EICs)
#' of data in an \linkS4class{msData} object.
#' The arguments for data collection are the same as the \link{EICs} method.
#' A \linkS4class{data.table} can be used instead.
#' The \code{colorBy} argument can be \code{"analyses"}, \code{replicates} or \code{targets}
#' (the default), for coloring by analyses, replicates or EICs targets, respectively.
#' The \code{legendNames} is a character vector with the same length as targets for plotting and
#' can be used to legend the plot. Note that, by setting \code{legendNames} the \code{colorBy}
#' is set to "targets".
#'
#' @template args-makeTargets
#' @template args_plots_colorby_legendNames_title_interactive
#'
#' @export
#'
#' @aliases plotEICs,msData,msData-method
#'
setMethod("plotEICs", "msData", function(object,
                                         analyses = NULL,
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 30, id = NULL,
                                         colorBy = "targets",
                                         legendNames = NULL,
                                         title = NULL,
                                         interactive = FALSE) {

  eic <- EICs(object, analyses, mz, ppm, rt, sec, id)

  return(
    plotEICs(eic,
      analyses = NULL, colorBy,
      legendNames, title, interactive
    )
  )
})

### TICs -----------------------------------------------------------------

#' @describeIn msData extracts total ion chromatograms (TICs)
#' for analyses in an \linkS4class{msData} object.
#'
#' @export
#'
#' @aliases TICs,msData,msData-method
#'
setMethod("TICs", "msData", function(object, analyses = NULL) {

  analyses <- checkAnalysesArgument(object, analyses)
  if (is.null(analyses)) return(data.table())

  tics <- lapply(object@analyses[analyses], function(x) TICs(x))

  return(rbindlist(tics))
})

### plotTICs -------------------------------------------------------------

#' @describeIn msData plots a total ion chromatogram (TIC) for abalyses in the object.
#' The colorBy argument can be "analyses" or "replicates"
#' to color the plot by analyses or by analysis replicates.
#'
#' @export
#'
#' @aliases plotTICs,msData,msData-method
#'
setMethod("plotTICs", "msData", function(object,
                                         analyses = NULL,
                                         colorBy = "analyses",
                                         title = NULL,
                                         interactive = FALSE) {

  tics <- TICs(object, analyses = analyses)


  return(
    plotTICs(tics,
      analyses = NULL, colorBy = colorBy,
      title = title, interactive = interactive
    )
  )
})

### XICs -----------------------------------------------------------------

#' @describeIn msData get three dimensional (\emph{m/z}, time and intensity)
#' extracted ion chromatograms (XICs) for specified \emph{m/z} and retention time pair targets
#' in analyses of an \linkS4class{msData} object. The arguments \code{mz}, \code{ppm}, \code{rt}, \code{sec} and \code{id}
#' are used to construct the targets. See ?\link{makeTargets} for more information.
#'
#' @export
#'
#' @aliases XICs,msData,msData-method
#'
setMethod("XICs", "msData", function(object,
                                     analyses = NULL,
                                     mz = NULL, ppm = 20,
                                     rt = NULL, sec = 60, id = NULL) {

  analyses <- checkAnalysesArgument(object, analyses)

  targets <- makeTargets(mz, rt, ppm, sec, id)

  rtr <- c(min(targets$rtmin) * 0.8, max(targets$rtmax) * 1.2)
  if (rtr[2] == 0) rtr = NULL

  spec <- lapply(object@analyses[analyses], function(x, rtr) {

    if (!hasLoadedSpectra(x)) {
      temp <- msAnalysis_loadRawData(
        fl = filePaths(x),
        chroms = FALSE, level = 1, rtr = rtr
      )
      temp <- temp$spectra
    } else {
      temp <- spectra(x)
      temp <- temp[lv == 1, ]
      temp <- temp[, .(index, scan, lv, rt, mz, intensity)]
    }
    return(temp)
  }, rtr = rtr)

  names(spec) <- analyses

  targets <- full_join(
    targets,
    data.table(
      analysis = analyses,
      replicate = replicateNames(object)[analyses]
    ),
    by = character()
  )

  xic <- rcpp_extract_xics(spec = spec, targets = targets)

  xic <- rbindlist(xic)

  return(xic)
})

### plotXICs -------------------------------------------------------------

#' @describeIn msData plots three dimensional (\emph{m/z}, time and intensity)
#' extracted ion chromatograms (XICs) for specified \emph{m/z} and retention time pair targets
#' in analyses of an \linkS4class{msData} object. The arguments \code{mz}, \code{ppm}, \code{rt},
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
#' @aliases plotXICs,msData,msData-method
#'
setMethod("plotXICs", "msData", function(object,
                                         analyses = NULL,
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 60, id = NULL,
                                         legendNames = NULL,
                                         plotTargetMark = TRUE,
                                         targetsMark = NULL,
                                         ppmMark = 5,
                                         secMark = 10,
                                         numberRows = 1) {

  xic <- XICs(object, analyses, mz, ppm, rt, sec, id)

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

#' @describeIn msData get MS2 data for specified \emph{m/z} and retention time (seconds) targets
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
#' When \code{NULL}, MS2 is given per target and per sample.
#'
#' @param settings A \linkS4class{settings} S4 class object with call for \code{extractMSn}.
#'
#' @export
#'
#' @aliases MS2s,msData,msData-method
#'
setMethod("MS2s", "msData", function(object = NULL,
                                     analyses = NULL,
                                     mz = NULL, ppm = 20,
                                     rt = NULL, sec = 60, id = NULL,
                                     settings = NULL) {

  level <- 2
  return(extractMSn(object, analyses, level, mz, ppm, rt, sec, id, settings))
})

### plotMS2s -------------------------------------------------------------

#' @describeIn msData plots MS2 data for specified \emph{m/z} and retention time (seconds) targets
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
#' \code{colorBy} argument are "targets", "samples", "replicates" and "voltages" to colour by
#' each target, sample, replicate or collision energy, respectively.
#'
#' @export
#'
#' @aliases plotMS2s,msData,msData-method
#'
setMethod("plotMS2s", "msData", function(object = NULL,
                                         analyses = NULL,
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 60, id = NULL,
                                         settings = NULL,
                                         legendNames = NULL,
                                         title = NULL,
                                         colorBy = "targets",
                                         interactive = FALSE) {

  level <- 2
  ms2 <- extractMSn(object, analyses, level, mz, ppm, rt, sec, id, settings)
  if (nrow(ms2) < 1) return(cat("Data was not found for any of the targets!"))
  return(
    plotMS2s(ms2, legendNames = legendNames, title = title,
      colorBy = colorBy, interactive = interactive
    )
  )
})



### loadSpectraInfo ----------------------------------------------------------

#' @describeIn msData adds raw spectra information (i.e., scan number,
#'  ms level and retention time of each spectrum) to the slot \code{spectra}
#'  of each \linkS4class{msAnalysis} in the \linkS4class{msData}.
#'  If the levels are higher than one, as the case of MS/MS data,
#'  the collision energy and precursor scan and \emph{m/z} are also returned.
#'
#' @export
#'
#' @aliases loadSpectraInfo,msData,msData-method
#'
setMethod("loadSpectraInfo", "msData", function(object, analyses = NULL) {

  analyses <- checkAnalysesArgument(object, analyses)

  temp <- lapply(object@analyses[analyses], function(x) {
    x <- loadSpectraInfo(x)
    return(x)
  })

  object@analyses[analyses] <- temp

  return(object)
})



### loadRawData ----------------------------------------------------------

#' @describeIn msData adds raw data to all or defined analyses in the
#' \linkS4class{msData} object.
#'
#' @param minIntensityMS1 Numeric value on length one with the
#' minimum intensity of MS1 level traces.
#' @param minIntensityMS2 Numeric value on length one with the
#' minimum intensity of MS2 level traces.
#'
#' @export
#'
#' @aliases loadRawData,msData,msData-method
#'
setMethod("loadRawData", "msData", function(object,
                                            analyses = NULL,
                                            minIntensityMS1 = 0,
                                            minIntensityMS2 = 0) {

  analyses <- checkAnalysesArgument(object, analyses)

  temp <- lapply(object@analyses[analyses], function(x, minIntensityMS1, minIntensityMS2) {
    x <- loadRawData(x, minIntensityMS1, minIntensityMS2)
    return(x)
  }, minIntensityMS1 = minIntensityMS1, minIntensityMS2 = minIntensityMS2)

  object@analyses[analyses] <- temp

  return(object)
})

### hasLoadedSpectra -----------------------------------------------------------

#' @describeIn msData check if the analyses in \linkS4class{msData} have loaded raw spectra.
#'
#' @export
#'
#' @aliases hasLoadedSpectra,msData,msData-method
#'
setMethod("hasLoadedSpectra", "msData", function(object) {

  return(sapply(object@analyses, function(x) hasLoadedSpectra(x)))
})

### hasLoadedChromatograms -----------------------------------------------------------

#' @describeIn msData check if the analyses in \linkS4class{msData} have loaded raw chromatograms.
#'
#' @export
#'
#' @aliases hasLoadedChromatograms,msData,msData-method
#'
setMethod("hasLoadedChromatograms", "msData", function(object) {

  return(sapply(object@analyses, function(x) hasLoadedChromatograms(x)))
})

### spectra ----------------------------------------------------------

#' @describeIn msData getter for slot spectra in analyses of an \linkS4class{msData} object.
#'
#' @export
#'
#' @aliases spectra,msData,msData-method
#'
setMethod("spectra", "msData", function(object) {

  spec <- lapply(object@analyses, function(x) {
    return(x@spectra)
  })
  spec <- rbindlist(spec, idcol = "analysis")

  return(spec)
})



### hasAdjustedRetentionTime ---------------------------------------------------

#' @describeIn msData getter for presence of adjusted retention time
#' in the analyses.
#'
#' @export
#'
#' @aliases hasAdjustedRetentionTime,msData,msData-method
#'
setMethod("hasAdjustedRetentionTime", "msData", function(object) {

  return(sapply(object@analyses,
    function(x) "rtAdjusted" %in% colnames(x@spectra)))
})



### addParameters --------------------------------------------------------------

#' @describeIn msData adds processing parameters to analyses or features as defined
#' by the argument \code{where}. So where can be either "analyses" or "features".
#'
#' @template args-single-settings
#' @param where A character vector defining where to add the \linkS4class{settings}.
#' @template args-single-analyses
#'
#' @export
#'
#' @aliases addParameters,msData,msData-method
#'
setMethod("addParameters", "msData", function(object,
                                              settings,
                                              where = "analyses",
                                              analyses = NULL) {

  valid <- FALSE

  valid <- testClass(settings, "settings")

  valid <- testChoice(where, c("analyses", "features"))

  if (!valid) {
    warning("Arguments not correct, returning original object!")
    return(object)
  }

  if (where %in% "analyses") {

    analyses <- checkAnalysesArgument(object, analyses)

    for (ana in analyses) {
      object@analyses[[ana]]@parameters[[settings@call]] <- settings
    }
  }

  if (where %in% "features") {
    object@features@parameters[[getCall(settings)]] <- settings
  }

  return(object)
})


### getParameters ----------------------------------------------------------

#' @describeIn msData gets processing parameters from analyses or features as defined
#' by the argument \code{where}. So where can be either "analyses" or "features".
#'
#' @param where A character vector defining where to get the \linkS4class{settings}.
#' @template args-single-analyses
#' @param call The call name of the settings to retrieve.
#'
#' @export
#'
#' @aliases addParameters,msData,msData-method
#'
setMethod("getParameters", "msData", function(object,
                                              where = "analyses",
                                              analyses = NULL,
                                              call = NULL) {

  valid <- testChoice(where, c("analyses", "features"))

  if (!valid) {
    warning("Arguments not correct, returning original object!")
    return(object)
  }

  if (where %in% "analyses") {

    analyses <- checkAnalysesArgument(object, analyses)

    param <- sapply(analyses, function(x, object, call) {

      if (is.null(call)) {
        object@analyses[[x]]@parameters
      } else {
        object@analyses[[x]]@parameters[[call]]
      }
    }, object = object, call)

  }

  if (where %in% "features") {
    if (is.null(call)) {
      param <- object@features@parameters
    } else {
      param <- object@features@parameters[[call]]
    }
  }


  return(param)
})


### as.features ----------------------------------------------------------

#' @describeIn msData converts the \linkS4class{msData}
#' to a \linkS4class{features} object from the package \pkg{patRoon}.
#'
#' @export
#'
#' @aliases as.features,msData,msData-method
#'
setMethod("as.features", "msData", function(object) {

  anaInfo <- analysisInfo(object)

  feat <- lapply(object@analyses, function(x) {

    ft <- copy(x@peaks)

    if ("filtered" %in% colnames(ft)) ft <- ft[!ft$filtered, ]

    setnames(ft,
      c("id", "rt", "rtmin", "rtmax", "feature"),
      c("ID", "ret", "retmin", "retmax", "group"),
      skip_absent = TRUE
    )

    if (nrow(ft) == 0) return(ft)

    ft <- select(ft,
      ID, ret, mz, area, intensity, retmin, retmax, mzmin, mzmax,
      everything()
    )

    return(ft)
  })

  # TODO adapt for as.featuresSet when multiple polarities present

  return(new("featuresOpenMS", features = feat, analysisInfo = anaInfo))
})

### hasPeaks -----------------------------------------------------------

#' @describeIn msData check if the analyses in \linkS4class{msData} have peaks.
#'
#' @export
#'
#' @aliases hasPeaks,msData,msData-method
#'
setMethod("hasPeaks", "msData", function(object) {

  return(sapply(object@analyses, function(x) hasPeaks(x)))
})

### peaks ----------------------------------------------------------------

#' @describeIn msData getter for chromatographic peaks.
#' The arguments \code{targetID} and \code{mz}/\code{rt} can be used
#' to select specific peaks. The \emph{id} of peaks and/or features can be
#' given in the \code{targetsID} argument to select the respective peaks.
#' Also, analyses can be selected using the \code{analyses} argument.
#'
#' @template args-single-targetsID
#' @template args-single-filtered
#'
#' @export
#'
#' @aliases peaks,msData,msData-method
#'
setMethod("peaks", "msData", function(object,
                                      analyses = NULL,
                                      targetsID = NULL,
                                      mz = NULL, ppm = 20,
                                      rt = NULL, sec = 60,
                                      filtered = TRUE) {

  analyses <- checkAnalysesArgument(object, analyses)
  obj <- object
  obj@analyses <- obj@analyses[analyses]

  pks <- lapply(obj@analyses, function(x, targetsID,
                                       mz, ppm, rt, sec, filtered) {
    pks_a <- peaks(
      x, targetsID = targetsID,
      mz = mz, ppm = ppm, rt = rt, sec = sec,
      filtered = filtered
    )
    return(pks_a)
  },
    filtered = filtered,
    targetsID = targetsID,
    mz = mz,
    rt = rt,
    ppm = ppm,
    sec = sec
  )

  pks <- rbindlist(pks, idcol = "analysis")
  rpl <- data.table(analysis = analysisNames(obj), replicate = replicateNames(obj))
  pks <- pks[rpl, on = .(analysis = analysis)]
  pks <- pks[!is.na(id), ]

  return(pks)
})


### plotPeaks ------------------------------------------------------------------------------------------------

#' @describeIn msData a method for plotting chromatographic peaks
#' in an \linkS4class{msData} object.
#' The arguments \code{targetID} and \code{mz}/\code{rt} can be used
#' to select specific peaks. The \emph{id} of peaks and/or features can be
#' given in the \code{targetsID} argument to select the respective peaks.
#' Also, analyses can be selected using the \code{analyses} argument.
#' The \code{colorBy} argument can be be \code{"analyses"}, \code{replicates} or \code{targets}
#' (the default), for coloring by analyses, replicates or peak targets, respectively.
#' The \code{legendNames} is a character vector with the same length as targets for plotting and
#' can be used to lengend the plot. Note that, by setting \code{legendNames} the \code{colorBy}
#' is set to "targets" automatically.
#'
#' @export
#'
#' @aliases plotPeaks,msData,msData-method
#'
setMethod("plotPeaks", "msData", function(object,
                                          analyses = NULL,
                                          targetsID = NULL,
                                          mz = NULL, ppm = 20,
                                          rt = NULL, sec = 30,
                                          filtered = TRUE,
                                          colorBy = "targets",
                                          legendNames = NULL,
                                          title = NULL,
                                          interactive = FALSE) {

  analyses <- checkAnalysesArgument(object, analyses)
  obj <- object
  obj@analyses <- obj@analyses[analyses]

  peaks <- peaks(obj, analyses = NULL, targetsID, mz, ppm, rt, sec, filtered)

  pks_tars <- copy(peaks[, .(analysis, replicate, id, mz, rt, mzmin, mzmax, rtmin, rtmax)])
  pks_tars$rtmin <- min(pks_tars$rtmin) - 60
  pks_tars$rtmax <- max(pks_tars$rtmax) + 60

  if (nrow(pks_tars) == 0) {
    warning("No peaks were found with the defined targets!")
    return(NULL)
  }

  eic <- lapply(obj@analyses, function(x, pks_tars) {
    tar <- pks_tars[analysis %in% analysisNames(x), ]
    if (nrow(tar) < 1) return(data.table())
    eic <- EICs(x, mz = tar)
    return(eic)
  }, pks_tars = pks_tars)

  eic <- rbindlist(eic)

  return(
    plotPeaks(eic, peaks, analyses = NULL, colorBy = colorBy,
              legendNames = legendNames,
              title = title,
              interactive = interactive
    )
  )
})

### mapPeaks ------------------------------------------------------------------------------------------------

#' @describeIn msData a method for mapping peaks mass and time space.
#' The \code{colorBy} argument can be be \code{"analyses"}, \code{replicates} or \code{targets}
#' (the default), for coloring by analyses, replicates or peak targets, respectively.
#' The \code{legendNames} is a character vector with the same length as targets for plotting and
#' can be used to legend the plot. Note that, by setting \code{legendNames} the \code{colorBy}
#' is set to "targets" automatically.
#'
#' @param xlim A length one or two numeric vector for setting the \emph{x} limits of a plot.
#' @param ylim A length one or two numeric vector for setting the \emph{y} limits of a plot.
#'
#' @export
#'
#' @aliases mapPeaks,msData,msData-method
#'
setMethod("mapPeaks", "msData", function(object,
                                         analyses = NULL,
                                         targetsID = NULL,
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 30,
                                         filtered = TRUE,
                                         colorBy = "targets",
                                         legendNames = NULL,
                                         xlim = 30,
                                         ylim = 0.05,
                                         title = NULL) {

  peaks <- peaks(
    object,
    analyses,
    targetsID,
    mz, ppm,
    rt, sec,
    filtered
  )

  if (nrow(peaks) < 1) return(cat("Requested peaks were not found!"))

  if (colorBy == "analyses") {
    leg <- unique(peaks$analysis)
    varkey <- peaks$analysis
  } else if (colorBy == "replicates") {
    leg <- unique(peaks[, .(analysis, replicate)])
    leg <- leg$replicate
    varkey <- peaks$replicate
  } else if (!is.null(legendNames) & length(legendNames) == length(unique(peaks$id))) {
    leg <- legendNames
    names(leg) <- unique(peaks$id)
    varkey <- sapply(peaks$id, function(x) leg[x])
  } else {
    leg <- peaks$id
    names(leg) <- peaks$id
    varkey <- sapply(peaks$id, function(x) leg[names(leg) == x])
  }

  peaks[, var := varkey][]

  plot <- mapPeaksInteractive(peaks, xlim, ylim, title)

  return(plot)
})

### features ------------------------------------------------------------------------------------------------

#' @describeIn msData getter for features (i.e., grouped peaks). When
#' complete is set to \code{TRUE}, additional feature metadata is also returned.
#'
#' @param complete Logical, set to \code{TRUE} for a complete version of the output.
#' @param average Logical, set to \code{TRUE} for returning the intensity of
#' features averaged for each replicate group.
#'
#' @export
#'
#' @aliases features,msData,msData-method
#'
setMethod("features", "msData", function(object,
                                         targetsID = NULL,
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 60,
                                         filtered = TRUE,
                                         complete = FALSE,
                                         average = TRUE) {

  return(
    features(
      object = object@features,
      targetsID = targetsID,
      mz = mz, ppm = ppm,
      rt = rt, sec = sec,
      filtered = filtered,
      complete = complete,
      average = average
    )
  )
})


### plotFeatures --------------------------------------------------------------------------------------------

#' @describeIn msData A method for plotting peaks from given features
#' in an \linkS4class{msData} object.
#' The \code{colorBy} argument can be be \code{"analyses"}, \code{replicates} or \code{targets}
#' (the default), for coloring by analyses, replicates or peak targets, respectively.
#' The \code{legendNames} is a character vector with the same length as targets for plotting and
#' can be used to lengend the plot. Note that, by setting \code{legendNames} the \code{colorBy}
#' is set to "targets" automatically.
#'
#' @export
#'
#' @aliases plotFeatures,msData,msData-method
#'
setMethod("plotFeatures", "msData", function(object,
                                             analyses = NULL,
                                             targetsID = NULL,
                                             mz = NULL, ppm = 20,
                                             rt = NULL, sec = 30,
                                             filtered = TRUE,
                                             colorBy = "targets",
                                             legendNames = NULL,
                                             title = NULL,
                                             interactive = FALSE) {

  analyses <- checkAnalysesArgument(object, analyses)
  obj <- object[which(analyses %in% analysisNames(object))]

  feats <- features(
    object = obj,
    targetsID = targetsID,
    mz = mz,
    ppm = ppm,
    rt = rt,
    sec = sec,
    filtered = filtered
  )

  peaks <- peaks(
    obj,
    analyses,
    targetsID = feats$id,
    filtered = filtered
  )

  if (!is.null(legendNames) & length(legendNames) == length(unique(peaks$feature))) {
    names(legendNames) <- unique(peaks$feature)
    peaks$feature <- sapply(peaks$feature, function(x) legendNames[x])
    names(legendNames) <- peaks$id
  } else if (colorBy %in% "targets") {
    legendNames <- peaks$feature
    names(legendNames) <- peaks$id
  }

  pks_tars <- copy(peaks[, .(analysis, replicate, id, mz, rt, mzmin, mzmax, rtmin, rtmax)])
  pks_tars$rtmin <- min(pks_tars$rtmin) - 60
  pks_tars$rtmax <- max(pks_tars$rtmax) + 60

  eic <- lapply(object@analyses, function(x, pks_tars) {
    eic <- EICs(x, mz = pks_tars[analysis %in% analysisNames(x), ])
  }, pks_tars = pks_tars)

  eic <- rbindlist(eic)

  adj_rt <- hasAdjustedRetentionTime(obj)
  if (TRUE %in% adj_rt) {
    for (i in names(adj_rt)[adj_rt]) {
      eic[analysis == i, rt := unlist(sapply(rt, function(x, ana) {
        temp <- copy(ana@spectra[rt == x, ])
        return(temp$rtAdjusted)
      }, ana = getAnalyses(obj, analyses = i)))]

      # TODO Adds the retention adjustment to the peak average, not working
      # pks[analysis == i, rt := unlist(sapply(rt, function(x, ana) {
      #   temp <- copy(ana@spectra[rt == x, ])
      #   return(temp$rtAdjusted)
      # }, ana = getAnalyses(obj, analyses = i)))]
    }
  }

  return(
    plotPeaks(eic, peaks, analyses = NULL,
              colorBy = colorBy,
              legendNames = legendNames,
              title = title,
              interactive = interactive
    )
  )
})


### as.featureGroups ----------------------------------------------------------

#' @describeIn msData converts the \linkS4class{msData}
#' to a \linkS4class{featureGroups} object from the package \pkg{patRoon}.
#'
#' @export
#'
#' @aliases as.featureGroups,msData,msData-method
#'
setMethod("as.featureGroups", "msData", function(object) {

  anaInfo <- analysisInfo(object)

  feat <- as.features(object)

  if ("filtered" %in% colnames(object@features@metadata)) {
    feat_id_notFiltered <- object@features@metadata$id[!object@features@metadata$filtered]
  } else {
    feat_id_notFiltered <- object@features@metadata$id
  }

  groups_temp <- features(object, targetsID = feat_id_notFiltered, average = FALSE)
  groups <- copy(groups_temp)
  groups <- as.data.table(t(groups[, id := NULL]))

  groupInfo_temp <- object@features@metadata[ id %in% feat_id_notFiltered, ]
  groupInfo <- copy(groupInfo_temp)
  groupInfo <- as.data.frame(groupInfo[, .(rt, mz, id)])
  colnames(groupInfo) <- c("rts", "mzs", "id")

  new_id <- groupInfo_temp$id

  # make group id as patRoon, so far works without it
  # new_id <- object@features@metadata[, .(index, mz, rt)]
  # new_id <- paste0("M", round(new_id$mz, digits = 0),
  #                 "_R", round(new_id$rt, digits = 0),
  #                 "_", new_id$index)

  colnames(groups) <- new_id
  rownames(groups) <- seq_len(nrow(groups))

  rownames(groupInfo) <- new_id

  ftindex <- rbindlist(groupInfo_temp$peaks)
  ftindex <- as.data.table(t(ftindex))
  colnames(ftindex) <- new_id
  rownames(ftindex) <- seq_len(nrow(ftindex))

  # TODO adapt for as.featuresSet when multiple polarities present

  return(new("featureGroupsOpenMS", groups = groups, analysisInfo = anaInfo, groupInfo = groupInfo, features = feat, ftindex = ftindex))
})


### annotation -----------------------------------------------------------

#' @describeIn msData getter for annotation (i.e., isotopes and adducts clusters).
#' When giving the argument \code{all} as \code{TRUE},
#' all the features within the target feature annotation cluster are included in the output.
#'
#' @param all Logical, set to \code{TRUE} for displaying all features/peaks.
#'
#' @export
#'
#' @aliases annotation,msData,msData-method
#'
setMethod("annotation", "msData", function(object,
                                           targetsID = NULL,
                                           mz = NULL, ppm = 20,
                                           rt = NULL, sec = 60, id = NULL,
                                           all = FALSE) {

  feats <- object@features@metadata

  if (!"component" %in% colnames(feats)) {
    return(cat("Annotation seems to not be present in the given object!"))
  }

  if (!is.null(targetsID)) {
    feats <- feats[id %in% targetsID, ]
  } else {
    targets <- makeTargets(mz = mz, rt = rt, ppm = ppm, sec = sec, id = id)

    sel <- rep(FALSE, nrow(feats))
    for (i in seq_len(nrow(targets))) {
      sel[between(feats$mz, targets$mzmin[i], targets$mzmax[i]) &
            between(feats$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE
    }

    feats <- feats[sel]
  }

  all_feats <- features(object, complete = TRUE, average = TRUE)

  if (all) {
    outfeats <- all_feats[component %in% feats$component, ]
  } else {
    outfeats <- all_feats[(monoiso %in% feats$id | neutralMass %in% feats$neutralMass) & component %in% feats$component, ]
  }

  return(outfeats)
})


### plotAnnotation -------------------------------------------------------

#' @describeIn msData plots peaks from given feature annotation targets
#' in the \linkS4class{msData} object. The \code{colorBy} argument can be
#' set to \code{"isotopes"} for coloring by monoisotopic mass instead of
#' neutral mass of the cluster.
#'
#' @export
#'
#' @aliases plotAnnotation,msData,msData-method
#'
setMethod("plotAnnotation", "msData", function(object,
                                               targetsID = NULL,
                                               mz = NULL, ppm = 20,
                                               rt = NULL, sec = 30, id = NULL,
                                               all = FALSE,
                                               colorBy = "mass") {

  comps <- annotation(
    object = object,
    targetsID = targetsID,
    mz = mz,
    ppm = ppm,
    rt = rt,
    sec = sec,
    all = all
  )

  return(
    plotAnnotationInteractive(
      object = object,
      comps = comps,
      colorBy = colorBy
    )
  )
})


### [ sub-setting features -----------------------------------------------

#' @describeIn msData subset on analyses and features, using index or name.
#' Note that this method is irreversible.
#'
#' @param x An \linkS4class{msData} object.
#' @param i The indice/s or name/s of the analyses to keep in \code{x}.
#' @param j The indice/s or \emph{id}/s for of features to keep.
#' @param drop Not applicable to \linkS4class{msData}.
#' @param ... Other arguments.
#'
#' @export
#'
setMethod("[", c("msData", "ANY", "ANY", "missing"), function(x, i, j, ...) {

  if (!missing(i)) x <- x[i]

  if (!missing(j)) {

    if (nrow(x@features@metadata) == 0) {
      warning("There are no features in the msData object!")
      return(x)
    }

    if (!is.character(j)) j <- features(x)$id[j]

    x@features@intensity <- x@features@intensity[id %in% j, ]
    x@features@metadata <- x@features@metadata[id %in% j, ]

    x@analyses <- lapply(x@analyses, function(z, j) {
      temp <- z@peaks
      temp[!feature %in% j, `:=`(feature = NA_character_, filtered = TRUE, filter = "grouping")]
      z@peaks <- copy(temp)
      return(z)
    }, j = j)
  }

  return(x)
})

### [ sub-setting peaks -----------------------------------------------

#' @describeIn msData subset on analyses, features and peaks, using index or name.
#' Note that this method is irreversible.
#'
#' @param p The indice/s or \emph{id}/s of peaks to keep.
#'
#' @export
#'
setMethod("[", c("msData", "ANY", "ANY", "ANY"), function(x, i, j, p) {

  if (!missing(i)) x <- x[i]

  if (!missing(j)) x <- x[, j]

  if (!missing(p)) {

    if (!is.character(p)) {
      p <- peaks(x)$id[p]
      x@analyses <- lapply(x@analyses, function(z, p) {
        temp <- z@peaks
        temp <- temp[id %in% p, ]
        z@peaks <- copy(temp)
        return(z)
      }, p = p)

    #sub-sets the index of each analysis
    } else {
      x@analyses <- lapply(x@analyses, function(z, p) {
        temp <- z@peaks
        temp <- temp[p, ]
        z@peaks <- copy(temp)
        return(z)
      }, p = p)
    }
  }

  return(x)
})

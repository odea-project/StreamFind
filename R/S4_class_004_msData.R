

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

  blks <- na.omit(blanks(object))
  if (length(blks) > 0) {
    if (FALSE %in% (blks %in% replicates(object))) {
      warning("Blank replicates not present in analyses set!")
      valid <- FALSE
    }
  }

  # TODO validation for features classes

  return(valid)
}

### msData ------------------------------------------------------------------------------------------------

#' @title msData
#'
#' @description An S4 class object to store and manage processing of files with MS data.
#'   The \code{msData} object inherits the \linkS4class{streamProject} structure with type defined as
#'   \emph{ms}.
#'
#' @template slot-streamProject
#' @template slot-msData
#'
#' @export
#'
#' @importClassesFrom patRoon featuresSIRIUS featureGroupsSIRIUS componentsCliqueMS
#' @importFrom purrr quietly
#'
#' @md
setClass("msData",
  slots = c(
    features = "msFeatures"
  ),
  contains = "streamProject",
  prototype = list(
    features = new("msFeatures")
  ),
  validity = msData_validity
)

### S4 methods ----------------------------------------------------------------------------------------------

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
    "  Project       ", object@project, "\n",
    "  Date          ", as.character(object@date), "\n",
    "  Path          ", object@path, "\n",
    "  Analyses: \n",
    sep = ""
  )
  if (length(object@analyses) > 0) {
    tb <- data.frame(
      replicate = sapply(object@analyses, function(x) x@replicate),
      blank = sapply(object@analyses, function(x) x@blank),
      class = sapply(object@analyses, function(x) is(x)),
      peaks = sapply(object@analyses, function(x) nrow(x@peaks))
    )
    print(tb)
    # for (i in seq_len(length(object@analyses))) {
    #   cat("      - ", names(object@analyses[i]), " (", is(object@analyses[[i]])  , "-class) \n", sep = "")
    # }
  } else {
    cat("     n.a.", "\n", sep = "")
  }

})

#### getAnalyses ---------------------------------------------------------

#' @describeIn msData getter for analysis names.
#'
#' @template args-single-analyses
#'
#' @export
#'
setMethod("getAnalyses", "msData", function(object, analyses = NULL) {

  if (!is.null(analyses)) {
    if (is.character(analyses)) {
      analyses <- which(analyses == analyses(object))
    }
    if (length(analyses) > 0 && is.numeric(analyses)) {
      if (length(analyses) == 1) return(object@analyses[[analyses]])
      return(object@analyses[analyses])
    }
  }
  warning("Analysis not specified or not found in object!")
  return(list())
})

#### [ sub-setting analysis ----------------------------------------------

#' @describeIn msData subset on analysis, using analysis index or name.
#' Note that filtering of features is lazy to make it fast.
#' Only average \emph{m/z}, retention time and intensity are updated.
#' Run updateFeatureTable() with \code{FAST} set to \code{FALSE} for a complete update of
#' features. Note that the update might invalitate present quality parameters and annotation.
#'
#' @template args-single-i-subsetting
#'
#' @export
#'
#' @importMethodsFrom patRoon analyses groupNames
#'
setMethod("[", c("msData", "ANY", "missing", "missing"), function(x, i, ...) {

  if (!missing(i)) {

    object <- callNextMethod()

    return(object)

    # if (!is.character(i)) {
    #   sname <- analyses(x)[i]
    #   sidx <- i
    # } else {
    #   if (FALSE %in% (i %in% analyses(x))) {
    #     warning("Given analysis name/s not found in the msData object.")
    #     return(x)
    #   }
    #   sname <- i
    #   sidx <- which(analyses(x) %in% sname)
    # }
    #
    # x@analyses <- x@analyses[sidx]


    # if (length(analyses(x@pat)) > 0) {
    #
    #   x@pat <- x@pat[sidx]
    #
    #   if (nrow(x@peaks) > 0) {
    #     x@peaks <- x@peaks[sample %in% sname, ]
    #     x@peaks <- x@peaks[feature %in% groupNames(x@pat), ]
    #   }
    #
    #   if (nrow(x@features) > 0) x <- updateFeatureTable(x, fast = TRUE)
    #
    #   if (nrow(x@unified) > 0) x@unified <- x@unified[id %in% unique(x@peaks$feature), ]
    # }
  }

  return(x)
})

#### polarities ----------------------------------------------------------

#' @describeIn msData getter for analyses polarity.
#'
#' @export
#'
setMethod("polarities", "msData", function(object) {
  mt <- metadata(object, which = "polarity")
  mt_v <- mt$polarity
  names(mt_v) <- mt$analysis
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
  return(extractEICs(object, analyses, mz, ppm, rt, sec, id))
})


### plotEICs -------------------------------------------------------------

#' @describeIn msData a method for plotting extracted ion chromatograms (EICs)
#' of data in an \linkS4class{msData} object.
#' The arguments for data collection are the same as the \link{EICs} method.
#' A \link[data.table]{date.table} can be used instead.
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
setMethod("plotEICs", "msData", function(object,
                                         analyses = NULL,
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 30, id = NULL,
                                         colorBy = "targets",
                                         legendNames = NULL,
                                         title = NULL,
                                         interactive = FALSE) {

  eic <- extractEICs(object, analyses, mz, rt, ppm, id)

  return(plotEICs(eic, analyses = NULL, colorBy, legendNames, title, interactive))
})


### TICs -----------------------------------------------------------------

#' @describeIn msData extracts total ion chromatograms (TICs)
#' for analyses in an \linkS4class{msData} object.
#'
#' @export
#'
#' @importFrom data.table `:=` setcolorder
#'
setMethod("TICs", "msData", function(object, analyses = NULL) {

  analyses <- checkAnalysesArgument(object, analyses)
  if (is.null(analyses)) return(data.table())

  tics <- list()

  for (i in analyses) {

    file <- files(object)[i]

    tic <- loadRawDataMZR(file, spectra = FALSE, chroms = TRUE, chromsID = "TIC")[[1]]

    if (nrow(tic) < 1) {
      tic <- extractEICs(object, analyses = i, mz = NULL, rt = NULL)
      tic <- tic[, .(id, rt, intensity)]
    } else {
      tic <- tic[, .(id, rt, intensity)]
    }
    tic[, `:=`(analysis = i, replicate = replicates(object)[i])]
    setcolorder(tic, c("analysis", "replicate", "id", "rt", "intensity"))
    if (max(tic$rt) < 120) tic[, rt := rt * 60]
    tic <- tic[intensity > 0, ]
    tics[[i]] <- tic
    rm(tic)
  }
  return(rbindlist(tics))
})


### plotTICs -------------------------------------------------------------

#' @describeIn msData plots a total ion chromatogram (TIC) for abalyses in the object.
#' The colorBy argument can be "analyses" or "replicates"
#' to color the plot by analyses or by analysis replicates.
#'
#'
#' @export
#'
setMethod("plotTICs", "msData", function(object,
                                         samples = NULL,
                                         colorBy = "analyses",
                                         title = NULL,
                                         interactive = FALSE) {

  tics <- TICs(object, analyses = analyses)


  return(plotTICs(tics, analyses = NULL, colorBy = colorBy, title = title, interactive = interactive))
})


### XICs -----------------------------------------------------------------

#' @describeIn msData get three dimensional (\emph{m/z}, time and intensity)
#' extracted ion chromatograms (XICs) for specified \emph{m/z} and retention time pair targets
#' in analyses of an \linkS4class{msData} object. The arguments \code{mz}, \code{ppm}, \code{rt}, \code{sec} and \code{id}
#' are used to construct the targets. See ?\link{makeTargets} for more information.
#'
#' @export
#'
setMethod("XICs", "msData", function(object,
                                     analyses = NULL,
                                     mz = NULL, ppm = 20,
                                     rt = NULL, sec = 60, id = NULL) {

  xic <- extractXICs(object, analyses, mz, ppm, rt, sec, id)

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
#' @importFrom data.table is.data.table
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


### addParameters ----------------------------------------------------------

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

  valid <- checkmate::testClass(settings, "settings")

  valid <- checkmate::testChoice(where, c("analyses", "features"))

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

  valid <- checkmate::testChoice(where, c("analyses", "features"))

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
#' @importFrom data.table setnames
#' @importFrom dplyr select everything
#' @importClassesFrom patRoon features featuresOpenMS
#'
#' @aliases as.features,msData,msData-method
#'
setMethod("as.features", "msData", function(object) {

  anaInfo <- analysisInfo(object)

  feat <- lapply(object@analyses, function(x) {
    ft <- x@peaks
    ft <- data.table::setnames(ft,
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


### peaks ----------------------------------------------------------------

#' @describeIn msData getter for chromatographic peaks.
#' The arguments \code{targetID} and \code{mz}/\code{rt} can be used
#' to select specific peaks. The \emph{id} of peaks and/or features can be
#' given in the \code{targetsID} argument to select the respective peaks.
#' Also, analyses can be selected using the \code{analyses} argument.
#'
#' @template args-single-targetsID
#'
#' @export
#'
#' @importFrom dplyr between
#'
#' @aliases peaks,msData,msData-method
#'
setMethod("peaks", "msData", function(object,
                                      analyses = NULL,
                                      targetsID = NULL,
                                      mz = NULL, ppm = 20,
                                      rt = NULL, sec = 60) {

  analyses <- checkAnalysesArgument(object, analyses)
  obj <- object[which(analyses %in% analyses(object))]

  pks <- lapply(obj@analyses, function(x) {
    pks_a <- peaks(x, targetsID, mz, ppm, rt, sec)
    return(pks_a)
  })

  pks <- rbindlist(pks, idcol = "analysis")
  rpl <- data.table(analysis = analyses(obj), replicate = replicates(obj))
  pks <- pks[rpl, on = .(analysis = analysis)]

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
#' @importFrom data.table rbindlist copy
#'
#' @aliases plotPeaks,msData,msData-method
#'
setMethod("plotPeaks", "msData", function(object,
                                          analyses = NULL,
                                          targetsID = NULL,
                                          mz = NULL, ppm = 20,
                                          rt = NULL, sec = 30,
                                          colorBy = "targets",
                                          legendNames = NULL,
                                          title = NULL,
                                          interactive = FALSE) {

  analyses <- checkAnalysesArgument(object, analyses)
  obj <- object[which(analyses %in% analyses(object))]

  pks <- peaks(obj, analyses = NULL, targetsID, mz, ppm, rt, sec)

  pks_tars <- copy(pks[, .(analysis, replicate, id, mz, rt, mzmin, mzmax, rtmin, rtmax)])
  pks_tars$rtmin <- min(pks_tars$rtmin) - 60
  pks_tars$rtmax <- max(pks_tars$rtmax) + 60

  eic <- lapply(obj@analyses, function(x, pks_tars) {
    eic <- EICs(x, mz = pks_tars[analysis %in% analyses(x), ])
  }, pks_tars = pks_tars)

  eic <- rbindlist(eic)

  return(
    plotPeaks(eic, pks, analyses = NULL, colorBy = colorBy,
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
setMethod("mapPeaks", "msData", function(object,
                                         analyses = NULL,
                                         targetsID = NULL,
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 30,
                                         colorBy = "targets",
                                         legendNames = NULL,
                                         xlim = 30,
                                         ylim = 0.05,
                                         title = NULL) {

  pks <- peaks(
    object,
    analyses,
    targetsID,
    mz, ppm,
    rt, sec
  )

  if (nrow(pks) < 1) return(cat("Requested peaks were not found!"))

  if (colorBy == "analyses") {
    leg <- unique(pks$analysis)
    varkey <- pks$analysis
  } else if (colorBy == "replicates") {
    leg <- unique(pks[, .(analysis, replicate)])
    leg <- leg$replicate
    varkey <- pks$replicate
  } else if (!is.null(legendNames) & length(legendNames) == length(unique(pks$id))) {
    leg <- legendNames
    names(leg) <- unique(pks$id)
    varkey <- sapply(pks$id, function(x) leg[x])
  } else {
    leg <- pks$id
    names(leg) <- pks$id
    varkey <- sapply(pks$id, function(x) leg[names(leg) == x])
  }

  pks[, var := varkey][]

  plot <- mapPeaksInteractive(pks, xlim, ylim, title)

  return(plot)
})

### features ------------------------------------------------------------------------------------------------

#' @describeIn msData getter for features (i.e., grouped peaks). When
#' complete is set to \code{TRUE}, additional feature metadata is also returned.
#'
#' @param complete Logical, set to \code{TRUE} for a complete version of the output.
#'
#' @export
#'
#' @importFrom dplyr left_join
#'
setMethod("features", "msData", function(object,
                                         targetsID = NULL,
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 60,
                                         complete = FALSE,
                                         average = TRUE) {

  feats <- object@features

  if (!is.null(targetsID)) {
    out_fts <- feats@features[id %in% targetsID, ]
  } else if (!is.null(mz)) {
    targets <- makeTargets(mz, rt, ppm, sec)
    sel <- rep(FALSE, nrow(feats@metadata))
    for (i in seq_len(nrow(targets))) {
      sel[between(feats@metadata$mz, targets$mzmin[i], targets$mzmax[i]) &
            between(feats@metadata$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE
    }
    out_fts <- feats@features[sel]
  } else {
    out_fts <- feats@features
  }

  if (!average) {
    tp_pks <- setNames(data.frame(matrix(ncol = length(analyses(object)), nrow = 1)), analyses(object))
    tp_pks[1, ] <- 0

    out_fts_list <- lapply(out_fts$id, function(x, object, tp_pks) {
      pks <- copy(tp_pks)
      pks[1, ] <- sapply(analyses(object), function(a, object, x) {
        temp <- copy(object@analyses[[a]]@peaks)
        temp <- temp$intensity[temp$feature %in% x]
        if (length(temp) == 0) temp <- 0
        return(temp)
      }, object = object, x = x)
      return(pks)
    }, object = object, tp_pks = tp_pks)
    names(out_fts_list) <- out_fts$id
    out_fts <- rbindlist(out_fts_list, idcol = "id")
  }

  if (complete) {
    out_mtd <- feats@metadata[id %in% out_fts$id, ]
    out_fts <- left_join(out_mtd, out_fts, by = "id")
  }

  if (nrow(out_fts) < 1) {
    warning("Features not found in the msData object.")
  }

  return(out_fts)
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
#' @importFrom data.table rbindlist
#'
setMethod("plotFeatures", "msData", function(object,
                                              analyses = NULL,
                                              targetsID = NULL,
                                              mz = NULL, ppm = 20,
                                              rt = NULL, sec = 30,
                                              colorBy = "targets",
                                              legendNames = NULL,
                                              title = NULL,
                                              interactive = FALSE) {

  analyses <- checkAnalysesArgument(object, analyses)
  obj <- object[which(analyses %in% analyses(object))]

  feats <- features(
    obj,
    targetsID,
    mz,
    ppm,
    rt,
    sec
  )

  pks <- peaks(
    obj,
    analyses,
    targetsID = feats$id
  )

  if (!is.null(legendNames) & length(legendNames) == length(unique(pks$feature))) {
    names(legendNames) <- unique(pks$feature)
    pks$feature <- sapply(pks$feature, function(x) legendNames[x])
    names(legendNames) <- pks$id
  } else if (colorBy %in% "targets") {
    legendNames <- pks$feature
    names(legendNames) <- pks$id
  }

  pks_tars <- copy(pks[, .(analysis, replicate, id, mz, rt, mzmin, mzmax, rtmin, rtmax)])
  pks_tars$rtmin <- min(pks_tars$rtmin) - 60
  pks_tars$rtmax <- max(pks_tars$rtmax) + 60

  eic <- lapply(object@analyses, function(x, pks_tars) {
    eic <- EICs(x, mz = pks_tars[analysis %in% analyses(x), ])
  }, pks_tars = pks_tars)

  eic <- rbindlist(eic)

  return(
    plotPeaks(eic, pks, analyses = NULL,
              colorBy = colorBy,
              legendNames = legendNames,
              title = title,
              interactive = interactive
    )
  )


  # TODO apply correction of retention time for peaks from features
  # if (hasAdjustedRetentionTime(object)) {
  #   spls <- unique(eic$sample)
  #   for (i in spls) {
  #     eic[sample == i, rt := sapply(rt, function(x, object, i) {
  #       object@scans[[i]][retentionTime == x, adjustedRetentionTime]
  #     }, object = object, i = i)]
  #   }
  # }

  #
  # for (f_tar in unique(pks$feature)) {
  #   pks[feature %in% f_tar, rt := feats[id %in% f_tar, rt]]
  # }
  #

})

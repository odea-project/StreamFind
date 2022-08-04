

## S4 methods - data.table -----------------------------------------------------

### plotEICs-data.table --------------------------------------------------

#' @title plotEICs-data.table
#'
#' @description An S4 method for plotting extracted ion chromatograms (EICs)
#' of data in a \link[data.table]{data.table} object obtained with the
#' \link{EICs} method. The \code{colorBy} argument can be be \code{"analyses"},
#' \code{replicates} or \code{targets} (the default), for coloring by analyses,
#' replicates or EICs targets (id), respectively. The \code{legendNames} is a
#' character vector with the same length as targets for plotting and can be
#' used to legend the plot. Note that, by setting \code{legendNames}
#' the \code{colorBy} is set to "targets" even when other colorBy is used.
#'
#' @param object A \link[data.table]{data.table} as produced by
#' the method \link{EICs}.
#' @template args-single-analyses
#' @template args_plots_colorby_legendNames_title_interactive
#'
#' @export
#'
#' @aliases plotEICs,data.table,data.table-method
#'
setMethod("plotEICs", "data.table", function(object,
                                             analyses = NULL,
                                             colorBy = "targets",
                                             legendNames = NULL,
                                             title = NULL,
                                             interactive = FALSE) {

  eic <- copy(object)

  if (!is.null(analyses)) {
    if (class(analyses) == "numeric") analyses <- unique(eic$analysis)[analyses]
    eic[analysis %in% analyses, ]
  }

  if (nrow(eic) < 1) return(cat("Data was not found for any of the targets!"))

  if (colorBy == "analyses") {
    leg <- unique(eic$analysis)
    varkey <- eic$analysis
  } else if (colorBy == "replicates") {
    leg <- unique(eic[, .(analysis, replicate)])
    leg <- leg$replicate
    varkey <- eic$replicate
  } else if (!is.null(legendNames) & length(legendNames) == length(unique(eic$id))) {
    leg <- legendNames
    names(leg) <- unique(eic$id)
    varkey <- sapply(eic$id, function(x) leg[[x]])
  } else {
    leg <- unique(eic$id)
    varkey <- eic$id
  }

  eic[, var := varkey][]

  if (!interactive) {

    # TODO improve the static plot to be available as object
    # win.metafile()
    # dev.control("enable")
    # plotStaticEICs(
    #   eic,
    #   title
    # )
    # plot <- recordPlot()
    # dev.off()

    return(
      plotStaticEICs(
        eic,
        title
      )
    )

  } else {

    plot <- plotInteractiveEICs(eic, title, colorBy)

    return(plot)
  }
})


### plotTICs-data.table --------------------------------------------------

#' @title plotTICs-data.table
#'
#' @description Plots a total ion chromatogram (TIC) from the
#' \link[data.table]{data.table} obtained by the S4 method \link{TICs}.
#' The colorBy argument can be "analyses" or "replicates" to color
#' the plot by analyses or by analysis replicates.
#'
#' @template args-single-analyses
#' @template args_plots_colorby_title_interactive
#'
#' @export
#'
#' @rdname data.table-methods
#' @aliases plotTICs,data.table,data.table-method
#'
setMethod("plotTICs", "data.table", function(object,
                                             analyses = NULL,
                                             colorBy = "analyses",
                                             title = NULL,
                                             interactive = FALSE) {
  return(
    plotEICs(object,
      analyses = analyses,
      colorBy = colorBy,
      title = title,
      interactive = interactive
    )
  )
})

### plotXICs-data.table --------------------------------------------------

#' @title plotXICs-data.table
#'
#' @description Plots three dimensional (\emph{m/z}, time and intensity)
#' extracted ion chromatograms (XICs) for specified \emph{m/z} and
#' retention time pair targets in analyses of a \link[data.table]{data.table}
#' object as produced by the \link{XICs} method.
#' \code{analyses} and \code{targets} can be used to filter the XIC table.
#' When \code{plotTargetMark} is \code{TRUE} a target is plotted representing
#' the deviations as defined by the arguments \code{ppmMark} and \code{secMark}
#' in ppm and seconds, respectively. When ranges were given to build the XIC,
#' exact \emph{m/z} and time targets can be specified with the argument
#' \code{targetsMark}. \code{targetsMark} should be a two column table
#' named mz and rt with exact \emph{m/z} and time targets.
#' Note that the number of rows should be the same as the number of target in
#' the XIC. The number of rows to plot multiple targets can be defined by
#' the \code{numberRows} argument.
#'
#' @param object A \link[data.table]{data.table} as produced by
#' the method \link{XICs}.
#' @template args-single-analyses
#' @param targets A character vector with target names.
#' @param legendNames A character vector with the same length and order
#' as the number and order of targets to be used as plot legend.
#' @template args_plots_xics
#'
#' @export
#'
#' @aliases plotXICs,data.table,data.table-method
#'
#' @importFrom data.table is.data.table
#' @importFrom stringr str_split
#'
setMethod("plotXICs", "data.table", function(object,
                                             analyses = NULL,
                                             targets = NULL,
                                             legendNames = NULL,
                                             plotTargetMark = TRUE,
                                             targetsMark = NULL,
                                             ppmMark = 5,
                                             secMark = 10,
                                             numberRows = 1) {

  xic <- object

  if (!is.null(analyses)) {
    if (is.numeric(analyses)) {
      analyses <- unique(xic$analysis)[analyses]
    }
    xic <- xic[analyses %in% analyses, ]
  }

  if (!is.null(targets)) xic <- xic[id %in% targets, ]

  if (nrow(xic) < 1) return(cat("Data was not found for any of the targets!"))

  ids <- unique(xic$id)
  if (!is.null(legendNames) & length(legendNames) == length(ids)) {
    names(legendNames) <- unique(xic$id)
    xic$id <- sapply(xic$id, function(x) legendNames[[x]])
  }

  if (plotTargetMark) {
    otherTargets <- FALSE
    if (!is.null(targetsMark)) {

      if ((!is.data.table(targetsMark) | is.data.frame(targetsMark))) {

        if (nrow(targetsMark) == length(ids) &
            "mz" %in% colnames(targetsMark) &
            "rt" %in% colnames(targetsMark)) {

          tgmMZ <- targetsMark$mz
          names(tgmMZ) <- unique(xic$id)
          tgmRT <- targetsMark$rt
          names(tgmRT) <- unique(xic$id)
          xic[, mz_id := tgmMZ[xic$id]]
          xic[, rt_id := tgmRT[xic$id]]
          otherTargets <- TRUE

        }
      }
    }

    if (!otherTargets & class(xic$mz_id) == "character") {

      tgmMZ <- sapply(xic$mz_id, function(x)
        mean(as.numeric(stringr::str_split(x, "-", simplify = TRUE)[1, ])))

      tgmRT <- sapply(xic$rt_id, function(x)
        mean(as.numeric(stringr::str_split(x, "-", simplify = TRUE)[1, ])))
      xic[, mz_id := tgmMZ]
      xic[, rt_id := tgmRT]
    }
  }

  plot <- plotInteractiveXICs(
    xic,
    plotTargetMark = plotTargetMark,
    ppmMark = ppmMark,
    secMark = secMark,
    numberRows = numberRows
  )

  return(plot)
})


### plotMS2s-data.table --------------------------------------------------

#' @title plotMS2s-data.table
#'
#' @description Plots MS2 data for specified \emph{m/z} and retention time
#' (seconds) targets in a \link[data.table]{data.table} as obtained by the
#' \link{MS2s}. The targets in the object can be filtered using the
#' \code{targets} argument. Also, "analyses" and "replicates" can be filtered
#' using the \code{analyses} and \code{replicates} arguments, respectively.
#' Note that the column analysis/replicate should be present.
#' The possible values for the \code{colorBy} argument are
#' "targets", "analyses", "replicates" and "voltages" to color by
#' each target, analysis, replicate or collision energy, respectively.
#'
#' @param object A \link[data.table]{data.table} as produced by
#' the method \link{MS2s}.
#' @template args-single-analyses
#' @param replicates A numeric or character vector with the indice/s or name/s
#' of replicates from the \code{object}.
#' @param targets A character vector with target names.
#' @param legendNames A character vector with the same length and order
#' as the number and order of targets to be used as plot legend.
#' @template args_plots_colorby_title_interactive
#'
#' @export
#'
#' @aliases plotMS2s,data.table,data.table-method
#'
#' @importFrom data.table data.table
#'
setMethod("plotMS2s", "data.table", function(object = NULL,
                                             analyses = NULL,
                                             replicates = NULL,
                                             targets = NULL,
                                             legendNames = NULL,
                                             title = NULL,
                                             colorBy = "targets",
                                             interactive = FALSE) {

  ms2 <- copy(object)

  if (!is.null(analyses) & "analysis" %in% colnames(ms2)) {
    if (class(analyses) == "numeric") analyses <- unique(ms2$analysis)[analyses]
    ms2[analysis %in% analyses, ]
  }

  if (!is.null(replicates) & "replicate" %in% colnames(ms2)) {
    if (class(replicates) == "numeric") replicates <- unique(ms2$replicate)[replicates]
    ms2[replicate %in% replicates, ]
  }

  if (!is.null(targets)) ms2[id %in% targets, ]

  if (nrow(ms2) < 1) return(cat("Data was not found for any of the targets!"))

  if (colorBy == "analyses" & "analysis" %in% colnames(ms2)) {
    leg <- unique(ms2$sample)
    varkey <- ms2$sample
  } else if (colorBy == "replicates" & "replicate" %in% colnames(ms2)) {
    leg <- unique(ms2$replicate)
    varkey <- ms2$replicate
  } else if (colorBy == "voltages" & "ce" %in% colnames(ms2)) {
    leg <- unique(ms2$voltage)
    varkey <- ms2$voltage
  } else if (!is.null(legendNames) & length(legendNames) == length(unique(ms2$id))) {
    leg <- legendNames
    names(leg) <- unique(ms2$id)
    varkey <- sapply(ms2$id, function(x) leg[[x]])
  } else {
    leg <- unique(ms2$id)
    varkey <- ms2$id
  }

  ms2[, var := varkey]
  ms2$var <- factor(ms2$var, levels = unique(ms2$var), labels = unique(ms2$var))

  if (!interactive) {

    win.metafile()
    dev.control("enable")
    plotStaticMSn(
      ms2,
      title
    )
    plot <- recordPlot()
    dev.off()

  } else {

    plot <- plotInteractiveMSn(ms2, title)

  }

  return(plot)
})


### plotPeaks-data.table -------------------------------------------------------

#' @title plotPeaks-data.table
#'
#' @description A method for plotting EIC (\code{object}) of chromatographic
#' peaks (\code{pks}). The \code{colorBy} argument can be be \code{"analyses"},
#' \code{replicates} or \code{targets} (the default), for coloring by analyses,
#' replicates or target peaks (id), respectively. The \code{legendNames} is a
#' character vector with the same length as target peaks for plotting and
#' can be used to legend the plot. Note that, by setting \code{legendNames}
#' the \code{colorBy} is set to "targets" even when other colorBy is used.
#'
#' @param object A \link[data.table]{data.table} as produced by
#' the method \link{EICs}.
#' @param peaks A table with the target peaks as obtained by
#' the method \link{peaks}.
#' @template args-single-analyses
#' @template args_plots_colorby_legendNames_title_interactive
#'
#' @export
#'
#' @importMethodsFrom xcms plotPeaks
#' @importFrom data.table rbindlist
#'
#' @aliases plotPeaks,data.table,data.table-method
#'
setMethod("plotPeaks", "data.table", function(object,
                                              peaks,
                                              analyses = NULL,
                                              colorBy = "targets",
                                              legendNames = NULL,
                                              title = NULL,
                                              interactive = FALSE) {


  eic <- copy(object)

  if (!is.null(analyses)) {
    if (class(analyses) == "numeric") analyses <- unique(eic$analysis)[analyses]
    eic[analysis %in% analyses, ]
  }

  if (nrow(eic) < 1) return(cat("Data was not found for any of the targets!"))

  if (colorBy == "analyses") {

    leg <- unique(eic$analysis)
    varkey <- eic$analysis

  } else if (colorBy == "replicates") {

    leg <- unique(eic[, .(analysis, replicate)])
    leg <- leg$replicate
    varkey <- eic$replicate

  } else if (!is.null(legendNames) &
             length(legendNames) == length(unique(eic$id))) {

    leg <- legendNames
    names(leg) <- unique(eic$id)
    varkey <- sapply(eic$id, function(x) leg[[x]])

  } else {

    leg <- unique(eic$id)
    varkey <- eic$id
  }

  eic[, var := varkey][]

  data.table::setorder(eic, var, rt)

  if (!interactive) {

    return(
      plotPeaksStatic(
        eic,
        peaks,
        title
      )
    )

  } else {

    plot <- plotPeaksInteractive(eic, peaks, title, colorBy)

    return(plot)
  }
})

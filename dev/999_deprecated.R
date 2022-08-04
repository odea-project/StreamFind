
#' @title extractEICs
#'
#' @description Extracts MS1 level ion chromatograms (EICs) from raw data
#' of specified \emph{m/z} and retention time pair/s. The function uses the
#' \link[patRoon]{getEICs} function from \pkg{patRoon}.
#'
#' @template args-single-object-msData
#' @template args-single-analyses
#' @template args-makeTargets
#'
#' @return A \code{data.table} with the columns
#' \code{analysis}, \code{replicate}, \code{id}, \code{rt}, and \code{intensity}
#' representing the analysis name, the analysis replicate name,
#' the target id, the retention time and the sum of the intensities
#' for the collected \emph{m/z} in each spectrum (i.e., MS scan), respectively.
#'
#' @export
#'
#' @references
#' \insertRef{patroon01}{streamFind}
#'
#' \insertRef{proteo01}{streamFind}
#'
#' @importFrom checkmate testClass
#' @importFrom data.table rbindlist setnames setcolorder copy
#' @importFrom patRoon getEICs
#'
extractEICs <- function(object = NULL,
                        analyses = NULL,
                        mz = NULL, ppm = 20,
                        rt = NULL, sec = 60, id = NULL) {

  valid <- FALSE

  if (checkmate::testClass(object, "msData") | checkmate::testClass(object, "msAnalysis"))
    valid = TRUE

  if (!valid) {
    warning("Invalid class object used as argument!")
    return(data.table())
  }

  analyses <- checkAnalysesArgument(object, analyses)
  if (is.null(analyses)) return(data.table())

  targets <- makeTargets(mz = mz, rt = rt, ppm = ppm, sec = sec, id = id)
  data.table::setnames(targets, c("rtmin", "rtmax"), c("retmin", "retmax"))

  eicList <- lapply(analyses, function(x, targets, object) {

    fl <- filePaths(object)[x]

    ms_ana <- object
    if (checkmate::testClass(ms_ana, "msData")) ms_ana <- ms_ana@analyses[[x]]

    targets[mz == 0 & rt == 0, id := "TIC"]
    targets[mz == 0 & rt == 0, mz := NA]
    targets[mz == 0 & rt == 0, rt := NA]
    targets[retmin == 0, retmin := metadata(ms_ana, which = "dStartTime")$dStartTime]
    targets[retmax == 0, retmax := metadata(ms_ana, which = "dEndTime")$dEndTime]
    targets[mzmin == 0, mzmin := metadata(ms_ana, which = "lowMz")$lowMz]
    targets[mzmax == 0, mzmax := metadata(ms_ana, which = "highMz")$highMz]

    # TODO check if analyses has spectra to use that instead

    eic <- patRoon::getEICs(fl, targets[, .(retmin, retmax, mzmin, mzmax)])

    names(eic) <- targets[["id"]]
    eic <- rbindlist(eic, idcol = "id")
    setnames(eic, c("id", "rt", "intensity"))
    eic[, analysis := x]
    eic[, replicate := replicates(object)[x]][]

    return(eic)
  }, targets = targets, object = object)

  eics <- rbindlist(eicList)
  setcolorder(eics, c("analysis", "replicate", "id", "rt", "intensity"))

  #removes zeros
  #eics <- eics[intensity > 0, ]

  return(copy(eics))
}


#' @title extractXICs
#'
#' @description Extracts three dimensional MS1 level ion chromatograms (XICs)
#' from raw data of specified \emph{m/z} and retention time pair/s.
#' The XIC extraction is slower than the \link{extractEICs}, therefore it should
#' be used for limited/narrow mass and time ranges. To load the raw spectra,
#' the \pkg{patRoon} package is used.
#'
#' @template args-single-object-msData
#' @template args-single-analyses
#' @template args-makeTargets
#'
#' @return A \link[data.table]{data.table} with the columns
#' \code{analysis}, \code{replicate}, \code{id}, \code{mz}, \code{rt}, and \code{intensity}
#' representing the analysis name, the analysis replicate name,
#' the XIC target id, the \emph{m/z}, the retention time and the intensity
#' of each \emph{m/z} and retention time pair, respectively.
#'
#' @export
#'
#' @importFrom checkmate assertClass
#' @importFrom data.table rbindlist setcolorder copy `:=`
#'
extractXICs <- function(object = NULL,
                        analyses = NULL,
                        mz = NULL, ppm = 20,
                        rt = NULL, sec = 60, id = NULL) {

  valid <- FALSE

  if (checkmate::testClass(object, "msData") | checkmate::testClass(object, "msAnalysis"))
    valid = TRUE

  if (!valid) {
    warning("Invalid class object used as argument!")
    return(data.table())
  }

  analyses <- checkAnalysesArgument(object, analyses)
  if (is.null(analyses)) return(data.table())

  targets <- makeTargets(mz, rt, ppm, sec, id)

  xicList <- list()

  # TODO check if analyses has spectra to use that instead

  xicList <- lapply(analyses, function(x, targets, object) {

    rtr <- c(min(targets$rtmin) * 0.7, max(targets$rtmax) * 1.3)
    if (rtr[1] == 0 & rtr[2] == 0) rtr <- NULL

    fl <- filePaths(object)[x]

    xic <- loadRawDataMZR(fl, level = 1, chroms = FALSE, rtr = rtr)
    xic <- xic$spectra

    xics <- list()

    for (i in seq_len(nrow(targets))) {
      xic_temp <- copy(xic[
        rt >= targets$rtmin[i] &
          rt <= targets$rtmax[i] &
          mz >= targets$mzmin[i] &
          mz <= targets$mzmax[i],
      ])
      xic_temp[, `:=`(id = targets$id[i], mz_id = targets$mz[i], rt_id = targets$rt[i])]
      xics[[targets$id[i]]] <- xic_temp
      rm(xic_temp)
    }

    xics <- rbindlist(xics)
    xics[, `:=`(analysis = x, replicate = replicates(object)[x])]

    return(xics)
  }, targets = targets, object = object)

  xics <- rbindlist(xicList)
  xics <- xics[, .(analysis, replicate, id, mz_id, rt_id, mz, rt, intensity)]

  # TODO implemented alignment correction for XICs
  # if (hasAdjustedRetentionTime(object)) {
  #
  #   spls <- unique(xics$sample)
  #
  #   for (i in spls) {
  #     xics[sample == i, rt := sapply(rt, function(x, object, i) {
  #       object@scans[[i]][retentionTime == x, adjustedRetentionTime]
  #     }, object = object, i = i)]
  #   }
  #
  # }

  return(copy(xics))
}

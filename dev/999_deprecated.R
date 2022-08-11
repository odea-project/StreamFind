
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

#' @title updateFeatureTable
#'
#' @description Function to update feature table from
#' peaks remaining in the \linkS4class{msData} object.
#'
#' @param object An \linkS4class{msData} object.
#' @param fast Logical, set to \code{TRUE} for a lazy update of features.
#' When \code{TRUE}, only the mz, rt, and intensity are updated.
#' When \code{FALSE}, the feature table is completly updated.
#' Note that UFIs, quality data and annotation info are lost.
#'
#' @return An \linkS4class{msData} object with updated features slot.
#'
#' @export
#'
#' @importMethodsFrom patRoon as.data.table groupNames
#' @importFrom data.table setnames setorder copy
#' @importFrom dplyr select everything
#'
updateFeatureTable <- function(object, fast = TRUE) {

  cat("Updating features... ")

  pat <- object@pat

  feat <- patRoon::as.data.table(pat, average = TRUE)

  feat <- setnames(feat, c("ret", "group"), c("rt", "id"))

  rpl <- unique(replicateNames(object))

  hasRemoved <- FALSE

  feat_org <- features(object)

  feat_org <- feat_org[id %in% feat$id, ]

  feat2org <- feat[id %in% feat_org$id, ]

  feat_org[, rt := feat2org$rt]
  feat_org[, mz := feat2org$mz]

  if (!TRUE %in% all.equal(feat_org$id, feat2org$id)) {
    warning("Mistach of features order during update
      of feature table! Feature table not updated."
    )
    return(object)
  }

  feat_org[, (rpl) := feat2org[, rpl, with = FALSE]]

  colRem <- colnames(feat_org)
  colRem <- c(which(colRem == "d_sec") + 1, which(colRem == "mzmin") - 1)
  colRem <- colnames(feat_org)[colRem[1]:colRem[2]]
  colRem <- colRem[!grepl(rpl, colRem, fixed = TRUE)]

  if (length(colRem) > 0) feat_org[, (colRem) := NULL]

  if (nrow(object@removed) > 0) {
    hasRemoved <- TRUE
    feat_rem <- object@removed
    feat_rem <- feat_rem[id %in% feat$id, ]
    feat2rem <- feat[id %in% feat_rem$id, ]
    feat_rem[, rt := feat2rem$rt]
    feat_rem[, mz := feat2rem$mz]
    feat_rem[, (rpl) := feat2rem[, rpl, with = FALSE]]
    if (length(colRem) > 0) feat_rem[, (colRem) := NULL]
  }

  if (fast) {

    object@features <- copy(feat_org)
    if (hasRemoved) object@removed <- copy(feat_rem)

    cat("Done with the fast method! \n")

    return(object)
  }

  #not fast
  if (hasRemoved) feat_org <- rbind(feat_org, feat_rem)

  ID <- patRoon::groupNames(object@pat)
  if (length(ID) != nrow(feat_org)) {
    warning("There is a mismatch in the number of features between
      ntsData and the patRoon object! Features not updated."
    )
    return(object)
  }
  feat_org <- feat_org[data.table::data.table(id = ID), on = "id"]

  rpl_sp <- lapply(rpl, function(x, st) {
    st$sample[st$replicate == x]
  }, st = samplesTable(object))

  feat_b <- patRoon::as.data.table(pat, average = FALSE)

  #update intensities sd
  feat_sd <- lapply(rpl_sp, function(x, feat_b) {
    temp <- feat_b[, x, with = FALSE]
    temp <- apply(temp, 1, function(x) sd(x) / mean(x) * 100)
    temp[is.nan(temp)] <- 0
    temp <- round(temp, digits = 0)
    return(temp)
  }, feat_b = feat_b)

  names(feat_sd) <- paste0(rpl, "_sd")
  feat_sd <- as.data.table(feat_sd)
  feat_sd[, id := feat$id]

  sd_cols <- paste0(rpl, "_sd")
  feat_org[, (sd_cols) := feat_sd[which(id %in% feat_org$id), sd_cols, with = FALSE]]

  #update mz and rt ranges
  pk <- peaks(object)

  pk$index <- seq_len(nrow(pk))
  index <- lapply(feat_org$id, function(x) pk[feature == x, index])
  feat_org$mzmin <- unlist(lapply(index, function(x) min(pk[x, mz])))
  feat_org$mzmax <- unlist(lapply(index, function(x) max(pk[x, mz])))
  feat_org$rtmin <- unlist(lapply(index, function(x) min(pk[x, rt])))
  feat_org$rtmax <- unlist(lapply(index, function(x) max(pk[x, rt])))

  feat_org$d_ppm <- round(((feat_org$mzmax - feat_org$mzmin) / feat_org$mz) * 1E6, digits = 1)

  feat_org$d_sec <- round(feat_org$rtmax - feat_org$rtmin, digits = 0)

  feat_org$p_id <- I(lapply(feat_org$id, function(x) pk[feature == x, id]))

  pk$replicate <- factor(pk$replicate, levels = rpl)

  feat_org$npeaks <- lapply(index, function(x) {
    as.data.frame(table(pk$replicate[x]))$Freq
  })

  if ("is_filled" %in% colnames(pk)) {
    feat_org$hasFilled <- unlist(lapply(index, function(x) 1 %in% pk$is_filled[x]))
  } else {
    feat_org$hasFilled <- FALSE
  }

  object@features <- feat_org

  if (hasRemoved) object <- removeFilteredFeatures(object)

  cat("Done with complete updating method! \n")

  return(object)
}


### initialize msAnalysis ------------------------------------------------------

#' @describeIn msAnalysis initializes an \linkS4class{msAnalysis} object.
#'
#' @usage ## S4 method to initialize an "msAnalysis" object: 'new("msAnalysis", file)'
#'
#' @param ... Other arguments.
#'
#' @importFrom mzR openMSfile header runInfo instrumentInfo close
#'
#' @export
#'
setMethod("initialize", "msAnalysis", function(.Object, ...) {

  .Object <- callNextMethod()

  if (is.na(.Object@file)) {
    warning("A file was not given to create the msAnalysis object!")
    return(NULL)

  } else {

    if (!requireNamespace("mzR")) {
      warning("Package mzR not installed!")
      return(NULL)
    }

    fl <- .Object@file
    msf <- mzR::openMSfile(fl, backend = "pwiz")

    hd <- mzR::header(msf)
    rInfo <- mzR::runInfo(msf)
    acInfo <- mzR::instrumentInfo(msf)

    .Object@analysis <- gsub(".mzML|.mzXML", "", basename(fl))

    if (is.na(.Object@replicate)) {
      .Object@replicate <- .Object@analysis
      .Object@replicate <- gsub( "-", "_", .Object@replicate)
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



#' @title msAnalysisLoadMZR
#'
#' @description Creates a \linkS4class{msAnalysis} for each mzML/mzXML file
#' in a given data.frame. The function uses the \pkg{base} and \pkg{mzR}
#' packages only to enable fast parallel processing.
#'
#' @param file_df A data.frame with the columns file, replicate and blank
#' for each file to be converted to a \linkS4class{msAnalysis}.
#'
#' @importFrom BiocParallel bplapply SerialParam SnowParam
#'
#' @noRd
msAnalysisLoadMZR <- function(file_df) {

  file_df <- split(file_df, file_df$file)

  bpp <- SerialParam(progressbar = TRUE)
  #bpp <- SnowParam(progressbar = TRUE)

  analyses <- bplapply(file_df, function(x) {

    if (!requireNamespace("mzR", quietly = TRUE)) {
      warning("Package mzR not installed!")
      return(NULL)
    }

    ana <- list()

    fl <- x$file
    msf <- mzR::openMSfile(fl, backend = "pwiz")

    hd <- mzR::header(msf)
    rInfo <- mzR::runInfo(msf)
    acInfo <- mzR::instrumentInfo(msf)

    ana$analysis <- gsub(".mzML|.mzXML", "", basename(fl))
    ana$file <- fl

    if (is.na(x$replicate)) {
      ana$replicate <- ana$analysis
      ana$replicate <- gsub( "-", "_", ana$replicate)
      ana$replicate <- sub("_[^_]+$", "", ana$replicate)
    } else {
      ana$replicate <- x$replicate
    }

    ana$blank <- x$blank

    if (nrow(hd) > 0) {
      polarity <- unique(hd$polarity)
      if (length(polarity) == 1) {
        if (polarity == 1) ana$metadata$polarity <- "positive"
        if (polarity == 0) ana$metadata$polarity <- "negative"
      } else if (0 %in% polarity | 1 %in% polarity) {
        ana$metadata$polarity <- "both"
      } else {
        ana$metadata$polarity <- NA_character_
      }
      ana$metadata$centroided <- TRUE %in% hd$centroided
      ana$metadata <- c(ana$metadata, rInfo, acInfo)
    } else {
      ana$metadata <- list()
    }

    suppressWarnings(mzR::close(msf))

    return(ana)
  },
  BPPARAM = bpp)

  analyses <- lapply(analyses, function(x) {

    ana <- new("msAnalysis",
               analysis = x$analysis, file = x$file,
               replicate = x$replicate, blank = x$blank,
               metadata = x$metadata)

    return(ana)
  })

  names(analyses) <- sapply(analyses, FUN = function(x) analysisNames(x))

  analyses <- analyses[sort(names(analyses), decreasing = FALSE)]

  return(analyses)
}

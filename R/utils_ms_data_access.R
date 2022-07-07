

#' @title makeTargets
#'
#' @description Helper function to build \emph{m/z} and retention time
#' target pairs for searching data. Each target is composed of an
#' id and \emph{m/z} (Da) and time (seconds) ranges. When mass is defined without
#' time, the time range return NA and vice versa.
#'
#' @template args-makeTargets
#'
#' @return A data.table with \emph{m/z} and retention time target pairs identified by an id.
#'
#' @export
#'
#' @importFrom data.table data.table is.data.table as.data.table
#'
makeTargets <- function(mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL) {

  mzrts <- data.table(
    id = NA_character_,
    mz = 0,
    rt = 0,
    mzmin = 0,
    mzmax = 0,
    rtmin = 0,
    rtmax = 0
  )

  # when only rt is given
  if (is.null(mz) & !is.null(rt)) {

    # as vector
    if (length(rt) >= 1 & is.vector(rt)) {

      mzrts <- data.table(
        id = NA_character_,
        mz = rt,
        rt = 0,
        mzmin = 0,
        mzmax = 0,
        rtmin = 0,
        rtmax = 0
      )
      mzrts[, rtmin := rt - sec]
      mzrts[, rtmax := rt + sec]

      #adds id
      if (!is.null(id) & length(id) == length(rt)) {
        mzrts[, id := id][]
      } else {
        mzrts[, id := paste(rtmin, "-", rtmax, sep = "")][]
      }

    # as table
    } else if (is.data.frame(rt) | is.data.table(rt)) {
      rt <- as.data.table(rt)

      if ("rt" %in% colnames(rt) & !"rtmin" %in% colnames(mz)) {
        mzrts <- data.table(
          id = NA_character_,
          mz = rt,
          rt = 0,
          mzmin = 0,
          mzmax = 0,
          rtmin = 0,
          rtmax = 0
        )
        mzrts$rtmin <- rt$rt - sec
        mzrts$rtmax <- rt$rt + sec

      } else if ("rtmin" %in% colnames(rt) & nrow(rt) == nrow(mz)) {
        mzrts <- data.table(
          id = NA_character_,
          mz = apply(rt[, .(rtmin, rtmax)], 1, mean),
          rt = 0,
          mzmin = rt$rtmin,
          mzmax = rt$rtmax,
          rtmin = 0,
          rtmax = 0
        )

        if ("rt" %in% colnames(rt)) {
          mzrts$rt <- mz$rt
        } else {
          mzrts$rt <-  apply(rt[, .(rtmin, rtmax)], 1, mean)
        }
      }

      #adds id
      if (!is.null(id) %in% length(id) == nrow(mzrts)) {
        mzrts$id <- id
      } else if ("id" %in% colnames(rt)) {
        mzrts$id <- rt$id
      } else {
        mzrts[, id := paste(rtmin, "-", rtmax, sep = "")][]
      }
    }

  #when mz is vector, expects rt as vector as well and ranges are calculated
  } else if (length(mz) >= 1 & is.vector(mz)) {

    mzrts <- data.table(
      id = NA_character_,
      mz = mz,
      rt = 0,
      mzmin = mz - ((ppm / 1E5) * mz),
      mzmax = mz + ((ppm / 1E5) * mz),
      rtmin = 0,
      rtmax = 0
    )
    #mzrts$mzmin <- mz - ((ppm / 1E6) * mz)
    #mzrts[, mzmax := mz + ((ppm / 1E6) * mz)]

    if (is.vector(rt) & length(rt) == length(mz)) {
      mzrts$rt <- rt
      mzrts$rtmin <- c(rt - sec)
      mzrts$rtmax <- c(rt + sec)
    }

    if (!is.null(id) & length(id) == nrow(mzrts)) {
      mzrts$id <- id
    } else {
      mzrts[, id := paste(
        round(mzmin, digits = 4),
        "-",
        round(mzmax, digits = 4),
        "/", rtmin,
        "-", rtmax,
        sep = ""
      )][]
    }

  #when mz is a table, ranges could be already in table
  } else if (is.data.frame(mz) | is.data.table(mz)) {
    mz <- as.data.table(mz)

    #when mz is in table but not ranges
    if ("mz" %in% colnames(mz) & !"mzmin" %in% colnames(mz)) {
      mzrts <- data.table(
        id = NA_character_,
        mz = mz$mz,
        rt = 0,
        mzmin = 0,
        mzmax = 0,
        rtmin = 0,
        rtmax = 0
      )
      mzrts[, mzmin := mz - ((ppm / 1E6) * mz)]
      mzrts[, mzmax := mz + ((ppm / 1E6) * mz)]

    #when mzmin is in table
    } else if ("mzmin" %in% colnames(mz)) {
      mzrts <- data.table(
        id = NA_character_,
        mz = apply(mz[, .(mzmin, mzmax)], 1, mean),
        rt = 0,
        mzmin = mz$mzmin,
        mzmax = mz$mzmax,
        rtmin = 0,
        rtmax = 0
      )
      if ("mz" %in% colnames(mz)) mzrts$mz <- mz$mz
    }

    #when rt in also in mz table
    if ("rt" %in% colnames(mz) & !"rtmin" %in% colnames(mz)) {
      mzrts$rt <- mz$rt
      mzrts$rtmin <- mz$rt - sec
      mzrts$rtmax <- mz$rt + sec
    } else if ("rtmin" %in% colnames(mz)) {
      mzrts$rt <-  apply(mz[, .(rtmin, rtmax)], 1, mean)
      mzrts$rtmin <- mz$rtmin
      mzrts$rtmax <- mz$rtmax
      if ("rt" %in% colnames(mz)) mzrts$rt <- mz$rt
    }

    #when rt is given as a table is rt argument
    if (is.data.frame(rt) | is.data.table(rt)) {
      rt <- as.data.table(rt)

      if ("rt" %in% colnames(rt) & nrow(rt) == nrow(mz) & !"rtmin" %in% colnames(mz)) {
        mzrts$rt <- rt$rt
        mzrts$rtmin <- rt$rt - sec
        mzrts$rtmax <- rt$rt + sec
      } else if ("rtmin" %in% colnames(rt) & nrow(rt) == nrow(mz)) {
        mzrts$rt <-  apply(rt[, .(rtmin, rtmax)], 1, mean)
        mzrts$rtmin <- rt$rtmin
        mzrts$rtmax <- rt$rtmax
        if ("rt" %in% colnames(rt)) mzrts$rt <- mz$rt
      }
    }

    #adds id
    if (!is.null(id) & length(id) == nrow(mzrts)) {
      mzrts$id <- id
    } else if ("id" %in% colnames(mz)) {
      mzrts$id <- mz$id
    } else {
      mzrts[, id := paste(
        round(mzmin, digits = 4),
        "-",
        round(mzmax, digits = 4),
        "/",
        rtmin,
        "-",
        rtmax,
        sep = ""
      )][]
    }
  }

  return(mzrts)
}


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

    fl <- files(object)[x]

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

  #eics <- eics[intensity > 0, ]

  return(copy(eics))
}


#' @importFrom data.table as.data.table
#'
loadBasicRawSpectraHeaderMZR <- function(file) {

  zF <- openMSfile(file, backend = "pwiz")

  zH <- as.data.table(header(zF))

  zH <- zH[, .(seqNum, acquisitionNum, msLevel, retentionTime)]

  colnames(zH) <- c("index", "scan", "lv", "rt")

  suppressWarnings(mzR::close(zF))

  return(zH)
}


#' @importFrom mzR openMSfile header peaks chromatogramHeader chromatograms
#' @importFrom data.table data.table as.data.table rbindlist copy
#' @importFrom dplyr inner_join
#'
loadRawDataMZR <- function(file, spectra = TRUE, level = 1, rtr = NULL,
                           chroms = TRUE, chromsID = NULL,
                           ifChromNoSpectra = FALSE) {

  dl <- list()

  zF <- openMSfile(file, backend = "pwiz")

  if (chroms) {
    cH <- as.data.table(suppressWarnings(mzR::chromatogramHeader(zF)))
    if (!is.null(chromsID)) cH <- cH[cH$chromatogramId %in% chromsID, ]
    if (nrow(cH) > 0) {
      cC <- mzR::chromatograms(zF, cH$chromatogramIndex)
      if (!is.data.frame(cC)) {
        names(cC) <- cH$chromatogramIndex
        cC <- rbindlist(cC, idcol = "index")
        cC$index <- as.numeric(cC$index)
        colnames(cC) <- c("index", "rt", "intensity")
        cH_b <- data.table(index = cH$chromatogramIndex, id = cH$chromatogramId)
        cH_n <- dplyr::inner_join(cH_b, cC, by = "index") #cH_b[cC, on = .(index = index)]
      } else {
        cH_n <- data.table(
          index = cH$chromatogramIndex,
          id = cH$chromatogramId,
          rt = cC[, 1],
          intensity = cC[, 2]
        )
      }
      dl[["chroms"]] <- cH_n
    } else {
      dl[["chroms"]] <- data.table()
    }
  }

  if (ifChromNoSpectra) {
    if ("chroms" %in% names(dl)) spectra <- FALSE
  }

  if (spectra) {
    zH <- header(zF)

    if (!is.null(rtr) & length(rtr) == 2) {
      rtr <- sort(rtr)
      zH <- zH[zH$retentionTime >= rtr[1] & zH$retentionTime <= rtr[2], ]
    }

    if (!is.null(level)) zH <- zH[zH$msLevel %in% level, ]

    if (nrow(zH) > 0) {
      zD <- mzR::peaks(zF, scans = zH$seqNum)
      zD <- lapply(zD, as.data.table)
      names(zD) <- zH$seqNum
      zD <- rbindlist(zD, idcol = "index")
      zD$index <- as.numeric(zD$index)
      if (2 %in% level) {
        zH_b <- data.table(
          index = zH$seqNum,
          scan = zH$acquisitionNum,
          lv = zH$msLevel,
          ce = zH$collisionEnergy,
          preScan = zH$precursorScanNum,
          preMZ = zH$precursorMZ,
          preCharge = zH$precursorCharge,
          rt = zH$retentionTime
        )
      } else {
        zH_b <- data.table(
          index = zH$seqNum,
          scan = zH$acquisitionNum,
          lv = zH$msLevel,
          rt = zH$retentionTime
        )
      }
      zH_n <- dplyr::inner_join(zH_b, zD, by = "index") #zH_b[zD, on = .(index = index)]
      dl[["spectra"]] <- zH_n
    } else {
      dl[["spectra"]] <- data.table()
    }
  }

  suppressWarnings(mzR::close(zF))
  return(dl)
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

    fl <- files(object)[x]

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
  xics[, .(analysis, replicate, id, mz_id, rt_id, mz, rt, intensity)]

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


#' @title extractMSn
#'
#' @description Extracts MSn spectra from defined isolated targets
#' defined by \emph{m/z} and retention time, including the respective deviations.
#'
#' @param object An \linkS4class{msData} or \linkS4class{msAnalysis} object.
#' @template args-single-analyses
#' @param level A numeric vector with length 1 to defined the MS level.
#' Currently, only level 2, corresponding to MS/MS, is possible.
#' @template args-makeTargets
#' @param settings A \linkS4class{settings} object with the parameter settings
#' for extracting and averaging MSn spectra.
#'
#' @return A \code{data.table} with the columns
#' \code{analysis}, \code{replicate}, \code{id}, \code{ce}, \code{preMZ}, \code{mz}, \code{intensity} and \code{precursor}
#' representing the analysis name (i.e., file), the analysis replicate name,
#' the isolation target id, the collision energy applied, the \emph{m/z} of the precursor and the MSn trace, the intensity and the presence of the precursor
#' (i.e., \emph{m/z} matching the isolation target), respectively.
#'
#' @export
#'
#' @importFrom fastcluster hclust
#' @importFrom checkmate assertClass
#' @importFrom data.table rbindlist setnames setorder as.data.table setcolorder data.table copy
#'
extractMSn <- function(
    object = NULL,
    analyses = NULL,
    level = 2,
    mz = NULL, ppm = 20,
    rt = NULL, sec = 60, id = NULL,
    settings = NULL) {

  valid <- FALSE

  if (checkmate::testClass(object, "msData") | checkmate::testClass(object, "msAnalysis"))
    valid = TRUE

  if (!valid) {
    warning("Invalid class object used as argument!")
    return(data.table())
  }

   if (checkmate::testClass(object, "msData")) {
     analyses <- checkAnalysesArgument(object, analyses)
     if (is.null(analyses)) return(data.table())
     object <- object[analyses]
   }

  targets <- makeTargets(mz, rt, ppm, sec, id)

  if (!is.null(settings)) {

    # TODO add default function to load extract MSn parameters
    #param <- fragmentSettingsDefault()

  }


  if (checkmate::testClass(settings, "settings")) {
    call <- getCall(settings)
    algorithm <- getAlgorithm(settings)
    settings <- getSettings(settings)

  } else {
    warning("Invalid settings for getting MSn data!")
    return(data.table())
  }

  settings$asPatRoon <- FALSE

  fls <- files(object)

  targets <- makeTargets(mz, rt, ppm, sec, id)


  # TODO make function to collect MS2 of a peak/feature
  # if (length(targets$mz) == 1) {
  #   if (targets$mz == 0) {
  #     targets <- features(object)
  #     targets <- targets[, .(id, mz, rt, mzmin, mzmax, rtmin, rtmax)]
  #   }
  # }

  isolationMassWindow <- settings$isolationMassWindow/2
  isolationTimeWindow <- settings$isolationTimeWindow
  targets <- targets[, `:=`(mzmin = mz - isolationMassWindow, mzmax = mz + isolationMassWindow)]
  targets <- targets[rt > 0, `:=`(rtmin = rtmin - isolationTimeWindow, rtmax = rtmax + isolationTimeWindow)][]

  # mlists <- list()
  #
  # dummy <- data.table(
  #   mz = numeric(),
  #   intensity = numeric(),
  #   seqNum = numeric(),
  #   ce = numeric(),
  #   preMZ = numeric()
  # )

  spt <- analysisTable(object)

  plists <- lapply(fls, function(
    x,
    targets,
    spt,
    level,
    minIntensityPre) {

      rtRange <- c(min(targets$rtmin) * 0.7, max(targets$rtmax) * 1.3)
      if (rtRange[1] == 0 & rtRange[2] == 0) rtRange <- NULL

      spectra <- loadRawDataMZR(x, level = c(1, 2), rtr = rtRange, chroms = FALSE)
      spectra <- spectra[["spectra"]]

      pHolder <- list()

      for (i in seq_len(nrow(targets))) {

        idf <- targets$id[i]

        pHolder[[idf]] <- list()

        msms <- copy(spectra[lv == level, ])

        msms <- msms[
          preMZ >= targets$mzmin[i] &
            preMZ <= targets$mzmax[i],
        ]

        if (targets$rt[i] > 0) {
          msms <- msms[
            rt >= targets$rtmin[i] &
              rt <= targets$rtmax[i],
          ]
        }

        ms <- copy(spectra[lv == (level - 1), ])
        ms <- ms[scan %in% unique(msms$preScan), ]

        msms <- msms[intensity >= settings$minIntensityPre, ]
        ms <- ms[intensity >= settings$minIntensityPre, ]

        msms[, id := idf]
        msms[, analysis := spt[file == x, analysis]]
        msms[, replicate := spt[file == x, replicate]]

        ms[, id := idf]
        ms[, analysis := spt[file == x, analysis]]
        ms[, replicate := spt[file == x, replicate]]

        pHolder[[idf]][["MS"]] <- ms
        pHolder[[idf]][["MSMS"]] <- msms
      }

      return(pHolder)
    },
    targets = targets,
    spt = spt,
    level = level,
    minIntensityPre = minIntensityPre
  )

  names(plists) <- spt[file %in% fls, analysis]

  #cat("Clustering spectra... \n")

  if (settings$asPatRoon) {

    cl_plists <- copy(plists)

    return(
      clusterMSnToPatRoon(
        cl_plists,
        mlists,
        targets,
        settings$clusteringMethod,
        settings$clusteringUnit,
        settings$clusteringWindow,
        settings$minIntensityPost
      )
    )

  } else {
    msnList <- lapply(plists, function(x) lapply(x, function(y) y[which(names(y) == "MSMS")]))
    msnList <- lapply(msnList, function(x) rbindlist(lapply(x, function(y) rbindlist(y, fill = TRUE)), fill = TRUE))
    msnList <- rbindlist(msnList, fill = TRUE)
    ids <- unique(msnList$id)

    if (length(ids) > 0) {
      msnList <- clusterMSn(
        ids,
        msnList,
        clusteringMethod = settings$clusteringMethod,
        clusteringUnit = settings$clusteringUnit,
        clusteringWindow = settings$clusteringWindow,
        mergeVoltages = settings$mergeVoltages,
        mergeBy = settings$mergeBy,
        targets
      )
    }

    msnList <- msnList[intensity >= settings$minIntensityPost, ]

    #cat("Done! \n")

    return(msnList)
  }
}


#' @title clusterMsn
#'
#' @description Function to cluster MSn data.
#'
#' @return A data table with clustered MSn data for given targets.
#'
#' @importFrom data.table copy setcolorder setorder
#' @importFrom fastcluster hclust
#'
clusterMSn <- function(
    ids,
    msnList,
    clusteringMethod,
    clusteringUnit,
    clusteringWindow,
    mergeVoltages,
    mergeBy,
    targets) {

  msnList <- lapply(ids, function(
    x,
    msnList,
    clusteringMethod,
    clusteringUnit,
    clusteringWindow,
    mergeVoltages,
    mergeBy,
    targets
  ) {

    t <- msnList[id == x, ]
    idf <- targets[id == x, ]

    if (nrow(t) > 2) {

      if (clusteringMethod == "distance") {
        setorder(t, mz)
        mzMat <- abs(diff(t$mz))
        if (clusteringUnit == "ppm") {
          mzMat <- (mzMat / t$mz[-1]) * 1E6
        }
        t[, cluster := 1 + c(0, cumsum(mzMat > clusteringWindow))]

      } else {
        mzMat <- dist(t$mz, method = clusteringMethod)
        if (clusteringUnit == "ppm") {
          mzMat <- as.data.table(as.matrix(mzMat))
          mzMat <- mzMat[, lapply(.SD, function(x, dt) x / t$mz * 1E6, dt = t), .SDcols = colnames(mzMat)]
          mzMat <- as.dist(mzMat)
        }
        hc <- fastcluster::hclust(mzMat, method = "complete")
        t[, cluster := cutree(hc, h = clusteringWindow)]

      }

      if ("ce" %in% colnames(t)) {
        if (any(t[, .(dup = anyDuplicated(index)), key = c("cluster", "analysis")][["dup"]] > 0)) {
          message(paste0("MSMS traces from the same spectrum were merged for ", idf$id, "\n"))
        }
      }

      if (is.null(mergeBy)) mergeBy <- "id"

      if (mergeVoltages) {

        if (mergeBy == "replicates" & "ce" %in% colnames(t)) {
          t <- t[, .(
            mz = mean(mz),
            intensity = sum(intensity) / length(unique(index)),
            ce = I(list(unique(ce))),
            preMZ = mean(preMZ)
          ), by = list(replicate, cluster)
          ][, cluster := NULL]

          t[mz >= idf$mzmin[1] & mz <= idf$mzmax[1], precursor := TRUE]
          t[is.na(precursor), precursor := FALSE]
          t[, id := idf$id]
          setorder(t, replicate, mz)
          setcolorder(t, c("replicate", "id", "mz", "intensity", "ce", "preMZ", "precursor"))

        } else if (mergeBy != "id"  & "ce" %in% colnames(t)) { #mergeBy analysis when mergeBy is not null
          t <- t[, .(
            mz = mean(mz),
            intensity = sum(intensity) / length(unique(index)),
            ce = I(list(unique(ce))),
            preMZ = mean(preMZ),
            replicate = unique(replicate)
          ), by = list(analysis, cluster)
          ][, cluster := NULL]

          t[mz >= idf$mzmin[1] & mz <= idf$mzmax[1], precursor := TRUE]
          t[is.na(precursor), precursor := FALSE]
          t[, id := idf$id]
          setorder(t, analysis, mz)
          setcolorder(t, c("analysis", "replicate", "id", "mz", "intensity", "ce", "preMZ", "precursor"))

        } else if ("ce" %in% colnames(t)) { #when NULL do not merge by analyses
          t <- t[, .(
            mz = mean(mz),
            intensity = sum(intensity) / length(unique(index)),
            ce = I(list(unique(ce))),
            preMZ = mean(preMZ)
          ), by = list(cluster)
          ][, cluster := NULL]

          t[mz >= idf$mzmin[1] & mz <= idf$mzmax[1], precursor := TRUE]
          t[is.na(precursor), precursor := FALSE]
          t[, id := idf$id]
          setorder(t, mz)
          setcolorder(t, c("id", "mz", "intensity", "ce", "preMZ", "precursor"))

        } else {
          t <- t[, .(
            mz = mean(mz),
            intensity = sum(intensity) / length(unique(index))
          ), by = list(cluster)
          ][, cluster := NULL]

          t[mz >= idf$mzmin[1] & mz <= idf$mzmax[1], precursor := TRUE]
          t[is.na(precursor), precursor := FALSE]
          t[, id := idf$id]
          setorder(t, mz)
          setcolorder(t, c("id", "mz", "intensity", "precursor"))
        }

      } else {

        if (mergeBy == "replicates") {
          t <- t[, .(
            mz = mean(mz),
            intensity = sum(intensity) / length(unique(index)),
            preMZ = mean(preMZ)
          ), by = list(ce, replicate, cluster)
          ][, cluster := NULL]

          t[mz >= idf$mzmin[1] & mz <= idf$mzmax[1], precursor := TRUE]
          t[is.na(precursor), precursor := FALSE]
          t[, id := idf$id]
          setorder(t, replicate, ce, mz)
          setcolorder(t, c("replicate", "id", "mz", "intensity", "ce", "preMZ", "precursor"))

        } else if (mergeBy != "id") { #mergeBy analyses when mergeBy is not null
          t <- t[, .(
            mz = mean(mz),
            intensity = sum(intensity) / length(unique(index)),
            preMZ = mean(preMZ),
            replicate = unique(replicate)
          ), by = list(ce, analysis, cluster)
          ][, cluster := NULL]

          t[mz >= idf$mzmin[1] & mz <= idf$mzmax[1], precursor := TRUE]
          t[is.na(precursor), precursor := FALSE]
          t[, id := idf$id]
          setorder(t, analysis, ce, mz)
          setcolorder(t, c("analysis", "replicate", "id", "mz", "intensity", "ce", "preMZ", "precursor"))

        } else { #when NULL do not merge by analyses
          t <- t[, .(
            mz = mean(mz),
            intensity = sum(intensity) / length(unique(index)),
            preMZ = mean(preMZ)
          ), by = list(ce, cluster)
          ][, cluster := NULL]

          t[mz >= idf$mzmin[1] & mz <= idf$mzmax[1], precursor := TRUE]
          t[is.na(precursor), precursor := FALSE]
          t[, id := idf$id]
          setorder(t, ce, mz)
          setcolorder(t, c("id", "mz", "intensity", "ce", "preMZ", "precursor"))
        }
      }
    } else if (nrow(t) == 1 | nrow(t) == 2) {

      if ("index" %in% colnames(t)) t[, index := NULL]

      if (mergeBy == "replicates" & "ce" %in% colnames(t)) {
        t[mz >= idf$mzmin[1] & mz <= idf$mzmax[1], precursor := TRUE]
        t[is.na(precursor), precursor := FALSE]
        t[, id := idf$id]
        t[, ce := I(ce)]
        setorder(t, replicate, mz)
        setcolorder(t, c("replicate", "id", "mz", "intensity", "ce", "preMZ", "precursor"))

      } else if (mergeBy != "id"  & "ce" %in% colnames(t)) { #mergeBy analyses when mergeBy is not null
        t[mz >= idf$mzmin[1] & mz <= idf$mzmax[1], precursor := TRUE]
        t[is.na(precursor), precursor := FALSE]
        t[, id := idf$id]
        t[, ce := I(ce)]
        setorder(t, analysis, mz)
        setcolorder(t, c("analysis", "replicate", "id", "mz", "intensity", "ce", "preMZ", "precursor"))

      } else if ("ce" %in% colnames(t)) { #when NULL do not merge by analyses
        t[mz >= idf$mzmin[1] & mz <= idf$mzmax[1], precursor := TRUE]
        t[is.na(precursor), precursor := FALSE]
        t[, id := idf$id]
        t[, ce := I(ce)]
        setorder(t, mz)
        setcolorder(t, c("id", "mz", "intensity", "ce", "preMZ", "precursor"))
      } else {
        t[mz >= idf$mzmin[1] & mz <= idf$mzmax[1], precursor := TRUE]
        t[is.na(precursor), precursor := FALSE]
        t[, id := idf$id][]
        setorder(t, mz)
        setcolorder(t, c("id", "mz", "intensity", "precursor"))
      }
    }

    return(t)
  },
  clusteringMethod = clusteringMethod,
  clusteringUnit = clusteringUnit,
  clusteringWindow = clusteringWindow,
  mergeVoltages = mergeVoltages,
  mergeBy = mergeBy,
  targets = targets,
  msnList = msnList)

  msnList <- rbindlist(msnList)

  return(msnList)
}


#' @title clusterMSnToPatRoon
#'
#' @description Function to cluster spectra and convert to
#' an \linkS4class{MSPeakLists} object.
#'
#' @return A \linkS4class{MSPeakLists} object with clustered data.
#'
#' @importFrom data.table copy setnames setcolorder setorder
#' @importClassesFrom patRoon MSPeakLists
#' @importFrom fastcluster hclust
#'
clusterMSnToPatRoon <- function(
    cl_plists,
    mlists,
    targets,
    clusteringMethod,
    clusteringUnit,
    clusteringWindow,
    minIntensityPost) {

  av_plists <- list()

  #cat("Clustering MS/MS for peaks...")

  LoopLength <- sum(lengths(cl_plists))

  pb <- txtProgressBar(
    min = 0,
    max = LoopLength,
    style = 3,
    width = 50,
    char = "="
  )

  loopN <- 0

  for (i in names(cl_plists)) {
    for (f in names(cl_plists[[i]])) {

      loopN <- loopN + 1

      av_ms <- clusterMSn(
        ids = f,
        msnList = cl_plists[[i]][[f]]$MS,
        clusteringMethod,
        clusteringUnit,
        clusteringWindow,
        mergeVoltages = TRUE,
        mergeBy = "analysis",
        targets
      )

      av_ms <- av_ms[intensity > minIntensityPost, ]
      av_ms[, analysis := i]

      av_msms <- clusterMSn(
        ids = f,
        msnList = cl_plists[[i]][[f]]$MSMS,
        clusteringMethod,
        clusteringUnit,
        clusteringWindow,
        mergeVoltages = TRUE,
        mergeBy = "analysis",
        targets
      )

      av_msms <- av_msms[intensity > minIntensityPost, ]

      av_plists[[f]][["MS"]] <- c(av_plists[[f]][["MS"]], list(av_ms))
      av_plists[[f]][["MSMS"]] <- c(av_plists[[f]][["MSMS"]], list(av_msms))

      cl_plists[[i]][[f]]$MS <- copy(av_ms)
      cl_plists[[i]][[f]]$MSMS <- copy(av_msms)

      setnames(cl_plists[[i]][[f]]$MS, "id", "ID")
      cl_plists[[i]][[f]]$MS[, analysis := NULL]
      cl_plists[[i]][[f]]$MS[, ID := seq_len(nrow(cl_plists[[i]][[f]]$MS))]

      setnames(cl_plists[[i]][[f]]$MSMS, "id", "ID")
      cl_plists[[i]][[f]]$MSMS[, analysis := NULL]
      cl_plists[[i]][[f]]$MSMS[, replicate := NULL]
      cl_plists[[i]][[f]]$MSMS[, ce := NULL]
      cl_plists[[i]][[f]]$MSMS[, preMZ := NULL]
      cl_plists[[i]][[f]]$MSMS[, ID := seq_len(nrow(cl_plists[[i]][[f]]$MSMS))]

      setTxtProgressBar(pb, loopN)
    }
  }

  #cat("Done! \n")
  close(pb)

  #cat("Clustering and averaging MS/MS for features...")

  LoopLength2 <- sum(lengths(av_plists))

  pb2 <- txtProgressBar(
    min = 0,
    max = LoopLength2,
    style = 3,
    width = 50,
    char = "="
  )

  loopN2 <- 0

  for (f in names(av_plists)) {
    for (lv in names(av_plists[[f]])) {

      loopN2 <- loopN2 + 1

      t <- av_plists[[f]][[lv]]
      t <- t[sapply(t, nrow) > 0]
      t <- copy(rbindlist(t, fill = TRUE))
      idf <- targets[id == f, ]

      if (nrow(t) > 2) {

        if (clusteringMethod == "distance") {
          setorder(t, mz)
          mzMat <- abs(diff(t$mz))
          if (clusteringUnit == "ppm") {
            mzMat <- (mzMat / t$mz[-1]) * 1E6
          }
          t[, cluster := 1 + c(0, cumsum(mzMat > clusteringWindow))]

        } else {
          mzMat <- dist(t$mz, method = clusteringMethod)
          if (clusteringUnit == "ppm") {
            mzMat <- as.data.table(as.matrix(mzMat))
            mzMat <- mzMat[, lapply(.SD, function(x, dt) x / t$mz * 1E6, dt = t), .SDcols = colnames(mzMat)]
            mzMat <- as.dist(mzMat)
          }
          hc <- fastcluster::hclust(mzMat, method = "complete")
          t[, cluster := cutree(hc, h = clusteringWindow)]

        }

        if ("ce" %in% colnames(t)) {
          t <- t[, .(
            mz = mean(mz),
            intensity = sum(intensity) / length(unique(analysis))
          ), by = list(cluster)
          ][, cluster := NULL]

          t[mz >= idf$mzmin[1] & mz <= idf$mzmax[1], precursor := TRUE]
          t[is.na(precursor), precursor := FALSE]
          t <- t[intensity > minIntensityPost, ]
          setorder(t, mz)
          t[, ID := seq_len(nrow(t))][]
          setcolorder(t, c("ID", "mz", "intensity", "precursor"))

        } else {
          t <- t[, .(
            mz = mean(mz),
            intensity = sum(intensity) / length(unique(analysis))
          ), by = list(cluster)
          ][, cluster := NULL]

          t[mz >= idf$mzmin[1] & mz <= idf$mzmax[1], precursor := TRUE]
          t[is.na(precursor), precursor := FALSE]
          t <- t[intensity > minIntensityPost, ]
          setorder(t, mz)
          t[, ID := seq_len(nrow(t))]
          setcolorder(t, c("ID", "mz", "intensity", "precursor"))
        }
      }

      av_plists[[f]][[lv]] <- copy(t)

      setTxtProgressBar(pb2, loopN2)
    }
  }

  #cat("Done! \n")
  close(pb2)

  final_plists <- new("MSPeakLists",
                      peakLists = cl_plists,
                      metadata = mlists,
                      averagedPeakLists = av_plists,
                      avgPeakListArgs = list(),
                      origFGNames = names(av_plists),
                      algorithm = "mzr"
  )

  #cat("Done! \n")

  final_plists@averagedPeakLists <- av_plists

  return(final_plists)
}


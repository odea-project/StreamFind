

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
      mzmin = 0,
      mzmax = 0,
      rtmin = 0,
      rtmax = 0
    )
    mzrts[, mzmin := mz - ((ppm / 1E6) * mz)]
    mzrts[, mzmax := mz + ((ppm / 1E6) * mz)]

    if (is.vector(rt) & length(rt) == length(mz)) {
      mzrts$rt <- rt
      mzrts[, rtmin := rt - sec]
      mzrts[, rtmax := rt + sec]
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

  targets <- makeTargets(mz, rt, ppm, sec, id)
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
#' representing the analysis name, the sample replicate name,
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
  #setcolorder(xics, c("analysis", "replicate", "id", "mz_id", "rt_id", "mz", "rt", "intensity"))

  # TODO implemented check for rt alignment
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

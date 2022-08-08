
#' @title msAnalysisLoadMetadata
#'
#' @description Creates a \linkS4class{msAnalysis} for each mzML/mzXML file
#' in a given data.frame with columns "file", "replicate" and "blank".
#' The function uses code from the \pkg{RaMS} package to obtain file metadata
#' using the \pkg{xml2}.
#'
#' @param file_df A data.frame with the columns "file", "replicate" and "blank"
#' for each file to be converted to a \linkS4class{msAnalysis}.
#'
#' @noRd
msAnalysisLoadMetadata <- function(file_df) {

  file_df <- split(file_df, file_df$file)

  # TODO Add parallel processing globally
  # 1. Check if number of files and workers
  # are high enough to add parallel processing
  # 2. Check if has linux or win to run show or fork
  # make global function for it

  #bpp <- SerialParam(progressbar = TRUE)
  #bpp <- SnowParam(progressbar = TRUE)

  analyses <- lapply(file_df, function(x) {

    if (!requireNamespace("RaMS", quietly = TRUE)) {
      warning("Package RaMS not installed!")
      return(NULL)
    }

    ana <- list()

    fl <- x$file
    if (grepl(".mzML", fl)) meta <- mzMLGrabMetadata(fl)
    if (grepl(".mzXML", fl)) meta <- mzXMLGrabMetadata(fl)

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

    ana$metadata <- meta

    return(ana)

  }) #BPPARAM = bpp

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



#' @title mzMLGrabMetadata
#'
#' @description Code adapted from package \pkg{RaMS} developed by
#' William Kumler. See more details in:
#' \href{https://github.com/wkumler/RaMS}.
#'
#' @param fl An mzML file.
#'
#' @importFrom xml2 read_xml xml_find_first xml_attr xml_find_all xml_name
#'
#' @noRd
mzMLGrabMetadata <- function(fl) {

  xml_data <- xml2::read_xml(fl)

  source_node <- xml2::xml_find_first(xml_data, xpath = "//d1:sourceFile")
  if (length(source_node) > 0) {
    source_file <- xml2::xml_attr(source_node, "name")
  } else {
    source_file <- "None found"
  }

  inst_xpath <- "//d1:referenceableParamGroup/d1:cvParam"
  inst_nodes <- xml2::xml_find_first(xml_data, xpath = inst_xpath)
  if (length(inst_nodes) > 0) {
    inst_val <- xml2::xml_attr(inst_nodes, "name")
  } else {
    inst_val <- "None found"
  }

  config_xpath <- "//d1:componentList/child::node()"
  config_nodes <- xml2::xml_find_all(xml_data, xpath = config_xpath)
  if (length(inst_nodes) > 0) {
    config_types <- xml2::xml_name(config_nodes)
    config_order <- xml2::xml_attr(config_nodes, "order")
    config_name_nodes <- xml2::xml_find_first(config_nodes, "d1:cvParam")
    config_names <- xml2::xml_attr(config_name_nodes, "name")
  } else {
    config_types <- "None found"
    config_order <- "None found"
    config_names <- "None found"
  }

  time_node <- xml2::xml_find_first(xml_data, xpath = "//d1:run")
  time_val <- xml2::xml_attr(time_node, "startTimeStamp")
  if (!is.na(time_val)) {
    time_stamp <- as.POSIXct(strptime(time_val, "%Y-%m-%dT%H:%M:%SZ"))
  } else {
    time_stamp <- as.POSIXct(NA)
  }

  # spectra_xpath <- '//d1:spectrum'
  # spectra_nodes <- xml2::xml_find_all(xml_data, xpath = spectra_xpath)
  # if (length(spectra_nodes) > 0) {
  #   number_spectra <- length(spectra_nodes)
  # } else {
  #   number_spectra <- 0
  # }

  mslevel_xpath <- '//d1:spectrum/d1:cvParam[@name="ms level"]'
  mslevel_nodes <- xml2::xml_find_all(xml_data, xpath = mslevel_xpath)
  number_spectra <- length(mslevel_nodes)
  if (length(mslevel_nodes) > 0) {
    ms_levels <- paste0(unique(xml2::xml_attr(mslevel_nodes, "value")),
                       collapse = ", ")
  } else {
    ms_levels <- "None found"
  }

  centroided_xpath <- '//d1:spectrum/d1:cvParam[@name="centroid spectrum"]'
  #profile_xpath <- '//d1:spectrum/d1:cvParam[@name="profile spectrum"]'
  centroided_nodes <- xml2::xml_find_all(xml_data, xpath = centroided_xpath)
  #profile_nodes <- xml2::xml_find_all(xml_data, xpath = profile_xpath)
  if (length(centroided_nodes) > 0) {
    centroided <- TRUE
  } else {
    centroided <- FALSE
  }

  polarity_xpath <- '//d1:spectrum/d1:cvParam[@accession="MS:1000130"]'
  polarity_nodes <- xml2::xml_find_all(xml_data, polarity_xpath)
  if (length(polarity_nodes) > 0) {
    polarities <- unique(
      gsub(" scan", "", xml2::xml_attr(polarity_nodes, "name"))
    )
  } else {
    polarities <- "None found"
  }

  chrom_xpath <- '//d1:chromatogram'
  chrom_nodes <- xml2::xml_find_all(xml_data, chrom_xpath)
  number_chromatograms <- length(chrom_nodes)

  meta <- list(
    "source_file" = source_file,
    "inst_data" = inst_val,
    "config_data" = list(data.frame(
      order = config_order,
      type = config_types,
      name = config_names
    )),
    "time_stamp" = time_stamp,
    "number_spectra" = number_spectra,
    "ms_levels" = ms_levels,
    "centroided" = centroided,
    "polarity" = polarities,
    "number_chromatograms" = number_chromatograms
  )

  return(meta)
}



#' @title mzXMLGrabMetadata
#'
#' @description Code adapted from package \pkg{RaMS} developed by
#' William Kumler. See more details in:
#' \href{https://github.com/wkumler/RaMS}.
#'
#' @param fl An mzXML file.
#'
#' @importFrom xml2 read_xml xml_find_first xml_attr xml_find_all xml_name
#'
#' @noRd
mzXMLGrabMetadata <- function(fl){

  xml_data <- xml2::read_xml(fl)

  source_node <- xml2::xml_find_first(xml_data, xpath = "//d1:parentFile")
  if (length(source_node) > 0) {
    source_file <- basename(xml2::xml_attr(source_node, "fileName"))
  } else {
    source_file <- "None found"
  }

  inst_xpath <- "//d1:msInstrument/child::node()[starts-with(name(), 'ms')]"
  inst_nodes <- xml2::xml_find_all(xml_data, xpath = inst_xpath)
  if (length(inst_nodes) > 0) {
    inst_names <- xml2::xml_attr(inst_nodes, "category")
    inst_vals <- xml2::xml_attr(inst_nodes, "value")
    names(inst_vals) <- inst_names
  } else {
    inst_vals <- "None found"
    names(inst_vals) <- "Instrument data"
  }

  mslevel_nodes <- xml2::xml_find_all(xml_data, xpath = "//d1:scan")
  if (length(mslevel_nodes) > 0) {
    mslevels <- paste0(unique(xml2::xml_attr(mslevel_nodes, "msLevel")),
                       collapse = ", ")
  } else {
    mslevels <- "None found"
  }

  polarity_nodes <- xml2::xml_find_all(xml_data, xpath = "//d1:scan")
  if (length(polarity_nodes) > 0) {
    polarities <- paste0(unique(xml2::xml_attr(polarity_nodes, "polarity")),
                         collapse = ", ")
  } else {
    polarities <- "None found"
  }

  meta <- list(
    source_file = source_file,
    inst_data = list(inst_vals),
    mslevels = mslevels,
    polarity = polarities
  )

  return(meta)
}



#' @importFrom xml2 read_xml xml_find_first xml_attr xml_find_all xml_name
#' @importFrom data.table data.table
#'
mzMLloadBasicSpectraHeader <- function(fl) {

  xml_data <- xml2::read_xml(fl)

  spectra_x <- '//d1:spectrum'
  spectra_n <- xml2::xml_find_all(xml_data, xpath = spectra_x)

  if (length(spectra_n) > 0) {

    index <- as.numeric(xml2::xml_attr(spectra_n, "index"))
    scan <- as.numeric(gsub("\\D", "", xml2::xml_attr(spectra_n, "id")))

    lv_x <- 'd1:cvParam[@name="ms level"]'
    lv <- xml2::xml_find_all(spectra_n, xpath = lv_x)
    lv <- as.numeric(xml2::xml_attr(lv, "value"))

    rt_x <- 'd1:scanList/d1:scan/d1:cvParam[@name="scan start time"]'
    rt <- xml2::xml_find_all(spectra_n, xpath = rt_x)
    rt <- as.numeric(xml2::xml_attr(rt, "value"))

    lvs <- unique(lv)

    if (2 %in% lvs) {

      total_scans <- length(scan)
      ce <- rep(NA_real_, total_scans)
      preScan <- rep(NA_real_, total_scans)
      preMZ <- rep(NA_real_, total_scans)

      for (i in lvs[-1]) {

        msn_x <- paste0('//d1:spectrum[d1:cvParam[@name="ms level" and @value="', i, '"]]')
        msn_n <- xml2::xml_find_all(xml_data, xpath = msn_x)

        msn_scan <- as.numeric(gsub("\\D", "", xml2::xml_attr(msn_n, "id")))

        preScan_x <- paste0('d1:precursorList/d1:precursor')
        preScan_v <- xml2::xml_find_all(msn_n, preScan_x)
        preScan_v <- xml2::xml_attr(preScan_v, "spectrumRef")
        preScan_v <- as.numeric(gsub("\\D", "", preScan_v))
        preScan[scan %in% msn_scan] <- preScan_v

        ce_x <- paste0('d1:precursorList/d1:precursor/d1:activation',
                           '/d1:cvParam[@name="collision energy"]')
        ce_v <- xml2::xml_find_all(msn_n, ce_x)
        ce_v <- as.integer(xml2::xml_attr(ce_v, "value"))
        ce[scan %in% msn_scan] <- ce_v

        preMZ_x <- paste0('d1:precursorList/d1:precursor/d1:selectedIonList',
                              '/d1:selectedIon/d1:cvParam[@name="selected ion m/z"]')
        preMZ_v <- xml2::xml_find_all(msn_n, preMZ_x)
        preMZ_v <- as.numeric(xml2::xml_attr(preMZ_v, "value"))
        preMZ[scan %in% msn_scan] <- preMZ_v
      }

      df_a <- data.table::data.table(
        "index" = index,
        "scan" = scan,
        "lv" = lv,
        "ce" = ce,
        "preScan" = preScan,
        "preMZ" = preMZ,
        "rt" = rt
      )

    } else {

      df_a <- data.table::data.table(
        "index" = index,
        "scan" = scan,
        "lv" = lv,
        "rt" = rt
      )
    }

  } else {

    df_a <- data.table::data.table(
      "index" = numeric(),
      "scan" = numeric(),
      "lv" = numeric(),
      "rt" = numeric()
    )
  }

  return(df_a)
}



#' @importFrom data.table as.data.table
#' @importFrom mzR openMSfile header close
#'
loadBasicRawSpectraHeaderMZR <- function(fl) {

  zF <- openMSfile(fl, backend = "pwiz")

  zH <- as.data.table(header(zF))

  if (nrow(zH) > 0) {
    zH <- zH[, .(seqNum, acquisitionNum, msLevel, retentionTime)]
    colnames(zH) <- c("index", "scan", "lv", "rt")

  } else {
    zH <- data.table(
      index = numeric(),
      scan = numeric(),
      lv = numeric(),
      rt = numeric()
    )
  }

  suppressWarnings(mzR::close(zF))

  return(zH)
}



#' @importFrom mzR openMSfile
#' @importClassesFrom mzR mzRpwiz
#' @importMethodsFrom mzR header peaks chromatogramHeader chromatograms
#' @importFrom data.table data.table as.data.table rbindlist copy
#' @importFrom dplyr inner_join
#'
loadRawDataMZR <- function(file, spectra = TRUE, level = 1, rtr = NULL,
                           minIntensityMS1 = 0, minIntensityMS2 = 0,
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
        cH_n <- dplyr::inner_join(cH_b, cC, by = "index")

      } else {
        cH_n <- data.table(
          index = cH$chromatogramIndex,
          id = cH$chromatogramId,
          rt = cC[, 1],
          intensity = cC[, 2]
        )
      }

      cH_n <- cH_n[intensity > minIntensityMS1, ]

      dl[["chroms"]] <- cH_n

    } else {
      dl[["chroms"]] <- data.table(
        index = numeric(),
        rt = numeric(),
        intensity = numeric(),
        id = character()
      )
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

      if (TRUE %in% (level == 2)) {
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

      zH_n <- dplyr::inner_join(zH_b, zD, by = "index")

      # TODO add an intensity threshold when loading raw data
      #removes empty traces
      zH_n <- zH_n[!(intensity <= minIntensityMS1 & lv == 1), ]
      zH_n <- zH_n[!(intensity <= minIntensityMS2 & lv == 2), ]

      dl[["spectra"]] <- zH_n

    } else {
      dl[["spectra"]] <- data.table(
        index = numeric(),
        scan = numeric(),
        lv = numeric(),
        rt = numeric(),
        mz = numeric(),
        intensity = numeric()
      )
    }
  }

  suppressWarnings(mzR::close(zF))

  return(dl)
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

  fls <- filePaths(object)

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
#' @param ids ...
#' @param msnList ...
#' @param clusteringMethod ...
#' @param clusteringUnit ...
#' @param clusteringWindow ...
#' @param mergeVoltages ...
#' @param mergeBy ...
#' @param targets ...
#'
#' @return A data table with clustered MSn data for given targets.
#'
#' @importFrom data.table copy setcolorder setorder
#' @importFrom fastcluster hclust
#' @importFrom stats as.dist cutree dist filter na.omit rt setNames var
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
        hc <- hclust(mzMat, method = "complete")
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
#' @param cl_plists ...
#' @param mlists ...
#' @param targets ...
#' @param clusteringMethod ...
#' @param clusteringUnit ...
#' @param clusteringWindow ...
#' @param minIntensityPost ...
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


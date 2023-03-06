#' parse_ms_spectra
#'
#' @description Parses spectra from mzML or mzXML files.
#'
#' @param files X.
#' @param levels X.
#' @param targets X.
#' @param allTraces X.
#' @param isolationWindow X.
#' @param runParallel X.
#' @param minIntensityMS1 X.
#' @param minIntensityMS2 X.
#'
#' @return A list with a spectra `data.frame` for each file in `files`.
#' On error, returns \code{NULL}.
#'
#' @export
#'
parse_ms_spectra <- function(files = NA_character_, levels = c(1, 2),
                             targets = NULL, allTraces = TRUE,
                             isolationWindow = 1.3, runParallel = FALSE,
                             minIntensityMS1 = 0, minIntensityMS2 = 0) {

  i = NULL
  spec_list = NULL
  with_targets <- FALSE

  possible_ms_file_formats <- ".mzML|.mzXML"
  valid_files <- vapply(files,
    FUN.VALUE = FALSE,
    function(x, possible_ms_file_formats) {
      if (!file.exists(x)) {
        return(FALSE)
      }
      if (FALSE %in% grepl(possible_ms_file_formats, x)) {
        return(FALSE)
      }
      TRUE
    }, possible_ms_file_formats = possible_ms_file_formats
  )

  if (all(valid_files)) {

    if (!any(is.numeric(minIntensityMS1) | is.integer(minIntensityMS1))) {
      minIntensityMS1 <- 0
    }

    if (!any(is.numeric(minIntensityMS2) | is.integer(minIntensityMS2))) {
      minIntensityMS1 <- 0
    }

    if (!2 %in% levels) allTraces <- TRUE

    if (!is.logical(allTraces)) allTraces <- TRUE

    if (!allTraces) {
      if (!any(is.numeric(isolationWindow) | is.integer(isolationWindow))) {
        isolationWindow <- 0
      }
      preMZr <- targets[, c("mzmin", "mzmax")]
      preMZr$mzmin <- preMZr$mzmin - (isolationWindow / 2)
      preMZr$mzmax <- preMZr$mzmax + (isolationWindow / 2)
      if (nrow(preMZr) == 1 & TRUE %in% (targets$mzmax == 0)) {
        preMZr <- NULL
      }
    } else {
      preMZr <- NULL
    }

    if (!is.logical(runParallel)) runParallel <- FALSE

    if (runParallel & length(files) > 1) {
      workers <- parallel::detectCores() - 1
      if (length(files) < workers) workers <- length(files)
      par_type <- "PSOCK"
      if (parallelly::supportsMulticore()) par_type <- "FORK"
      cl <- parallel::makeCluster(workers, type = par_type)
      doParallel::registerDoParallel(cl)
      # on.exit(parallel::stopCluster(cl))
    } else {
      registerDoSEQ()
    }

    # with mzR -----
    if (requireNamespace("mzR")) {
      spec_list <- foreach(i = files, .packages = "mzR") %dopar% {

        if (!is.null(targets)) {
          with_targets <- TRUE

          trim <- function(v, a, b) {
            rowSums(mapply(function(a, b) v >= a & v <= b, a = a, b = b)) > 0
          }

          trim_targets <- function(traces, targets, preMZr) {
            trim <- function(v, a, b) {
              rowSums(mapply(function(a, b) v >= a & v <= b, a = a, b = b)) > 0
            }
            tg_list <- lapply(seq_len(nrow(targets)),
              function(z, traces, targets, trim, preMZr) {
                tg <- traces
                cutRt <- trim(tg$rt, targets$rtmin[z], targets$rtmax[z])
                tg <- tg[cutRt, ]
                if (nrow(tg) > 0) {
                  if (!is.null(preMZr)) {
                    cutMZ <- trim(tg$mz, targets$mzmin[z], targets$mzmax[z])
                    tg <- tg[tg$level == 2 | (tg$level == 1 & cutMZ), ]
                    if (nrow(tg) > 0) {
                      cutPreMZ <- trim(tg$preMZ, preMZr$mzmin[z], preMZr$mzmax[z])
                      tg <- tg[tg$level == 1 | (tg$level == 2 & cutPreMZ), ]
                    }
                  } else {
                    cutMZ <- trim(tg$mz, targets$mzmin[z], targets$mzmax[z])
                    tg <- tg[cutMZ, ]
                  }
                }
                if (nrow(tg) > 0) {
                  tg$id <- targets$id[z]
                } else {
                  tg$id <- character()
                }
                tg
              },
              traces = traces, preMZr = preMZr, targets = targets, trim = trim
            )
            tg_df <- do.call("rbind", tg_list)
            tg_df
          }
        }

        file_link <- mzR::openMSfile(i, backend = "pwiz")

        sH <- mzR::header(file_link)

        if (nrow(sH) > 0) {
          if (max(sH$retentionTime) < 60) {
            sH$retentionTime <- sH$retentionTime * 60
          }

          if (!is.null(levels)) sH <- sH[sH$msLevel %in% levels, ]

          if (with_targets) {
            if ("analysis" %in% colnames(targets)) {
              ana_name <- gsub(".mzML|.mzXML", "", basename(i))
              tp_tar <- targets[targets$analysis %in% ana_name, ]
              if (nrow(tp_tar) > 0) {
                sH <- sH[trim(sH$retentionTime, tp_tar$rtmin, tp_tar$rtmax), ]
              } else {
                sH <- data.frame()
              }
            } else {
              sH <- sH[trim(sH$retentionTime, targets$rtmin, targets$rtmax), ]
            }
          }

          if (!is.null(preMZr) & with_targets) {
            if ("analysis" %in% colnames(targets)) {
              ana_name <- gsub(".mzML|.mzXML", "", basename(i))
              pre_tar <- preMZr[targets$analysis %in% ana_name, ]
              preMZ_check <- trim(sH$precursorMZ, pre_tar$mzmin, pre_tar$mzmax)
              sH <- sH[(preMZ_check %in% TRUE) | is.na(preMZ_check), ]
            } else {
              preMZ_check <- trim(sH$precursorMZ, preMZr$mzmin, preMZr$mzmax)
              sH <- sH[(preMZ_check %in% TRUE) | is.na(preMZ_check), ]
            }
          }

          if (nrow(sH) > 0) {
            scans <- mzR::peaks(file_link, scans = sH$seqNum)

            mat_idx <- rep(sH$seqNum, sapply(scans, nrow))
            scans <- as.data.frame(do.call(rbind, scans))
            scans$index <- mat_idx

            if (TRUE %in% (unique(sH$msLevel) == 2)) {
              sH_b <- data.frame(
                "index" = sH$seqNum,
                "scan" = sH$acquisitionNum,
                "level" = sH$msLevel,
                "ce" = sH$collisionEnergy,
                "preScan" = sH$precursorScanNum,
                "preMZ" = sH$precursorMZ,
                "rt" = sH$retentionTime
              )
            } else {
              sH_b <- data.frame(
                "index" = sH$seqNum,
                "scan" = sH$acquisitionNum,
                "level" = sH$msLevel,
                "rt" = sH$retentionTime
              )
            }

            if (!all(is.na(sH$ionMobilityDriftTime))) {
              rt_unique <- unique(sH_b$rt)
              frame_numbers <- seq_len(length(rt_unique))
              if ("preMZ" %in% colnames(sH_b)) sH_b$preMZ <- NA_real_
              sH_b$frame <- factor(sH_b$rt,
                                   levels = rt_unique, labels = frame_numbers
              )
              sH_b$driftTime <- sH$ionMobilityDriftTime
            }

            sH <- merge(sH_b, scans, by = "index")

            if (with_targets) {
              if ("analysis" %in% colnames(targets)) {
                ana_name <- gsub(".mzML|.mzXML", "", basename(i))
                tp_tar <- targets[targets$analysis %in% ana_name, ]
                if (!is.null(preMZr)) {
                  pre_tar <- preMZr[targets$analysis %in% ana_name, ]
                } else {
                  pre_tar <- NULL
                }
                if (nrow(tp_tar) > 0) {
                  sH <- trim_targets(sH, tp_tar, pre_tar)
                } else {
                  sH <- data.frame()
                }
              } else {
                sH <- trim_targets(sH, targets, preMZr)
              }
            }

            if (exists("file_link")) suppressWarnings(mzR::close(file_link))
            sH

          } else {
            data.frame()
          }
        } else {
          data.frame()
        }
      }

  ## with xml2 -----
    } else if (requireNamespace("xml2")) {
      spec_list <- foreach(i = files, .packages = "xml2") %dopar% {

        if (!is.null(targets)) {
          with_targets <- TRUE

          trim <- function(v, a, b) {
            rowSums(mapply(function(a, b) v >= a & v <= b, a = a, b = b)) > 0
          }

          trim_targets <- function(traces, targets, preMZr) {
            trim <- function(v, a, b) {
              rowSums(mapply(function(a, b) v >= a & v <= b, a = a, b = b)) > 0
            }
            tg_list <- lapply(seq_len(nrow(targets)),
              function(z, traces, targets, trim, preMZr) {
                tg <- traces
                cutRt <- trim(tg$rt, targets$rtmin[z], targets$rtmax[z])
                tg <- tg[cutRt, ]
                if (nrow(tg) > 0) {
                  if (!is.null(preMZr)) {
                    cutMZ <- trim(tg$mz, targets$mzmin[z], targets$mzmax[z])
                    tg <- tg[tg$level == 2 | (tg$level == 1 & cutMZ), ]
                    if (nrow(tg) > 0) {
                      cutPreMZ <- trim(tg$preMZ, preMZr$mzmin[z], preMZr$mzmax[z])
                      tg <- tg[tg$level == 1 | (tg$level == 2 & cutPreMZ), ]
                    }
                  } else {
                    cutMZ <- trim(tg$mz, targets$mzmin[z], targets$mzmax[z])
                    tg <- tg[cutMZ, ]
                  }
                }
                if (nrow(tg) > 0) {
                  tg$id <- targets$id[z]
                } else {
                  tg$id <- character()
                }
                tg
              },
              traces = traces, preMZr = preMZr, targets = targets, trim = trim
            )
            tg_df <- do.call("rbind", tg_list)
            tg_df
          }
        }

        xml_data <- read_xml(i)

        ### for mzML ----
        if (grepl("mzML", i)) {

          spectra_x <- '//d1:spectrum'
          spectra_n <- xml_find_all(xml_data, xpath = spectra_x)

          if (length(spectra_n) > 0) {

            level_x <- 'd1:cvParam[@name="ms level"]'
            level <- xml_find_all(spectra_n, xpath = level_x)
            level <- as.integer(xml_attr(level, "value"))

            if (!is.null(levels)) {
              check_level <- level %in% levels
              level <- level[check_level]
              spectra_n <- spectra_n[check_level]
            }

            rt_x <- 'd1:scanList/d1:scan/d1:cvParam[@name="scan start time"]'
            rt <- xml_find_all(spectra_n, xpath = rt_x)
            unit <- unique(xml_attr(rt, "unitName"))
            rt <- as.numeric(xml_attr(rt, "value"))
            if ("minute" %in% unit) rt = rt * 60

            if (with_targets) {
              if ("analysis" %in% colnames(targets)) {
                ana_name <- gsub(".mzML|.mzXML", "", basename(i))
                tp_tar <- targets[targets$analysis %in% ana_name, ]
                if (nrow(tp_tar) > 0) {
                  check_rt <- trim(rt, tp_tar$rtmin, tp_tar$rtmax)
                } else {
                  check_rt <- rep(FALSE, length(rt))
                }
              } else {
                check_rt <- trim(rt, targets$rtmin, targets$rtmax)
              }
              rt <- rt[check_rt]
              level <- level[check_rt]
              spectra_n <- spectra_n[check_rt]
            }

            index <- as.numeric(xml_attr(spectra_n, "index"))
            scan <- as.numeric(gsub("\\D", "", xml_attr(spectra_n, "id")))
            level_unique <- sort(unique(level))

            if (TRUE %in% (level_unique >= 2)) {
              total_scans <- length(scan)
              ce <- rep(NA_real_, total_scans)
              preScan <- rep(NA_real_, total_scans)
              preMZ <- rep(NA_real_, total_scans)

              for (lv in level_unique[!level_unique %in% 1]) {
                msn_x <- paste0('//d1:spectrum[d1:cvParam[@name="ms level" and @value="', lv, '"]]')
                msn_n <- xml_find_all(xml_data, xpath = msn_x)
                msn_scan <- as.numeric(gsub("\\D", "", xml_attr(msn_n, "id")))
                preScan_x <- paste0('d1:precursorList/d1:precursor')
                preScan_v <- xml_find_all(msn_n, preScan_x)
                preScan_v <- xml_attr(preScan_v, "spectrumRef")
                preScan_v <- as.numeric(gsub("\\D", "", preScan_v))
                preScan[scan %in% msn_scan] <- preScan_v[msn_scan %in% scan]
                ce_x <- paste0('d1:precursorList/d1:precursor/d1:activation',
                               '/d1:cvParam[@name="collision energy"]')
                ce_v <- xml_find_all(msn_n, ce_x)
                ce_v <- as.integer(xml_attr(ce_v, "value"))
                ce[scan %in% msn_scan] <- ce_v[msn_scan %in% scan]
                preMZ_x <- paste0('d1:precursorList/d1:precursor/d1:selectedIonList',
                                  '/d1:selectedIon/d1:cvParam[@name="selected ion m/z"]')
                preMZ_v <- xml_find_all(msn_n, preMZ_x)
                preMZ_v <- as.numeric(xml_attr(preMZ_v, "value"))
                preMZ[scan %in% msn_scan] <- preMZ_v[msn_scan %in% scan]
              }

              if (!is.null(preMZr)) {
                if ("analysis" %in% colnames(targets)) {
                  ana_name <- gsub(".mzML|.mzXML", "", basename(i))
                  pre_tar <- preMZr[targets$analysis %in% ana_name, ]
                  if (nrow(pre_tar) > 0) {
                    check_preMZ <- trim(preMZ, pre_tar$mzmin, pre_tar$mzmax)
                  } else {
                    check_preMZ <- rep(FALSE, length(preMZ))
                  }
                } else {
                  check_preMZ <- trim(preMZ, preMZr$mzmin, preMZr$mzmax)
                }
                spectra_n <- spectra_n[check_preMZ]
                index <- index[check_preMZ]
                scan <- scan[check_preMZ]
                level <- level[check_preMZ]
                ce <- ce[check_preMZ]
                preScan <- preScan[check_preMZ]
                preMZ <- preMZ[check_preMZ]
                rt <- rt[check_preMZ]
              }

              df_a <- data.frame(
                "index" = index,
                "scan" = scan,
                "level" = level,
                "ce" = ce,
                "preScan" = preScan,
                "preMZ" = preMZ,
                "rt" = rt
              )
            } else {
              df_a <- data.frame(
                "index" = index,
                "scan" = scan,
                "level" = level,
                "rt" = rt
              )
            }

            comp_x <- paste0('//d1:cvParam[@accession="MS:1000574"]|',
                             '//d1:cvParam[@accession="MS:1000576"]')
            comp_n <- xml_find_first(spectra_n[[1]], comp_x)
            comp <- xml_attr(comp_n, "name")
            comp <- switch(comp,
                           `zlib` = "gzip",
                           `zlib compression` = "gzip",
                           `no compression` = "none",
                           `none` = "none"
            )

            mz_prs <- '//d1:cvParam[@accession="MS:1000523"]'
            mz_prs <- xml_find_first(spectra_n[[1]], xpath = mz_prs)
            mz_prs <- xml_attr(mz_prs, "name")
            mz_prs <- sub(mz_prs, pattern = "-bit float", replacement = "")
            mz_prs <- as.numeric(mz_prs)/8

            int_prs <- '//d1:cvParam[@accession="MS:1000521"]'
            int_prs <- xml_find_first(spectra_n[[1]], xpath = int_prs)
            int_prs <- xml_attr(int_prs, "name")
            int_prs <- sub(int_prs, pattern = "-bit float", replacement = "")
            int_prs <- as.numeric(int_prs)/8

            if (is.na(int_prs)) int_prs <- mz_prs
            if (is.na(mz_prs)) mz_prs <- int_prs

            mz_x <- 'd1:binaryDataArrayList/d1:binaryDataArray[1]/d1:binary'
            mz <- xml_text(xml_find_all(spectra_n, mz_x))
            mz <- lapply(mz, function(z, comp, mz_prs) {
              if (!nchar(z)) return(numeric(0))
              temp <- base64enc::base64decode(z)
              temp <- as.raw(temp)
              temp <- memDecompress(temp, type = comp)
              temp <- readBin(temp, what = "double",
                              n = length(temp)/mz_prs, size = mz_prs)
            }, comp = comp, mz_prs = mz_prs)

            intensity_x <- 'd1:binaryDataArrayList/d1:binaryDataArray[2]/d1:binary'
            intensity <- xml_text(xml_find_all(spectra_n, intensity_x))
            intensity <- lapply(intensity, function(z, comp, int_prs) {
              if (!nchar(z)) return(numeric(0))
              temp <- base64enc::base64decode(z)
              temp <- as.raw(temp)
              temp <- memDecompress(temp, type = comp)
              temp <- readBin(temp, what = "double",
                              n = length(temp)/int_prs, size = int_prs)
            }, comp = comp, int_prs = int_prs)

            df_b <- data.frame(
              "index" = rep(index, sapply(mz, length)),
              "mz" = as.numeric(unlist(mz)),
              "intensity" = as.numeric(unlist(intensity))
            )

            df_out <- merge(df_a, df_b, by = "index")

            if (with_targets) {
              if ("analysis" %in% colnames(targets)) {
                ana_name <- gsub(".mzML|.mzXML", "", basename(i))
                tp_tar <- targets[targets$analysis %in% ana_name, ]
                if (!is.null(preMZr)) {
                  pre_tar <- preMZr[targets$analysis %in% ana_name, ]
                } else {
                  pre_tar <- NULL
                }
                if (nrow(tp_tar) > 0) {
                  df_out <- trim_targets(df_out, tp_tar, pre_tar)
                } else {
                  df_out <- data.frame()
                }
              } else {
                df_out <- trim_targets(df_out, targets, preMZr)
              }
            }
            df_out

          } else {
            data.frame()
          }

        ### for mzXML ----
        } else if (grepl("mzXML", i)) {

          scan_n <- xml_find_all(xml_data, xpath = "//d1:scan")
          if (length(scan_n) > 0) {

            level <- as.integer(xml_attr(scan_n, "msLevel"))

            if (!is.null(levels)) {
              check_level <- level %in% levels
              level <- level[check_level]
              scan_n <- scan_n[check_level]
            }

            rt <- xml_attr(scan_n, "retentionTime")
            rt <- gsub("[^0-9.-]", "", rt)
            rt <- as.numeric(rt)
            if (max(rt) < 70) rt <- rt * 60

            if (with_targets) {
              if ("analysis" %in% colnames(targets)) {
                ana_name <- gsub(".mzML|.mzXML", "", basename(i))
                tp_tar <- targets[targets$analysis %in% ana_name, ]
                if (nrow(tp_tar) > 0) {
                  check_rt <- trim(rt, tp_tar$rtmin, tp_tar$rtmax)
                } else {
                  check_rt <- rep(FALSE, length(rt))
                }
              } else {
                check_rt <- trim(rt, targets$rtmin, targets$rtmax)
              }
              rt <- rt[check_rt]
              level <- level[check_rt]
              scan_n <- scan_n[check_rt]
            }

            scan <- as.numeric(xml_attr(scan_n, "num"))
            level_unique <- sort(unique(level))

            if (TRUE %in% (level_unique >= 2)) {
              total_scans <- length(scan)
              ce <- rep(NA_real_, total_scans)
              preMZ <- rep(NA_real_, total_scans)

              for (lv in level_unique[!level_unique %in% 1]) {
                msn_x <- paste0('//d1:scan[@msLevel="', lv, '"]')
                msn_n <- xml_find_all(xml_data, xpath = msn_x)
                msn_scan <- as.numeric(xml_attr(msn_n, "num"))
                check_msn_scan <- msn_scan %in% scan
                msn_scan <- msn_scan[check_msn_scan]
                msn_n <- msn_n[check_msn_scan]
                msn_ce <- as.numeric(xml_attr(msn_n, "collisionEnergy"))
                ce[scan %in% msn_scan] <- msn_ce
                msn_pre_n <- xml_find_all(msn_n, xpath = "d1:precursorMz")
                msn_preMz <- as.numeric(xml_text(msn_pre_n))
                preMZ[scan %in% msn_scan] <- msn_preMz
              }

              if (!is.null(preMZr)) {
                if ("analysis" %in% colnames(targets)) {
                  ana_name <- gsub(".mzML|.mzXML", "", basename(i))
                  pre_tar <- preMZr[targets$analysis %in% ana_name, ]
                  if (nrow(pre_tar) > 0) {
                    check_preMZ <- trim(preMZ, pre_tar$mzmin, pre_tar$mzmax)
                  } else {
                    check_preMZ <- rep(FALSE, length(preMZ))
                  }
                } else {
                  check_preMZ <- trim(preMZ, preMZr$mzmin, preMZr$mzmax)
                }
                scan_n <- scan_n[check_preMZ]
                scan <- scan[check_preMZ]
                level <- level[check_preMZ]
                ce <- ce[check_preMZ]
                preMZ <- preMZ[check_preMZ]
                rt <- rt[check_preMZ]
              }

              df_a <- data.table(
                "scan" = scan,
                "level" = level,
                "ce" = ce,
                "preMZ" = preMZ,
                "rt" = rt
              )

            } else {
              df_a <- data.table(
                "scan" = scan,
                "level" = level,
                "rt" = rt
              )
            }

            enc_n <- xml_find_first(xml_data, '//d1:peaks')
            comp <- xml_attr(enc_n, "compressionType")
            comp <- switch(comp,
                           `zlib` = "gzip",
                           `zlib compression` = "gzip",
                           `no compression` = "none",
                           `none` = "none"
            )
            if (is.null(comp)) comp <- "none"
            prs <- xml_attr(enc_n, "precision")
            prs <- as.numeric(prs)/8
            byte_order <- xml_attr(enc_n, "byteOrder")
            endi_enc <- switch(byte_order, `network` = "big")

            peak_n <- xml_text(xml_find_all(scan_n, xpath = "d1:peaks"))
            vals <- lapply(peak_n, function(z, comp, prs, endi_enc = endi_enc){
              if (!nchar(z)) return(matrix(ncol = 2, nrow = 0))
              temp <- base64enc::base64decode(z)
              temp <- as.raw(temp)
              temp <- memDecompress(temp, type = comp)
              temp <- readBin(temp, what = "numeric",
                n = length(temp)/prs, size = prs, endian = endi_enc
              )
              temp <- matrix(temp, ncol = 2, byrow = TRUE)
              temp
            }, comp = comp, prs = prs, endi_enc = endi_enc)

            mz <- unlist(lapply(vals, function(x) x[, 1]))
            intensity <- unlist(lapply(vals, function(x) x[, 2]))

            df_b <- data.frame(
              "scan" = rep(scan, vapply(vals, nrow, 0)),
              "mz" = as.numeric(mz),
              "intensity" = as.numeric(intensity)
            )

            df_out <- merge(df_a, df_b, by = "scan")

            if (with_targets) {
              if ("analysis" %in% colnames(targets)) {
                ana_name <- gsub(".mzML|.mzXML", "", basename(i))
                tp_tar <- targets[targets$analysis %in% ana_name, ]
                if (!is.null(preMZr)) {
                  pre_tar <- preMZr[targets$analysis %in% ana_name, ]
                } else {
                  pre_tar <- NULL
                }
                if (nrow(tp_tar) > 0) {
                  df_out <- trim_targets(df_out, tp_tar, pre_tar)
                } else {
                  df_out <- data.frame()
                }
              } else {
                df_out <- trim_targets(df_out, targets, preMZr)
              }
            }
            df_out

          } else {
            data.frame()
          }
        } else {
          warning("File format must be either mzML or mzXML!")
          data.frame()
        }
      }
    } else {
      warning("Both mzR and xml2 R packages are not installed or available!")
    }
    if (runParallel) parallel::stopCluster(cl)

    if (length(spec_list) == length(files)) {
      spec_list <- lapply(spec_list, function(x, minMS1, minMS2) {
        x <- x[!(x$intensity <= minMS1 & x$level == 1), ]
        x <- x[!(x$intensity <= minMS2 & x$level == 2), ]
        x
      }, minMS1 = minIntensityMS1, minMS2 = minIntensityMS2)
      names(spec_list) = files
    }
  } else {
    warning("File/s not valid!")
  }
  # end -----
  spec_list
}

#' parse_ms_chromatograms
#'
#' @param files X.
#' @param runParallel X.
#'
#' @return X.
#'
parse_ms_chromatograms <- function(files = NA_character_, runParallel = FALSE) {

  i = NULL
  chrom_list = list()

  if (all(!is.na(files))) {

    if (runParallel & length(files) > 1) {
      workers <- parallel::detectCores() - 1
      if (length(files) < workers) workers <- length(files)
      par_type <- "PSOCK"
      if (parallelly::supportsMulticore()) par_type <- "FORK"
      cl <- parallel::makeCluster(workers, type = par_type)
      doParallel::registerDoParallel(cl)
      # on.exit(parallel::stopCluster(cl))
    } else {
      registerDoSEQ()
    }

    # with mzR -----
    if (requireNamespace("mzR")) {
      chrom_list <- foreach(i = files, .packages = "mzR") %dopar% {

        file_link <- mzR::openMSfile(i, backend = "pwiz")
        cH <- suppressWarnings(mzR::chromatogramHeader(file_link))

        if (nrow(cH) > 0) {
          cH$polarity <- as.character(cH$polarity)
          cH[cH$polarity == 1, "polarity"] <- "positive"
          cH[cH$polarity == 0, "polarity"] <- "negative"
          cH[cH$polarity == -1, "polarity"] <- NA_character_

          chroms <- mzR::chromatograms(file_link, cH$chromatogramIndex)

          if (!is.data.frame(chroms)) {
            chroms <- lapply(cH$chromatogramIndex, function(x, chroms) {
              temp <- chroms[[x]]
              temp <- as.data.frame(temp)
              colnames(temp) <- c("rt", "intensity")
              temp$index <- x
              if (max(temp$rt) < 60) temp$rt <- temp$rt * 60
              temp
            }, chroms = chroms)

            chroms <- do.call("rbind", chroms)

            cH_b <- data.frame(
              "index" = cH$chromatogramIndex,
              "id" = cH$chromatogramId,
              "polarity" = cH$polarity,
              "preMZ" = cH$precursorIsolationWindowTargetMZ,
              "mz" = cH$productIsolationWindowTargetMZ
            )

            chrom_data <- merge(cH_b, chroms, by = "index")
          } else {
            colnames(chroms) <- c("rt", "intensity")
            if (max(chroms$rt) < 60) chroms$rt <- chroms$rt * 60
            chrom_data <- data.frame(
              "index" = cH$chromatogramIndex,
              "id" = cH$chromatogramId,
              "polarity" = cH$polarity,
              "preMZ" = cH$precursorIsolationWindowTargetMZ,
              "mz" = cH$productIsolationWindowTargetMZ,
              "rt" = chroms$rt,
              "intensity" = chroms$intensity
            )
          }
          if (exists("file_link")) suppressWarnings(mzR::close(file_link))
          chrom_data
        } else {
          data.frame()
        }
      }

      ## with xml2 -----
    } else if (requireNamespace("xml2")) {
      chrom_list <- foreach(i = files, .packages = "xml2") %dopar% {
        xml_data <- read_xml(i)

        ### for mzML ----
        if (grepl("mzML", i)) {

          chrom_x <- '//d1:chromatogram'
          chrom_n <- xml_find_all(xml_data, chrom_x)
          chromatograms_number <- length(chrom_n)

          if (chromatograms_number > 0) {
            if (length(chrom_n) == 1) chrom_n <- list(chrom_n)

            chrom_data <- lapply(chrom_n, function(x) {
              idx <- as.numeric(xml_attr(x, "index"))
              id <- xml_attr(x, "id")

              polarity_pos <- 'd1:cvParam[@accession="MS:1000130"]'
              polarity_pos <- xml_child(x, polarity_pos)

              polarity_neg <- 'd1:cvParam[@accession="MS:1000129"]'
              polarity_neg <- xml_child(x, polarity_neg)

              if (length(polarity_pos) > 0 | length(polarity_neg) > 0) {

                polarity <- c(
                  unique(gsub(" scan", "", xml_attr(polarity_pos, "name"))),
                  unique(gsub(" scan", "", xml_attr(polarity_neg, "name")))
                )

                polarity <- polarity[!is.na(polarity)]

              } else {
                polarity <- NA_character_
              }

              preMZ_xpath <- 'd1:precursor//d1:cvParam[@name="isolation window target m/z"]'
              preMZ <- as.numeric(xml_attr(xml_child(x, preMZ_xpath), "value"))

              product_mz_xpath <- 'd1:product//d1:cvParam[@name="isolation window target m/z"]'
              product_mz <- as.numeric(xml_attr(xml_child(x, product_mz_xpath), "value"))

              arrays <- 'd1:binaryDataArrayList/d1:binaryDataArray'
              arrays <- xml_find_all(x, xpath = arrays)
              arrays <- lapply(arrays, function(z) {
                precision_23 <- 'd1:cvParam[@accession="MS:1000523"]'
                precision_22 <- 'd1:cvParam[@accession="MS:1000522"]'
                precision <- xml_find_all(z, xpath = precision_23)
                if (length(precision) == 0) {
                  precision <- xml_find_all(z, xpath = precision_22)
                }
                precision <- xml_attr(precision, "name")
                precision <- sub(precision, pattern = "-bit float", replacement = "")
                precision <- sub(precision, pattern = "-bit integer", replacement = "")
                precision <- as.numeric(precision)/8
                comp <- 'd1:cvParam[@accession="MS:1000574"]'
                comp <- xml_find_all(z, xpath = comp)
                comp <- xml_attr(comp, "name")
                comp <- switch(comp,
                               `zlib` = "gzip",
                               `zlib compression` = "gzip",
                               `no compression` = "none",
                               `none` = "none"
                )
                val <- 'd1:binary'
                val <- xml_find_all(z, xpath = val)
                val <- xml_text(val)
                val <- base64enc::base64decode(val)
                val <- as.raw(val)
                val <- memDecompress(val, type = comp)
                val <- readBin(
                  val, what = "double",
                  n = length(val)/precision, size = precision
                )
                time_unit <- 'd1:cvParam[@name="time array"]'
                time_unit <- xml_find_all(z, xpath = time_unit)
                if (length(time_unit) > 0) {
                  unit <- xml_attr(time_unit, "unitName")
                  if ("minute" %in% unit) val <- val * 60
                }
                val
              })

              rt <- arrays[[1]]
              intensity <- arrays[[2]]
              out_df <- data.frame(
                "index" = idx,
                "id" = id,
                "polarity" = polarity,
                "preMZ" = preMZ,
                "mz" = product_mz,
                "rt" = rt,
                "intensity" = intensity
              )
              out_df
            })

            if (length(chrom_data) > 0) {
              chrom_data <- do.call("rbind", chrom_data)
            } else {
              data.frame()
            }
          } else {
            data.frame()
          }

          ### for mzXML ----
          # not possible to store chromatograms in mzXML files
        } else {
          data.frame()
        }
      }
    } else {
      warning("Both mzR and xml2 R packages are not installed or available!")
    }

    if (runParallel) parallel::stopCluster(cl)

    if (length(chrom_list) == length(files)) {
      names(chrom_list) = files
    }
  }
  # end -----
  chrom_list
}

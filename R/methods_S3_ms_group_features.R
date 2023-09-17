
#' @title .s3_ms_group_features.patRoon
#'
#' @description Groups features using the package patRoon.
#'
#' @noRd
#'
.s3_ms_group_features.patRoon <- function(settings, self) {

  if (FALSE & requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }

  pat_features <- self$as_features_patRoon()

  if (length(pat_features) == 0) {
    warning("Features were not found! Run find_features method first!")
    return(FALSE)
  }

  algorithm <- settings$algorithm

  if (grepl("_", algorithm, fixed = FALSE)) {
    algorithm <- gsub("^(.*?)_.*$", "\\1", algorithm)
  }

  if ("xcms" %in% algorithm || "xcms3" %in% algorithm) {
    if (!requireNamespace("xcms")) {
      warning("xcms package is not installed!")
      return(FALSE)
    }
  }

  parameters <- settings$parameters

  if ("class" %in% names(parameters)) {
    parameters[["Class"]] <- parameters$class
    parameters[["class"]] <- NULL

    parameters <- lapply(parameters, function(z) {
      if (is.list(z) & length(z) > 0) {
        z[[1]]
      } else {
        z
      }
    })

    if (parameters$Class %in% "PeakGroupsParam") {
      parameters$peakGroupsMatrix <- as.matrix(parameters$peakGroupsMatrix)
    }

    if (parameters$Class %in% "PeakGroupsParam") {
      parameters$subset <- as.integer(parameters$subset)
    }

    parameters <- do.call("new", parameters)

  } else if (is.list(parameters)) {

    parameters <- lapply(parameters, function(par) {
      if (is.list(par)) {
        if ("class" %in% names(par)) {
          par[["Class"]] <- par$class
          par[["class"]] <- NULL

          par <- lapply(par, function(z) {
            if (is.list(z) & length(z) > 0) {
              z[[1]]
            } else {
              z
            }
          })

          if (par$Class %in% "PeakGroupsParam") {
            par$peakGroupsMatrix <- as.matrix(par$peakGroupsMatrix)
          }

          if (par$Class %in% "PeakGroupsParam") {
            par$subset <- as.integer(par$subset)
          }

          par <- do.call("new", par)
        }
      }
      par
    })
  }

  if (algorithm == "xcms3") {
    if ("Param" %in% is(parameters)) {
      parameters <- list("groupParam" = parameters)
    }

    parameters$groupParam@sampleGroups <- self$get_replicate_names()

    if ("rtalign" %in% names(parameters)) {
      if (parameters$rtalign) {
        parameters$preGroupParam@sampleGroups <- self$get_replicate_names()
      }
    }

    # when multiple polarities it makes setFeatureGroups, no rt alignment possible
    if (length(unique(self$get_polarities())) > 1) {
      parameters <- parameters["groupParam"]
    }
  }

  ag <- list(obj = pat_features, algorithm = algorithm)

  gr_fun <- patRoon::groupFeatures

  pat <- do.call(gr_fun, c(ag, parameters))
  
  pat_ft <- pat@features@features
  
  pat_ft <- rbindlist(pat_ft, idcol = "analysis")
  
  setnames(pat_ft,
    c("ID", "ret", "retmin", "retmax"),
    c("feature", "rt", "rtmin", "rtmax"),
    skip_absent = TRUE
  )
  
  mz_as_mass <- grepl("Set", class(pat))
  
  groups <- rcpp_ms_groups_make_dataframe(pat_ft, self$get_analysis_names(), mz_as_mass, TRUE)
  
  if (self$has_groups()) self$remove_groups()
  
  new_groups_id <- groups$group
  names(new_groups_id) <- groups$old_group
  pat_ft$group <- new_groups_id[pat_ft$group] 
  
  self$add_group_to_features(pat_ft)

  groups$old_group <- NULL
  
  self$add_groups(groups)
  
  alignment <- .extract_time_alignment(pat, self)

  if (!is.null(alignment)) {
    self$add_alignment(alignment)
    message("\U2713 Added alignment of retention time for each analysis!")
  }

  TRUE
}

#' .extract_time_alignment
#'
#' @description Function to extract adjusted retention time information from
#' alignment results when using `xcms3` as algorithm for grouping and retention
#' time alignment.
#'
#' @param pat An object with class `features` or `featureGroups` from the
#' package \pkg{patRoon}.
#'
#' @param self A `MassSpecData` object. When applied within the R6, the self
#' object.
#'
#' @noRd
#'
.extract_time_alignment <- function(pat, self) {
  if ("featureGroupsXCMS3" %in% is(pat)) {

    if (xcms::hasAdjustedRtime(pat@xdata)) {
      rtAdj <- xcms::adjustedRtime(pat@xdata)
      pkAdj <- xcms::processHistory(pat@xdata,
                                    type = "Retention time correction"
      )[[1]]
      pkAdj <- pkAdj@param

      addAdjPoints <- FALSE
      if ("PeakGroupsParam" %in% is(pkAdj)) {
        addAdjPoints <- TRUE
        pkAdj <- xcms::peakGroupsMatrix(pkAdj)
      }

      # hasSpectra = all(self$has_loaded_spectra())
      hasSpectra <- FALSE

      if (!hasSpectra) {
        rtOrg <- lapply(self$get_files(), function(x) {
          file_link <- mzR::openMSfile(x, backend = "pwiz")
          sH <- suppressWarnings(mzR::header(file_link))
          suppressWarnings(mzR::close(file_link))
          sH$retentionTime
        })
      }

      alignment <- lapply(self$get_analysis_names(),
        function(ana, rtOrg, rtAdj, addAdjPoints, pkAdj, all_ana) {
          ana_idx <- which(all_ana %in% ana)
          n_ana <- length(all_ana)

          rts <- names(rtAdj)
          ana_idx_string <- paste0(
            "F",
            paste(rep("0", nchar(n_ana) - nchar(ana_idx)), collapse = ""),
            ana_idx
          )
          rts <- grepl(ana_idx_string, rts)
          rts <- rtAdj[rts]

          temp <- data.frame(
            "rt_original" = rtOrg[[ana]],
            "rt_adjusted" = rts
          )

          temp$adjustment <- temp$rt_original - temp$rt_adjusted

          if (addAdjPoints) {
            adjPoints <- unique(pkAdj[, ana_idx])
            adjPoints <- adjPoints[adjPoints %in% temp$rt_original]
            temp$adjPoints[temp$rt_original %in% adjPoints] <- adjPoints
          }
          row.names(temp) <- seq_len(nrow(temp))
          temp
        },
        rtOrg = rtOrg,
        rtAdj = rtAdj,
        addAdjPoints = addAdjPoints,
        pkAdj = pkAdj,
        all_ana = self$get_analysis_names()
      )

      return(alignment)
    }
  }
  NULL
}

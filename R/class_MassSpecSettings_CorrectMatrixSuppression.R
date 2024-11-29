#' @noRd
.calculate_tic_matrix_suppression <- function(x, rtWindow = 10) {
  info <- x$info
  if (nrow(info) == 0) {
    warning("No analyses available!")
    return(NULL)
  }

  tics <- get_spectra_tic(x, levels = 1)
  if (nrow(tics) == 0) {
    warning("No TICs available!")
    return(NULL)
  }

  tics <- split(tics, tics$analysis)

  rpls <- info$replicate
  names(rpls) <- info$analysis
  blankRpls <- info$blank
  names(blankRpls) <- info$analysis
  blankAnalyses <- info$analysis
  names(blankAnalyses) <- info$blank
  blankAnalyses <- blankAnalyses[info$blank %in% info$replicate]

  if (length(blankAnalyses) == 0) {
    warning("No blank analyses defined!")
    return(NULL)
  }

  mpList <- lapply(info$analysis, function(a, rpls, blankRpls, blankAnalyses, tics, rtWindow) {
    ticA <- tics[[a]]
    blkG <- unique(blankRpls[a])
    blkA <- blankAnalyses[rpls %in% blkG]

    if (length(blkA) == 0) {
      warning("No blank analyses defined for ", a, "!")
      ticA$mp <- -1
      return(ticA)
    }

    mpA <- lapply(tics[blkA], function(z, ticA, rtWindow) {
      mp <- vapply(seq_len(nrow(ticA)), function(j, z, rtWindow) {
        rt <- ticA$rt[j]
        rtr <- c(rt - rtWindow, rt + rtWindow)
        intB <- mean(z$intensity[.trim_vector(z$rt, rtr[1], rtr[2])])
        intX <- mean(ticA$intensity[.trim_vector(ticA$rt, rtr[1], rtr[2])])
        # eq. 6 from 10.1021/acs.analchem.1c00357
        (intX / intB) * -1
      }, z = z, rtWindow = rtWindow, 0)
      mp
    }, ticA = ticA, rtWindow = rtWindow)

    ticA$mp <- Reduce(`+`, mpA) / length(mpA)

    ticA
  },
  rpls = rpls,
  blankRpls = blankRpls,
  blankAnalyses = blankAnalyses,
  tics = tics,
  rtWindow = rtWindow
  )

  names(mpList) <- info$analysis
  mpList
}

# ______________________________________________________________________________________________________________________
# TiChri -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_CorrectMatrixSuppression_TiChri**
#'
#' @description Settings for correcting matrix suppression based on the TiChri algorithm from 
#' \href{https://pubs.acs.org/doi/10.1021/acs.analchem.1c00357}{Tisler et al. (2021)}. The algorithm calculates the 
#' matrix profile for the total ion chromatogram (TIC) and corrects the matrix suppression for features. Internal 
#' standards can be assigned to improve the correction. The `suppression_factor` is added to the feature list and can 
#' be used to correct the features intensity. The argument/parameter `correctSuppression` is available in plotting 
#' and processing methods and when `TRUE`, the suppression factor is used to correct the feature intensity for better 
#' comparison across analyses with different matrix suppression.
#'
#' @param mpRtWindow Numeric of length one with the retention time window (in seconds) for calculating the matrix profile.
#' @param istdAssignment Character of length one with the assignment method for internal standards. Possible values are
#' `"nearest"`, `"range"`, and `"none"`. Default is `"nearest"`. Setting `"nearest"` assigns the nearest `istdN` internal
#' standard/s, `"range"` assigns internal standard/s within the `istdRtWindow` window, and `"none"` does not assign internal
#' standards. If internal standards are assigned, the `tichri` value is calculated for each internal standard and used 
#' to correct the matrix suppression for the features. If no internal standards are assigned, the correction is based 
#' only on the TIC matrix profile, which is less accurate.
#' @param istdRtWindow Numeric of length one with the retention time window (in seconds) for assigning internal standards.
#' Default is `5`.
#' @param istdN Integer of length one with the number of internal standards to assign. Default is `2`.
#'
#' @return A `MassSpecSettings_CorrectMatrixSuppression_TiChri` object.
#'
#' @references
#' \insertRef{tisler01}{StreamFind}
#'
#' @export
#'
MassSpecSettings_CorrectMatrixSuppression_TiChri <- S7::new_class("MassSpecSettings_CorrectMatrixSuppression_TiChri",
  parent = ProcessingSettings,
  package = "StreamFind",
  constructor = function(mpRtWindow = 10,
                         istdAssignment = "none",
                         istdRtWindow = 5,
                         istdN = 2) {
    
    required <- c("FindFeatures", "GroupFeatures")
    
    if (!istdAssignment %in% "none") {
      required <- c(required, "FindInternalStandards")
    }
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "CorrectMatrixSuppression",
      required = required,
      algorithm = "TiChri",
      parameters = list(
        "mpRtWindow" = as.numeric(mpRtWindow),
        "istdAssignment" = as.character(istdAssignment),
        "istdRtWindow" = as.numeric(istdRtWindow),
        "istdN" = as.integer(istdN)
      ),
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "TiChri",
      developer = "Selina Tisler",
      contact = "seti@plen.ku.dk",
      link = "https://pubs.acs.org/doi/10.1021/acs.analchem.1c00357",
      doi = "10.1021/acs.analchem.1c00357"
    ))
  },
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "CorrectMatrixSuppression")
    checkmate::assert_choice(self@algorithm, "TiChri")
    checkmate::assert_numeric(self@parameters$mpRtWindow, lower = 0)
    checkmate::assert_choice(self@parameters$istdAssignment, c("nearest", "range", "none"))
    checkmate::assert_numeric(self@parameters$istdRtWindow, lower = 0)
    checkmate::assert_integer(self@parameters$istdN, lower = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_CorrectMatrixSuppression_TiChri) <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (!engine$has_nts()) {
    warning("No NTS object available! Not done.")
    return(FALSE)
  }

  nts <- engine$nts

  if (!nts@has_features) {
    warning("NTS object does not have features! Not done.")
    return(FALSE)
  }

  feature_list <- nts$feature_list

  feature_list <- lapply(feature_list, function(z) {
    if (!"quality" %in% colnames(z)) z$quality <- rep(list(), nrow(z))
    if (!"eic" %in% colnames(z)) z$eic <- rep(list(), nrow(z))
    z
  })

  cache <- .load_chache("correct_matrix_suppression", feature_list, x)

  if (!is.null(cache$data)) {
    feature_list <- cache$data
    tryCatch(
      {
        engine$nts$feature_list <- feature_list
        message("\U2139 Corrected matrix suppression loaded from cache!")
        return(TRUE)
      },
      error = function(e) {
        warning(e)
        return(FALSE)
      }
    )
  }

  parameters <- x$parameters

  message("\U2699 Calculating TIC matrix suppression")
  ticMp <- .calculate_tic_matrix_suppression(engine$analyses, rtWindow = parameters$mpRtWindow)

  if (is.null(ticMp)) {
    return(FALSE)
  }

  info <- engine$analyses$info
  rpls <- info$replicate
  names(rpls) <- info$analysis
  blankRpls <- info$blank
  names(blankRpls) <- info$analysis
  blankAnalyses <- info$analysis
  names(blankAnalyses) <- info$blank
  blankAnalyses <- blankAnalyses[info$blank %in% info$replicate]

  if (!"none" %in% parameters$istdAssignment) {
    message("\U2699 Calculating internal standards matrix suppression")
    istd <- engine$get_internal_standards(average = FALSE)

    if (nrow(istd) == 0) {
      warning("No internal standards available!")
      return(FALSE)
    }

    if (!"replicate" %in% colnames(istd)) istd$replicate <- rpls[istd$analysis]

    istd$matrixEffect <- NA_real_
    istd$mp <- NA_real_
    istd$tichri <- NA_real_

    for (i in unique(istd$name)) {
      i_sel <- istd$name == i
      i_istd <- istd[i_sel, ]
      i_rpls <- unique(i_istd$replicate)

      for (j in i_rpls) {
        j_sel <- i_istd$replicate == j
        istd_rpl_int <- i_istd$intensity[j_sel]

        istd_blk <- unique(info$blank[info$replicate %in% j])
        istd_sel_blk <- i_istd$replicate %in% istd_blk
        istd_blk_int <- i_istd$intensity[istd_sel_blk]

        if (length(istd_blk_int) == 0) next

        istd_matrixEffect <- (mean(istd_rpl_int) / mean(istd_blk_int)) - 1
        istd_anas <- unique(info$analysis[info$replicate %in% j])
        istd_mp <- 0
        for (a in istd_anas) {
          a_mp <- ticMp[[a]]
          a_sel <- (a_mp$rt >= (min(i_istd$rt[j_sel]) - parameters$mpRtWindow)) & (a_mp$rt <= (max(i_istd$rt[j_sel]) + parameters$mpRtWindow))
          a_mp <- a_mp[a_sel, ]
          istd_mp <- istd_mp + mean(a_mp$mp)
        }
        istd_mp <- istd_mp / length(istd_anas)

        # eq. 4 from 10.1021/acs.analchem.1c00357
        istd_tichri <- ((mean(istd_blk_int) / mean(istd_rpl_int)) - 1) * (-1)

        istd$matrixEffect[istd$replicate %in% j & istd$name %in% i] <- istd_matrixEffect
        istd$mp[istd$replicate %in% j & istd$name %in% i] <- istd_mp
        istd$tichri[istd$replicate %in% j & istd$name %in% i] <- istd_tichri
      }
    }

    istd <- istd[!is.na(istd$matrixEffect), ]
  }

  message("\U2699 Correcting matrix suppression for features in ", length(feature_list), " analyses")
  feature_list <- lapply(names(feature_list), function(z, feature_list, ticMp, rpls, istd, parameters) {
    fts <- feature_list[[z]]
    if (nrow(fts) == 0) {
      return(fts)
    }

    mp <- ticMp[[z]]
    if (is.null(mp)) {
      return(fts)
    }

    rpl <- rpls[z]

    message("\U2699 Correcting matrix suppression for ", nrow(fts), " features in ", z)
    suppresion_factor <- vapply(seq_len(nrow(fts)), function(i, z, rpl, fts, mp, istd, parameters) {
      ft <- fts[i, ]
      if (is.null(parameters$mpRtWindow)) parameters$mpRtWindow <- (ft[["rtmax"]] - ft[["rtmin"]]) / 2
      mp_ft <- mean(mp$mp[.trim_vector(mp$rt, ft[["rtmin"]] - parameters$mpRtWindow, ft[["rtmax"]] + parameters$mpRtWindow)])

      if ("none" %in% parameters$istdAssignment) {
        # first part of eq. 7 from 10.1021/acs.analchem.1c00357
        return(-mp_ft + 1)
      } else if ("nearest" %in% parameters$istdAssignment) {
        valid_istd <- istd[istd$replicate %in% rpl, ]

        # first part of eq. 7 from 10.1021/acs.analchem.1c00357
        if (nrow(valid_istd) == 0) {
          return(-mp_ft + 1)
        }

        rt_distances <- abs(valid_istd$rt - ft$rt)
        names(rt_distances) <- valid_istd$name
        sorted_rt_distances <- sort(rt_distances)

        if (length(sorted_rt_distances) < parameters$istdN) {
          sel_is <- names(sorted_rt_distances)
        } else {
          sel_is <- names(sorted_rt_distances)[seq_len(parameters$istdN)]
        }

        valid_istd <- valid_istd[valid_istd$name %in% sel_is, ]

        # eq. 7 from 10.1021/acs.analchem.1c00357
        ft_sup_factor <- mp_ft * median(valid_istd$tichri) / median(valid_istd$mp)
        ft_sup_factor <- -ft_sup_factor + 1
        return(ft_sup_factor)
      } else {
        rt_range <- c(ft$rt - parameters$istdRtWindow, ft$rt + parameters$istdRtWindow)
        sel_is <- which(istd$rt >= ret_range[1] & istd$rt <= rt_range[2])
        valid_istd <- istd[sel_is, ]

        # first part of eq. 7 from 10.1021/acs.analchem.1c00357
        if (nrow(valid_istd) == 0) {
          return(ft$intensity * (-mp_ft + 1))
        }

        # eq. 7 from 10.1021/acs.analchem.1c00357
        ft_sup_factor <- mp_ft * median(valid_istd$tichri) / median(valid_istd$mp)
        ft_sup_factor <- -ft_sup_factor + 1
        return(ft_sup_factor)
      }
    }, z = z, rpl = rpl, fts = fts, mp = mp, istd = istd, parameters = parameters, 0)

    fts$suppression_factor <- suppresion_factor
    fts
  }, feature_list = feature_list, ticMp = ticMp, rpls = rpls, istd = istd, parameters = parameters)

  names(feature_list) <- info$analysis

  if (!is.null(cache$hash)) {
    .save_cache("correct_matrix_suppression", feature_list, cache$hash)
    message("\U1f5ab Corrected matrix suppression cached!")
  }

  tryCatch(
    {
      engine$nts$feature_list <- feature_list
      return(TRUE)
    },
    error = function(e) {
      warning(e)
      return(FALSE)
    }
  )
}

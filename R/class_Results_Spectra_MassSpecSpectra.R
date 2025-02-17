#' @export
#' @noRd
MassSpecSpectra <- S7::new_class(
  name = "MassSpecSpectra",
  package = "StreamFind",
  parent = Spectra,
  properties = list(
    replicates = S7::new_property(S7::class_character, default = character()),
    is_neutralized = S7::new_property(S7::class_logical, default = FALSE),
    charges = S7::new_property(S7::class_list, default = list())
  ),
  constructor = function(spectra = list(),
                         replicates = character(),
                         is_averaged = FALSE,
                         is_neutralized = FALSE,
                         peaks = list(),
                         charges = list()) {
    S7::new_object(
      Spectra(), 
      name = "MassSpecSpectra",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      spectra = spectra,
      is_averaged = is_averaged,
      peaks = peaks,
      replicates = replicates,
      is_neutralized = is_neutralized,
      charges = charges
    )
  },
  
  validator = function(self) {
    checkmate::assert_logical(self@is_neutralized, max.len = 1)
    if (length(self@charges) > 0) {
      for (charge in self@charges) {
        checkmate::assert_data_frame(charge)
      }
    }
    NULL
  }
)

# MARK: Methods
# Methods ------

# MARK: length
## length -----
#' @export
#' @noRd
S7::method(length, MassSpecSpectra) <- function(x) {
  length(x@spectra)
}

# MARK: names
## names -----
#' @export
#' @noRd
S7::method(names, MassSpecSpectra) <- function(x) {
  names(x@spectra)
}

# MARK: show
## show -----
#' @export
#' @noRd
S7::method(show, MassSpecSpectra) <- function(x) {
  if (length(x@spectra) > 0) {
    cat("Number spectra: ", length(x@spectra), "\n")
    cat("Averaged: ", x@is_averaged, "\n")
    cat("Neutralized: ", x@is_neutralized, "\n")
    if (x@has_peaks) {
      cat("Number peaks: ", vapply(x@peaks, nrow, 0), "\n")
    } else {
      cat("Number peaks: ", 0, "\n")
    }
    if (length(x@charges) > 0) {
      cat("Number charges: ", vapply(x@charges, nrow, 0), "\n")
    }
  } else {
    cat("Number spectra: ", 0, "\n")
  }
}

# MARK: `[`
## `[` -----
#' @export
#' @noRd
S7::method(`[`, MassSpecSpectra) <- function(x, i) {
  x@spectra <- x@spectra[i]
  if (x@has_peaks) {
    x@peaks <- x@peaks[i]
  }
  if (length(x@charges) > 0) {
    x@charges <- x@charges[i]
  }
  
  if (x@is_averaged) {
    x@replicates <- x@replicates[i]
  } else {
    x@replicates <- x@replicates[names(x@spectra)]
  }
  x
}

# MARK: get_spectra
## get_spectra -----
#' @export
#' @noRd
S7::method(get_spectra, MassSpecSpectra) <- function(x,
                                                     analyses = NULL,
                                                     levels = NULL,
                                                     mass = NULL,
                                                     mz = NULL,
                                                     rt = NULL,
                                                     mobility = NULL,
                                                     ppm = 20,
                                                     sec = 60,
                                                     millisec = 5,
                                                     id = NULL,
                                                     allTraces = TRUE,
                                                     isolationWindow = 1.3,
                                                     minIntensityMS1 = 0,
                                                     minIntensityMS2 = 0) {
  
  if (length(x@spectra) == 0) {
    warning("No spectra results available!")
    return(list())
  }
  
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) {
    return(list())
  }
  
  if (is.data.frame(mz)) {
    if ("analysis" %in% colnames(mz)) {
      analyses <- mz$analysis
    }
  }
  
  if (is.data.frame(mass)) {
    if ("analysis" %in% colnames(mass)) {
      analyses <- mass$analysis
    }
  }
  
  if (x$is_averaged) {
    rpl <- x$replicates
    rpl <- rpl[analyses]
    x$spectra <- x$spectra[names(x$spectra) %in% unname(rpl)]
    x$spectra <- Map( function(z, y) {
      if (nrow(z) > 0) {
        z$replicate <- y
        data.table::setcolorder(z, c("replicate"))
      }
      z
    }, x$spectra, names(x$spectra))
  } else {
    rpl <- x$replicates[analyses]
    x$spectra <- x$spectra[analyses]
    x$spectra <- Map( function(z, y) {
      if (nrow(z) > 0) {
        z$analysis <- y
        z$replicate <- rpl[y]
        data.table::setcolorder(z, c("analysis", "replicate"))
      }
      z
    }, x$spectra, names(x$spectra))
  }
  
  if (!any(is.numeric(minIntensityMS1) | is.integer(minIntensityMS1))) minIntensityMS1 <- 0
  
  if (!any(is.numeric(minIntensityMS2) | is.integer(minIntensityMS2))) minIntensityMS2 <- 0
  
  polarities <- x$spectra_polarity[analyses]
  
  targets <- MassSpecTargets(mass, mz, rt, mobility, ppm, sec, millisec, id, analyses, polarities)
  
  targets <- targets@targets
  
  if ("polarity" %in% colnames(targets) && nrow(targets) > 0) {
    for (i in seq_len(nrow(targets))) {
      if (targets$polarity[i] == "positive") targets$polarity[i] <- 1
      if (targets$polarity[i] == "negative") targets$polarity[i] <- -1
    }
  }
  
  num_cols <- c(
    "mz", "rt", "mobility", "mzmin", "mzmax", "rtmin", "rtmax", "mobilitymin", "mobilitymax"
  )
  
  if (all(apply(targets[, num_cols, with = FALSE], 1, function(z) sum(z, na.rm = TRUE)) != 0)) {
    if (TRUE %in% is.na(targets$mz)) {
      targets$mz[is.na(targets$mz)] <- 0
    }
    if (TRUE %in% is.na(targets$mzmax)) {
      targets$mzmax[is.na(targets$mzmax)] <- max(x$spectra_highest_mz[analyses])
    }
    if (TRUE %in% is.na(targets$mzmin)) {
      targets$mzmin[is.na(targets$mzmin)] <- min(x$spectra_lowest_mz[analyses])
    }
    if (TRUE %in% (targets$mzmax == 0)) {
      targets$mzmax[targets$mzmax == 0] <- max(x$spectra_highest_mz[analyses])
    }
    if (TRUE %in% is.na(targets$rt)) {
      targets$rt[is.na(targets$rt)] <- 0
    }
    if (TRUE %in% is.na(targets$rtmax)) {
      targets$rtmax[is.na(targets$rtmax)] <- max(x$spectra_highest_rt[analyses])
    }
    if (TRUE %in% is.na(targets$rtmin)) {
      targets$rtmin[is.na(targets$rtmin)] <- min(x$spectra_lowest_rt[analyses])
    }
    if (TRUE %in% (targets$rtmax == 0)) {
      targets$rtmax[targets$rtmax == 0] <- max(x$spectra_highest_rt[analyses])
    }
    if (TRUE %in% is.na(targets$mobility)) {
      targets$mobility[is.na(targets$mobility)] <- 0
    }
    if (TRUE %in% is.na(targets$mobilitymax) && any(x$has_ion_mobility[analyses])) {
      targets$mobilitymax[is.na(targets$mobilitymax)] <- max(
        x$spectra_highest_mobility[analyses], na.rm = TRUE
      )
    }
    if (TRUE %in% is.na(targets$mobilitymin) && any(x$has_ion_mobility[analyses])) {
      targets$mobilitymin[is.na(targets$mobilitymin)] <- min(
        x$spectra_lowest_mobility[analyses], na.rm = TRUE
      )
    }
    if (TRUE %in% (targets$mobilitymax == 0) && any(x$has_ion_mobility[analyses])) {
      targets$mobilitymax[targets$mobilitymax == 0] <- max(
        x$spectra_highest_mobility[analyses], na.rm = TRUE
      )
    }
  } else {
    targets$id <- targets$analysis
  }
  
  if (is.null(levels)) {
    levels <- unique(x$spectra_level[analyses])
    levels <- as.numeric(unlist(strsplit(levels, ", ")))
  }
  
  if (!2 %in% levels) allTraces <- TRUE
  
  if (!is.logical(allTraces)) allTraces <- TRUE
  
  if (nrow(targets) > 0) {
    if ("polarity" %in% colnames(targets)) targets$polarity <- as.numeric(targets$polarity)
    targets$precursor <- FALSE
    if (!allTraces) {
      if (!any(is.numeric(isolationWindow) | is.integer(isolationWindow))) isolationWindow <- 0
      targets$precursor <- TRUE
      targets$mzmin <- targets$mzmin - (isolationWindow / 2)
      targets$mzmax <- targets$mzmax + (isolationWindow / 2)
      # TODO make case for DIA when pre_mz is not available
    }
  }
  
  spec_list <- lapply(x$spectra, function(temp, rpl, analyses) {
    with_im <- any(temp$mobility > 0)
    if (!is.null(levels)) temp <- temp[temp$level %in% levels, ]
    
    if (nrow(targets) > 0) {
      if ("analysis" %in% colnames(targets)) {
        if ("analysis" %in% colnames(temp)) {
          targets <- targets[targets$analysis %in% temp$analysis, ]
        } else {
          targets <- targets[rpl[targets$analysis] %in% temp$replicate, ]
        }
      }
      
      if (nrow(targets) > 0) {
        if ("polarity" %in% colnames(targets)) temp <- temp[temp$polarity %in% targets$polarity, ]
        temp <- .trim_spectra_targets(temp, targets, with_im)
      } else {
        return(data.table::data.table())
      }
    }
    
    if (with_im) temp$mobility <- NULL
    temp <- temp[!(temp$intensity <= minIntensityMS1 & temp$level == 1), ]
    temp <- temp[!(temp$intensity <= minIntensityMS2 & temp$level == 2), ]
    temp
  }, rpl = rpl, analyses = analyses)
  
  names(spec_list) <- names(x$spectra)
}

# MARK: get_spectra_matrix
## get_spectra_matrix -----
#' @export
#' @noRd
S7::method(get_spectra_matrix, MassSpecSpectra) <- function(x, analyses = NULL) {
  if (length(x@spectra) == 0) {
    warning("No spectra results object available!")
    return(matrix())
  }
  analyses <- .check_analyses_argument(x, analyses)
  spec_list <- x@spectra
  if (x@is_averaged) {
    rpl <- x@replicates
    rpl <- unique(rpl[analyses])
    spec_list <- spec_list[rpl]
  } else {
    spec_list <- spec_list[analyses]
  }
  intensity <- NULL
  spec_list <- spec_list[vapply(spec_list, function(z) nrow(z) > 0, FALSE)]
  
  spec_list <- lapply(spec_list, function(z) {
    if (!"bins" %in% colnames(z)) {
      if ("mass" %in% colnames(z)) z$mz <- z$mass
      if (!"level" %in% colnames(z)) z$level <- 1
      if ("mobility" %in% colnames(z)) {
        z$bins <- paste0("r", z$rt, "_m", z$mz, "_d", z$mobility, "_p", z$polarity, "_l", z$level)
      } else {
        z$bins <- paste0("r", z$rt, "_m", z$mz, "_p", z$polarity, "_l", z$level)
      }
    }
    z <- z[, .(intensity = mean(intensity)), by = c("bins")]
    z <- data.table::dcast(z, formula = 1 ~ bins, value.var = "intensity")[, -1]
    z
  })
  
  spec <- data.table::rbindlist(spec_list, fill = TRUE)
  spec[is.na(spec)] <- 0
  spec <- as.matrix(spec)
  rownames(spec) <- names(spec_list)
  spec
}

# MARK: get_spectra_peaks
## get_spectra_peaks -----
#' @export
#' @noRd
S7::method(get_spectra_peaks, MassSpecSpectra) <- function(x, analyses = NULL) {
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) {
    return(data.table::data.table())
  }
  
  if (length(x@spectra) == 0) {
    return(data.table::data.table())
  }
  
  pks <- x$peaks
  if (length(pks) == 0) {
    return(data.table::data.table())
  }
  
  if (x$is_averaged) {
    pks <- data.table::rbindlist(x$peaks, idcol = "replicate", fill = TRUE)
  } else {
    pks <- data.table::rbindlist(x$peaks, idcol = "analysis", fill = TRUE)
  }
  
  if ("analysis" %in% colnames(pks)) {
    pks <- pks[pks$analysis %in% analyses, ]
  } else if ("replicate" %in% colnames(pks)) {
    rpl <- x$replicates
    rpl <- rpl[analyses]
    pks <- pks[pks$replicate %in% unname(rpl)]
    
    if (!"analysis" %in% colnames(pks)) {
      pks$analysis <- pks$replicate
      data.table::setcolorder(pks, c("analysis", "replicate"))
    }
  }
  
  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found for the targets!")
    return(data.table::data.table())
  }
  
  pks
}

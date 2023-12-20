#' .trim_vector
#'
#' @param v A vector to trim based on minimum and maximum value pairs.
#' @param a A vector with minimum values to evaluate `v`.
#' @param b A vector with maximum values to evaluate `v`.
#'
#' @return A logical vector with the same length as `v` with \code{TRUE} for
#' regions between `a` and `b` value pairs.
#'
#' @noRd
#'
.trim_vector <- function(v, a, b) {
  rowSums(as.matrix(mapply(function(a, b) v >= a & v <= b, a = a, b = b))) > 0
}

#' .trim_spectra_targets
#'
#' @param traces A data.frame with spectra.
#' @param targets A data.frame with targets (minimum and maximum).
#' @param preMZr A data.frame with precursor minimum and maximum values as
#' `targets` but expanded with the isolation window.
#'
#' @return The filtered `traces` data.frame.
#'
#' @noRd
#'
.trim_spectra_targets <- function(traces, targets, preMZr, with_im) {
  
  tg_list <- lapply(seq_len(nrow(targets)),
    function(z, traces, targets, preMZr) {
      
      tg <- traces
      
      cutRt <- .trim_vector(tg$rt, targets$rtmin[z], targets$rtmax[z])
      
      tg <- tg[cutRt, ]
      
      if (nrow(tg) == 0) return(NULL)
      
      if (with_im) {
        cutIM <- .trim_vector(tg$drift, targets$driftmin[z], targets$driftmax[z])
        tg <- tg[cutIM, ]
      }
      
      if (nrow(tg) == 0) return(NULL)

      if ("polarity" %in% colnames(targets)) {
        tg <- tg[tg$polarity == targets$polarity[z], ]
      }
      
      if (nrow(tg) == 0) return(NULL)
      
      if (nrow(tg) > 0) {
        
        if (!is.null(preMZr)) {
          cutMZ <- .trim_vector(tg$mz, targets$mzmin[z], targets$mzmax[z])
          tg <- tg[tg$level == 2 | (tg$level == 1 & cutMZ), ]
          
          if (nrow(tg) > 0) {
            cutPreMZ <- .trim_vector(tg$pre_mz, preMZr$mzmin[z], preMZr$mzmax[z])
            tg <- tg[tg$level == 1 | (tg$level == 2 & cutPreMZ), ]
          }
          
        } else {
          cutMZ <- .trim_vector(tg$mz, targets$mzmin[z], targets$mzmax[z])
          tg <- tg[cutMZ, ]
        }
      }
      
      if (nrow(tg) == 0) return(NULL)
      
      if (nrow(tg) > 0) {
        tg$id <- targets$id[z]
        
      }
      
      tg
    },
    traces = traces,
    preMZr = preMZr,
    targets = targets
  )
  
  tg_list <- tg_list[!is.null(tg_list)]
  
  tg_df <- do.call("rbind", tg_list)
  
  tg_df
}

#' .caches_data
#'
#' @description Check if cache is possible and enabled via the global options.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
.caches_data <- function() {
  if (requireNamespace("patRoon", quietly = TRUE)) {
    ret <- getOption("patRoon.cache.mode", default = "both")
    if (ret %in% c("both", "save", "load")) {
      TRUE
    } else {
      FALSE
    }
  } else {
    FALSE
  }
}

#' .correlate_analysis_spectra
#'
#' @description Function to correlate MS spectra from analyses.
#'
#' @param spectra A data.table with columns "analysis", "mz" and "intensity".
#' Optionally, a column named "id" or "group" can be given to split the
#' data.table before correlation analysis by setting the argument
#' \code{splitSpectra} to \code{TRUE}. Note that when both "id" and "group"
#' columns are present "group" is used for splitting the data.table not "id".
#' If a column "replicate" is present and the argument \code{byReplicates}
#' is set to \code{TRUE}, the correlation is performed by replicate analysis
#' groups.
#' @param splitSpectra X.
#' @param byReplicates X.
#' @param decimals X.
#' @param minIntensity X.
#' @param method X.
#'
#' @return X.
#'
#' @noRd
#'
.correlate_analysis_spectra <- function(spectra,
                                        splitSpectra = FALSE,
                                        byReplicates = FALSE,
                                        decimals = 2,
                                        minIntensity = 1000,
                                        method = "pearson") {

  analysis <- NULL
  intensity <- NULL

  if (!is.data.table(spectra)) {
    warning("Spectra must be a data.table!")
    return(data.table())
  }

  if ("replicate" %in% colnames(spectra) & byReplicates) {
    spectra$analysis <- spectra$replicate
  } else {
    byReplicates <- FALSE
  }

  if (!"id" %in% colnames(spectra)) spectra$id <- NA_character_

  if ("group" %in% colnames(spectra)) spectra$id <- spectra$group

  if (!all(c("id", "analysis", "mz", "intensity") %in% colnames(spectra))) {
    warning("Spectra data.table does not containg mandatory columns!")
    return(data.table())
  }

  if (splitSpectra) {
    cor_list <- split(spectra, spectra$id)
  } else {
    cor_list <- list(spectra)
  }

  cor_list <- lapply(cor_list, function(x, minIntensity, decimals, method) {
    temp <- copy(x[, c("analysis", "mz", "intensity")])

    temp <- temp[temp$intensity >= minIntensity, ]

    for (i in unique(temp$analysis)) {
      temp$intensity[temp$analysis %in% i] <-
        temp$intensity[temp$analysis %in% i] /
        max(temp$intensity[temp$analysis %in% i])
    }

    temp$mz <- round(temp$mz, digits = decimals)

    mz <- NULL
    analysis <- NULL

    temp <- temp[
      data.table::CJ(analysis = analysis, mz = mz, unique = TRUE),
      on = list(analysis, mz)
    ]

    data.table::setnafill(temp, fill = 0, cols = "intensity")

    temp <- temp[, `:=`(intensity = sum(intensity)),
                 by = c("analysis", "mz")
    ][]

    temp <- unique(temp)

    temp <- matrix(temp$intensity,
                   nrow = length(unique(temp$mz)),
                   ncol = length(unique(temp$analysis)),
                   dimnames = list(
                     unique(temp$mz),
                     unique(temp$analysis)
                   )
    )

    temp <- cor(temp, method = method)

    temp <- as.data.table(temp, keep.rownames = "analysis")

    return(temp)
  }, decimals = decimals, minIntensity = minIntensity, method = method)

  id_col <- "id"

  if ("group" %in% colnames(spectra)) id_col <- "group"

  cor_list <- rbindlist(cor_list, idcol = "id")

  if (byReplicates) {
    setnames(cor_list, "analysis", "replicate")
  }

  cor_list
}



#### validity ------------------------------------------------------------

msFeatures_validity <- function(object) {

  valid <- TRUE

  if (nrow(object@metadata) > 0) {
    must_have_names <- c("id", "index", "rt", "mz", "drt", "rtmin", "rtmax", "dppm", "mzmin", "mzmax")
    valid <- !FALSE %in% (must_have_names %in% colnames(object@metadata))
  }

  # if (nrow(object@features) > 0) {
  #   must_have_names <- paste0("id")
  #   valid <- !FALSE %in% (must_have_names %in% colnames(object@features))
  # }

  return(valid)
}


### msFeatures ----------------------------------------------------------------------------------------------

#' msFeatures-class
#'
#' @description An S4 class representing an MS sample/file within the \pkg{streamFind} package.
#' The \code{msFeatures} is used to store and manage MS data and the respective methods can be used
#' for inspection, processing and evaluation.
#'
#' @template slot-msFeatures
#'
#' @references
#' \insertRef{patroon01}{streamFind}
#'
#' \insertRef{proteo01}{streamFind}
#'
#' \insertRef{proteo02}{streamFind}
#'
#' \insertRef{mzr01}{streamFind}
#'
#' \insertRef{mzr02}{streamFind}
#'
#' \insertRef{mzr03}{streamFind}
#'
#' \insertRef{mzr04}{streamFind}
#'
#' @export
#'
#' @importFrom data.table data.table
#'
#' @md
setClass("msFeatures",
         slots = c(
           analyses = "data.table",
           intensity = "data.table",
           metadata = "data.table",
           annotation = "list",
           parameters = "list"
         ),
         prototype = list(
           analyses = data.table(),
           intensity = data.table(),
           metadata = data.table(),
           annotation = list(),
           parameters = list()
         ),
         validity = msFeatures_validity
)


### S4 methods ----------------------------------------------------------------------------------------------

#### analysisNames ------------------------------------------------------------

#' @describeIn msFeatures getter for analysis names.
#'
#' @param object An \linkS4class{msFeatures} object.
#'
#' @export
#'
#' @aliases analysisNames,msFeatures,msFeatures-method
#'
setMethod("analysisNames", "msFeatures", function(object) object@analyses$analysis)

#### replicateNames ----------------------------------------------------------

#' @describeIn msFeatures getter for replicate names.
#'
#' @export
#'
#' @aliases replicateNames,msFeatures,msFeatures-method
#'
setMethod("replicateNames", "msFeatures", function(object) object@analyses$replicate)

#### blankReplicateNames --------------------------------------------------------------

#' @describeIn msFeatures getter for blank names.
#'
#' @export
#'
#' @aliases blankReplicateNames,msFeatures,msFeatures-method
#'
setMethod("blankReplicateNames", "msFeatures", function(object) object@analyses$blank)


### addParameters ----------------------------------------------------------

#' @describeIn msFeatures adds processing parameters for features.
#'
#' @template args-single-settings
#'
#' @export
#'
#' @aliases addParameters,msFeatures,msFeatures-method
#'
setMethod("addParameters", "msFeatures", function(object, settings) {

  valid <- checkmate::testClass(settings, "settings")

  if (!valid) {
    warning("Arguments not correct, returning original object!")
    return(object)
  }

  object@features@parameters[[getCall(settings)]] <- settings

  return(object)
})

### getParameters ----------------------------------------------------------

#' @describeIn msFeatures gets processing parameters.
#'
#' @param call The call name of the settings to retrieve.
#'
#' @export
#'
#' @aliases getParameters,msFeatures,msFeatures-method
#'
setMethod("getParameters", "msFeatures", function(object, call = NULL) {

  if (is.null(call)) {
    param <- object@parameters
  } else {
    param <- object@parameters[[call]]
  }

  return(param)
})

### features ------------------------------------------------------------------------------------------------

#' @describeIn msFeatures getter for features (i.e., grouped peaks). When
#' complete is set to \code{TRUE}, additional feature metadata is also returned.
#'
#' @template args-single-targetsID
#' @template args-makeTargets
#' @template args-single-filtered
#' @param complete Logical, set to \code{TRUE} for a complete
#' version of the output.
#' @param average Logical, set to \code{TRUE} for returning the intensity of
#' features averaged for each replicate group.
#'
#' @export
#'
#' @importFrom dplyr left_join
#' @importFrom data.table data.table
#'
#' @aliases features,msFeatures,msFeatures-method
#'
setMethod("features", "msFeatures", function(object,
                                             targetsID = NULL,
                                             mz = NULL, ppm = 20,
                                             rt = NULL, sec = 60, id = NULL,
                                             filtered = TRUE,
                                             complete = FALSE,
                                             average = TRUE) {

  if (!filtered) {
    mtd <- copy(object@metadata)
    ft_to_keep <- !mtd$filtered
    ft_to_keep <- mtd$id[ft_to_keep]
    feats <- object[, ft_to_keep]
  } else {
    feats <- object
  }

  if (!is.null(targetsID)) {
    out_fts <- feats@intensity[id %in% targetsID, ]
  } else if (!is.null(mz)) {
    targets <- makeTargets(mz = mz, rt = rt, ppm = ppm, sec = sec)
    sel <- rep(FALSE, nrow(feats@metadata))
    for (i in seq_len(nrow(targets))) {
      if (targets$rtmax[i] > 0) {
        sel[between(feats@metadata$mz, targets$mzmin[i], targets$mzmax[i]) &
              between(feats@metadata$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE
      } else {
        sel[between(feats@metadata$mz, targets$mzmin[i], targets$mzmax[i])] <- TRUE
      }
    }
    out_fts <- feats@intensity[sel]
  } else {
    out_fts <- feats@intensity
  }

  if (average) {
    rpl <- unique(feats@analyses$replicate)
    rpl_ana <- lapply(rpl, function(x, st) {
      st$analysis[st$replicate == x]
    }, st = feats@analyses)
    names(rpl_ana) <- rpl

    out_sd <- lapply(rpl_ana, function(x, out_fts) {
      temp <- out_fts[, x, with = FALSE]
      temp <- apply(temp, 1, function(x) sd(x) / mean(x) * 100)
      temp[is.nan(temp)] <- 0
      temp <- round(temp, digits = 0)
      return(temp)
    }, out_fts = out_fts)

    for (r in rpl) {
      out_fts[[r]] <- apply(out_fts[, .SD, .SDcols = rpl_ana[[r]]], 1, mean)
    }

    out_fts[, (feats@analyses$analysis) := NULL]

    names(out_sd) <- paste0(rpl, "_sd")
    out_fts <- cbind(out_fts, as.data.table(out_sd))
  }

  if (complete) {
    out_mtd <- feats@metadata[id %in% out_fts$id, ]
    out_fts <- left_join(out_mtd, out_fts, by = "id")
  }

  if (nrow(out_fts) < 1) {
    warning("Features not found in the msFeatures object.")
  }

  return(out_fts)
})

#### [ sub-setting analyses ----------------------------------------------

#' @describeIn msFeatures subset on analyses, using analysis index or name.
#'
#' @param x An \linkS4class{msFeatures} object.
#' @param i The indice/s or name/s of the analyses to keep in \code{x}.
#' @param drop Not applicable to \linkS4class{msFeatures}.
#' @param ... Other arguments.
#'
#' @export
#'
setMethod("[", c("msFeatures", "ANY", "missing", "missing"), function(x, i, ...) {

  if (!missing(i)) {
    if (nrow(x@analyses) > 0) {
      if (!is.character(i)) {
        sname <- x@analyses$analysis[i]
        sidx <- i
      } else {
        if (FALSE %in% (i %in% x@analyses$analysis[i])) {
          warning("Given analysis name/s not found in the msFeatures object.")
          return(x)
        }
        sname <- i
        sidx <- which(x@analyses$analysis %in% sname)
      }

      cols_rem <- which(!x@analyses$analysis %in% sname)

      cols_rem <- x@analyses$analysis[cols_rem]

      if (length(cols_rem) > 0) {

        x@analyses <- x@analyses[sidx, ]

        temp_int <- copy(x@intensity)

        temp_int[, (cols_rem) := NULL]

        check_null_intensity <- apply(temp_int[, 2:ncol(temp_int)], 1, function(z) max(z))
        check_null_intensity <- check_null_intensity == 0

        x@intensity <- temp_int[!check_null_intensity, ]

        x@metadata <- x@metadata[!check_null_intensity, ]

        x@metadata$peaks <- lapply(x@metadata$peaks, function(p, sname) {
          p <- p[, !names(p) %in% sname]
          return(p)
        }, sname = sname)

        # TODO check is is really necessary to have the components object, maybe a method to produce the components object can be added
        # if (length(x@annotation) > 0) {
        #   x@annotation[[1]] <- x@annotation[[1]][, which(check_null_intensity)]
        # }
      }
    }
  }
  return(x)
})

#### [ sub-setting features ----------------------------------------------

#' @describeIn msFeatures subset on features, using feature index or name.
#'
#' @param j The indice/s or \emph{id}/s for of features to keep..
#'
#' @export
#'
setMethod("[", c("msFeatures", "ANY", "ANY", "missing"), function(x, i, j,...) {

  if (!missing(i)) {
    x <- x[i]
  }

  if (!missing(j)) {
    if (nrow(x@metadata) == 0) {
      warning("There are no features in the msData object!")
      return(x)
    }

    if (!is.character(j)) j <- features(x)$id[j]

    x@intensity <- x@intensity[id %in% j, ]
    x@metadata <- x@metadata[id %in% j, ]
  }

  return(x)
})

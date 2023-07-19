#' trim_vector
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
trim_vector <- function(v, a, b) {
  rowSums(mapply(function(a, b) v >= a & v <= b, a = a, b = b)) > 0
}

#' trim_spectra_targets
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
trim_spectra_targets <- function(traces, targets, preMZr) {
  tg_list <- lapply(seq_len(nrow(targets)),
    function(z, traces, targets, preMZr) {
      tg <- traces
      cutRt <- trim_vector(tg$rt, targets$rtmin[z], targets$rtmax[z])
      tg <- tg[cutRt, ]
      if (nrow(tg) > 0) {
        if (!is.null(preMZr)) {
          cutMZ <- trim_vector(tg$mz, targets$mzmin[z], targets$mzmax[z])
          tg <- tg[tg$level == 2 | (tg$level == 1 & cutMZ), ]
          if (nrow(tg) > 0) {
            cutPreMZ <- trim_vector(tg$pre_mz, preMZr$mzmin[z], preMZr$mzmax[z])
            tg <- tg[tg$level == 1 | (tg$level == 2 & cutPreMZ), ]
          }
        } else {
          cutMZ <- trim_vector(tg$mz, targets$mzmin[z], targets$mzmax[z])
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
    traces = traces,
    preMZr = preMZr,
    targets = targets
  )
  tg_df <- do.call("rbind", tg_list)
  tg_df
}

#' Function to make targets for parsing data within MassSpecData class methods
#'
#' @description Helper function to build \emph{m/z} and retention time
#' target pairs for searching data. Each target is composed of an
#' id and \emph{m/z} (Da) and time (seconds) ranges. When mass is defined
#' without time, the time range return 0 and vice versa.
#'
#' @param mz A vector with target \emph{m/z} values (in Da) or a two columns
#' data.table or data.frame with minimum and maximum \emph{m/z} values (in Da).
#' Alternatively, \emph{m/z} and retention time values (in seconds) can be given
#' as one data.table or data.frame and the deviations given in the \code{ppm}
#' and \code{sec} arguments are used to calculate the ranges. Also works with a
#' data.table or data.frame with minimum and maximum values of \emph{m/z} and
#' retention time targets. Note that when mass/time ranges are given, the
#' \code{ppm} and \code{sec} arguments are not used.
#' @param rt A vector with target retention time values (in seconds) or
#' a two columns data.table or data.frame with minimum and maximum retention
#' time values (in seconds).
#' @param ppm Numeric of length one with the mass deviation, in ppm.
#' @param sec Numeric of length one with the time deviation, in seconds.
#' @param id Character with the same length as \emph{m/z} and retention time
#' targets to be used as identifiers. When not given, the id is built as a
#' combination of the \emph{m/z} and retention time ranges or values.
#'
#' @return A data.frame with columns: *id*, *mz*, *rt*, *mzmin*, *mzmax*,
#' *rtmin*, *rtmax*.
#'
#' @export
#'
make_ms_targets <- function(mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL) {
  mzrts <- data.table(
    id = NA_character_,
    mz = 0,
    rt = 0,
    mzmin = 0,
    mzmax = 0,
    rtmin = 0,
    rtmax = 0
  )

  cols_mz_ranges <- c("mzmin", "mzmax")
  cols_rt_ranges = c("rtmin", "rtmax")

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
      mzrts$rtmin <- mzrts$rt - sec
      mzrts$rtmax <- mzrts$rt + sec

      # adds id
      if (!is.null(id) & length(id) == length(rt)) {
        mzrts$id <- id
      } else {
        mzrts$id <- paste(mzrts$rtmin, "-", mzrts$rtmax, sep = "")
      }

      # as table
    } else if (is.data.frame(rt) | is.data.table(rt)) {
      rt <- as.data.table(rt)

      if ("rt" %in% colnames(rt) & !"rtmin" %in% colnames(mz)) {
        mzrts <- data.table(
          id = NA_character_,
          mz = 0,
          rt = rt,
          mzmin = 0,
          mzmax = 0,
          rtmin = 0,
          rtmax = 0
        )
        mzrts$rtmin <- rt$rt - sec
        mzrts$rtmax <- rt$rt + sec
      } else if ("rtmin" %in% colnames(rt)) {
        mzrts <- data.table(
          id = NA_character_,
          mz = 0,
          rt = apply(rt[, cols_rt_ranges, with = FALSE], 1, mean),
          mzmin = 0,
          mzmax = 0,
          rtmin = rt$rtmin,
          rtmax = rt$rtmax
        )

        if ("rt" %in% colnames(rt)) {
          mzrts$rt <- mz$rt
        } else {
          mzrts$rt <- apply(rt[, cols_rt_ranges, with = FALSE], 1, mean)
        }
      }

      # adds id
      if (length(id) == nrow(mzrts) & !is.null(id)) {
        mzrts$id <- id
      } else if ("id" %in% colnames(rt)) {
        mzrts$id <- rt$id
      } else {
        mzrts$id <- paste(mzrts$rtmin, "-", mzrts$rtmax, sep = "")
      }

      if ("analysis" %in% colnames(rt)) mzrts$analysis <- rt$analysis
    }

    # when mz is vector, expects rt as vector as well and ranges are calculated
  } else if (length(mz) >= 1 & is.vector(mz)) {
    mzrts <- data.table(
      id = NA_character_,
      mz = mz,
      rt = 0,
      mzmin = mz - ((ppm / 1E6) * mz),
      mzmax = mz + ((ppm / 1E6) * mz),
      rtmin = 0,
      rtmax = 0
    )

    if (is.vector(rt) & length(rt) == length(mz)) {
      mzrts$rt <- rt
      mzrts$rtmin <- c(rt - sec)
      mzrts$rtmax <- c(rt + sec)
    }

    if (!is.null(id) & length(id) == nrow(mzrts)) {
      mzrts$id <- id
    } else {
      mzrts$id <- paste(
        round(mzrts$mzmin, 4),
        "-",
        round(mzrts$mzmax, 4),
        "/", mzrts$rtmin,
        "-", mzrts$rtmax,
        sep = ""
      )
    }

    # when mz is a table, ranges could be already in table
  } else if (is.data.frame(mz) | is.data.table(mz)) {
    mz <- as.data.table(mz)

    # when mz is in table but not ranges
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
      mzrts$mzmin <- mzrts$mz - ((ppm / 1E6) * mzrts$mz)
      mzrts$mzmax <- mzrts$mz + ((ppm / 1E6) * mzrts$mz)

      # when mzmin is in table
    } else if ("mzmin" %in% colnames(mz)) {
      mzrts <- data.table(
        id = NA_character_,
        mz = apply(mz[, cols_mz_ranges, with = FALSE], 1, mean),
        rt = 0,
        mzmin = mz$mzmin,
        mzmax = mz$mzmax,
        rtmin = 0,
        rtmax = 0
      )
      if ("mz" %in% colnames(mz)) mzrts$mz <- mz$mz
    }

    # when rt in also in mz table
    if ("rt" %in% colnames(mz) & !"rtmin" %in% colnames(mz)) {
      mzrts$rt <- mz$rt
      mzrts$rtmin <- mz$rt - sec
      mzrts$rtmax <- mz$rt + sec
    } else if ("rtmin" %in% colnames(mz)) {
      mzrts$rt <- apply(mz[, cols_rt_ranges, with = FALSE], 1, mean)
      mzrts$rtmin <- mz$rtmin
      mzrts$rtmax <- mz$rtmax
      if ("rt" %in% colnames(mz)) mzrts$rt <- mz$rt
    }

    # when rt is given as a table is rt argument
    if (is.data.frame(rt) | is.data.table(rt)) {
      rt <- as.data.table(rt)

      if ("rt" %in% colnames(rt) &
          nrow(rt) == nrow(mz) &
          !"rtmin" %in% colnames(mz)) {
        mzrts$rt <- rt$rt
        mzrts$rtmin <- rt$rt - sec
        mzrts$rtmax <- rt$rt + sec
      } else if ("rtmin" %in% colnames(rt) & nrow(rt) == nrow(mz)) {
        mzrts$rt <- apply(rt[, cols_rt_ranges, with = FALSE], 1, mean)
        mzrts$rtmin <- rt$rtmin
        mzrts$rtmax <- rt$rtmax
        if ("rt" %in% colnames(rt)) mzrts$rt <- mz$rt
      }
    }

    # adds id
    if (!is.null(id) & length(id) == nrow(mzrts)) {
      mzrts$id <- id
    } else if ("id" %in% colnames(mz)) {
      mzrts$id <- mz$id
    } else {
      mzrts$id <- paste(
        round(mzrts$mzmin, 4),
        "-",
        round(mzrts$mzmax, 4),
        "/",
        mzrts$rtmin,
        "-",
        mzrts$rtmax,
        sep = ""
      )
    }

    if ("analysis" %in% colnames(mz)) mzrts$analysis <- mz$analysis
  }

  mzrts
}

#' @title get_colors
#'
#' @description Function to produce colors for a character vector.
#'
#' @param obj A character vector to associate with the colors.
#'
#' @return A named vector of colors. The names of the vector is the \code{obj}.
#'
#' @noRd
#'
get_colors <- function(obj) {
  colors <- c(
    brewer.pal(8, "Greys")[6],
    brewer.pal(8, "Greens")[6],
    brewer.pal(8, "Blues")[6],
    brewer.pal(8, "Oranges")[6],
    brewer.pal(8, "Purples")[6],
    brewer.pal(8, "PuRd")[6],
    brewer.pal(8, "YlOrRd")[6],
    brewer.pal(8, "PuBuGn")[6],
    brewer.pal(8, "GnBu")[6],
    brewer.pal(8, "BuPu")[6],
    brewer.pal(8, "Dark2")
  )

  Ncol <- length(unique(obj))

  if (Ncol > 18) {
    colors <- colorRampPalette(colors)(Ncol)
  }

  if (length(unique(obj)) < length(obj)) {
    Vcol <- colors[seq_len(Ncol)]
    Ncol <- length(obj)
    char <- NULL
    count <- dplyr::count(data.frame(n = seq_len(Ncol), char = obj), char)
    Vcol <- rep(Vcol, times = count[, "n"])
    names(Vcol) <- obj
  } else {
    Vcol <- colors[seq_len(Ncol)]
    names(Vcol) <- obj
  }

  Vcol
}

#' @title correlate_analysis_spectra
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
correlate_analysis_spectra <- function(spectra,
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

#' caches_data
#'
#' @description Check if cache is possible and enabled via the global options.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
caches_data <- function() {
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

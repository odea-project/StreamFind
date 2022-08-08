
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
      mzmin = mz - ((ppm / 1E6) * mz),
      mzmax = mz + ((ppm / 1E6) * mz),
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

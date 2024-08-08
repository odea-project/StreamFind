#' Function to make targets for parsing data within MassSpecData class methods
#'
#' @description Helper function to build \emph{m/z} and retention time and/or drift time targets for searching MS data. 
#' Each target is composed of an id, \emph{m/z} (in Da), retention time (in seconds) and drift time (in milliseconds) 
#' ranges.
#'
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-drift
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#'
#' @return A data.frame with columns: *id*, *mz*, *rt*, *drift*, *mzmin*, *mzmax*, *rtmin*, *rtmax*, *driftmin* and 
#' *driftmax*.
#'
#' @export
#'
make_ms_targets <- function(mz = NULL, rt = NULL, drift = NULL, ppm = 20, sec = 60, millisec = 5, id = NULL) {
  
  cols_mz <- c("mz")
  cols_rt <- c("rt")
  cols_drift <- c("drift")
  cols_mz_ranges <- c("mzmin", "mzmax")
  cols_rt_ranges = c("rtmin", "rtmax")
  cols_drift_ranges = c("driftmin", "driftmax")
  
  if (is.data.frame(mz)) {
    checkmate::assert_true(cols_mz %in% colnames(mz) || all(cols_mz_ranges %in% colnames(mz)))
  } else {
    checkmate::assert_numeric(mz, null.ok = TRUE)
    if (is.vector(mz)) mz <- data.table::data.table("mz" = mz)
  }
  
  if (is.data.frame(rt)) {
    checkmate::assert_true(cols_rt %in% colnames(rt) || all(cols_rt_ranges %in% colnames(mz)))
  } else {
    checkmate::assert_numeric(rt, null.ok = TRUE)
    if (is.vector(rt)) rt <- data.table::data.table("rt" = rt)
  }
  
  if (is.data.frame(drift)) {
    checkmate::assert_true(cols_drift %in% colnames(drift) || all(cols_drift_ranges %in% colnames(drift)))
  } else {
    checkmate::assert_numeric(drift, null.ok = TRUE)
    if (is.vector(drift)) drift <- data.table::data.table("drift" = drift)
  }
  
  checkmate::assert_numeric(ppm, len = 1, null.ok = TRUE)
  
  checkmate::assert_numeric(sec, len = 1, null.ok = TRUE)
  
  checkmate::assert_numeric(millisec, len = 1, null.ok = TRUE)
  
  checkmate::assert_character(as.character(id), null.ok = TRUE)
  
  if (is.data.frame(mz) & "name" %in% colnames(mz)) data.table::setnames(mz, "name", "id")
  
  if (is.data.frame(rt) & "name" %in% colnames(rt)) data.table::setnames(rt, "name", "id")
  
  if (is.data.frame(drift) & "name" %in% colnames(drift)) data.table::setnames(drift, "name", "id")
  
  targets <- data.table::data.table(
    mz = 0, rt = 0, drift = 0,
    mzmin = 0, mzmax = 0, rtmin = 0, rtmax = 0, driftmin = 0, driftmax = 0
  )
  
  if (is.data.frame(mz)) {
    
    targets <- mz
    
    if (cols_mz %in% colnames(mz) && !all(cols_mz_ranges %in% colnames(mz))) {
      targets$mzmin <- targets$mz - ((ppm / 1E6) * targets$mz)
      targets$mzmax <- targets$mz + ((ppm / 1E6) * targets$mz)
    }
    
    if (!cols_mz %in% colnames(mz)) targets$mz <- apply(mz[, cols_mz_ranges, with = FALSE], 1, mean)
    
    if (is.data.frame(rt)) {
      
      if (nrow(rt) == nrow(targets)) {
        
        if (cols_rt %in% colnames(rt) && !all(cols_rt_ranges %in% colnames(rt))) {
          targets$rt <- rt$rt
          targets$rtmin <- rt$rt - sec
          targets$rtmax <- rt$rt + sec
          
        } else if (all(cols_rt_ranges %in% colnames(rt))) {
          targets$rtmin <- rt$rtmin
          targets$rtmax <- rt$rtmax
        }
        
        if (!cols_rt %in% colnames(rt)) targets$rt <- apply(rt[, cols_rt_ranges, with = FALSE], 1, mean)
      }
      
    } else if ("rt" %in% colnames(targets)) {
      
      if (!all(cols_rt_ranges %in% colnames(targets))) {
        targets$rtmin <- targets$rt - sec
        targets$rtmax <- targets$rt + sec
      }
    }
    
    if (is.data.frame(drift)) {
      
      if (nrow(drift) == nrow(targets)) {
        
        if (cols_drift %in% colnames(drift) && !all(cols_drift_ranges %in% colnames(drift))) {
          targets$drift <- drift$drift
          targets$driftmin <- drift$drift - millisec
          targets$driftmax <- drift$drift + millisec
          
        } else if (all(cols_drift_ranges %in% colnames(drift))) {
          targets$driftmin <- drift$driftmin
          targets$driftmax <- drift$driftmax
        }
        
        if (!cols_drift %in% colnames(drift)) targets$drift <- apply(drift[, cols_drift_ranges, with = FALSE], 1, mean)
      }
      
    } else if ("drift" %in% colnames(targets)) {
      
      if (!all(cols_drift_ranges %in% colnames(targets))) {
        targets$driftmin <- targets$drift - millisec
        targets$driftmax <- targets$drift + millisec
      }
    }
    
    if ("analysis" %in% colnames(mz)) targets$analysis <- mz$analysis
    
    if ("polarity" %in% colnames(mz)) targets$polarity <- mz$polarity
    
  } else if (is.data.frame(rt)) {
    
    targets <- rt
    
    if (cols_rt %in% colnames(rt) && !all(cols_rt_ranges %in% colnames(rt))) {
      targets$rtmin <- targets$rt - sec
      targets$rtmax <- targets$rt + sec
    }
    
    if (!cols_rt %in% colnames(rt)) targets$rt <- apply(rt[, cols_rt_ranges, with = FALSE], 1, mean)
    
    if (is.data.frame(drift)) {
      
      if (nrow(drift) == nrow(targets)) {
        
        if (cols_drift %in% colnames(drift) && !all(cols_drift_ranges %in% colnames(drift))) {
          targets$drift <- drift$drift
          targets$driftmin <- drift$drift - millisec
          targets$driftmax <- drift$drift + millisec
          
        } else if (all(cols_drift_ranges %in% colnames(drift))) {
          targets$driftmin <- drift$driftmin
          targets$driftmax <- drift$driftmax
        }
        
        if (!cols_drift %in% colnames(drift)) targets$drift <- apply(drift[, cols_drift_ranges, with = FALSE], 1, mean)
      }
      
    } else if ("drift" %in% colnames(targets)) {
      
      if (!all(cols_drift_ranges %in% colnames(targets))) {
        targets$driftmin <- targets$drift - millisec
        targets$driftmax <- targets$drift + millisec
      }
    }
    
  } else if (is.data.frame(drift)) {
    
    targets <- drift
    
    if (cols_drift %in% colnames(drift) && !all(cols_drift_ranges %in% colnames(drift))) {
      targets$driftmin <- targets$drift - millisec
      targets$driftmax <- targets$drift + millisec
    }
    
    if (!cols_drift %in% colnames(drift)) targets$drift <- apply(drift[, cols_drift_ranges, with = FALSE], 1, mean)
  }
  
  if (!cols_mz %in% colnames(targets)) targets$mz <- 0
  if (!cols_rt %in% colnames(targets)) targets$rt <- 0
  if (!cols_drift %in% colnames(targets)) targets$drift <- 0
  if (!cols_mz_ranges[1] %in% colnames(targets)) targets$mzmin <- 0
  if (!cols_mz_ranges[2] %in% colnames(targets)) targets$mzmax <- 0
  if (!cols_rt_ranges[1] %in% colnames(targets)) targets$rtmin <- 0
  if (!cols_rt_ranges[2] %in% colnames(targets)) targets$rtmax <- 0
  if (!cols_drift_ranges[1] %in% colnames(targets)) targets$driftmin <- 0
  if (!cols_drift_ranges[2] %in% colnames(targets)) targets$driftmax <- 0
  
  targets$mz[targets$mz < 0] <- 0
  targets$mzmin[targets$mzmin < 0] <- 0
  targets$mzmax[targets$mzmax < 0] <- 0
  targets$rt[targets$rt < 0] <- 0
  targets$rtmin[targets$rtmin < 0] <- 0
  targets$rtmax[targets$rtmax < 0] <- 0
  targets$drift[targets$drift < 0] <- 0
  targets$driftmin[targets$driftmin < 0] <- 0
  targets$driftmax[targets$driftmax < 0] <- 0
  
  if (!is.null(id)) if (length(id) == nrow(targets)) targets$id <- id
  
  if (!"id" %in% colnames(targets)) {
    
    if (!"id" %in% colnames(targets)) {
      targets$id <- paste(
        round(targets$mzmin, 3),
        "-",
        round(targets$mzmax, 3),
        "/",
        round(targets$rtmin, 0),
        "-",
        round(targets$rtmax, 0),
        "/",
        round(targets$driftmin, 0),
        "-",
        round(targets$driftmax, 0),
        sep = ""
      )
    }
  }
  
  data.table::setcolorder(targets, c("id", "mz", "rt", "drift", "mzmin", "mzmax", "rtmin", "rtmax", "driftmin", "driftmax"))
  
  targets
}

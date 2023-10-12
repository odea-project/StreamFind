#' Function to make targets for parsing data within MassSpecData class methods
#'
#' @description Helper function to build \emph{m/z} and retention time and/or
#' drift time targets for searching the MS data. Each target is composed of an
#' id, \emph{m/z} (in Da), retention time (in seconds) and drift time (in 
#' milliseconds) ranges. When mass is defined without time variables, the 
#' retention and drift time ranges return 0 and vice versa.
#'
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-drift
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#'
#' @return A data.frame with columns: *id*, *mz*, *rt*, *drift*, *mzmin*, 
#' *mzmax*, *rtmin*, *rtmax*, *driftmin* and *driftmax*.
#'
#' @export
#'
make_ms_targets <- function(mz = NULL,
                            rt = NULL,
                            drift = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            id = NULL) {
  
  targets <- data.table(
    id = NA_character_,
    mz = 0,
    rt = 0,
    drift = 0,
    mzmin = 0,
    mzmax = 0,
    rtmin = 0,
    rtmax = 0,
    driftmin = 0,
    driftmax = 0
  )
  
  if (is.data.frame(mz) & "name" %in% colnames(mz)) {
    colnames(mz)[colnames(mz) == "name"] <- "id"
  }
  
  if (is.data.frame(rt) & "name" %in% colnames(rt)) {
    colnames(rt)[colnames(rt) == "name"] <- "id"
  }
  
  if (is.data.frame(drift) & "name" %in% colnames(drift)) {
    colnames(drift)[colnames(drift) == "name"] <- "id"
  }
  
  cols_mz_ranges <- c("mzmin", "mzmax")
  
  cols_rt_ranges = c("rtmin", "rtmax")
  
  cols_drift_ranges = c("driftmin", "driftmax")
  
  # when mz is not given but rt and possible drift
  if (is.null(mz) & !is.null(rt)) {
    
    # as vector
    if (length(rt) >= 1 & is.vector(rt)) {
      targets <- data.table(
        id = NA_character_,
        mz = 0,
        rt = rt,
        drift = 0,
        mzmin = 0,
        mzmax = 0,
        rtmin = 0,
        rtmax = 0,
        driftmin = 0,
        driftmax = 0
      )
      
      targets$rtmin <- targets$rt - sec
      targets$rtmax <- targets$rt + sec
      
      # adds id
      if (!is.null(id) & length(id) == length(rt)) {
        targets$id <- id
      } else {
        targets$id <- paste(targets$rtmin, "-", targets$rtmax, sep = "")
      }
      
      # as table
    } else if (is.data.frame(rt) | is.data.table(rt)) {
      
      rt <- as.data.table(rt)
      
      if ("rt" %in% colnames(rt) & !"rtmin" %in% colnames(rt)) {
        targets <- data.table(
          id = NA_character_,
          mz = 0,
          rt = rt,
          drift = 0,
          mzmin = 0,
          mzmax = 0,
          rtmin = 0,
          rtmax = 0,
          driftmin = 0,
          driftmax = 0
        )
        
        targets$rtmin <- rt$rt - sec
        targets$rtmax <- rt$rt + sec
        
      } else if ("rtmin" %in% colnames(rt)) {
        targets <- data.table(
          id = NA_character_,
          mz = 0,
          rt = apply(rt[, cols_rt_ranges, with = FALSE], 1, mean),
          drift = 0,
          mzmin = 0,
          mzmax = 0,
          rtmin = rt$rtmin,
          rtmax = rt$rtmax,
          driftmin = 0,
          driftmax = 0
        )
        
        if ("rt" %in% colnames(rt)) {
          targets$rt <- rt$rt
        } else {
          targets$rt <- apply(rt[, cols_rt_ranges, with = FALSE], 1, mean)
        }
      }
      
      # adds id
      if (length(id) == nrow(targets) & !is.null(id)) {
        targets$id <- id
      } else if ("id" %in% colnames(rt)) {
        targets$id <- rt$id
      } else {
        targets$id <- paste(targets$rtmin, "-", targets$rtmax, sep = "")
      }
      
      if ("analysis" %in% colnames(rt)) targets$analysis <- rt$analysis
    }
    
    # when drift in also in rt table
    if ("drift" %in% colnames(rt) & !"driftmin" %in% colnames(rt)) {
      targets$drift <- rt$drift
      targets$driftmin <- rt$drift - millisec
      targets$driftmax <- rt$drift + millisec
      
    } else if ("driftmin" %in% colnames(rt)) {
      targets$drift <- apply(rt[, cols_drift_ranges, with = FALSE], 1, mean)
      targets$driftmin <- rt$driftmin
      targets$driftmax <- rt$driftmax
      
      if ("drift" %in% colnames(rt)) targets$drift <- rt$drift
    }
    
    # when drift is given as a table in drift argument
    if (is.data.frame(drift) | is.data.table(drift)) {
      drift <- as.data.table(drift)
      
      if ("drift" %in% colnames(drift) & nrow(drift) == nrow(rt) & !"driftmin" %in% colnames(drift)) {
        targets$drift <- drift$drift
        targets$driftmin <- drift$drift - millisec
        targets$driftmax <- drift$drift + millisec
        
      } else if ("driftmin" %in% colnames(drift) & nrow(drift) == nrow(rt)) {
        targets$drift <- apply(drift[, cols_drift_ranges, with = FALSE], 1, mean)
        targets$driftmin <- drift$driftmin
        targets$driftmax <- drift$driftmax
        
        if ("drift" %in% colnames(drift)) targets$drift <- drift$drift
      }
    }
    
    # when only drift is given
  } else if (is.null(mz) & is.null(rt) & !is.null(drift)) {
    
    # as vector
    if (length(drift) >= 1 & is.vector(drift)) {
      targets <- data.table(
        id = NA_character_,
        mz = 0,
        rt = 0,
        drift = drift,
        mzmin = 0,
        mzmax = 0,
        rtmin = 0,
        rtmax = 0,
        driftmin = 0,
        driftmax = 0
      )
      
      targets$driftmin <- targets$drift - millisec
      targets$driftmax <- targets$drift + millisec
      
      # adds id
      if (!is.null(id) & length(id) == length(drift)) {
        targets$id <- id
      } else {
        targets$id <- paste(targets$driftmin, "-", targets$driftmax, sep = "")
      }
      
      # as table
    } else if (is.data.frame(drift)) {
      
      drift <- as.data.table(drift)
      
      if ("drift" %in% colnames(drift) & !"driftmin" %in% colnames(drift)) {
        targets <- data.table(
          id = NA_character_,
          mz = 0,
          rt = 0,
          drift = drift,
          mzmin = 0,
          mzmax = 0,
          rtmin = 0,
          rtmax = 0,
          driftmin = 0,
          driftmax = 0
        )
        
        targets$driftmin <- drift$drift - millisec
        targets$driftmax <- drift$drift + millisec
        
      } else if ("driftmin" %in% colnames(drift)) {
        targets <- data.table(
          id = NA_character_,
          mz = 0,
          rt = 0,
          drift = apply(drift[, cols_drift_ranges, with = FALSE], 1, mean),
          mzmin = 0,
          mzmax = 0,
          rtmin = 0,
          rtmax = 0,
          driftmin = drift$driftmin,
          driftmax = drift$driftmax
        )
        
        if ("drift" %in% colnames(drift)) {
          targets$drift <- drift$drift
        } else {
          targets$drift <- apply(drift[, cols_drift_ranges, with = FALSE], 1, mean)
        }
      }
      
      # adds id
      if (length(id) == nrow(targets) & !is.null(id)) {
        targets$id <- id
      } else if ("id" %in% colnames(drift)) {
        targets$id <- drift$id
      } else {
        targets$id <- paste(targets$driftmin, "-", targets$driftmax, sep = "")
      }
      
      if ("analysis" %in% colnames(drift)) targets$analysis <- drift$analysis
    }
    
    # when mz is vector, expects rt as vector as well and ranges are calculated
  } else if (length(mz) >= 1 & is.vector(mz)) {
    targets <- data.table(
      id = NA_character_,
      mz = mz,
      rt = 0,
      drift = 0,
      mzmin = mz - ((ppm / 1E6) * mz),
      mzmax = mz + ((ppm / 1E6) * mz),
      rtmin = 0,
      rtmax = 0,
      driftmin = 0,
      driftmax = 0
    )
    
    if (is.vector(rt) & length(rt) == length(mz)) {
      targets$rt <- rt
      targets$rtmin <- c(rt - sec)
      targets$rtmax <- c(rt + sec)
    }
    
    with_drift = FALSE
    
    if (is.vector(drift) & length(drift) == length(mz)) {
      targets$drift <- drift
      targets$driftmin <- c(drift - millisec)
      targets$driftmax <- c(drift + millisec)
      with_drift = TRUE
    }
    
    if (!is.null(id) & length(id) == nrow(targets)) {
      targets$id <- id
      
    } else if (with_drift) {
      targets$id <- paste(
        round(targets$mzmin, 4),
        "-",
        round(targets$mzmax, 4),
        "/",
        round(targets$rtmin, 1),
        "-",
        round(targets$rtmax, 1),
        "/",
        round(targets$driftmin, 1),
        "-",
        round(targets$driftmax, 1),
        sep = ""
      )
      
    } else {
      targets$id <- paste(
        round(targets$mzmin, 4),
        "-",
        round(targets$mzmax, 4),
        "/",
        round(targets$rtmin, 1),
        "-",
        round(targets$rtmax, 1),
        sep = ""
      )
    }
    
    # when mz is a table, ranges could be already in table
  } else if (is.data.frame(mz)) {
    mz <- as.data.table(mz)
    
    # when mz is in table but not ranges
    if ("mz" %in% colnames(mz) & !"mzmin" %in% colnames(mz)) {
      targets <- data.table(
        id = NA_character_,
        mz = mz$mz,
        rt = 0,
        drift = 0,
        mzmin = 0,
        mzmax = 0,
        rtmin = 0,
        rtmax = 0,
        driftmin = 0,
        driftmax = 0
      )
      targets$mzmin <- targets$mz - ((ppm / 1E6) * targets$mz)
      targets$mzmax <- targets$mz + ((ppm / 1E6) * targets$mz)
      
      # when mzmin is in table
    } else if ("mzmin" %in% colnames(mz)) {
      targets <- data.table(
        id = NA_character_,
        mz = apply(mz[, cols_mz_ranges, with = FALSE], 1, mean),
        rt = 0,
        drift = 0,
        mzmin = mz$mzmin,
        mzmax = mz$mzmax,
        rtmin = 0,
        rtmax = 0,
        driftmin = 0,
        driftmax = 0
      )
      
      if ("mz" %in% colnames(mz)) targets$mz <- mz$mz
    }
    
    # when rt in also in mz table
    if ("rt" %in% colnames(mz) & !"rtmin" %in% colnames(mz)) {
      targets$rt <- mz$rt
      targets$rtmin <- mz$rt - sec
      targets$rtmax <- mz$rt + sec
      
    } else if ("rtmin" %in% colnames(mz)) {
      targets$rt <- apply(mz[, cols_rt_ranges, with = FALSE], 1, mean)
      targets$rtmin <- mz$rtmin
      targets$rtmax <- mz$rtmax
      
      if ("rt" %in% colnames(mz)) targets$rt <- mz$rt
    }
    
    # when rt is given as a table is rt argument
    if (is.data.frame(rt)) {
      rt <- as.data.table(rt)
      
      if ("rt" %in% colnames(rt) & nrow(rt) == nrow(mz) & !"rtmin" %in% colnames(rt)) {
        targets$rt <- rt$rt
        targets$rtmin <- rt$rt - sec
        targets$rtmax <- rt$rt + sec
        
      } else if ("rtmin" %in% colnames(rt) & nrow(rt) == nrow(mz)) {
        targets$rt <- apply(rt[, cols_rt_ranges, with = FALSE], 1, mean)
        targets$rtmin <- rt$rtmin
        targets$rtmax <- rt$rtmax
        
        if ("rt" %in% colnames(rt)) targets$rt <- rt$rt
      }
    }
    
    with_drift <- FALSE
    
    # when drift in also in mz table
    if ("drift" %in% colnames(mz) & !"driftmin" %in% colnames(mz)) {
      targets$drift <- mz$drift
      targets$driftmin <- mz$drift - millisec
      targets$driftmax <- mz$drift + millisec
      with_drift <- TRUE
      
    } else if ("driftmin" %in% colnames(mz)) {
      targets$drift <- apply(mz[, cols_drift_ranges, with = FALSE], 1, mean)
      targets$driftmin <- mz$driftmin
      targets$driftmax <- mz$driftmax
      with_drift <- TRUE
      
      if ("drift" %in% colnames(mz)) targets$drift <- mz$drift
    }
    
    # when drift is given as a table in drift argument
    if (is.data.frame(drift)) {
      drift <- as.data.table(drift)
      
      if ("drift" %in% colnames(drift) & nrow(drift) == nrow(mz) & !"driftmin" %in% colnames(drift)) {
        targets$drift <- drift$drift
        targets$driftmin <- drift$drift - millisec
        targets$driftmax <- drift$drift + millisec
        
      } else if ("driftmin" %in% colnames(drift) & nrow(drift) == nrow(mz)) {
        targets$drift <- apply(drift[, cols_drift_ranges, with = FALSE], 1, mean)
        targets$driftmin <- drift$driftmin
        targets$driftmax <- drift$driftmax
        
        if ("drift" %in% colnames(drift)) targets$drift <- drift$drift
      }
    }
    
    # adds id
    if (!is.null(id) & length(id) == nrow(targets)) {
      targets$id <- id
      
    } else if ("id" %in% colnames(mz)) {
      targets$id <- mz$id
      
    } else if (with_drift) {
      targets$id <- paste(
        round(targets$mzmin, 4),
        "-",
        round(targets$mzmax, 4),
        "/",
        round(targets$rtmin, 1),
        "-",
        round(targets$rtmax, 1),
        "/",
        round(targets$driftmin, 1),
        "-",
        round(targets$driftmax, 1),
        sep = ""
      )
      
    } else {
      targets$id <- paste(
        round(targets$mzmin, 4),
        "-",
        round(targets$mzmax, 4),
        "/",
        round(targets$rtmin, 1),
        "-",
        round(targets$rtmax, 1),
        sep = ""
      )
    }
    
    if ("analysis" %in% colnames(mz)) targets$analysis <- mz$analysis
    
    if ("polarity" %in% colnames(mz)) targets$polarity <- mz$polarity
  }
  
  targets
}


#' @export
#' @noRd
MassSpecTargets <- S7::new_class("MassSpecTargets", package = "StreamFind",

  properties = list(
   
    targets = S7::new_property(S7::class_data.frame, default = data.frame())
   
  ),
  
  constructor = function(
    mz = NULL,
    rt = NULL,
    mobility = NULL,
    ppm = 20,
    sec = 60,
    millisec = 5,
    id = NULL,
    analyses = NULL,
    polarities = NULL) {
    
    cols_mass <- c("mass")
    cols_mz <- c("mz")
    cols_rt <- c("rt")
    cols_mobility <- c("mobility")
    cols_mz_ranges <- c("mzmin", "mzmax")
    cols_rt_ranges = c("rtmin", "rtmax")
    cols_mobility_ranges = c("mobilitymin", "mobilitymax")
    
    if (is.data.frame(mz)) {
      checkmate::assert_true(cols_mz %in% colnames(mz) || all(cols_mz_ranges %in% colnames(mz)) || cols_mass %in% colnames(mz))
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
    
    if (is.data.frame(mobility)) {
      checkmate::assert_true(cols_mobility %in% colnames(mobility) || all(cols_mobility_ranges %in% colnames(mobility)))
    } else {
      checkmate::assert_numeric(mobility, null.ok = TRUE)
      if (is.vector(mobility)) mobility <- data.table::data.table("mobility" = mobility)
    }
    
    checkmate::assert_numeric(ppm, len = 1, null.ok = TRUE)
    
    checkmate::assert_numeric(sec, len = 1, null.ok = TRUE)
    
    checkmate::assert_numeric(millisec, len = 1, null.ok = TRUE)
    
    checkmate::assert_character(as.character(id), null.ok = TRUE)
    
    if (is.data.frame(mz) & "name" %in% colnames(mz)) data.table::setnames(mz, "name", "id")
    
    if (is.data.frame(rt) & "name" %in% colnames(rt)) data.table::setnames(rt, "name", "id")
    
    if (is.data.frame(mobility) & "name" %in% colnames(mobility)) data.table::setnames(mobility, "name", "id")
    
    targets <- data.table::data.table(
      mz = 0, rt = 0, mobility = 0,
      mzmin = 0, mzmax = 0, rtmin = 0, rtmax = 0, mobilitymin = 0, mobilitymax = 0
    )
    
    if (is.data.frame(mz)) {
      
      targets <- mz
      
      if (!cols_mz %in% colnames(mz) && !any(cols_mz_ranges %in% colnames(mz)) && cols_mass %in% colnames(mz)) {
        
        if (!is.null(polarities) && (!is.null(analyses) || "analysis" %in% colnames(mz))) {
          checkmate::assert_character(polarities, null.ok = TRUE)
          checkmate::assert_named(polarities, type = "named")
          checkmate::assert_true(length(polarities) == length(analyses) || (length(polarities) == nrow(mz) && "analysis" %in% colnames(mz)))
          checkmate::assert_true(all(names(polarities) %in% analyses) || all(names(polarities) %in% mz[["analysis"]]))
        } else if (!is.null(polarities)) {
          checkmate::assert_character(polarities, null.ok = TRUE)
          checkmate::assert_true(
            (all(polarities %in% c("positive", "negative")) && length(polarities) == 2) ||
              (all(polarities %in% c("positive", "negative")) && length(polarities) == nrow(targets))
          )
        }
        
        if (is.null(polarities) && !"polarity" %in% colnames(mz)) {
          warning("No polarity column found in mz data frame but only mass was given!")
        
        # case 1: polarity is given in as column for each target
        } else if (is.null(polarities) && "polarity" %in% colnames(mz)) {
          targets$mz <- 0
          for (i in seq_len(nrow(targets))) {
            if (as.character(targets$polarity[i]) %in% "positive") {
              targets$mz[i] <- targets$mass[i] + 1.007276
            } else if (as.character(targets$polarity[i]) %in% "negative") {
              targets$mz[i] <- targets$mass[i] - 1.007276
            } else if (grepl("positive", targets$polarity[i]) && grepl("negative", targets$polarity[i])) {
              targets <- lapply(c("positive", "negative"), function(p, t) {
                if (p == "positive") {
                  t$mz <- t$mass + 1.007276
                  t$polarity <- t$polarity
                } else if (p == "negative") {
                  t$mz <- t$mass - 1.007276
                  t$polarity <- t$polarity
                }
                t
              }, t = targets)
              targets <- data.table::rbindlist(targets)
            } else {
              warning("Polarity column must contain positive, negative or both!")
            }
          }
        
        # case 2: polarity is given as named vector and requires analyses argument with same size
        } else if (!is.null(polarities) && !is.null(analyses) && length(polarities) == length(analyses)) {
          targets$mz <- 0
          targets <- lapply(analyses, function(x, t) {
            t$analysis <- x
            pol <- polarities[x]
            if (pol %in% "positive") {
              t$mz <- t$mass + 1.007276
              t$polarity <- pol
            } else if (pol %in% "negative") {
              t$mz <- t$mass - 1.007276
              t$polarity <- pol
            } else if (grepl("positive", pol) && grepl("negative", pol)) {
              t <- lapply(c("positive", "negative"), function(p, t2) {
                if (p %in% "positive") {
                  t2$mz <- t2$mass + 1.007276
                  t2$polarity <- pol
                } else if (x %in% "negative") {
                  t2$mz <- t2$mass - 1.007276
                  t2$polarity <- pol
                }
                t2
              }, t2 = t)
              t <- data.table::rbindlist(t)
            }
            t
          }, t = targets)
          targets <- data.table::rbindlist(targets)
        
        ## case 3: polarity is given but analyses in as col name
        } else if (!is.null(polarities) && "analysis" %in% colnames(mz)) {
          targets$mz <- 0
          targets$polarity <- polarities[targets$analysis]
          for (i in seq_len(nrow(targets))) {
            if (targets$polarity[i] %in% "positive") {
              targets$mz[i] <- targets$mass[i] + 1.007276
            } else if (targets$polarity[i] %in% "negative") {
              targets$mz[i] <- targets$mass[i] - 1.007276
            } else if (grepl("positive", targets$polarity[i]) && grepl("negative", targets$polarity[i])) {
              targets <- lapply(c("positive", "negative"), function(p, t) {
                t$polarity <- p
                if (p %in% "positive") {
                  t$mz <- t$mass + 1.007276
                } else if (p %in% "negative") {
                  t$mz <- t$mass - 1.007276
                }
                t
              }, t = targets)
              targets <- data.table::rbindlist(targets)
            }
          }
        
        # case 4: polarity is given with the same length as targets
        } else if (!is.null(polarities) && length(polarities) == nrow(targets)) {
          targets$mz <- 0
          targets$polarity <- polarities
          for (i in seq_len(nrow(targets))) {
            if (polarities[i] %in% "positive") {
              targets$mz[i] <- targets$mass[i] + 1.007276
            } else if (polarities[i] %in% "negative") {
              targets$mz[i] <- targets$mass[i] - 1.007276
            } else if (grepl("positive", polarities[i]) && grepl("negative", polarities[i])) {
              targets <- lapply(c("positive", "negative"), function(p, t) {
                t$polarity <- p
                if (p %in% "positive") {
                  t$mz <- t$mass + 1.007276
                } else if (p %in% "negative") {
                  t$mz <- t$mass - 1.007276
                }
                t
              }, t = targets)
              targets <- data.table::rbindlist(targets)
            }
          }
        
        # case 5: poalrity is of length two positive and negative
        } else if (!is.null(polarities) && length(polarities) == 2) {
          targets$mz <- 0
          targets <- lapply(c("positive", "negative"), function(p, t) {
            if (p %in% "positive") {
              t$mz <- t$mass + 1.007276
              t$polarity <- p
            } else if (p %in% "negative") {
              t$mz <- t$mass - 1.007276
              t$polarity <- p
            }
            t
          }, t = targets)
          targets <- data.table::rbindlist(targets)
        } else {
          warning("Polarity could not be calculated from mass!")
        }
      }
      
      if (cols_mz %in% colnames(targets) && !all(cols_mz_ranges %in% colnames(mz))) {
        targets$mzmin <- targets$mz - ((ppm / 1E6) * targets$mz)
        targets$mzmax <- targets$mz + ((ppm / 1E6) * targets$mz)
      }
      
      if (!cols_mz %in% colnames(targets)) targets$mz <- apply(mz[, cols_mz_ranges, with = FALSE], 1, mean)
      
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
      
      if (is.data.frame(mobility)) {
        
        if (nrow(mobility) == nrow(targets)) {
          
          if (cols_mobility %in% colnames(mobility) && !all(cols_mobility_ranges %in% colnames(mobility))) {
            targets$mobility <- mobility$mobility
            targets$mobilitymin <- mobility$mobility - millisec
            targets$mobilitymax <- mobility$mobility + millisec
            
          } else if (all(cols_mobility_ranges %in% colnames(mobility))) {
            targets$mobilitymin <- mobility$mobilitymin
            targets$mobilitymax <- mobility$mobilitymax
          }
          
          if (!cols_mobility %in% colnames(mobility)) targets$mobility <- apply(mobility[, cols_mobility_ranges, with = FALSE], 1, mean)
        }
        
      } else if ("mobility" %in% colnames(targets)) {
        
        if (!all(cols_mobility_ranges %in% colnames(targets))) {
          targets$mobilitymin <- targets$mobility - millisec
          targets$mobilitymax <- targets$mobility + millisec
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
      
      if (is.data.frame(mobility)) {
        
        if (nrow(mobility) == nrow(targets)) {
          
          if (cols_mobility %in% colnames(mobility) && !all(cols_mobility_ranges %in% colnames(mobility))) {
            targets$mobility <- mobility$mobility
            targets$mobilitymin <- mobility$mobility - millisec
            targets$mobilitymax <- mobility$mobility + millisec
            
          } else if (all(cols_mobility_ranges %in% colnames(mobility))) {
            targets$mobilitymin <- mobility$mobilitymin
            targets$mobilitymax <- mobility$mobilitymax
          }
          
          if (!cols_mobility %in% colnames(mobility)) targets$mobility <- apply(mobility[, cols_mobility_ranges, with = FALSE], 1, mean)
        }
        
      } else if ("mobility" %in% colnames(targets)) {
        
        if (!all(cols_mobility_ranges %in% colnames(targets))) {
          targets$mobilitymin <- targets$mobility - millisec
          targets$mobilitymax <- targets$mobility + millisec
        }
      }
      
      if ("analysis" %in% colnames(rt)) targets$analysis <- rt$analysis
      
      if ("polarity" %in% colnames(rt)) targets$polarity <- rt$polarity
      
    } else if (is.data.frame(mobility)) {
      
      targets <- mobility
      
      if (cols_mobility %in% colnames(mobility) && !all(cols_mobility_ranges %in% colnames(mobility))) {
        targets$mobilitymin <- targets$mobility - millisec
        targets$mobilitymax <- targets$mobility + millisec
      }
      
      if (!cols_mobility %in% colnames(mobility)) targets$mobility <- apply(mobility[, cols_mobility_ranges, with = FALSE], 1, mean)
      
      if ("analysis" %in% colnames(mobility)) targets$analysis <- mobility$analysis
      
      if ("polarity" %in% colnames(mobility)) targets$polarity <- mobility$polarity
    }
    
    if (!cols_mz %in% colnames(targets)) targets$mz <- 0
    if (!cols_rt %in% colnames(targets)) targets$rt <- 0
    if (!cols_mobility %in% colnames(targets)) targets$mobility <- 0
    if (!cols_mz_ranges[1] %in% colnames(targets)) targets$mzmin <- 0
    if (!cols_mz_ranges[2] %in% colnames(targets)) targets$mzmax <- 0
    if (!cols_rt_ranges[1] %in% colnames(targets)) targets$rtmin <- 0
    if (!cols_rt_ranges[2] %in% colnames(targets)) targets$rtmax <- 0
    if (!cols_mobility_ranges[1] %in% colnames(targets)) targets$mobilitymin <- 0
    if (!cols_mobility_ranges[2] %in% colnames(targets)) targets$mobilitymax <- 0
    
    targets$mz[targets$mz < 0] <- 0
    targets$mzmin[targets$mzmin < 0] <- 0
    targets$mzmax[targets$mzmax < 0] <- 0
    targets$rt[targets$rt < 0] <- 0
    targets$rtmin[targets$rtmin < 0] <- 0
    targets$rtmax[targets$rtmax < 0] <- 0
    targets$mobility[targets$mobility < 0] <- 0
    targets$mobilitymin[targets$mobilitymin < 0] <- 0
    targets$mobilitymax[targets$mobilitymax < 0] <- 0
    
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
          round(targets$mobilitymin, 0),
          "-",
          round(targets$mobilitymax, 0),
          sep = ""
        )
      }
    }
    
    if (!is.null(analyses) && !"analysis" %in% colnames(targets)) {
      if (length(analyses) > 1) {
        targets <- lapply(analyses, function(x) {
          targets$analysis <- x
          targets
        })
        targets <- data.table::rbindlist(targets)
      } else {
        targets$analysis <- analyses
      }
    }
    
    cols <- c("id", "mz", "rt", "mobility", "mzmin", "mzmax", "rtmin", "rtmax", "mobilitymin", "mobilitymax", "analysis", "polarity")
    
    targets <- targets[, cols[cols %in% colnames(targets)], with = FALSE]
    
    data.table::setcolorder(targets, c("id", "mz", "rt", "mobility", "mzmin", "mzmax", "rtmin", "rtmax", "mobilitymin", "mobilitymax"))
    
    if (any(duplicated(targets$id))) {
      
      if ("analysis" %in% colnames(targets)) {
        unique_analyses <- unique(targets$analysis)
        for (i in unique_analyses) {
          if (any(duplicated(targets$id[targets$analysis == i]))) {
            if ("polarity" %in% colnames(targets)) {
              targets$id[targets$analysis == i] <- paste0(targets$id[targets$analysis == i], " ", targets$polarity[targets$analysis == i])
              if (any(duplicated(targets$id[targets$analysis == i]))) {
                targets$id[targets$analysis == i] <- paste0(targets$id[targets$analysis == i], " ", seq_len(nrow(targets[targets$analysis == i, ])))
              }
            } else {
              targets$id[targets$analysis == i] <- paste0(targets$id[targets$analysis == i], " ", seq_len(nrow(targets[targets$analysis == i, ])))
            }
          }
        }
      } else if ("polarity" %in% colnames(targets)) {
        targets$id <- paste0(targets$id, " ", targets$polarity)
        if (any(duplicated(targets$id))) {
          targets$id <- paste0(targets$id, " ", seq_len(nrow(targets)))
        }
      } else {
        targets$id <- paste0(targets$id, " ", seq_len(nrow(targets)))
      }
    }
    
    S7::new_object(S7::S7_object(), targets = targets)
  },
    
  validator = function(self) {
    cols <- c("id", "mz", "rt", "mobility", "mzmin", "mzmax", "rtmin", "rtmax", "mobilitymin", "mobilitymax")
    valid <- all(
      checkmate::test_data_frame(self@targets),
      checkmate::test_true(all(cols %in% colnames(self@targets)))
    )
    if (!valid) return(FALSE)
    NULL
  }
)

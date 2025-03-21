#' @export
#' @noRd
MassSpecTargets <- S7::new_class(
  name = "MassSpecTargets",
  package = "StreamFind",
  properties = list(
    targets = S7::new_property(S7::class_data.frame, default = data.frame())
  ),
  
  constructor = function(
    mass = NULL,
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
    cols_mass_ranges <- c("min", "max")
    cols_mz_ranges <- c("mzmin", "mzmax")
    cols_rt_ranges = c("rtmin", "rtmax")
    cols_mobility_ranges = c("mobilitymin", "mobilitymax")
    
    targets <- NULL
    
    if (is.data.frame(mass)) {
      checkmate::assert_true(
        cols_mass %in% colnames(mass) ||
        all(cols_mass_ranges %in% colnames(mass))
      )
      targets <- mass
      
    } else if (!is.null(mass)) {
      checkmate::assert_numeric(mass)
      
      if (is.vector(mass)) {
        targets <- data.table::data.table("mass" = mass)
      } else {
        warning("mz must be a data frame or a numeric vector!")
      }
    }
    
    if (is.data.frame(mz)) {
      checkmate::assert_true(
        cols_mz %in% colnames(mz) ||
        all(cols_mz_ranges %in% colnames(mz)) ||
        cols_mass %in% colnames(mz)
      )
      targets <- mz
      
    } else if (!is.null(mz)) {
      checkmate::assert_numeric(mz)
      
      if (is.vector(mz)) {
        targets <- data.table::data.table("mz" = mz)
      } else {
        warning("mz must be a data frame or a numeric vector!")
      }
    }
    
    if (is.data.frame(rt)) {
      checkmate::assert_true(
        cols_rt %in% colnames(rt) ||
        all(cols_rt_ranges %in% colnames(mz))
      )
      
      if (is.null(targets)) {
        targets <- rt
      } else {
        if (nrow(rt) == nrow(targets)) {
          targets <- cbind(targets, rt)
        } else {
          warning("RT data frame has different number of rows than targets!")
        }
      }
      
    } else if (!is.null(rt)) {
      checkmate::assert_numeric(rt)
      
      if (is.vector(rt)) {
        if (is.null(targets)) {
          targets <- data.table::data.table("rt" = rt)
        } else {
          if (length(rt) == nrow(targets)) {
            targets$rt <- rt
          } else {
            warning("RT vector length has different length and the number of rows in targets!")
          }
        }
      } else {
        warning("RT must be a data frame or a numeric vector!")
      }
    }
    
    if (is.data.frame(mobility)) {
      checkmate::assert_true(
        cols_mobility %in% colnames(mobility) ||
        all(cols_mobility_ranges %in% colnames(mobility))
      )
      
      if (is.null(targets)) {
        targets <- mobility
      } else {
        if (nrow(mobility) == nrow(targets)) {
          targets <- cbind(targets, mobility)
        } else {
          warning("Mobility data frame has different number of rows than targets!")
        }
      }
      
    } else if (!is.null(mobility)) {
      checkmate::assert_numeric(mobility)
      
      if (is.vector(mobility)) {
        if (is.null(targets)) {
          targets <- data.table::data.table("mobility" = mobility)
        } else {
          if (length(mobility) == nrow(targets)) {
            targets$mobility <- mobility
          } else {
            warning("Mobility vector length has different length and the number of rows in targets!")
          }
        }
      } else {
        warning("Mobility must be a data frame or a numeric vector!")
      }
    }
    
    checkmate::assert_numeric(ppm, len = 1, null.ok = TRUE)
    
    checkmate::assert_numeric(sec, len = 1, null.ok = TRUE)
    
    checkmate::assert_numeric(millisec, len = 1, null.ok = TRUE)
    
    checkmate::assert_character(as.character(id), null.ok = TRUE)
    
    if (!is.null(targets)) {
      if ("name" %in% colnames(targets)) {
        targets$id <- targets$name
        targets$name <- NULL
      }
    }
    
    checkmate::assert_character(analyses, null.ok = TRUE)
    
    checkmate::assert_character(polarities, null.ok = TRUE)
    
    if (!is.null(polarities) && !is.null(analyses)) {
      checkmate::assert_true(all(names(polarities) %in% analyses))
    }
    
    if (!is.null(targets)) {
      
      if (is.null(polarities) && !"polarity" %in% colnames(targets)) {
        targets <- lapply(c("positive", "negative"), function(p, t) {
          t$polarity <- p
          t
        }, t = targets)
        targets <- data.table::rbindlist(targets)
        
      } else if (!"polarity" %in% colnames(targets)) {
        
        if (!is.null(polarities) && "analysis" %in% colnames(targets) && !is.null(names(polarities))) {
          targets$polarity <- polarities[targets$analysis]
        
        } else if (!is.null(polarities) && !is.null(analyses) && !is.null(names(polarities))) {
          targets <- lapply(analyses, function(x, t) {
            t$analysis <- x
            t$polarity <- polarities[x]
            t
          }, t = targets)
          targets <- data.table::rbindlist(targets)

        } else if (!is.null(polarities) && length(polarities) <= 2) {
          targets <- lapply(unique(polarities), function(p, t) {
            t$polarity <- p
            t
          }, t = targets)
          targets <- data.table::rbindlist(targets)
          
        } else {
          warning("Polarity could not be defined!")
          targets <- data.table::data.table()
        }
      } else {
        if ("polarity" %in% colnames(targets) && nrow(targets) > 0) {
          for (i in seq_len(nrow(targets))) {
            if (targets$polarity[i] == 1) targets$polarity[i] <- "positive"
            if (targets$polarity[i] == -1) targets$polarity[i] <- "negative"
          }
        }
      }
      
      if (!all(grepl("positive|negative", targets$polarity))) {
        warning("Polarity must be either positive, negative or both!")
        targets <- data.table::data.table()
      }
      
      for (i in seq_len(nrow(targets))) {
        if (grepl("positive", targets$polarity[i]) && grepl("negative", targets$polarity[i])) {
          if (cols_mz %in% colnames(targets) || all(cols_mz_ranges %in% colnames(targets))) {
            warning("Polarity is both positive and negative, but m/z is given! Not possible to define polarity for target.")
            targets <- targets[, -i]
          } else {
            targets$polarity[i] <- "positive"
            new_row <- targets[i, ]
            new_row$polarity <- "negative"
            targets <- rbind(targets, new_row)
          }
        }
      }
      
      if (nrow(targets) > 0) {
        
        # if m/z is not given, calculate from mass and polarity
        if (cols_mass %in% colnames(targets) && !cols_mz %in% colnames(targets) && !all(cols_mz_ranges %in% colnames(targets))) {
          pols <- rep(1, nrow(targets))
          pols[targets$polarity %in% "negative"] <- -1
          targets$mz <- targets$mass + (1.007276 * pols)
        }
        
        if (!cols_mass %in% colnames(targets) && !cols_mz %in% colnames(targets) && all(cols_mass_ranges %in% colnames(targets))) {
          pols <- rep(1, nrow(targets))
          pols[targets$polarity %in% "negative"] <- -1
          targets$mzmin <- targets$min + (1.007276 * pols)
          targets$mzmax <- targets$max + (1.007276 * pols)
        }
        
        if (cols_mz %in% colnames(targets) && !all(cols_mz_ranges %in% colnames(targets))) {
          if (is.null(ppm)) ppm <- 0
          targets$mzmin <- targets$mz - ((ppm / 1E6) * targets$mz)
          targets$mzmax <- targets$mz + ((ppm / 1E6) * targets$mz)
        }
        
        if (!cols_mz %in% colnames(targets) && all(cols_mz_ranges %in% colnames(targets))) {
          targets$mz <- apply(targets[, cols_mz_ranges, with = FALSE], 1, mean)
        }
        
        if (cols_rt %in% colnames(targets) && !all(cols_rt_ranges %in% colnames(targets))) {
          if (is.null(sec)) sec <- 0
          targets$rtmin <- targets$rt - sec
          targets$rtmax <- targets$rt + sec
        }
          
        if (!cols_rt %in% colnames(targets) && all(cols_rt_ranges %in% colnames(targets))) {
          targets$rt <- apply(targets[, cols_rt_ranges, with = FALSE], 1, mean)
        }
        
        if (cols_mobility %in% colnames(targets) && !all(cols_mobility_ranges %in% colnames(targets))) {
          if (is.null(millisec)) millisec <- 0
          targets$mobilitymin <- targets$mobility - millisec
          targets$mobilitymax <- targets$mobility + millisec
        }
        
        if (!cols_mobility %in% colnames(targets) && all(cols_mobility_ranges %in% colnames(targets))) {
          targets$mobility <- apply(targets[, cols_mobility_ranges, with = FALSE], 1, mean)
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
        
        cols <- c(
          "id", "mz", "rt", "mobility", "mzmin", "mzmax",
          "rtmin", "rtmax", "mobilitymin", "mobilitymax",
          "analysis", "polarity"
        )
        
        targets <- targets[, cols[cols %in% colnames(targets)], with = FALSE]
        
        data.table::setcolorder(targets, cols[1:10])
        
        if (any(duplicated(targets$id))) {
          
          if ("analysis" %in% colnames(targets)) {
            unique_analyses <- unique(targets$analysis)
            
            for (i in unique_analyses) {
              if ("polarity" %in% colnames(targets)) {
                unique_id_pol <- paste0(
                  targets$id[targets$analysis == i],
                  " ",
                  targets$polarity[targets$analysis == i]
                )
                
                if (any(duplicated(unique_id_pol))) {
                  targets$id[targets$analysis == i] <- paste0(
                    targets$id[targets$analysis == i],
                    " ",
                    targets$polarity[targets$analysis == i]
                  )
                }
                
                unique_id_pol <- paste0(
                  targets$id[targets$analysis == i],
                  " ",
                  targets$polarity[targets$analysis == i]
                )
                
                if (any(duplicated(unique_id_pol))) {
                  targets$id[targets$analysis == i] <- paste0(
                    targets$id[targets$analysis == i],
                    " ",
                    seq_len(nrow(targets[targets$analysis == i, ]))
                  )
                }
              } else {
                targets$id[targets$analysis == i] <- paste0(
                  targets$id[targets$analysis == i],
                  " ",
                  seq_len(nrow(targets[targets$analysis == i, ]))
                )
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
      }
    }
    
    if (is.null(targets)) {
      targets <- data.table::data.table(
        "id" = 0,
        "mz" = 0,
        "rt" = 0,
        "mobility" = 0,
        "mzmin" = 0,
        "mzmax" = 0,
        "rtmin" = 0,
        "rtmax" = 0,
        "mobilitymin" = 0,
        "mobilitymax" = 0,
        "analysis" = analyses,
        "polarity" = polarities
      )
    }
    
    S7::new_object(S7::S7_object(), targets = targets)
  },
    
  validator = function(self) {
    cols <- c(
      "id", "mz", "rt", "mobility", "mzmin", "mzmax",
      "rtmin", "rtmax", "mobilitymin", "mobilitymax",
      "analysis", "polarity"
    )
    checkmate::assert_data_frame(self@targets)
    checkmate::assert_true(all(cols %in% colnames(self@targets)))
    NULL
  }
)

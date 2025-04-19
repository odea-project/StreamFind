#' **MassSpecMethod_FilterFeaturesMS2_native**
#'
#' @description Settings for filtering (i.e., cleaning) non-relevant traces in features MS2
#' spectrum.
#' 
#' @param top An integer specifying the number of top features to be kept.
#' @param minIntensity A numeric value specifying the minimum intensity.
#' @param relMinIntensity A numeric value specifying the relative minimum intensity.
#' @param blankClean A logical value specifying if blank cleaning should be performed. When `TRUE`
#' MS2 traces present in the blank samples above the defined `blankPresenceThreshold` and above the
#' `globalPresenceThreshold` are excluded.
#' @param mzClust A numeric value specifying the m/z clustering threshold.
#' @param blankPresenceThreshold A numeric value specifying the blank presence threshold.
#' @param globalPresenceThreshold A numeric value specifying the global presence threshold.
#'
#' @return A `MassSpecMethod_FilterFeaturesMS2_native` object.
#'
#' @export
#'
MassSpecMethod_FilterFeaturesMS2_native <- S7::new_class(
  name = "MassSpecMethod_FilterFeaturesMS2_native",
  parent = ProcessingStep,
  package = "StreamFind",
  constructor = function(top = NULL,
                         minIntensity = NULL,
                         relMinIntensity = NULL,
                         blankClean = FALSE,
                         mzClust = 0.005,
                         blankPresenceThreshold = 0.8,
                         globalPresenceThreshold = 0.1) {
    S7::new_object(
      ProcessingStep(
        data_type = "MassSpec",
        method = "FilterFeaturesMS2",
        required = "LoadFeaturesMS2",
        algorithm = "native",
        parameters = list(
          top = as.integer(top),
          minIntensity = as.numeric(minIntensity),
          relMinIntensity = as.numeric(relMinIntensity),
          blankClean = as.logical(blankClean),
          mzClust = as.numeric(mzClust),
          blankPresenceThreshold = as.numeric(blankPresenceThreshold),
          globalPresenceThreshold = as.numeric(globalPresenceThreshold)
        ),
        number_permitted = Inf,
        version = as.character(packageVersion("StreamFind")),
        software = "StreamFind",
        developer = "Ricardo Cunha",
        contact = "cunha@iuta.de",
        link = "https://odea-project.github.io/StreamFind",
        doi = NA_character_
      )
    )
  },
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "MassSpec")
    checkmate::assert_choice(self@method, "FilterFeaturesMS2")
    checkmate::assert_choice(self@algorithm, "native")
    checkmate::assert_integer(self@parameters$top)
    checkmate::assert_numeric(self@parameters$minIntensity)
    checkmate::assert_numeric(self@parameters$relMinIntensity)
    checkmate::assert_logical(self@parameters$blankClean)
    checkmate::assert_numeric(self@parameters$mzClust)
    checkmate::assert_numeric(self@parameters$blankPresenceThreshold)
    checkmate::assert_numeric(self@parameters$globalPresenceThreshold)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_FilterFeaturesMS2_native) <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_results_nts()) {
    warning("No NTS object available! Not done.")
    return(FALSE)
  }
  
  if (!engine$NTS$has_features) {
    warning("There are no features! Run FindFeatures first!")
    return(FALSE)
  }
  
  if (!engine$NTS$has_features_ms2) {
    warning("There are no features MS2! Run LoadFeaturesMS2 first!")
    return(FALSE)
  }
  
  feature_list <- engine$NTS$feature_list
  
  parameters <- x@parameters  
  
  if (parameters$blankClean) {
    blk_ms2 <- data.table::data.table()
    blk_analyses <- engine$NTS$blanks
    blk_rpls <- engine$NTS$replicates
    blk_rpls <- blk_rpls[blk_rpls %in% blk_analyses]
    blk_analyses <- names(blk_analyses)[engine$NTS$replicates %in% blk_rpls] 
    
    if (length(blk_analyses) == 0) {
      message("No blank analyses available to clean blank MS2 traces! Not done.")
    } else {
      blk_fts <- feature_list[blk_analyses]
      
      # TODO add process for multiple polarities
      blk_polarities <- engine$Analyses$spectra_polarity[blk_analyses]
      if (length(unique(blk_polarities)) > 1) {
        warning("There are blanks with different polarities! Not done.")
        return(FALSE)
      }
      
      blk_fts <- lapply(blk_fts, function(z) {
        ms2 <- z$ms2
        names(ms2) <- z$feature
        ms2 <- data.table::rbindlist(ms2, idcol = "feature")
        ms2
      })
      
      blk_fts <- data.table::rbindlist(blk_fts, idcol = "analysis")
      
      if (nrow(blk_fts) == 0) {
        message("No blank MS2 traces available to clean! Using global background cleaning only.")
      } else {
        
        blk_fts$uid <- paste0(blk_fts$analysis, "_", blk_fts$feature)
        rt_seq <- seq_along(unique(blk_fts$uid))
        names(rt_seq) <- unique(blk_fts$uid)
        blk_fts$rt <- rt_seq[blk_fts$uid]
        blk_fts$analysis <- "blank"
        blk_fts$id <- "blank"
        blk_fts$unique_id <- "blank"
        
        message("Averaging blank MS2 traces...", appendLF = FALSE)
        
        blk_ms2 <- rcpp_ms_cluster_spectra(
          blk_fts,
          mzClust = parameters$mzClust,
          presence = parameters$blankPresenceThreshold
        )[[1]]
        
        message("Done! Retrieved ", nrow(blk_ms2), " MS2 traces.")
      }
    }
    
    all_fts <- lapply(feature_list, function(z) {
      ms2 <- z$ms2
      names(ms2) <- z$feature
      ms2 <- data.table::rbindlist(ms2, idcol = "feature")
      ms2
    })
    
    all_fts <- data.table::rbindlist(all_fts, idcol = "analysis")
    
    if (nrow(all_fts) == 0) {
      message("No global MS2 traces that match criteria available to clean! Not done.")
    } else {
      all_fts$uid <- paste0(all_fts$analysis, "_", all_fts$feature)
      rt_seq <- seq_along(unique(all_fts$uid))
      names(rt_seq) <- unique(all_fts$uid)
      all_fts$rt <- rt_seq[all_fts$uid]
      all_fts$analysis <- "all"
      all_fts$id <- "all"
      all_fts$unique_id <- "all"
      
      message("Averaging global MS2 traces...", appendLF = FALSE)
      
      all_ms2 <- rcpp_ms_cluster_spectra(
        all_fts,
        mzClust = parameters$mzClust,
        presence = parameters$globalPresenceThreshold
      )[[1]]
      
      message("Done! Retrieved ", nrow(all_ms2), " MS2 traces.")
      
      if (nrow(all_ms2) > 0 && nrow(blk_ms2) > 0) {
        sel_also_blk <- vapply(blk_ms2$mz, function(z) {
          any(all_ms2$mz >= z - parameters$mzClust & all_ms2$mz <= z + parameters$mzClust)
        }, logical(1))
        
        blk_ms2 <- blk_ms2[sel_also_blk, ]
      } else if (nrow(all_ms2) > 0) {
        blk_ms2 <- all_ms2
      } else {
        blk_ms2 <- data.table::data.table()
      }
    }
      
    if (nrow(blk_ms2) > 0) {
      message("Cleaning ", nrow(blk_ms2) ," background MS2 traces...", appendLF = FALSE)
      
      feature_list <- lapply(feature_list, function(z, blk_ms2, parameters) {
        ms2 <- z$ms2
        ms2 <- lapply(ms2, function(y) {
          if (length(y) > 0) {
            if (nrow(y) > 0) {
              sel_also_blk <- vapply(y$mz, function(z) {
                any(blk_ms2$mz >= z - parameters$mzClust & blk_ms2$mz <= z + parameters$mzClust)
              }, logical(1))
              y <- y[!sel_also_blk, ]
            }
          }
          y
        })
        z$ms2 <- ms2
        z
      }, blk_ms2 = blk_ms2, parameters = parameters)
      
      message("Done!")
    } else {
      message("No MS2 traces that match criteria available to clean! Not done.")
    }
  }
  
  feature_list <- lapply(feature_list, function(z, parameters) {
    ms2 <- z$ms2
    ms2 <- lapply(ms2, function(y, parameters) {
      
      if (length(y) > 0) {
        if (nrow(y) > 0) {
          
          if (length(parameters$top) > 0) {
            y <- y[order(y$intensity, decreasing = TRUE), ]
            y <- y[seq_len(min(parameters$top, nrow(y))), ]
            y <- y[order(y$mz), ]
          }
          
          if (length(parameters$minIntensity) > 0) {
            y <- y[y$intensity >= parameters$minIntensity, ]
          }
          
          if (length(parameters$relMinIntensity) > 0) {
            threshold <- max(y$intensity) * parameters$relMinIntensity
            y <- y[y$intensity >= threshold, ]
          }
        }
      }
      
      y
    }, parameters = parameters)
    z$ms2 <- ms2
    z
  }, parameters = parameters)
  
  engine$NTS$feature_list <- feature_list
  
  message(paste0("\U2713 Features MS2 filtered!"))
  
  TRUE
}
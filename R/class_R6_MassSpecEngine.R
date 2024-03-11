#' **MassSpecEngine** R6 class and methods
#'
#' @description The MassSpecEngine R6 class is a framework for parsing, processing, inspecting and storing 
#' mass spectrometry (MS) data. The MassSpecEngine is using \href{https://github.com/rickhelmus/patRoon}{patRoon} for 
#' assembly of Non-Target Screening data processing workflows.
#'
#' @template arg-ms-files
#' @template arg-headers
#' @template arg-results
#' @template arg-settings
#' @template arg-runParallel
#' @template arg-analyses
#' @template arg-chromatograms
#' @template arg-ms-levels
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-drift
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-allTraces
#' @template arg-ms-isolationWindow
#' @template arg-ms-minIntensityMS1
#' @template arg-ms-minIntensityMS2
#' @template arg-ms-index
#' @template arg-ms-minIntensity
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-verbose
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-filtered
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-ms-loaded
#' @template arg-ms-rtWindow
#' @template arg-ms-mzWindow
#' @template arg-ms-loadedMS1
#' @template arg-ms-loadedMS2
#' @template arg-ms-groups
#' @template arg-ms-intensities
#' @template arg-ms-average
#' @template arg-ms-groupBy
#' @template arg-ms-mzClustFeatures
#' @template arg-ms-presenceFeatures
#' @template arg-ms-minIntensityFeatures
#' @template arg-ms-loadedFeaturesMS1
#' @template arg-ms-mzClustGroups
#' @template arg-ms-presenceGroups
#' @template arg-ms-minIntensityGroups
#' @template arg-ms-loadedGroupsMS1
#' @template arg-ms-loadedFeaturesMS2
#' @template arg-ms-loadedGroupsMS2
#' @template arg-ms-settingsFeatures
#' @template arg-ms-legendNames
#' @template arg-ms-colorBy
#' @template arg-title
#' @template arg-ms-labs
#' @template arg-ms-interactive
#' @template arg-ms-save-format
#' @template arg-ms-save-name
#' @template arg-ms-save-path
#' @template arg-import-file
#' @template arg-ms-xlim-ylim
#' @template arg-ms-cex
#' @template arg-ms-showLegend
#' @template arg-ms-clusters
#' @template arg-ms-onGroups
#' @template arg-ms-addSuspects
#' @template arg-ms-ppmMS2
#' @template arg-ms-minFragments
#'
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' \insertRef{pugixml01}{StreamFind}
#'
#' @export
#'
MassSpecEngine <- R6::R6Class("MassSpecEngine",
                              
  inherit = CoreEngine,

  # _ private fields -----
  private = list(
    
    ## ___ .utils -----
    
    # Removes patRoon data from the results private field.
    #
    .remove_patRoon_data = function() {
      if (self$has_results("patRoon")) {
        warning("patRoon data removed! Redo the data processing workflow to regenarate the data.")
        private$.results[["patRoon"]] <- NULL
      }
    },
    
    # Adds an extra column to features.
    #
    .add_features_column = function(name = NULL, data = NULL) {
      
      if (any(self$has_features())) {
        
        all_features <- self$feature_list
        
        feature_list <- self$features@features
        
        feature_list <- Map(function(x, y, z) {
          y_to_add <- y[!z$filtered]
          if (nrow(x) == length(y_to_add)) x[[name]] <- y_to_add
          x
        }, feature_list, data, all_features)
        
        self$feature_list <- feature_list
        
        filtered_feature_list <- self$filtered_features
        
        filtered_feature_list <- Map(function(x, y, z) {
          y_to_add <- y[z$filtered]
          if (length(y_to_add) > 0) if (nrow(x) == length(y_to_add)) x[[name]] <- y_to_add
          x
        }, filtered_feature_list, data, all_features)
        
        self$filtered_features <- filtered_feature_list
        
        TRUE
        
      } else {
        FALSE
      }
    }
  ),
  
  # _ active bindings -----
  
  active = list(
    
    #' @field analysisInfo The analysisInfo data.frame with the list of analyses.
    #'
    analysisInfo = function() {
      
      if (self$has_results("patRoon")) {
        private$.results$patRoon$data@analysisInfo
        
      } else if (self$get_number_analyses() > 0) {
        
        anaInfo <- self$get_overview()
        
        anaInfo <- data.frame(
          "path" = dirname(anaInfo$file),
          "analysis" = anaInfo$analysis,
          "group" = anaInfo$replicate,
          "blank" = anaInfo$blank
        )
        
        anaInfo$blank[is.na(anaInfo$blank)] <- ""
        
        anaInfo
        
      } else {
        data.frame()
      }
    },
    
    #' @field features The features S4 class object from patRoon.
    #' 
    features = function(value) {
      
      if (missing(value)) {
        
        if (self$has_results("patRoon")) {
          pat <- private$.results$patRoon$data
          
          if ("featureGroups" %in% is(pat)) pat <- pat@features
          
          pat
          
        } else {
          NULL
        }
        
      } else {
        
        if ("features" %in% is(value)) {
          
          if (self$has_results("patRoon")) {
            
            if (identical(value@analysisInfo, self$analysisInfo)) {
              
              if ("features" %in% is(private$.results$patRoon$data)) {
                
                if (identical(unname(self$get_analysis_names()), names(value@features))) {
                  
                  private$.results$patRoon$data <- value
                  
                } else {
                  warning("Feature list names not matching analysis names! Not done.")
                }
                
              } else if ("featureGroups" %in% is(private$.results$patRoon$data)) {
                
                if (identical(unname(self$get_analysis_names()), names(value@features))) {
                  
                  if (all(vapply(value@features, function(x) "group" %in% colnames(x), FALSE))) {
                    
                    private$.results$patRoon$data@features <- value[unname(self$get_analysis_names())]
                    
                    fg <- self$featureGroups[unname(self$get_analysis_names())]
                    
                    fg_left <- unique(unlist(lapply(value@features, function(x) x$group)))
                    
                    fg_left <- fg_left[!is.na(fg_left)]
                    
                    fg <- fg[, fg_left]
                    
                    private$.results$patRoon$data <- fg
                    
                    if (self$has_MSPeakLists()) {
                      self$MSPeakLists <- self$MSPeakLists[patRoon::analyses(fg), patRoon::groupNames(fg)]
                    }
                    
                    if (self$has_formulas()) {
                      self$formulas <- self$formulas[patRoon::analyses(fg), patRoon::groupNames(fg)]
                    }
                    
                    if (self$has_compounds()) {
                      self$compounds <- self$compounds[patRoon::groupNames(fg)]
                    }
                    
                  } else {
                    warning("Feature groups not present in features! Not done.")
                  }
                  
                } else {
                  warning("Feature list names not matching analysis names! Not done.")
                }
                
              } else {
                warning("Features not found! Not done.")
              } 
              
            } else {
              warning("Analyses not the same in features! Not done.")
            }
            
          } else {
            
            if (all(patRoon::analyses(value) %in% self$get_analysis_names())) {
              
              pols <- self$get_polarities()
              
              if (length(unique(pols)) > 1 && !("featuresSet" %in% is(value))) {
                value <- patRoon::makeSet(
                  value[pols %in% "positive"],
                  value[pols %in% "negative"],
                  adducts = list("[M+H]+", "[M-H]-")
                )
              }
              
              filtered <- lapply(patRoon::analyses(value), function(x) value@features[[x]][0, ])
              names(filtered) <- patRoon::analyses(value)
              
              self$add_results(
                list("patRoon" = list(
                  "data" = value,
                  "filtered" = filtered,
                  "mspl" = NULL,
                  "formulas" = NULL,
                  "compounds" = NULL,
                  "software" = "patRoon",
                  "version" = as.character(packageVersion("patRoon"))
                ))
              )
              
            } else {
              
              # TODO add features from scratch loading analyses files, mostly when the engine is empty
              
            } 
          }
          
        } else {
          warning("Features must be an S4 class object! Not done.")
        }
        
        invisible(self)
      }
    },
    
    #' @field filtered_features The filtered features as list of data.table objects for each analysis.
    #' 
    filtered_features = function(value) {
      
      if (missing(value)) {
        
        if (self$has_results("patRoon")) {
          private$.results$patRoon$filtered
          
        } else {
          NULL
        }
        
      } else {
        
        if (self$has_results("patRoon")) {
          
          value <- lapply(value, function(x) {
            setnames(x, "feature", "ID", skip_absent = TRUE)
            setnames(x, "rt", "ret", skip_absent = TRUE)
            setnames(x, "rtmin", "retmin", skip_absent = TRUE)
            setnames(x, "rtmax", "retmax", skip_absent = TRUE)
          })
          
          if ("filtered" %in% names(private$.results$patRoon)) {
            
            if (identical(unname(self$get_analysis_names()), names(value))) {
              
              filtered_feature_list <- self$filtered_features[unname(self$get_analysis_names())]
              
              value <- Map(
                function(x, y) {
                  if (nrow(y) > 0 && nrow(x) > 0) x <- x[!(x$ID %in% y$ID), ]
                  y <- rbindlist(list(y, x), fill = TRUE)
                  y
                },
                value, filtered_feature_list
              )
              
              private$.results$patRoon$filtered <- value
              
            } else {
              warning("Filtered feature list names not matching analysis names! Not done.")
            }
            
          } else {
            warning("Filtered features not found! Not done.")
          }
          
        } else {
          warning("Filetered features not found! Not done.")
        }
        
        invisible(self)
      }
    },
    
    #' @field feature_list List of features data.table objects for each analysis.
    #'
    feature_list = function(value) {
      
      if (missing(value)) {
        
        if (ms$has_results("patRoon")) {
          
          pat <- private$.results$patRoon$data
          
          if (self$has_groups()) pat <- pat@features
          
          f_list <- pat@features
          
          filtered_list <- private$.results$patRoon$filtered
          
          f_list <- Map(
            function(x, y) {
              
              if (nrow(x) > 0) x <- x[!(x$ID %in% y$ID), ]
              
              y <- rbindlist(list(y, x), fill = TRUE)
              
              y
            },
            filtered_list, f_list
          )

          if ("featuresSet" %in% is(pat)) {
            
            for (x in names(f_list)) {
              
              pol <- self$get_polarities(x)
              
              if ("positive" %in% pol) adduct_val <- -1.007276
              if ("negative" %in% pol) adduct_val <- 1.007276
              
              sel_to_change <- round(f_list[[x]]$mz, 0) == round(f_list[[x]]$mass, 0)
              
              f_list[[x]]$mz[sel_to_change] <- f_list[[x]]$mz - adduct_val
              f_list[[x]]$mzmin[sel_to_change] <- f_list[[x]]$mzmin - adduct_val
              f_list[[x]]$mzmax[sel_to_change] <- f_list[[x]]$mzmax - adduct_val
            }
          }
          
          f_list <- lapply(f_list, function(x) {
            z <- copy(x)
            setnames(z, "ID", "feature")
            setnames(z, "ret", "rt")
            setnames(z, "retmin", "rtmin")
            setnames(z, "retmax", "rtmax")
          })
          
          f_list
          
        } else {
          NULL
        }
        
      } else {
        
        if (self$has_results("patRoon")) {
          
          value <- lapply(value, function(x) {
            setnames(x, "feature", "ID", skip_absent = TRUE)
            setnames(x, "rt", "ret", skip_absent = TRUE)
            setnames(x, "rtmin", "retmin", skip_absent = TRUE)
            setnames(x, "rtmax", "retmax", skip_absent = TRUE)
          })
          
          if ("features" %in% is(private$.results$patRoon$data)) {
            
            if (identical(names(private$.results$patRoon$data@features), names(value))) {
              private$.results$patRoon$data@features <- value

            } else {
              warning("Feature list names not matching analysis names! Not done.")
            }
            
          } else if ("featureGroups" %in% is(private$.results$patRoon$data)) {
            
            if (identical(names(private$.results$patRoon$data@features@features), names(value))) {
              
              if (all(vapply(value, function(x) "group" %in% colnames(x), FALSE))) {
                
                private$.results$patRoon$data@features@features <- value
                
                fg <- self$featureGroups
                
                fg_left <- unique(unlist(lapply(value, function(x)x$group[!x$filtered])))
                
                fg_left <- fg_left[!is.na(fg_left)]
                
                if (length(fg_left) > 0) {
                  fg <- fg[, fg_left]
                  private$.results$patRoon$data <- fg
                }
                
              } else {
                warning("Feature groups not present in features! Not done.")
              }

            } else {
              warning("Feature list names not matching analysis names! Not done.")
            }
            
          } else {
            warning("Features not found! Not done.")
          }
          
        } else {
          warning("Features not found! Not done.")
        }
        
        invisible(self)
      }
    },
    
    #' @field featureGroups The featureGroups S4 class object from patRoon.
    #' 
    featureGroups = function(value) {
      
      if (missing(value)) {
        
        if (self$has_results("patRoon")) {
          pat <- private$.results$patRoon$data
          
          if ("featureGroups" %in% is(pat)) return(pat)
          
          NULL
          
        } else {
          NULL
        }
        
      } else {
        
        if (self$has_results("patRoon")) {
          
          if ("featureGroups" %in% is(value)) {
            
            if (identical(patRoon::analyses(value), unname(self$get_analysis_names()))) {
              private$.results$patRoon$data <- value
              
            } else {
              warning("Feature list names not matching analysis names! Not done.")
            }

          } else {
            warning("Value must be a featureGroups S4 class object! Not done.")
          }
          
        } else if (identical(patRoon::analyses(value), unname(self$get_analysis_names()))) {
          
          filtered <- lapply(patRoon::analyses(value), function(x) value@features@features[[x]][0, ])
          names(filtered) <- patRoon::analyses(value)
          
          self$add_results(
            list("patRoon" = list(
              "data" = value,
              "filtered" = filtered,
              "mspl" = NULL,
              "formulas" = NULL,
              "compounds" = NULL,
              "software" = "patRoon",
              "version" = as.character(packageVersion("patRoon"))
            ))
          )
          
        } else {
          # TODO add features from scratch loading analyses, as features
        }

        invisible(self)
      }
    },
    
    #' @field MSPeakLists The MSPeakLists S4 class object from patRoon.
    #' 
    MSPeakLists = function(value) {
      
      if (missing(value)) {
        
        if (self$has_results("patRoon")) {
          mspl <- private$.results$patRoon$mspl
          
          if ("MSPeakLists" %in% is(mspl)) return(mspl)
          
          NULL
          
        } else {
          NULL
        }
        
      } else {
        
        if ("MSPeakLists" %in% is(value)) {
          
          if (all(value@origFGNames %in% patRoon::groupNames(self$featureGroups))) {
            
            private$.results$patRoon$mspl <- value
            
          } else {
            warning("Feature groups in MSPeakLists do not match groups in the engine! Not done.")
          }
        } else {
          warning("Value must be an MSPeakLists S4 class object! Not done.")
        }
        
        invisible(self)
      }
    },
    
    #' @field formulas The formulas S4 class object from patRoon.
    #' 
    formulas = function(value) {
      
      if (missing(value)) {
        
        if (self$has_results("patRoon")) {
          formulas <- private$.results$patRoon$formulas
          
          if ("formulas" %in% is(formulas)) return(formulas)
          
          NULL
          
        } else {
          NULL
        }
        
      } else {
        
        if ("formulas" %in% is(value)) {
          
          if (all(value@origFGNames %in% patRoon::groupNames(self$featureGroups))) {
            
            private$.results$patRoon$formulas <- value
            
          } else {
            warning("Feature groups in formulas do not match groups in the engine! Not done.")
          }
        } else {
          warning("Value must be a formulas S4 class object! Not done.")
        }
        
        invisible(self)
      }
    },
    
    #' @field compounds The compounds S4 class object from patRoon.
    #' 
    compounds = function(value) {
      
      if (missing(value)) {
        
        if (self$has_results("patRoon")) {
          res <- private$.results$patRoon$compounds
          
          if ("compounds" %in% is(res)) return(res)
          
          NULL
          
        } else {
          NULL
        }
        
      } else {
        
        if ("compounds" %in% is(value)) {
          
          if (all(value@origFGNames %in% patRoon::groupNames(self$featureGroups))) {
            
            private$.results$patRoon$compounds <- value
            
          } else {
            warning("Feature groups in compounds do not match groups in the engine! Not done.")
          }
          
        } else {
          warning("Value must be a compounds S4 class object! Not done.")
        }
        
        invisible(self)
      }
    },
    
    #' @field spectra_charges `data.table` with charges assigned to spectra for each analyses.
    #' 
    spectra_charges = function() {
      
      if (self$has_spectra_charges()) {
        res <- private$.results$spectra$data
        res <- lapply(res, function(x) x$charges)
        
        res
        
      } else {
        data.table()
      }
    }
  ),

  # _ public fields/methods -----
  
  public = list(
    
    ## ___ create -----

    #' @description Creates an R6 MassSpecEngine class object.
    #'
    #' @param settings A named list of ProcessingSettings objects or a single ProcessingSettings object. The list 
    #' names should match the call name of each ProcessingSettings object. Alternatively, a named list with call name, 
    #' algorithm and parameters to be transformed and added as ProcessingSettings object.
    #'
    #' @param analyses A MassSpecAnalysis S3 class object or a list with MassSpecAnalysis S3 class objects as 
    #' elements (see `?MassSpecAnalysis` for more information).
    #'
    #' @return A new MassSpecEngine class object.
    #'
    initialize = function(files = NULL, headers = NULL, settings = NULL, analyses = NULL, results = NULL, runParallel = FALSE) {

      if (is.null(analyses) & !is.null(files)) {
        analyses <- parse_MassSpecAnalysis(files, runParallel)
        if (is.null(analyses)) {
          warning("No valid files were given! MassSpecEngine object is empty. \n")
        }
      }
      
      super$initialize(headers, settings, analyses, results)

      private$.register(
        "created",
        "MassSpecEngine",
        headers$name,
        "StreamFind",
        as.character(packageVersion("StreamFind")),
        paste(c(headers$author, headers$path), collapse = ", ")
      )
    },

    ## ___ get -----

    #' @description Gets an overview data.frame of all the analyses.
    #'
    get_overview = function() {
      
      if (length(private$.analyses) > 0) {
        
        ov <- super$get_overview()
        
        ov$type <- vapply(private$.analyses, function(x) x$type, "")
        
        ov$polarity <- vapply(private$.analyses, function(x) paste(x$polarity, collapse = "; "), "")
        
        ov$spectra <- vapply(private$.analyses, function(x) x$spectra_number, 0)
        
        if (self$has_features()) {
          ov$features <- vapply(self$features@features, function(x) nrow(x[!x$filtered, ]), 0)
          
          # TODO MS get_overview add more details, such s suspects, isotopes, etc.
        }
        
        ov$file <- vapply(private$.analyses, function(x) x$file, NA_character_)

        row.names(ov) <- seq_len(nrow(ov))
        
        ov
        
      } else {
        data.frame()
      }
    },

    #' @description Gets the time stamp of the each analysis.
    #'
    #' @return A character vector.
    #'
    get_time_stamps = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "time_stamp")
    },

    #' @description Gets the number of spectra in each analysis.
    #'
    #' @return A character vector.
    #'
    get_spectra_number = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "spectra_number")
    },

    #' @description Gets the spectra mode of each analysis (i.e., profile or centroided).
    #'
    #' @return A character vector.
    #'
    get_spectra_mode = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "spectra_mode")
    },

    #' @description Gets the spectra levels of each analysis.
    #'
    #' @return A list for each analysis with an integer vector.
    #'
    get_spectra_levels = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "spectra_levels")
    },

    #' @description Gets the lower \emph{m/z} value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_mz_low = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "mz_low")
    },

    #' @description Gets the higher \emph{m/z} value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_mz_high = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "mz_high")
    },

    #' @description Gets the start retention time value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_rt_start = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "rt_start")
    },

    #' @description Gets the end retention time value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_rt_end = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "rt_end")
    },

    #' @description Gets the polarity of each analysis.
    #'
    #' @return A character vector.
    #'
    get_polarities = function(analyses = NULL) {

      polarities <- private$.get_analyses_entry(analyses, "polarity")

      analyses <- unique(names(polarities))

      if (length(polarities) > length(analyses)) {
        message("\U2139 Multiple polarities detected in each analysis! Some find_features algorithms cannot handled multiple polarities properly.")
      }

      # if (length(polarities) > length(analyses)) {
      #
      #   polarities <- vapply(private$.analyses[analyses], function(x) {
      #     run <- x$run
      #     polarity <- run$polarity
      #
      #     scans_pos <- length(polarity[polarity == 1])
      #     scans_neg <- length(polarity[polarity == -1])
      #
      #     ratio <- scans_pos/scans_neg
      #
      #     if (ratio < 1.2 & ratio > 0.8) {
      #       warning("Multiple polarities detected! Currently, find_features algorithms cannot handled multiple polarities properly.", )
      #       return(NA_character_)
      #
      #     } else if (ratio > 1.2) {
      #       per_pos_pol <- round((scans_pos / nrow(run)) * 100, digits = 0)
      #       warning("Multiple polarities detected but positive polarity is present in ", per_pos_pol, "% of the spectra! Advisable to remove data from negative ionization." )
      #       return("positive")
      #
      #     } else {
      #       per_neg_pol <- round((scans_neg / nrow(run)) * 100, digits = 0)
      #       warning("Multiple polarities detected but negative polarity is present in ", per_neg_pol, "% of the spectra! Advisable to remove data from positive ionization." )
      #       return("negative")
      #     }
      #   }, "")
      #
      #   names(polarities) <- analyses
      # }
      polarities
    },

    #' @description Gets the number of chromatograms in each analysis.
    #'
    #' @return A character vector.
    #'
    get_chromatograms_number = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "chromatograms_number")
    },

    #' @description Gets the instrument information of each analysis.
    #'
    #' @return A data.table.
    #'
    get_instrument_info = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())
      value <- lapply(private$.analyses[analyses], function(x) {
        x$instrument
      })
      value <- rbindlist(value, idcol = "analysis", fill = TRUE)
      value
    },

    #' @description Gets the software information of each analysis.
    #'
    #' @return A data.table.
    #'
    get_software_info = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())
      value <- lapply(private$.analyses[analyses], function(x) {
        x$software
      })
      value <- rbindlist(value, idcol = "analysis", fill = TRUE)
      value
    },

    #' @description Gets the run summary data.table of each analysis.
    #'
    #' @return A data.table.
    #'
    get_run = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())
      value <- lapply(private$.analyses[analyses], function(x) {
        x$run
      })
      value <- rbindlist(value, idcol = "analysis", fill = TRUE)
      value
    },

    #' @description Gets the total ion chromatogram (TIC) of each analysis.
    #'
    #' @return A data.table with the TIC chromatogram.
    #'
    get_tic = function(analyses = NULL, levels = c(1, 2)) {

      analyses <- private$.check_analyses_argument(analyses)

      if (is.null(analyses)) return(data.table())

      tic <- lapply(private$.analyses[analyses], function(x) {
        data.table(
          "polarity" = x$run$polarity,
          "level" = x$run$level,
          "rt" = x$run$rt,
          "intensity" = x$run$tic_intensity
        )
      })

      tic <- rbindlist(tic, idcol = "analysis", fill = TRUE)

      tic <- tic[tic$level %in% levels, ]

      tic
    },

    #' @description Gets the base peak chromatogram (BPC) of each analysis.
    #'
    #' @return A character vector.
    #'
    get_bpc = function(analyses = NULL, levels = c(1, 2)) {
      
      analyses <- private$.check_analyses_argument(analyses)

      if (is.null(analyses)) return(data.table())

      bpc <- lapply(private$.analyses[analyses], function(x) {
        data.table(
          "polarity" = x$run$polarity,
          "level" = x$run$level,
          "rt" = x$run$rt,
          "mz" = x$run$bpc_mz,
          "intensity" = x$run$bpc_intensity
        )
      })

      bpc <- rbindlist(bpc, idcol = "analysis", fill = TRUE)

      bpc <- bpc[bpc$level %in% levels, ]

      bpc
    },

    #' @description Gets metadata from each analysis.
    #'
    #' @return A data.table.
    #'
    get_metadata = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())
      metadata <- lapply(private$.analyses[analyses], function(x) {
        as.data.table(x$metadata)
      })
      metadata <- rbindlist(metadata, idcol = "analysis", fill = TRUE)

      metadata
    },

    #' @description Gets spectra from each analysis.
    #'
    #' @return A data.table with spectra for each analyses and targets, when defined.
    #'
    get_spectra = function(analyses = NULL,
                           levels = NULL,
                           mass = NULL,
                           mz = NULL,
                           rt = NULL,
                           drift = NULL,
                           ppm = 20,
                           sec = 60,
                           millisec = 5,
                           id = NULL,
                           allTraces = TRUE,
                           isolationWindow = 1.3,
                           minIntensityMS1 = 0,
                           minIntensityMS2 = 0,
                           runParallel = FALSE) {

      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(data.table())

      if (!any(is.numeric(minIntensityMS1) | is.integer(minIntensityMS1))) {
        minIntensityMS1 <- 0
      }

      if (!any(is.numeric(minIntensityMS2) | is.integer(minIntensityMS2))) {
        minIntensityMS2 <- 0
      }

      polarities <- unique(self$get_polarities(analyses))
      
      if (!is.null(mass)) {

        if (is.data.frame(mass)) {
          if ("mass" %in% colnames(mass)) {
            mass$mz <- mass$mass
          }
        }

        neutral_targets <- make_ms_targets(mass, rt, drift, ppm, sec, millisec, id)

        targets <- list()

        for (i in polarities) {

          if (i %in% "positive") {
            temp_tar <- copy(neutral_targets)
            temp_tar$mz <- temp_tar$mz + 1.00726
            temp_tar$mzmax <- temp_tar$mzmax + 1.00726
            temp_tar$mzmin <- temp_tar$mzmin + 1.00726
            temp_tar$polarity <- 1
            targets[[length(targets) + 1]] <- temp_tar

          } else if (i %in% "negative") {
            temp_tar <- copy(neutral_targets)
            temp_tar$mz <- temp_tar$mz - 1.00726
            temp_tar$mzmax <- temp_tar$mzmax - 1.00726
            temp_tar$mzmin <- temp_tar$mzmin - 1.00726
            temp_tar$polarity <- -1
            targets[[length(targets) + 1]] <- temp_tar
          }
        }

        if (length(targets) > 0) {
          targets <- rbindlist(targets, fill = TRUE)

        } else {
          warning("Targets could not be made from mass as polarities are not defined!")
          return(data.table())
        }

      } else {
        
        mz_targets <- make_ms_targets(mz, rt, drift, ppm, sec, millisec, id)

        if (!"polarity" %in% colnames(mz_targets)) {
          targets <- list()

          for (i in polarities) {

            if (i %in% "positive") {
              temp_tar <- copy(mz_targets)
              temp_tar$polarity <- 1
              targets[[length(targets) + 1]] <- temp_tar

            } else if (i %in% "negative") {
              temp_tar <- copy(mz_targets)
              temp_tar$polarity <- -1
              targets[[length(targets) + 1]] <- temp_tar
            }
          }

          targets <- rbindlist(targets, fill = TRUE)

        } else {
          targets <- mz_targets
        }
      }

      num_cols <- c("mz", "rt", "drift", "mzmin", "mzmax", "rtmin", "rtmax", "driftmin", "driftmax")

      if (all(apply(targets[, num_cols, with = FALSE], 1, function(x) sum(x, na.rm = TRUE)) != 0)) {
        
        if (TRUE %in% is.na(targets$mz)) {
          targets$mz[is.na(targets$mz)] <- 0
        }
        
        if (TRUE %in% is.na(targets$mzmax)) {
          targets$mzmax[is.na(targets$mzmax)] <- max(self$get_mz_high(analyses))
        }
        
        if (TRUE %in% is.na(targets$mzmin)) {
          targets$mzmin[is.na(targets$mzmin)] <- min(self$get_mz_low(analyses))
        }
        
        if (TRUE %in% (targets$mzmax == 0)) {
          targets$mzmax[targets$mzmax == 0] <- max(self$get_mz_high(analyses))
        }
        
        if (TRUE %in% is.na(targets$rt)) {
          targets$rt[is.na(targets$rt)] <- 0
        }
        
        if (TRUE %in% is.na(targets$rtmax)) {
          targets$rtmax[is.na(targets$rtmax)] <- max(self$get_rt_end(analyses))
        }
        
        if (TRUE %in% is.na(targets$rtmin)) {
          targets$rtmin[is.na(targets$rtmin)] <- min(self$get_rt_start(analyses))
        }

        if (TRUE %in% (targets$rtmax == 0)) {
          targets$rtmax[targets$rtmax == 0] <- max(self$get_rt_end(analyses))
        }
        
        if (TRUE %in% is.na(targets$drift)) {
          targets$drift[is.na(targets$drift)] <- 0
        }
        
        if (TRUE %in% is.na(targets$driftmax) & any(self$has_ion_mobility())) {
          targets$driftmax[is.na(targets$driftmax)] <- max(self$get_run(analyses)[["drift"]], na.rm = TRUE)
        }
        
        if (TRUE %in% is.na(targets$driftmin) & any(self$has_ion_mobility())) {
          targets$driftmin[is.na(targets$driftmin)] <- min(self$get_run(analyses)[["drift"]], na.rm = TRUE)
        }

        if (TRUE %in% (targets$driftmax == 0) & any(self$has_ion_mobility())) {
          targets$driftmax[targets$driftmax == 0] <- max(self$get_run(analyses)[["drift"]], na.rm = TRUE)
        }

      } else {
        targets <- NULL
      }

      if (!2 %in% levels) allTraces <- TRUE

      if (!is.logical(allTraces)) allTraces <- TRUE

      if (!allTraces & !is.null(targets)) {

        if (!any(is.numeric(isolationWindow) | is.integer(isolationWindow))) {
          isolationWindow <- 0
        }

        # TODO make case for DIA when pre_mz is not available
        preMZr <- targets[, c("mzmin", "mzmax")]
        preMZr$mzmin <- preMZr$mzmin - (isolationWindow / 2)
        preMZr$mzmax <- preMZr$mzmax + (isolationWindow / 2)

        if (nrow(preMZr) == 1 & TRUE %in% (targets$mzmax == 0)) {
          preMZr <- NULL
        }

      } else {
        preMZr <- NULL
      }

      cached_spectra <- FALSE

      has_spectra <- self$has_loaded_spectra(analyses)
      
      if (.caches_data() & !all(has_spectra)) {
        hash <- patRoon::makeHash(
          analyses, levels, targets, allTraces,
          isolationWindow, minIntensityMS1, minIntensityMS2
        )

        spec_list <- patRoon::loadCacheData("parsed_ms_spectra", hash)

        if (!is.null(spec_list)) {
          message("\U2139 Spectra loaded from cache!")
          return(spec_list)
        }

      } else {
        hash <- NULL
        spec_list <- NULL
      }

      message("\U2699 Parsing spectra from ", length(analyses),  " MS file/s..." ,appendLF = FALSE)

      if (!is.logical(runParallel)) runParallel <- FALSE

      has_spectra <- all(self$has_loaded_spectra(analyses))

      if (runParallel & length(analyses) > 1 & !has_spectra) {
        workers <- parallel::detectCores() - 1
        if (length(analyses) < workers) workers <- length(analyses)
        par_type <- "PSOCK"
        if (parallelly::supportsMulticore()) par_type <- "FORK"
        cl <- parallel::makeCluster(workers, type = par_type)
        doParallel::registerDoParallel(cl)
      } else {
        registerDoSEQ()
      }

      i <- NULL

      if (has_spectra) {
        spec_list <- lapply(self$get_analyses(analyses),
          function(x, levels, targets, preMZr) {

            temp <- x$run

            with_im <- x$has_ion_mobility

            if (!is.null(levels)) temp <- temp[temp$level %in% levels, ]

            if (!is.null(targets)) {

              if ("polarity" %in% colnames(targets)) {
                polarities_targets <- unique(targets$polarity)

                if (length(polarities_targets) == 1) {
                  temp <- temp[temp$polarity == polarities_targets, ]
                }
              }

              if ("analysis" %in% colnames(targets)) {
                tp_tar <- targets[targets$analysis %in% x$name, ]

                if (!is.null(preMZr)) {
                  pre_tar <- preMZr[targets$analysis %in% x$name, ]

                } else {
                  pre_tar <- NULL
                }

                if (nrow(tp_tar) > 0) {
                  temp <- temp[x$spectra, on = .(scan)]
                  temp <- .trim_spectra_targets(temp, tp_tar, pre_tar, with_im)

                } else {
                  temp <- data.frame()
                }
              } else {
                temp <- temp[x$spectra, on = .(scan)]
                temp <- .trim_spectra_targets(temp, targets, preMZr, with_im)
              }
            } else {
              temp <- temp[x$spectra, on = .(scan)]
            }

            if (!with_im) temp[["drift"]] <- NULL

            temp
          },
          levels = levels,
          targets = targets,
          preMZr = preMZr
        )

      } else {
        
        vars <- c(
          "rcpp_parse_ms_analysis_spectra",
          ".trim_spectra_targets"
        )

        spec_list <- foreach(i = self$get_analyses(analyses), .packages = "StreamFind", .export = vars) %dopar% {
          run <- i$run
          
          if (nrow(run) > 0) {

            if (!is.null(levels)) run <- run[run$level %in% levels, ]

            if (!is.null(targets)) {

              trim <- function(v, a, b) rowSums(as.matrix(mapply(function(a, b) v >= a & v <= b, a = a, b = b))) > 0
              
              tp_tar <- targets
              pre_tar <- preMZr

              if ("polarity" %in% colnames(tp_tar)) {
                tar_polarities <- unique(tp_tar$polarity)
                ana_polarities <- unique(run$polarity)
                sel_tar <- tp_tar$polarity %in% ana_polarities
                run <- run[run$polarity %in% tar_polarities, ]
                tp_tar <- tp_tar[sel_tar, ]
                
                if (!is.null(pre_tar)) {
                  pre_tar <- pre_tar[sel_tar, ]  
                }
              }
              
              if (nrow(tp_tar) == 0) return(data.frame())

              if ("analysis" %in% colnames(tp_tar)) {
                sel_tar <- tp_tar$analysis %in% i$name
                tp_tar <- tp_tar[sel_tar, ]

                if (nrow(tp_tar) > 0) {
                  run <- run[trim(run$rt, tp_tar$rtmin, tp_tar$rtmax), ]

                  if (i$has_ion_mobility && nrow(run) > 0) {
                    run <- run[trim(run$drift, tp_tar$driftmin, tp_tar$driftmax), ]
                  }
                  
                  if (!is.null(pre_tar)) {
                    pre_tar <- pre_tar[sel_tar, ]
                    pre_tar_check <- trim(run$pre_mz, pre_tar$mzmin, pre_tar$mzmax)
                    run <- run[(pre_tar_check %in% TRUE) | is.na(pre_tar_check), ]
                  }

                } else {
                  return(data.frame())
                }

              } else {
                run <- run[trim(run$rt, tp_tar$rtmin, tp_tar$rtmax), ]

                if (i$has_ion_mobility && nrow(run) > 0) {
                  run <- run[trim(run$drift, tp_tar$driftmin, tp_tar$driftmax), ]
                }
                
                if (!is.null(pre_tar)) {
                  pre_tar_check <- trim(run$pre_mz, pre_tar$mzmin, pre_tar$mzmax)
                  run <- run[(pre_tar_check %in% TRUE) | is.na(pre_tar_check), ]
                }
              }
            }

            if (nrow(run) > 0) {

              spec <- rcpp_parse_ms_analysis_spectra(i, run$index)

              if (!is.null(targets)) {
                spec <- .trim_spectra_targets(spec, tp_tar, pre_tar, i$has_ion_mobility)
              }

              if (!i$has_ion_mobility) spec[["drift"]] <- NULL

              spec

            } else {
              data.frame()
            }
          } else {
            data.frame()
          }
        }

        if (runParallel & length(analyses) > 1) parallel::stopCluster(cl)
      }

      if (length(spec_list) == length(analyses)) {

        spec_list <- lapply(spec_list, function(x, minMS1, minMS2) {
          x <- x[!(x$intensity <= minMS1 & x$level == 1), ]
          x <- x[!(x$intensity <= minMS2 & x$level == 2), ]
          x
        }, minMS1 = minIntensityMS1, minMS2 = minIntensityMS2)

        names(spec_list) <- analyses

        spec <- rbindlist(spec_list, idcol = "analysis", fill = TRUE)
        
        message(" Done!")

        if (!cached_spectra & !is.null(hash)) {
          if (!is.null(spec)) {
            message("\U1f5ab Parsed spectra cached!")
            patRoon::saveCacheData("parsed_ms_spectra", spec, hash)
          }
        }

        spec

      } else {
        warning("Defined analyses not found!")
        data.table()
      }
    },

    #' @description Gets chromatograms from each analysis.
    #'
    #' @return A data.table with chromatogram/s.
    #'
    get_chromatograms = function(analyses = NULL,
                                 chromatograms = NA_integer_,
                                 minIntensity = 0,
                                 runParallel = FALSE) {

      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())

      if (!(is.numeric(chromatograms) || is.character(chromatograms))) {
        warning("The argument chromatograms must be an integer or character vector with indeces or names!")
        return(data.table())
      }

      has_chroms <- self$has_loaded_chromatograms(analyses)

      if (all(has_chroms)) {
        chrom_list <- lapply(self$get_analyses(analyses), function(x, chromatograms) {
          chroms <- x$chromatograms
          
          if (is.numeric(chromatograms)) {
            which_chroms <- chroms$index %in% chromatograms
            chroms <- chroms[which_chroms, ]
            
          } else if (is.character(chromatograms)) {
            which_chroms <- chroms$id %in% chromatograms
            chroms <- chroms[which_chroms, ]
            
          } else if (!is.na(chromatograms)) {
            return(data.table())
          }
          
          chroms
          
        }, chromatograms = chromatograms)

      } else {

        message("\U2699 Parsing chromatograms from ", length(analyses),  " MS file/s..." ,
          appendLF = FALSE
        )

        if (!is.logical(runParallel)) runParallel <- FALSE

        if (runParallel & length(analyses) > 1) {
          workers <- parallel::detectCores() - 1
          if (length(files) < workers) workers <- length(files)
          par_type <- "PSOCK"
          if (parallelly::supportsMulticore()) par_type <- "FORK"
          cl <- parallel::makeCluster(workers, type = par_type)
          doParallel::registerDoParallel(cl)
        } else {
          registerDoSEQ()
        }

        i = NULL
        
        if (is.numeric(chromatograms)) {
          index <- chromatograms
        } else {
          index <- NA_integer_
        }

        chrom_list <- foreach(i = self$get_analyses(analyses)) %dopar% {
          chroms <- rcpp_parse_ms_analysis_chromatograms(i, index)
          chroms
        }
        
        if (is.character(chromatograms)) {
          chrom_list <- lapply(chrom_list, function(x, chromatograms) {
            which_chroms <- x$id %in% chromatograms
            x <- x[which_chroms, ]
            x
          }, chromatograms = chromatograms)
        }

        message(" Done!")
      }

      if (length(chrom_list) == length(analyses)) {
        names(chrom_list) <- analyses
        chrom_df <- rbindlist(chrom_list, idcol = "analysis", fill = TRUE)
        chrom_df <- chrom_df[chrom_df$intensity > minIntensity, ]
        chrom_df
      } else {
        warning("Defined analyses not found!")
        data.table()
      }
    },

    #' @description Gets extract ion chromatograms (EIC) from the analyses based on targets as a data.table.
    #'
    get_eic = function(analyses = NULL,
                       mass = NULL,
                       mz = NULL,
                       rt = NULL,
                       drift = NULL,
                       ppm = 20,
                       sec = 60,
                       millisec = 5,
                       id = NULL,
                       runParallel = FALSE) {

      eic <- self$get_spectra(
        analyses, levels = 1,
        mass, mz, rt, drift, ppm, sec, millisec, id,
        allTraces = TRUE,
        isolationWindow = 1.3,
        minIntensityMS1 = 0,
        minIntensityMS2 = 0,
        runParallel = runParallel
      )

      if (nrow(eic) > 0) {
        eic <- as.data.table(eic)

        if (!"id" %in% colnames(eic)) eic$id <- NA_character_

        if (!"polarity" %in% colnames(eic)) eic$polarity <- 0

        eic <- eic[, `:=`(intensity = sum(intensity)),
          by = c("analysis", "polarity", "id", "rt")
        ][]

        eic <- eic[, c("analysis", "polarity", "id", "rt", "intensity"), with = FALSE]

        eic <- unique(eic)
      }

      eic
    },

    #' @description Gets MS1 data from the analyses based on targets as a data.frame.
    #'
    get_ms1 = function(analyses = NULL,
                       mass = NULL,
                       mz = NULL,
                       rt = NULL,
                       drift = NULL,
                       ppm = 20,
                       sec = 60,
                       millisec = 5,
                       id = NULL,
                       mzClust = 0.003,
                       presence = 0.8,
                       verbose = FALSE,
                       minIntensity = 1000,
                       runParallel = FALSE) {

      ms1 <- self$get_spectra(
        analyses, levels = 1,
        mass, mz, rt, drift, ppm, sec, millisec, id,
        allTraces = TRUE,
        minIntensityMS1 = minIntensity,
        minIntensityMS2 = 0,
        runParallel = runParallel
      )

      if (nrow(ms1) == 0) return(ms1)

      if (!"id" %in% colnames(ms1)) {

        if (any(self$has_ion_mobility())) {
          ms1$id <- paste(
            round(min(ms1$mz), 4),
            "-",
            round(max(ms1$mz), 4),
            "/",
            round(max(ms1$rt), 0),
            "-",
            round(min(ms1$rt), 0),
            "/",
            round(max(ms1$drift), 0),
            "-",
            round(min(ms1$drift), 0),
            sep = ""
          )

        } else {
          ms1$id <- paste(
            round(min(ms1$mz), 4),
            "-",
            round(max(ms1$mz), 4),
            "/",
            round(max(ms1$rt), 0),
            "-",
            round(min(ms1$rt), 0),
            sep = ""
          )
        }
      }

      if (!is.logical(verbose)) verbose = FALSE

      if (!is.numeric(mzClust)) mzClust = 0.01

      ms1$unique_id <- paste0(ms1$analysis, "_", ms1$id, "_", ms1$polarity)

      ms1_list <- rcpp_ms_cluster_spectra(ms1, mzClust, presence, verbose)

      ms1_df <- rbindlist(ms1_list, fill = TRUE)

      ms1_df <- ms1_df[order(ms1_df$mz), ]

      ms1_df <- ms1_df[order(ms1_df$id), ]

      ms1_df <- ms1_df[order(ms1_df$analysis), ]

      ms1_df
    },

    #' @description Gets MS2 data from the analyses based on targets as a data.frame.
    #'
    get_ms2 = function(analyses = NULL,
                       mass = NULL,
                       mz = NULL,
                       rt = NULL,
                       drift = NULL,
                       ppm = 20,
                       sec = 60,
                       millisec = 5,
                       id = NULL,
                       isolationWindow = 1.3,
                       mzClust = 0.005,
                       presence = 0.8,
                       verbose = FALSE,
                       minIntensity = 0,
                       runParallel = FALSE) {

      ms2 <- self$get_spectra(
        analyses, levels = 2,
        mass, mz, rt, drift, ppm, sec, millisec, id,
        isolationWindow = isolationWindow,
        allTraces = FALSE,
        minIntensityMS1 = 0,
        minIntensityMS2 = minIntensity,
        runParallel = runParallel
      )

      if (nrow(ms2) == 0) return(ms2)

      if (!"id" %in% colnames(ms2)) {
        if (any(self$has_ion_mobility())) {
          ms2$id <- paste(
            round(min(ms2$mz), 4),
            "-",
            round(max(ms2$mz), 4),
            "/",
            round(max(ms2$rt), 0),
            "-",
            round(min(ms2$rt), 0),
            "/",
            round(max(ms2$drift), 0),
            "-",
            round(min(ms2$drift), 0),
            sep = ""
          )

        } else {
          ms2$id <- paste(
            round(min(ms2$mz), 4),
            "-",
            round(max(ms2$mz), 4),
            "/",
            round(max(ms2$rt), 0),
            "-",
            round(min(ms2$rt), 0),
            sep = ""
          )
        }
      }

      if (!is.logical(verbose)) verbose = FALSE

      if (!is.numeric(mzClust)) mzClust = 0.01

      ms2$unique_id <- paste0(ms2$analysis, "_", ms2$id, "_", ms2$polarity)

      ms2_list <- rcpp_ms_cluster_spectra(ms2, mzClust, presence, verbose)

      ms2_df <- rbindlist(ms2_list, fill = TRUE)

      ms2_df <- ms2_df[order(ms2_df$mz), ]

      ms2_df <- ms2_df[order(ms2_df$id), ]

      ms2_df <- ms2_df[order(ms2_df$analysis), ]

      ms2_df
    },
    
    #' @description Gets the list of features for each analysis.
    #' 
    #' @return A list of data.table objects for each analysis.
    #'
    get_feature_list = function(analyses = NULL, filtered = FALSE) {
      
      analyses <- private$.check_analyses_argument(analyses)

      if (ms$has_results("patRoon")) {
        
        pat <- private$.results$patRoon$data
        
        if (self$has_groups()) pat <- pat@features
        
        f_list <- pat@features
        
        f_list <- f_list[analyses]
        
        if (filtered) {
          
          if (is.null(private$.results$patRoon$filtered)) {
            private$.results$patRoon$filtered <- lapply(self$get_analysis_names(), function(x) data.table()) 
          }
          
          filtered_list <- private$.results$patRoon$filtered[analyses]
          
          f_list <- Map(
            function(x, y) {
              
              if (nrow(x) > 0) x <- x[!(x$ID %in% y$ID), ]
              
              y <- rbindlist(list(y, x), fill = TRUE)
              
              y
            },
            filtered_list, f_list
          )
        }
        
        if ("featuresSet" %in% is(pat)) {
          
          for (x in names(f_list)) {
            
            pol <- self$get_polarities(x)
            
            if ("positive" %in% pol) adduct_val <- -1.007276
            if ("negative" %in% pol) adduct_val <- 1.007276
            
            sel_to_change <- round(f_list[[x]]$mz, 0) == round(f_list[[x]]$mass, 0)
            
            f_list[[x]]$mz[sel_to_change] <- f_list[[x]]$mz - adduct_val
            f_list[[x]]$mzmin[sel_to_change] <- f_list[[x]]$mzmin - adduct_val
            f_list[[x]]$mzmax[sel_to_change] <- f_list[[x]]$mzmax - adduct_val
          }
        }
        
        f_list <- lapply(f_list, function(x) {
          z <- copy(x)
          setnames(z, "ID", "feature")
          setnames(z, "ret", "rt")
          setnames(z, "retmin", "rtmin")
          setnames(z, "retmax", "rtmax")
        })
        
        f_list

      } else {
        NULL
      }
    },

    #' @description Gets a data.table with all features in analyses or as selected by the arguments.
    #'
    get_features = function(analyses = NULL,
                            features = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            drift = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            filtered = FALSE) {

      analyses <- private$.check_analyses_argument(analyses)

      if (is.null(analyses)) return(data.table())
      
      fts <- NULL
      
      if (ms$has_results("patRoon")) fts <- self$feature_list[analyses]
      
      if (is.null(fts)) return(data.table())
      
      fts <- rbindlist(fts, idcol = "analysis", fill = TRUE)
      
      if (nrow(fts) == 0) return(data.table())

      if (!filtered) fts <- fts[!fts$filtered, ]

      if (!is.null(features)) {
        target_id <- features

        if (is.character(target_id)) {

          if ("group" %in% colnames(fts)) {
            fts <- fts[fts$feature %in% target_id | fts$group %in% target_id, ]

          } else {
            fts <- fts[fts$feature %in% target_id, ]
          }

          return(fts)

        } else if (is.numeric(target_id)) {
          fts <- fts[target_id, ]

          return(fts)
        }

        if (is.data.frame(target_id)) {

          if (all(colnames(fts) %in% colnames(target_id))) return(target_id)

          if ("analysis" %in% colnames(target_id)) {
            sel <- rep(FALSE, nrow(fts))

            for (i in seq_len(nrow(target_id))) {
              sel[(fts$feature %in% target_id$feature[i] &
                fts$analysis %in% target_id$analysis[i]) |
                fts$group %in% target_id$group] <- TRUE
            }

            fts <- fts[sel, ]

            if ("name" %in% colnames(target_id)) {
              ids <- target_id$name
              names(ids) <- target_id$feature
              fts$name <- ids[fts$feature]
            }

            return(fts)

          } else if ("group" %in% colnames(target_id)) {
            sel <- rep(FALSE, nrow(fts))

            for (i in seq_len(nrow(target_id))) {
              sel[fts$feature %in% target_id$feature[i] |
                fts$group %in% target_id$group] <- TRUE
            }

            fts <- fts[sel, ]

            if ("name" %in% colnames(target_id)) {
              ids <- unique(target_id$name)
              names(ids) <- unique(target_id$group)
              fts$name <- ids[fts$group]
            }

            return(fts)
          }
        }

        return(data.table())
      }

      if (!is.null(mass)) {

        if (is.data.frame(mass)) {
          colnames(mass) <- gsub("mass", "mz", colnames(mass))
          colnames(mass) <- gsub("neutralMass", "mz", colnames(mass))
          colnames(mass) <- gsub("min", "mzmin", colnames(mass))
          colnames(mass) <- gsub("max", "mzmax", colnames(mass))
        }

        targets <- make_ms_targets(mass, rt, drift, ppm, sec, millisec)

        for (i in seq_len(nrow(targets))) {

          if (targets$rtmax[i] == 0) targets$rtmax[i] <- max(fts$rtmax)

          if (targets$mzmax[i] == 0) targets$mzmax[i] <- max(fts$mass)

          if ("drift" %in% colnames(fts)) {
            if (targets$driftmax[i] == 0) targets$driftmax[i] <- max(fts$drift)
          }
        }

        sel <- rep(FALSE, nrow(fts))

        ids <- rep(NA_character_, nrow(fts))

        for (i in seq_len(nrow(targets))) {

          if ("drift" %in% colnames(fts)) {
            sel[between(fts$mass, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
                    between(fts$drift, targets$driftmin[i], targets$driftmax[i])] <- TRUE

            ids[between(fts$mass, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
                    between(fts$drift, targets$driftmin[i], targets$driftmax[i])] <- targets$id[i]

          } else {
            sel[between(fts$mass, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE

            ids[between(fts$mass, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- targets$id[i]
          }
        }

        fts$name <- ids

        return(fts[sel])
      }

      if (!is.null(mz)) {
        targets <- make_ms_targets(mz, rt, drift, ppm, sec, millisec)

        for (i in seq_len(nrow(targets))) {

          if (targets$rtmax[i] == 0) targets$rtmax[i] <- max(fts$rtmax)

          if (targets$mzmax[i] == 0) targets$mzmax[i] <- max(fts$mzmax)

          if ("drift" %in% colnames(fts)) {
            if (targets$driftmax[i] == 0) targets$driftmax[i] <- max(fts$drift)
          }
        }

        sel <- rep(FALSE, nrow(fts))

        ids <- rep(NA_character_, nrow(fts))

        for (i in seq_len(nrow(targets))) {

          if ("drift" %in% colnames(fts)) {
            sel[between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
                  between(fts$drift, targets$driftmin[i], targets$driftmax[i])] <- TRUE

            ids[between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
                  between(fts$drift, targets$driftmin[i], targets$driftmax[i])] <- targets$id[i]

          } else {
            sel[between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE

            ids[between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
                  between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- targets$id[i]
          }
        }

        fts$name <- ids

        return(fts[sel])
      }

      fts
    },

    #' @description Gets a data.table with feature EICs following the targets from the arguments.
    #'
    get_features_eic = function(analyses = NULL,
                                features = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                drift = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                rtExpand = 120,
                                mzExpand = NULL,
                                filtered = FALSE,
                                loaded = TRUE,
                                runParallel = FALSE) {

      fts <- self$get_features(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (nrow(fts) == 0) return(data.table())

      analysis_names <- unique(fts$analysis)

      if (loaded & any(self$has_features_eic(analysis_names))) {
        
        eic_list <- lapply(seq_len(nrow(fts)), function(x, fts) {
          temp <- fts[x, ]
          
          temp_ms <- temp[["eic"]][[1]]
          
          if (is.null(temp_ms)) return(data.table())
          
          temp_ms$analysis <- temp$analysis
          
          temp_ms$feature <- temp$feature
          
          temp_ms
        }, fts = fts)
        
        eic <- rbindlist(eic_list, fill = TRUE)
        
        add_filtered <- FALSE
        
        if (any(fts$filtered)) {
          if (nrow(eic) == 0) {
            add_filtered <- TRUE
          } else if (any(!(fts$feature[fts$filtered] %in% eic$feature))) {
            add_filtered <- TRUE
          }
        }

        if (add_filtered) {
          fts_filtered <- fts[fts$filtered, ]
          fts_filtered <- fts_filtered[!(fts_filtered$feature %in% eic$feature), ]

          settings <- self$get_settings("load_features_eic")[[1]]
          
          if (!is.null(settings)) {
            
            parameters <- settings$parameters
            
            if ("rtExpand" %in% names(parameters)) {
              rtExpand <- parameters[["rtExpand"]]
            } else {
              if (is.null(rtExpand)) rtExpand <- 0
            }
            
            if ("mzExpand" %in% names(parameters)) {
              mzExpand <- parameters[["mzExpand"]]
            } else {
              if (is.null(mzExpand)) mzExpand <- 0
            }
            
            fts_filtered$rtmin <- fts_filtered$rtmin - rtExpand
            fts_filtered$rtmax <- fts_filtered$rtmax + rtExpand
            fts_filtered$mzmin <- fts_filtered$mzmin - mzExpand
            fts_filtered$mzmax <- fts_filtered$mzmax + mzExpand
          }

          eic_2 <- self$get_spectra(
            analyses = analyses,
            levels = 1,
            mz = fts_filtered, id = fts_filtered$feature,
            runParallel = runParallel
          )

          eic_2 <- eic_2[, c("analysis", "polarity", "id", "rt", "mz", "intensity"), with = FALSE]

          setnames(eic_2, "id", "feature")

          eic <- list(eic, eic_2)

          eic <- rbindlist(eic, fill = TRUE)
        }

        if (nrow(eic) == 0) return(data.table())

      } else {

        if (is.null(rtExpand)) rtExpand <- 0
        if (is.null(mzExpand)) mzExpand <- 0

        fts$rtmin <- fts$rtmin - rtExpand
        fts$rtmax <- fts$rtmax + rtExpand
        fts$mzmin <- fts$mzmin - mzExpand
        fts$mzmax <- fts$mzmax + mzExpand

        eic <- self$get_spectra(
          analyses = analyses,
          levels = 1,
          mz = fts,
          id = fts$feature,
          runParallel = runParallel
        )

        eic <- eic[, c("analysis", "polarity", "id", "rt", "mz", "intensity"), with = FALSE]

        setnames(eic, "id", "feature")
      }

      if ("group" %in% colnames(fts)) {
        fgs <- fts$group
        names(fgs) <- fts$feature
        eic$group <- fgs[eic$feature]
      }

      if ("name" %in% colnames(fts)) {
        tar_ids <- fts$name
        names(tar_ids) <- fts$feature
        eic$name <- tar_ids[eic$feature]
      }

      eic
    },

    #' @description Gets a data.table of averaged MS1 spectrum for features in the analyses or as selected from the 
    #' arguments.
    #'
    get_features_ms1 = function(analyses = NULL,
                                features = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                drift = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                rtWindow = c(-2, 2),
                                mzWindow = c(-5, 100),
                                mzClust = 0.003,
                                presence = 0.8,
                                minIntensity = 1000,
                                verbose = FALSE,
                                filtered = FALSE,
                                loadedMS1 = TRUE,
                                runParallel = FALSE) {

      fts <- self$get_features(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (nrow(fts) == 0) return(data.table())

      if (!is.null(rtWindow) & length(rtWindow) == 2 & is.numeric(rtWindow)) {
        fts$rtmin <- fts$rt + rtWindow[1]
        fts$rtmax <- fts$rt + rtWindow[2]
      }

      if (!is.null(mzWindow) & length(mzWindow) == 2 & is.numeric(mzWindow)) {
        fts$mzmin <- fts$mz + mzWindow[1]
        fts$mzmax <- fts$mz + mzWindow[2]
      }

      analysis_names <- unique(fts$analysis)

      if (loadedMS1 & any(self$has_loaded_features_ms1(analysis_names))) {

        ms1_list <- lapply(seq_len(nrow(fts)), function(x, fts) {
          temp <- fts[x, ]

          temp_ms <- temp[["ms1"]][[1]]

          if (is.null(temp_ms)) return(data.table())

          temp_ms$analysis <- temp$analysis

          temp_ms$feature <- temp$feature

          temp_ms
        }, fts = fts)

        ms1 <- rbindlist(ms1_list, fill = TRUE)

        add_filtered <- FALSE

        if (any(fts$filtered)) {
          if (nrow(ms1) == 0) {
            add_filtered <- TRUE
          } else if (any(!(fts$feature[fts$filtered] %in% ms1$feature))) {
            add_filtered <- TRUE
          }
        }

        if (add_filtered) {
          fts_filtered <- fts[fts$filtered, ]
          fts_filtered <- fts_filtered[!(fts_filtered$feature %in% ms1$feature), ]

          settings <- self$get_settings("load_features_ms1")[[1]]
          
          if (!is.null(settings)) {

            parameters <- settings$parameters
            
            if ("mzClust" %in% names(parameters)) {
              mzClust <- parameters[["mzClust"]]
            }
            
            if ("presence" %in% names(parameters)) {
              presence <- parameters[["presence"]]
            }
            
            if ("minIntensity" %in% names(parameters)) {
              minIntensity <- parameters[["minIntensity"]]
            }
          }

          ms1_2 <- self$get_ms1(
            analyses = unique(fts$analysis),
            mz = fts_filtered,
            id = fts_filtered$feature,
            mzClust = mzClust,
            presence = presence,
            minIntensity = minIntensity,
            verbose = verbose,
            runParallel = runParallel
          )

          setnames(ms1_2, "id", "feature")

          ms1 <- list(ms1, ms1_2)

          ms1 <- rbindlist(ms1, fill = TRUE)
        }

        if (nrow(ms1) == 0) return(data.table())

      } else {
        ms1 <- self$get_ms1(
          analyses = unique(fts$analysis),
          mz = fts,
          id = fts$feature,
          mzClust = mzClust,
          presence = presence,
          minIntensity = minIntensity,
          verbose = verbose,
          runParallel = runParallel
        )

        setnames(ms1, "id", "feature")
      }

      unique_fts_id <- paste0(fts$analysis, "-", fts$feature)

      unique_ms1_id <- paste0(ms1$analysis, "-", ms1$feature)

      if ("group" %in% colnames(fts)) {
        fgs <- fts$group
        names(fgs) <- unique_fts_id
        ms1$group <- fgs[unique_ms1_id]
      }

      if ("name" %in% colnames(fts)) {
        tar_ids <- fts$name
        names(tar_ids) <- unique_fts_id
        ms1$name <- tar_ids[unique_ms1_id]
      }

      copy(ms1)
    },

    #' @description Gets a data.table of averaged MS2 spectrum for features in the analyses or as selected from the 
    #' arguments.
    #'
    get_features_ms2 = function(analyses = NULL,
                                features = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                drift = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                isolationWindow = 1.3,
                                mzClust = 0.003,
                                presence = 0.8,
                                minIntensity = 0,
                                verbose = FALSE,
                                filtered = FALSE,
                                loadedMS2 = TRUE,
                                runParallel = FALSE) {

      fts <- self$get_features(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (nrow(fts) == 0) return(data.table())

      analysis_names <- unique(fts$analysis)
      
      if (loadedMS2 & any(self$has_loaded_features_ms2(analysis_names))) {

        ms2_list <- lapply(seq_len(nrow(fts)), function(x, fts) {
          temp <- fts[x, ]

          temp_ms <- temp[["ms2"]][[1]]

          if (is.null(temp_ms)) return(data.table())

          temp_ms$analysis <- temp$analysis

          temp_ms$feature <- temp$feature

          temp_ms
        }, fts = fts)

        ms2 <- rbindlist(ms2_list, fill = TRUE)

        add_filtered <- FALSE

        if (any(fts$filtered)) {
          if (nrow(ms2) == 0) {
            add_filtered <- TRUE
          } else if (any(!(fts$feature[fts$filtered] %in% ms2$feature))) {
            add_filtered <- TRUE
          }
        }

        if (add_filtered) {
          fts_filtered <- fts[fts$filtered, ]
          fts_filtered <- fts_filtered[!(fts_filtered$feature %in% ms2$feature), ]

          settings <- self$get_settings("load_features_ms2")[[1]]

          if (!is.null(settings)) {
            
            parameters <- settings$parameters
            
            if ("isolationWindow" %in% names(parameters)) {
              isolationWindow <- parameters[["isolationWindow"]]
            }
            
            if ("mzClust" %in% names(parameters)) {
              mzClust <- parameters[["mzClust"]]
            }
            
            if ("presence" %in% names(parameters)) {
              presence <- parameters[["presence"]]
            }
            
            if ("minIntensity" %in% names(parameters)) {
              minIntensity <- parameters[["minIntensity"]]
            }
          }

          ms2_2 <- self$get_ms2(
            analyses = unique(fts$analysis),
            mz = fts_filtered,
            id = fts_filtered$feature,
            isolationWindow = isolationWindow,
            mzClust = mzClust,
            presence = presence,
            minIntensity = minIntensity,
            verbose = verbose,
            runParallel = runParallel
          )
          
          if (nrow(ms2_2) == 0) return(data.table())

          setnames(ms2_2, "id", "feature", skip_absent = TRUE)

          ms2 <- list(ms2, ms2_2)

          ms2 <- rbindlist(ms2, fill = TRUE)
        }

        if (nrow(ms2) == 0) return(data.table())

      } else {
        ms2 <- self$get_ms2(
          analyses = unique(fts$analysis),
          mz = fts,
          id = fts$feature,
          isolationWindow = isolationWindow,
          mzClust = mzClust,
          presence = presence,
          minIntensity = minIntensity,
          verbose = verbose,
          runParallel = runParallel
        )
        
        if (nrow(ms2) == 0) return(data.table())

        setnames(ms2, "id", "feature", skip_absent = TRUE)
      }

      unique_fts_id <- paste0(fts$analysis, "-", fts$feature)

      unique_ms2_id <- paste0(ms2$analysis, "-", ms2$feature)

      if ("group" %in% colnames(fts)) {
        fgs <- fts$group
        names(fgs) <- unique_fts_id
        ms2$group <- fgs[unique_ms2_id]
      }

      if ("name" %in% colnames(fts)) {
        tar_ids <- fts$name
        names(tar_ids) <- unique_fts_id
        ms2$name <- tar_ids[unique_ms2_id]
      }

      copy(ms2)
    },

    #' @description Gets a data.table with feature groups from the analyses.
    #' 
    #' @param sdValues Logical length 1. Set to `TRUE` for returning the sd values when averaging the intensity 
    #' within analysis replicates.
    #' @param metadata Logical length 1. Set to `TRUE` for returning extra metadata from feature groups 
    #' (e.g., presence in each analysis replicate and mass and time widths).
    #'
    get_groups = function(groups = NULL,
                          mass = NULL,
                          mz = NULL,
                          rt = NULL,
                          drift = NULL,
                          ppm = 20,
                          sec = 60,
                          millisec = 5,
                          filtered = FALSE,
                          intensities = TRUE,
                          average = FALSE,
                          sdValues = FALSE,
                          metadata = FALSE) {

      if (!self$has_groups()) return(data.table())

      fts <- self$get_features(
        analyses = NULL,
        features = groups,
        mass, mz, rt, drift, ppm, sec, millisec,
        filtered = filtered
      )
      
      if (nrow(fts) > 0) {
        g_ids <- unique(fts$group)
        
        fgroups <- data.table("group" = g_ids)
        
        if (intensities) {
          
          if (average) {
            rpls <- self$get_replicate_names()
            fts_temp <- copy(fts)
            fts_temp$analysis <- rpls[fts_temp$analysis]
            fts_av <- fts_temp[, .(intensity = mean(intensity), sd = sd(intensity)), by = c("group", "analysis")]
            fts_av$sd[is.na(fts_av$sd)] <- 0 
            fts_av$sd <- round(fts_av$sd / fts_av$intensity * 100, digits = 0)
            fts_sd <- copy(fts_av)
            fts_sd$intensity <- NULL
            fts_sd$analysis <- paste(fts_sd$analysis, "_sd", sep = "")
            fts_sd <- tidyr::spread(fts_sd[, c("group", "analysis", "sd"), with = TRUE], key = analysis, value = sd, fill = NA)
            fts_av$sd <- NULL
            fts_av <- tidyr::spread(fts_av, key = analysis, value = intensity, fill = 0)
            
          } else {
            fts_av <- fts[, .(intensity = max(intensity)), by = c("group", "analysis")]
            fts_av <- tidyr::spread(fts_av, key = analysis, value = intensity, fill = 0)
          }
        }

        if ("name" %in% colnames(fts)) {
          g_names <- fts$name
          names(g_names) <- fts$group
          g_names <- g_names[!duplicated(names(g_names))]
          fgroups$name <- g_names[fgroups$group]
        }
        
        if (metadata) {
          cols <- colnames(fts)
          if (!"istd" %in% cols) fts[["istd"]] <- list(NULL)
          if (!"quality" %in% cols) fts[["quality"]] <- list(NULL)
          if (!"isotope" %in% cols) fts[["isotope"]] <- list(NULL)
          
          fts_meta <- fts[, .(
            rt = round(mean(rt), digits = 0),
            mass = round(mean(mass), digits = 4),
            rtdev = round(max(rtmax - rtmin), digits = 0),
            massdev = round(max(mzmax - mzmin), digits = 4),
            presence = round(length(feature) / self$get_number_analyses() * 100, digits = 0),
            sn = round(max(vapply(quality, function(x) if (!is.null(x)) x$sn else 0, 0), na.rm = TRUE), digits = 1),
            iso = min(vapply(isotope, function(x) if (!is.null(x)) x$step else 0, 0)),
            istd = paste0(unique(vapply(istd, function(x) if (!is.null(x)) x$name else NA_character_, NA_character_)), collapse = "; "),
            filtered = all(filtered),
            filter = paste(unique(filter), collapse = "; ")
          ), by = "group"]
          
          fgroups <- fgroups[fts_meta, on = "group"]
        }
        
        if (intensities) fgroups <- fgroups[fts_av, on = "group"]
        
        if (average && sdValues) fgroups <- fgroups[fts_sd, on = "group"]
        
        fgroups
        
      } else {
        data.table()
      }
    },

    #' @description Gets a data.table of averaged MS1 spectrum for feature groups in the analyses.
    #'
    get_groups_ms1 = function(groups = NULL,
                              mass = NULL,
                              mz = NULL,
                              rt = NULL,
                              drift = NULL,
                              ppm = 20,
                              sec = 60,
                              millisec = 5,
                              rtWindow = c(-2, 2),
                              mzWindow = c(-5, 90),
                              mzClustFeatures = 0.003,
                              presenceFeatures = 0.8,
                              minIntensityFeatures = 1000,
                              loadedFeaturesMS1 = TRUE,
                              mzClust = 0.003,
                              presence = 0.8,
                              minIntensity = 1000,
                              groupBy = "groups",
                              verbose = FALSE,
                              filtered = FALSE,
                              runParallel = FALSE) {

      fgs <- self$get_groups(
        groups, mass, mz, rt, drift, ppm, sec, millisec, filtered,
        intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = FALSE
      )

      if (nrow(fgs) == 0) return(data.table())

      fts <- self$get_features(features = fgs$group)

      if (nrow(fts) == 0) return(data.table())

      ms1 <- self$get_features_ms1(
        analyses = unique(fts$analysis),
        features = fts$feature,
        rtWindow = rtWindow,
        mzWindow = mzWindow,
        mzClust = mzClustFeatures,
        presence = presenceFeatures,
        minIntensity = minIntensityFeatures,
        verbose = verbose,
        filtered = filtered,
        loadedMS1 = loadedFeaturesMS1,
        runParallel = runParallel
      )

      ms1 <- ms1[ms1$intensity > minIntensity, ]

      if (nrow(ms1) == 0) return(data.table())

      polarities <- unique(self$get_polarities(analyses = unique(ms1$analysis)))

      multiple_polarities <- FALSE

      if (length(polarities) > 1) multiple_polarities <- TRUE

      if ("groups" %in% groupBy) {

        if (multiple_polarities) {
          ms1$unique_id <- paste0(ms1$group, "_", ms1$polarity)
          ms1$analysis <- NA_character_

        } else {
          ms1$unique_id <- ms1$group
          ms1$analysis <- NA_character_
        }

      } else {
        rpls <- self$get_replicate_names()
        ms1$analysis <- rpls[ms1$analysis]

        if (multiple_polarities) {
          ms1$unique_id <- paste0(ms1$analysis, "_", ms1$group, "", ms1$polarity)
        } else {
          ms1$unique_id <- paste0(ms1$analysis, "_", ms1$group)
        }
      }

      ms1$id <- ms1$group

      ms1_list <- rcpp_ms_cluster_spectra(ms1, mzClust, presence, verbose)

      ms1_df <- rbindlist(ms1_list, fill = TRUE)

      ms1_df$group <- ms1_df$id

      ms1_df[["id"]] <- NULL

      ms1_df <- ms1_df[order(ms1_df$mz), ]

      ms1_df <- ms1_df[order(ms1_df$group), ]

      if ("groups" %in% groupBy) {
        ms1_df[["analysis"]] <- NULL

      } else {
        ms1_df <- ms1_df[order(ms1_df$analysis), ]
        setnames(ms1_df, "analysis", "replicate")
      }

      if ("name" %in% colnames(fgs)) {
        tar_ids <- fgs$name
        names(tar_ids) <- fgs$group
        ms1_df$name <- tar_ids[ms1_df$group]
      }

      copy(ms1_df)
    },

    #' @description Gets a data.table of averaged MS2 spectrum for feature groups in the analyses.
    #'
    get_groups_ms2 = function(groups = NULL,
                              mass = NULL,
                              mz = NULL,
                              rt = NULL,
                              drift = NULL,
                              ppm = 20,
                              sec = 60,
                              millisec = 5,
                              isolationWindow = 1.3,
                              mzClustFeatures = 0.003,
                              presenceFeatures = 0.8,
                              minIntensityFeatures = 100,
                              loadedFeaturesMS2 = TRUE,
                              mzClust = 0.003,
                              presence = 0.8,
                              minIntensity = 100,
                              groupBy = "groups",
                              verbose = FALSE,
                              filtered = FALSE,
                              runParallel = FALSE) {

      fgs <- self$get_groups(
        groups, mass, mz, rt, drift, ppm, sec, millisec, filtered,
        intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = FALSE
      )

      if (nrow(fgs) == 0) return(data.table())

      fts <- self$get_features(features = fgs$group, filtered = filtered)

      if (nrow(fts) == 0) return(data.table())

      ms2 <- self$get_features_ms2(
        analyses = unique(fts$analysis),
        features = fts$feature,
        isolationWindow = isolationWindow,
        mzClust = mzClustFeatures,
        presence = presenceFeatures,
        minIntensity = minIntensityFeatures,
        verbose = verbose,
        filtered = filtered,
        loadedMS2 = loadedFeaturesMS2,
        runParallel = runParallel
      )

      ms2 <- ms2[ms2$intensity > minIntensity, ]

      if (nrow(ms2) == 0) return(data.table())

      polarities <- unique(self$get_polarities(analyses = unique(ms2$analysis)))

      multiple_polarities <- FALSE

      if (length(polarities) > 1) multiple_polarities <- TRUE

      if ("groups" %in% groupBy) {
        if (multiple_polarities) {
          ms2$unique_id <- paste0(ms2$group, "_", ms2$polarity)
          ms2$analysis <- NA_character_

        } else {
          ms2$unique_id <- ms2$group
          ms2$analysis <- NA_character_
        }

      } else {
        rpls <- self$get_replicate_names()
        ms2$analysis <- rpls[ms2$analysis]

        if (multiple_polarities) {
          ms2$unique_id <- paste0(ms2$analysis, "_", ms2$group, "", ms2$polarity)
        } else {
          ms2$unique_id <- paste0(ms2$analysis, "_", ms2$group)
        }
      }

      ms2$id <- ms2$group

      ms2_list <- rcpp_ms_cluster_spectra(ms2, mzClust, presence, verbose)

      ms2_df <- rbindlist(ms2_list, fill = TRUE)

      ms2_df$group <- ms2_df$id

      ms2_df[["id"]] <- NULL

      ms2_df <- ms2_df[order(ms2_df$mz), ]

      ms2_df <- ms2_df[order(ms2_df$group), ]

      if ("groups" %in% groupBy) {
        ms2_df[["analysis"]] <- NULL

      } else {
        ms2_df <- ms2_df[order(ms2_df$analysis), ]
        setnames(ms2_df, "analysis", "replicate")
      }

      if ("name" %in% colnames(fgs)) {
        tar_ids <- fgs$name
        names(tar_ids) <- fgs$group
        ms2_df$name <- tar_ids[ms2_df$group]
      }

      copy(ms2_df)
    },
    
    #' @description Creates S4 class `MSPeakLists`. Note that feature groups are required. The MS and MSMS spectra 
    #' of each feature are then average by \pkg{patRoon} to produce the feature group spectra using the parameters 
    #' of the function \link[patRoon]{getDefAvgPListParams}.
    #' 
    #' @param useLoaded Logical of length one. When `TRUE` and both MS1 and MS2 are loaded to features, 
    #' these are used otherwise the native function `generateMSPeakLists` from \pkg{patRoon} is used instead.
    #' @param maxMSRtWindow Maximum chromatographic peak window used for spectrum 
    #' averaging (in seconds, +/- retention time). If NULL all spectra from a feature 
    #' will be taken into account. Lower to decrease processing time.
    #' @param precursorMzWindow The m/z window (in Da) to find MS/MS spectra of a precursor. 
    #' This is typically used for Data-Dependent like MS/MS data and should correspond to the 
    #' isolation m/z window (i.e. +/- the precursor m/z) that was used to collect the data. 
    #' For Data-Independent MS/MS experiments, where precursor ions are not isolated prior to 
    #' fragmentation (e.g. bbCID, MSe, all-ion, ...) the value should be NULL.
    #' @param clusterMzWindow m/z window (in Da) used for clustering m/z values
    #' when spectra are averaged. For method="hclust" this corresponds to the
    #' cluster height, while for method="distance" this value is used to find
    #' nearby masses (+/- window). Too small windows will prevent clustering
    #' m/z values (thus erroneously treating equal masses along spectra as
    #' different), whereas too big windows may cluster unrelated m/z values
    #' from different or even the same spectrum together.
    #' @param topMost Only retain this maximum number of MS peaks when generating
    #' averaged spectra. Lowering this number may exclude more irrelevant (noisy)
    #' MS peaks and decrease processing time, whereas higher values may avoid
    #' excluding lower intense MS peaks that may still be of interest.
    #' @param minIntensityPre MS peaks with intensities below this value will
    #' be removed (applied prior to selection by `topMost`) before averaging.
    #' @param minIntensityPost MS peaks with intensities below this value will
    #' be removed after averaging.
    #' @param avgFun Function that is used to calculate average m/z values.
    #' @param method Method used for producing averaged MS spectra. Valid
    #' values are "hclust", used for hierarchical clustering (using the
    #' fastcluster package), and "distance", to use the between peak distance.
    #' The latter method may reduces processing time and memory requirements,
    #' at the potential cost of reduced accuracy.
    #' @param pruneMissingPrecursorMS For MS data only: if TRUE then peak lists
    #' without a precursor peak are removed. Note that even when this is set to
    #' FALSE, functionality that relies on MS (not MS/MS) peak lists (e.g.
    #' formulae calculation) will still skip calculation if a precursor is not
    #' found.
    #' @param retainPrecursorMSMS For MS/MS data only: if TRUE then always
    #' retain the precursor mass peak even if is not among the `topMost` peaks.
    #' Note that MS precursor mass peaks are always kept. Furthermore, note
    #' that precursor peaks in both MS and MS/MS data may still be removed by
    #' intensity thresholds (this is unlike the filter method function).
    #'
    get_MSPeakLists = function(useLoaded = TRUE,
                               maxMSRtWindow = 10,
                               precursorMzWindow = 4,
                               clusterMzWindow = 0.005,
                               topMost = 100,
                               minIntensityPre = 10,
                               minIntensityPost = 10,
                               avgFun = mean,
                               method = "distance",
                               retainPrecursorMSMS = TRUE) {
      
      if (!requireNamespace("patRoon", quietly = TRUE)) {
        warning("Package patRoon is not installed! Install it via https://github.com/rickhelmus/patRoon.")
        return(NULL)
      }
      
      if (!useLoaded) {
        
        if (!self$has_groups()) {
          warning("Feature groups not found! Not loaded.")
          return(NULL)
        }
        
        av_args <- list(
          clusterMzWindow = clusterMzWindow,
          topMost = topMost,
          minIntensityPre = minIntensityPre,
          minIntensityPost = minIntensityPost,
          avgFun = avgFun,
          method = method,
          pruneMissingPrecursorMS = TRUE,
          retainPrecursorMSMS = retainPrecursorMSMS
        )
        
        mspl <- patRoon::generateMSPeakLists(
          self$featureGroups,
          algorithm = "mzr",
          maxMSRtWindow = maxMSRtWindow,
          precursorMzWindow = precursorMzWindow,
          topMost = topMost,
          avgFeatParams = av_args,
          avgFGroupParams = av_args
        )
        
        return(mspl)
      }
      
      if (!any(c(self$has_loaded_features_ms1(), self$has_loaded_features_ms2()))) {
        warning("Features MS1 and/or MS2 not loaded! Not done.")
        return(NULL)
      }
      
      if (self$has_groups()) {
        
        parameters = list(
          clusterMzWindow = clusterMzWindow,
          topMost = topMost,
          minIntensityPre = minIntensityPre,
          minIntensityPost = minIntensityPost,
          avgFun = avgFun,
          method = method
        )
        
        plfinal <- .convert_ms1_ms2_columns_to_MSPeakLists(self, parameters)
        
        plfinal
        
      } else {
        warning("No feature groups found to make the MSPeakLists!")
        NULL
      }
    },

    #' @description Gets a data.table of feature isotopes (i.e., isotope clusters) in the analyses.
    #'
    get_isotopes = function(analyses = NULL,
                            groups = NULL,
                            features = NULL,
                            clusters = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            drift = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            filtered = FALSE) {

      if (is.null(features) & !is.null(groups)) {
        features <- groups
      }

      fts <- self$get_features(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (nrow(fts) == 0) return(data.table())
      
      if (!("isotope" %in% colnames(fts))) {
        warning("Isotopes not found! Run annotate_features.")
        return(data.table())
      }
      
      if (is.numeric(clusters)) {
        fts_clusters <- vapply(fts$isotope, function(x) x$cluster, NA_real_)
        fts <- fts[fts_clusters %in% clusters, ]
        
        if (nrow(fts) == 0) return(data.table())
        
        fts$cluster <- fts_clusters
        
      } else {
        fts_clusters <- vapply(fts$isotope, function(x) x$cluster, NA_real_)
        fts$cluster <- fts_clusters
      }

      all_fts <- self$get_features(filtered = TRUE)
      
      isos <- split(fts, fts$analysis)
      
      isos <- lapply(isos, function(x, all_fts) {
        i_fts <- all_fts[all_fts$analysis %in% x$analysis, ]
        i_iso <- i_fts$isotope
        sel <- vapply(i_iso, function(z) z$cluster, NA_real_) %in% x$cluster
        sel_iso <- i_iso[sel]
        sel_iso <- lapply(sel_iso, function(z) as.data.table(z))
        sel_iso <- rbindlist(sel_iso)
        sel_iso$feature <- NULL
        colnames(sel_iso) <- paste0("iso_", colnames(sel_iso))
        
        sel_fts <- i_fts[sel, ]
        
        out <- cbind(
          sel_fts[, c("analysis", "feature"), with = FALSE],
          sel_iso,
          sel_fts[, c("rt", "mass", "mz", "intensity", "rtmin", "rtmax", "mzmin", "mzmax"), with = FALSE]
        )
        
        if ("name" %in% colnames(x)) {
          tar_names <- x$name
          names(tar_names) <- as.character(x$cluster)
          out$name <- tar_names[as.character(out$iso_cluster)]
        }
        
        out
        
      }, all_fts = all_fts)
      
      isos_df <- rbindlist(isos)

      isos_df
    },

    #' @description Gets a data.table of suspects from features according to a defined database and mass (`ppm`) 
    #' and time (`sec`) deviations.
    #'
    #' @param database A data.frame with at least the columns name and mass, indicating the name and neutral 
    #' monoisotopic mass of the suspect targets.
    #'
    #' @details The `ppm` and `sec` which indicate the mass (im ppm) and time (in seconds) deviations applied during 
    #' the screening.
    #'
    get_suspects = function(analyses = NULL,
                            database = NULL,
                            features = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            drift = NULL,
                            ppm = 5,
                            sec = 10,
                            millisec = 5,
                            ppmMS2 = 10,
                            minFragments = 3,
                            isolationWindow = 1.3,
                            mzClust = 0.003,
                            presence = 0.8,
                            minIntensity = 0,
                            runParallel = FALSE,
                            filtered = FALSE,
                            onGroups = TRUE) {
      
      if (!any(self$has_features(analyses))) {
        warning("Features not found in the MassSpecEngine object!")
        return(data.table())
      }

      valid_db <- FALSE
      
      # Check if suspects are available in features
      if (is.null(database)) {
        features <- self$get_features(
          analyses,
          features,
          mass,
          mz,
          rt,
          drift,
          ppm,
          sec,
          millisec,
          filtered
        )
        
        if (nrow(features) == 0) {
          message("\U2717 Features not found for targets!")
          return(data.table())
        }
        
        features[["name"]] <- NULL
        
        if ("suspects" %in% colnames(features)) {
          
          sel <- !vapply(features$suspects, function(x) {
            if (is.null(x)) {
              TRUE
            } else if (is.data.frame(x)) {
              if (nrow(x) == 0) { TRUE } else { FALSE }
            } else { TRUE }
          }, TRUE)
          
          if (any(sel)) {
            features <- features[sel, ]
            
            suspects_l <- features[["suspects"]]
            
            suspects_l2 <- lapply(seq_len(length(suspects_l)),
              function(x, suspects_l, features) {
                temp <- suspects_l[[x]]
                temp_ft <- features[x, ]
                temp_ft[["suspects"]] <- NULL
                temp$feature <- temp_ft$feature
                temp <- merge(temp, temp_ft, by = "feature", all = TRUE)
                setcolorder(temp, colnames(temp)[2:(which(colnames(temp) %in% "analysis"))])
                temp
              },
              suspects_l = suspects_l, features = features
            )
            
            analyses_df <- rbindlist(suspects_l2, fill = TRUE)

          } else {
            warning("Suspects were not found! Run suspect_screening or give a database.")
            return(data.table())
          }
          
        } else {
          warning("Suspects were not found! Run suspect_screening or give a database.")
          return(data.table())
        }

      } else {
        
        database <- as.data.table(database)
        
        if (is.data.frame(database)) {
          database <- as.data.table(database)
          if (any(c("mass", "neutralMass") %in% colnames(database)) | "mz" %in% colnames(database)) {
            if ("name" %in% colnames(database)) {
              if ("neutralMass" %in% colnames(database)) {
                setnames(database, "neutralMass", "mass")
              }
              valid_db = TRUE
            }
          }
        }
        
        if (!valid_db) {
          warning("Argument database must be a data.frame with at least the columns name and mass or mz!")
          return(data.table())
        }
        
        if (!"rt" %in% colnames(database)) {
          database$rt <- NA_real_
          
        } else {
          database$rt[database$rt == ""] <- NA_real_
        }
        
        database$rt <- as.numeric(database$rt)
        
        analyses <- lapply(self$get_analysis_names(analyses),
          function(analysis, database, ppm, sec) {
           
            cols_db <- colnames(database)
            
            pol <- self$get_polarities(analysis)
            
            it <- seq_len(nrow(database))
            
            suspects <- lapply(it, function(x, analysis, database, ppm, sec) {

            x_rt <- database$rt[x]
            
            if ("mz" %in% cols_db) x_mz <- database$mz[x] else x_mz <- NA_real_
            
            if (!is.na(x_mz)) {
              x_mz <- database$mz[x]
              
              temp <- self$get_features(
                analyses = analysis,
                mz = x_mz,
                rt = x_rt,
                ppm = ppm,
                sec = sec,
                filtered = filtered
              )
             
            } else {
              x_mass <- database$mass[x]
              
              temp <- self$get_features(
                analyses = analysis,
                mass = x_mass,
                rt = x_rt,
                ppm = ppm,
                sec = sec,
                filtered = filtered
              )
            }
            
            if (is.na(x_rt)) x_rt <- NULL
            
            if (nrow(temp) > 0) {
             
              temp$name <- database$name[x]
              cols_front <- c("name")
              
              if ("formula" %in% colnames(database)) {
                temp$formula <- database$formula[x]
                cols_front <- c(cols_front, "formula")
              }
              
              if ("SMILES" %in% colnames(database)) {
                temp$SMILES <- database$SMILES[x]
                cols_front <- c(cols_front, "SMILES")
              }
              
              if ("mz" %in% cols_db) {
               
                temp$exp_mass <- database$mz[x]
               
                temp$error_mass <- round(
                  ((temp$mz - x_mz) / temp$mz) * 1E6, digits = 1
                )
               
              } else {
               
                temp$exp_mass <- database$mass[x]
                
                temp$error_mass <- round(
                  ((temp$mass - x_mass) / temp$mass) * 1E6, digits = 1
                )
              }
              
              cols_front <- c(cols_front, "exp_mass")
              
              temp$exp_rt <- database$rt[x]
              
              cols_front <- c(cols_front, "exp_rt")
              
              temp$id_level <- NA_character_
              
              temp$error_rt <- NA_real_
              
              temp$shared_fragments <- 0
              
              temp$fragments <- NA_character_
              
              cols_front <- c(cols_front, "id_level", "error_mass", "error_rt", "shared_fragments", "fragments")
              
              setcolorder(temp, cols_front)
              
              for (i in seq_len(nrow(temp))) {
                temp$id_level[i] = "4"
                
                if (!is.null(x_rt)) {
                  temp$id_level[i] = "3b"
                  temp$error_rt[i] = round(temp$rt[i] - x_rt, digits = 1)
                }
                
                if ("fragments" %in% colnames(database)) {
                  fragments <- database$fragments[x]
                  
                  if (!is.na(fragments)) {
                    
                    if ("ms2" %in% colnames(temp)) {
                      ms2 <- temp$ms2[[1]]
                      
                      if (is.null(ms2)) ms2 <- data.table()
                      
                    } else {
                      ms2 <- self$get_features_ms2(
                        temp$analysis,
                        temp$feature,
                        isolationWindow = isolationWindow,
                        mzClust = mzClust,
                        presence = presence,
                        minIntensity = minIntensity,
                        runParallel = runParallel
                      )
                    }
                    
                    if (nrow(ms2) > 0) {
                      fragments <- unlist(strsplit(fragments, split = "; ", fixed = TRUE))
                      fragments <- strsplit(fragments, " ")
                      fragments <- data.table(
                        "mz" = vapply(fragments, function(x) as.numeric(x[1]), NA_real_),
                        "intensity" = vapply(fragments, function(x) as.numeric(x[2]), NA_real_)
                      )
                      
                      setorder(fragments, -intensity)
                      
                      setorder(ms2, -intensity)
                      
                      fragments$intensity <- -fragments$intensity
                      
                      mzr <- fragments$mz * ppm / 1E6
                      fragments$mzmin <- fragments$mz - mzr
                      fragments$mzmax <- fragments$mz + mzr
                      
                      fragments$shared <- apply(fragments, 1, function(x) {
                        any(ms2$mz >= x[3] & ms2$mz <= x[4])
                      })
                      
                      temp$shared_fragments[i] = sum(fragments$shared)
                      
                      if (temp$shared_fragments[i] > 3) {
                        
                        temp$fragments <- database$fragments[x]
                        
                        if (temp$id_level[i] == "3b") {
                          temp$id_level[i] = "1"
                          
                        } else if (temp$id_level[i] == "4") {
                          temp$id_level[i] = "2"
                        }
                      }
                    }
                  }
                }
              }
            } else {
             temp <- data.table()
            }
            
            temp
          
          },
          analysis = analysis,
          database = database,
          ppm = ppm,
          sec = sec
        )
        
        suspects <- rbindlist(suspects, fill = TRUE)

        },
        database = database,
        ppm = ppm,
        sec = sec
        )
        
        analyses_df <- rbindlist(analyses, fill = TRUE)
        
      }
      
      if (nrow(analyses_df) > 0 && !filtered && self$has_groups() && onGroups) {
        
        if (all(!is.na(analyses_df$group))) {
          
          keep_cols <- colnames(analyses_df)
          keep_cols <- c(keep_cols[1:which(keep_cols %in% "id_level") - 1], "group")
          
          order_cols <- colnames(analyses_df)
          order_cols <- c(order_cols[1:which(order_cols %in% "shared_fragments")], "group")
          
          temp_fts <- analyses_df[, keep_cols, with = FALSE]

          analyses_df$id_level <- factor(
            analyses_df$id_level,
            levels = c("1", "2", "3a", "3b", "4"),
            ordered = TRUE
          )
          
          error_vals <- analyses_df[, .(
            id_level = min(id_level),
            error_mass = max(abs(error_mass)),
            error_rt = max(abs(error_rt)),
            shared_fragments = max(shared_fragments)
          ), by = "group"]
          
          groups_df <- self$get_groups(groups = unique(temp_fts$group),
            intensities = TRUE, average = TRUE, sdValues = FALSE, metadata = FALSE
          )
          
          groups_df <- groups_df[error_vals, on = .(group)]
          
          groups_df <- groups_df[temp_fts, on = .(group)]
          
          data.table::setkey(groups_df, group)
          
          groups_df <- groups_df[unique(group), mult = "first"]
          
          setcolorder(groups_df, order_cols)
          
          return(groups_df)
        }
      }
      
      analyses_df
    },

    #' @description Gets a data.table with internal standards found by the `find_internal_standards` module.
    #'
    #' @param average Logical of length one. When `TRUE` and groups are present, internal standards are averaged per 
    #' analysis replicate group.
    #'
    get_internal_standards = function(average = TRUE) {
      istd <- self$get_features(filtered = TRUE)

      if ("istd" %in% colnames(istd)) {
        
        sel <- !vapply(istd$istd, is.null, TRUE)

        istd <- istd[sel, ]

        if (nrow(istd) > 0) {
          
          istd_l <- istd[["istd"]]
          
          istd_l2 <- lapply(seq_len(length(istd_l)), function(x, istd_l, istd) {
            temp <- istd_l[[x]]
            temp_ft <- istd[x, ]
            temp <- cbind(temp, temp_ft)
            temp
          }, istd = istd, istd_l = istd_l)
          
          istd <- rbindlist(istd_l2, fill = TRUE)

          istd$rtr <- round(istd$rtmax - istd$rtmin, digits = 1)

          istd$mzr <- round(istd$mzmax - istd$mzmin, digits = 4)

          if ("isotope" %in% colnames(istd)) {
            istd$iso_n <- vapply(istd$isotope, function(x) x$cluster_size, 0)
            istd$iso_c <- vapply(istd$isotope, function(x) x$carbons, 0)
            
          } else {
            istd$iso_n <- NA
            istd$iso_c <- NA
          }

          if (self$has_groups() & average) {

            rpl <- self$get_replicate_names()

            istd$replicate <- rpl[istd$analysis]

            cols <- c(
              "name",
              "rt",
              "mass",
              "intensity",
              "area",
              "rtr",
              "mzr",
              "error_rt",
              "error_mass",
              "rec",
              "iso_n",
              "iso_c",
              "replicate",
              "group"
            )

            istd <- istd[, cols, with = FALSE]

            istd <- istd[, `:=`(
                freq = length(area),
                rt = round(mean(rt, na.rm = TRUE), digits = 0),
                mass = round(mean(mass, na.rm = TRUE), digits = 4),
                intensity = round(mean(intensity, na.rm = TRUE), digits = 0),
                intensity_sd = round(sd(intensity, na.rm = TRUE), digits = 0),
                area = round(mean(area, na.rm = TRUE), digits = 0),
                area_sd = round(sd(area, na.rm = TRUE), digits = 0),
                rtr = round(mean(rtr, na.rm = TRUE), digits = 1),
                rtr_sd = round(sd(rtr, na.rm = TRUE), digits = 1),
                mzr = round(mean(mzr, na.rm = TRUE), digits = 4),
                mzr_sd = round(sd(mzr, na.rm = TRUE), digits = 4),
                error_rt = round(mean(error_rt, na.rm = TRUE), digits = 1),
                error_rt_sd = round(sd(error_rt, na.rm = TRUE), digits = 1),
                error_mass = round(mean(error_mass, na.rm = TRUE), digits = 1),
                error_mass_sd = round(sd(error_mass, na.rm = TRUE), digits = 1),
                rec = round(mean(rec, na.rm = TRUE), digits = 1),
                rec_sd = round(sd(rec, na.rm = TRUE), digits = 1),
                iso_n = round(mean(iso_n, na.rm = TRUE), digits = 0),
                iso_n_sd = round(sd(iso_n, na.rm = TRUE), digits = 0),
                iso_c = round(mean(iso_c, na.rm = TRUE), digits = 0),
                iso_c_sd = round(sd(iso_c, na.rm = TRUE), digits = 0)
              ),
              by = c("name", "group", "replicate")
            ][]

            istd <- unique(istd)

            istd$rec[is.nan(istd$rec)] <- NA_real_

          } else {
            cols <- c(
              "name",
              "rt",
              "mass",
              "intensity",
              "area",
              "rtr",
              "mzr",
              "error_rt",
              "error_mass",
              "rec",
              "iso_n",
              "iso_c",
              "analysis",
              "feature"
            )

            if (self$has_groups()) cols <- c(cols, "group")

            istd <- istd[, cols, with = FALSE]
            istd$intensity <- round(istd$intensity, digits = 0)
            istd$area <- round(istd$area, digits = 0)
          }

          setorder(istd, "name")

          istd

        } else {
          warning("Internal standards not found!")
          data.table()
        }

      } else {
        warning("Not present! Run find_internal_standards method to tag the internal standards!")
        data.table()
      }
    },

    ## ___ add/update -----

    #' @description Adds analyses. Note that when adding new analyses, any existing grouping of features are removed.
    #'
    #' @param analyses A MassSpecAnalysis S3 class object or a list with MassSpecAnalysis S3 class objects as 
    #' elements (see `?MassSpecAnalysis` for more information).
    #'
    #' @return Invisible.
    #'
    add_analyses = function(analyses = NULL) {
      
      analyses <- private$.validate_list_analyses(analyses, childClass = "MassSpecAnalysis")

      if (!is.null(analyses)) {
        
        n_analyses <- self$get_number_analyses()
        
        super$add_analyses(analyses)
        
        if (self$get_number_analyses() > n_analyses) private$.remove_patRoon_data()
      }
      
      invisible(self)
    },

    #' @description Adds analyses based on mzML/mzXML files. Note that when adding new mzML/mzXML 
    #' files, any existing grouping or features are removed.
    #'
    #' @return Invisible.
    #'
    add_files = function(files = NULL, runParallel = FALSE) {

      if (!is.null(files)) {

        new_analyses <- parse_MassSpecAnalysis(files, runParallel)

        if (all(vapply(new_analyses, function(x) "MassSpecAnalysis" %in% is(x), FALSE))) {
          self$add_analyses(new_analyses)

        } else {
          warning("Not all added files could be converted as MassSpecAnalysis!")
        }

      } else {
        warning("Files were not added!")
      }

      invisible(self)
    },
    
    #' @description Adds or redefines the analysis replicate names.
    #'
    #' @param value A character vector with the analysis replicate names.
    #' Must be of the same length as the number of analyses.
    #'
    #' @return Invisible.
    #'
    add_replicate_names = function(value = NULL) {
      
      super$add_replicate_names(value)
      
      private$.remove_patRoon_data()
      
      invisible(self)
    },
    
    #' @description Adds or redefines the analysis blank replicate names.
    #'
    #' @param value A character vector with the analysis blank replicate names.
    #' Must be of the same length as the number of analyses.
    #'
    #' @return Invisible.
    #'
    add_blank_names = function(value = NULL) {
      
      super$add_blank_names(value)
      
      private$.remove_patRoon_data()
      
      invisible(self)
    },

    #' @description Adds spectra to analyses.
    #'
    #' @param spectra A data.table with spectra from MS analyses as obtained
    #' by the method `get_spectra()` with columns "scan", "mz" and "intensity".
    #' Other columns might be added from specific processing algorithms!
    #'
    #' @param replace Logical. When `TRUE`, existing spectra are replaced by
    #' the new features.
    #'
    #' @return Invisible.
    #'
    add_spectra = function(spectra = NULL, replace = TRUE) {

      valid <- FALSE

      org_analysis_names <- unname(self$get_analysis_names())

      must_have_cols <- c("scan", "mz", "intensity")

      if (is.data.frame(spectra)) {
        must_have_cols <- c("analysis", must_have_cols)

        if (all(must_have_cols %in% colnames(spectra))) {
          spectra <- spectra[order(spectra$analysis), ]
          analysis_names <- unique(spectra$analysis)

          if (all(analysis_names %in% org_analysis_names)) {
            valid <- TRUE
            split_vector <- spectra$analysis
            spectra$analysis <- NULL
            spectra <- split(spectra, split_vector)
          }

        } else {
          warning("Features data frame does not have all mandatory columns!")
        }

      } else if (is.list(spectra)) {
        analysis_names <- sort(names(spectra))

        if (all(analysis_names %in% org_analysis_names)) {
          spectra <- spectra[analysis_names]
          valid <- vapply(spectra, function(x, must_have_cols) {

            if (is.data.frame(x)) {
              if (all(must_have_cols %in% colnames(x))) {
                return(TRUE)
              }
            }
            FALSE
          }, must_have_cols = must_have_cols, FALSE)

          valid <- all(valid)
        }
      }

      if (valid) {
        n_data <- sum(vapply(spectra, function(x) nrow(x), 0))

        org_spectra <- lapply(private$.analyses, function(x) x$spectra)
        names(org_spectra) <- names(private$.analyses)

        if (replace | n_data == 0) {
          rem_run_cols <- lapply(private$.analyses, function(x) colnames(x$run))
          rem_run_cols <- unique(unname(unlist(rem_run_cols)))
          rem_run_cols <- rem_run_cols[!rem_run_cols %in% "scan"]

          spectra <- lapply(spectra, function(x, rem_run_cols) {
            if (nrow(x) == 0) return(data.table())
            keep_cols <- colnames(x)[!colnames(x) %in% rem_run_cols]
            x <- x[, keep_cols, with = FALSE]
            x
          }, rem_run_cols = rem_run_cols)

          org_spectra[names(spectra)] <- spectra

          private$.analyses <- Map(
            function(x, y) {
              x$spectra <- y
              x
            },
            private$.analyses, org_spectra
          )

          private$.register(
            "added",
            "analyses",
            "spectra",
            NA_character_,
            NA_character_,
            n_data
          )

          message("\U2713 ", n_data, " spectra added!")

        } else {
          warning("Spectra already presence and not replaced!")
        }
      } else {
        warning("Invalid spectra content or structure! Not added.")
      }
      invisible(self)
    },
    
    #' @description Adds data from results to the engine.
    #'
    #' @param value A named list with data from results.
    #'
    #' @return Invisible.
    #'
    add_results = function(value = NULL) {
      
      value_names <- names(value)
      
      if (!is.null(value_names)) {
        
        if ("patRoon" %in% value_names) {
          order_analysis <- patRoon::analyses(value[["patRoon"]]$data)
          private$.analyses <- private$.analyses[order_analysis]
        }
        
        super$add_results(value)
        
      } else {
        warning("Not done, the value must be a named list!")
      }
      
      invisible(self)
    },

    ## ___ load -----

    #' @description Loads all spectra from all analyses.
    #'
    #' @return Invisible.
    #'
    load_spectra = function(runParallel = FALSE) {
      
      spec <- self$get_spectra(
        analyses = NULL, levels = NULL, mass = NULL,
        mz = NULL, rt = NULL, ppm = 20, sec = 60, id = NULL,
        allTraces = TRUE, isolationWindow = 1.3,
        minIntensityMS1 = 0, minIntensityMS2 = 0,
        runParallel = runParallel
      )

      split_vector <- spec$analysis
      
      spec$analysis <- NULL
      
      spec_list <- split(spec, split_vector)

      if (length(spec_list) == self$get_number_analyses()) {
        
        suppressMessages(self$add_spectra(spec_list, replace = TRUE))

        private$.register(
          "loaded",
          "analyses",
          "raw spectra",
          NA_character_,
          NA_character_,
          NA_character_
        )

        message("\U2713 Spectra loaded to all analyses!")

      } else {
        warning("Not done, check the MS file paths and formats!")
      }
      invisible(self)
    },

    #' @description Loads all chromatograms from all analyses.
    #'
    #' @return Invisible.
    #'
    load_chromatograms = function(runParallel = FALSE) {

      chrom <- self$get_chromatograms(
        analyses = NULL, minIntensity = 0,
        runParallel = runParallel
      )

      if (nrow(chrom) > 0) {
        split_vector <- chrom$analysis
        chrom$analysis <- NULL
        chrom_list <- split(chrom, split_vector)

        if (length(chrom_list) == self$get_number_analyses()) {
          private$.analyses <- Map(
            function(x, y) {
              x$chromatograms <- y
              x
            },
            private$.analyses, chrom_list
          )

          private$.register(
            "loaded",
            "analyses",
            "raw chromatograms",
            NA_character_,
            NA_character_,
            NA_character_
          )

          message("\U2713 Chromatograms loaded to all analyses!")

        } else {
          warning("Not done! Chromatograms not found.")
        }

      } else {
        warning("Not done! Chromatograms not found.")
      }
      invisible(self)
    },

    ## ___ remove -----

    #' @description Removes analyses.
    #'
    #' @return Invisible.
    #'
    remove_analyses = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (!is.null(analyses)) {
        
        super$remove_analyses(analyses)
        
        if (self$has_features()) {
          analyses_left <- unname(self$get_analysis_names())
          
          if (is.null(analyses_left)) {
            private$.remove_patRoon_data()
            
          } else if (self$has_groups()) {
            self$featureGroups <- self$featureGroups[analyses_left]
            self$filtered_features <- self$filtered_features[analyses_left]
            
            if (self$has_MSPeakLists()) {
              self$MSPeakLists <- self$MSPeakLists[analyses_left]
            }
            
            if (self$has_formulas()) {
              self$formulas <- self$formulas[analyses_left]
            }
            
            if (self$has_compounds()) {
              self$compounds <- self$compounds[patRoon::groupNames(self$featureGroups)]
            }
            
          } else {
            self$features <- self$features[analyses_left]
            self$filtered_features <- self$filtered_features[analyses_left]
          }
        }
      }
      
      invisible(self)
    },
    
    #' @description Removes features. Note that feature groups are also removed when features is set to `NULL` 
    #'
    #' @return Invisible.
    #'
    remove_features = function(features = NULL, filtered = FALSE) {

      if (is.null(features) & !filtered) {

        private$.remove_patRoon_data()

        private$.register(
          "removed",
          "features",
          "all",
          NA_character_,
          NA_character_,
          NA_character_
        )

        message("\U2713 Removed all features!")

        return(invisible(self))
      }
      
      if (filtered && self$has_results("patRoon")) {
        
        n_filtered_features <- sum(vapply(private$.results$patRoon$filtered, nrow, 0))
        
        private$.results$patRoon$filtered <- lapply(private$.results$patRoon$filtered, function(x) {
          x <- x[0, ]
          x[["suspects"]] <- NULL
          x[["isotopes"]] <- NULL
          x[["ms2"]] <- NULL
          x[["ms1"]] <- NULL
          x[["istd"]] <- NULL
          x
        })
        
        private$.register(
          "removed",
          "filtered features",
          n_filtered_features,
          NA_character_,
          NA_character_,
          NA_character_
        )
        
        message("\U2713 Removed ", n_filtered_features, " feature/s!")
      }

      if (is.data.frame(features) && self$has_results("patRoon")) {
        
        if (all(c("analysis", "feature") %in% colnames(features))) {
          
          n_feat_before <- length(self$features)
          
          original_features <- self$features
          
          feature_list <- original_features@features
          
          feature_list <- lapply(names(feature_list), function(x, features, feature_list) {
            
            ft_to_rem <- features[features$analysis %in% x, ]
            
            fl <- feature_list[[x]]
            
            if (nrow(ft_to_rem) > 0) fl <- fl[!(fl$ID %in% features$feature)]
            
            fl
            
          }, features = features, feature_list = feature_list)
          
          names(feature_list) <- names(self$features@features)
          
          original_features@features <- feature_list
          
          self$features <- original_features
          
          filtered_feature_list <- self$filtered_features
          
          filtered_feature_list <- lapply(names(filtered_feature_list), function(x, features, filtered_feature_list) {
            
            ft_to_rem <- features[features$analysis %in% x, ]
            
            fl <- filtered_feature_list[[x]]
            
            if (nrow(ft_to_rem) > 0) fl <- fl[!(fl$ID %in% features$feature)]
            
            fl
            
          }, features = features, filtered_feature_list = filtered_feature_list)
          
          names(filtered_feature_list) <- names(self$filtered_features)
          
          private$.results$patRoon$filtered <- filtered_feature_list
          
          n_feat_after <- length(self$features)
          
          if (n_feat_after < n_feat_before) {
            
            private$.register(
              "removed",
              "features",
              n_feat_before - n_feat_after,
              NA_character_,
              NA_character_,
              NA_character_
            )
            
            message("\U2713 Removed ", n_feat_before - n_feat_after, " feature/s!")
            
          } else {
            message("\U2717 There are no features to remove!")
          }
          
        } else {
          warning("\U2717 Features data.frame not conform!")
        }
        
      } else {
        message("\U2717 There are no features to remove!")
      }
      
      invisible(self)
    },

    #' @description Removes loaded MS1 spectra from features in the analyses. In practice, the column \emph{ms1} in 
    #' the features data.table of each analysis object is removed.
    #'
    #' @return Invisible.
    #'
    remove_features_ms1 = function() {
      
      if (self$has_features()) {
        
        if (self$has_loaded_features_ms1()) {
          
          original_features <- self$features
          
          feature_list <- original_features@features
          
          feature_list <- lapply(feature_list, function(x) {
            x[["ms1"]] <- NULL
            x
          })
          
          names(feature_list) <- names(self$features@features)
          
          original_features@features <- feature_list
          
          self$features <- original_features
          
          filtered_feature_list <- self$filtered_features
          
          filtered_feature_list <- lapply(filtered_feature_list, function(x) {
            x[["ms1"]] <- NULL
            x
          })
          
          names(filtered_feature_list) <- names(self$filtered_features)
          
          private$.results$patRoon$filtered <- filtered_feature_list

          private$.register(
            "removed",
            "features_ms1",
            "all",
            NA_character_,
            NA_character_,
            NA_character_
          )

          message("\U2713 Removed all MS1 spectra from features!")

        } else {
          message("\U2717 Features MS1 spectra not loaded!")
        }
      } else {
        message("\U2717 Features not present!")
      }
      invisible(self)
    },

    #' @description Removes loaded MS2 spectra from features in the analyses. In practice, the column \emph{ms2} in 
    #' the features data.table of each analysis object is removed.
    #'
    #' @return Invisible.
    #'
    remove_features_ms2 = function() {
      
      if (self$has_features()) {
        
        if (self$has_loaded_features_ms2()) {
          
          original_features <- self$features
          
          feature_list <- original_features@features
          
          feature_list <- lapply(feature_list, function(x) {
            x[["ms2"]] <- NULL
            x
          })
          
          names(feature_list) <- names(self$features@features)
          
          original_features@features <- feature_list
          
          self$features <- original_features
          
          filtered_feature_list <- self$filtered_features
          
          filtered_feature_list <- lapply(filtered_feature_list, function(x) {
            x[["ms2"]] <- NULL
            x
          })
          
          names(filtered_feature_list) <- names(self$filtered_features)
          
          private$.results$patRoon$filtered <- filtered_feature_list

          private$.register(
            "removed",
            "features_ms2",
            "all",
            NA_character_,
            NA_character_,
            NA_character_
          )

          message("\U2713 Removed all MS2 spectra from features!")
        } else {
          message("\U2717 Features MS2 spectra not loaded!")
        }
      } else {
        message("\U2717 Features not present!")
      }
      invisible(self)
    },

    #' @description Removes feature groups.
    #'
    #' @return Invisible.
    #'
    remove_groups = function(groups = NULL, filtered = FALSE) {

      if (is.null(groups) & !filtered) {
        
        original_features <- self$features
        
        feature_list <- original_features@features
        
        feature_list <- lapply(feature_list, function(x) {
          x[["group"]] <- NULL
          x
        })
        
        names(feature_list) <- names(self$features@features)
        
        original_features@features <- feature_list
        
        private$.remove_patRoon_data()
        
        self$features <- original_features
        
        self$remove_features(filtered = TRUE)

        private$.register(
          "removed",
          "feature groups",
          "all",
          NA_character_,
          NA_character_,
          NA_character_
        )

        message("\U2713 Removed all groups!")

        return(invisible(self))
      }
      
      if (self$has_groups()) {
        
        if (filtered) self$remove_features(filtered = TRUE)
        
        if ((is.character(groups) || is.numeric(groups)) && length(groups) > 0) {
          
          n_groups <- length(self$featureGroups)
          
          gr_to_keep <- patRoon::groupNames(self$featureGroups)
          
          gr_to_keep <- gr_to_keep[!(gr_to_keep %in% groups)]
          
          fg <- self$featureGroups[, gr_to_keep]
          
          n_groups_after <- length(fg)
          
          self$featureGroups <- fg
          
          if (self$has_MSPeakLists()) {
            self$MSPeakLists <- self$MSPeakLists[patRoon::analyses(fg), patRoon::groupNames(fg)]
          }
          
          if (self$has_formulas()) {
            self$formulas <- self$formulas[patRoon::analyses(fg), patRoon::groupNames(fg)]
          }
          
          if (self$has_compounds()) {
            self$compounds <- self$compounds[patRoon::groupNames(fg)]
          }
          
          if (n_groups_after < n_groups) {
            
            private$.register(
              "removed",
              "feature groups",
              n_groups - n_groups_after,
              NA_character_,
              NA_character_,
              NA_character_
            )
            
            message("\U2713 Removed ", n_groups - n_groups_after, " group/s!")
          }
          
        } else {
          message("\U2717 There are no groups to remove!")
        }
      } else {
        message("\U2717 There are no groups to remove!")
      }
      
      invisible(self)
    },

    ## ___ subset -----

    #' @description Subsets a MassSpecEngine object on analyses.
    #'
    #' @return A new cloned MassSpecEngine object with only the analyses as defined by the analyses argument.
    #'
    subset_analyses = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)

      if (!is.null(analyses)) {
        
        allNames <- self$get_analysis_names()
        
        keepAnalyses <- unname(allNames[allNames %in% analyses])
        
        if (length(keepAnalyses) > 0) {
          
          newAnalyses <- self$get_analyses(keepAnalyses)
          
          new_ms <- suppressMessages(MassSpecEngine$new(
            headers = self$get_headers(),
            settings = self$get_settings(),
            analyses = newAnalyses
          ))
        
          if (self$has_features()) {
            
            if (self$has_groups()) {
              new_fg <- self$featureGroups[unname(new_ms$get_analysis_names())]
              new_ms$featureGroups <- new_fg
              
              if (self$has_MSPeakLists()) {
                new_ms$MSPeakLists <- self$MSPeakLists[patRoon::analyses(new_fg), patRoon::groupNames(new_fg)]
              }
              
              if (self$has_formulas()) {
                new_ms$formulas <- self$formulas[patRoon::analyses(new_fg), patRoon::groupNames(new_fg)]
              }
              
              if (self$has_compounds()) {
                new_ms$compounds <- self$compounds[patRoon::groupNames(new_fg)]
              }
              
            } else {
              new_ms$features <- self$features[unname(new_ms$get_analysis_names())]
            }
            
            new_ms$filtered_features <- self$filtered_features[unname(new_ms$get_analysis_names())]
          }
          
          message("\U2713 Subset with ", new_ms$get_number_analyses(), " analyses created!")
          
          return(new_ms)
          
        } else {
          warning("No analyses selected to subset! Not done.")
        }
        
        NULL
      }
    },

    #' @description Subsets a MassSpecEngine object on features from analyses.
    #'
    #' @return A new cloned MassSpecEngine object with only the features as defined by the features argument.
    #'
    subset_features = function(features = NULL) {
      
      if (is.data.frame(features)) {
        cols_must_have <- c("analysis", "feature")
        
        if (all(cols_must_have %in% colnames(features))) {
          
          all_fts <- self$get_features(filtered = TRUE)
          n_all <- nrow(all_fts)

          if (n_all > 0) {
            
            unique_fts_ids <- paste0(all_fts$analysis, all_fts$feature)
            keep_fts <- paste0(features$analysis, features$feature)
            rem_fts <- !(unique_fts_ids %in% keep_fts)
            rem_fts <- all_fts[rem_fts, ]

            if (nrow(rem_fts) > 0) {
              new_ms <- self$clone()
              new_ms <- suppressMessages(new_ms$remove_features(rem_fts, filtered = FALSE))
              message("\U2713 Subset with ", length(new_ms$features), " features created!")
              return(new_ms)

            } else {
              message("\U2717 There are no features to subset!")
            }
            
          } else {
            message("\U2717 There are no features to subset!")
          }
          
        } else {
          message("\U2717 Data.frame with analysis and feature IDs not given!")
        }
        
      } else {
        message("\U2717 Data.frame with analysis and feature IDs not given!")
      }
      
      NULL
    },

    #' @description Subsets a MassSpecEngine object on feature groups. Note that when sub-setting groups, features 
    #' that lose correspondence are removed.
    #'
    #' @return A new cloned MassSpecEngine object with only the groups as defined by the `groups` argument.
    #'
    subset_groups = function(groups = NULL) {
      
      if (self$has_groups() && (is.character(groups) || is.numeric(groups)) && length(groups) > 0) {
        
        all_fg <- patRoon::groupNames(self$featureGroups)
        rem_fg <- all_fg[!(all_fg %in% groups)]

        if (length(rem_fg) > 0) {
          new_ms <- self$clone(deep = TRUE)
          new_ms <- suppressMessages(new_ms$remove_groups(rem_fg, filtered = TRUE))
          message("\U2713 Subset with ", length(all_fg) - length(rem_fg), " feature groups created!")
          return(new_ms)

        } else {
          message("\U2717 There are no groups to subset!")
        }
        
      } else {
        message("\U2717 There are no groups to subset!")
      }
      
      NULL
    },

    ## ___ has -----

    #' @description Checks if analyses have have drift time from ion mobility, returning `TRUE` or `FALSE` for 
    #' each analysis.
    #'
    has_ion_mobility = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)

      if (is.null(analyses)) return(FALSE)

      has_im <- vapply(
        private$.analyses[analyses],
        function(x) x$has_ion_mobility, FALSE
      )

      names(has_im) <- self$get_analysis_names(analyses)

      has_im
    },

    #' @description Checks for loaded spectra in given analyses names/indices, returning `TRUE` or `FALSE` for each 
    #' analysis.
    #'
    has_loaded_spectra = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) {
        return(FALSE)
      }

      has_spectra <- vapply(
        private$.analyses[analyses],
        function(x) nrow(x$spectra) > 0, FALSE
      )

      names(has_spectra) <- self$get_analysis_names(analyses)
      has_spectra
    },

    #' @description Checks for loaded chromatograms in given analyses names/indices, returning `TRUE` or `FALSE` for 
    #' each analysis.
    #'
    has_loaded_chromatograms = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) {
        return(FALSE)
      }

      has_chromatograms <- vapply(
        private$.analyses[analyses],
        function(x) nrow(x$chromatograms) > 0, FALSE
      )

      names(has_chromatograms) <- self$get_analysis_names(analyses)
      has_chromatograms
    },

    #' @description Checks if given analyses have features, returning `TRUE` or `FALSE`.
    #'
    has_features = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(FALSE)
      
      if (self$has_results("patRoon")) {
        
        if ("features" %in% is(private$.results$patRoon$data)) {
          length(private$.results$patRoon$data[analyses]) > 0
          
        } else if ("featureGroups" %in% is(private$.results$patRoon$data)) {
          length(private$.results$patRoon$data@features[analyses]) > 0
          
        } else {
          FALSE
        }
        
      } else {
        FALSE
      }
    },
    
    #' @description Checks if there are feature groups from grouping features across analyses, returning `TRUE` 
    #' or `FALSE`.
    #'
    has_groups = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(FALSE)
      
      if (self$has_results("patRoon")) {
        
        if ("featureGroups" %in% is(private$.results$patRoon$data)) {
          length(private$.results$patRoon$data@features[analyses, ]) > 0
          
        } else {
          FALSE
        }
        
      } else {
        FALSE
      }
    },
    
    #' @description Checks for loaded features MS1 in given analyses names/indices, returning `TRUE` or `FALSE`.
    #'
    has_loaded_features_ms1 = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(FALSE)
      
      has_loaded <- any(vapply(self$features@features[analyses], function(x) {
        
        if ("ms1" %in% colnames(x)) {
          any(vapply(x$ms1, is.data.frame, FALSE))
          
        } else {
          FALSE
        }
        
      }, FALSE))
      
      has_loaded
    },

    #' @description Checks for loaded features MS2 in given analyses names/indices, returning `TRUE` or `FALSE`.
    #'
    has_loaded_features_ms2 = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(FALSE)
      
      has_loaded <- any(vapply(self$features@features[analyses], function(x) {
        
        if ("ms2" %in% colnames(x)) {
          any(vapply(x$ms2, is.data.frame, FALSE))
          
        } else {
          FALSE
        }
        
      }, FALSE))

      has_loaded
    },

    #' @description Checks for presence of feature extracted ion chromatograms (EICs) in given analyses names/indices,
    #' returning `TRUE` or `FALSE`.
    #'
    has_features_eic = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(FALSE)
      
      has_loaded <- any(vapply(self$features@features[analyses], function(x) {
        
        if ("eic" %in% colnames(x)) {
          any(vapply(x$eic, is.data.frame, FALSE))
          
        } else {
          FALSE
        }
        
      }, FALSE))
      
      has_loaded
    },
    
    #' @description Checks for presence of suspects in given analyses names/indices, returning `TRUE` or `FALSE`.
    #'
    has_suspects = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(FALSE)
      
      has_loaded <- vapply(self$features@features[analyses], function(x) {
        
        if ("suspects" %in% colnames(x)) {
          any(vapply(x$suspects, function(z) !is.null(z), FALSE))
          
        } else {
          FALSE
        }
        
      }, FALSE)
      
      has_loaded
    },
    
    #' @description Checks if there are MSPeakLists for analyses, returning `TRUE` or `FALSE`.
    #'
    has_MSPeakLists = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(FALSE)
      
      if (self$has_results("patRoon")) {
        
        if ("MSPeakLists" %in% is(private$.results$patRoon$mspl)) {
          length(private$.results$patRoon$mspl[analyses, ]) > 0
          
        } else {
          FALSE
        }
        
      } else {
        FALSE
      }
    },
    
    #' @description Checks if there are formulas assigned to feature groups, returning `TRUE` or `FALSE`.
    #'
    has_formulas = function() {
      
      if (self$has_results("patRoon")) {
        
        if ("formulas" %in% is(private$.results$patRoon$formulas)) {
          length(private$.results$patRoon$formulas) > 0
          
        } else {
          FALSE
        }
        
      } else {
        FALSE
      }
    },
    
    #' @description Checks if there are compounds assigned to feature groups, returning `TRUE` or `FALSE`.
    #'
    has_compounds = function() {
      
      if (self$has_results("patRoon")) {
        
        if ("formulas" %in% is(private$.results$patRoon$formulas)) {
          length(private$.results$patRoon$formulas) > 0
          
        } else {
          FALSE
        }
        
      } else {
        FALSE
      }
    },
    
    #' @description Checks if there are charges assigned to spectra, returning `TRUE` or `FALSE`.
    #'
    has_spectra_charges = function() {
      
      if (self$has_results("spectra")) {
        sum(vapply(private$.results$spectra$data, function(x) nrow(x$charges), 0)) > 0
        
      } else {
        FALSE
      }
    },

    ## ___ plot -----

    #' @description Plots (3D) spectra for given MS analyses.
    #'
    #' @param colorBy A string of length 1. One of `analyses` (the default), `polarities`, `levels`, `targets` or `replicates`.
    #' @param xVal Character length one. Possible values are "mz", "rt" or "drift".
    #' @param yVal Character length one. Possible values are "mz", "rt" or "drift".
    #' @param zLab A string with the title for the z axis.
    #'
    plot_spectra = function(analyses = NULL,
                            levels = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            drift = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            id = NULL,
                            allTraces = TRUE,
                            isolationWindow = 1.3,
                            minIntensityMS1 = 0,
                            minIntensityMS2 = 0,
                            runParallel = FALSE,
                            legendNames = TRUE,
                            colorBy = "analyses",
                            xVal = "rt",
                            yVal = "mz",
                            xLab = NULL,
                            yLab = NULL,
                            zLab = NULL) {

      spec <- self$get_spectra(
        analyses, levels,
        mass, mz, rt, drift, ppm, sec, millisec, id,
        allTraces = allTraces,
        isolationWindow,
        minIntensityMS1,
        minIntensityMS2,
        runParallel
      )

      if (nrow(spec) == 0) {
        message("\U2717 Traces not found for the targets!")
        return(NULL)
      }

      if ("drift" %in% c(xVal, yVal)) {
        if (!"drift" %in% colnames(spec)) {
          warning("Drift time values not found!")
          return(NULL)
        }
      }

      if ("feature" %in% colnames(spec)) spec$id <- spec$feature

      if ("replicates" %in% colorBy) {
        spec$replicate <- self$get_replicate_names()[spec$analysis]
      }

      .plot_spectra_interactive(spec, colorBy, legendNames, xVal, yVal, xLab, yLab, zLab)
    },
    
    #' @description Plots chromatograms in the analyses.
    #'
    plot_chromatograms = function(analyses = NULL,
                                  chromatograms = NA_integer_,
                                  title = NULL,
                                  colorBy = "targets",
                                  legendNames = NULL,
                                  showLegend = TRUE,
                                  xlim = NULL,
                                  ylim = NULL,
                                  cex = 0.6,
                                  interactive = TRUE) {
      
      chromatograms <- self$get_chromatograms(analyses, chromatograms)
      
      if (nrow(chromatograms) == 0) {
        message("\U2717 Chromatograms not found for the analyses!")
        return(NULL)
      }
      
      if ("replicates" %in% colorBy) {
        chromatograms$replicate <- self$get_replicate_names()[chromatograms$analysis]
      }
      
      # if (!"id" %in% colnames(chromatograms)) tic$id <- tic$analysis
      
      # polarities <- self$get_polarities()
      # polarities_names <- unique(names(polarities))
      
      pol_key <- c("positive", "negative", "nd")
      names(pol_key) <- c("1", "-1", "0")
      chromatograms$polarity <- as.character(chromatograms$polarity)
      chromatograms$polarity <- pol_key[chromatograms$polarity]
      
      if (!interactive) {
        .plot_eic_static(chromatograms, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
        NULL
      } else {
        .plot_chromatograms_interactive(chromatograms, legendNames, colorBy, title, showLegend)
      }
    },

    #' @description Plots extract ion chromatograms (EIC) and \emph{m/z} vs retention time from the analyses.
    #'
    #' @param plotTargetMark Logical, set to \code{TRUE} to plot a target mark.
    #'
    #' @param targetsMark A data.frame with columns `mz` and `rt`, defining the
    #' \emph{m/z} and retention time values of each target. Note that the number
    #'  of rows should match with the number of targets.
    #'
    #' @param ppmMark A numeric vector of length one to define the mass deviation,
    #' in ppm, of the target mark. The default is 5 ppm.
    #'
    #' @param secMark A numeric vector of length one to define the time deviation,
    #' in seconds, of the target mark. The default is 10 ppm.
    #'
    #' @param numberRows An integer vector of length one to define the number of
    #' rows to grid the plots. Note that each target is always plotted in one row
    #' for all selected analyses.
    #'
    plot_xic = function(analyses = NULL,
                        mass = NULL,
                        mz = NULL,
                        rt = NULL,
                        drift = NULL,
                        ppm = 20,
                        sec = 60,
                        millisec = 5,
                        id = NULL,
                        runParallel = FALSE,
                        legendNames = NULL,
                        plotTargetMark = TRUE,
                        targetsMark = NULL,
                        ppmMark = 5,
                        secMark = 10,
                        numberRows = 1) {

      xic <- self$get_spectra(
        analyses,
        levels = 1,
        mass,
        mz, rt, drift, ppm, sec, millisec, id,
        allTraces = TRUE,
        isolationWindow = 1.3,
        minIntensityMS1 = 0,
        minIntensityMS2 = 0,
        runParallel = runParallel
      )

      if (nrow(xic) == 0) {
        message("\U2717 Traces not found for the targets!")
        return(NULL)
      }

      .plot_xic_interactive(
        xic,
        legendNames,
        plotTargetMark,
        targetsMark,
        ppmMark,
        secMark,
        numberRows
      )
    },

    #' @description Plots extract ion chromatograms (EIC) from the analyses based on targets.
    #'
    plot_eic = function(analyses = NULL,
                        mass = NULL,
                        mz = NULL,
                        rt = NULL,
                        drift = NULL,
                        ppm = 20,
                        sec = 60,
                        millisec = 5,
                        id = NULL,
                        runParallel = FALSE,
                        legendNames = NULL,
                        title = NULL,
                        colorBy = "targets",
                        showLegend = TRUE,
                        xlim = NULL,
                        ylim = NULL,
                        cex = 0.6,
                        interactive = TRUE) {

      eic <- self$get_eic(analyses, mass, mz, rt, drift, ppm, sec, millisec, id, runParallel)

      if (nrow(eic) == 0) {
        message("\U2717 Traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        eic$replicate <- self$get_replicate_names()[eic$analysis]
      }

      if (!interactive) {
        .plot_eic_static(eic, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
      } else {
        .plot_eic_interactive(eic, legendNames, colorBy, title, showLegend)
      }
    },

    #' @description Plots the total ion chromatogram (TIC) of the analyses.
    #'
    plot_tic = function(analyses = NULL,
                        levels = c(1, 2),
                        title = NULL,
                        colorBy = "analyses",
                        legendNames = NULL,
                        showLegend = TRUE,
                        xlim = NULL,
                        ylim = NULL,
                        cex = 0.6,
                        interactive = TRUE) {

      tic <- self$get_tic(analyses, levels)

      if (nrow(tic) == 0) {
        message("\U2717 TIC not found for the analyses!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        tic$replicate <- self$get_replicate_names()[tic$analysis]
      }

      if (!"id" %in% colnames(tic)) tic$id <- tic$analysis

      # polarities <- self$get_polarities()
      # polarities_names <- unique(names(polarities))
      # 
      # if (length(polarities) > length(polarities_names) &
      #   colorBy %in% c("analyses", "levels", "replicates")) {
      # 
      #   pol_key <- c("positive", "negative", "nd")
      #   names(pol_key) <- c("1", "-1", "0")
      #   tic$polarity <- as.character(tic$polarity)
      #   tic$polarity <- pol_key[tic$polarity]
      # 
      #   if (grepl("analyses", colorBy)) {
      #     tic$analysis <- paste0(tic$analysis, "/", tic$polarity)
      #   } else if (grepl("replicates", colorBy)) {
      #     tic$replicate <- paste0(tic$replicate, "/", tic$polarity)
      #   } else {
      #     tic$level <- paste("MS", tic$level, sep = "")
      #     tic$level <- factor(tic$level)
      #     tic$level <- paste0(tic$level, "/", tic$polarity)
      #   }
      # }

      if (!interactive) {
        .plot_eic_static(tic, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
      } else {
        .plot_eic_interactive(tic, legendNames, colorBy, title, showLegend)
      }
    },

    #' @description Plots the base peak chromatogram (BPC) of the analyses.
    #'
    #' @return A plot.
    #'
    plot_bpc = function(analyses = NULL,
                        levels = c(1, 2),
                        title = NULL,
                        colorBy = "analyses",
                        legendNames = NULL,
                        showLegend = TRUE,
                        xlim = NULL,
                        ylim = NULL,
                        cex = 0.6,
                        interactive = TRUE) {

      bpc <- self$get_bpc(analyses, levels)

      if (nrow(bpc) == 0) {
        message("\U2717 BPC not found for the analyses!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        bpc$replicate <- self$get_replicate_names()[bpc$analysis]
      }

      if (!"id" %in% colnames(bpc)) bpc$id <- bpc$analysis

      # polarities <- self$get_polarities()
      # polarities_names <- unique(names(polarities))
      # 
      # if (length(polarities) > length(polarities_names) &
      #     colorBy %in% c("analyses", "levels", "replicates")) {
      # 
      #   pol_key <- c("positive", "negative", "nd")
      #   names(pol_key) <- c("1", "-1", "0")
      #   bpc$polarity <- as.character(bpc$polarity)
      #   bpc$polarity <- pol_key[bpc$polarity]
      # 
      #   if (grepl("analyses", colorBy)) {
      #     bpc$analysis <- paste0(bpc$analysis, "/", bpc$polarity)
      #   } else if (grepl("replicates", colorBy)) {
      #     bpc$replicate <- paste0(bpc$replicate, "/", bpc$polarity)
      #   } else {
      #     bpc$level <- paste("MS", bpc$level, sep = "")
      #     bpc$level <- factor(bpc$level)
      #     bpc$level <- paste0(bpc$level, "/", bpc$polarity)
      #   }
      # }

      if (!interactive) {
        .plot_eic_static(bpc, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
      } else {
        .plot_bpc_interactive(bpc, legendNames, colorBy, title, showLegend)
      }
    },

    #' @description Plots MS2 spectra from the analyses based on targets.
    #'
    plot_ms2 = function(analyses = NULL,
                        mass = NULL,
                        mz = NULL,
                        rt = NULL,
                        drift = NULL,
                        ppm = 20,
                        sec = 60,
                        millisec = 5,
                        id = NULL,
                        isolationWindow = 1.3,
                        mzClust = 0.005,
                        presence = 0.8,
                        verbose = FALSE,
                        minIntensity = 0,
                        runParallel = FALSE,
                        legendNames = NULL,
                        title = NULL,
                        colorBy = "targets",
                        interactive = TRUE) {

      ms2 <- self$get_ms2(
        analyses, mass, mz, rt, drift, ppm, sec, millisec, id, isolationWindow,
        mzClust, presence, verbose, minIntensity, runParallel
      )

      if (nrow(ms2) == 0) {
        message("\U2717 MS2 traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        ms2$replicate <- self$get_replicate_names()[ms2$analysis]
      }

      if (!interactive) {
        .plot_ms2_static(ms2, legendNames, colorBy, title)
      } else {
        .plot_ms2_interactive(ms2, legendNames, colorBy, title)
      }
    },

    #' @description Plots MS1 spectra from the analyses based on targets.
    #' 
    #' @param showText X.
    #'
    plot_ms1 = function(analyses = NULL,
                        mass = NULL,
                        mz = NULL,
                        rt = NULL,
                        drift = NULL,
                        ppm = 20,
                        sec = 60,
                        millisec = 5,
                        id = NULL,
                        mzClust = 0.003,
                        presence = 0.8,
                        verbose = FALSE,
                        minIntensity = 1000,
                        runParallel = FALSE,
                        legendNames = NULL,
                        title = NULL,
                        colorBy = "targets",
                        showText = TRUE,
                        interactive = TRUE) {

      ms1 <- self$get_ms1(
        analyses, mass, mz, rt, drift, ppm, sec, millisec, id, mzClust, presence,
        verbose, minIntensity, runParallel
      )

      if (nrow(ms1) == 0) {
        message("\U2717 MS1 traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        ms1$replicate <- self$get_replicate_names()[ms1$analysis]
      }

      if (!interactive) {
        .plot_ms1_static(ms1, legendNames, colorBy, title)
      } else {
        .plot_ms1_interactive(ms1, legendNames, colorBy, title, showText)
      }
    },

    #' @description
    #' Plots features from analyses.
    #'
    #' @return plot.
    #'
    plot_features = function(analyses = NULL,
                             features = NULL,
                             mass = NULL,
                             mz = NULL,
                             rt = NULL,
                             drift = NULL,
                             ppm = 20,
                             sec = 60,
                             millisec = 5,
                             rtExpand = 120,
                             mzExpand = NULL,
                             loaded = TRUE,
                             filtered = FALSE,
                             runParallel = FALSE,
                             legendNames = NULL,
                             title = NULL,
                             colorBy = "targets",
                             showLegend = TRUE,
                             xlim = NULL,
                             ylim = NULL,
                             cex = 0.6,
                             interactive = TRUE) {

      fts <- self$get_features(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (nrow(fts) == 0) {
        message("\U2717 Features not found for the targets!")
        return(NULL)
      }

      eic <- self$get_features_eic(
        analyses = unique(fts$analysis),
        features = fts,
        rtExpand = rtExpand,
        mzExpand = mzExpand,
        filtered = filtered,
        loaded = loaded,
        runParallel = runParallel
      )

      eic <- eic[, `:=`(intensity = sum(intensity)),
        by = c("analysis", "polarity", "feature", "rt")
      ][]

      if (nrow(eic) == 0) {
        message("\U2717 Traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        eic$replicate <- self$get_replicate_names()[eic$analysis]
      }

      if (!interactive) {
        .plot_features_static(eic, fts, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
      } else {
        .plot_features_interactive(eic, fts, legendNames, colorBy, title, showLegend)
      }
    },

    #' @description
    #' Plots a map of the retention time vs \emph{m/z} of features from analyses.
    #'
    #' @return A plot.
    #'
    map_features = function(analyses = NULL,
                            features = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            drift = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            filtered = FALSE,
                            legendNames = NULL,
                            title = NULL,
                            colorBy = "targets",
                            showLegend = TRUE,
                            xlim = 30,
                            ylim = 0.05,
                            cex = 0.6,
                            interactive = TRUE) {

      fts <- self$get_features(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (nrow(fts) == 0) {
        message("\U2717 Features not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        fts$replicate <- self$get_replicate_names()[fts$analysis]
      }

      if (!interactive) {
        .map_features_static(fts, colorBy, legendNames, title, showLegend, xlim, ylim, cex)
      } else {
        .map_features_interactive(fts, colorBy, legendNames, xlim, ylim, title)
      }
    },

    #' @description
    #' Plots MS1 spectra from features in the analyses.
    #'
    #' @return A plot.
    #'
    plot_features_ms1 = function(analyses = NULL,
                                 features = NULL,
                                 mass = NULL,
                                 mz = NULL,
                                 rt = NULL,
                                 drift = NULL,
                                 ppm = 20,
                                 sec = 60,
                                 millisec = 5,
                                 rtWindow = c(-2, 2),
                                 mzWindow = c(-5, 100),
                                 mzClust = 0.003,
                                 presence = 0.8,
                                 minIntensity = 1000,
                                 verbose = FALSE,
                                 filtered = FALSE,
                                 loadedMS1 = TRUE,
                                 runParallel = FALSE,
                                 legendNames = NULL,
                                 title = NULL,
                                 colorBy = "targets",
                                 interactive = TRUE) {

      ms1 <- self$get_features_ms1(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec,
        rtWindow, mzWindow, mzClust, presence, minIntensity,
        verbose, filtered, loadedMS1, runParallel
      )

      if (nrow(ms1) == 0) {
        message("\U2717 MS1 traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        ms1$replicate <- self$get_replicate_names()[ms1$analysis]
      }

      if (!interactive) {
        .plot_ms1_static(ms1, legendNames, colorBy, title)
      } else {
        .plot_ms1_interactive(ms1, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Plots MS2 spectra from features in the analyses.
    #'
    #' @return A plot.
    #'
    plot_features_ms2 = function(analyses = NULL,
                                 features = NULL,
                                 mass = NULL,
                                 mz = NULL,
                                 rt = NULL,
                                 drift = NULL,
                                 ppm = 20,
                                 sec = 60,
                                 millisec = 5,
                                 isolationWindow = 1.3,
                                 mzClust = 0.005,
                                 presence = 0.8,
                                 minIntensity = 0,
                                 verbose = FALSE,
                                 filtered = FALSE,
                                 loadedMS2 = TRUE,
                                 runParallel = FALSE,
                                 legendNames = NULL,
                                 title = NULL,
                                 colorBy = "targets",
                                 interactive = TRUE) {

      ms2 <- self$get_features_ms2(
        analyses, features, mass, mz, rt, drift, ppm, sec, millisec,
        isolationWindow, mzClust, presence, minIntensity,
        verbose, filtered, loadedMS2, runParallel
      )

      if (nrow(ms2) == 0) {
        message("\U2717 MS2 traces not found for the targets!")
        return(NULL)
      }
      if (grepl("replicates", colorBy)) {
        ms2$replicate <- self$get_replicate_names()[ms2$analysis]
      }

      if (!interactive) {
        .plot_ms2_static(ms2, legendNames, colorBy, title)
      } else {
        .plot_ms2_interactive(ms2, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Plots the results from the retention time alignment across analyses.
    #'
    #' @return A plot with the retention time alignment differences
    #' for each sample.
    #'
    plot_alignment = function() {
      if (!self$has_alignment()) {
        warning("\U2717 Adjusted retention time not found!")
        return(NULL)
      }

      alignment <- private$.alignment
      colors <- .get_colors(names(alignment))

      xaxis <- list(
        linecolor = toRGB("black"),
        linewidth = 2,
        title = "Retention time / seconds",
        titlefont = list(size = 12, color = "black")
      )
      yaxis <- list(
        linecolor = toRGB("black"),
        linewidth = 2,
        title = "RT<sub>Raw</sub> - RT<sub>Adjusted</sub> / seconds",
        titlefont = list(size = 12, color = "black")
      )

      plot <- plot_ly()

      for (i in names(alignment)) {
        df <- alignment[[i]]

        plot <- plot %>% add_trace(
          x = df$rt_original,
          y = df$adjustment,
          type = "scatter",
          mode = "lines",
          line = list(
            shape = "spline", width = 0.5,
            color = colors[i]
          ),
          name = i,
          legendgroup = i,
          showlegend = TRUE
        )

        df_pt <- df[!is.na(df$adjPoints), ]

        plot <- plot %>% add_trace(
          x = df_pt$adjPoints,
          y = df_pt$adjustment,
          type = "scatter",
          mode = "markers",
          marker = list(
            size = 5,
            color = colors[i]
          ),
          name = i,
          legendgroup = i,
          showlegend = FALSE
        )
      }

      plot <- plot %>% plotly::layout(
        legend = list(title = list(text = "<b> Analyses: </b>")),
        xaxis = xaxis, yaxis = yaxis
      )

      plot
    },

    #' @description
    #' Plots feature groups EIC.
    #'
    #' @return A plot.
    #'
    plot_groups = function(groups = NULL,
                           mass = NULL,
                           mz = NULL,
                           rt = NULL,
                           drift = NULL,
                           ppm = 20,
                           sec = 60,
                           millisec = 5,
                           rtExpand = 15,
                           mzExpand = 0.005,
                           filtered = FALSE,
                           runParallel = FALSE,
                           legendNames = NULL,
                           title = NULL,
                           colorBy = "targets",
                           showLegend = TRUE,
                           xlim = NULL,
                           ylim = NULL,
                           cex = 0.6,
                           interactive = TRUE) {

      fts <- self$get_features(
        analyses = NULL,
        groups, mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (grepl("targets", colorBy) & !isTRUE(legendNames)) {
        fts$name <- fts$group
        if (is.null(legendNames)) legendNames <- TRUE
      }

      self$plot_features(
        features = fts,
        rtExpand = rtExpand,
        mzExpand = mzExpand,
        filtered = filtered,
        runParallel = runParallel,
        legendNames = legendNames,
        title = title,
        colorBy = colorBy,
        showLegend = showLegend,
        xlim = xlim,
        ylim = ylim,
        cex = cex,
        interactive = interactive
      )
    },

    #' @description
    #' Plots MS1 spectra from feature groups in the analyses.
    #'
    #' @return A plot.
    #'
    plot_groups_ms1 = function(groups = NULL,
                               mass = NULL,
                               mz = NULL,
                               rt = NULL,
                               drift = NULL,
                               ppm = 20,
                               sec = 60,
                               millisec = 5,
                               rtWindow = c(-2, 2),
                               mzWindow = c(-5, 90),
                               mzClustFeatures = 0.005,
                               presenceFeatures = 0.8,
                               minIntensityFeatures = 1000,
                               loadedFeaturesMS1 = TRUE,
                               mzClust = 0.005,
                               presence = 0.8,
                               minIntensity = 1000,
                               groupBy = "groups",
                               verbose = FALSE,
                               filtered = FALSE,
                               runParallel = FALSE,
                               legendNames = NULL,
                               title = NULL,
                               colorBy = "targets",
                               interactive = TRUE) {

      if (grepl("groups", colorBy) || grepl("targets", colorBy)) {
        groupBy <- "groups"
      } else {
        groupBy <- "replicates"
      }

      ms1 <- self$get_groups_ms1(
        groups, mass, mz, rt, drift, ppm, sec, millisec,
        rtWindow, mzWindow,
        mzClustFeatures,
        presenceFeatures,
        minIntensityFeatures,
        loadedFeaturesMS1,
        mzClust,
        presence,
        minIntensity,
        groupBy,
        verbose,
        filtered,
        runParallel
      )

      if (nrow(ms1) == 0) {
        message("\U2717 MS1 traces not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) colorBy <- "replicates"
      
      if (grepl("analyses", colorBy) && grepl("targets", colorBy)) colorBy <- "replicates+targets"

      if (!interactive) {
        .plot_ms1_static(ms1, legendNames, colorBy, title)
      } else {
        .plot_ms1_interactive(ms1, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Plots MS1 spectra from feature groups in the analyses.
    #'
    #' @return A plot.
    #'
    plot_groups_ms2 = function(groups = NULL,
                               mass = NULL,
                               mz = NULL,
                               rt = NULL,
                               drift = NULL,
                               ppm = 20,
                               sec = 60,
                               millisec = 5,
                               isolationWindow = 1.3,
                               mzClustFeatures = 0.003,
                               presenceFeatures = 0.8,
                               minIntensityFeatures = 100,
                               loadedFeaturesMS2 = TRUE,
                               mzClust = 0.003,
                               presence = TRUE,
                               minIntensity = 100,
                               groupBy = "groups",
                               verbose = FALSE,
                               filtered = FALSE,
                               runParallel = FALSE,
                               legendNames = NULL,
                               title = NULL,
                               colorBy = "targets",
                               interactive = TRUE) {
      
      if (grepl("groups", colorBy) || grepl("targets", colorBy)) {
        groupBy <- "groups"
      } else {
        groupBy <- "replicates"
      }

      ms2 <- self$get_groups_ms2(
        groups, mass, mz, rt, drift, ppm, sec, millisec,
        isolationWindow,
        mzClustFeatures,
        presenceFeatures,
        minIntensityFeatures,
        loadedFeaturesMS2,
        mzClust,
        presence,
        minIntensity,
        groupBy,
        verbose,
        filtered,
        runParallel
      )

      if (nrow(ms2) == 0) {
        message("\U2717 MS2 traces not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) colorBy <- "replicates"
      
      if (grepl("analyses", colorBy) && grepl("targets", colorBy)) colorBy <- "replicates+targets"

      if (!interactive) {
        .plot_ms2_static(ms2, legendNames, colorBy, title)
      } else {
        .plot_ms2_interactive(ms2, legendNames, colorBy, title)
      }
    },

    #' @description
    #' Method to give an overview of the EIC, alignment and intensity variance
    #' from features within target feature groups.
    #'
    #' @param heights A numeric vector of length 3 to control the height of
    #' the first, second and third plot, respectively.
    #'
    #' @return A plot.
    #'
    plot_groups_overview = function(analyses = NULL,
                                    groups = NULL,
                                    mass = NULL,
                                    mz = NULL,
                                    rt = NULL,
                                    drift = NULL,
                                    ppm = 20,
                                    sec = 60,
                                    millisec = 5,
                                    rtExpand = 120,
                                    mzExpand = 0.005,
                                    loaded = TRUE,
                                    filtered = FALSE,
                                    runParallel = FALSE,
                                    legendNames = NULL,
                                    title = NULL,
                                    heights = c(0.35, 0.5, 0.15)) {

      fts <- self$get_features(analyses, groups, mass, mz, rt, drift, ppm, sec, millisec, filtered)

      eic <- self$get_features_eic(
        analyses = unique(fts$analysis),
        features = fts,
        rtExpand = rtExpand,
        mzExpand = mzExpand,
        filtered = filtered,
        loaded = loaded,
        runParallel = runParallel
      )

      eic <- eic[, `:=`(intensity = sum(intensity)),
        by = c("analysis", "polarity", "feature", "rt")
      ][]

      if (nrow(eic) == 0) {
        message("\U2717 Traces and/or features not found for targets!")
        return(NULL)
      }


      if (is.character(legendNames) & length(legendNames) == length(unique(fts$group))) {
        leg <- legendNames
        names(leg) <- unique(fts$group)
        leg <- leg[fts$group]
      } else if (isTRUE(legendNames) & "name" %in% colnames(fts)) {
        leg <- fts$name
      } else {
        leg <- fts$group
      }

      names(leg) <- paste0(fts$feature, "_", fts$analysis)
      eic$uid <- paste0(eic$feature, "_", eic$analysis)
      fts$uid <- paste0(fts$feature, "_", fts$analysis)
      eic$var <- leg[eic$uid]
      fts$var <- leg

      analyses <- private$.check_analyses_argument(analyses)

      .plot_groups_overview_aux(fts, eic, heights, analyses)
    },

    #' @description
    #' Plots the quality control assessment of internal standards.
    #'
    #' @return A plot.
    #'
    plot_internal_standards_qc = function() {

      if (self$has_groups()) {
        istd <- self$get_internal_standards(average = TRUE)
        .plot_internal_standards_qc_interactive(istd, self$get_replicate_names())
      } else {
        istd <- self$get_internal_standards(average = FALSE)
        .plot_internal_standards_qc_interactive(istd, self$get_analysis_names())
      }
    },
    
    #' @description
    #' Plots suspects.
    #' 
    #' @param database A data.frame with at least the columns name
    #' and mass, indicating the name and neutral monoisotopic
    #' mass of the suspect targets.
    #'
    #' @details The `ppm` and `sec` which indicate the
    #' mass (im ppm) and time (in seconds) deviations applied during the
    #' screening.
    #'
    #' @return A plot.
    #'
    plot_suspects = function(analyses = NULL,
                             database = NULL,
                             features = NULL,
                             mass = NULL,
                             mz = NULL,
                             rt = NULL,
                             drift = NULL,
                             ppm = 4,
                             sec = 10,
                             millisec = 5,
                             ppmMS2 = 10,
                             minFragments = 3,
                             isolationWindow = 1.3,
                             mzClust = 0.003,
                             presence = 0.8,
                             minIntensity = 0,
                             filtered = FALSE,
                             rtExpand = 120,
                             mzExpand = 0.005,
                             loaded = TRUE,
                             runParallel = FALSE,
                             colorBy = "targets") {
      
      if (any(self$has_suspects())) {
        
        suspects <- self$get_suspects(
          analyses,
          database,
          features,
          mass,
          mz,
          rt,
          drift,
          ppm,
          sec,
          millisec,
          ppmMS2,
          minFragments,
          isolationWindow ,
          mzClust,
          presence,
          minIntensity,
          runParallel,
          filtered,
          onGroups = FALSE
        )
        
        if (nrow(suspects) == 0) return(NULL)
        
        eic <- self$get_features_eic(
          analyses = unique(suspects$analysis),
          features = suspects$feature,
          rtExpand = rtExpand,
          mzExpand = mzExpand,
          filtered = filtered,
          loaded = loaded,
          runParallel = runParallel
        )
        
        eic <- eic[, `:=`(intensity = sum(intensity)),
          by = c("analysis", "polarity", "feature", "rt")
        ][]
        
        if (nrow(eic) == 0) {
          message("\U2717 Traces and/or features not found for targets!")
          return(NULL)
        }
        
        if (grepl("replicates", colorBy)) {
          eic$replicate <- self$get_replicate_names()[eic$analysis]
          suspects$replicate <- self$get_replicate_names()[suspects$analysis]
        }
        
        suspects <- .make_colorBy_varkey(suspects, colorBy, TRUE)
        
        leg <- suspects$var
        names(leg) <- paste0(suspects$feature, "_", suspects$analysis)
        eic$uid <- paste0(eic$feature, "_", eic$analysis)
        suspects$uid <- paste0(suspects$feature, "_", suspects$analysis)
        eic$var <- leg[eic$uid]
        
        .plot_suspects_interactive(suspects, eic, heights = c(0.5, 0.5))
      }
    },

    #' @description
    #' Maps isotopic clusters in the analyses.
    #'
    #' @return A plot.
    #'
    map_isotopes = function(analyses = NULL,
                            groups = NULL,
                            features = NULL,
                            clusters = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            drift = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            filtered = FALSE,
                            xlim = 30,
                            ylim = 0.05,
                            showLegend = TRUE,
                            legendNames = NULL,
                            title = NULL,
                            colorBy = "targets",
                            interactive = TRUE) {

      isotopes <- self$get_isotopes(
        analyses, groups, features, clusters,
        mass, mz, rt, drift, ppm, sec, millisec, filtered
      )

      if (nrow(isotopes) == 0) {
        message("\U2717 Feature isotopes not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        isotopes$replicate <- self$get_replicate_names()[isotopes$analysis]
      }
      
      setorder(isotopes, "analysis", "mz", "iso_cluster")
      
      if (!interactive) {
        .map_isotopes_static(
          isotopes, colorBy, legendNames,
          xlim, ylim, title, showLegend
        )
      } else {
        .map_isotopes_interactive(
          isotopes, colorBy, legendNames,
          xlim, ylim, title
        )
      }
    },
    
    #' @description Plots charge assignment of deconvoluted spectra from analyses.
    #'
    plot_spectra_charges = function(analyses = NULL,
                                    legendNames = NULL,
                                    title = NULL,
                                    colorBy = "analyses",
                                    showLegend = TRUE,
                                    xlim = NULL,
                                    ylim = NULL,
                                    cex = 0.6,
                                    xLab = NULL,
                                    yLab = NULL,
                                    interactive = TRUE) {
      
      if (!self$has_spectra_charges()) return(NULL)
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(NULL)
      
      res <- self$spectra_charges
      
      res <- rbindlist(res)
      
      if (nrow(res) > 0) {
        res$replicate <- self$get_replicate_names()[res$analysis]
      }
      
      res <- res[res$analysis %in% analyses, ]
      
      if (nrow(res) == 0) {
        message("\U2717 Spectra charges not found for the targets!")
        return(NULL)
      }

      sp_data <- self$get_results("spectra")
      sp_data <- sp_data$spectra$data
      sp_data <- sp_data[unique(res$analysis)]
      
      spec <- lapply(sp_data, function(x) x$raw)
      spec <- rbindlist(spec, fill = TRUE)
      
      if (!interactive) {
        .plot_spec_charges_static(spec, res, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
      } else {
        .plot_spec_charges_interactive(spec, res, legendNames, colorBy, title, showLegend, xLab, yLab)
      }
    },

    ## ___ processing -----

    #' @description Loads features EICs in each analyses.
    #'
    #' @return Invisible.
    #'
    load_features_eic = function(settings) {
      
      if (missing(settings)) settings <- Settings_load_features_eic_StreamFind()
      
      .dispatch_process_method("ms_load_features_eic", settings, self, private)
      
      invisible(self)
    },
    
    #' @description Loads and average MS1 spectra from features in the analyses.
    #'
    #' @return Invisible.
    #'
    load_features_ms1 = function(settings) {
      
      if (missing(settings)) settings <- Settings_load_features_ms1_StreamFind()
      
      .dispatch_process_method("ms_load_features_ms1", settings, self, private)
      
      invisible(self)
    },
    
    #' @description Loads and average MS2 spectra from features in the analyses.
    #'
    #' @return Invisible.
    #'
    load_features_ms2 = function(settings) {
      
      if (missing(settings)) settings <- Settings_load_features_ms2_StreamFind()
      
      .dispatch_process_method("ms_load_features_ms2", settings, self, private)
      
      invisible(self)
    },
    
    #' @description Loads MS1 and MS2 spectra and converts to a `MSPeakLists` object from patRoon.
    #'
    #' @return Invisible.
    #'
    load_MSPeakLists = function(settings) {
      
      if (missing(settings)) settings <- Settings_load_MSPeakLists_StreamFind()
      
      .dispatch_process_method("ms_load_MSPeakLists", settings, self, private)
      
      invisible(self)
    },
    
    #' @description Centroids profile spectra data for each MS analysis.
    #'
    #' @return Invisible.
    #'
    centroid_spectra = function(settings = NULL) {
      
      .dispatch_process_method("ms_centroid_spectra", settings, self, private)
      
      invisible(self)
    },

    #' @description Bins centroided spectra for each MS analysis.
    #'
    #' @return Invisible.
    #'
    bin_spectra = function(settings = NULL) {
      
      .dispatch_process_method("bin_spectra", settings, self, private)

      invisible(self)
    },

    #' @description Finds features (i.e. chromatographic peaks) in the spectra
    #' data of the analyses. Note, MS data structure requirements vary between
    #' the available processing algorithm for finding features.
    #'
    #' @return Invisible.
    #'
    find_features = function(settings = NULL) {
      
      .dispatch_process_method("ms_find_features", settings, self, private)

      invisible(self)
    },

    #' @description Groups and possibly aligns features across analyses.
    #'
    #' @return Invisible.
    #'
    group_features = function(settings = NULL) {

      .dispatch_process_method("ms_group_features", settings, self, private)

      invisible(self)
    },

    #' @description Filters features and feature groups according to defined settings.
    #'
    #' @return Invisible.
    #'
    #' @details The features are not entirely removed but tagged as filtered. 
    #' See columns `filtered` and `filter` of the features data.table as 
    #' obtained with the method `get_features()`.
    #'
    filter_features = function(settings = NULL) {
      
      .dispatch_process_method("ms_filter_features", settings, self, private)
      
      invisible(self)
    },

    #' @description Annotates isotopic features according to defined settings.
    #'
    #' @return Invisible.
    #'
    #' @details Extra columns are added to the features data.table in each analysis.
    #'
    annotate_features = function(settings = NULL) {
      
      .dispatch_process_method("ms_annotate_features", settings, self, private)

      invisible(self)
    },

    #' @description Finds internal standards in features according to defined settings.
    #' 
    #' @return Invisible.
    #'
    find_internal_standards = function(settings = NULL) {
      
      .dispatch_process_method("ms_find_internal_standards", settings, self, private)

      invisible(self)
    },

    #' @description Screens for suspect targets in features according to defined settings.
    #'
    suspect_screening = function(settings = NULL) {

      .dispatch_process_method("ms_suspect_screening", settings, self, private)

      invisible(self)
    },

    #' @description Calculates quality parameters of features that can be used for filtering/prioritization.
    #'
    calculate_quality = function(settings = NULL) {

      .dispatch_process_method("ms_calculate_quality", settings, self, private)

      invisible(self)
    },
    
    #' @description Generates formulas from feature groups using the package patRoon.
    #'
    #' @return Invisible.
    #'
    generate_formulas = function(settings = NULL) {
      
      .dispatch_process_method("ms_generate_formulas", settings, self, private)
      
      invisible(self)
    },
    
    #' @description Generates compounds from feature groups using the package patRoon.
    #'
    #' @return Invisible.
    #'
    generate_compounds = function(settings = NULL) {
      
      .dispatch_process_method("ms_generate_compounds", settings, self, private)
      
      invisible(self)
    },
    
    #' @description Integrates chromatograms, returning a peak list for each chromatogram for each analyses.
    #'
    #' @return Invisible.
    #'
    integrate_chromatograms = function(settings = NULL) {
      
      .dispatch_process_method("integrate_chromatograms", settings, self, private)
      
      invisible(self)
    },
    
    #' @description Deconvolutes spectra with multi-charged compounds, such as monoclonal antibodies.
    #'
    #' @return Invisible.
    #'
    deconvolute_spectra_charges = function(settings = NULL) {
      
      .dispatch_process_method("ms_deconvolute_spectra_charges", settings, self, private)
      
      invisible(self)
    },
    
    
    ## ___ as -----

    #' @description
    #' Creates an object with S4 class `features` from the package \pkg{patRoon}
    #' with the features in the analyses.
    #'
    #' @return An object with S4 class `features`.
    #'
    as_patRoon_features = function(filtered = FALSE) {

      if (!requireNamespace("patRoon", quietly = TRUE)) {
        warning("Package patRoon is not installed! Install it via https://github.com/rickhelmus/patRoon.")
        return(NULL)
      }

      if (self$get_number_analyses() == 0) {
        warning("There are no MS analyses!")
        return(NULL)
      }

      if (!any(self$has_features())) {
        warning("There are no features in the MS analyses!")
        return(NULL)
      }

      anaInfo <- self$get_overview()

      anaInfo <- data.frame(
        "path" = dirname(anaInfo$file),
        "analysis" = anaInfo$analysis,
        "group" = anaInfo$replicate,
        "blank" = anaInfo$blank
      )

      anaInfo$blank[is.na(anaInfo$blank)] <- ""

      multiple_polarities <- FALSE

      polarities <- self$get_polarities()

      if (length(polarities) > self$get_number_analyses()) {
        stop("Multiple polarities detected within one file! Not permited in patRoon currently.")
      }

      if (length(unique(polarities)) > 1) {
        multiple_polarities <- TRUE
        anaInfo$set <- polarities
      }

      anaInfo$file <- self$get_files()

      features <- lapply(self$get_analyses(), function(x, without_filtered_features) {

        ft <- copy(x$features)

        if (without_filtered_features) {
          if ("filtered" %in% colnames(ft)) ft <- ft[!ft$filtered, ]
        }

        setnames(ft, c("feature", "rt", "rtmin", "rtmax"),
          c("ID", "ret", "retmin", "retmax"),
          skip_absent = TRUE
        )

        setcolorder(
          ft,
          c(
            "ID", "mz", "mzmin", "mzmax", "ret", "retmin", "retmax",
            "intensity", "area", "polarity"
          )
        )

        ft <- ft[order(ft$mz), ]
        ft <- ft[order(ft$ret), ]
        ft$index <- seq_len(nrow(ft))

        ft
      }, without_filtered_features = !filtered)

      remove_empty <- !vapply(features, function(x) nrow(x) == 0, FALSE)
      
      features <- features[remove_empty]
      
      anaInfo <- anaInfo[remove_empty, ]
      
      rownames(anaInfo) <- seq_len(nrow(anaInfo))
      
      if (multiple_polarities) {

        pol_key <- c("[M+H]+", "[M-H]-", "[M]")
        names(pol_key) <- c("1", "-1", "0")

        features <- lapply(features, function(x, pol_key) {

          if (nrow(x) == 0) {
            x$mass <- NULL
            x[, adduct := character(.N)]
            return(x)
          }

          x$mzmin <- x$mass - (x$mz - x$mzmin)
          x$mzmax <- x$mass + (x$mzmax - x$mz)
          x$mz <- x$mass
          x$mass <- NULL

          pol_char <- as.character(x$polarity)
          x$adduct <- pol_key[pol_char]

          x
        }, pol_key = pol_key)

        features_obj <- new("featuresSet",
          features = features, analysisInfo = anaInfo,
          algorithm = "openms-set"
        )

      } else {
        features_obj <- new("featuresOpenMS",
          features = features, analysisInfo = anaInfo
        )
      }

      return(features_obj)
    },

    #' @description
    #' Creates an object with S4 class `featureGroups` from the package
    #' \pkg{patRoon} with the features in the analyses and feature groups.
    #' 
    #' 
    #'
    #' @return An object with S4 class `featureGroups`.
    #'
    as_patRoon_featureGroups = function(filtered = FALSE, addSuspects = TRUE) {

      if (!requireNamespace("patRoon", quietly = TRUE)) {
        warning("Package patRoon is not installed! Install it via https://github.com/rickhelmus/patRoon.")
        return(NULL)
      }

      if (self$get_number_analyses() == 0) {
        warning("There are no MS analyses!")
        return(NULL)
      }

      if (!any(self$has_features())) {
        warning("There are no features in the MS analyses!")
        return(NULL)
      }

      if (!self$has_groups()) {
        warning("No feature groups found!")
        return(NULL)
      }

      pat_features <- self$as_patRoon_features(filtered)

      features <- pat_features@features

      n_analyses <- length(features)

      groups <- self$get_groups(
        filtered = filtered,
        intensities = TRUE,
        average = FALSE,
        sdValues = FALSE,
        metadata = TRUE
      )
      
      analyses <- self$get_analysis_names()[self$get_analysis_names() %in% colnames(groups)]
      
      groups_info <- copy(groups)
      groups_cols <- groups$group
      groups_ints <- groups[, analyses, with = FALSE]
      groups_trans <- data.table::transpose(groups_ints)
      colnames(groups_trans) <- groups_cols

      groups_info <- groups_info[, 1:3]
      groups_info_rows <- groups_info$group
      groups_info[["group"]] <- NULL
      groups_info <- as.data.frame(groups_info)
      rownames(groups_info) <- groups_info_rows
      colnames(groups_info) <- c("mzs", "rts")
      # Note that here the mzs is still neutral mass
      
      fts_idx <- self$get_features(filtered = filtered)
      fts_idx$new_index <- stats::ave(seq_along(fts_idx$analysis), fts_idx$analysis, FUN = seq_along)
      fts_idx <- fts_idx[, .(index = new_index), by = c("group", "analysis")]
      fts_idx <- tidyr::spread(fts_idx, key = analysis, value = index, fill = 0)
      group_cols <- fts_idx$group
      fts_idx$group <- NULL
      fts_idx <- t(fts_idx)
      colnames(fts_idx) <- group_cols
      ftindex <- as.data.table(fts_idx)
      
      add_suspects_to_pat <- FALSE
      
      if (addSuspects && any(self$has_suspects())) {
        suspects_df <- self$get_suspects(onGroups = FALSE)
        
        cols_keep <- colnames(suspects_df)
        cols_keep <- c(cols_keep[1:(which(cols_keep %in% "analysis") - 1)], "group", "polarity")
        suspects_df <- suspects_df[, cols_keep, with = FALSE]
        
        pol_key <- c("positive", "negative", "nd")
        names(pol_key) <- c("1", "-1", "0")
        suspects_df$polarity <- as.character(suspects_df$polarity)
        suspects_df$polarity <- pol_key[suspects_df$polarity]
        
        setnames(suspects_df, c("exp_rt", "exp_mass"), c("rt", "neutralMass"), skip_absent = TRUE)
        
        suspects_df$error_mass <- suspects_df$error_mass * suspects_df$neutralMass / 1E6
        
        suspects_df_av <- data.table::copy(suspects_df)
        suspects_df_av <- suspects_df_av[, `:=`(d_rt = max(abs(error_rt)), d_mz = max(abs(error_mass))), by = list(group)]
        
        suspects_df_av[["error_mass"]] <- NULL
        suspects_df_av[["error_rt"]] <- NULL
        
        cols_group_by <- colnames(suspects_df_av)
        cols_group_by <- cols_group_by[!cols_group_by %in% "polarity"]
        
        polarity <- NULL
        
        suspects_df_av <- suspects_df_av[, polarity := paste(unique(polarity), collapse = ","), by = cols_group_by]
        
        suspects_df_av <- unique(suspects_df_av)
        
        setcolorder(suspects_df_av, c("name", "group"))
        
        setnames(suspects_df_av, "polarity", "sets")
        
        setnames(suspects_df, c("error_rt", "error_mass"), c("d_rt", "d_mz"), skip_absent = TRUE)
        
        setcolorder(suspects_df, c("name", "group"))
        
        add_suspects_to_pat <- TRUE
      }

      if (TRUE %in% grepl("Set", is(pat_features))) {
        polarity <- c("[M+H]+", "[M-H]-", "[M]")
        names(polarity) <- c("1", "-1", "0")

        polarity_set <- c("positive", "negative", "not defined")
        names(polarity_set) <- c("1", "-1", "0")

        neutralMasses <- groups_info$mzs
        names(neutralMasses) <- groups_cols

        annotations_entry <- data.table::rbindlist(pat_features@features, fill = TRUE)
        annotations_entry$neutralMass <- neutralMasses[annotations_entry$group]
        polarity_column <- as.character(annotations_entry$polarity)
        annotations_entry$adduct <- polarity[polarity_column]
        annotations_entry$set <- polarity_set[polarity_column]

        cols_to_keep <- c("set", "group", "adduct", "neutralMass")
        annotations_entry <- annotations_entry[, cols_to_keep, with = FALSE]
        annotations_entry <- unique(annotations_entry)
        
        pat <- new("featureGroupsSet",
          groups = groups_trans,
          analysisInfo = pat_features@analysisInfo,
          groupInfo = groups_info,
          features = pat_features,
          ftindex = ftindex,
          groupQualities = data.table(),
          groupScores = data.table(),
          annotations = annotations_entry,
          ISTDs = data.table(),
          ISTDAssignments = list(),
          concentrations = data.table(),
          toxicities = data.table(),
          algorithm = "openms-set"
        )
        
        if (add_suspects_to_pat) {
          
          pat_positive <- patRoon::unset(pat, "positive")
          suspects_df_pos <- suspects_df[suspects_df$polarity %in% "positive", ]
          if (nrow(suspects_df_pos) > 0) {
            suspects_df_pos <- suspects_df_pos[, `:=`(d_rt = max(abs(d_rt)), d_mz = max(abs(d_mz))), by = list(group)]
            suspects_df_pos <- unique(suspects_df_pos)
          }
          
          pat_negative <- patRoon::unset(pat, "negative")
          suspects_df_neg <- suspects_df[suspects_df$polarity %in% "negative", ]
          if (nrow(suspects_df_neg) > 0) {
            suspects_df_neg <- suspects_df_neg[, `:=`(d_rt = max(abs(d_rt)), d_mz = max(abs(d_mz))), by = list(group)]
            suspects_df_neg <- unique(suspects_df_neg)
          }

          pat_positive_sus <- new("featureGroupsScreening",
            screenInfo = suspects_df_pos,
            groups = pat_positive@groups,
            analysisInfo = pat_positive@analysisInfo,
            groupInfo = pat_positive@groupInfo,
            features = pat_positive@features,
            ftindex = pat_positive@ftindex,
            groupQualities = data.table(),
            groupScores = data.table(),
            annotations = pat_positive@annotations,
            ISTDs = data.table(),
            ISTDAssignments = list(),
            concentrations = data.table(),
            toxicities = data.table()
          )
          
          pat_negative_sus <- new("featureGroupsScreening",
            screenInfo = suspects_df_neg,
            groups = pat_negative@groups,
            analysisInfo = pat_negative@analysisInfo,
            groupInfo = pat_negative@groupInfo,
            features = pat_negative@features,
            ftindex = pat_negative@ftindex,
            groupQualities = data.table(),
            groupScores = data.table(),
            annotations = pat_negative@annotations,
            ISTDs = data.table(),
            ISTDAssignments = list(),
            concentrations = data.table(),
            toxicities = data.table()
          )
          
          pat_sus <- new("featureGroupsScreeningSet",
            screenInfo = suspects_df_av,
            groups = groups_trans,
            analysisInfo = pat_features@analysisInfo,
            groupInfo = groups_info,
            features = pat_features,
            ftindex = ftindex,
            annotations = annotations_entry,
            ISTDs = data.table(),
            ISTDAssignments = list(),
            concentrations = data.table(),
            toxicities = data.table(),
            setObjects = list(
              "positive" = pat_positive_sus,
              "negative" = pat_negative_sus
            )
          )
          
          return(pat_sus)
        }
        
        pat

      } else {

        polarity <- unique(self$get_polarities())

        if (length(polarity) > 1) {
          stop("Multiple polarities detected but Features is not a set!")
        }

        if (polarity %in% "positive") {
          groups_info$mzs <- groups_info$mzs + 1.00726
        } else if (polarity %in% "negative") {
          groups_info$mzs <- groups_info$mzs - 1.00726
        } else {
          stop("Polarity should be defined as positive or negative!")
        }

        pat <- new("featureGroupsOpenMS",
          groups = groups_trans,
          analysisInfo = pat_features@analysisInfo,
          groupInfo = groups_info,
          features = pat_features,
          ftindex = ftindex
        )
        
        if (add_suspects_to_pat) {
          suspects_df_av[["sets"]] <- NULL

          pat_sus <- new("featureGroupsScreening",
            screenInfo = suspects_df_av,
            groups = pat@groups,
            analysisInfo = pat@analysisInfo,
            groupInfo = pat@groupInfo,
            features = pat@features,
            ftindex = pat@ftindex,
            groupQualities = data.table(),
            groupScores = data.table(),
            annotations = pat@annotations,
            ISTDs = data.table(),
            ISTDAssignments = list(),
            concentrations = data.table(),
            toxicities = data.table()
          )
          
          return(pat_sus)
        }
        
        pat
      }
    },

    #' @description
    #' Creates an object with S4 class `MSPeakLists` from the package
    #' \pkg{patRoon} with MS and MSMS data from features in the analyses.
    #' Note that feature groups are required. The MS and MSMS spectra of each
    #' feature are then average by \pkg{patRoon} to produce the feature group
    #' spectra using the parameters of the function
    #' \link[patRoon]{getDefAvgPListParams}. The arguments described below are
    #' taken from the documentation available in \pkg{patRoon}.
    #'
    #' @param clusterMzWindow m/z window (in Da) used for clustering m/z values
    #' when spectra are averaged. For method="hclust" this corresponds to the
    #' cluster height, while for method="distance" this value is used to find
    #' nearby masses (+/- window). Too small windows will prevent clustering
    #' m/z values (thus erroneously treating equal masses along spectra as
    #' different), whereas too big windows may cluster unrelated m/z values
    #' from different or even the same spectrum together.
    #' @param topMost Only retain this maximum number of MS peaks when generating
    #' averaged spectra. Lowering this number may exclude more irrelevant (noisy)
    #' MS peaks and decrease processing time, whereas higher values may avoid
    #' excluding lower intense MS peaks that may still be of interest.
    #' @param minIntensityPre MS peaks with intensities below this value will
    #' be removed (applied prior to selection by `topMost`) before averaging.
    #' @param minIntensityPost MS peaks with intensities below this value will
    #' be removed after averaging.
    #' @param avgFun Function that is used to calculate average m/z values.
    #' @param method Method used for producing averaged MS spectra. Valid
    #' values are "hclust", used for hierarchical clustering (using the
    #' fastcluster package), and "distance", to use the between peak distance.
    #' The latter method may reduces processing time and memory requirements,
    #' at the potential cost of reduced accuracy.
    #' @param pruneMissingPrecursorMS For MS data only: if TRUE then peak lists
    #' without a precursor peak are removed. Note that even when this is set to
    #' FALSE, functionality that relies on MS (not MS/MS) peak lists (e.g.
    #' formulae calculation) will still skip calculation if a precursor is not
    #' found.
    #' @param retainPrecursorMSMS For MS/MS data only: if TRUE then always
    #' retain the precursor mass peak even if is not among the `topMost` peaks.
    #' Note that MS precursor mass peaks are always kept. Furthermore, note
    #' that precursor peaks in both MS and MS/MS data may still be removed by
    #' intensity thresholds (this is unlike the filter method function).
    #'
    #' @return An object with S4 class `MSPeakLists`.
    #'
    as_patRoon_MSPeakLists = function(filtered = FALSE,
                                      clusterMzWindow = 0.005,
                                      topMost = 100,
                                      minIntensityPre = 50,
                                      minIntensityPost = 50,
                                      avgFun = mean,
                                      method = "hclust",
                                      retainPrecursorMSMS = TRUE) {

      if (!requireNamespace("patRoon", quietly = TRUE)) {
        warning("Package patRoon is not installed! Install it via https://github.com/rickhelmus/patRoon.")
        return(NULL)
      }
      
      if (self$has_groups()) {

        pruneMissingPrecursorMS = FALSE

        correct_spectrum <- function(s, t, out) {

          if (length(s) > 1) s <- s[1]

          names(s) <- t

          if (!is.null(s[[1]])) {
            n_traces <- nrow(s[[1]])

            if (n_traces > 0) {
              s[[1]][["id"]] <- seq_len(n_traces)

              if (!"is_pre" %in% colnames(s[[1]])) {
                s[[1]][["is_pre"]] <- rep(FALSE, n_traces)
              }

              cols_to_keep <- c("id", "mz", "intensity", "is_pre")
              s[[1]] <- s[[1]][, cols_to_keep, with = FALSE]

              colnames(s[[1]]) <- c("ID", "mz", "intensity", "precursor")
            }
          }

          out <- c(out, s)

          out
        }

        plist <- lapply(self$get_analyses(), function(x, filtered, correct_spectrum) {

          features <- x$features

          if (!filtered) features <- features[!features$filtered, ]

          groups <- unique(features$group)
          groups <- groups[!is.na(groups)]

          glist <- lapply(groups, function(x2, features, correct_spectrum) {
            out <- list()

            MS <- features$ms1[features$group %in% x2]

            if (length(MS) > 1) MS <- MS[1]

            if (!is.null(MS[[1]])) {

              if (!"is_pre" %in% colnames(MS[[1]])) {

                t_mz_min <- features$mzmin[features$group %in% x2]
                t_mz_max <- features$mzmax[features$group %in% x2]

                MS[[1]][["is_pre"]] <- vapply(MS[[1]][["mz"]],
                  function(x, t_mz_min, t_mz_max) {
                    x >= t_mz_min & x <= t_mz_max
                  }, t_mz_min = t_mz_min,
                  t_mz_max = t_mz_max,
                  FALSE
                )
              }
            }

            MSMS <- features$ms2[features$group %in% x2]

            out <- correct_spectrum(MS, "MS", out)

            out <- correct_spectrum(MSMS, "MSMS", out)

            out

          }, features = features, correct_spectrum = correct_spectrum)

          names(glist) <- groups

          glist = glist[order(names(glist))]

          glist
        }, filtered = filtered, correct_spectrum = correct_spectrum)

        names(plist) <- self$get_analysis_names()

        plist <- plist[vapply(plist, function(x) length(x) > 0, FALSE)]



        mlist <- lapply(self$get_analyses(), function(x, filtered) {

          features <- x$features

          run <- x$run

          pol_col <- as.character(run$polarity)
          pol_key = c(1, 0, -1)
          names(pol_key) <- c("1", "-1", "0")
          run$polarity <- pol_key[pol_col]

          setnames(run,
            c("index", "level", "ce", "pre_mz"),
            c("seqNum", "msLevel", "collisionEnergy", "precursorMZ"),
            skip_absent = TRUE
          )

          if (!filtered) features <- features[!features$filtered, ]

          groups <- unique(features$group)
          groups <- groups[!is.na(groups)]

          glist <- lapply(groups, function(x2, features, run) {
            out <- list()

            ft <- features[features$group %in% x2, ]

            if (nrow(ft) > 0) {
              MS <- run[run$rt >= ft$rtmin &
                          run$rt <= ft$rtmax &
                            run$msLevel == 1, ]

              if (nrow(MS) > 0) out[["MS"]] <- MS

              MSMS <- run[run$rt >= ft$rtmin &
                            run$rt <= ft$rtmax &
                              run$precursorMZ >= ft$mzmin - 1.3/2 &
                                run$precursorMZ <= ft$mzmax + 1.3/2 &
                                  run$msLevel == 2, ]

              if (nrow(MSMS) > 0) out[["MSMS"]] <- MSMS
            }

            out

          }, features = features, run = run)

          names(glist) <- groups

          glist = glist[order(names(glist))]

          glist
        }, filtered = filtered)

        names(mlist) <- self$get_analysis_names()

        mlist <- mlist[vapply(mlist, function(x) length(x) > 0, FALSE)]

        groups <- self$get_groups(
          filtered = filtered,
          intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = FALSE
        )

        # group_names <- unique(groups$group)
        #
        # group_names = group_names[order(group_names)]
        #
        # aplist <- lapply(seq_len(nrow(groups)), function(x, groups, correct_spectrum) {
        #
        #   out <- list()
        #
        #   MS <- groups$ms1[x]
        #
        #   MSMS <- groups$ms2[x]
        #
        #   out <- correct_spectrum(MS, "MS", out)
        #
        #   out <- correct_spectrum(MSMS, "MSMS", out)
        #
        #   out
        #
        # }, groups = groups, correct_spectrum = correct_spectrum)
        #
        # names(aplist) <- groups$group
        #
        # aplist <- aplist[vapply(aplist, function(x) length(x) > 0, FALSE)]
        #
        # aplist = aplist[order(names(aplist))]

        settings <- self$get_settings("load_groups_ms2")

        parameters <- settings$load_groups_ms2$parameters

        pat_param <- list(
          "clusterMzWindow" = clusterMzWindow,
          "topMost" = topMost,
          "minIntensityPre" = minIntensityPre,
          "minIntensityPost" = minIntensityPost,
          "avgFun" = avgFun,
          "method" = method,
          "pruneMissingPrecursorMS" = pruneMissingPrecursorMS,
          "retainPrecursorMSMS" = retainPrecursorMSMS
        )

        if ("mzClust" %in% names(parameters)) {
          pat_param$clusterMzWindow <- parameters$mzClust
        }

        if ("minIntensity" %in% names(parameters)) {
          pat_param$minIntensityPre <- parameters$minIntensity
          pat_param$minIntensityPost <- parameters$minIntensity
        }

        plfinal <- new("MSPeakLists",
          peakLists = plist,
          metadata = mlist,
          # averagedPeakLists = aplist,
          avgPeakListArgs = pat_param,
          origFGNames = groups$group,
          algorithm = "mzr"
        )

        # plfinal@averagedPeakLists <- aplist

        plfinal

      } else {
        warning("No feature groups found to make the MSPeakLists S4 object!")
      }
    },

    ## ___ check -----

    #' @description Checks the correspondence of features within feature groups, returning `TRUE` or `FALSE`.
    #'
    check_correspondence = function() {
      valid <- FALSE
      
      if (all(self$has_features()) & self$has_groups()) {
        valid <- rcpp_ms_groups_correspondence(
          groups = self$get_groups(intensities = TRUE, average = FALSE, metadata = TRUE),
          features = self$get_features(),
          verbose = TRUE
        )
      }
      
      valid
    },

    ## ___ save -----

    #' @description
    #' Saves the private fields (i.e., headers, settings, analyses,
    #' groups and alignment) of the MassSpecEngine object.
    #'
    #' @return Saves the private fields of the msdata as the defined `format`
    #' in the \code{path} and returns invisible.
    #'
    save = function(format = "json", name = "MassSpecEngine", path = getwd()) {

      if (format %in% "json") {
        list_all <- list()

        headers <- private$.headers
        settings <- private$.settings
        analyses <- private$.analyses
        history <- private$.history
        results <- private$.results

        if (length(headers) > 0) list_all$headers <- headers
        if (!is.null(settings)) list_all$settings <- settings
        if (!is.null(analyses)) list_all$analyses <- analyses
        if (!is.null(history)) list_all$history <- history
        if (!is.null(results)) list_all$results <- results

        js_all <- toJSON(
          list_all,
          dataframe = "columns",
          Date = "ISO8601",
          POSIXt = "string",
          factor = "string",
          complex = "string",
          null = "null",
          na = "null",
          auto_unbox = FALSE,
          digits = 8,
          pretty = TRUE,
          force = TRUE
        )

        write(js_all, file = paste0(path, "/", name, ".", "json"))
      }

      if (format %in% "rds") saveRDS(self, file = paste0(path, "/", name, ".rds"))

      invisible(self)
    },

    ## ___ import -----

    #' @description Imports a MassSpecEngine object saved as \emph{json}.
    #'
    #' @param file A \emph{json} file representing a `MassSpecEngine` object.
    #'
    #' @return Invisible.
    #'
    import = function(file = NA_character_) {

      if (file.exists(file)) {
        
        if (file_ext(file) %in% "json") {
          js_ms <- fromJSON(file, simplifyDataFrame = FALSE)

          fields_present <- names(js_ms)

          if ("headers" %in% fields_present) self$add_headers(js_ms[["headers"]])

          if ("settings" %in% fields_present) {
            if (!is.null(js_ms[["settings"]])) {
              self$add_settings(js_ms[["settings"]], replace = TRUE)
            }
          }

          if ("analyses" %in% fields_present) {
            if (!is.null(js_ms[["analyses"]])) {
              self$add_analyses(js_ms[["analyses"]])
            }
          }

          if ("history" %in% fields_present) {
            private$.history <- js_ms[["history"]]
          }

          if ("results" %in% fields_present) {
            private$.results <- js_ms[["results"]]
          }
        }
        
      } else {
        warning("File not found in given path!")
        NULL
      }

      invisible(self)
    },

    ## ___ report -----

    #' @description
    #' Saves the HTML report from the function \link[patRoon]{report} from the
    #' package \pkg{patRoon}. The interface is exactly the same and the
    #' arguments description are taken from the documentation in \pkg{patRoon}.
    #' Therefore, for further information, we recommend to consult directly the
    #' function \link[patRoon]{report} in \pkg{patRoon}.
    #'
    #' @param settingsFile The path to the report settings file used for report
    #' configuration (see Report settings in \link[patRoon]{report}).
    #' @param EICParams A named list with parameters used for extracted ion
    #' chromatogram (EIC) creation. See \link[patRoon]{getDefEICParams}.
    #' @param specSimParams A named list with parameters that influence the
    #' calculation of MS spectra similarities. See \link[patRoon]{getDefSpecSimParams}.
    #' @param clearPath If TRUE then the report destination path will be
    #' (recursively) removed prior to reporting.
    #' @param openReport If set to TRUE then the output report file will be
    #' opened with the system browser.
    #' @param parallel If set to TRUE then code is executed in parallel.
    #' @param overrideSettings A list with settings that override those from
    #' the report settings file. See \link[patRoon]{report}.
    #'
    #' @return An interactive HTML report from the package \pkg{patRoon}.
    #'
    report = function(path = paste0(getwd(), "/report"),
                      filtered = FALSE,
                      settingsFile = system.file("report", "settings.yml", package = "patRoon"),
                      EICParams = patRoon::getDefEICParams(topMost = 1, topMostByRGroup = TRUE),
                      specSimParams = patRoon::getDefSpecSimParams(),
                      clearPath = FALSE,
                      openReport = TRUE,
                      parallel = TRUE,
                      overrideSettings = list()) {

      if (!requireNamespace("patRoon", quietly = TRUE)) {
        return(invisible(self))
      }

      if (self$get_number_analyses() == 0) {
        warning("There are no MS analyses!")
        return(invisible(self))
      }

      if (!self$has_groups()) {
        warning("No feature groups found!")
        return(invisible(self))
      }

      if (!(any(self$has_loaded_features_ms2()) | any(self$has_loaded_features_ms1()))) {
        warning("MS or MSMS spectra for features not loaded!")
        # TODO add possibility to generate using patRoon
        MSPeakLists <- NULL

      } else {
        MSPeakLists <- self$as_patRoon_MSPeakLists(filtered)
      }

      patRoon::report(
        self$featureGroups,
        MSPeakLists,
        formulas = self$formulas,
        compounds = self$compounds,
        compsCluster = NULL,
        components = NULL,
        TPs = NULL,
        settingsFile = settingsFile,
        path = path,
        EICParams = EICParams,
        specSimParams = specSimParams,
        clearPath = clearPath,
        openReport = openReport,
        parallel = parallel,
        overrideSettings = overrideSettings
      )

      invisible(self)
    },

    ## ___ info -----

    ### ___ processing_function_calls -----

    #' @description A data.table with available data processing methods.
    #'
    processing_methods = function() {
      
      data.table(
        name = c(
          "centroid_spectra",
          "bin_spectra",
          "find_features",
          "annotate_features",
          "load_features_eic",
          "load_features_ms1",
          "load_features_ms2",
          "load_MSPeakLists",
          "group_features",
          "fill_features",
          "filter_features",
          "suspect_screening",
          "find_internal_standards",
          "calculate_quality",
          "generate_formulas",
          "generate_compounds",
          "integrate_chromatograms",
          "deconvolute_spectra_charges"
        ),
        max = c(
          1,
          1,
          1,
          1,
          1,
          1,
          1,
          1,
          1,
          1,
          Inf,
          1,
          1,
          1,
          1,
          1,
          1,
          1
        )
      )
    },

    ### ___ help -----

    #' @field help (`list()`)\cr
    #' List of function elements to access specific reference help pages.
    help = list(
      methods = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/MassSpecEngine.html#methods")
      },
      get_groups = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/MassSpecEngine.html#method-MassSpecEngine-get_groups")
      },
      get_features = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/MassSpecEngine.html#method-MassSpecEngine-get_features")
      },
      settings_centroid_spectra = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#centroid-spectra")
      },
      settings_bin_spectra = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#bin-spectra")
      },
      settings_find_features = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#find-features")
      },
      settings_annotate_features = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#annotate-features")
      },
      settings_load_features_eic = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#load-features-eic")
      },
      settings_load_features_ms1 = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#load-features-ms1")
      },
      settings_load_features_ms2 = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#load-features-ms2")
      },
      settings_group_features = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#group-features")
      },
      settings_fill_features = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#fill-features")
      },
      settings_filter_features = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#filter-features")
      },
      settings_suspect_screening = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#suspect-screening")
      },
      settings_find_internal_standards = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#find-internal-standards")
      },
      settings_calculate_quality = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/index.html#calculate-quality")
      }
    )
  )
)

# _ import MassSpecEngine class -----

#' Function to import a MassSpecEngine class object from a *json* or *rds* file.
#'
#' @description Function to import a `MassSpecEngine` class object from a saved
#' *json* or *rds* file.
#'
#' @template arg-import-file
#'
#' @return A `MassSpecEngine` class object.
#'
#' @export
#'
import_MassSpecEngine <- function(file) {

  if (file.exists(file)) {
    if (file_ext(file) %in% "json") {

      new_ms <- MassSpecEngine$new()
      new_ms$import(file)
      message("\U2713 MassSpecEngine class object imported from json file!")
    }

    if (file_ext(file) %in% "rds") {
      new_ms <- readRDS(file)
      # TODO is it important to validate object
      message("\U2713 MassSpecEngine class object imported from rds file!")
    }

    new_ms

  } else {
    warning("File not found in given path!")
    NULL
  }
}


#' Function to combine MassSpecEngine class objects.
#'
#' @param combineFeatureLists Logical, set to TRUE to combine feature lists.
#' @param ... *MassSpecEngine* class object.
#'
#' @return A *MassSpecEngine* class object.
#'
combine_MassSpecEngine <- function(combineFeatureLists = TRUE, ...) {

  combined_analyses <- list()

  for (obj in list(...)) {

    if (inherits(obj, "MassSpecEngine")) {
      combined_analyses <- c(combined_analyses, obj$get_analyses())
    }

    # if duplicated files, and cflists is TRUE {
    #  each duplicated files return grouped feature list.
    #  how to combine, using the first group_features settings?
    #}
  }

  return(MassSpecEngine$new(analyses = combined_analyses))
}

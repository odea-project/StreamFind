#' **MassSpecEngine** R6 class and methods
#'
#' @description The MassSpecEngine R6 class is a framework for parsing, processing, inspecting and storing 
#' mass spectrometry (MS) data. The MassSpecEngine is using \href{https://github.com/rickhelmus/patRoon}{patRoon} for 
#' assembly of Non-Target Screening data processing workflows.
#'
#' @template arg-ms-files
#' @template arg-headers
#' @template arg-results
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-mass
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
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-chromatograms
#' @template arg-ms-features
#' @template arg-ms-filtered
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-ms-rtWindow
#' @template arg-ms-mzWindow
#' @template arg-ms-loaded
#' @template arg-ms-groups
#' @template arg-ms-intensities
#' @template arg-ms-average
#' @template arg-ms-mzClustFeatures
#' @template arg-ms-presenceFeatures
#' @template arg-ms-minIntensityFeatures
#' @template arg-ms-groupBy
#' @template arg-ms-clusters
#' @template arg-ms-ppmMS2
#' @template arg-ms-minFragments
#' @template arg-ms-onGroups
#' @template arg-legendNames
#' @template arg-ms-colorBy
#' @template arg-labs
#' @template arg-interactive
#' @template arg-cex
#' @template arg-title
#' @template arg-showLegend
#' @template arg-xlim-ylim
#' @template arg-showText
#' @template arg-settings
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
        anaInfo <- private$.results$patRoon$data@analysisInfo
        anaInfo <- anaInfo[anaInfo$analysis %in% self$get_analysis_names(), ]
        anaInfo
        
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
            
            if (identical(self$analysisInfo$analysis, value@analysisInfo$analysis)) {
              
              if ("features" %in% is(private$.results$patRoon$data)) {
                
                if (all(unname(self$get_analysis_names()) %in% names(value@features))) {
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
              
              pols <- self$get_spectra_polarity()
              
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
                list(
                  "patRoon" = list(
                    "data" = value,
                    "filtered" = filtered,
                    "mspl" = NULL,
                    "formulas" = NULL,
                    "compounds" = NULL,
                    "software" = "patRoon",
                    "version" = as.character(packageVersion("patRoon"))
                  )
                )
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
        
        if (self$has_results("patRoon")) {
          
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
              
              pol <- self$get_spectra_polarity(x)
              
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
    
    #' @field chromatograms `data.table` with processed chromatograms for each analyses.
    #' 
    chromatograms = function(value) {
      
      if (missing(value)) {
        
        if (self$has_results("chromatograms")) {
          res <- private$.results$chromatograms
          res <- lapply(res, function(x) x$chromatograms)
          
        } else {
          res <- lapply(self$get_analyses(), function(x) x$chromatograms)
          names(res) <- self$get_analysis_names()
          res <- Map(function(x, y) {
            if (is.data.table(x)) {
              if (nrow(x) > 0) {
                x$analysis <- y
                setcolorder(x, "analysis")
              }
            }
            
            x
          }, res, names(res))
        }
        
        res
        
      } else {
        
        if (self$has_results("chromatograms")) {
          
          if (identical(names(private$.results$chromatograms), names(value))) {
            private$.results$chromatograms <- Map(function(x, y) {
              x$chromatograms <- y
              x
            }, private$.results$chromatograms, value)
            
          } else {
            private$.results[["chromatograms"]] <- Map(function(x, y) {
              x <- list("chromatograms" = y)
              x
            }, names(value), value)
          }
          
        } else {
          private$.results[["chromatograms"]] <- Map(function(x, y) {
            x <- list("chromatograms" = y)
            x
          }, names(value), value)
        }
        
        invisible(self)
      }
    },
    
    #' @field chromatograms_peaks `data.table` with integrated peaks from chromatograms for each analyses.
    #' 
    chromatograms_peaks = function(value) {
      
      if (missing(value)) {
        
        if (self$has_chromatograms_peaks()) {
          pks <- private$.results$chromatograms
          pks <- lapply(pks, function(x) x$peaks)
          
          pks
          
        } else {
          data.table()
        }
        
      } else {
        
        if (self$has_results("chromatograms")) {
          
          if (identical(names(private$.results$chromatograms), names(value))) {
            private$.results$chromatograms <- Map(function(x, y) {
              x$peaks <- y
              x
            }, private$.results$chromatograms, value)
          }
        }
        
        invisible(self)
      }
    },
    
    #' @field spectra List of spectra `data.table` objects for each analysis.
    #'
    spectra = function(value) {
      
      if (missing(value)) {
        
        if (self$has_results("spectra")) {
          res <- private$.results$spectra
          res <- lapply(res, function(x) x$spectra)
          
        } else {
          res <- lapply(self$get_analyses(), function(x) x$spectra)
          names(res) <- self$get_analysis_names()
          res <- Map(function(x, y) {
            if (is.data.table(x)) {
              if (nrow(x) > 0) {
                x$analysis <- y
                setcolorder(x, "analysis")
              }
            }
            x
          }, res, names(res))
        }
        
        res
        
      } else {
        
        if (self$has_results("spectra")) {
          
          if (identical(names(private$.results$spectra), names(value))) {
            private$.results$spectra <- Map(function(x, y) {
              x$spectra <- y
              x
            }, private$.results$spectra, value)
            
          } else {
            private$.results[["spectra"]] <- Map(function(x, y) {
              x <- list("spectra" = y)
              x
            }, names(value), value)
          }
          
        } else {
          private$.results[["spectra"]] <- Map(function(x, y) {
            x <- list("spectra" = y)
            x
          }, names(value), value)
        }
        
        invisible(self)
      }
    },
    
    #' @field spectra_charges `data.table` with charges assigned to spectra for each analyses.
    #' 
    spectra_charges = function(value) {
      
      if (missing(value)) {
        
        if (self$has_spectra_charges()) {
          res <- private$.results$spectra
          res <- lapply(res, function(x) x$charges)
          
          res
          
        } else {
          data.table()
        }
        
      } else {
        
        if (self$has_results("spectra")) {
          
          if (identical(names(private$.results$spectra), names(value))) {
            private$.results$spectra <- Map(function(x, y) {
              x$charges <- y
              x
            }, private$.results$spectra, value)
          }
        }
        
        invisible(self)
      }
    },
    
    #' @field deconvoluted_spectra List of deconvoluted spectra for each analysis.
    #' 
    deconvoluted_spectra = function(value) {
      
      if (missing(value)) {
        
        if (self$has_deconvoluted_spectra()) {
          res <- private$.results$spectra
          res <- lapply(res, function(x) x$deconvoluted)
          
          res
          
        } else {
          data.table()
        }
        
      } else {
        
        if (self$has_results("spectra")) {
          
          if (identical(names(private$.results$spectra), names(value))) {
            private$.results$spectra <- Map(function(x, y) {
              x$deconvoluted <- y
              x
            }, private$.results$spectra, value)
          }
        }
        
        invisible(self)
      }
    },
    
    #' @field spectra_peaks `data.table` with integrated spectra peaks for each analysis.
    #' 
    spectra_peaks = function() {
      
      if (self$has_spectra_peaks()) {
        pks <- private$.results$spectra$data
        pks <- lapply(pks, function(x) x$peaks)
        pks <- rbindlist(pks)
        
        if (nrow(pks) > 0) pks$replicate <- self$get_replicate_names()[pks$analysis]
        
        pks
        
      } else {
        data.table()
      }
    }
  ),

  # _ public fields -----
  public = list(

    #' @description Creates an R6 MassSpecEngine class object.
    #'
    #' @param settings A named list of ProcessingSettings objects or a single ProcessingSettings object. The list 
    #' names should match the call name of each ProcessingSettings object. Alternatively, a named list with call name, 
    #' algorithm and parameters to be transformed and added as ProcessingSettings object.
    #'
    #' @param analyses A MassSpecAnalysis S3 class object or a list with MassSpecAnalysis S3 class objects as elements.
    #' (see `?MassSpecAnalysis` for more information).
    #'
    #' @return A new MassSpecEngine class object.
    #'
    initialize = function(files = NULL, headers = NULL, settings = NULL, analyses = NULL, results = NULL) {
      
      if (is.list(analyses)) {
        
        if (is(analyses, "MassSpecAnalysis")) analyses <- list(analyses)
        
        if (!all(vapply(analyses, function(x) is(x, "MassSpecAnalysis"), FALSE))) {
          warning("The argument analyses must be a MassSpecAnalysis object or a list of MassSpecAnalysis objects! Not done.")
          analyses <- NULL
        }
      }
      
      super$initialize(headers, settings, analyses, results)
      
      if (!is.null(files)) self$add_files(files)

      private$.register("created", "MassSpecEngine", headers$name, paste(c(headers$author, headers$path), collapse = ", "))
    },

    ## ___ get -----

    #' @description Gets an overview data.frame of all the analyses.
    #'
    get_overview = function() {
      
      if (length(private$.analyses) > 0) {
        
        ov <- super$get_overview()
        
        ov$type <- vapply(private$.analyses, function(x) x$type, "")
        
        pols <- self$get_spectra_polarity()
        
        ov$polarity <- vapply(names(private$.analyses), function(x) paste(pols[x], collapse = "; "), "")
        
        ov$spectra <- vapply(private$.analyses, function(x) round(x$spectra_number, digits = 0), 0)
        
        if (any(self$get_chromatograms_number() > 0)) {
          ov$chromatograms <- vapply(private$.analyses, function(x) x$chromatograms_number, 0)
        }
        
        if (self$has_features()) {
          ov$features <- vapply(self$features@features, function(x) nrow(x[!x$filtered, ]), 0)
          
          # TODO  get_overview add more details, such s suspects, isotopes, etc.
        }
        
        ov$file <- vapply(private$.analyses, function(x) x$file, NA_character_)

        row.names(ov) <- seq_len(nrow(ov))
        
        ov
        
      } else {
        data.frame()
      }
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
    
    #' @description Gets the instrument information of each analysis.
    #'
    #' @return A list.
    #'
    get_instrument_info = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())
      value <- lapply(private$.analyses[analyses], function(x) {
        x$instrument
      })
      value
    },
    
    #' @description Gets the software information of each analysis.
    #'
    #' @return A list.
    #'
    get_software_info = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.table())
      value <- lapply(private$.analyses[analyses], function(x) {
        x$software
      })
      value
    },

    #' @description Gets the number of spectra in each analysis.
    #'
    #' @return A character vector.
    #'
    get_spectra_number = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "spectra_number")
    },

    #' @description Gets the spectra mode of each analysis (i.e., profile or centroid).
    #'
    #' @return A character vector.
    #'
    get_spectra_mode = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(character())
      
      value <- lapply(private$.analyses[analyses], function(x) { unique(x$spectra_headers$mode) })
      
      value <- Map(function(x, y) {
        x[x == 0] <- "unkown"
        x[x == 1] <- "profile"
        x[x == 2] <- "centroid"
        names(x) <- rep(y, length(x))
        x
      }, value, analyses)
      
      names(value) <- NULL
      
      value <- unlist(value)
      
      value
    },

    #' @description Gets the spectra levels of each analysis.
    #'
    #' @return A list for each analysis with an integer vector.
    #'
    get_spectra_level = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(character())
      
      value <- lapply(private$.analyses[analyses], function(x) { unique(x$spectra_headers$level) })
      
      value <- Map(function(x, y) {
        names(x) <- rep(y, length(x))
        x
      }, value, analyses)
      
      names(value) <- NULL
      
      value <- unlist(value)
      
      value
    },

    #' @description Gets the lower \emph{m/z} value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_spectra_lowest_mz = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(character())
      
      value <- lapply(private$.analyses[analyses], function(x) { unique(x$spectra_headers$lowmz) })
      
      value <- Map(function(x, y) {
        names(x) <- rep(y, length(x))
        x
      }, value, analyses)
      
      names(value) <- NULL
      
      value <- unlist(value)
      
      value
    },

    #' @description Gets the higher \emph{m/z} value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_spectra_highest_mz = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(character())
      
      value <- lapply(private$.analyses[analyses], function(x) { unique(x$spectra_headers$highmz) })
      
      value <- Map(function(x, y) {
        names(x) <- rep(y, length(x))
        x
      }, value, analyses)
      
      names(value) <- NULL
      
      value <- unlist(value)
      
      value
    },

    #' @description Gets the start retention time value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_spectra_lowest_rt = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(character())
      
      value <- lapply(private$.analyses[analyses], function(x) { min(x$spectra_headers$rt) })
      
      value <- Map(function(x, y) {
        names(x) <- rep(y, length(x))
        x
      }, value, analyses)
      
      names(value) <- NULL
      
      value <- unlist(value)
      
      value
    },

    #' @description Gets the end retention time value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_spectra_highest_rt = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(character())
      
      value <- lapply(private$.analyses[analyses], function(x) { max(x$spectra_headers$rt) })
      
      value <- Map(function(x, y) {
        names(x) <- rep(y, length(x))
        x
      }, value, analyses)
      
      names(value) <- NULL
      
      value <- unlist(value)
      
      value
    },

    #' @description Gets the polarity of each analysis.
    #'
    #' @return A character vector.
    #'
    get_spectra_polarity = function(analyses = NULL) {

      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(character())
      
      value <- lapply(private$.analyses[analyses], function(x) { unique(x$spectra_headers$polarity) })
      
      value <- Map(function(x, y) {
        x[x == 0] <- "unkown"
        x[x == 1] <- "positive"
        x[x == -1] <- "negative"
        names(x) <- rep(y, length(x))
        x
      }, value, analyses)
      
      names(value) <- NULL
      
      value <- unlist(value)
      
      value

      # if (length(polarities) > length(analyses)) {
      #   message("\U2139 Multiple polarities detected in each analysis! Some find_features algorithms cannot handled multiple polarities properly.")
      # }

      # if (length(polarities) > length(analyses)) {
      #
      #   polarities <- vapply(private$.analyses[analyses], function(x) {
      #     spectra_headers <- x$spectra_headers
      #     polarity <- spectra_headers$polarity
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
      #       per_pos_pol <- round((scans_pos / nrow(spectra_headers)) * 100, digits = 0)
      #       warning("Multiple polarities detected but positive polarity is present in ", per_pos_pol, "% of the spectra! Advisable to remove data from negative ionization." )
      #       return("positive")
      #
      #     } else {
      #       per_neg_pol <- round((scans_neg / nrow(spectra_headers)) * 100, digits = 0)
      #       warning("Multiple polarities detected but negative polarity is present in ", per_neg_pol, "% of the spectra! Advisable to remove data from positive ionization." )
      #       return("negative")
      #     }
      #   }, "")
      #
      #   names(polarities) <- analyses
      # }
    },

    #' @description Gets the spectra headers data.table of each analysis.
    #'
    #' @return A data.table.
    #'
    get_spectra_headers = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(data.table())
      
      value <- lapply(private$.analyses[analyses], function(x) { x$spectra_headers })
      
      value <- rbindlist(value, idcol = "analysis", fill = TRUE)
      
      value
    },

    #' @description Gets the total ion chromatogram (TIC) of each analysis.
    #' 
    #' @param rt Numeric (length 2). The retention time range to filter the TIC.
    #'
    #' @return A data.table with the TIC chromatogram.
    #'
    get_spectra_tic = function(analyses = NULL, levels = c(1, 2), rt = NULL) {

      analyses <- private$.check_analyses_argument(analyses)

      if (is.null(analyses)) return(data.table())

      tic <- lapply(private$.analyses[analyses], function(x) {
        data.table(
          "polarity" = x$spectra_headers$polarity,
          "level" = x$spectra_headers$level,
          "rt" = x$spectra_headers$rt,
          "intensity" = x$spectra_headers$tic
        )
      })

      tic <- rbindlist(tic, idcol = "analysis", fill = TRUE)

      tic <- tic[tic$level %in% levels, ]
      
      if (!is.null(rt)) {
        if (length(rt) == 2 && is.numeric(rt)) {
          rt <- sort(rt)
          sel <- tic$rt >= rt[1] & tic$rt <= rt[2]
          tic <- tic[sel, ]
        }
      }

      tic
    },

    #' @description Gets the base peak chromatogram (BPC) of each analysis.
    #' 
    #' @param rt Numeric (length 2). The retention time range to filter the BPC.
    #'
    #' @return A character vector.
    #'
    get_spectra_bpc = function(analyses = NULL, levels = c(1, 2), rt = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)

      if (is.null(analyses)) return(data.table())

      bpc <- lapply(private$.analyses[analyses], function(x) {
        data.table(
          "polarity" = x$spectra_headers$polarity,
          "level" = x$spectra_headers$level,
          "rt" = x$spectra_headers$rt,
          "mz" = x$spectra_headers$bpmz,
          "intensity" = x$spectra_headers$bpint
        )
      })

      bpc <- rbindlist(bpc, idcol = "analysis", fill = TRUE)

      bpc <- bpc[bpc$level %in% levels, ]
      
      if (!is.null(rt)) {
        if (length(rt) == 2 && is.numeric(rt)) {
          rt <- sort(rt)
          sel <- bpc$rt >= rt[1] & bpc$rt <= rt[2]
          bpc <- bpc[sel, ]
        }
      }

      bpc
    },

    #' @description Gets spectra from each analysis.
    #' 
    #' @param raw_spectra Logical of length one. Set to `TRUE` for parsing raw spectra not spectra results/processed.
    #' @param loaded_spectra Logical of length one. Set to `TRUE` for parsing loaded spectra not raw data files.
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
                           raw_spectra = FALSE,
                           loaded_spectra = TRUE) {

      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(data.table())

      if (!any(is.numeric(minIntensityMS1) | is.integer(minIntensityMS1))) minIntensityMS1 <- 0

      if (!any(is.numeric(minIntensityMS2) | is.integer(minIntensityMS2))) minIntensityMS2 <- 0
      
      polarities <- unique(self$get_spectra_polarity(analyses))
      
      if (!is.null(mass)) {

        if (is.data.frame(mass)) if ("mass" %in% colnames(mass)) mass$mz <- mass$mass

        neutral_targets <- make_ms_targets(mass, rt, drift, ppm, sec, millisec, id)

        targets <- list()

        for (i in polarities) {

          if (i %in% "positive") {
            temp_tar <- copy(neutral_targets)
            temp_tar$mz <- temp_tar$mz + 1.00726
            temp_tar$mzmax <- temp_tar$mzmax + 1.00726
            temp_tar$mzmin <- temp_tar$mzmin + 1.00726
            temp_tar$polarity <- as.numeric(rep(1, nrow(temp_tar)))
            targets[[length(targets) + 1]] <- temp_tar

          } else if (i %in% "negative") {
            temp_tar <- copy(neutral_targets)
            temp_tar$mz <- temp_tar$mz - 1.00726
            temp_tar$mzmax <- temp_tar$mzmax - 1.00726
            temp_tar$mzmin <- temp_tar$mzmin - 1.00726
            temp_tar$polarity <- as.numeric(rep(-1, nrow(temp_tar)))
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
        
        if (TRUE %in% is.na(targets$mz)) targets$mz[is.na(targets$mz)] <- 0
        
        if (TRUE %in% is.na(targets$mzmax)) targets$mzmax[is.na(targets$mzmax)] <- max(self$get_spectra_highest_mz(analyses))
        
        if (TRUE %in% is.na(targets$mzmin)) targets$mzmin[is.na(targets$mzmin)] <- min(self$get_spectra_lowest_mz(analyses))
        
        if (TRUE %in% (targets$mzmax == 0)) targets$mzmax[targets$mzmax == 0] <- max(self$get_spectra_highest_mz(analyses))
        
        if (TRUE %in% is.na(targets$rt)) targets$rt[is.na(targets$rt)] <- 0
        
        if (TRUE %in% is.na(targets$rtmax)) targets$rtmax[is.na(targets$rtmax)] <- max(self$get_spectra_highest_rt(analyses))
        
        if (TRUE %in% is.na(targets$rtmin)) targets$rtmin[is.na(targets$rtmin)] <- min(self$get_spectra_lowest_rt(analyses))

        if (TRUE %in% (targets$rtmax == 0)) targets$rtmax[targets$rtmax == 0] <- max(self$get_spectra_highest_rt(analyses))
        
        if (TRUE %in% is.na(targets$drift)) targets$drift[is.na(targets$drift)] <- 0
        
        if (TRUE %in% is.na(targets$driftmax) & any(self$has_ion_mobility())) targets$driftmax[is.na(targets$driftmax)] <- max(self$get_spectra_headers(analyses)[["drift"]], na.rm = TRUE)
        
        if (TRUE %in% is.na(targets$driftmin) & any(self$has_ion_mobility())) targets$driftmin[is.na(targets$driftmin)] <- min(self$get_spectra_headers(analyses)[["drift"]], na.rm = TRUE)

        if (TRUE %in% (targets$driftmax == 0) & any(self$has_ion_mobility())) targets$driftmax[targets$driftmax == 0] <- max(self$get_spectra_headers(analyses)[["drift"]], na.rm = TRUE)
      
      } else {
        targets <- targets[0, ]
      }
      
      if (is.null(levels)) levels <- unique(self$get_spectra_level(analyses))
      
      if (!2 %in% levels) allTraces <- TRUE

      if (!is.logical(allTraces)) allTraces <- TRUE
      
      if (nrow(targets) > 0) {
        
        targets$polarity <- as.numeric(targets$polarity)
        
        targets$precursor <- FALSE
        
        if (!allTraces) {
          
          if (!any(is.numeric(isolationWindow) | is.integer(isolationWindow))) isolationWindow <- 0
          
          targets$precursor <- TRUE
          
          targets$mzmin <- targets$mzmin - (isolationWindow / 2)
          
          targets$mzmax <- targets$mzmax + (isolationWindow / 2)
          
          # TODO make case for DIA when pre_mz is not available
        }
      }
      
      #__________________________________________________________________
      # Extracts spectra results 
      #__________________________________________________________________
      if (self$has_spectra() && !raw_spectra) {
        
        spec <- rbindlist(self$spectra, fill = TRUE)
        
        if ("analysis" %in% colnames(spec)) {
          spec <- spec[spec$analysis %in% analyses, ]
          
        } else if ("replicate" %in% colnames(spec)) {
          rpl <- self$get_replicate_names()
          rpl <- rpl[analyses]
          spec <- spec[spec$replicate %in% unname(rpl)]
          
          if (!"analysis" %in% colnames(spec)) {
            spec$analysis <- spec$replicate
            setcolorder(spec, c("analysis", "replicate"))
          }
        }
        
        return(spec)
        
      #__________________________________________________________________
      # Extracts spectra loaded 
      #__________________________________________________________________
      } else if (all(self$has_loaded_spectra(analyses)) && loaded_spectra) {
        
        spec_list <- lapply(self$get_analyses(analyses), function(x) {
          
          temp <- x$spectra

          with_im <- any(temp$drift > 0)

          if (!is.null(levels)) temp <- temp[temp$level %in% levels, ]

          if (nrow(targets) > 0) {
            
            if ("analysis" %in% colnames(targets)) targets <- targets[targets$analysis %in% x$name, ]
            
            if (nrow(targets) > 0) {
              
              if ("polarity" %in% colnames(targets)) temp <- temp[temp$polarity == targets$polarity, ]
              
              if (nrow(targets) > 0) {
                temp <- .trim_spectra_targets(temp, targets, with_im)
                
              } else {
                temp <- data.table()
              }
            } else {
              temp <- data.table()
            }
          }

          if (with_im) temp$drift <- NULL
          
          if (!"analysis" %in% colnames(temp)) temp$analysis <- x$name
          
          if (!"replicate" %in% colnames(temp)) temp$replicate <- x$replicate
          
          temp <- temp[!(temp$intensity <= minIntensityMS1 & temp$level == 1), ]
          
          temp <- temp[!(temp$intensity <= minIntensityMS2 & temp$level == 2), ]

          temp
          
        })
      
      #__________________________________________________________________
      # Extracts spectra from raw data 
      #__________________________________________________________________
      } else {
        
        spec_list <- lapply(self$get_analyses(analyses), function(x, levels, targets) {
          
          cache <- .load_chache("parsed_ms_spectra", x$file, levels, targets, minIntensityMS1, minIntensityMS2)
          
          if (!is.null(cache$data)) {
            message("\U2139 Spectra loaded from cache!")
            return(cache$data)
          }
          
          message("\U2699 Parsing spectra from ", basename(x$file), "...", appendLF = FALSE)
          
          if ("analysis" %in% colnames(targets)) targets <- targets[targets$analysis %in% x$name, ]

          spec <- rcpp_parse_ms_spectra(x, levels, targets, minIntensityMS1, minIntensityMS2)
          
          message(" Done!")
          
          if (nrow(spec) == 0) return(data.table())
          
          if ("id" %in% colnames(spec)) {
            setorder(spec, id, rt, mz)
          } else {
            setorder(spec, rt, mz)
          }
          
          if (!any(spec$drift > 0)) spec$drift <- NULL
          
          if (!"analysis" %in% colnames(spec)) spec$analysis <- x$name
          
          if (!"replicate" %in% colnames(spec)) spec$replicate <- x$replicate
          
          if (!is.null(cache$hash)) {
            .save_cache("parsed_ms_spectra", spec, cache$hash)
            message("\U1f5ab Parsed spectra cached!")
          }
          
          gc()
          
          spec
          
        }, levels = levels, targets = targets)
      }

      if (length(spec_list) == length(analyses)) {
        
        spec <- rbindlist(spec_list, fill = TRUE)
        
        if (nrow(spec) > 0) setcolorder(spec, c("analysis", "replicate"))

        spec

      } else {
        warning("Defined analyses not found!")
        data.table()
      }
    },
    
    #' @description Gets a matrix with spectra from analyses.
    #' 
    get_spectra_matrix = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      mat <- self$get_spectra(analyses)
      
      if (nrow(mat) == 0) return(matrix())
      
      mat <- mat[order(mat$analysis), ]
      
      if ("bins" %in% colnames(mat)) {
        col_key <- unique(mat$bins)
      } else if ("drift" %in% colnames(mat)) {
        col_key <- unique(paste0("r", mat$rt, "_m", mat$mz, "_d", mat$drift, "_p", mat$polarity, "_l", mat$level))
      } else {
        col_key <- unique(paste0("r", mat$rt, "_m", mat$mz, "_p", mat$polarity, "_l", mat$level))
      }
      
      matrix(
        mat$intensity,
        nrow = length(unique(mat$analysis)),
        ncol = length(col_key),
        byrow = TRUE,
        dimnames = list(as.character(unique(mat$analysis)), as.character(col_key))
      )
    },
    
    #' @description Gets spectra extract ion chromatograms (EIC) from the analyses based on targets as a data.table.
    #'
    get_spectra_eic = function(analyses = NULL,
                               mass = NULL,
                               mz = NULL,
                               rt = NULL,
                               drift = NULL,
                               ppm = 20,
                               sec = 60,
                               millisec = 5,
                               id = NULL) {
      
      eic <- self$get_spectra(
        analyses, levels = 1,
        mass, mz, rt, drift, ppm, sec, millisec, id,
        allTraces = TRUE,
        isolationWindow = 1.3,
        minIntensityMS1 = 0,
        minIntensityMS2 = 0,
        raw_spectra = TRUE,
        loaded_spectra = TRUE
      )
      
      if (nrow(eic) > 0) {
        eic <- as.data.table(eic)
        if (!"id" %in% colnames(eic)) eic$id <- NA_character_
        if (!"polarity" %in% colnames(eic)) eic$polarity <- 0
        eic <- eic[, `:=`(intensity = sum(intensity)), by = c("analysis", "polarity", "id", "rt")][]
        eic <- eic[, c("analysis", "polarity", "id", "rt", "intensity"), with = FALSE]
        eic <- unique(eic)
      }
      
      eic
    },
    
    #' @description Gets level 1 spectra from the analyses based on targets as a data.frame.
    #'
    get_spectra_ms1 = function(analyses = NULL,
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
                               minIntensity = 1000) {
      
      ms1 <- self$get_spectra(
        analyses, levels = 1,
        mass, mz, rt, drift, ppm, sec, millisec, id,
        allTraces = TRUE,
        minIntensityMS1 = minIntensity,
        minIntensityMS2 = 0,
        raw_spectra = TRUE,
        loaded_spectra = TRUE
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
      
      if (!is.numeric(mzClust)) mzClust = 0.01
      
      ms1$unique_id <- paste0(ms1$analysis, "_", ms1$id, "_", ms1$polarity)
      
      ms1_list <- rcpp_ms_cluster_spectra(ms1, mzClust, presence, FALSE)
      
      ms1_df <- rbindlist(ms1_list, fill = TRUE)
      
      ms1_df <- ms1_df[order(ms1_df$mz), ]
      
      ms1_df <- ms1_df[order(ms1_df$id), ]
      
      ms1_df <- ms1_df[order(ms1_df$analysis), ]
      
      ms1_df
    },
    
    #' @description Gets level 2 spectra from the analyses based on targets as a data.frame.
    #'
    get_spectra_ms2 = function(analyses = NULL,
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
                               minIntensity = 0) {
      
      ms2 <- self$get_spectra(
        analyses, levels = 2,
        mass, mz, rt, drift, ppm, sec, millisec, id,
        isolationWindow = isolationWindow,
        allTraces = FALSE,
        minIntensityMS1 = 0,
        minIntensityMS2 = minIntensity,
        raw_spectra = TRUE,
        loaded_spectra = TRUE
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
      
      if (!is.numeric(mzClust)) mzClust = 0.01
      
      ms2$unique_id <- paste0(ms2$analysis, "_", ms2$id, "_", ms2$polarity)
      
      ms2_list <- rcpp_ms_cluster_spectra(ms2, mzClust, presence, FALSE)
      
      ms2_df <- rbindlist(ms2_list, fill = TRUE)
      
      ms2_df <- ms2_df[order(ms2_df$mz), ]
      
      ms2_df <- ms2_df[order(ms2_df$id), ]
      
      ms2_df <- ms2_df[order(ms2_df$analysis), ]
      
      ms2_df
    },
    
    #' @description Gets the number of chromatograms in each analysis.
    #'
    #' @return A character vector.
    #'
    get_chromatograms_number = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "chromatograms_number")
    },
    
    #' @description Gets the chromatograms headers data.table of each analysis.
    #'
    #' @return A data.table.
    #'
    get_chromatograms_headers = function(analyses = NULL) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(data.table())
      
      value <- lapply(private$.analyses[analyses], function(x) { x$chromatograms_headers })
      
      value <- rbindlist(value, idcol = "analysis", fill = TRUE)
      
      value
    },
    
    #' @description Gets chromatograms from each analysis.
    #' 
    #' @param raw_chromatograms Logical of length one. Set to `TRUE` for parsing raw chromatograms not chromatograms 
    #' results/processed.
    #' @param loaded_chromatograms Logical of length one. Set to `TRUE` for parsing loaded chromatograms not 
    #' chromatograms from raw data files.
    #'
    #' @return A data.table with chromatogram/s.
    #'
    get_chromatograms = function(analyses = NULL,
                                 chromatograms = NULL,
                                 minIntensity = NULL,
                                 raw_chromatograms = FALSE,
                                 loaded_chromatograms = TRUE) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(data.table())
      
      #__________________________________________________________________
      # Extracts chromatograms from results via the active binding
      #__________________________________________________________________
      if (self$has_chromatograms() && !raw_chromatograms && !loaded_chromatograms) {
        
        chroms <- rbindlist(self$chromatograms, fill = TRUE)
        
        if ("analysis" %in% colnames(chroms)) {
          chroms <- chroms[chroms$analysis %in% analyses, ]
          
        } else if ("replicate" %in% colnames(chroms)) {
          rpl <- self$get_replicate_names()
          rpl <- rpl[analyses]
          chroms <- chroms[chroms$replicate %in% unname(rpl)]
          
          if (!"analysis" %in% colnames(chroms)) {
            chroms$analysis <- chroms$replicate
            setcolorder(chroms, c("analysis", "replicate"))
          }
        }
        
        if (is.numeric(chromatograms)) {
          which_chroms <- chroms$index %in% chromatograms
          chroms <- chroms[which_chroms, ]
          
        } else if (is.character(chromatograms)) {
          which_chroms <- chroms$id %in% chromatograms
          chroms <- chroms[which_chroms, ]
          
        } else if (!is.null(chromatograms)) {
          return(data.table())
        }
        
        if (is.numeric(minIntensity)) chroms <- chroms[chroms$intensity > minIntensity, ]
        
        chroms
      
      #__________________________________________________________________
      # Extracts loaded chromatograms
      #__________________________________________________________________
      } else if (all(self$has_loaded_chromatograms(analyses)) && loaded_chromatograms) {
        
        chroms <- lapply(private$.analyses[analyses], function(x) x$chromatograms)
        chroms <- rbindlist(chroms, idcol = "analysis", fill = TRUE)
        
        if (is.numeric(chromatograms)) {
          which_chroms <- chroms$index %in% chromatograms
          chroms <- chroms[which_chroms, ]
          
        } else if (is.character(chromatograms)) {
          which_chroms <- chroms$id %in% chromatograms
          chroms <- chroms[which_chroms, ]
          
        } else if (!is.null(chromatograms)) {
          return(data.table())
        }
        
        if (is.numeric(minIntensity)) chroms <- chroms[chroms$intensity > minIntensity, ]
        
        chroms
      
      #__________________________________________________________________
      # Extracts loaded chromatograms
      #__________________________________________________________________
      } else {
        
        chroms_list <- lapply(self$get_analyses(analyses), function(x, chromatograms) {
          
          if (nrow(x$chromatograms_headers) == 0) return(data.frame())
          
          idx <- x$chromatograms_headers$index
          
          if (is.numeric(chromatograms)) {
            idx <- idx[chromatograms + 1]
            
          } else if (is.character(chromatograms)) {
            cid <- x$chromatograms_headers$id
            which_chroms <- cid %in% chromatograms
            idx <- idx[which_chroms]
            
          } else if (!is.null(chromatograms)) {
            return(data.table())
          }
          
          cache <- .load_chache("parsed_ms_chromatograms", x$file, idx)
          
          if (!is.null(cache$data)) {
            message("\U2139 Chromatograms loaded from cache!")
            return(cache$data)
          }
          
          message("\U2699 Parsing chromatograms from ", basename(x$file), "...", appendLF = FALSE)
          
          chrom <- rcpp_parse_ms_chromatograms(x, idx)
          
          message(" Done!")
          
          if (nrow(chrom) == 0) return(data.frame())
          
          if (!"analysis" %in% colnames(chrom)) chrom$analysis <- x$name
          
          if (!"replicate" %in% colnames(chrom)) chrom$replicate <- x$replicate
          
          if (!is.null(cache$hash)) {
            .save_cache("parsed_ms_chromatograms", chrom, cache$hash)
            message("\U1f5ab Parsed chromatograms cached!")
          }
          
          chrom
          
        }, chromatograms = chromatograms)
        
        if (length(chroms_list) == length(analyses)) {
          
          chroms <- rbindlist(chroms_list, fill = TRUE)
          
          if (nrow(chroms) > 0) setcolorder(chroms, c("analysis", "replicate"))
          
          if (is.numeric(minIntensity)) chroms <- chroms[chroms$intensity > minIntensity, ]
          
          chroms
          
        } else {
          warning("Defined analyses not found!")
          data.table()
        }
      }
    },
    
    #' @description Gets the list of features for each analysis.
    #' 
    #' @return A list of data.table objects for each analysis.
    #'
    get_feature_list = function(analyses = NULL, filtered = FALSE) {
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (self$has_results("patRoon")) {
        
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
            
            pol <- self$get_spectra_polarity(x)
            
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
      
      if (self$has_results("patRoon")) fts <- self$feature_list[analyses]
      
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
              ids <- target_id$name
              names(ids) <- target_id$group
              ids <- ids[!duplicated(names(ids))]
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
                                loaded = TRUE) {
      
      fts <- self$get_features(analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered)
      
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
          
          eic_2 <- self$get_spectra(analyses = analyses, levels = 1, mz = fts_filtered, id = fts_filtered$feature)
          
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
        
        eic <- self$get_spectra(analyses = analyses, levels = 1, mz = fts, id = fts$feature, raw_spectra = TRUE, loaded_spectra = TRUE)
        
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
                                filtered = FALSE,
                                loaded = TRUE) {
      
      fts <- self$get_features(analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered)
      
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
      
      if (loaded & any(self$has_loaded_features_ms1(analysis_names))) {
        
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
          
          ms1_2 <- self$get_spectra_ms1(
            analyses = unique(fts$analysis),
            mz = fts_filtered,
            id = fts_filtered$feature,
            mzClust = mzClust,
            presence = presence,
            minIntensity = minIntensity
          )
          
          setnames(ms1_2, "id", "feature")
          
          ms1 <- list(ms1, ms1_2)
          
          ms1 <- rbindlist(ms1, fill = TRUE)
        }
        
        if (nrow(ms1) == 0) return(data.table())
        
      } else {
        ms1 <- self$get_spectra_ms1(
          analyses = unique(fts$analysis),
          mz = fts,
          id = fts$feature,
          mzClust = mzClust,
          presence = presence,
          minIntensity = minIntensity
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
                                filtered = FALSE,
                                loaded = TRUE) {
      
      fts <- self$get_features(analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered)
      
      if (nrow(fts) == 0) return(data.table())
      
      analysis_names <- unique(fts$analysis)
      
      if (loaded & any(self$has_loaded_features_ms2(analysis_names))) {
        
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
          
          ms2_2 <- self$get_spectra_ms2(
            analyses = unique(fts$analysis),
            mz = fts_filtered,
            id = fts_filtered$feature,
            isolationWindow = isolationWindow,
            mzClust = mzClust,
            presence = presence,
            minIntensity = minIntensity
          )
          
          if (nrow(ms2_2) == 0) return(data.table())
          
          setnames(ms2_2, "id", "feature", skip_absent = TRUE)
          
          ms2 <- list(ms2, ms2_2)
          
          ms2 <- rbindlist(ms2, fill = TRUE)
        }
        
        if (nrow(ms2) == 0) return(data.table())
        
      } else {
        ms2 <- self$get_spectra_ms2(
          analyses = unique(fts$analysis),
          mz = fts,
          id = fts$feature,
          isolationWindow = isolationWindow,
          mzClust = mzClust,
          presence = presence,
          minIntensity = minIntensity
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
    #' @param sdValues Logical (length 1). Set to `TRUE` for returning the sd values when averaging the intensity 
    #' within analysis replicates.
    #' @param metadata Logical (length 1). Set to `TRUE` for returning extra metadata from feature groups 
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
            filtered = all(filtered)
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
                              loaded = TRUE,
                              mzClust = 0.003,
                              presence = 0.8,
                              minIntensity = 1000,
                              groupBy = "groups",
                              filtered = FALSE) {
      
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
        filtered = filtered,
        loaded = loaded
      )
      
      ms1 <- ms1[ms1$intensity > minIntensity, ]
      
      if (nrow(ms1) == 0) return(data.table())
      
      polarities <- unique(self$get_spectra_polarity(analyses = unique(ms1$analysis)))
      
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
                              loaded = TRUE,
                              mzClust = 0.003,
                              presence = 0.8,
                              minIntensity = 100,
                              groupBy = "groups",
                              filtered = FALSE) {
      
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
        filtered = filtered,
        loaded = loaded
      )
      
      ms2 <- ms2[ms2$intensity > minIntensity, ]
      
      if (nrow(ms2) == 0) return(data.table())
      
      polarities <- unique(self$get_spectra_polarity(analyses = unique(ms2$analysis)))
      
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
      
      if (is.null(features) & !is.null(groups)) features <- groups
      
      fts <- self$get_features(analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered)
      
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
            
            pol <- self$get_spectra_polarity(analysis)
            
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
                        minIntensity = minIntensity
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
    
    #' @description Adds analyses. Note that when adding new analyses, any existing results are removed.
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
        
        if (self$get_number_analyses() > n_analyses) self$remove_results()
      }
      
      invisible(self)
    },
    
    #' @description Adds analyses based on mzML/mzXML files. Note that when adding new mzML/mzXML files, any existing 
    #' grouping or features are removed.
    #'
    #' @return Invisible.
    #'
    add_files = function(files = NULL) {

      if (!is.null(files)) {
        
        if (is.data.frame(files)) {
          
          if (all(c("path", "analysis") %in% colnames(files))) {
            files$file <- vapply(seq_len(nrow(files)), function(x) {
              list.files(files$path[x], pattern = files$analysis[x], full.names = TRUE, recursive = FALSE)
            }, "")
          }
          
          if ("file" %in% colnames(files)) {
            
            if ("replicate" %in% colnames(files)) {
              replicates <- as.character(files$replicate)
            } else if ("group" %in% colnames(files)) {
              replicates <- as.character(files$group)
            } else {
              replicates <- rep(NA_character_, nrow(files))
            }
            
            if ("blank" %in% colnames(files)) {
              blanks <- as.character(files$blank)
            } else {
              blanks <- rep(NA_character_, nrow(files))
            }
            
            files <- files$file
            
          } else {
            files <- ""
          }
          
        } else {
          replicates <- rep(NA_character_, length(files))
          blanks <- rep(NA_character_, length(files))
        }
        
        possible_ms_file_formats <- ".mzML|.mzXML"
        
        valid_files <- vapply(files,
          FUN.VALUE = FALSE,
          function(x, possible_ms_file_formats) {
            if (!file.exists(x)) {
              return(FALSE)
            }
            if (FALSE %in% grepl(possible_ms_file_formats, x)) {
              return(FALSE)
            }
            TRUE
          }, possible_ms_file_formats = possible_ms_file_formats
        )
        
        if (!all(valid_files)) {
          warning("File/s not valid!")
          return(NULL)
        }
        
        names(replicates) <- as.character(files)
        
        names(blanks) <- as.character(files)
        
        analyses <- lapply(files, function(x) {
          
          cache <- .load_chache("parsed_ms_analyses", x)
          
          if (!is.null(cache$data)) {
            message("\U2139 ", basename(x), " analysis loaded from cache!")
            cache$data
            
          } else {
            
            message("\U2699 Parsing ", basename(x), "...", appendLF = FALSE)
            
            ana <- rcpp_parse_ms_analysis(x)
            
            class_ana <- class(ana)[1]
            
            if (!class_ana %in% "MassSpecAnalysis") {
              message(" Not Done!")
              return(NULL)
            }
            
            message(" Done!")
            
            rpl <- replicates[x]
            
            if (is.na(rpl)) {
              rpl <- ana$name
              rpl <- sub("-[^-]+$", "", rpl)
            }
            
            ana$replicate <- rpl
            
            blk <- blanks[x]
            
            if (!is.na(blk)) ana$blank <- blk
            
            ana$blank <- blk
            
            if (!is.null(cache$hash)) {
              .save_cache("parsed_ms_analyses", ana, cache$hash)
              message("\U1f5ab Parsed analysis cached!")
            }
            
            ana
          }
        })
        
        names(analyses) <- vapply(analyses, function(x) x[["name"]], "")
        
        analyses <- analyses[order(names(analyses))]

        if (all(vapply(analyses, function(x) "MassSpecAnalysis" %in% is(x), FALSE))) {
          self$add_analyses(analyses)

        } else {
          warning("Not all added files could be converted as MassSpecAnalysis!")
        }

      } else {
        warning("Files were not added!")
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
    load_spectra = function(analyses = NULL,
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
                            minIntensityMS2 = 0) {
      
      spec <- self$get_spectra(
        analyses, levels, mass, mz, rt, drift, ppm, sec, millisec, id, allTraces, isolationWindow, 
        minIntensityMS1, minIntensityMS2, raw_spectra = TRUE, loaded_spectra = FALSE
      )

      split_vector <- spec$analysis
      
      spec$analysis <- NULL
      
      spec_list <- split(spec, split_vector)
      
      if (length(spec_list) == self$get_number_analyses()) {
        
        private$.analyses <- Map(
          function(x, y) {
            x$spectra <- y
            x
          },
          private$.analyses, spec_list
        )
        
        private$.register("added", "analyses", "raw spectra")
        message("\U2713 ", " Spectra loaded!")

      } else {
        warning("Not done, check the MS file paths and formats!")
      }
      
      invisible(self)
    },

    #' @description Loads all chromatograms from all analyses.
    #'
    #' @return Invisible.
    #'
    load_chromatograms = function(chromatograms = NULL) {

      chrom <- self$get_chromatograms(chromatograms = chromatograms)

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

          private$.register("loaded", "analyses", "raw chromatograms")
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
            self$remove_results()
            
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

        self$remove_results("patRoon")

        private$.register("removed", "results", "features", "all")

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
        
        private$.register("removed", "results", "filtered features", n_filtered_features)
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
            
            private$.register("removed", "results", "features", n_feat_before - n_feat_after)
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

          private$.register("removed", "results", "features_ms1", "all")
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

          private$.register("removed", "results", "features_ms2", "all")
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
        
        self$remove_results("patRoon")
        
        self$features <- original_features
        
        self$remove_features(filtered = TRUE)

        private$.register("removed", "results", "feature groups", "all")
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
            private$.register("removed", "results", "feature groups", n_groups - n_groups_after)
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
      has_im <- vapply(private$.analyses[analyses], function(x) any(x$spectra_headers$drift > 0), FALSE)
      names(has_im) <- analyses
      has_im
    },

    #' @description Checks for loaded spectra in given analyses names/indices, returning `TRUE` or `FALSE` for each 
    #' analysis.
    #'
    has_loaded_spectra = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(FALSE)
      has_spectra <- vapply(private$.analyses[analyses], function(x) nrow(x$spectra) > 0, FALSE)
      names(has_spectra) <- analyses
      has_spectra
    },

    #' @description Checks for loaded chromatograms in given analyses names/indices, returning `TRUE` or `FALSE` for 
    #' each analysis.
    #'
    has_loaded_chromatograms = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(FALSE)
      has_chromatograms <- vapply(private$.analyses[analyses], function(x) nrow(x$chromatograms) > 0, FALSE)
      names(has_chromatograms) <- analyses
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
    
    #' @description Checks if there are chromatograms, returning `TRUE` or `FALSE`.
    #'
    has_chromatograms = function() {
      if (self$has_results("chromatograms")) {
        if (all(vapply(private$.results$chromatograms, function(x) "chromatograms" %in% names(x), FALSE))) {
          sum(vapply(private$.results$chromatograms, function(x) nrow(x$chromatograms), 0)) > 0
        } else {
          all(vapply(self$get_analyses(), function(x) nrow(x$chromatograms) > 0, FALSE))
        }
      } else {
        all(vapply(self$get_analyses(), function(x) nrow(x$chromatograms) > 0, FALSE))
      }
    },
    
    #' @description Checks if there are integrated peaks from chromatograms, returning `TRUE` or `FALSE`.
    #'
    has_chromatograms_peaks = function() {
      if (self$has_results("chromatograms")) {
        if (all(vapply(private$.results$chromatograms, function(x) "peaks" %in% names(x), FALSE))) {
          sum(vapply(private$.results$chromatograms, function(x) nrow(x$peaks), 0)) > 0
        } else {
          FALSE
        }
      } else {
        FALSE
      }
    },
    
    #' @description Checks if there are spectra, returning `TRUE` or `FALSE`.
    #'
    has_spectra = function() {
      if (self$has_results("spectra")) {
        if (all(vapply(private$.results$spectra, function(x) "spectra" %in% names(x), FALSE))) {
          sum(vapply(private$.results$spectra, function(x) nrow(x$spectra), 0)) > 0
        } else {
          all(vapply(self$get_analyses(), function(x) nrow(x$spectra) > 0, FALSE))
        }
      } else {
        all(vapply(self$get_analyses(), function(x) nrow(x$spectra) > 0, FALSE))
      }
    },
    
    #' @description Checks if there are spectra peaks, returning `TRUE` or `FALSE`.
    #'
    has_spectra_peaks = function() {
      if (self$has_results("spectra")) {
        if (all(vapply(private$.results$spectra, function(x) "peaks" %in% names(x), FALSE))) {
          sum(vapply(private$.results$spectra, function(x) nrow(x$peaks), 0)) > 0
        } else {
          FALSE
        }
      } else {
        FALSE
      }
    },
    
    #' @description Checks if there are deconvoluted spectra, returning `TRUE` or `FALSE`.
    #'
    has_deconvoluted_spectra = function() {
      if (self$has_results("spectra")) {
        if (all(vapply(private$.results$spectra, function(x) "deconvoluted" %in% names(x), FALSE))) {
          sum(vapply(private$.results$spectra, function(x) nrow(x$deconvoluted), 0)) > 0
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
        if (all(vapply(private$.results$spectra, function(x) "charges" %in% names(x), FALSE))) {
          sum(vapply(private$.results$spectra, function(x) nrow(x$charges), 0)) > 0
        } else {
          FALSE
        }
      } else {
        FALSE
      }
    },

    ## ___ plot -----

    #' @description Plots raw spectra in 3D for given MS analyses.
    #' 
    #' @param xVal Character length one. Possible values are "mz", "rt" or "drift".
    #' @param yVal Character length one. Possible values are "mz", "rt" or "drift".
    #' @param zLab A string with the title for the z axis.
    #'
    plot_raw_spectra = function(analyses = NULL,
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
        minIntensityMS2
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

      if ("replicates" %in% colorBy) spec$replicate <- self$get_replicate_names()[spec$analysis]

      .plot_spectra_interactive(spec, colorBy, legendNames, xVal, yVal, xLab, yLab, zLab)
    },
    
    #' @description Plots spectra given MS analyses.
    #'
    #' @param xVal Character length one. Possible values are "mz", "rt", "drift" or "mass".
    #' @param raw_spectra Logical of length one. Set to `TRUE` for plotting the spectra in results not the raw spectra.
    #' @param loaded_spectra Logical of length one. Set to `TRUE` for parsing loaded spectra not 
    #' spectra from raw data files.
    #' @param averaged Logical of length one. Set to `TRUE` for plotting the averaged spectra.
    #' @param baseline Logical of length one. Set to `TRUE` for plotting the spectra with baseline corrected.
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
                            raw_spectra = FALSE,
                            loaded_spectra = TRUE,
                            averaged = TRUE,
                            baseline = FALSE,
                            legendNames = TRUE,
                            colorBy = "analyses",
                            xVal = "mz",
                            xLab = NULL,
                            yLab = NULL,
                            title = NULL,
                            cex = 0.6,
                            showLegend = TRUE,
                            interactive = TRUE) {
      
      spec <- self$get_spectra(
        analyses, levels, mass, mz, rt, drift, ppm, sec, millisec, id, allTraces = allTraces,
        isolationWindow, minIntensityMS1, minIntensityMS2, raw_spectra, loaded_spectra
      )
      
      if (nrow(spec) == 0) {
        message("\U2717 Traces not found for the targets!")
        return(NULL)
      }
      
      if (xVal == "mz" && (!"mz" %in% colnames(spec)) && "mass" %in% colnames(spec)) xVal = "mass"
      
      if (!xVal %in% colnames(spec)) {
        message("\U2717 xVal not found in spectra data.table!")
        return(NULL)
      }
      
      spec$x <- spec[[xVal]]
      
      if ("feature" %in% colnames(spec)) spec$id <- spec$feature
      
      if ("replicates" %in% colorBy) spec$replicate <- self$get_replicate_names()[spec$analysis]
      
      spec <- .make_colorBy_varkey(spec, colorBy, legendNames = NULL)
      
      unique_key <- c("analysis", "var", "x")
      
      spec <- spec[, .(intensity = sum(intensity)), by = c(unique_key)]
      
      spec <- unique(spec)
      
      if ("rt" %in% xVal) {
        if (is.null(xLab)) xLab = "Retention time / seconds"
        
      } else if ("mz" %in% xVal) {
        if (is.null(xLab)) {
          if (interactive) {
            xLab = "<i>m/z</i> / Da"
          } else {
            xLab = expression(italic("m/z ") / " Da")
          }
        }
        
      } else if ("mass" %in% xVal) {
        if (is.null(xLab)) xLab = "Mass / Da"
        
      } else if ("drift" %in% xVal) {
        if (is.null(xLab)) xLab = "Drift time / milliseconds"
      }
      
      if (is.null(yLab)) yLab = "Intensity / counts"
      
      setorder(spec, var, x)

      if (!interactive) {
        return(.plot_x_spectra_static(spec, xLab, yLab, title, cex, showLegend))
        
      } else {
        return(.plot_x_spectra_interactive(spec, xLab, yLab, title, colorBy))
      }
    },
    
    #' @description Plots chromatograms in the analyses.
    #' 
    #' @param raw_chromatograms Logical of length one. Set to `TRUE` for parsing raw chromatograms not chromatograms 
    #' results/processed.
    #' @param loaded_chromatograms Logical of length one. Set to `TRUE` for parsing loaded chromatograms not 
    #' chromatograms from raw data files.
    #'
    plot_chromatograms = function(analyses = NULL,
                                  chromatograms = NULL,
                                  minIntensity = NULL,
                                  raw_chromatograms = FALSE,
                                  loaded_chromatograms = TRUE,
                                  title = NULL,
                                  colorBy = "targets",
                                  legendNames = NULL,
                                  showLegend = TRUE,
                                  xlim = NULL,
                                  ylim = NULL,
                                  cex = 0.6,
                                  interactive = TRUE) {
      
      chromatograms <- self$get_chromatograms(analyses, chromatograms, minIntensity, raw_chromatograms, loaded_chromatograms)
      
      if (nrow(chromatograms) == 0) {
        message("\U2717 Chromatograms not found for the analyses!")
        return(NULL)
      }
      
      if ("replicates" %in% colorBy) chromatograms$replicate <- self$get_replicate_names()[chromatograms$analysis]

      pol_key <- c("positive", "negative", "nd")
      names(pol_key) <- c("1", "-1", "0")
      chromatograms$polarity <- as.character(chromatograms$polarity)
      chromatograms$polarity <- pol_key[chromatograms$polarity]
      
      if (!interactive) {
        .plot_spectra_eic_static(chromatograms, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
      } else {
        .plot_chromatograms_interactive(chromatograms, legendNames, colorBy, title, showLegend)
      }
    },
    
    #' @description Plots chromatograms corrected baseline for given analyses.
    #'
    #' @return A plot.
    #' 
    plot_chromatograms_baseline = function(analyses = NULL,
                                           chromatograms = NULL,
                                           xLab = NULL,
                                           yLab = NULL,
                                           title = NULL,
                                           cex = 0.6,
                                           showLegend = TRUE,
                                           colorBy = "analyses",
                                           interactive = TRUE) {
      
      chroms <- self$get_chromatograms(analyses, chromatograms, 0, FALSE)
      
      if (!("baseline" %in% colnames(chroms) && "raw" %in% colnames(chroms))) {
        warning("Baseline not found!")
        return(NULL)
      }
      
      if ("replicates" %in% colorBy) chroms$replicate <- self$get_replicate_names()[chroms$analysis]
      
      chroms <- .make_colorBy_varkey(chroms, colorBy, legendNames = NULL)
      
      setnames(chroms, "rt", "x")
      
      if (is.null(xLab)) xLab = "Retention time / seconds"
      
      if (is.null(yLab)) yLab = "Intensity / counts"
      
      if (!interactive) {
        return(.plot_x_spectra_baseline_static(chroms, xLab, yLab, title, cex, showLegend))
      } else {
        return(.plot_x_spectra_baseline_interactive(chroms, xLab, yLab, title, colorBy))
      }
    },

    #' @description Plots spectra extract ion chromatograms (EIC) and \emph{m/z} vs retention time from the analyses.
    #'
    #' @param plotTargetMark Logical (length 1), set to \code{TRUE} to plot a target mark.
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
    plot_spectra_xic = function(analyses = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                drift = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                id = NULL,
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
        minIntensityMS2 = 0
      )

      if (nrow(xic) == 0) {
        message("\U2717 Traces not found for the targets!")
        return(NULL)
      }

      .plot_spectra_xic_interactive(
        xic,
        legendNames,
        plotTargetMark,
        targetsMark,
        ppmMark,
        secMark,
        numberRows
      )
    },

    #' @description Plots spectra extract ion chromatograms (EIC) from the analyses based on targets.
    #'
    plot_spectra_eic = function(analyses = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                drift = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                id = NULL,
                                legendNames = NULL,
                                title = NULL,
                                colorBy = "targets",
                                showLegend = TRUE,
                                xlim = NULL,
                                ylim = NULL,
                                cex = 0.6,
                                interactive = TRUE) {

      eic <- self$get_spectra_eic(analyses, mass, mz, rt, drift, ppm, sec, millisec, id)

      if (nrow(eic) == 0) {
        message("\U2717 Traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) eic$replicate <- self$get_replicate_names()[eic$analysis]

      if (!interactive) {
        .plot_spectra_eic_static(eic, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
      } else {
        .plot_spectra_eic_interactive(eic, legendNames, colorBy, title, showLegend)
      }
    },

    #' @description Plots the spectra total ion chromatogram (TIC) of each analysis.
    #'
    plot_spectra_tic = function(analyses = NULL,
                                levels = c(1, 2),
                                rt = NULL,
                                title = NULL,
                                colorBy = "analyses",
                                legendNames = NULL,
                                showLegend = TRUE,
                                xlim = NULL,
                                ylim = NULL,
                                cex = 0.6,
                                interactive = TRUE) {

      tic <- self$get_spectra_tic(analyses, levels, rt)

      if (nrow(tic) == 0) {
        message("\U2717 TIC not found for the analyses!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) tic$replicate <- self$get_replicate_names()[tic$analysis]

      if (!"id" %in% colnames(tic)) tic$id <- tic$analysis

      if (!interactive) {
        .plot_spectra_eic_static(tic, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
      } else {
        .plot_spectra_eic_interactive(tic, legendNames, colorBy, title, showLegend)
      }
    },

    #' @description Plots the spectra base peak chromatogram (BPC) of each analysis.
    #'
    #' @return A plot.
    #'
    plot_spectra_bpc = function(analyses = NULL,
                                levels = c(1, 2),
                                rt = NULL,
                                title = NULL,
                                colorBy = "analyses",
                                legendNames = NULL,
                                showLegend = TRUE,
                                xlim = NULL,
                                ylim = NULL,
                                cex = 0.6,
                                interactive = TRUE) {

      bpc <- self$get_spectra_bpc(analyses, levels, rt)

      if (nrow(bpc) == 0) {
        message("\U2717 BPC not found for the analyses!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) bpc$replicate <- self$get_replicate_names()[bpc$analysis]

      if (!"id" %in% colnames(bpc)) bpc$id <- bpc$analysis

      if (!interactive) {
        .plot_spectra_eic_static(bpc, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
      } else {
        .plot_spectra_bpc_interactive(bpc, legendNames, colorBy, title, showLegend)
      }
    },

    #' @description Plots level 2 spectra from the analyses based on targets.
    #'
    plot_spectra_ms2 = function(analyses = NULL,
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
                                minIntensity = 0,
                                legendNames = NULL,
                                title = NULL,
                                colorBy = "targets",
                                interactive = TRUE) {

      ms2 <- self$get_spectra_ms2(analyses, mass, mz, rt, drift, ppm, sec, millisec, id, isolationWindow, mzClust, presence, minIntensity)

      if (nrow(ms2) == 0) {
        message("\U2717 MS2 traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) ms2$replicate <- self$get_replicate_names()[ms2$analysis]

      if (!interactive) {
        .plot_spectra_ms2_static(ms2, legendNames, colorBy, title)
      } else {
        .plot_spectra_ms2_interactive(ms2, legendNames, colorBy, title)
      }
    },

    #' @description Plots level 1 spectra from the analyses based on targets.
    #'
    plot_spectra_ms1 = function(analyses = NULL,
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
                                minIntensity = 1000,
                                legendNames = NULL,
                                title = NULL,
                                colorBy = "targets",
                                showText = FALSE,
                                interactive = TRUE) {

      ms1 <- self$get_spectra_ms1(analyses, mass, mz, rt, drift, ppm, sec, millisec, id, mzClust, presence, minIntensity)

      if (nrow(ms1) == 0) {
        message("\U2717 MS1 traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) ms1$replicate <- self$get_replicate_names()[ms1$analysis]

      if (!interactive) {
        .plot_spectra_ms1_static(ms1, legendNames, colorBy, title, showText)
      } else {
        .plot_spectra_ms1_interactive(ms1, legendNames, colorBy, title, showText)
      }
    },

    #' @description Plots features from analyses.
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
                             legendNames = NULL,
                             title = NULL,
                             colorBy = "targets",
                             showLegend = TRUE,
                             xlim = NULL,
                             ylim = NULL,
                             cex = 0.6,
                             interactive = TRUE) {

      fts <- self$get_features(analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered)

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
        loaded = loaded
      )

      eic <- eic[, `:=`(intensity = sum(intensity)), by = c("analysis", "polarity", "feature", "rt")][]

      if (nrow(eic) == 0) {
        message("\U2717 Traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) eic$replicate <- self$get_replicate_names()[eic$analysis]

      if (!interactive) {
        .plot_features_static(eic, fts, legendNames, colorBy, title, showLegend, xlim, ylim, cex)
      } else {
        .plot_features_interactive(eic, fts, legendNames, colorBy, title, showLegend)
      }
    },

    #' @description Plots a map of the retention time vs \emph{m/z} of features from analyses.
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

      fts <- self$get_features(analyses, features, mass, mz, rt, drift, ppm, sec, millisec, filtered)

      if (nrow(fts) == 0) {
        message("\U2717 Features not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) fts$replicate <- self$get_replicate_names()[fts$analysis]

      if (!interactive) {
        .map_features_static(fts, colorBy, legendNames, title, showLegend, xlim, ylim, cex)
      } else {
        .map_features_interactive(fts, colorBy, legendNames, xlim, ylim, title)
      }
    },

    #' @description Plots level 1 spectra from features in the analyses.
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
                                 filtered = FALSE,
                                 loaded = TRUE,
                                 legendNames = NULL,
                                 title = NULL,
                                 colorBy = "targets",
                                 interactive = TRUE) {

      ms1 <- self$get_features_ms1(analyses, features, mass, mz, rt, drift, ppm, sec, millisec,
        rtWindow, mzWindow, mzClust, presence, minIntensity, filtered, loaded
      )

      if (nrow(ms1) == 0) {
        message("\U2717 MS1 traces not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) {
        ms1$replicate <- self$get_replicate_names()[ms1$analysis]
      }

      if (!interactive) {
        .plot_spectra_ms1_static(ms1, legendNames, colorBy, title)
      } else {
        .plot_spectra_ms1_interactive(ms1, legendNames, colorBy, title)
      }
    },

    #' @description Plots level 2 spectra from features in the analyses.
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
                                 filtered = FALSE,
                                 loaded = TRUE,
                                 legendNames = NULL,
                                 title = NULL,
                                 colorBy = "targets",
                                 interactive = TRUE) {

      ms2 <- self$get_features_ms2(analyses, features, mass, mz, rt, drift, ppm, sec, millisec,
        isolationWindow, mzClust, presence, minIntensity, filtered, loaded
      )

      if (nrow(ms2) == 0) {
        message("\U2717 MS2 traces not found for the targets!")
        return(NULL)
      }
      if (grepl("replicates", colorBy)) {
        ms2$replicate <- self$get_replicate_names()[ms2$analysis]
      }

      if (!interactive) {
        .plot_spectra_ms2_static(ms2, legendNames, colorBy, title)
      } else {
        .plot_spectra_ms2_interactive(ms2, legendNames, colorBy, title)
      }
    },

    #' @description Plots feature groups EIC.
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
                           legendNames = NULL,
                           title = NULL,
                           colorBy = "targets",
                           showLegend = TRUE,
                           xlim = NULL,
                           ylim = NULL,
                           cex = 0.6,
                           interactive = TRUE) {

      fts <- self$get_features(analyses = NULL, groups, mass, mz, rt, drift, ppm, sec, millisec, filtered)

      if (grepl("targets", colorBy) & !isTRUE(legendNames)) {
        fts$name <- fts$group
        if (is.null(legendNames)) legendNames <- TRUE
      }

      self$plot_features(
        features = fts,
        rtExpand = rtExpand,
        mzExpand = mzExpand,
        filtered = filtered,
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

    #' @description Plots level 1 spectra from feature groups in the analyses.
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
                               loaded = TRUE,
                               mzClust = 0.005,
                               presence = 0.8,
                               minIntensity = 1000,
                               groupBy = "groups",
                               filtered = FALSE,
                               legendNames = NULL,
                               title = NULL,
                               colorBy = "targets",
                               interactive = TRUE) {

      if (grepl("groups", colorBy) || grepl("targets", colorBy)) {
        groupBy <- "groups"
      } else {
        groupBy <- "replicates"
      }

      ms1 <- self$get_groups_ms1(groups, mass, mz, rt, drift, ppm, sec, millisec, rtWindow, mzWindow, mzClustFeatures, 
        presenceFeatures, minIntensityFeatures, loaded, mzClust, presence, minIntensity, groupBy, filtered
      )

      if (nrow(ms1) == 0) {
        message("\U2717 MS1 traces not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) colorBy <- "replicates"
      
      if (grepl("analyses", colorBy) && grepl("targets", colorBy)) colorBy <- "replicates+targets"

      if (!interactive) {
        .plot_spectra_ms1_static(ms1, legendNames, colorBy, title)
      } else {
        .plot_spectra_ms1_interactive(ms1, legendNames, colorBy, title)
      }
    },

    #' @description Plots level 1 spectra from feature groups in the analyses.
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
                               loaded = TRUE,
                               mzClust = 0.003,
                               presence = TRUE,
                               minIntensity = 100,
                               groupBy = "groups",
                               filtered = FALSE,
                               legendNames = NULL,
                               title = NULL,
                               colorBy = "targets",
                               interactive = TRUE) {
      
      if (grepl("groups", colorBy) || grepl("targets", colorBy)) {
        groupBy <- "groups"
      } else {
        groupBy <- "replicates"
      }

      ms2 <- self$get_groups_ms2(groups, mass, mz, rt, drift, ppm, sec, millisec, isolationWindow, mzClustFeatures,
        presenceFeatures, minIntensityFeatures, loaded, mzClust, presence, minIntensity, groupBy, filtered
      )

      if (nrow(ms2) == 0) {
        message("\U2717 MS2 traces not found for the targets!")
        return(NULL)
      }

      if ("analyses" %in% colorBy) colorBy <- "replicates"
      
      if (grepl("analyses", colorBy) && grepl("targets", colorBy)) colorBy <- "replicates+targets"

      if (!interactive) {
        .plot_spectra_ms2_static(ms2, legendNames, colorBy, title)
      } else {
        .plot_spectra_ms2_interactive(ms2, legendNames, colorBy, title)
      }
    },

    #' @description Method to give an overview of the EIC, alignment and intensity variance from features within 
    #' target feature groups.
    #'
    #' @param heights A numeric vector of length 3 to control the height of the first, second and third plot, respectively.
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
                                    legendNames = NULL,
                                    title = NULL,
                                    heights = c(0.35, 0.5, 0.15)) {

      fts <- self$get_features(analyses, groups, mass, mz, rt, drift, ppm, sec, millisec, filtered)

      eic <- self$get_features_eic(analyses = unique(fts$analysis), features = fts,
        rtExpand = rtExpand, mzExpand = mzExpand, filtered = filtered, loaded = loaded
      )

      eic <- eic[, `:=`(intensity = sum(intensity)), by = c("analysis", "polarity", "feature", "rt")][]

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
    
    #' @description Method to plot the intensity profile of feature groups across the analyses.
    #' 
    #' @param normalized Logical (length 1). When `TRUE` the profile intensities are normalized.
    #' 
    plot_groups_profile = function(analyses = NULL,
                                   groups = NULL,
                                   mass = NULL,
                                   mz = NULL,
                                   rt = NULL,
                                   drift = NULL,
                                   ppm = 20,
                                   sec = 60,
                                   millisec = 5,
                                   filtered = FALSE,
                                   normalized = TRUE,
                                   legendNames = NULL,
                                   title = NULL) {
      
      fts <- self$get_features(analyses, groups, mass, mz, rt, drift, ppm, sec, millisec, filtered)
      
      if (nrow(fts) == 0) {
        message("\U2717 Features not found for the targets!")
        return(NULL)
      }
      
      if (!"polarity" %in% colnames(fts)) {
        polarities <- self$get_spectra_polarity()
        fts$polarity <- polarities[fts$analysis]
      }
      
      if (normalized && "intensity_rel" %in% colnames(fts)) fts$intensity <- as.numeric(fts$intensity_rel)
      
      if (is.character(legendNames) & length(legendNames) == length(unique(fts$group))) {
        leg <- legendNames
        names(leg) <- unique(fts$group)
        leg <- leg[fts$group]
        fts$var <- leg[fts$group]
      } else if (isTRUE(legendNames) & "name" %in% colnames(fts)) {
        leg <- fts$name
        fts$var <- fts$name
      } else {
        leg <- fts$group
        fts$var <- fts$group
      }
      
      u_leg <- unique(leg)
      
      colors <- .get_colors(u_leg)
      
      analyses <- private$.check_analyses_argument(analyses)
      
      showLeg <- rep(TRUE, length(u_leg))
      names(showLeg) <- u_leg
      
      rpls <- self$get_replicate_names()
      
      plot <- plot_ly(fts, x = sort(unique(fts$analysis)))
      
      for (g in u_leg) {
        
        df <- fts[fts$var == g, ]
        
        if (!all(analyses %in% df$analysis)) {
          extra <- data.frame(
            "analysis" = analyses[!analyses %in% df$analysis],
            "polarity" = polarities[!analyses %in% df$analysis],
            "var" = g,
            "intensity" = 0
          )
          df <- rbind(df[, c("analysis", "var", "intensity", "polarity")], extra)
        }
        
        df <- df[order(df$analysis), ]
        
        if (normalized) {
          
          if (length(unique(df$polarity)) > 1) {
            
            for (p in unique(df$polarity)) {
              max_int <- max(df$intensity[df$polarity == p])
              if (max_int > 0) df$intensity[df$polarity == p] <- df$intensity[df$polarity == p] / max_int
            }
          } else {
            max_int <- max(df$intensity)
            if (max_int > 0) df$intensity <- df$intensity / max_int
          }
        }
        
        plot <- plot %>% add_trace(df,
          x = df$analysis,
          y = df$intensity,
          type = "scatter", mode = "lines",
          line = list(width = 0.5, color = colors[g], dash = "dash"),
          connectgaps = FALSE,
          name = g,
          legendgroup = g,
          showlegend = FALSE
        )
        
        df$replicate <- rpls[df$analysis]
        
        for (r in unique(df$replicate)) {
          df_r <- df[df$replicate %in%  r, ]
          
          plot <- plot %>% add_trace(df,
            x = df_r$analysis,
            y = df_r$intensity,
            type = "scatter", mode = "lines+markers",
            line = list(width = 1.5, color = colors[g]),
            marker = list(size = 5, color = colors[g]),
            connectgaps = FALSE,
            name = g,
            legendgroup = g,
            showlegend = showLeg[g]
          )
          
          showLeg[g] <- FALSE
        }
      }
      
      xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = NULL)
      
      yaxis <- list(
        linecolor = toRGB("black"), linewidth = 2,
        title = "Normalized intensity",
        titlefont = list(size = 12, color = "black")
      )
      
      plot <- plot %>% plotly::layout(xaxis = xaxis, yaxis = yaxis)
      
      plot
    },

    #' @description Plots the quality control assessment of the internal standards.
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
    
    #' @description Plots suspects.
    #' 
    #' @param database A data.frame with at least the columns name and mass, indicating the name and neutral monoisotopic
    #' mass of the suspect targets.
    #'
    #' @details The `ppm` and `sec` which indicate the mass (im ppm) and time (in seconds) deviations applied during the
    #' screening.
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
          loaded = loaded
        )
        
        eic <- eic[, `:=`(intensity = sum(intensity)), by = c("analysis", "polarity", "feature", "rt")][]
        
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

    #' @description Maps isotopic clusters in the analyses.
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

      isotopes <- self$get_isotopes(analyses, groups, features, clusters, mass, mz, rt, drift, ppm, sec, millisec, filtered)

      if (nrow(isotopes) == 0) {
        message("\U2717 Feature isotopes not found for the targets!")
        return(NULL)
      }

      if (grepl("replicates", colorBy)) isotopes$replicate <- self$get_replicate_names()[isotopes$analysis]
      
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
    
    #' @description Plots peaks from chromatograms from analyses.
    #'
    plot_chromatograms_peaks = function(analyses = NULL,
                                        chromatograms = NULL,
                                        legendNames = NULL,
                                        title = NULL,
                                        colorBy = "targets",
                                        showLegend = TRUE,
                                        xlim = NULL,
                                        ylim = NULL,
                                        cex = 0.6,
                                        xLab = NULL,
                                        yLab = NULL,
                                        interactive = TRUE) {
      
      if (!self$has_chromatograms_peaks()) return(NULL)
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(NULL)
      
      pks <- self$chromatograms_peaks
      pks <- rbindlist(pks)
      pks <- pks[pks$analysis %in% analyses, ]
      
      if (is.numeric(chromatograms)) {
        which_pks <- pks$index %in% chromatograms
        pks <- pks[which_pks, ]
        
      } else if (is.character(chromatograms)) {
        which_pks <- pks$id %in% chromatograms
        pks <- pks[which_pks, ]
        
      } else if (!is.null(chromatograms)) {
        return(NULL)
      }
      
      if (nrow(pks) == 0) {
        message("\U2717 Peaks not found for the targets!")
        return(NULL)
      }
      
      chroms <- self$get_chromatograms(analyses = analyses, chromatograms = chromatograms)
      
      if ("replicates" %in% colorBy) {
        chroms$replicate <- self$get_replicate_names()[chroms$analysis]
        pks$replicate <- self$get_replicate_names()[pks$analysis]
      }
      
      if (!interactive) {
        .plot_chrom_peaks_static(chroms, pks, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
      } else {
        .plot_chrom_peaks_interactive(chroms, pks, legendNames, colorBy, title, showLegend, xLab, yLab)
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
      
      if (nrow(res) > 0) res$replicate <- self$get_replicate_names()[res$analysis]
      
      res <- res[res$analysis %in% analyses, ]
      
      if (nrow(res) == 0) {
        message("\U2717 Spectra charges not found for the targets!")
        return(NULL)
      }
      
      spec <- self$spectra
      
      spec <- rbindlist(spec, fill = TRUE)
      
      if ("mass" %in% colnames(spec)) spec <- self$get_spectra(loaded_spectra = TRUE, raw_spectra = TRUE)
      
      setorder(spec, mz)
      
      if (!interactive) {
        .plot_spec_charges_static(spec, res, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
      } else {
        .plot_spec_charges_interactive(spec, res, legendNames, colorBy, title, showLegend, xLab, yLab)
      }
    },

    #' @description Plots deconvoluted spectra in given MS analyses.
    #'
    plot_deconvoluted_spectra = function(analyses = NULL,
                                         legendNames = NULL,
                                         colorBy = "analyses",
                                         xLab = NULL,
                                         yLab = NULL,
                                         title = NULL,
                                         cex = 0.6,
                                         showLegend = TRUE,
                                         interactive = TRUE) {
      
      if (!self$has_deconvoluted_spectra()) {
        message("\U2717 Deconvoluted spectra not found!")
        return(NULL)
      }
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(NULL)
      
      spec <- self$deconvoluted_spectra
      
      spec <- spec[analyses]
      
      spec <- rbindlist(spec, fill = TRUE)
      
      if (nrow(spec) == 0) {
        message("\U2717 Deconvoluted spectra not found!")
        return(NULL)
      }
      
      if ("replicates" %in% colorBy) spec$replicate <- self$get_replicate_names()[spec$analysis]
      
      spec <- .make_colorBy_varkey(spec, colorBy, legendNames = NULL)
      
      if (is.null(xLab)) xLab = "Mass / Da"
      
      if (is.null(yLab)) yLab = "Intensity / counts"
      
      spec$x <- spec$mass
      
      if (!interactive) {
        .plot_x_spectra_static(spec, xLab, yLab, title, cex, showLegend)
      } else {
        .plot_x_spectra_interactive(spec, xLab, yLab, title, colorBy)
      }
    },
    
    #' @description Plots peaks from spectra from analyses.
    #'
    plot_spectra_peaks = function(analyses = NULL,
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
      
      if (!self$has_spectra_peaks()) return(NULL)
      
      analyses <- private$.check_analyses_argument(analyses)
      
      if (is.null(analyses)) return(NULL)
      
      pks <- self$spectra_peaks
      
      pks <- pks[pks$analysis %in% analyses, ]
      
      if (nrow(pks) == 0) {
        message("\U2717 Peaks not found for the targets!")
        return(NULL)
      }
      
      setnames(pks, c("mass", "massmin", "massmax"), c("rt", "rtmin", "rtmax"), skip_absent = TRUE)
      
      sp_data <- self$get_results("spectra")
      sp_data <- sp_data$spectra$data
      sp_data <- sp_data[unique(pks$analysis)]
      
      if (self$has_averaged_spectra()) {
        spec <- lapply(sp_data, function(x) x$average)
        spec <- rbindlist(spec, fill = TRUE)
        if ("rt" %in% colnames(spec)) spec$rt <- NULL
        setnames(spec, c("mass", "massmin", "massmax"), c("rt", "rtmin", "rtmax"), skip_absent = TRUE)
        setnames(spec, c("mz", "mzmin", "mzmax"), c("rt", "rtmin", "rtmax"), skip_absent = TRUE)
        
      } else {
        spec <- lapply(sp_data, function(x) x$raw)
        spec <- rbindlist(spec, fill = TRUE)
        if ("rt" %in% colnames(spec)) spec$rt <- NULL
        setnames(spec, c("mass", "massmin", "massmax"), c("rt", "rtmin", "rtmax"), skip_absent = TRUE)
        setnames(spec, c("mz", "mzmin", "mzmax"), c("rt", "rtmin", "rtmax"), skip_absent = TRUE)
      }
      
      if ("smoothed" %in% colnames(spec)) {
        spec$raw <- spec$smoothed
      }
      
      ids <- spec$id
      names(ids) <- spec$analysis
      ids <- ids[!duplicated(names(ids))]
      
      pks$id = ids[pks$analysis]
      
      if (is.null(xLab)) xLab <- "Mass / Da"
      if (is.null(yLab)) yLab <- "Intensity"
      
      if (!interactive) {
        .plot_chrom_peaks_static(spec, pks, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
      } else {
        .plot_chrom_peaks_interactive(spec, pks, legendNames, colorBy, title, showLegend, xLab, yLab)
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

    ## ___ report -----

    #' @description Saves the HTML report from the function \link[patRoon]{report} from the package \pkg{patRoon}. 
    #' The interface is exactly the same and the arguments description are taken from the documentation in \pkg{patRoon}.
    #' Therefore, for further information, we recommend to consult directly the function \link[patRoon]{report} in \pkg{patRoon}.
    #'
    #' @param path Character (length 1) with the path to the report destination.
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
      ps <- list()
      ps[["CentroidSpectra"]] <- 1
      ps[["BinSpectra"]] <- 1
      ps[["FindFeatures"]] <- 1
      ps[["AnnotateFeatures"]] <- 1
      ps[["LoadFeaturesEIC"]] <- 1
      ps[["LoadFeaturesMS1"]] <- 1
      ps[["LoadFeaturesMS2"]] <- 1
      ps[["LoadMSPeakLists"]] <- 1
      ps[["GroupFeatures"]] <- 1
      ps[["FillFeatures"]] <- 1
      ps[["FilterFeatures"]] <- Inf
      ps[["SuspectScreening"]] <- 1
      ps[["FindInternalStandards"]] <- 1
      ps[["CalculateQuality"]] <- 1
      ps[["GenerateFormulas"]] <- 1
      ps[["GenerateCompounds"]] <- 1
      ps[["SmoothChromatograms"]] <- 1
      ps[["CorrectChromatogramsBaseline"]] <- 1
      ps[["IntegrateChromatograms"]] <- 1
      ps[["ClusterSpectra"]] <- 1
      ps[["CalculateSpectraCharges"]] <- 1
      ps[["DeconvoluteSpectra"]] <- 1
      ps[["SmoothSpectra"]] <- Inf
      ps[["CorrectSpectraBaseline"]] <- 1
      ps[["NormalizeSpectra"]] <- Inf
      ps[["AverageSpectra"]] <- 1
      ps[["SubtractBlankSpectra"]] <- 1
      ps[["NormalizeFeatures"]] <- 1
      data.table(name = names(ps), max = unlist(ps))
    },

    ### ___ help -----

    #' @field help (`list()`)\cr
    #' List of function elements to access specific reference help pages.
    help = list(
      methods = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/MassSpecEngine.html#methods")
      }
    )
  )
)

# _ import MassSpecEngine class -----

#' Function to import a MassSpecEngine class object from a *json* or *rds* file.
#'
#' @description Function to import a `MassSpecEngine` class object from a saved *json* or *rds* file.
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

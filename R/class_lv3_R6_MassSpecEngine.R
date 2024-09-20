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
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-allTraces
#' @template arg-ms-isolationWindow
#' @template arg-ms-minIntensityMS1
#' @template arg-ms-minIntensityMS2
#' @template arg-ms-useRawData
#' @template arg-ms-useLoadedData
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
  
  # _ active bindings -----
  
  active = list(
    
    #' @field nts Get/Set for the `NTS` results class.
    #' 
    nts = function(value) {
      if (missing(value)) return(self$results[["nts"]])
      if (is(value, "StreamFind::NTS")) {
        self$analyses$nts <- value
      } else {
        warning("Value must be an NTS results object! Not done.")
      }
      invisible(self)
    },
    
    #' @field spectra Get/set for the `Spectra` results class.
    #'
    spectra = function(value) {
      if (missing(value)) return(self$analyses$spectra)
      if (is(value, "StreamFind::Spectra")) {
        self$analyses$spectra <- value
      } else {
        warning("Value must be a Spectra object! Not done.") 
      }
      invisible(self)
    },
    
    #' @field chromatograms Get/set for the chromatograms results class.
    #'
    chromatograms = function(value) {
      if (missing(value)) return(self$analyses$chromatograms)
      if (is(value, "StreamFind::Chromatograms")) {
        self$analyses$chromatograms <- value
      } else {
        warning("Value must be a Chromatograms object! Not done.") 
      }
      invisible(self)
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
    #' @param file Character of length one with the full path to the `sqlite` save file of the engine.
    #' @param headers A `ProjectHeaders` S7 class object.
    #' @param analyses A `MassSpecAnalyses` S7 class object or a `character vector` with full file paths to ms files or 
    #' a `data.frame` as described in `?MassSpecAnalyses`.
    #' @param workflow A `Workflow` S7 class object.
    #'
    #' @return A new MassSpecEngine class object.
    #'
    initialize = function(file = NULL, headers = NULL, workflow = NULL, analyses = NULL) {
      super$initialize(file, headers, workflow, analyses)
      invisible(self)
      # private$.register("created", "MassSpecEngine", headers$name, paste(c(headers$author, headers$path), collapse = ", "))
    },
    
    ## ___ get -----
    
    #' @description Gets the analysis replicate names.
    #'
    get_analysis_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$names[analyses]
    },
    
    #' @description Gets the analysis replicate names.
    #'
    get_replicate_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$replicates[analyses]
    },
    
    #' @description Gets the analysis blank replicate names.
    #'
    get_blank_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$blanks[analyses]
    },
    
    #' @description Gets the full file paths of each analysis.
    #'
    get_files = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$files[analyses]
    },
    
    #' @description Gets the file format of each analysis.'
    #'
    get_formats = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$formats[analyses]
    },
    
    #' @description Gets the instruments information of each analysis.
    #'
    #' @return A list.
    #'
    get_instruments = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$instruments[analyses]
    },
    
    #' @description Gets the software information of each analysis.
    #'
    #' @return A list.
    #'
    get_software = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$software[analyses]
    },
    
    #' @description Gets the number of spectra in each analysis.
    #'
    #' @return A character vector.
    #'
    get_spectra_number = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$spectra_number[analyses]
    },
    
    #' @description Gets the spectra mode of each analysis (i.e., profile or centroid).
    #'
    #' @return A character vector.
    #'
    get_spectra_mode = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$spectra_mode[analyses]
    },
    
    #' @description Gets the spectra levels of each analysis.
    #'
    #' @return A list for each analysis with an integer vector.
    #'
    get_spectra_level = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$spectra_level[analyses]
    },
    
    #' @description Gets the lower \emph{m/z} value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_spectra_lowest_mz = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$spectra_lowest_mz[analyses]
    },
    
    #' @description Gets the higher \emph{m/z} value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_spectra_highest_mz = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$spectra_highest_mz[analyses]
    },
    
    #' @description Gets the start retention time value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_spectra_lowest_rt = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$spectra_lowest_rt[analyses]
    },
    
    #' @description Gets the end retention time value of each analysis.
    #'
    #' @return A character vector.
    #'
    get_spectra_highest_rt = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$spectra_highest_rt[analyses]
    },
    
    #' @description Gets the polarity of each analysis.
    #'
    #' @return A character vector.
    #'
    get_spectra_polarity = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$spectra_polarity[analyses]
    },
    
    #' @description Gets the spectra headers data.table of each analysis.
    #'
    #' @return A data.table.
    #'
    get_spectra_headers = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      value <- self$analyses$spectra_headers[analyses]
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
      get_spectra_tic(self$analyses, analyses, levels, rt)
    },
    
    #' @description Gets the base peak chromatogram (BPC) of each analysis.
    #' 
    #' @param rt Numeric (length 2). The retention time range to filter the BPC.
    #'
    #' @return A character vector.
    #'
    get_spectra_bpc = function(analyses = NULL, levels = c(1, 2), rt = NULL) {
      get_spectra_bpc(self$analyses, analyses, levels, rt)
    },
    
    #' @description Gets spectra from each analysis.
    #'
    get_spectra = function(analyses = NULL,
                           levels = NULL,
                           mass = NULL,
                           mz = NULL,
                           rt = NULL,
                           mobility = NULL,
                           ppm = 20,
                           sec = 60,
                           millisec = 5,
                           id = NULL,
                           allTraces = TRUE,
                           isolationWindow = 1.3,
                           minIntensityMS1 = 0,
                           minIntensityMS2 = 0,
                           useRawData = TRUE,
                           useLoadedData = TRUE) {
      
      get_spectra(self$analyses, analyses, levels, mass, mz, rt, mobility, ppm, sec, millisec, id, allTraces, 
                  isolationWindow, minIntensityMS1, minIntensityMS2, useRawData, useLoadedData)
    },
    
    #' @description Gets a matrix with spectra from analyses.
    #' 
    get_spectra_matrix = function(analyses = NULL) {
      
      analyses <- .check_analyses_argument(self$analyses, analyses)
      
      mat <- self$get_spectra(analyses)
      
      if (nrow(mat) == 0) return(matrix())
      
      mat <- mat[order(mat$analysis), ]
      
      if ("bins" %in% colnames(mat)) {
        col_key <- unique(mat$bins)
      } else if ("mobility" %in% colnames(mat)) {
        col_key <- unique(paste0("r", mat$rt, "_m", mat$mz, "_d", mat$mobility, "_p", mat$polarity, "_l", mat$level))
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
                               mobility = NULL,
                               ppm = 20,
                               sec = 60,
                               millisec = 5,
                               id = NULL,
                               useRawData = TRUE,
                               useLoadedData = FALSE) {
      
      get_spectra_eic(self$analyses, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id, useRawData, useLoadedData)
    },
    
    #' @description Gets level 1 spectra from the analyses based on targets as a data.frame.
    #'
    get_spectra_ms1 = function(analyses = NULL,
                               mass = NULL,
                               mz = NULL,
                               rt = NULL,
                               mobility = NULL,
                               ppm = 20,
                               sec = 60,
                               millisec = 5,
                               id = NULL,
                               mzClust = 0.003,
                               presence = 0.8,
                               minIntensity = 1000,
                               useRawData = TRUE,
                               useLoadedData = FALSE) {
      
      get_spectra_ms1(self$analyses, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id, mzClust, presence, minIntensity, useRawData, useLoadedData)
    },
    
    #' @description Gets level 2 spectra from the analyses based on targets as a data.frame.
    #'
    get_spectra_ms2 = function(analyses = NULL,
                               mass = NULL,
                               mz = NULL,
                               rt = NULL,
                               mobility = NULL,
                               ppm = 20,
                               sec = 60,
                               millisec = 5,
                               id = NULL,
                               isolationWindow = 1.3,
                               mzClust = 0.005,
                               presence = 0.8,
                               minIntensity = 0,
                               useRawData = TRUE,
                               useLoadedData = FALSE) {
      
      get_spectra_ms2(self$analyses, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id, isolationWindow, mzClust, presence, minIntensity, useRawData, useLoadedData)
    },
    
    #' @description Gets the number of chromatograms in each analysis.
    #'
    get_chromatograms_number = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses$chromatograms_number[analyses]
    },
    
    #' @description Gets the chromatograms headers data.table of each analysis.
    #'
    get_chromatograms_headers = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      value <- self$analyses$chromatograms_headers[analyses]
      value <- rbindlist(value, idcol = "analysis", fill = TRUE)
      value
    },
    
    #' @description Gets chromatograms from each analysis.
    #'
    get_chromatograms = function(analyses = NULL,
                                 chromatograms = NULL,
                                 minIntensity = NULL,
                                 useRawData = FALSE,
                                 useLoadedData = TRUE) {
      get_chromatograms(self$analyses, analyses, chromatograms, minIntensity, useRawData, useLoadedData)
    },
    
    #' @description Gets integrated peaks from chromatograms.
    #'
    get_chromatograms_peaks = function(analyses = NULL, chromatograms = NULL) {
      get_chromatograms_peaks(self$analyses, analyses, chromatograms)
    },
    
    #' @description Gets a data.table with all features from NTS results or as selected by the arguments.
    #'
    get_features = function(analyses = NULL,
                            features = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            mobility = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            filtered = FALSE) {
      
      get_features(self$analyses, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
    },
    
    #' @description Gets a data.table with feature EICs following the targets from the arguments.
    #'
    get_features_eic = function(analyses = NULL,
                                features = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                mobility = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                rtExpand = 120,
                                mzExpand = NULL,
                                filtered = FALSE,
                                useLoadedData = TRUE) {
      
      get_features_eic(self$analyses, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, rtExpand, mzExpand, filtered, useLoadedData)
    },
    
    #' @description Gets a data.table of averaged MS1 spectrum for features in the analyses or as selected from the 
    #' arguments.
    #'
    get_features_ms1 = function(analyses = NULL,
                                features = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                mobility = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                rtWindow = c(-2, 2),
                                mzWindow = c(-5, 100),
                                mzClust = 0.003,
                                presence = 0.8,
                                minIntensity = 1000,
                                filtered = FALSE,
                                useLoadedData = TRUE) {
      
      get_features_ms1(self$analyses, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, rtWindow, mzWindow, mzClust, presence, minIntensity, filtered, useLoadedData)
    },
    
    #' @description Gets a data.table of averaged MS2 spectrum for features in the analyses or as selected from the 
    #' arguments.
    #'
    get_features_ms2 = function(analyses = NULL,
                                features = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                mobility = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                isolationWindow = 1.3,
                                mzClust = 0.003,
                                presence = 0.8,
                                minIntensity = 0,
                                filtered = FALSE,
                                useLoadedData = TRUE) {
      
      get_features_ms2(self$analyses, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, isolationWindow, mzClust, presence, minIntensity, filtered, useLoadedData)
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
                          mobility = NULL,
                          ppm = 20,
                          sec = 60,
                          millisec = 5,
                          filtered = FALSE,
                          intensities = TRUE,
                          average = FALSE,
                          sdValues = FALSE,
                          metadata = FALSE) {
      
      get_groups(self$analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered, intensities, average, sdValues, metadata)
    },
    
    #' @description Gets a data.table of averaged MS1 spectrum for feature groups in the analyses.
    #'
    get_groups_ms1 = function(groups = NULL,
                              mass = NULL,
                              mz = NULL,
                              rt = NULL,
                              mobility = NULL,
                              ppm = 20,
                              sec = 60,
                              millisec = 5,
                              rtWindow = c(-2, 2),
                              mzWindow = c(-5, 90),
                              mzClustFeatures = 0.003,
                              presenceFeatures = 0.8,
                              minIntensityFeatures = 1000,
                              useLoadedData = TRUE,
                              mzClust = 0.003,
                              presence = 0.8,
                              minIntensity = 1000,
                              groupBy = "groups",
                              filtered = FALSE) {
      
      get_groups_ms1(self$analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, rtWindow, mzWindow, mzClustFeatures, presenceFeatures, minIntensityFeatures, useLoadedData, mzClust, presence, minIntensity, groupBy, filtered)
    },
    
    #' @description Gets a data.table of averaged MS2 spectrum for feature groups in the analyses.
    #'
    get_groups_ms2 = function(groups = NULL,
                              mass = NULL,
                              mz = NULL,
                              rt = NULL,
                              mobility = NULL,
                              ppm = 20,
                              sec = 60,
                              millisec = 5,
                              isolationWindow = 1.3,
                              mzClustFeatures = 0.003,
                              presenceFeatures = 0.8,
                              minIntensityFeatures = 100,
                              useLoadedData = TRUE,
                              mzClust = 0.003,
                              presence = 0.8,
                              minIntensity = 100,
                              groupBy = "groups",
                              filtered = FALSE) {
      
      get_groups_ms2(self$analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, isolationWindow, mzClustFeatures, presenceFeatures, minIntensityFeatures, useLoadedData, mzClust, presence, minIntensity, groupBy, filtered)
    },
    
    #' @description Creates S4 class `MSPeakLists`. Note that feature groups are required. The MS and MSMS spectra 
    #' of each feature are then average by \pkg{patRoon} to produce the feature group spectra using the parameters 
    #' of the function \link[patRoon]{getDefAvgPListParams}.
    #' 
    #' @param useLoadedData Logical of length one. When `TRUE` and both MS1 and MS2 are loaded to features, 
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
    get_MSPeakLists = function(useLoadedData = TRUE,
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
      
      if (!useLoadedData) {
        
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
          self$nts$features,
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
    
    #' @description Gets feature components (i.e., isotope and adduct related to a main feature) in the analyses.
    #'
    get_components = function(analyses = NULL,
                              features = NULL,
                              mass = NULL,
                              mz = NULL,
                              rt = NULL,
                              mobility = NULL,
                              ppm = 20,
                              sec = 60,
                              millisec = 5,
                              filtered = FALSE) {
      
      get_components(self$analyses, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
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
                            mobility = NULL,
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
      
      get_suspects(ms$analyses, analyses, database, features, mass, mz, rt, mobility, ppm, sec, millisec, 
                   ppmMS2, minFragments, isolationWindow, mzClust, presence, minIntensity, filtered, onGroups)
    },
    
    #' @description Gets a data.table with internal standards found by the `find_internal_standards` module.
    #'
    #' @param average Logical of length one. When `TRUE` and groups are present, internal standards are averaged per 
    #' analysis replicate group.
    #'
    get_internal_standards = function(average = TRUE) {
      get_internal_standards(self$analyses, average)
    },
    
    #' @description Gets metadata from each analysis.
    #'
    #' @return A data.table.
    #'
    get_metadata = function(analyses = NULL) {
      
      # TODO manage metadata
      
      # analyses <- .check_analyses_argument(self$analyses, analyses)
      # if (is.null(analyses)) return(data.table())
      # metadata <- lapply(private$.analyses[analyses], function(x) {
      #   as.data.table(x$metadata)
      # })
      # metadata <- rbindlist(metadata, idcol = "analysis", fill = TRUE)
      # 
      # metadata
    },
    
    ## ___ add/remove -----
    
    #' @description Adds analyses. Note that when adding new analyses, any existing results are removed.
    #'
    #' @param analyses A MassSpecAnalysis S3 class object or a list with MassSpecAnalysis S3 class objects as 
    #' elements (see `?MassSpecAnalysis` for more information) or a character vector with full path to mzML/mzXML files.
    #'
    #' @return Invisible.
    #'
    add_analyses = function(analyses = NULL) {
      self$analyses <- add(self$analyses, analyses)
      invisible(self)
    },
    
    #' @description Removes analyses.
    #'
    #' @return Invisible.
    #'
    remove_analyses = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses <- remove(self$analyses, analyses)
      invisible(self)
    },
    
    ## ___ load -----
    
    #' @description Loads spectra from analyses.
    #'
    #' @return Invisible.
    #'
    load_spectra = function(analyses = NULL,
                            levels = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            mobility = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            id = NULL,
                            allTraces = TRUE,
                            isolationWindow = 1.3,
                            minIntensityMS1 = 0,
                            minIntensityMS2 = 0) {
      
      self$analyses <- load_spectra(self$analyses, analyses, levels, mass, mz, rt, mobility, ppm, sec, millisec, id, allTraces, isolationWindow, minIntensityMS1, minIntensityMS2)
      invisible(self)
    },
    
    #' @description Loads all chromatograms from all analyses.
    #'
    #' @return Invisible.
    #'
    load_chromatograms = function(analyses = NULL, minIntensity = NULL, chromatograms = NULL) {
      self$analyses <- load_chromatograms(self$analyses, analyses, minIntensity, chromatograms)
      invisible(self)
    },
    
    ## ___ remove -----
    
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
        feature_list <- nts$feature_list
        feature_list <- lapply(feature_list, function(x) {
          x[["ms1"]] <- NULL
          x
        })
        
        self$nts$feature_list <- feature_list
        message("\U2713 Removed all MS1 spectra from features!")
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
        feature_list <- nts$feature_list
        feature_list <- lapply(feature_list, function(x) {
          x[["ms2"]] <- NULL
          x
        })
        self$nts$feature_list <- feature_list
        message("\U2713 Removed all MS2 spectra from features!")
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
    
    #' @description Checks if there are NTS results, returning `TRUE` or `FALSE`.
    #' 
    has_nts = function() {
      if (is(self$nts, "StreamFind::NTS")) return(TRUE)
      FALSE
    },
    
    #' @description Checks if there are features from NTS results, returning `TRUE` or `FALSE`.
    #' 
    has_features = function() {
      if (self$has_nts()) if (self$nts$has_features) return(TRUE)
      FALSE
    },
    
    #' @description Checks if there are feature groups from NTS results, returning `TRUE` or `FALSE`.
    #' 
    has_groups = function() {
      if (self$has_nts()) if (self$nts$has_groups) return(TRUE)
      FALSE
    },
    
    #' @description Checks if there are MSPeakLists for analyses, returning `TRUE` or `FALSE`.
    #'
    has_MSPeakLists = function(analyses = NULL) {
      if (self$has_nts()) if (length(self$nts$mspl) > 0) return(TRUE)
      FALSE
    },
    
    #' @description Checks if there are formulas assigned to feature groups, returning `TRUE` or `FALSE`.
    #'
    has_formulas = function() {
      if (self$has_nts()) if (length(self$nts$formulas) > 0) return(TRUE)
      FALSE
    },
    
    #' @description Checks if there are compounds assigned to feature groups, returning `TRUE` or `FALSE`.
    #'
    has_compounds = function() {
      if (self$has_nts()) if (length(self$nts$compounds) > 0) return(TRUE)
      FALSE
    },
    
    #' @description Checks if there are chromatograms, returning `TRUE` or `FALSE`.
    #'
    has_chromatograms = function() {
      if (is(self$chromatograms, "StreamFind::Chromatograms")) return(TRUE)
      FALSE
    },
    
    #' @description Checks if there are integrated peaks from chromatograms, returning `TRUE` or `FALSE`.
    #'
    has_chromatograms_peaks = function() {
      if (self$has_chromatograms()) {
        if (length(self$chromatograms$peaks) > 0) return(TRUE)
      }
      FALSE
    },
    
    #' @description Checks if there are spectra, returning `TRUE` or `FALSE`.
    #'
    has_spectra = function() {
      if (is(self$spectra, "StreamFind::Spectra")) return(TRUE)
      FALSE
    },
    
    #' @description Checks if there are spectra peaks, returning `TRUE` or `FALSE`.
    #'
    has_spectra_peaks = function() {
      if (self$has_spectra()) {
        if (length(self$spectra$peaks) > 0) return(TRUE)
      }
      FALSE
    },
    
    #' @description Checks if there are spectra calculated charges, returning `TRUE` or `FALSE`.
    #'
    has_spectra_charges = function() {
      if (self$has_spectra()) {
        if (length(self$spectra$charges) > 0) return(TRUE)
      }
      FALSE
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
    
    ## ___ plot -----
    
    #' @description Plots raw spectra in 3D for given MS analyses.
    #' 
    #' @param xVal Character length one. Possible values are "mz", "rt" or "mobility".
    #' @param yVal Character length one. Possible values are "mz", "rt" or "mobility".
    #' @param zLab A string with the title for the z axis.
    #'
    plot_raw_data = function(analyses = NULL,
                                levels = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                mobility = NULL,
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
        mass, mz, rt, mobility, ppm, sec, millisec, id,
        allTraces = allTraces,
        isolationWindow,
        minIntensityMS1,
        minIntensityMS2
      )
      
      if (nrow(spec) == 0) {
        message("\U2717 Traces not found for the targets!")
        return(NULL)
      }
      
      if ("mobility" %in% c(xVal, yVal)) {
        if (!"mobility" %in% colnames(spec)) {
          warning("mobility time values not found!")
          return(NULL)
        }
      }
      
      if ("feature" %in% colnames(spec)) spec$id <- spec$feature
      
      if ("replicates" %in% colorBy) spec$replicate <- self$get_replicate_names()[spec$analysis]
      
      .plot_spectra_interactive(spec, colorBy, legendNames, xVal, yVal, xLab, yLab, zLab)
    },
    
    #' @description Plots spectra given MS analyses.
    #'
    #' @param xVal Character length one. Possible values are "mz", "rt", "mobility" or "mass".
    #' spectra from raw data files.
    #' @param averaged Logical of length one. Set to `TRUE` for plotting the averaged spectra.
    #' @param baseline Logical of length one. Set to `TRUE` for plotting the spectra with baseline corrected.
    #'
    plot_spectra = function(analyses = NULL,
                            levels = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            mobility = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            id = NULL,
                            allTraces = TRUE,
                            isolationWindow = 1.3,
                            minIntensityMS1 = 0,
                            minIntensityMS2 = 0,
                            useRawData = FALSE,
                            useLoadedData = TRUE,
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
      
      plot_spectra(ms$analyses, analyses, levels, mass, mz, rt, mobility, ppm, sec, millisec, id, allTraces, isolationWindow, minIntensityMS1, minIntensityMS2, useRawData, useLoadedData, averaged, baseline, legendNames, colorBy, xVal, xLab, yLab, title, cex, showLegend, interactive)
    },
    
    #' @description Plots chromatograms in the analyses.
    #'
    plot_chromatograms = function(analyses = NULL,
                                  chromatograms = NULL,
                                  minIntensity = NULL,
                                  useRawData = FALSE,
                                  useLoadedData = TRUE,
                                  xLab = NULL,
                                  yLab = NULL,
                                  title = NULL,
                                  colorBy = "targets",
                                  legendNames = NULL,
                                  showLegend = TRUE,
                                  xlim = NULL,
                                  ylim = NULL,
                                  cex = 0.6,
                                  interactive = TRUE) {
      
      plot_chromatograms(self$analyses, analyses, chromatograms, minIntensity, useRawData, useLoadedData, xLab, yLab, title, colorBy, legendNames, showLegend, xlim, ylim, cex, interactive)
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
                                mobility = NULL,
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
        mz, rt, mobility, ppm, sec, millisec, id,
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
                                mobility = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                id = NULL,
                                legendNames = NULL,
                                xLab = NULL,
                                yLab = NULL,
                                title = NULL,
                                colorBy = "targets",
                                showLegend = TRUE,
                                xlim = NULL,
                                ylim = NULL,
                                cex = 0.6,
                                interactive = TRUE) {
      
      plot_spectra_eic(self$analyses, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id, legendNames, xLab, yLab, title, colorBy, showLegend, xlim, ylim, cex, interactive)
    },
    
    #' @description Plots the spectra total ion chromatogram (TIC) of each analysis.
    #' 
    #' @param downsize An integer of length one to downsize the TIC plot. The default is 1.
    #'
    plot_spectra_tic = function(analyses = NULL,
                                levels = c(1, 2),
                                rt = NULL,
                                xLab = NULL,
                                yLab = NULL,
                                title = NULL,
                                colorBy = "analyses",
                                legendNames = NULL,
                                showLegend = TRUE,
                                xlim = NULL,
                                ylim = NULL,
                                cex = 0.6,
                                downsize = 1,
                                interactive = TRUE) {
      
      plot_spectra_tic(self$analyses, analyses, levels, rt, xLab, yLab, title, colorBy, legendNames, showLegend, xlim, ylim, cex, downsize, interactive)
    },
    
    #' @description Plots the spectra base peak chromatogram (BPC) of each analysis.
    #'
    #' @return A plot.
    #'
    plot_spectra_bpc = function(analyses = NULL,
                                levels = c(1, 2),
                                rt = NULL,
                                xLab = NULL,
                                yLab = NULL,
                                title = NULL,
                                colorBy = "analyses",
                                legendNames = NULL,
                                showLegend = TRUE,
                                xlim = NULL,
                                ylim = NULL,
                                cex = 0.6,
                                interactive = TRUE) {
      
      plot_spectra_bpc(self$analyses, analyses, levels, rt, xLab, yLab, title, colorBy, legendNames, showLegend, xlim, ylim, cex, interactive)
    },

    #' @description Plots level 2 spectra from the analyses based on targets.
    #'
    plot_spectra_ms2 = function(analyses = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                mobility = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                id = NULL,
                                isolationWindow = 1.3,
                                mzClust = 0.005,
                                presence = 0.8,
                                minIntensity = 0,
                                legendNames = NULL,
                                xLab = NULL,
                                yLab = NULL,
                                title = NULL,
                                colorBy = "targets",
                                interactive = TRUE) {

      ms2 <- plot_spectra_ms2(self$analyses, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id, isolationWindow, mzClust, presence, minIntensity, legendNames, xLab, yLab, title, colorBy, interactive)
    },

    #' @description Plots level 1 spectra from the analyses based on targets.
    #'
    plot_spectra_ms1 = function(analyses = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                mobility = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                id = NULL,
                                mzClust = 0.003,
                                presence = 0.8,
                                minIntensity = 1000,
                                legendNames = NULL,
                                xLab = NULL,
                                yLab = NULL,
                                title = NULL,
                                colorBy = "targets",
                                showText = FALSE,
                                interactive = TRUE) {

      ms1 <- plot_spectra_ms1(self$analyses, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id, mzClust, presence, minIntensity, legendNames, xLab, yLab, title, colorBy, showText, interactive)
    },

    #' @description Plots features from analyses.
    #'
    plot_features = function(analyses = NULL,
                             features = NULL,
                             mass = NULL,
                             mz = NULL,
                             rt = NULL,
                             mobility = NULL,
                             ppm = 20,
                             sec = 60,
                             millisec = 5,
                             rtExpand = 120,
                             mzExpand = 0.001,
                             useLoadedData = TRUE,
                             filtered = FALSE,
                             legendNames = NULL,
                             xLab = NULL,
                             yLab = NULL,
                             title = NULL,
                             colorBy = "targets",
                             showLegend = TRUE,
                             xlim = NULL,
                             ylim = NULL,
                             cex = 0.6,
                             interactive = TRUE) {

      plot_features(self$analyses, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, rtExpand, mzExpand, useLoadedData, filtered, legendNames, xLab, yLab, title, colorBy, showLegend, xlim, ylim, cex, interactive)
    },

    #' @description Plots a map of the retention time vs \emph{m/z} of features from analyses.
    #'
    map_features = function(analyses = NULL,
                            features = NULL,
                            mass = NULL,
                            mz = NULL,
                            rt = NULL,
                            mobility = NULL,
                            ppm = 20,
                            sec = 60,
                            millisec = 5,
                            filtered = FALSE,
                            legendNames = NULL,
                            xLab = NULL,
                            yLab = NULL,
                            title = NULL,
                            colorBy = "targets",
                            showLegend = TRUE,
                            xlim = 30,
                            ylim = 0.05,
                            cex = 0.6,
                            interactive = TRUE) {

      map_features(self$analyses, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered, legendNames, xLab, yLab, title, colorBy, showLegend, xlim, ylim, cex, interactive)
    },

    #' @description Plots level 1 spectra from features in the analyses.
    #'
    plot_features_ms1 = function(analyses = NULL,
                                 features = NULL,
                                 mass = NULL,
                                 mz = NULL,
                                 rt = NULL,
                                 mobility = NULL,
                                 ppm = 20,
                                 sec = 60,
                                 millisec = 5,
                                 rtWindow = c(-2, 2),
                                 mzWindow = c(-5, 100),
                                 mzClust = 0.003,
                                 presence = 0.8,
                                 minIntensity = 1000,
                                 filtered = FALSE,
                                 useLoadedData = TRUE,
                                 legendNames = NULL,
                                 xLab = NULL,
                                 yLab = NULL,
                                 title = NULL,
                                 colorBy = "targets",
                                 interactive = TRUE) {

      plot_features_ms1(self$analyses, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, rtWindow, mzWindow, mzClust, presence, minIntensity, filtered, useLoadedData, legendNames, xLab, yLab, title, colorBy, interactive)
    },

    #' @description Plots level 2 spectra from features in the analyses.
    #'
    plot_features_ms2 = function(analyses = NULL,
                                 features = NULL,
                                 mass = NULL,
                                 mz = NULL,
                                 rt = NULL,
                                 mobility = NULL,
                                 ppm = 20,
                                 sec = 60,
                                 millisec = 5,
                                 isolationWindow = 1.3,
                                 mzClust = 0.005,
                                 presence = 0.8,
                                 minIntensity = 0,
                                 filtered = FALSE,
                                 useLoadedData = TRUE,
                                 legendNames = NULL,
                                 xLab = NULL,
                                 yLab = NULL,
                                 title = NULL,
                                 colorBy = "targets",
                                 interactive = TRUE) {

      plot_features_ms2(self$analyses, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, isolationWindow, mzClust, presence, minIntensity, filtered, useLoadedData, legendNames, xLab, yLab, title, colorBy, interactive)
    },

    #' @description Plots feature groups EIC.
    #'
    plot_groups = function(groups = NULL,
                           mass = NULL,
                           mz = NULL,
                           rt = NULL,
                           mobility = NULL,
                           ppm = 20,
                           sec = 60,
                           millisec = 5,
                           rtExpand = 15,
                           mzExpand = 0.001,
                           filtered = FALSE,
                           legendNames = NULL,
                           xLab = NULL,
                           yLab = NULL,
                           title = NULL,
                           colorBy = "targets",
                           showLegend = TRUE,
                           xlim = NULL,
                           ylim = NULL,
                           cex = 0.6,
                           interactive = TRUE) {

      plot_groups(self$analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, rtExpand, mzExpand, filtered, legendNames, xLab, yLab, title, colorBy, showLegend, xlim, ylim, cex, interactive)
    },

    #' @description Plots level 1 spectra from feature groups in the analyses.
    #'
    plot_groups_ms1 = function(groups = NULL,
                               mass = NULL,
                               mz = NULL,
                               rt = NULL,
                               mobility = NULL,
                               ppm = 20,
                               sec = 60,
                               millisec = 5,
                               rtWindow = c(-2, 2),
                               mzWindow = c(-5, 90),
                               mzClustFeatures = 0.005,
                               presenceFeatures = 0.8,
                               minIntensityFeatures = 1000,
                               useLoadedData = TRUE,
                               mzClust = 0.005,
                               presence = 0.8,
                               minIntensity = 1000,
                               groupBy = "groups",
                               filtered = FALSE,
                               legendNames = NULL,
                               xLab = NULL,
                               yLab = NULL,
                               title = NULL,
                               colorBy = "targets",
                               interactive = TRUE) {

      plot_groups_ms1(self$analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, rtWindow, mzWindow, mzClustFeatures, presenceFeatures, minIntensityFeatures, useLoadedData, mzClust, presence, minIntensity, groupBy, filtered, legendNames, xLab, yLab, title, colorBy, interactive)
    },

    #' @description Plots level 1 spectra from feature groups in the analyses.
    #'
    plot_groups_ms2 = function(groups = NULL,
                               mass = NULL,
                               mz = NULL,
                               rt = NULL,
                               mobility = NULL,
                               ppm = 20,
                               sec = 60,
                               millisec = 5,
                               isolationWindow = 1.3,
                               mzClustFeatures = 0.003,
                               presenceFeatures = 0.8,
                               minIntensityFeatures = 100,
                               useLoadedData = TRUE,
                               mzClust = 0.003,
                               presence = TRUE,
                               minIntensity = 100,
                               groupBy = "groups",
                               filtered = FALSE,
                               legendNames = NULL,
                               xLab = NULL,
                               yLab = NULL,
                               title = NULL,
                               colorBy = "targets",
                               interactive = TRUE) {
      
      plot_groups_ms2(self$analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, isolationWindow, mzClustFeatures, presenceFeatures, minIntensityFeatures, useLoadedData, mzClust, presence, minIntensity, groupBy, filtered, legendNames, xLab, yLab, title, colorBy, interactive)
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
                                    mobility = NULL,
                                    ppm = 20,
                                    sec = 60,
                                    millisec = 5,
                                    rtExpand = 120,
                                    mzExpand = 0.001,
                                    useLoadedData = TRUE,
                                    filtered = FALSE,
                                    legendNames = NULL,
                                    title = NULL,
                                    heights = c(0.35, 0.5, 0.15)) {

      plot_groups_overview(self$analyses, analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, rtExpand, mzExpand, useLoadedData, filtered, legendNames, title, heights)
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
                                   mobility = NULL,
                                   ppm = 20,
                                   sec = 60,
                                   millisec = 5,
                                   filtered = FALSE,
                                   normalized = TRUE,
                                   legendNames = NULL,
                                   yLab = NULL,
                                   title = NULL) {
      
      plot_groups_profile(self$analyses, analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered, normalized, legendNames, yLab, title)
    },
    
    #' @description Maps feature components in the analyses.
    #'
    map_components = function(analyses = NULL,
                              features = NULL,
                              mass = NULL,
                              mz = NULL,
                              rt = NULL,
                              mobility = NULL,
                              ppm = 20,
                              sec = 60,
                              millisec = 5,
                              filtered = FALSE,
                              xlim = 30,
                              ylim = 0.05,
                              showLegend = TRUE,
                              legendNames = NULL,
                              xLab = NULL,
                              yLab = NULL,
                              title = NULL,
                              colorBy = "targets",
                              interactive = TRUE) {
      
      map_components(self$analyses, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered, xlim, ylim, showLegend, legendNames, xLab, yLab, title, colorBy, interactive)
    },

    #' @description Plots the quality control assessment of the internal standards.
    #' 
    #' @param presence Logical (length 1). When `TRUE` the presence of the internal standards is plotted.
    #' @param recovery Logical (length 1). When `TRUE` the recovery of the internal standards is plotted.
    #' @param deviations Logical (length 1). When `TRUE` the deviations of the internal standards is plotted.
    #' @param widths Logical (length 1). When `TRUE` the widths of the internal standards is plotted.
    #'
    plot_internal_standards = function(analyses = NULL, presence = TRUE, recovery = TRUE, deviations = TRUE, widths = TRUE) {
      plot_internal_standards(self$analyses, analyses)
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
                             mobility = NULL,
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
                             useLoadedData = TRUE,
                             colorBy = "targets") {
      
      if (any(self$has_suspects())) {
        
        suspects <- self$get_suspects(
          analyses,
          database,
          features,
          mass,
          mz,
          rt,
          mobility,
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
          useLoadedData = useLoadedData
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
      
      plot_chromatograms_peaks(ms$analyses, analyses, chromatograms, legendNames, title, colorBy, showLegend, xlim, ylim, cex, xLab, yLab, interactive)
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
      
      analyses <- .check_analyses_argument(self$analyses, analyses)
      
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
      
      if ("mass" %in% colnames(spec)) spec <- self$get_spectra(useLoadedData = TRUE, useRawData = TRUE)
      
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
      
      analyses <- .check_analyses_argument(self$analyses, analyses)
      
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
      
      analyses <- .check_analyses_argument(self$analyses, analyses)
      
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

      if (length(self$analyses) == 0) {
        warning("There are no MS analyses!")
        return(invisible(self))
      }

      if (!self$analyses$has_nts) {
        warning("No NTS results found!")
        return(invisible(self))
      }
      
      if (!self$analyses$nts$has_groups) {
        warning("No NTS feature groups found!")
        return(invisible(self))
      }
      
      nts <- self$analyses$nts

      patRoon::report(
        nts$features,
        nts$mspl,
        formulas = nts$formulas,
        compounds = nts$compounds,
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
    }
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

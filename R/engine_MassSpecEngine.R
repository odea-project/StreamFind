# MARK: MassSpecEngine
# MassSpecEngine -----
#' Mass Spectrometry Engine
#' 
#' @description The `MassSpecEngine` R6 class is a framework for parsing, processing and inspecting
#' mass spectrometry (MS) data. MS data (i.e., spectra and chromatograms, including
#' chromatograms produced by UV detection) can be loaded from *mzML* and *mzXML*. If `msconvert`
#' from \href{https://proteowizard.sourceforge.io/}{ProteoWizard} is installed and found via CLI
#' (i.e., must be added to the environmental variables), the engine can also load vendor formats
#' (i.e., Agilent MassHunter .d, Thermo Scientific RAW, Shimadzu LCD (except ITOF), Sciex WIFF /
#' WIFF2) by direct conversion to mzML. Note that conversion of vendor formats is only possible
#' under Windows OS.
#' 
#' @details The `MassSpecEngine` is using several patRoon \href{https://github.com/rickhelmus/patRoon}{patRoon} for assembly of Non-Target Analysis (NTA) data processing workflows.
#' 
#' @template arg-core-metadata
#' @template arg-core-workflow
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
#' @template arg-useRawData
#' @template arg-useLoadedData
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-rtmin
#' @template arg-ms-rtmax
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
#' @template arg-ms-correctIntensity
#' @template arg-renderEngine
#' 
#' @references
#' \insertRef{patroon01}{StreamFind}
#' 
#' \insertRef{patroon02}{StreamFind}
#' 
#' \insertRef{pugixml01}{StreamFind}
#' 
#' \insertRef{proteo01}{StreamFind}
#' 
#' \insertRef{proteo02}{StreamFind}
#' 
#' @export
#' 
MassSpecEngine <- R6::R6Class("MassSpecEngine",
  inherit = CoreEngine,

  # MARK: active_bindings
  # Active Bindings -----
  active = list(

    # MARK: NonTargetAnalysisResults
    ## NonTargetAnalysisResults -----
    #' @field NonTargetAnalysisResults Get/Set for the `NonTargetAnalysisResults` results class.
    NonTargetAnalysisResults = function(value) {
      if (missing(value)) {
        return(self$Analyses@results[["NonTargetAnalysisResults"]])
      }
      if (is(value, "StreamFind::NonTargetAnalysisResults")) {
        self$Analyses@NonTargetAnalysisResults <- value
      } else {
        warning("Value must be an NonTargetAnalysisResults results object! Not done.")
      }
      invisible(self)
    },

    # MARK: Spectra
    ## Spectra -----
    #' @field Spectra Get/set for the `Spectra` results class.
    Spectra = function(value) {
      if (missing(value)) {
        return(self$Analyses@Spectra)
      }
      if (is(value, "StreamFind::MassSpecSpectra")) {
        self$Analyses@Spectra <- value
      } else {
        warning("Value must be a Spectra object! Not done.")
      }
      invisible(self)
    },

    # MARK: Chromatograms
    ## Chromatograms -----
    #' @field Chromatograms Get/set for the chromatograms results class.
    Chromatograms = function(value) {
      if (missing(value)) {
        return(self$Analyses@Chromatograms)
      }
      if (is(value, "StreamFind::Chromatograms")) {
        self$Analyses@Chromatograms <- value
      } else {
        warning("Value must be a Chromatograms object! Not done.")
      }
      invisible(self)
    }
  ),

  # MARK: public fields
  # Public Fields -----
  public = list(

    # MARK: initialize
    ## initialize -----
    #' @description Creates an R6 `MassSpecEngine` class object.
    #'
    #' @param analyses A `MassSpecAnalyses` S7 class object or a `character vector` with full file
    #' paths to mzML files or a `data.frame` with `colnames`: "file", "replicate" and "blank". The
    #' "replicate" column is used to group the analyses and the "blank" column is used to identify
    #' the blank samples. The "file" column is the full path to the mzML files. If `msconvert` from
    #' \href{https://proteowizard.sourceforge.io/}{ProteoWizard} is installed and found via CLI 
    #' (i.e., must be added to the environmental variables), the engine can also load vendor formats
    #' (i.e., Agilent MassHunter .d, Thermo Scientific RAW, Shimadzu LCD (except ITOF), Sciex WIFF
    #' / WIFF2) by direct conversion to mzML. Note that conversion of vendor formats is only
    #' possible under Windows OS.
    #' @param centroid Logical (length 1). Set to `TRUE` to centroid data when converting from
    #' vendor formats to mzML.
    #' @param levels Numeric vector with the MS levels to consider when centroiding data. Default
    #' is `c(1, 2)`.
    #' 
    #' @seealso [StreamFind::CoreEngine]
    #'
    #' @return A new MassSpecEngine class object.
    #'
    initialize = function(metadata = NULL,
                          workflow = NULL,
                          analyses = NULL,
                          centroid = FALSE,
                          levels = c(1, 2)) {
      super$initialize(metadata, workflow, analyses, centroid, levels)
      invisible(self)
    },
    
    # MARK: add_analyses
    ## add_analyses -----
    #' @description Adds analyses. Note that when adding new analyses, any existing results are
    #' removed.
    #'
    #' @param analyses A character vector with full file paths to mzML/mzXML files. f `msconvert`
    #' from \href{https://proteowizard.sourceforge.io/}{ProteoWizard} is installed and found via CLI 
    #' (i.e., must be added to the environmental variables), the engine can also load vendor formats
    #' (i.e., Agilent MassHunter .d, Thermo Scientific RAW, Shimadzu LCD (except ITOF), Sciex WIFF
    #' / WIFF2) by direct conversion to mzML. Note that conversion of vendor formats is only
    #' possible under Windows OS.
    #'
    add_analyses = function(analyses = NULL) {
      self$Analyses <- add(self$Analyses, analyses)
      invisible(self)
    },
    
    # MARK: remove_analyses
    ## remove_analyses -----
    #' @description Removes analyses.
    remove_analyses = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses <- remove(self$Analyses, analyses)
      invisible(self)
    },
    
    # MARK: add_replicate_names
    ## add_replicate_names -----
    #' @description Adds replicate names to the analysis.
    #' 
    #' @param value Character vector with the replicate names. Must have the same length as the
    #' number of analyses.
    #' 
    add_replicate_names = function(value) {
      self$Analyses@replicates <- value
      invisible(self)
    },
    
    # MARK: add_blank_names
    ## add_blank_names -----
    #' @description Adds blank names to the analysis.
    #' 
    #' @param value Character vector with the replicate names. Must have the same length as the
    #' number of analyses and must be one of replicate names.
    #' 
    add_blank_names = function(value) {
      self$Analyses@blanks <- value
      invisible(self)
    },

    # MARK: get_analysis_names
    ## get_analysis_names -----
    #' @description Gets a character vector with the analysis replicate names.
    get_analysis_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      names(self$Analyses)[analyses]
    },

    # MARK: get_replicate_names
    ## get_replicate_names -----
    #' @description Gets a character vector with the analysis replicate names.
    get_replicate_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@replicates[analyses]
    },

    # MARK: get_blank_names
    ## get_blank_names -----
    #' @description Gets a character vector with the analysis blank replicate names.
    get_blank_names = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@blanks[analyses]
    },

    # MARK: get_files
    ## get_files -----
    #' @description Gets a character vector with the full file paths of each analysis.
    get_files = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@files[analyses]
    },

    # MARK: get_formats
    ## get_formats -----
    #' @description Gets a character vector with the file format of each analysis.'
    get_formats = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@formats[analyses]
    },

    # MARK: get_instruments
    ## get_instruments -----
    #' @description Gets a list of instrument information from each analysis as elements.
    get_instruments = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@instruments[analyses]
    },

    # MARK: get_software
    ## get_software -----
    #' @description Gets a list of software information from each analysis as elements.
    get_software = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@software[analyses]
    },

    # MARK: get_spectra_number
    ## get_spectra_number -----
    #' @description Gets a numeric vector with the number of spectra in each analysis.
    get_spectra_number = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@spectra_number[analyses]
    },

    # MARK: get_spectra_mode
    ## get_spectra_mode -----
    #' @description Gets a character vector with the spectra mode of each analysis
    #' (i.e., profile or centroid).
    get_spectra_mode = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@spectra_mode[analyses]
    },

    # MARK: get_spectra_level
    ## get_spectra_level -----
    #' @description Gets a character vector with the spectra levels (e.g., "1, 2") of each analysis.
    get_spectra_level = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@spectra_level[analyses]
    },

    # MARK: get_spectra_lowest_mz
    ## get_spectra_lowest_mz -----
    #' @description Gets a numeric vector with the lower \emph{m/z} value of each analysis.
    get_spectra_lowest_mz = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@spectra_lowest_mz[analyses]
    },

    # MARK: get_spectra_highest_mz
    ## get_spectra_highest_mz -----
    #' @description Gets a numeric vector with the higher \emph{m/z} value of each analysis.
    get_spectra_highest_mz = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@spectra_highest_mz[analyses]
    },

    # MARK: get_spectra_lowest_rt
    ## get_spectra_lowest_rt -----
    #' @description Gets a numeric vector with the start retention time value of each analysis.
    get_spectra_lowest_rt = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@spectra_lowest_rt[analyses]
    },

    # MARK: get_spectra_highest_rt
    ## get_spectra_highest_rt -----
    #' @description Gets a numeric vector with the end retention time value of each analysis.
    get_spectra_highest_rt = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@spectra_highest_rt[analyses]
    },

    # MARK: get_spectra_polarity
    ## get_spectra_polarity -----
    #' @description Gets a character vector with the polarity of each analysis.
    get_spectra_polarity = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@spectra_polarity[analyses]
    },

    # MARK: get_spectra_headers
    ## get_spectra_headers -----
    #' @description Gets a data.table of the spectra headers from each analysis.
    get_spectra_headers = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      value <- self$Analyses@spectra_headers[analyses]
      value <- rbindlist(value, idcol = "analysis", fill = TRUE)
      value
    },

    # MARK: get_spectra_tic
    ## get_spectra_tic -----
    #' @description Gets a data.table of the total ion chromatogram (TIC) from each analysis.
    #'
    #' @param rt Numeric (length 2). The retention time range to filter the TIC.
    #'
    get_spectra_tic = function(analyses = NULL, levels = c(1, 2), rt = NULL) {
      StreamFind::get_spectra_tic(self$Analyses, analyses, levels, rt)
    },

    # MARK: get_spectra_bpc
    ## get_spectra_bpc -----
    #' @description Gets a data.table of the base peak chromatogram (BPC) from each analysis.
    #'
    #' @param rt Numeric (length 2). The retention time range to filter the BPC.
    #'
    get_spectra_bpc = function(analyses = NULL, levels = c(1, 2), rt = NULL) {
      StreamFind::get_spectra_bpc(self$Analyses, analyses, levels, rt)
    },
    
    # MARK: get_raw_spectra
    ## get_raw_spectra -----
    #' @description Gets a data.table of spectra from the analyses based on targets.
    get_raw_spectra = function(analyses = NULL,
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
      StreamFind::get_raw_spectra(
        self$Analyses,
        analyses, levels, mass, mz, rt, mobility, ppm, sec, millisec, id,
        allTraces, isolationWindow, minIntensityMS1, minIntensityMS2
      )
    },
    
    # MARK: get_spectra_eic
    ## get_spectra_eic -----
    #' @description Gets a data.table of extract ion chromatograms (EIC) from the analyses based
    #' on targets.
    get_spectra_eic = function(analyses = NULL,
                               mass = NULL,
                               mz = NULL,
                               rt = NULL,
                               mobility = NULL,
                               ppm = 20,
                               sec = 60,
                               millisec = 5,
                               id = NULL) {
      StreamFind::get_spectra_eic(
        self$Analyses,
        analyses, mass, mz, rt, mobility, ppm, sec, millisec, id
      )
    },
    
    # MARK: get_spectra_ms1
    ## get_spectra_ms1 -----
    #' @description Gets a data.table of level 1 spectra from the analyses based on targets.
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
                               minIntensity = 1000) {
      StreamFind::get_spectra_ms1(
        self$Analyses,
        analyses, mass, mz, rt, mobility, ppm, sec, millisec, id,
        mzClust, presence, minIntensity
      )
    },
    
    # MARK: get_spectra_ms2
    ## get_spectra_ms2 -----
    #' @description Gets a data.table of level 2 spectra from the analyses based on targets.
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
                               minIntensity = 0) {
      StreamFind::get_spectra_ms2(
        self$Analyses,
        analyses, mass, mz, rt, mobility, ppm, sec, millisec, id,
        isolationWindow, mzClust, presence, minIntensity
      )
    },
    
    # MARK: plot_spectra_tic
    ## plot_spectra_tic -----
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
                                downsize = 1,
                                interactive = TRUE,
                                renderEngine = "webgl") {
      StreamFind::plot_spectra_tic(
        self$Analyses, analyses,levels, rt, xLab, yLab, title, colorBy, legendNames,
        downsize, interactive, renderEngine
      )
    },
    
    # MARK: plot_spectra_bpc
    ## plot_spectra_bpc -----
    #' @description Plots the spectra base peak chromatogram (BPC) of each analysis.
    plot_spectra_bpc = function(analyses = NULL,
                                levels = c(1, 2),
                                rt = NULL,
                                xLab = NULL,
                                yLab = NULL,
                                title = NULL,
                                colorBy = "analyses",
                                legendNames = NULL,
                                interactive = TRUE,
                                renderEngine = "webgl") {
      StreamFind::plot_spectra_bpc(
        self$Analyses, analyses, levels, rt, xLab, yLab, title, colorBy, legendNames,
        interactive, renderEngine
      )
    },
    
    # MARK: plot_spectra_eic
    ## plot_spectra_eic -----
    #' @description Plots spectra extract ion chromatograms (EIC) from the analyses based on targets.
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
                                interactive = TRUE,
                                renderEngine = "webgl") {
      StreamFind::plot_spectra_eic(
        self$Analyses, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id, legendNames, xLab,
        yLab, title, colorBy, interactive, renderEngine
      )
    },
    
    # MARK: plot_spectra_xic
    ## plot_spectra_xic -----
    #' @description Plots spectra extract ion chromatograms (EIC) and \emph{m/z} vs retention time
    #' from the analyses.
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
                                numberRows = 1,
                                renderEngine = "webgl") {
      StreamFind::plot_spectra_xic(
        self$Analyses, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id,
        legendNames, plotTargetMark, targetsMark, ppmMark, secMark, numberRows, renderEngine
      )
    },
    
    # MARK: plot_spectra_ms1
    ## plot_spectra_ms1 -----
    #' @description Plots level 1 spectra from the analyses based on targets.
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
      StreamFind::plot_spectra_ms1(
        self$Analyses, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id, mzClust,
        presence, minIntensity, legendNames, xLab, yLab, title, colorBy, showText, interactive
      )
    },
    
    # MARK: plot_spectra_ms2
    ## plot_spectra_ms2 -----
    #' @description Plots level 2 spectra from the analyses based on targets.
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
      StreamFind::plot_spectra_ms2(
        self$Analyses, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id, isolationWindow,
        mzClust, presence, minIntensity, legendNames, xLab, yLab, title, colorBy, interactive
      )
    },
    
    # MARK: get_chromatograms_number
    ## get_chromatograms_number -----
    #' @description Gets the number of chromatograms in each analysis.
    get_chromatograms_number = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      self$Analyses@chromatograms_number[analyses]
    },
    
    # MARK: get_chromatograms_headers
    ## get_chromatograms_headers -----
    #' @description Gets the chromatograms headers data.table of each analysis.
    get_chromatograms_headers = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$Analyses, analyses)
      value <- self$Analyses@chromatograms_headers[analyses]
      value <- rbindlist(value, idcol = "analysis", fill = TRUE)
      value
    },
    
    # MARK: get_raw_chromatograms
    ## get_raw_chromatograms -----
    #' @description Gets a list of chromatograms data.table objects from each analysis.
    get_raw_chromatograms = function(analyses = NULL,
                                     chromatograms = NULL,
                                     rtmin = 0,
                                     rtmax = 0,
                                     minIntensity = NULL) {
      StreamFind::get_raw_chromatograms(
        self$Analyses,
        analyses, chromatograms, rtmin, rtmax, minIntensity
      )
    },
    
    # MARK: has_results_nts
    ## has_results_nts -----
    #' @description Checks if there are NonTargetAnalysisResults results, returning `TRUE` or `FALSE`.
    has_results_nts = function() {
      self$Analyses@has_results_nts
    },
    
    # MARK: has_results_spectra
    ## has_results_spectra -----
    #' @description Checks if there are spectra, returning `TRUE` or `FALSE`.
    has_results_spectra = function() {
      self$Analyses@has_results_spectra
    },
    
    # MARK: has_results_chromatograms
    ## has_results_chromatograms -----
    #' @description Checks if there are chromatograms, returning `TRUE` or `FALSE`.
    has_results_chromatograms = function() {
      self$Analyses@has_results_chromatograms
    },
    
    # MARK: Chromatograms
    # Chromatograms -----
    
    # MARK: get_chromatograms
    ## get_chromatograms -----
    #' @description Gets a list of chromatograms from the Chromatograms results.
    get_chromatograms = function(analyses = NULL,
                                 chromatograms = NULL,
                                 rtmin = 0,
                                 rtmax = 0,
                                 minIntensity = NULL) {
      if (self$has_results_chromatograms()) {
        StreamFind::get_chromatograms(
          self$Chromatograms, analyses, chromatograms, rtmin, rtmax, minIntensity
        )
      } else {
        warning("No chromatograms results available! Not done.")
        return(list())
      }
    },
    
    # MARK: get_chromatograms_peaks
    ## get_chromatograms_peaks -----
    #' @description Gets a data.table of integrated peaks from the Chromatograms results.
    get_chromatograms_peaks = function(analyses = NULL,
                                       chromatograms = NULL,
                                       rtmin = 0,
                                       rtmax = 0,
                                       minIntensity = NULL) {
      if (self$has_results_chromatograms()) {
        StreamFind::get_chromatograms_peaks(
          self$Chromatograms, analyses, chromatograms, rtmin, rtmax, minIntensity
        )
      } else {
        warning("No chromatograms results available! Not done.")
        return(list())
      }
    },
    
    # MARK: plot_chromatograms
    ## plot_chromatograms -----
    #' @description Plots chromatograms from the Chromatograms results.
    #' @param normalized Logical (length 1). Set to `TRUE` to normalize the chromatograms.
    plot_chromatograms = function(analyses = NULL,
                                  chromatograms = NULL,
                                  rtmin = 0,
                                  rtmax = 0,
                                  minIntensity = NULL,
                                  normalized = TRUE,
                                  xLab = NULL,
                                  yLab = NULL,
                                  title = NULL,
                                  colorBy = "analyses+targets",
                                  legendNames = NULL,
                                  interactive = TRUE,
                                  renderEngine = "webgl") {
      if (self$has_results_chromatograms()) {
        StreamFind::plot_chromatograms(
          self$Chromatograms, analyses, chromatograms, rtmin, rtmax, minIntensity,
          normalized, xLab, yLab, title, colorBy, legendNames, interactive, renderEngine
        )
      } else {
        warning("No chromatograms results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: plot_chromatograms_baseline
    ## plot_chromatograms_baseline -----
    #' @description Plots baseline correction from the Chromatograms results.
    plot_chromatograms_baseline = function(analyses = NULL,
                                           chromatograms = NULL,
                                           xLab = NULL,
                                           yLab = NULL,
                                           title = NULL,
                                           colorBy = "analyses",
                                           interactive = TRUE,
                                           renderEngine = "webgl") {
      if (self$has_results_chromatograms()) {
        StreamFind::plot_chromatograms_baseline(
          self$Chromatograms, analyses, chromatograms,
          xLab, yLab, title, colorBy, interactive, renderEngine
        )
      } else {
        warning("No chromatograms results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: plot_chromatograms_peaks
    ## plot_chromatograms_peaks -----
    #' @description Plots chromatographic peaks from the Chromatograms results.
    plot_chromatograms_peaks = function(analyses = NULL,
                                        chromatograms = NULL,
                                        rtmin = 0,
                                        rtmax = 0,
                                        minIntensity = NULL,
                                        xLab = NULL,
                                        yLab = NULL,
                                        title = NULL,
                                        colorBy = "analyses+targets",
                                        legendNames = NULL,
                                        interactive = TRUE,
                                        renderEngine = "webgl") {
      if (self$has_results_chromatograms()) {
        StreamFind::plot_chromatograms_peaks(
          self$Chromatograms, analyses, chromatograms, rtmin, rtmax, minIntensity,
          xLab, yLab, title, colorBy, legendNames, interactive, renderEngine
        )
      } else {
        warning("No chromatograms results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: Spectra
    # Spectra -----
    
    # MARK: get_spectra
    ## get_spectra -----
    #' @description Gets a list of data.table object with spectra each analysis.
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
                           minIntensityMS2 = 0) {
      if (self$has_results_spectra()) {
        StreamFind::get_spectra(
          self$Spectra,
          analyses, levels, mass, mz, rt, mobility, ppm, sec, millisec, id, allTraces,
          isolationWindow, minIntensityMS1, minIntensityMS2
        )
      } else {
        warning("No spectra results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: plot_spectra
    ## plot_spectra -----
    #' @description Plots spectra.
    #'
    #' @param xVal Character length one. Possible values are "mz", "rt", "mobility" or "mass".
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
                            legendNames = TRUE,
                            colorBy = "analyses",
                            xVal = "mz",
                            xLab = NULL,
                            yLab = NULL,
                            title = NULL,
                            interactive = TRUE,
                            renderEngine = "webgl") {
      if (self$has_results_spectra()) {
        StreamFind::plot_spectra(
          self$Spectra,
          analyses, levels, mass, mz, rt, mobility, ppm, sec, millisec, id,
          allTraces, isolationWindow, minIntensityMS1, minIntensityMS2,
          legendNames, colorBy, xVal, xLab, yLab, title, interactive, renderEngine
        )
      } else {
        warning("No spectra results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: plot_spectra_3d
    ## plot_spectra_3d -----
    #' @description Plots spectra in 3D.
    #'
    #' @param xVal Character length one. Possible values are "mz", "rt" or "mobility".
    #' @param yVal Character length one. Possible values are "mz", "rt" or "mobility".
    #' @param zLab A string with the title for the z axis.
    #'
    plot_spectra_3d = function(analyses = NULL,
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
                               zLab = NULL,
                               renderEngine = "webgl") {
      if (self$has_results_spectra()) {
        StreamFind::plot_spectra(
          self$Spectra,
          analyses, levels, mass, mz, rt, mobility, ppm, sec, millisec, id,
          allTraces, isolationWindow, minIntensityMS1, minIntensityMS2,
          legendNames, colorBy, xVal, yVal, xLab, yLab, zLab, renderEngine 
        )
      } else {
        warning("No spectra results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: plot_spectra_charges
    ## plot_spectra_charges -----
    #' @description Plots charge assignment of deconvoluted spectra from analyses.
    plot_spectra_charges = function(analyses = NULL,
                                    legendNames = NULL,
                                    title = NULL,
                                    colorBy = "analyses",
                                    xLab = NULL,
                                    yLab = NULL,
                                    interactive = TRUE,
                                    renderEngine = "webgl") {
      if (self$has_results_spectra()) {
        StreamFind::plot_spectra_charges(
          self$Spectra,
          analyses, legendNames, title, colorBy, xLab, yLab, interactive, renderEngine
        )
      } else {
        warning("No spectra results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: get_spectra_peaks
    ## get_spectra_peaks -----
    #' @description Gets a data.table with peaks from spectra in each analysis.
    get_spectra_peaks = function(analyses = NULL) {
      if (self$has_results_spectra()) {
        StreamFind::get_spectra_peaks(self$Spectra, analyses)
      } else {
        warning("No spectra results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: plot_spectra_peaks
    ## plot_spectra_peaks -----
    #' @description Plots peaks from spectra in each analysis.
    #' 
    #' @param xVal Character length one. Possible values are "mz" or "mobility".
    #' 
    plot_spectra_peaks = function(analyses = NULL,
                                  legendNames = TRUE,
                                  colorBy = "analyses",
                                  xVal = "mz",
                                  xLab = NULL,
                                  yLab = NULL,
                                  title = NULL,
                                  interactive = TRUE,
                                  renderEngine = "webgl") {
      if (self$has_results_spectra()) {
        StreamFind::plot_spectra_charges(
          self$Spectra,
          analyses, legendNames, colorBy, xVal, xLab, yLab, title, interactive, renderEngine
        )
      } else {
        warning("No spectra results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: plot_spectra_baseline
    ## plot_spectra_baseline -----
    #' @description Plots spectra baseline correction.
    #' 
    #' @param xVal Character length one. Possible values are "mz" or "mobility".
    #' 
    plot_spectra_baseline = function(analyses = NULL,
                                     legendNames = TRUE,
                                     colorBy = "analyses",
                                     xVal = "mz",
                                     xLab = NULL,
                                     yLab = NULL,
                                     title = NULL,
                                     interactive = TRUE,
                                     renderEngine = "webgl") {
      if (self$has_results_spectra()) {
        StreamFind::plot_spectra_baseline(
          self$Spectra,
          analyses, legendNames, colorBy, xVal, xLab, yLab, title, interactive, renderEngine
        )
      } else {
        warning("No spectra results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: get_spectra_matrix
    ## get_spectra_matrix -----
    #' @description Gets a matrix with spectra from analyses.
    get_spectra_matrix = function(analyses = NULL) {
      if (self$has_results_spectra()) {
        StreamFind::get_spectra_matrix(self$Spectra, analyses)
      } else {
        warning("No spectra results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: NonTargetAnalysisResults
    # NonTargetAnalysisResults -----
    
    # MARK: get_features_count
    ## get_features_count -----
    #' @description Gets a data.table with the features count from NonTargetAnalysisResults results.
    get_features_count = function(analyses = NULL, filtered = FALSE) {
      if (self$has_results_nts()) {
        StreamFind::get_features_count(
          self$NonTargetAnalysisResults, analyses, filtered
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(data.table::data.table())
      }
    },
    
    # MARK: plot_features_count
    ## plot_features_count -----
    #' @description Plots the features count from NonTargetAnalysisResults results.
    #' 
    #' @param showHoverText Logical (length 1). Set to \code{TRUE} to show hover text.
    #' 
    plot_features_count = function(analyses = NULL,
                                   filtered = FALSE,
                                   yLab = NULL,
                                   title = NULL,
                                   colorBy = "analyses",
                                   showLegend = TRUE,
                                   showHoverText = TRUE) {
      if (self$has_results_nts()) {
        StreamFind::plot_features_count(
          self$NonTargetAnalysisResults, analyses, filtered, yLab, title, colorBy, showLegend, showHoverText
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: get_features
    ## get_features -----
    #' @description Gets a data.table with all features from NonTargetAnalysisResults results or as selected by the 
    #' arguments.
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
      if (self$has_results_nts()) {
        StreamFind::get_features(
          self$NonTargetAnalysisResults, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(data.table::data.table())
      }
    },
    
    # MARK: map_features
    ## map_features -----
    #' @description Plots a map of the retention time vs \emph{m/z} of features from analyses.
    #' 
    #' @param neutral_mass Logical (length 1). Set to \code{TRUE} to use neutral mass.
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
                            neutral_mass = TRUE,
                            filtered = FALSE,
                            legendNames = NULL,
                            xLab = NULL,
                            yLab = NULL,
                            title = NULL,
                            colorBy = "replicates+targets",
                            showLegend = TRUE,
                            interactive = TRUE,
                            renderEngine = "webgl") {
      if (self$has_results_nts()) {
        StreamFind::map_features(
          self$NonTargetAnalysisResults, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, neutral_mass,
          filtered, legendNames, xLab, yLab, title, colorBy, showLegend, interactive, renderEngine
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: map_features_intensity
    ## map_features_intensity -----
    #' @description Plots a map of the retention time vs \emph{m/z} of features from analyses.
    map_features_intensity = function(analyses = NULL,
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
                                      colorBy = "replicates+targets",
                                      renderEngine = "webgl") {
      if (self$has_results_nts()) {
        StreamFind::map_features_intensity(
          self$NonTargetAnalysisResults, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec,
          filtered, legendNames, xLab, yLab, title, colorBy, renderEngine
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: get_features_eic
    ## get_features_eic -----
    #' @description Gets a data.table with feature EICs following the targets from the arguments.
    get_features_eic = function(analyses = NULL,
                                features = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                mobility = NULL,
                                ppm = 20,
                                sec = 60,
                                millisec = 5,
                                rtExpand = 0,
                                mzExpand = 0,
                                filtered = FALSE,
                                useLoadedData = TRUE) {
      if (self$has_results_nts()) {
        StreamFind::get_features_eic(
          self$NonTargetAnalysisResults, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec,
          rtExpand, mzExpand, filtered, useLoadedData
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(data.table::data.table())
      }
    },
    
    # MARK: plot_features
    ## plot_features -----
    #' @description Plots features from analyses.
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
                             interactive = TRUE,
                             renderEngine = "webgl") {
      if (self$has_results_nts()) {
        StreamFind::plot_features(
          self$NonTargetAnalysisResults, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec,
          rtExpand, mzExpand, useLoadedData, filtered, legendNames, xLab, yLab, title, colorBy,
          interactive, renderEngine
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: get_features_ms1
    ## get_features_ms1 -----
    #' @description Gets a data.table of averaged MS1 spectrum for features in the analyses or as
    #' selected from the arguments.
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
      if (self$has_results_nts()) {
        StreamFind::get_features_ms1(
          self$NonTargetAnalysisResults, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec,
          rtWindow, mzWindow, mzClust, presence, minIntensity, filtered, useLoadedData
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(data.table::data.table())
      }
    },
    
    # MARK: plot_features_ms1
    ## plot_features_ms1 -----
    #' @description Plots level 1 spectra from features in the analyses.
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
                                 showText = FALSE,
                                 interactive = TRUE) {
      if (self$has_results_nts()) {
        StreamFind::plot_features_ms1(
          self$NonTargetAnalysisResults, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, rtWindow,
          mzWindow, mzClust, presence, minIntensity, filtered, useLoadedData, legendNames,
          xLab, yLab, title, colorBy, showText, interactive
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: get_features_ms2
    ## get_features_ms2 -----
    #' @description Gets a data.table of averaged MS2 spectrum for features in the analyses or as
    #' selected from the arguments.
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
      if (self$has_results_nts()) {
        StreamFind::get_features_ms2(
          self$NonTargetAnalysisResults, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec,
          isolationWindow, mzClust, presence, minIntensity, filtered, useLoadedData
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(data.table::data.table())
      }
    },
    
    # MARK: plot_features_ms2
    ## plot_features_ms2 -----
    #' @description Plots level 2 spectra from features in the analyses.
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
                                 showText = TRUE,
                                 interactive = TRUE) {
      if (self$has_results_nts()) {
        StreamFind::plot_features_ms2(
          self$NonTargetAnalysisResults, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, isolationWindow,
          mzClust, presence, minIntensity, filtered, useLoadedData, legendNames, xLab, yLab, title,
          colorBy, showText, interactive
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: get_groups
    ## get_groups -----
    #' @description Gets a data.table with feature groups from the analyses.
    #'
    #' @param sdValues Logical (length 1). Set to `TRUE` for returning the sd values when averaging
    #' the intensity within analysis replicates.
    #' @param metadata Logical (length 1). Set to `TRUE` for returning extra metadata from feature
    #' groups (e.g., presence in each analysis replicate and mass and time widths).
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
                          metadata = FALSE,
                          correctIntensity = FALSE) {
      if (self$has_results_nts()) {
        StreamFind::get_groups(
          self$NonTargetAnalysisResults, groups, mass, mz, rt, mobility, ppm, sec, millisec,
          filtered, intensities, average, sdValues, metadata, correctIntensity
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(data.table::data.table())
      }
    },
    
    # MARK: plot_groups
    ## plot_groups -----
    #' @description Plots feature groups EIC.
    plot_groups = function(analyses = NULL,
                           groups = NULL,
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
                           interactive = TRUE,
                           renderEngine = "webgl") {
      if (self$has_results_nts()) {
        StreamFind::plot_groups(
          self$NonTargetAnalysisResults, analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, rtExpand, mzExpand,
          filtered, legendNames, xLab, yLab, title, colorBy, interactive, renderEngine
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: plot_groups_overview
    ## plot_groups_overview -----
    #' @description Method to give an overview of the EIC, alignment and intensity variance from
    #' features within target feature groups.
    #'
    #' @param correctIntensity Logical (length 1). When `TRUE` and suppression factor is available 
    #' the intensities are corrected for suppression.
    #' @param heights A numeric vector of length 3 to control the height of the first, second and
    #' third plot, respectively.
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
                                    mzExpand = 0.005,
                                    useLoadedData = TRUE,
                                    correctIntensity = TRUE,
                                    filtered = FALSE,
                                    legendNames = NULL,
                                    title = NULL,
                                    heights = c(0.35, 0.5, 0.15),
                                    renderEngine = "webgl") {
      if (self$has_results_nts()) {
        StreamFind::plot_groups_overview(
          self$NonTargetAnalysisResults, analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, rtExpand,
          mzExpand, useLoadedData, correctIntensity, filtered, legendNames, title, heights,
          renderEngine
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: plot_groups_profile
    ## plot_groups_profile -----
    #' @description Method to plot the intensity profile of feature groups across the analyses.
    #' 
    #' @param averaged Logical (length 1). When `TRUE` the profile intensities are averaged.
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
                                   correctIntensity = TRUE,
                                   averaged = FALSE,
                                   normalized = TRUE,
                                   legendNames = NULL,
                                   yLab = NULL,
                                   title = NULL,
                                   showLegend = TRUE,
                                   renderEngine = "webgl") {
      if (self$has_results_nts()) {
        StreamFind::plot_groups_profile(
          self$NonTargetAnalysisResults, analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered,
          correctIntensity, averaged, normalized, legendNames, yLab, title, showLegend, renderEngine
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: get_groups_ms1
    ## get_groups_ms1 -----
    #' @description Gets a data.table of averaged MS1 spectrum for feature groups in the analyses.
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
      if (self$has_results_nts()) {
        StreamFind::get_groups_ms1(
          self$NonTargetAnalysisResults, groups, mass, mz, rt, mobility, ppm, sec, millisec,
          rtWindow, mzWindow, mzClustFeatures, presenceFeatures, minIntensityFeatures,
          useLoadedData, mzClust, presence, minIntensity, groupBy, filtered
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(data.table::data.table())
      }
    },
    
    # MARK: plot_groups_ms1
    ## plot_groups_ms1 -----
    #' @description Plots level 1 spectra from feature groups in the analyses.
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
                               showText = FALSE,
                               interactive = TRUE) {
      if (self$has_results_nts()) {
        StreamFind::plot_groups_ms1(
          self$NonTargetAnalysisResults, groups, mass, mz, rt, mobility, ppm, sec, millisec, rtWindow, mzWindow,
          mzClustFeatures, presenceFeatures, minIntensityFeatures, useLoadedData,
          mzClust, presence, minIntensity, groupBy, filtered, legendNames,
          xLab, yLab, title, colorBy, showText, interactive
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: get_groups_ms2
    ## get_groups_ms2 -----
    #' @description Gets a data.table of averaged MS2 spectrum for feature groups in the analyses.
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
      if (self$has_results_nts()) {
        StreamFind::get_groups_ms2(
          self$NonTargetAnalysisResults, groups, mass, mz, rt, mobility, ppm, sec, millisec,
          isolationWindow, mzClustFeatures, presenceFeatures, minIntensityFeatures,
          useLoadedData, mzClust, presence, minIntensity, groupBy, filtered
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(data.table::data.table())
      }
    },
    
    # MARK: plot_groups_ms2
    ## plot_groups_ms2 -----
    #' @description Plots level 1 spectra from feature groups in the analyses.
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
                               showText = TRUE,
                               interactive = TRUE) {
      if (self$has_results_nts()) {
        StreamFind::plot_groups_ms2(
          self$NonTargetAnalysisResults, groups, mass, mz, rt, mobility, ppm, sec, millisec, isolationWindow,
          mzClustFeatures, presenceFeatures, minIntensityFeatures, useLoadedData,
          mzClust, presence, minIntensity, groupBy, filtered, legendNames,
          xLab, yLab, title, colorBy, showText, interactive
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: get_components
    ## get_components -----
    #' @description Gets feature components (i.e., isotope and adduct related to a main feature)
    #' in the analyses.
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
      if (self$has_results_nts()) {
        StreamFind::get_components(
          self$NonTargetAnalysisResults, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(data.table::data.table())
      }
    },
    
    # MARK: map_components
    ## map_components -----
    #' @description Maps feature components in the analyses.
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
                              legendNames = NULL,
                              xLab = NULL,
                              yLab = NULL,
                              title = NULL,
                              colorBy = "targets",
                              interactive = TRUE,
                              showLegend = TRUE,
                              renderEngine = "webgl") {
      if (self$has_results_nts()) {
        StreamFind::map_components(
          self$NonTargetAnalysisResults, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered,
          legendNames, xLab, yLab, title, colorBy, interactive, showLegend, renderEngine
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: get_suspects
    ## get_suspects -----
    #' @description Gets a data.table of suspects from features according to a defined database and
    #' mass (`ppm`) and time (`sec`) deviations.
    #'
    #' @param database A data.frame with at least the columns name and mass, indicating the name
    #' and neutral monoisotopic mass of the suspect targets.
    #' @param mzrMS2 Numeric (length 1). The m/z resolution for MS2 spectra.
    #' @param minCusiness Numeric (length 1). The minimum Cusiness value for the suspects.
    #'
    #' @details The `ppm` and `sec` which indicate the mass (im ppm) and time (in seconds)
    #' deviations applied during the screening.
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
                            mzrMS2 = 0.008,
                            minCusiness = 0.7,
                            minFragments = 3,
                            filtered = FALSE) {
      if (self$has_results_nts()) {
        StreamFind::get_suspects(
          self$NonTargetAnalysisResults, analyses, database, features, mass, mz, rt, mobility, ppm, sec, millisec,
          ppmMS2, mzrMS2, minCusiness, minFragments, filtered
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(data.table::data.table())
      }
    },
    
    # MARK: plot_suspects
    ## plot_suspects -----
    #' @description Plots suspects.
    #'
    #' @param database A data.frame with at least the columns name and mass, indicating the name
    #' and neutral monoisotopic mass of the suspect targets.
    #' @param mzrMS2 Numeric (length 1). The m/z resolution for MS2 spectra.
    #' @param minCusiness Numeric (length 1). The minimum Cusiness value for the suspects.
    #' @param heights A numeric vector of length 2 to control the height of the first and second
    #' plot, respectively.
    #'
    #' @details The `ppm` and `sec` which indicate the mass (im ppm) and time (in seconds)
    #' deviations applied during the screening.
    #'
    plot_suspects = function(analyses = NULL,
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
                             mzrMS2 = 0.008,
                             minCusiness = 0.7,
                             minFragments = 3,
                             filtered = FALSE,
                             rtExpand = 120,
                             mzExpand = 0.005,
                             useLoadedData = TRUE,
                             legendNames = NULL,
                             colorBy = "replicates+targets",
                             heights = c(0.5, 0.5),
                             interactive = TRUE) {
      if (self$has_results_nts()) {
        StreamFind::plot_suspects(
          self$NonTargetAnalysisResults, analyses, database, features, mass, mz, rt, mobility, ppm,
          sec, millisec, ppmMS2, mzrMS2, minCusiness, minFragments, filtered, rtExpand, mzExpand,
          useLoadedData, legendNames, colorBy, heights, interactive
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: get_internal_standards
    ## get_internal_standards -----
    #' @description Gets a data.table with internal standards found by the `FindInternalStandards`
    #' processing method.
    #'
    #' @param average Logical of length one. When `TRUE` and groups are present, internal standards
    #' are averaged per analysis replicate group.
    #'
    get_internal_standards = function(average = TRUE) {
      if (self$has_results_nts()) {
        StreamFind::get_internal_standards(self$NonTargetAnalysisResults, average)
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(data.table::data.table())
      }
    },
    
    # MARK: plot_internal_standards
    ## plot_internal_standards -----
    #' @description Plots the quality control assessment of the internal standards.
    #'
    #' @param presence Logical (length 1). When `TRUE` the presence of the internal standards is
    #' plotted.
    #' @param recovery Logical (length 1). When `TRUE` the recovery of the internal standards is
    #' plotted.
    #' @param deviations Logical (length 1). When `TRUE` the deviations of the internal standards
    #' is plotted.
    #' @param widths Logical (length 1). When `TRUE` the widths of the internal standards is
    #' plotted.
    #'
    plot_internal_standards = function(analyses = NULL,
                                       presence = TRUE,
                                       recovery = TRUE,
                                       deviations = TRUE,
                                       widths = TRUE,
                                       renderEngine = "webgl") {
      if (self$has_results_nts()) {
        StreamFind::plot_internal_standards(
          self$NonTargetAnalysisResults, analyses, presence, recovery, deviations, widths, renderEngine
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: get_fold_change
    ## get_fold_change -----
    #' @description Gets a data.table with fold-change analysis between the `replicatesIn` and
    #' `replicatesOut`.
    #' 
    #' @param replicatesIn Character vector with the names of the replicates to be considered as
    #' the denominator.
    #' @param replicatesOut Character vector with the names of the replicates to be considered as
    #' the numerator.
    #' @param constantThreshold Numeric of length one. The threshold to consider a feature as
    #' constant.
    #' @param eliminationThreshold Numeric of length one. The threshold to consider a feature as
    #' eliminated.
    #' @param correctIntensity Logical of length one. When `TRUE` the suppression factor (when
    #' available) is used to correct the intensity before fold-change analysis.
    #' @param fillZerosWithLowerLimit Logical of length one. When `TRUE` the zero values are filled
    #' with the lower limit.
    #' @param lowerLimit Numeric of length one. The lower limit to fill the zero values.
    #' 
    get_fold_change = function(replicatesIn = NULL,
                               replicatesOut = NULL,
                               groups = NULL,
                               mass = NULL,
                               mz = NULL,
                               rt = NULL,
                               mobility = NULL,
                               ppm = 4,
                               sec = 10,
                               millisec = 5,
                               filtered = FALSE,
                               constantThreshold = 0.5,
                               eliminationThreshold = 0.2,
                               correctIntensity = FALSE,
                               fillZerosWithLowerLimit = FALSE,
                               lowerLimit = NA_real_) {
      if (self$has_results_nts()) {
        StreamFind::get_fold_change(
          self$NonTargetAnalysisResults, replicatesIn, replicatesOut, groups, mass, mz, rt, mobility, ppm, sec, millisec,
          filtered, constantThreshold, eliminationThreshold, correctIntensity,
          fillZerosWithLowerLimit, lowerLimit
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(data.table::data.table())
      }
    },
    
    # MARK: plot_fold_change
    ## plot_fold_change -----
    #' @description Plots the fold-change analysis between the `replicatesIn` and `replicatesOut`.
    #' 
    #' @param replicatesIn Character vector with the names of the replicates to be considered as
    #' the denominator.
    #' @param replicatesOut Character vector with the names of the replicates to be considered as
    #' the numerator.
    #' @param constantThreshold Numeric of length one. The threshold to consider a feature as
    #' constant.
    #' @param eliminationThreshold Numeric of length one. The threshold to consider a feature as
    #' eliminated.
    #' @param correctIntensity Logical of length one. When `TRUE` the suppression factor (when
    #' available) is used to correct the intensity before fold-change analysis.
    #' @param fillZerosWithLowerLimit Logical of length one. When `TRUE` the zero values are filled
    #' with the lower limit.
    #' @param lowerLimit Numeric of length one. The lower limit to fill the zero values.
    #' @param normalized Logical of length one. When `TRUE` the fold-change values are normalized.
    #' 
    plot_fold_change = function(replicatesIn = NULL,
                                replicatesOut = NULL,
                                groups = NULL,
                                mass = NULL,
                                mz = NULL,
                                rt = NULL,
                                mobility = NULL,
                                ppm = 4,
                                sec = 10,
                                millisec = 5,
                                filtered = FALSE,
                                constantThreshold = 0.5,
                                eliminationThreshold = 0.2,
                                correctIntensity = FALSE,
                                fillZerosWithLowerLimit = FALSE,
                                lowerLimit = NA_real_,
                                normalized = TRUE,
                                yLab = NULL,
                                title = NULL,
                                interactive = TRUE,
                                showLegend = TRUE) {
      if (self$has_results_nts()) {
        StreamFind::plot_fold_change(
          self$NonTargetAnalysisResults, replicatesIn, replicatesOut, groups, mass, mz, rt, mobility, ppm, sec, millisec,
          filtered, constantThreshold, eliminationThreshold, correctIntensity,
          fillZerosWithLowerLimit, lowerLimit, normalized, yLab, title, interactive, showLegend
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(data.table::data.table())
      }
    },
    
    # MARK: get_compounds
    ## get_compounds -----
    #' @description Gets a data.table with compounds from the analyses.
    #' 
    #' @param averaged Logical of length one. When `TRUE` the compounds are averaged per analysis
    #' replicate group.
    #'  
    get_compounds = function(analyses = NULL,
                             features = NULL,
                             mass = NULL,
                             mz = NULL,
                             rt = NULL,
                             mobility = NULL,
                             ppm = 20,
                             sec = 60,
                             millisec = 5,
                             filtered = FALSE,
                             averaged = TRUE) {
      if (self$has_results_nts()) {
        StreamFind::get_compounds(
          self$NonTargetAnalysisResults, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec,
          filtered, averaged
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(data.table::data.table())
      }
    },
    
    # MARK: get_patRoon_features
    ## get_patRoon_features -----
    #' @description Creates an S4 class `features` or `featureGroups`from the \pkg{patRoon} package.
    #' 
    #' @param featureGroups Logical of length one. When `TRUE` the `featureGroups` class is
    #' returned.
    #'
    get_patRoon_features = function(filtered = FALSE, featureGroups = TRUE) {
      if (self$has_results_nts()) {
        StreamFind::get_patRoon_features( self$NonTargetAnalysisResults, filtered, featureGroups)
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    },
    
    # MARK: get_patRoon_MSPeakLists
    ## get_patRoon_MSPeakLists -----
    #' @description Creates S4 class `MSPeakLists`. Note that feature groups are required. The MS
    #' and MSMS spectra of each feature are then average by \pkg{patRoon} to produce the feature
    #' group spectra using the parameters of the function \link[patRoon]{getDefAvgPListParams}.
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
    #'
    get_patRoon_MSPeakLists = function(clusterMzWindow = 0.005,
                                       topMost = 100,
                                       minIntensityPre = 50,
                                       minIntensityPost = 50,
                                       avgFun = "mean",
                                       method = "distance") {
      if (self$has_results_nts()) {
        StreamFind::get_patRoon_MSPeakLists(
          self$NonTargetAnalysisResults, clusterMzWindow, topMost, minIntensityPre, minIntensityPost, avgFun, method
        )
      } else {
        warning("No NonTargetAnalysisResults results available! Not done.")
        return(NULL)
      }
    }
  )
)

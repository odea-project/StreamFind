
### Generics -------------------------------------------------------------------

#' @title setInfo
#'
#' @description Generic for getting set information.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("setInfo", function(object, ...) standardGeneric("setInfo"))



#' @title analysisTable
#'
#' @description Generic for getting an analysis table.
#'
#' @param object A method specific class object.
#'
setGeneric("analysisTable", function(object) standardGeneric("analysisTable"))



#' @title filePaths
#'
#' @description Generic for getting file paths
#'
#' @param object A method specific class object.
#'
setGeneric("filePaths", function(object) standardGeneric("filePaths"))



#' @title analysisNames
#'
#' @description Generic for getting analysis names.
#'
#' @param object A method specific class object.
#'
setGeneric("analysisNames", function(object) standardGeneric("analysisNames"))



#' @title replicateNames
#'
#' @description Generic for getting replicate names.
#'
#' @param object A method specific class object.
#'
setGeneric("replicateNames", function(object) standardGeneric("replicateNames"))



#' @title replicateNames<-
#'
#' @description Generic for setting replicate names.
#'
#' @param object A method specific class object.
#' @param value A method specific vector with names.
#'
setGeneric("replicateNames<-", function(object, value) standardGeneric("replicateNames<-"))



#' @title blankReplicateNames
#'
#' @description Generic for getting blank replicate names.
#'
#' @param object A method specific class object.
#'
setGeneric("blankReplicateNames", function(object) standardGeneric("blankReplicateNames"))



#' @title blankReplicateNames<-
#'
#' @description Generic for setting blank replicate names.
#'
#' @param object A method specific class object.
#' @param value A method specific vector with names.
#'
setGeneric("blankReplicateNames<-", function(object, value) standardGeneric("blankReplicateNames<-"))



#' @title polarities
#'
#' @description Generic for getting polarities.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("polarities", function(object, ...) standardGeneric("polarities"))



#' @title addAnalyses
#'
#' @description Generic for adding analyses.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("addAnalyses", function(object, ...) standardGeneric("addAnalyses"))



#' @title getAnalyses
#'
#' @description Generic for getting analyses.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getAnalyses", function(object, ...) standardGeneric("getAnalyses"))


#' @title getMetadata
#'
#' @description Generic for getting metadata.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getMetadata", function(object, ...) standardGeneric("getMetadata"))


#' @title addMetadata
#'
#' @description Generic for adding metadata.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("addMetadata", function(object, ...) standardGeneric("addMetadata"))



#' @title loadSpectraInfo
#'
#' @description Generic for loading basic spectra information.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("loadSpectraInfo", function(object, ...) standardGeneric("loadSpectraInfo"))



#' @title loadRawData
#'
#' @description Generic for loading raw data.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("loadRawData", function(object, ...) standardGeneric("loadRawData"))



#' @title hasLoadedSpectra
#'
#' @description Generic for checking if has loaded spectra.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("hasLoadedSpectra", function(object, ...) standardGeneric("hasLoadedSpectra"))



#' @title hasLoadedChromatograms
#'
#' @description Generic for checking if has loaded chromatograms.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("hasLoadedChromatograms", function(object, ...) standardGeneric("hasLoadedChromatograms"))



#' @title EICs
#'
#' @description Generic for getting extracted ion chromatograms (EICs).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("EICs", function(object, ...) standardGeneric("EICs"))



#' @title plotEICs
#'
#' @description Generic for plotting extracted ion chromatograms (EICs).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotEICs", function(object, ...) standardGeneric("plotEICs"))



#' @title TICs
#'
#' @description Generic for getting total ion chromatograms (EICs).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("TICs", function(object, ...) standardGeneric("TICs"))



#' @title plotTICs
#'
#' @description Generic for plotting total ion chromatograms (EICs).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotTICs", function(object, ...) standardGeneric("plotTICs"))



#' @title XICs
#'
#' @description Generic for getting three dimensional ion chromatograms (XICs).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("XICs", function(object, ...) standardGeneric("XICs"))



#' @title plotXICs
#'
#' @description Generic for plotting three dimensional ion chromatograms (XICs).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotXICs", function(object, ...) standardGeneric("plotXICs"))



#' @title MS2s
#'
#' @description Generic for getting MS 2 spectra.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("MS2s", function(object, ...) standardGeneric("MS2s"))



#' @title plotMS2s
#'
#' @description Generic for plotting MS 2 spectra.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotMS2s", function(object, ...) standardGeneric("plotMS2s"))



#' @title hasPeaks
#'
#' @description Generic for checking if has peaks.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("hasPeaks", function(object, ...) standardGeneric("hasPeaks"))



#' @title peaks
#'
#' @description Generic for getting peaks.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("peaks", function(object, ...) standardGeneric("peaks"))



#' @title mapPeaks
#'
#' @description Generic for plotting a map of peaks.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("mapPeaks", function(object, ...) standardGeneric("mapPeaks"))



#' @title features
#'
#' @description Generic for getting features.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("features", function(object, ...) standardGeneric("features"))



#' @title plotFeatures
#'
#' @description Generic for plotting features.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotFeatures", function(object, ...) standardGeneric("plotFeatures"))



#' @title mapFeatures
#'
#' @description Generic for plotting a map of features.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("mapFeatures", function(object, ...) standardGeneric("mapFeatures"))



#' @title hasAdjustedRetentionTime
#'
#' @description Generic for checking is has adjusted retention time.
#'
#' @param object A method specific class object.
#'
setGeneric("hasAdjustedRetentionTime", function(object) standardGeneric("hasAdjustedRetentionTime"))



#' @title plotAnnotation
#'
#' @description Generic for plotting annotation.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotAnnotation", function(object, ...) standardGeneric("plotAnnotation"))



#' @title addParameters
#'
#' @description Generic for adding parameters.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("addParameters", function(object, ...) standardGeneric("addParameters"))



#' @title getParameters
#'
#' @description Generic for getting parameters.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getParameters", function(object, ...) standardGeneric("getParameters"))



#' @title getAlgorithm
#'
#' @description Generic for getting algorithm.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getAlgorithm", function(object, ...) standardGeneric("getAlgorithm"))



#' @title getSettings
#'
#' @description Generic for getting settings.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getSettings", function(object, ...) standardGeneric("getSettings"))



#' @title exportSettings
#'
#' @description Generic for exporting settings.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("exportSettings", function(object, ...) standardGeneric("exportSettings"))



#' @title as.features
#'
#' @description Generic for converting to features.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("as.features", function(object, ...) standardGeneric("as.features"))



#' @title as.featureGroups
#'
#' @description Generic for converting to featureGroups.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("as.featureGroups", function(object, ...) standardGeneric("as.featureGroups"))



### External Generics ----------------------------------------------------------

#### ProtGenerics --------------------------------------------------------
#' @importMethodsFrom ProtGenerics spectra
#' @noRd
NULL

#### patRoon -------------------------------------------------------------
#' @importMethodsFrom patRoon analysisInfo
#' @export
#' @noRd
NULL

#### xcms ----------------------------------------------------------------
#' @importMethodsFrom xcms plotPeaks
#' @export
#' @noRd
NULL

#### BiocGenerics --------------------------------------------------------
#' @importMethodsFrom BiocGenerics path annotation
#' @export
#' @noRd
NULL

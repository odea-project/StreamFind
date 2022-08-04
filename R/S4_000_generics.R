
### Generics ------------------------------------------------------------------------------------------------

#' setInfo
#'
#' @noRd
setGeneric("setInfo", function(object, ...) standardGeneric("setInfo"))

#' analysisTable
#'
#' @noRd
setGeneric("analysisTable", function(object) standardGeneric("analysisTable"))

#' filePaths
#'
#' @noRd
setGeneric("filePaths", function(object) standardGeneric("filePaths"))

#' analysisNames
#'
#' @noRd
setGeneric("analysisNames", function(object) standardGeneric("analysisNames"))

#' replicateNames
#'
#' @noRd
setGeneric("replicateNames", function(object) standardGeneric("replicateNames"))

#' replicateNames<-
#'
#' @noRd
setGeneric("replicateNames<-", function(object, value) standardGeneric("replicateNames<-"))

#' blankReplicateNames
#'
#' @noRd
setGeneric("blankReplicateNames", function(object) standardGeneric("blankReplicateNames"))

#' blankReplicateNames<-
#'
#' @noRd
setGeneric("blankReplicateNames<-", function(object, value) standardGeneric("blankReplicateNames<-"))

#' polarities
#'
#' @noRd
setGeneric("polarities", function(object, ...) standardGeneric("polarities"))

# TODO remove setter for polarities as it should be as in the file

#' polarities<-
#'
#' @noRd
setGeneric("polarities<-", function(object, value) standardGeneric("polarities<-"))

#' addAnalyses
#'
#' @noRd
setGeneric("addAnalyses", function(object, ...) standardGeneric("addAnalyses"))

#' getAnalyses
#'
#' @noRd
setGeneric("getAnalyses", function(object, ...) standardGeneric("getAnalyses"))

#' addMetadata
#'
#' @noRd
setGeneric("addMetadata", function(object, ...) standardGeneric("addMetadata"))

#' loadRawData
#'
#' @noRd
setGeneric("loadRawData", function(object, ...) standardGeneric("loadRawData"))

#' hasLoadedSpectra
#'
#' @noRd
setGeneric("hasLoadedSpectra", function(object, ...) standardGeneric("hasLoadedSpectra"))

#' hasLoadedChromatograms
#'
#' @noRd
setGeneric("hasLoadedChromatograms", function(object, ...) standardGeneric("hasLoadedChromatograms"))

#' EICs
#'
#' @noRd
setGeneric("EICs", function(object, ...) standardGeneric("EICs"))

#' plotEICs
#'
#' @noRd
setGeneric("plotEICs", function(object, ...) standardGeneric("plotEICs"))

#' TICs
#'
#' @noRd
setGeneric("TICs", function(object, ...) standardGeneric("TICs"))

#' plotTICs
#'
#' @noRd
setGeneric("plotTICs", function(object, ...) standardGeneric("plotTICs"))

#' XICs
#'
#' @noRd
setGeneric("XICs", function(object, ...) standardGeneric("XICs"))

#' plotXICs
#'
#' @noRd
setGeneric("plotXICs", function(object, ...) standardGeneric("plotXICs"))

#' MS2s
#'
#' @noRd
setGeneric("MS2s", function(object, ...) standardGeneric("MS2s"))

#' plotMS2s
#'
#' @noRd
setGeneric("plotMS2s", function(object, ...) standardGeneric("plotMS2s"))

#' hasPeaks
#'
#' @noRd
setGeneric("hasPeaks", function(object, ...) standardGeneric("hasPeaks"))

#' peaks
#'
#' @noRd
setGeneric("peaks", function(object, ...) standardGeneric("peaks"))

#' mapPeaks
#'
#' @noRd
setGeneric("mapPeaks", function(object, ...) standardGeneric("mapPeaks"))

#' features
#'
#' @noRd
setGeneric("features", function(object, ...) standardGeneric("features"))

#' plotFeatures
#'
#' @noRd
setGeneric("plotFeatures", function(object, ...) standardGeneric("plotFeatures"))

#' mapFeatures
#'
#' @noRd
setGeneric("mapFeatures", function(object, ...) standardGeneric("mapFeatures"))

#' hasAdjustedRetentionTime
#'
#' @noRd
setGeneric("hasAdjustedRetentionTime", function(object) standardGeneric("hasAdjustedRetentionTime"))

#' plotAnnotation
#'
#' @noRd
setGeneric("plotAnnotation", function(object, ...) standardGeneric("plotAnnotation"))

#' addParameters
#'
#' @noRd
setGeneric("addParameters", function(object, ...) standardGeneric("addParameters"))

#' getParameters
#'
#' @noRd
setGeneric("getParameters", function(object, ...) standardGeneric("getParameters"))

#' getAlgorithm
#'
#' @noRd
setGeneric("getAlgorithm", function(object, ...) standardGeneric("getAlgorithm"))

#' getSettings
#'
#' @noRd
setGeneric("getSettings", function(object, ...) standardGeneric("getSettings"))

#' exportSettings
#'
#' @noRd
setGeneric("exportSettings", function(object, ...) standardGeneric("exportSettings"))

#' as.features
#'
#' @noRd
setGeneric("as.features", function(object, ...) standardGeneric("as.features"))

#' as.featureGroups
#'
#' @noRd
setGeneric("as.featureGroups", function(object, ...) standardGeneric("as.featureGroups"))

### External Generics ---------------------------------------------------------------------------------------

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

#### S4Vectors -----------------------------------------------------------
#' @importMethodsFrom S4Vectors metadata
#' @export
#' @noRd
NULL

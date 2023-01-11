
### Generics -------------------------------------------------------------------

#### setTitle -----

#' @title setTitle
#'
#' @description Generic for getting the title.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("setTitle", function(object, ...) standardGeneric("setTitle"))

#### setTitle<- -----

#' @title setTitle<-
#'
#' @description Generic for setting the title.
#'
#' @param object A method specific class object.
#' @param value A method specific vector with title/s.
#'
setGeneric("setTitle<-", function(object, value) standardGeneric("setTitle<-"))

#### setDate -----

#' @title setDate
#'
#' @description Generic for getting the date.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("setDate", function(object, ...) standardGeneric("setDate"))

#### analysisTable -----

#' @title analysisTable
#'
#' @description Generic for getting an analysis table.
#'
#' @param object A method specific class object.
#'
setGeneric("analysisTable", function(object) standardGeneric("analysisTable"))

#### analysisInfo -----

#' @title analysisInfo
#'
#' @description Generic for getting an analysis table.
#'
#' @param object A method specific class object.
#'
setGeneric("analysisInfo", function(obj) standardGeneric("analysisInfo"))

#### filePath -----

#' @title filePath
#'
#' @description Generic for getting file path
#'
#' @param object A method specific class object.
#'
setGeneric("filePath", function(object) standardGeneric("filePath"))

#### filePaths -----

#' @title filePaths
#'
#' @description Generic for getting file paths
#'
#' @param object A method specific class object.
#'
setGeneric("filePaths", function(object) standardGeneric("filePaths"))

#### analysisName -----

#' @title analysisName
#'
#' @description Generic for getting analysis names.
#'
#' @param object A method specific class object.
#'
setGeneric("analysisName", function(object) standardGeneric("analysisName"))

#### analysisNames -----

#' @title analysisNames
#'
#' @description Generic for getting analysis names.
#'
#' @param object A method specific class object.
#'
setGeneric("analysisNames", function(object) standardGeneric("analysisNames"))

#### replicateNames -----

#' @title replicateNames
#'
#' @description Generic for getting replicate names.
#'
#' @param object A method specific class object.
#'
setGeneric("replicateNames", function(object) standardGeneric("replicateNames"))

#### replicateNames<- -----

#' @title replicateNames<-
#'
#' @description Generic for setting replicate names.
#'
#' @param object A method specific class object.
#' @param value A method specific vector with names.
#'
setGeneric("replicateNames<-", function(object, value) standardGeneric("replicateNames<-"))

#### blankReplicateNames -----

#' @title blankReplicateNames
#'
#' @description Generic for getting blank replicate names.
#'
#' @param object A method specific class object.
#'
setGeneric("blankReplicateNames", function(object) standardGeneric("blankReplicateNames"))

#### blankReplicateNames<- -----

#' @title blankReplicateNames<-
#'
#' @description Generic for setting blank replicate names.
#'
#' @param object A method specific class object.
#' @param value A method specific vector with names.
#'
setGeneric("blankReplicateNames<-", function(object, value) standardGeneric("blankReplicateNames<-"))

#### polarity -----

#' @title polarity
#'
#' @description Generic for getting polarity.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("polarity", function(object, ...) standardGeneric("polarity"))

#### polarities -----

#' @title polarities
#'
#' @description Generic for getting polarities.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("polarities", function(object, ...) standardGeneric("polarities"))

#### addAnalyses -----

#' @title addAnalyses
#'
#' @description Generic for adding analyses.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("addAnalyses", function(object, ...) standardGeneric("addAnalyses"))

#### getAnalyses -----

#' @title getAnalyses
#'
#' @description Generic for getting analyses.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getAnalyses", function(object, ...) standardGeneric("getAnalyses"))

#### getMetadataNames -----

#' @title getMetadataNames
#'
#' @description Generic for getting metadata.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getMetadataNames", function(object, ...) standardGeneric("getMetadataNames"))

#### getMetadata -----

#' @title getMetadata
#'
#' @description Generic for getting metadata.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getMetadata", function(object, ...) standardGeneric("getMetadata"))

#### addMetadata -----

#' @title addMetadata
#'
#' @description Generic for adding metadata.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("addMetadata", function(object, ...) standardGeneric("addMetadata"))

#### getSpectraInfo -----

#' @title getSpectraInfo
#'
#' @description Generic for getting basic spectra information.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getSpectraInfo", function(object, ...) standardGeneric("getSpectraInfo"))

#### getRawData -----

#' @title getRawData
#'
#' @description Generic for getting raw data.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getRawData", function(object, ...) standardGeneric("getRawData"))

#### loadSpectraInfo -----

#' @title loadSpectraInfo
#'
#' @description Generic for loading basic spectra information.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("loadSpectraInfo", function(object, ...) standardGeneric("loadSpectraInfo"))

#### loadRawData -----

#' @title loadRawData
#'
#' @description Generic for loading raw data.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("loadRawData", function(object, ...) standardGeneric("loadRawData"))

#### getSpectra -----

#' @title getSpectra
#'
#' @description Generic for getting spectra.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getSpectra", function(object, ...) standardGeneric("getSpectra"))

#### loadSpectra -----

#' @title loadSpectra
#'
#' @description Generic for loading spectra.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("loadSpectra", function(object, ...) standardGeneric("loadSpectra"))

#### hasLoadedSpectra -----

#' @title hasLoadedSpectra
#'
#' @description Generic for checking if has loaded spectra.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("hasLoadedSpectra", function(object, ...) standardGeneric("hasLoadedSpectra"))

#### spectra -----

#' @title spectra
#'
#' @description Generic for getting spectra.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("spectra", function(object, ...) standardGeneric("spectra"))

#### plotSpectra -----

#' @title plotSpectra
#'
#' @description Generic for plotting spectra.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotSpectra", function(object, ...) standardGeneric("plotSpectra"))

#### getChromatograms -----

#' @title getChromatograms
#'
#' @description Generic for loading chromatograms.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getChromatograms", function(object, ...) standardGeneric("getChromatograms"))

#### loadChromatograms -----

#' @title loadChromatograms
#'
#' @description Generic for loading chromatograms.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("loadChromatograms", function(object, ...) standardGeneric("loadChromatograms"))

#### hasLoadedChromatograms -----

#' @title hasLoadedChromatograms
#'
#' @description Generic for checking if has loaded chromatograms.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("hasLoadedChromatograms", function(object, ...) standardGeneric("hasLoadedChromatograms"))

#### chromatograms -----

#' @title chromatograms
#'
#' @description Generic for getting chromatograms.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("chromatograms", function(object, ...) standardGeneric("chromatograms"))

#### plotChromatograms -----

#' @title plotChromatograms
#'
#' @description Generic for plotting chromatograms.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotChromatograms", function(object, ...) standardGeneric("plotChromatograms"))

#### EICs -----

#' @title EICs
#'
#' @description Generic for getting extracted ion chromatograms (EICs).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("EICs", function(object, ...) standardGeneric("EICs"))

#### plotEICs -----

#' @title plotEICs
#'
#' @description Generic for plotting extracted ion chromatograms (EICs).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotEICs", function(object, ...) standardGeneric("plotEICs"))

#### BPC -----

#' @title BPC
#'
#' @description Generic for getting base peak chromatogram (BPC).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("BPC", function(object, ...) standardGeneric("BPC"))

#### BPCs -----

#' @title BPCs
#'
#' @description Generic for getting base peak chromatograms (BPCs).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("BPCs", function(object, ...) standardGeneric("BPCs"))

#### plotBPC -----

#' @title plotBPC
#'
#' @description Generic for getting base peak chromatogram (BPC).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotBPC", function(object, ...) standardGeneric("plotBPC"))

#### plotBPCs -----

#' @title plotBPCs
#'
#' @description Generic for getting base peak chromatograms (BPCs).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotBPCs", function(object, ...) standardGeneric("plotBPCs"))

#### TIC -----

#' @title TIC
#'
#' @description Generic for getting total ion chromatogram (TIC).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("TIC", function(object, ...) standardGeneric("TIC"))

#### TICs -----

#' @title TICs
#'
#' @description Generic for getting total ion chromatograms (TICs).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("TICs", function(object, ...) standardGeneric("TICs"))

#### plotTIC -----

#' @title plotTIC
#'
#' @description Generic for plotting total ion chromatogram (TIC).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotTIC", function(object, ...) standardGeneric("plotTIC"))

#### plotTICs -----

#' @title plotTICs
#'
#' @description Generic for plotting total ion chromatograms (EICs).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotTICs", function(object, ...) standardGeneric("plotTICs"))

#### XICs -----

#' @title XICs
#'
#' @description Generic for getting three dimensional ion chromatograms (XICs).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("XICs", function(object, ...) standardGeneric("XICs"))

#### plotXICs -----

#' @title plotXICs
#'
#' @description Generic for plotting three dimensional ion chromatograms (XICs).
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotXICs", function(object, ...) standardGeneric("plotXICs"))

#### MS2s -----

#' @title MS2s
#'
#' @description Generic for getting MS 2 spectra.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("MS2s", function(object, ...) standardGeneric("MS2s"))

#### plotMS2s -----

#' @title plotMS2s
#'
#' @description Generic for plotting MS 2 spectra.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotMS2s", function(object, ...) standardGeneric("plotMS2s"))

#### hasPeaks -----

#' @title hasPeaks
#'
#' @description Generic for checking if has peaks.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("hasPeaks", function(object, ...) standardGeneric("hasPeaks"))

#### peaks -----

#' @title peaks
#'
#' @description Generic for getting peaks.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("peaks", function(object, ...) standardGeneric("peaks"))

#### peakEICs -----

#' @title peakEICs
#'
#' @description Generic for getting peak EICs.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("peakEICs", function(object, ...) standardGeneric("peakEICs"))

#### plotPeaks -----

#' @title plotPeaks
#'
#' @description Generic for plotting peaks.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotPeaks", function(object, ...) standardGeneric("plotPeaks"))

#### mapPeaks -----

#' @title mapPeaks
#'
#' @description Generic for plotting a map of peaks.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("mapPeaks", function(object, ...) standardGeneric("mapPeaks"))

#### hasFeatures -----

#' @title hasFeatures
#'
#' @description Generic for check the presence of features.
#'
#' @param object A method specific class object.
#'
setGeneric("hasFeatures", function(object) standardGeneric("hasFeatures"))

#### features -----

#' @title features
#'
#' @description Generic for getting features.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("features", function(object, ...) standardGeneric("features"))

#### plotFeatures -----

#' @title plotFeatures
#'
#' @description Generic for plotting features.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotFeatures", function(object, ...) standardGeneric("plotFeatures"))

#### mapFeatures -----

#' @title mapFeatures
#'
#' @description Generic for plotting a map of features.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("mapFeatures", function(object, ...) standardGeneric("mapFeatures"))

#### hasAdjustedRetentionTime -----

#' @title hasAdjustedRetentionTime
#'
#' @description Generic for checking is has adjusted retention time.
#'
#' @param object A method specific class object.
#'
setGeneric("hasAdjustedRetentionTime", function(object) standardGeneric("hasAdjustedRetentionTime"))

#### annotation -----

#' @title annotation
#'
#' @description Generic for getting annotation.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("annotation", function(object, ...) standardGeneric("annotation"))

#### plotAnnotation -----

#' @title plotAnnotation
#'
#' @description Generic for plotting annotation.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("plotAnnotation", function(object, ...) standardGeneric("plotAnnotation"))

#### addSettings -----

#' @title addSettings
#'
#' @description Generic for adding settings.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("addSettings", function(object, ...) standardGeneric("addSettings"))

#### getSettingsNames -----

#' @title getSettingsNames
#'
#' @description Generic for getting the settings names.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getSettingsNames", function(object, ...) standardGeneric("getSettingsNames"))

#### getSettings -----

#' @title getSettings
#'
#' @description Generic for getting settings.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getSettings", function(object, ...) standardGeneric("getSettings"))

#### getAlgorithm -----

#' @title getAlgorithm
#'
#' @description Generic for getting algorithm.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getAlgorithm", function(object, ...) standardGeneric("getAlgorithm"))

#### getParameters -----

#' @title getParameters
#'
#' @description Generic for getting parameters.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("getParameters", function(object, ...) standardGeneric("getParameters"))

#### exportSettings -----

#' @title exportSettings
#'
#' @description Generic for exporting settings.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("exportSettings", function(object, ...) standardGeneric("exportSettings"))

#### as.features -----

#' @title as.features
#'
#' @description Generic for converting to features.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("as.features", function(object, ...) standardGeneric("as.features"))

#### as.featureGroups -----

#' @title as.featureGroups
#'
#' @description Generic for converting to featureGroups.
#'
#' @param object A method specific class object.
#' @param ... Other method specific arguments.
#'
setGeneric("as.featureGroups", function(object, ...) standardGeneric("as.featureGroups"))

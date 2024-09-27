library(StreamFind)
library(StreamFindData)

# resources --------------------------------------------------------------------
## files -------------------------------------------------------x----------------
all_ms_files <- StreamFindData::get_ms_file_paths()
mrm_files <- all_ms_files[grepl("mrm", all_ms_files)]
ms_files <- all_ms_files[grepl("influent|blank", all_ms_files)]
raman_files <- StreamFindData::get_raman_file_paths()
dev_files <- list.files("E:/example_ms_files", pattern = "mzML", full.names = TRUE)[1:3]
## settings -------------------------------------------------------------------
ps <- list(
  MassSpecSettings_FindFeatures_openms(),
  MassSpecSettings_AnnotateFeatures_StreamFind(),
  MassSpecSettings_GroupFeatures_openms(),
  MassSpecSettings_FilterFeatures_StreamFind(excludeIsotopes = TRUE),
  MassSpecSettings_FilterFeatures_patRoon(absMinIntensity = 5000, maxReplicateIntRSD = 30, blankThreshold = 10, absMinReplicateAbundance = 3),
  MassSpecSettings_LoadFeaturesEIC_StreamFind(rtExpand = 60, mzExpand = 0.0005),
  MassSpecSettings_CalculateQuality_StreamFind(),
  MassSpecSettings_FilterFeatures_StreamFind(minSnRatio = 5),
  MassSpecSettings_LoadFeaturesMS1_StreamFind(),
  MassSpecSettings_LoadFeaturesMS2_StreamFind(),
  MassSpecSettings_LoadMSPeakLists_StreamFind(),
  MassSpecSettings_GenerateFormulas_genform(),
  MassSpecSettings_GenerateCompounds_metfrag()
)

## databases -------------------------------------------------------------------
db <- StreamFindData::get_ms_tof_spiked_chemicals()

## run_app() function ---------------------------------------------------------

run_app()

# CoreEngine -------------------------------------------------------------------
core <- CoreEngine$new()
core$save(paste0(getwd(), "/core.sqlite"))
core$run_app()

# MassSpecEngine ---------------------------------------------------------------
ms <- MassSpecEngine$new(files = dev_files, settings = NULL)
ms$save(paste0(getwd(), "/ms.sqlite"))
ms$run_app()

# RamanEngine ------------------------------------------------------------------
raman <- RamanEngine$new(files = raman_files)
raman$save(paste0(getwd(), "/raman.sqlite"))
raman$run_app()

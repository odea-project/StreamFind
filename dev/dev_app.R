library(StreamFind)
library(StreamFindData)

# resources --------------------------------------------------------------------
## files -------------------------------------------------------x----------------
all_ms_files <- StreamFindData::get_ms_file_paths()
mrm_files <- all_ms_files[grepl("mrm", all_ms_files)]
ms_files <- all_ms_files[grepl("influent|blank", all_ms_files)]
raman_files <- StreamFindData::get_raman_file_paths()
dev_file_path <- "C:/Users/apoli/Documents/example_files"
dev_files <- list.files(dev_file_path, pattern = "mzML", full.names = TRUE)[1:3]
## settings -------------------------------------------------------------------
workflow <- StreamFind::Workflow(
  list(
    MassSpecSettings_FindFeatures_openms(),
    MassSpecSettings_AnnotateFeatures_StreamFind(),
    MassSpecSettings_GroupFeatures_openms(),
    MassSpecSettings_FilterFeatures_StreamFind(excludeIsotopes = TRUE),
    MassSpecSettings_FilterFeatures_patRoon(absMinIntensity = 5000, maxReplicateIntRSD = 30, blankThreshold = 10, absMinReplicateAbundance = 3),
    MassSpecSettings_LoadFeaturesEIC_StreamFind(rtExpand = 60, mzExpand = 0.0005),
    MassSpecSettings_CalculateFeaturesQuality_StreamFind(),
    MassSpecSettings_FilterFeatures_StreamFind(minSnRatio = 5),
    MassSpecSettings_LoadFeaturesMS1_StreamFind(),
    MassSpecSettings_LoadFeaturesMS2_StreamFind()
    # MassSpecSettings_LoadMSPeakLists_StreamFind(),
    # MassSpecSettings_GenerateFormulas_genform(),
    # MassSpecSettings_GenerateCompounds_metfrag()
  )
)

# save(workflow, format = "rds", name = "workflow")

## databases -------------------------------------------------------------------
db <- StreamFindData::get_ms_tof_spiked_chemicals()

## run_app() function ---------------------------------------------------------

# run_app()

# CoreEngine -------------------------------------------------------------------
# core <- CoreEngine$new()
# core$save(paste0(getwd(), "/core.rds"))
# core$run_app()

# MassSpecEngine ---------------------------------------------------------------
ms <- MassSpecEngine$new(analyses = dev_files, workflow = workflow)
ms$save(paste0(getwd(), "/ms.rds"))
ms$run_app()

# RamanEngine ------------------------------------------------------------------
raman <- RamanEngine$new(analyses = raman_files)
raman$analyses$replicates <- c(rep("BVCZ", 11), rep("Blank", 11))
raman$analyses$blanks <- rep("Blank", 22)
raman_workflow <- StreamFind::Workflow(
  list(
    RamanSettings_AverageSpectra_StreamFind(),
    RamanSettings_SubtractBlankSpectra_StreamFind()
  )
)
raman$workflow <- raman_workflow
raman$run_workflow()
raman$save(paste0(getwd(), "/raman.rds"))
raman$run_app()

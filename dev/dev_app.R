library(StreamFind)
library(StreamFindData)

# resources --------------------------------------------------------------------
## files -------------------------------------------------------x----------------
all_ms_files <- StreamFindData::get_ms_file_paths()
mrm_files <- all_ms_files[grepl("mrm", all_ms_files)]
ms_files <- all_ms_files[grepl("influent|blank", all_ms_files)]
raman_files <- StreamFindData::get_raman_file_paths()
dev_file_path <- "C:/Users/apoli/Documents/example_ms_files"
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
    MassSpecSettings_LoadFeaturesMS2_StreamFind(),
    MassSpecSettings_LoadMSPeakLists_StreamFind(),
    MassSpecSettings_GenerateFormulas_genform(),
    MassSpecSettings_GenerateCompounds_metfrag()
  )
)

save(workflow, format = "rds", name = "workflow")

## databases -------------------------------------------------------------------
db <- StreamFindData::get_ms_tof_spiked_chemicals()

## run_app() function ---------------------------------------------------------

run_app()

# CoreEngine -------------------------------------------------------------------
core <- CoreEngine$new()
core$save(paste0(getwd(), "/core.rds"))
core$run_app()

# MassSpecEngine ---------------------------------------------------------------
ms <- MassSpecEngine$new(analyses = dev_files, workflow = NULL)
ms$save(paste0(getwd(), "/ms.rds"))
ms$run_app()

# RamanEngine ------------------------------------------------------------------
raman <- RamanEngine$new(analyses = raman_files)
raman$save(paste0(getwd(), "/raman.rds"))
raman$run_app()


new_order <- c("X\n1_FindFeatures_openms\nDetails", "X\n2_AnnotateFeatures_StreamFind\nDetails", "X\n3_GroupFeatures_openms\nDetails")

vapply(new_order, function(x) strsplit(x, "\n")[[1]][2], NA_character_)

StreamFind::MassSpecSettings_BinSpectra_StreamFind()
# , "X\n4_FilterFeatures_StreamFind\nDetails"
# [5] "X\n5_FilterFeatures_patRoon\nDetails"              "X\n6_LoadFeaturesEIC_StreamFind\nDetails"          "X\n7_CalculateFeaturesQuality_StreamFind\nDetails" "X\n8_FilterFeatures_StreamFind\nDetails"
# [9] "X\n9_LoadFeaturesMS1_StreamFind\nDetails"          "X\n10_LoadFeaturesMS2_StreamFind\nDetails"         "X\n11_LoadMSPeakLists_StreamFind\nDetails"         "X\n12_GenerateFormulas_genform\nDetails"
# [13] "X\n13_GenerateCompounds_metfrag\nDetails"

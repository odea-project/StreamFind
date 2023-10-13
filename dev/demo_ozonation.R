
library(StreamFind)




# Create -----------------------------------------------------------------------

all_files <- StreamFindData::get_ms_file_paths()

files <- all_files[grepl("blank|influent|o3sw", all_files)]

ms <- MassSpecData$new(files = files)

ms$add_headers(name = "Wastewater Project")

# Print method and access methods
ms

# ms$plot_bpc(levels = 1, colorBy = "replicates")







# Change -----------------------------------------------------------------------

rpls <- c(
  rep("blank_neg", 3),
  rep("blank_pos", 3),
  rep("in_neg", 3),
  rep("in_pos", 3),
  rep("out_neg", 3),
  rep("out_pos", 3)
)

blks <- c(
  rep("blank_neg", 3),
  rep("blank_pos", 3),
  rep("blank_neg", 3),
  rep("blank_pos", 3),
  rep("blank_neg", 3),
  rep("blank_pos", 3)
)

ms$add_replicate_names(rpls)$add_blank_names(blks)

ms






# Processing settings ----------------------------------------------------------

ffs <- Settings_find_features_xcms3_centwave()

# Settings documentation
# ?Settings_find_features_xcms3_centwave

# browseURL(ffs$link)


# Saves an example of settings on disk
save_default_ProcessingSettings(
  call = "find_features",
  algorithm = "xcms3_centwave",
  format = "json",
  name = "ffs"
)

ms$import_settings("ffs.json")

# Other module processing settings
fls <- Settings_filter_features_StreamFind(
  minIntensity = 5000,
  minSnRatio = 20,
  maxGroupSd = 30,
  blank = 5,
  minGroupAbundance = 3,
  excludeIsotopes = TRUE
)

afs <- Settings_annotate_features_StreamFind(
  maxIsotopes = 5,
  elements = c("C", "H", "N", "O", "S", "Cl", "Br"),
  mode = "small molecules",
  maxCharge = 2,
  rtWindowAlignment = 0.3,
  maxGaps = 1,
  runParallel = FALSE
)

gfs <- Settings_group_features_xcms3_peakdensity()






# Process data -----------------------------------------------------------------

ms$find_features()$annotate_features(afs)$group_features(gfs)$filter_features(fls)

# Alternative to chaining the processing modules
# ms$run_workflow()

ms

# Access and plot data ---------------------------------------------------------

# Database
db <- StreamFindData::get_ms_tof_spiked_chemicals()
db <- db[grepl("S", db$tag), ]
cols <- c("name", "formula", "mass", "rt", "tag")
db <- db[, cols, with = FALSE]
db



# Getting feature groups from the database
ms$get_groups(mass = db, ppm = 8, sec = 10, average = TRUE)



# Change filtered to TRUE to show hidden data
ms$get_groups(mass = db, ppm = 8, sec = 10, filtered = TRUE, average = TRUE)



# Plot overview of feature groups
ms$plot_groups_overview(
  mass = db,
  ppm = 8, sec = 10,
  filtered = TRUE,
  legendNames = TRUE
)



# Raw extracted ion chromatogram
ms$plot_eic(
  mz = 434.22, # Valsartan m/z as [M+H]+
  rt = 1176,
  ppm = 20,
  sec = 60,
  colorBy = "analyses"
)



# Isotopic chains
ms$get_components(
  analyses = 10,
  mass = db,
  ppm = 8, sec = 10
)






# Audit trail ------------------------------------------------------------------

ms$get_history()







# Export -----------------------------------------------------------------------

# Saves processing settings
ms$save_settings(format = "json", name = "settings")



# Saves the hole project
ms$save(format = "json", name = "MassSpecData_example")










file.remove("ffs.json")
file.remove("settings.json")
file.remove("MassSpecData_example.json")





# files <- files[c(1:2, 4:5, 7:8, 10:11)]

# sss <- Settings_suspect_screening_forident(addMS2 = TRUE)
# ms$add_settings(list(afs, gfs, fls, sss))

# ms2 <- ms$subset_groups(groups = c("m440.160_rt1098_g1126", "m267.180_rt916_g374", "m295.020_rt1256_g527"))
# ms2 <- ms2$subset_analyses(analyses = 7:8)
# ms2
# ms2$load_features_ms2(Settings_load_features_ms2_StreamFind())
# ms2$load_groups_ms2(Settings_load_groups_ms2_StreamFind())
# ms2$plot_groups()
# ms2$suspect_screening()
# ms2

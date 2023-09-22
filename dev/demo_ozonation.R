
library(StreamFind)

# Create -----------------------------------------------------------------------

all_files <- StreamFindData::get_all_file_paths()
files <- all_files[grepl("blank|influent|o3sw", all_files)]

headers <- ProjectHeaders(
  name = "Wastewater Ozonation",
  author = "Ricardo Cunha",
  description = "StreamFind Demonstration project"
)

ms <- MassSpecData$new(files = files, headers = headers)

# Print method and access methods
ms






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
?Settings_find_features_xcms3_centwave

browseURL(ffs$link)


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

afs <- Settings_annotate_features_StreamFind()

gfs <- Settings_group_features_xcms3_peakdensity()






# Process data -----------------------------------------------------------------

ms$find_features()$annotate_features(afs)$group_features(gfs)$filter_features(fls)

ms






# Access and plot data ---------------------------------------------------------

# Database
db <- StreamFindData::get_tof_spiked_chemicals()
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
ms$map_components(
  analyses = 10,
  mass = db,
  ppm = 8, sec = 10,
  legendNames = TRUE
)






# Audit trail ------------------------------------------------------------------

ms$get_history()






# Export -----------------------------------------------------------------------

# Saves processing settings
ms$save_settings(format = "json", name = "settings")

# Saves the hole project
ms$save(format = "json", name = "MassSpecData_example")







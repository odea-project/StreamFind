
library(StreamFind)


# Convert files ----------------------------------------------------------------

# Convert vendor file formats to open source mzML
# ?convert_ms_files

# Load file paths
all_files <- StreamFindData::get_ms_file_paths()
files <- all_files[grepl("blank|influent|o3sw", all_files)]







# Create -----------------------------------------------------------------------

# Make project headers
headers <- ProjectHeaders(
  name = "Wastewater project",
  author = "Ricardo Cunha",
  description = "Demo"
)

  
# Create the "Engine"
ms <- MassSpecEngine$new(files = files, headers = headers)

ms$plot_spectra_bpc(analyses = c(4, 10), levels = 1)





# Change -----------------------------------------------------------------------

# Replicate names
rpls <- c(
  rep("blank_neg", 3), rep("blank_pos", 3),
  rep("influent_neg", 3), rep("influent_pos", 3),
  rep("effluent_neg", 3), rep("effluent_pos", 3)
)

# Respective blank replicate names
blks <- c(
  rep("blank_neg", 3), rep("blank_pos", 3),
  rep("blank_neg", 3), rep("blank_pos", 3),
  rep("blank_neg", 3), rep("blank_pos", 3)
)

# Add names to the Engine
ms$add_replicate_names(rpls)$add_blank_names(blks)







# Processing settings ----------------------------------------------------------

ffs <- Settings_find_features_openms()

# Settings documentation
# ?Settings_find_features_openms

# Web reference page 
# ms$help$settings_find_features()

# Saves an example of settings on disk
save_default_ProcessingSettings(
  call = "find_features",
  algorithm = "openms",
  format = "json",
  name = "ffs"
)


ms$import_settings("ffs.json")
# ms$add_settings(ffs)




# Loads a database of chemicals
db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
cols <- c("name", "formula", "mass", "rt", "polarity", "fragments", "tag")
db <- db[, cols, with = FALSE]
dbis <- db[grepl("IS", db$tag), ] # Only spiked internal standards
dbsus <- db[!grepl("IS", db$tag), ]

# Add other module processing settings
ms$add_settings(
  list(
    Settings_annotate_features_StreamFind(),

    Settings_group_features_openms(),

    Settings_find_internal_standards_StreamFind(database = dbis, ppm = 8, sec = 10),

    Settings_filter_features_StreamFind(excludeIsotopes = TRUE),

    Settings_filter_features_patRoon(
      absMinIntensity = 5000, maxReplicateIntRSD = 30, blankThreshold = 5, absMinReplicateAbundance = 3
    ),

    Settings_load_features_eic_StreamFind(rtExpand = 60, mzExpand = 0.0005),

    Settings_calculate_quality_StreamFind(),

    Settings_filter_features_StreamFind(minSnRatio = 3),
    
    Settings_load_features_ms2_StreamFind(),

    Settings_suspect_screening_StreamFind(database = dbsus, ppm = 5, sec = 10)
  )
)







# Process data -----------------------------------------------------------------

# Process all modules in workflow
ms$run_workflow()

# Alternative to chaining the processing modules
# ms$find_features()$annotate_features()$group_features()$filter_features()







# Access and plot data ---------------------------------------------------------


# Getting feature groups from the database
ms$get_groups(mass = db, ppm = 8, sec = 10)

# Plot suspect identification results
ms$plot_suspects()

# Plot overview of feature groups
ms$plot_groups_overview(
  mass = db,
  ppm = 8, sec = 10,
  legendNames = TRUE
)


# Change filtered to TRUE to show hidden data
ms$get_groups(mass = db, ppm = 8, sec = 10, filtered = TRUE, average = TRUE)

# Quality check for spiked internal standards
ms$plot_internal_standards_qc()


# EIC for given targets
ms$plot_spectra_eic(
  mass = 233.1131, # Naproxen-d3
  rt = 1169,
  ppm = 10,
  sec = 30,
  colorBy = "analyses"
)

# Plot suspect identification results
ms$get_suspects()

# MS2 spectra
ms$plot_groups_ms2(
  mass = db,
  colorBy = "targets+polarities",
  legendNames = TRUE
)


# Isotopic clusters
ms$map_isotopes(
  analyses = 4,
  mass = db,
  filtered = TRUE,
  ppm = 8, sec = 10
)






# Audit trail ------------------------------------------------------------------

ms$get_history()





# Export -----------------------------------------------------------------------

# Saves processing settings
ms$save_settings(format = "json", name = "settings")


# Fast reproducibility of workflow
ms2 <- MassSpecEngine$new(files = all_files[1:3])
ms2$import_settings(file = "settings.json")
ms2$run_workflow()
ms2




file.remove("ffs.json")
file.remove("settings.json")

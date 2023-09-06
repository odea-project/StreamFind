
library(StreamFind)

# create -----------------------------------------------------------------------

all_files <- StreamFindData::msFilePaths()

files <- all_files[grepl("blank|influent|o3sw", all_files)]

headers <- ProjectHeaders(
  name = "Example wastewater ozonation",
  author = "Ricardo Cunha"
)

ms <- MassSpecData$new(files = files, headers = headers)

# print method and access methods
ms






# change -----------------------------------------------------------------------

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






# processing settings ----------------------------------------------------------

# saves an example of settings on disk
save_default_ProcessingSettings(
  call = "find_features",
  algorithm = "xcms3_centwave",
  format = "json",
  name = "ffs"
)

ms$import_settings("ffs.json")






# process ----------------------------------------------------------------------

# gets settings as object
fls <- Settings_filter_features_StreamFind(
  minIntensity = 5000,
  minSnRatio = 20,
  maxGroupSd = 30,
  blank = 5,
  minGroupAbundance = 3,
  excludeIsotopes = TRUE
)

fls

# other module settings
afs <- Settings_annotate_features_StreamFind()
gfs <- Settings_group_features_xcms3_peakdensity()


ms$find_features()$annotate_features(afs)$group_features(gfs)$filter_features(fls)

ms






# plot -------------------------------------------------------------------------

# database
db <- StreamFindData::msSpikedChemicals()
db <- db[grepl("S", db$tag), ]
cols <- c("name", "formula", "mass", "rt", "tag")
db <- db[, cols, with = FALSE]
db



# getting feature groups from the database
ms$get_groups(mass = db, ppm = 8, sec = 10, average = TRUE)



# change filtered to TRUE to show hidden data
ms$get_features(analyses = 10, mass = db, ppm = 8, sec = 10, filtered = TRUE)



# overview plot
ms$plot_groups_overview(
  mass = db,
  ppm = 8, sec = 10,
  filtered = TRUE,
  legendNames = TRUE
)



# extracted ion chromatogram
ms$plot_eic(
  mz = 434.22, # Valsartan m/z as [M+H]+
  rt = 1176,
  ppm = 20,
  sec = 60,
  colorBy = "analyses"
)



# isotopic chains
ms$map_components(
  analyses = 10,
  mass = db,
  ppm = 8, sec = 10,
  legendNames = TRUE
)






# history ----------------------------------------------------------------------

ms$get_history()

# extra ------------------------------------------------------------------------

ms$save_settings(format = "json", name = "settings")

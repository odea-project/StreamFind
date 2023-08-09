
library(streamFind)

# create -----------------------------------------------------------------------

all_files <- streamFindData::msFilePaths()
files <- all_files[grepl("blank|influent|o3sw", all_files)]

headers <- Headers(
  name = "Example wastewater ozonation",
  author = "Ricardo Cunha"
)

ms <- MassSpecData$new(files = files, headers = headers)

# print method
ms

# ms$plot_bpc(interactive = TRUE)

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
  algorithm = "centwave",
  format = "json",
  name = "ffs"
)

ms$import_settings("ffs.json")

ms


# process ----------------------------------------------------------------------

# gets an example of settings as object
afs <- get_default_ProcessingSettings(
  call = "annotate_features",
  algorithm = "streamFind"
)

afs

gfs <- get_default_ProcessingSettings(
  call = "group_features",
  algorithm = "peakdensity"
)

gfs

fls <- get_default_ProcessingSettings(
  call = "filter_features",
  algorithm = "streamFind"
)

fls

fls$parameters$minIntensity <- 3000

ms$find_features()$annotate_features(afs)$group_features(gfs)$filter_features(fls)

ms


# plot -------------------------------------------------------------------------

db <- streamFindData::msSpikedChemicals()
db <- db[grepl("S", db$tag), ]
cols <- c("name", "formula", "mass", "rt", "tag")
db <- db[, cols, with = FALSE]
db

ms$get_groups(mass = db, ppm = 8, sec = 10, average = TRUE)

# changing filtered to TRUE to show hidden data
ms$get_groups(mass = db, ppm = 8, sec = 10, average = TRUE, filtered = TRUE)

ms$plot_groups_overview(
  mass = db,
  ppm = 8, sec = 10,
  filtered = TRUE,
  legendNames = TRUE
)

ms$plot_eic(
  analyses = ms$get_analysis_names()[grepl("neg", ms$get_analysis_names())],
  mz = db$mass[db$name %in% "Valsartan"] - 1.0073,
  ppm = 20,
  colorBy = "analyses"
)

ms$map_components(mass = db[24, ], ppm = 8, sec = 10, colorBy = "replicates")

# history ----------------------------------------------------------------------

ms$get_history()


# extra ------------------------------------------------------------------------

ms$save_settings(format = "json", name = "settings")


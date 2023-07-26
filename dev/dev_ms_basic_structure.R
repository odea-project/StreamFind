
# mzml files -------------------------------------------------------------------

all_files <- streamFindData::msFilePaths()

files <- all_files[grepl("blank|influent|o3sw", all_files)]



# create -----------------------------------------------------------------------

headers <- Headers(
  name = "Example wastewater ozonation",
  author = "Ricardo Cunha"
)

ms <- MassSpecData$new(files = files, headers = headers)

# print method
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

save_default_ProcessingSettings(
  call = "find_features",
  algorithm = "centwave",
  format = "json",
  name = "ffs"
)

ms$add_replicate_names(rpls)$add_blank_names(blks)$import_settings("ffs.json")

ms$get_settings()

ms$get_overview()

ms$plot_bpc(interactive = TRUE)


# process ----------------------------------------------------------------------

gfs <- get_default_ProcessingSettings(
  call = "group_features",
  algorithm = "peakdensity"
)

gfs

ms$find_features()$group_features(gfs)

ms


# filter -----------------------------------------------------------------------

filters <- ProcessingSettings(
  call = "filter_features",
  algorithm = "streamFind",
  parameters = list(
    "minIntensity" = 5000,
    "minSnRatio" = 25,
    "maxGroupSd" = 30,
    "blank" = 5,
    "minGroupAbundance" = 3
  )
)

ms$filter_features(filters)

ms$get_overview()


# plot -------------------------------------------------------------------------

db <- streamFindData::msSpikedChemicals()

iStandards <- db[grepl("IS", db$tag), ]
iStandards <- iStandards[, .(name, formula, mass, rt)]
iStandards

standards <- db[db$tag %in% "S", ]
standards <- standards[, .(name, formula, mass, rt)]
standards

ms$get_groups(mass = standards, ppm = 5, sec = 10, average = TRUE)

ms$plot_groups(mass = standards, ppm = 5, sec = 10, legendNames = TRUE)

# changing filtered to TRUE to show hidden data
ms$get_groups(mass = iStandards, ppm = 8, sec = 15, average = TRUE, filtered = FALSE)

ms$plot_groups_overview(
  mass = rbind(iStandards, standards),
  ppm = 5, sec = 10,
  filtered = TRUE,
  legendNames = TRUE
)

# useful for development/optimization
ms$plot_xic(
  analyses = 4,
  mz = 239.062,
  rt = 1157,
  ppm = 50,
  sec = 120,
  plotTargetMark = TRUE,
  targetsMark = data.frame(
    mz = 239.062,
    rt = 1157
  ),
  ppmMark = 5,
  secMark = 10
)


# history ----------------------------------------------------------------------

ms$get_history()


# extra ------------------------------------------------------------------------

ms$get_headers()

ms$save_settings()



all_files <- streamFindData::msFilePaths()

files_df <- data.frame(
  "file" = all_files[grepl("blank|influent|o3sw", all_files)],
  "replicate" = c(
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("in_neg", 3),
    rep("in_pos", 3),
    rep("out_neg", 3),
    rep("out_pos", 3)
  ),
  "blank" = c(
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("blank_neg", 3),
    rep("blank_pos", 3)
  )
)

ms <- MassSpecData$new(files = files_df)

# save_default_ProcessingSettings(
#   call = "find_features",
#   algorithm = "centwave",
#   format = "json",
#   name = "ffs"
# )
#
# save_default_ProcessingSettings(
#   call = "group_features",
#   algorithm = "peakdensity",
#   name = "gfs"
# )
#
# save_default_ProcessingSettings(
#   call = "filter_features",
#   algorithm = "streamFind",
#   name = "fls"
# )

# save_default_ProcessingSettings(
#   call = "find_features",
#   algorithm = "kpic2",
#   format = "json",
#   name = "ffs"
# )




ms$import_settings("ffs.json")
ms$import_settings("gfs.json")
ms$import_settings("fls.json")

ms$find_features()#$group_features()#$filter_features()

suspects <- ms$suspect_screening(database = db)

View(suspects)


# plot -------------------------------------------------------------------------

db <- streamFindData::msSpikedChemicals()
db <- db[grepl("S", db$tag), ]
cols <- c("name", "formula", "mass", "rt")
db <- db[, cols, with = FALSE]
db

ms$plot_groups_overview(
  mass = db,
  ppm = 8, sec = 10,
  filtered = TRUE,
  legendNames = TRUE
)


# history ----------------------------------------------------------------------

ms$get_history()


# extra ------------------------------------------------------------------------

ms$save_settings(format = "json", name = "settings")


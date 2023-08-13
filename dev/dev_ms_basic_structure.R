
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

x <- Settings_filter_features_streamFind()
y <- Settings_annotate_features_streamFind()
z <- Settings_load_features_ms1_streamFind()
t <- Settings_load_features_ms2_streamFind()
g <- Settings_load_groups_ms1_streamFind()
f <- Settings_load_groups_ms2_streamFind()
j <- Settings_find_features_xcms3_centwave()

class(x)
class(y)
class(z)
class(t)
class(j)

sloop::s3_dispatch(validate(x))
sloop::s3_dispatch(validate(y))
sloop::s3_dispatch(validate(z))
sloop::s3_dispatch(validate(t))
sloop::s3_dispatch(validate(j))
sloop::s3_dispatch(validate(g))
sloop::s3_dispatch(validate(f))



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

suspects <- ms$get_suspects(database = db)

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


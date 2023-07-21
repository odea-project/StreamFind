
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

#ms

oview <- ms$get_overview()

#View(oview)

#ms$plot_bpc()



# process ----------------------------------------------------------------------

gfs <- get_default_ProcessingSettings(
  call = "group_features",
  algorithm = "peakdensity"
)

#gfs

ms$find_features()$group_features(gfs)

#ms

#ms$save_settings()

# filter -----------------------------------------------------------------------



# plot -------------------------------------------------------------------------

db <- streamFindData::msSpikedChemicals()
db <- db[grepl("IS", db$tag), ]

db[, .(name, formula, mass, rt)]



ms$get_groups(mass = db[, .(name, formula, mass, rt)], ppm = 20, sec = 30, onlyIntensities = F, average = T)

ms$plot_groups(mass = db[, .(name, formula, mass, rt)], ppm = 20, sec = 30)

ms$plot_groups_overview(mass = db[, .(name, formula, mass, rt)], ppm = 20, sec = 30, legendNames = T)



ms$get_features_eic(mass = db[1:3, .(name, mass, rt)], ppm = 20, sec = 30)
ms$plot_features_ms2(mass = db[1:3, .(name, mass, rt)], ppm = 20, sec = 30, legendNames = T)
ms$get_features_ms2(mass = db[1:3, .(name, mass, rt)], ppm = 20, sec = 30)
ms$plot_features(mass = db[, .(name, mass, rt)], ppm = 20, sec = 30, legendNames = TRUE, interactive = T)
ms$map_features(mass = db[, .(name, mass, rt)], ppm = 20, sec = 30, legendNames = F, interactive = F)




#ms$plot_groups(mass = db[, .(name, formula, mass, rt)], ppm = 20, sec = 30)


#






ms$plot_xic(
  analyses = 16:17,
  mz = 266.0625 + 1.0073,
  rt = 1007,
  ppm = 12,
  sec = 60
)

ms$plot_features(
  analyses = 16:17,
  mz = 266.0625 + 1.0073,
  rt = 1007,
  ppm = 12,
  sec = 60
)










# settings ---------------------------------------------------------------------

save_default_ProcessingSettings(
  call = "find_features",
  algorithm = "centwave",
  format = "json",
  name = "settings_find_features"
)

save_default_ProcessingSettings(
  call = "group_features",
  algorithm = "peakdensity",
  format = "json",
  name = "settings_group_features"
)




# create ----------------------------------------------------------------------



ms$find_features()















# other code -------------------------------------------------------------------

db_cols <- c("name", "mass", "rt")
carbamazepin_d10 <- db[db$name %in% "Carbamazepin-d10", db_cols, with = FALSE]
diuron_d6 <- db[db$name %in% "Diuron-d6", db_cols, with = FALSE]
carb_pos <- carbamazepin_d10$mass + 1.007276
carb <- carbamazepin_d10$mass
carb_rt <- carbamazepin_d10$rt
diu_pos <- diuron_d6$mass + 1.007276
diu <- diuron_d6$mass
diu_rt <- diuron_d6$rt

sec_dev <- 30
ppm_dev <- 10

targets <- make_ms_targets(
  mz = data.frame(
    id = c("tg1", "tg2"),
    mz = c(carb_pos, diu_pos),
    rt = c(carb_rt, diu_rt)
  ),
  ppm = ppm_dev,
  sec = sec_dev
)

neutral_targets <- make_ms_targets(
  mz = data.frame(
    id = c("tg1", "tg2"),
    mz = c(carb, diu),
    rt = c(carb_rt, diu_rt)
  ),
  ppm = ppm_dev,
  sec = sec_dev
)




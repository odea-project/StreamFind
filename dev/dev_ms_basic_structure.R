
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

oview <- ms$get_overview()

#View(oview)

#ms$plot_bpc(interactive = TRUE)


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

ms$filter(filters)

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

ms$get_groups(mass = iStandards, ppm = 8, sec = 15, average = TRUE, filtered = TRUE)

ms$plot_groups_overview(
  mass = rbind(iStandards, standards),
  ppm = 5, sec = 10,
  filtered = TRUE,
  legendNames = TRUE
)


# history ----------------------------------------------------------------------

ms$get_history()























# other code -------------------------------------------------------------------


patRoon::clearCache("parsed_ms_spectra")


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




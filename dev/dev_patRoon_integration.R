
library(patRoon)

# Resources -------------------------------------------------------------------

pat_all_files <- StreamFindData::get_ms_file_paths()

pat_all_db <- StreamFindData::get_ms_tof_spiked_chemicals()

cols <- c("name", "formula", "mass", "rt", "polarity", "fragments", "tag")
db <- pat_all_db[, cols, with = FALSE]
dbis <- db[grepl("IS", db$tag), ] # Only spiked internal standards
dbsus <- db[!grepl("IS", db$tag), ]

pat_db <- pat_all_db[!grepl("IS", pat_all_db$tag), ]
pat_cols <- c("name", "formula", "mass", "SMILES", "rt")
pat_db <- pat_db[, pat_cols, with = FALSE]
data.table::setnames(pat_db, "mass", "neutralMass")

pat_db$neutralMass <- pat_db$neutralMass + 1.0073
data.table::setnames(pat_db, "neutralMass", "mz")

pat_dbis <- pat_all_db[grepl("IS", pat_all_db$tag), ]
pat_cols <- c("name", "formula", "mass", "rt")
pat_dbis <- pat_dbis[, pat_cols, with = FALSE]
data.table::setnames(pat_dbis, "mass", "neutralMass")



# Test find_internal_standards -------------------------------------------------

anaInfo <- data.frame(
  "path" = dirname(pat_all_files[1]),
  "analysis" = tools::file_path_sans_ext(basename(pat_all_files[grepl("blank|influent|o3sw", pat_all_files)])),
  "group" = c(
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

ms <- MassSpecEngine$new(files = anaInfo)

ms$add_settings(list(
  Settings_find_features_openms(),
  Settings_group_features_openms(),
  Settings_fill_features_StreamFind(),
  # Settings_annotate_features_StreamFind(),
  Settings_find_internal_standards_StreamFind(database = dbis, ppm = 8, sec = 10)
  # Settings_filter_features_StreamFind(excludeIsotopes = TRUE),
  # Settings_filter_features_patRoon(absMinIntensity = 5000, maxReplicateIntRSD = 30, blankThreshold = 5, absMinReplicateAbundance = 3),
  # Settings_load_features_eic_StreamFind(rtExpand = 60, mzExpand = 0.0005),
  # Settings_calculate_quality_StreamFind(),
  # Settings_filter_features_StreamFind(minSnRatio = 3),
  # Settings_load_features_ms2_StreamFind(),
  # Settings_suspect_screening_StreamFind(database = dbsus, ppm = 5, sec = 10)
))

ms$run_workflow()

ms$plot_groups_profile(groups = ms$get_internal_standards()$group, normalized = TRUE, legendNames = TRUE, filtered = TRUE)

ms$get_features(mass = dbis[1, ], filtered = TRUE)

ms$plot_features(mass = dbis[1, ], legendNames = FALSE, filtered = TRUE, colorBy = "analyses+targets")

ms$plot_spectra_xic(analyses = 1, mass = dbis[1, ])

# ms$plot_groups_profile(mass = dbsus, legendNames = TRUE)

ms$featureGroups <- patRoon::normInts(
  ms$featureGroups,
  featNorm = "istd",
  groupNorm = FALSE,
  normFunc = max,
  standards = pat_dbis,
  ISTDRTWindow = 120,
  ISTDMZWindow = 300,
  minISTDs = 3
)

ms$plot_groups_profile(groups = ms$get_internal_standards()$group, normalized = TRUE, legendNames = TRUE)

# ms$plot_groups_profile(mass = dbsus, legendNames = TRUE)

View(ms$get_features(mass = dbsus))


# anaInfoPos <- anaInfo[grepl("pos", anaInfo$analysis), ]
# anaInfoNeg <- anaInfo[grepl("neg", anaInfo$analysis), ]
# fListPos <- findFeatures(anaInfoPos, "openms")
# fListNeg <- findFeatures(anaInfoNeg, "openms")
# fList <- makeSet(fListPos, fListNeg, adducts = c("[M+H]+", "[M-H]-"))
# fList <- fListPos 
# gfs <- groupFeatures(fListPos, "openms", verbose = TRUE)
# getTICs(fListPos[2], MSLevel = 2)
# getTICs(gfs[1, ])
# getBPCs(fListPos)
# getBPCs(gfs[3, ])
# plotTICs(fListPos, colourBy = "rGroups", MSLevel = 1)
# plotBPCs(fListPos, colourBy = "analyses", MSLevel = 1)
# plotTICs(gfs, colourBy = "analyses", MSLevel = 1, retentionRange = c(1100, 1200))
# plotBPCs(gfs, colourBy = "analyses", MSLevel = 1, retentionRange = c(1100, 1200))
# plotChroms(gfs[5, 3])
# plot(gfs)
# plotInt(gfs)
# sus <- screenSuspects(
#   gfs,
#   pat_db,
#   rtWindow = 10,
#   mzWindow = 0.005,
#   skipInvalid = TRUE,
#   prefCalcChemProps = TRUE,
#   neutralChemProps = FALSE,
#   onlyHits = FALSE
# )
# View(gfs[, !names(gfs@groups) %in% gfs@features@features$`01_tof_ww_is_pos_blank-r002`$group])
# temp <- normInts(gfs, featNorm = "istd", standards = pat_dbis, adduct = "[M+H]+")
# all.equal(gfs@groups, temp@groups)
# sustox <- predictTox(sus)
# suscalctox <- calculateTox(sustox)
# toxicities(suscalctox)

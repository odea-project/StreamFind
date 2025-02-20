
library(StreamFind)

all_files <- StreamFindData::get_ms_file_paths()
files <- all_files[grepl("blank|influent|o3sw", all_files)]
db_all <- StreamFindData::get_ms_tof_spiked_chemicals()
db_all <- db_all[grepl("S", db_all$tag), ]
cols <- c("name", "formula", "mass", "rt", "tag")
db_is <- db_all[db_all$tag %in% "IS", ]
db_is <- db_is[, cols, with = FALSE]
db_is <- db_is[!db_is$name %in% c("Ibuprofen-d3", "Naproxen-d3"), ]
db <- db_all[db_all$tag %in% "S", ]
db <- db[, cols, with = FALSE]
db_with_ms2 <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
db_with_ms2 <- db_with_ms2[db_with_ms2$tag %in% "S", ]
db_with_ms2 <- db_with_ms2[, c("name", "formula", "mass", "SMILES", "rt", "polarity", "fragments"), with = FALSE]
db_with_ms2$polarity[db_with_ms2$polarity == 1] <- "positive"
db_with_ms2$polarity[is.na(db_with_ms2$polarity)] <- "positive"
db_with_ms2$polarity[db_with_ms2$polarity == -1] <- "negative"

ms <- MassSpecEngine$new(analyses = files)

# ms$Analyses$has_results_nts
# ms$has_results_nts()
# ms$Analyses$NTS

# ms$get_spectra_bpc(analyses = 1:2)
# ms$get_spectra_headers()
# ms$get_instruments()
# ms$plot_spectra_tic(levels = 1)
# ms$plot_spectra_bpc(levels = 1)
# plot_spectra_eic(ms$Analyses, mass = db[2, ], colorBy = "replicates+targets")
# plot_spectra_xic(ms$Analyses, analyses = c(11, 17), mass = db[2, ],)
# plot_spectra_ms1(
#   ms$Analyses,
#   analyses = c(11),
#   mass = data.frame(min = 265, max =  280, rtmin = 910, rtmax = 920),
#   colorBy = "replicates+targets",
#   interactive = T
# )
# plot_spectra_ms2(
#   ms$Analyses,
#   analyses = c(11, 17),
#   mass = db[2, ],
#   colorBy = "replicates+targets",
#   interactive = T
# )

ms$Metadata <- list(
  name = "Wastewater Ozonation Showcase",
  author = "Ricardo Cunha",
  description = "Demonstration project"
)

rpls <- c(
  rep("blank_neg", 3),
  rep("blank_pos", 3),
  rep("influent_neg", 3),
  rep("influent_pos", 3),
  rep("effluent_neg", 3),
  rep("effluent_pos", 3)
)

blks <- c(
  rep("blank_neg", 3),
  rep("blank_pos", 3),
  rep("blank_neg", 3),
  rep("blank_pos", 3),
  rep("blank_neg", 3),
  rep("blank_pos", 3)
)

ms$add_replicate_names(rpls)
ms$add_blank_names(blks)

# ms$run(MassSpecMethod_FindFeatures_xcms3_centwave())

ms$run(
  MassSpecMethod_FindFeatures_openms(
    noiseThrInt = 1000,
    chromSNR = 3,
    chromFWHM = 7,
    mzPPM = 15,
    reEstimateMTSD = TRUE,
    traceTermCriterion = "sample_rate",
    traceTermOutliers = 5,
    minSampleRate = 1,
    minTraceLength = 4,
    maxTraceLength = 70,
    widthFiltering = "fixed",
    minFWHM = 4,
    maxFWHM = 35,
    traceSNRFiltering = TRUE,
    localRTRange = 0,
    localMZRange = 0,
    isotopeFilteringModel = "none",
    MZScoring13C = FALSE,
    useSmoothedInts = FALSE,
    intSearchRTWindow = 3,
    useFFMIntensities = FALSE,
    verbose = FALSE
  )
)

ms$run(
  MassSpecMethod_AnnotateFeatures_StreamFind(
    rtWindowAlignment = 0.3,
    maxIsotopes = 8,
    maxCharge = 2,
    maxGaps = 1
  )
)

ms$run(
  MassSpecMethod_FilterFeatures_StreamFind(
    excludeIsotopes = TRUE,
    excludeAdducts = TRUE
  )
)

# ms$run(MassSpecMethod_GroupFeatures_xcms3_peakdensity()

ms$run(
  MassSpecMethod_GroupFeatures_openms(
    rtalign = FALSE,
    QT = FALSE,
    maxAlignRT = 5,
    maxAlignMZ = 0.008,
    maxGroupRT = 5,
    maxGroupMZ = 0.008,
    verbose = FALSE
  )
)

show(ms$NTS)
nts <- ms$NTS

# nts@number_analyses
# nts@number_features
# nts@has_features
# nts@number_filtered_features
# nts@has_filtered_features
# nts@has_groups
# nts@has_features_eic
# nts@has_features_ms1
# nts@has_features_ms2
# nts@has_features_suspects
# nts@number_groups
# nts@group_names

# plot_matrix_suppression(ms$Analyses)

# plot_features_count(nts)
# get_features(nts, mass = db[2:3, ])
# get_features_eic(nts, mass = db[2, ])
# plot_features(nts, mass = db[2, ])
# plot_features(nts, mass = db[2:3, ], legendNames = TRUE)
# get_features_ms1(nts, mass = db[2, ])
# plot_features_ms1(nts, mass = db[2:3, ], legendNames = TRUE)
# View(get_features_ms2(nts, mass = db[2:3, ]))
# plot_features_ms2(nts, mass = db[2:3, ], legendNames = TRUE)
# map_features(nts, mass = db[2:3, ])
# map_features_intensity(nts, mass = db[2:3, ])

get_groups(nts, mass = db[2:3, ], metadata = TRUE)
plot_groups(nts, mass = db[2:3, ])

plot_groups_overview(nts, mass = db[2:3, ])
plot_groups_profile(nts, mass = db[2:3, ])

get_groups_ms1(nts, mass = db[2:3, ])
plot_groups_ms1(nts, mass = db[2:3, ], legendNames = TRUE, interactive = T)

get_groups_ms2(nts, mass = db[2:3, ])
plot_groups_ms2(nts, mass = db[2:3, ], legendNames = TRUE, interactive = T)



# ms$clear_cache()




ms$run(
  MassSpecMethod_FilterFeatures_StreamFind(
    minIntensity = 3000
  )
)

ms$run(
  MassSpecMethod_FillFeatures_StreamFind(
    withinReplicate = FALSE,
    rtExpand = 2,
    mzExpand = 0.0005,
    minTracesIntensity = 1000,
    minNumberTraces = 6,
    baseCut = 0.3,
    minSignalToNoiseRatio = 3,
    minGaussianFit = 0.2
  )
)

ms$run(
  MassSpecMethod_CalculateFeaturesQuality_StreamFind(
    filtered = FALSE,
    rtExpand = 2,
    mzExpand = 0.0005,
    minTracesIntensity = 1000,
    minNumberTraces = 6,
    baseCut = 0
  )
)

ms$run(
  MassSpecMethod_FilterFeatures_StreamFind(
    minSnRatio = 5
  )
)

ms$run(
  MassSpecMethod_FilterFeatures_patRoon(
    maxReplicateIntRSD = 40,
    blankThreshold = 5,
    absMinReplicateAbundance = 3
  )
)

ms$run(
  MassSpecMethod_FindInternalStandards_StreamFind(
    database = db_is,
    ppm = 8,
    sec = 10
  )
)

ms$run(
  MassSpecMethod_CorrectMatrixSuppression_TiChri(
    mpRtWindow = 10,
    istdAssignment = "range",
    istdRtWindow = 50,
    istdN = 2
  )
)

ms$run(
  MassSpecMethod_LoadFeaturesMS1_StreamFind(
    filtered = FALSE
  )
)

ms$run(
  MassSpecMethod_LoadFeaturesMS2_StreamFind(
    filtered = FALSE
  )
)

ms$run(
  MassSpecMethod_LoadFeaturesEIC_StreamFind(
    filtered = FALSE
  )
)

ms$run(
  MassSpecMethod_SuspectScreening_StreamFind(
    database = db_with_ms2,
    ppm = 10,
    sec = 15,
    ppmMS2 = 10,
    minFragments = 3
  )
)

#StreamFind::clear_cache("all")

# Access NTS object and print to console
#show(ms$NTS)

# Access feature_list
#names(ms$NTS$feature_list)

#fts <- ms$NTS$feature_list

# Access properties
#NTS <- ms$NTS
#NTS@number_features
#NTS@analyses_info


#show(ms$Analyses)

#show(ms$Analyses$results$NTS)

#plot_features_count(ms$Analyses)

#map_features(ms$Analyses, analyses = 6:7)

#plot_features(ms$Analyses, analyses = 7, features = 1:5)

ms$save("ms.rds")

ms$run_app()



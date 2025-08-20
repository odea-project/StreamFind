devtools::load_all()
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

dt <- as.data.table(ms$Metadata)
as.Metadata(dt)

# ms$Analyses$has_results_nts
# ms$has_results_nts()
# ms$Analyses$NonTargetAnalysisResults

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

ms$Analyses <- set_replicate_names(ms$Analyses, rpls)
ms$Analyses <- set_blank_names(ms$Analyses, blks)

ms$run(MassSpecMethod_FindFeatures_xcms3_centwave())

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

ms$run(MassSpecMethod_GroupFeatures_xcms3_peakdensity())

ms$run(
  MassSpecMethod_FilterFeatures_StreamFind(
    minIntensity = 3000
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
  MassSpecMethod_FilterFeatures_StreamFind(
    maxDeviationInReplicate = 40,
    blankThreshold = 5,
    minAbundanceInReplicate = 3
  )
)

ms$run(
  MassSpecMethod_FindInternalStandards_StreamFind(
    database = db_is,
    ppm = 8,
    sec = 10
  )
)

ms$save("ms.rds")
ms$run_app()

ms$run(
  MassSpecMethod_CorrectMatrixSuppression_TiChri(
    mpRtWindow = 15,
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

get_features(ms$MassSpecResults_NonTargetAnalysis, analyses = 11, mass = db[2, ])
get_features_eic(ms$MassSpecResults_NonTargetAnalysis, analyses = 11, mass = db[2, ])
plot_features(ms$MassSpecResults_NonTargetAnalysis, analyses = 11, mass = db[2, ])

# show(ms$NonTargetAnalysisResults)
# Access NonTargetAnalysisResults object and print to console
# nts <- ms$NonTargetAnalysisResults
# show(nts)
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
# plot_features_count(nts, colorBy = "replicates") #imp
# View(get_features(nts, mass = db[2:3, ]))
# get_features_eic(nts, mass = db[2, ])
# plot_features(nts, features=data.frame(analysis="02_tof_ww_is_pos_influent-r001",feature="F43_MZ277_RT914"))
# plot_features(nts, mass = db[2:3, ], legendNames = TRUE)
# get_features_ms1(nts, mass = db[2, ])
# plot_features_ms1(nts, mass = db[2:3, ], legendNames = TRUE)
# View(get_features_ms2(nts, mass = db[2:3, ]))
# plot_features_ms2(nts, mass = db[2:3, ], legendNames = TRUE)
# map_features(nts, mass = db[2:3, ])
# map_features_intensity(nts, mass = db[2:3, ])
# get_groups(nts, metadata = FALSE, average = TRUE) #mass = db[2:3, ]
# plot_groups(nts, mass = db[2, ], colorBy = "replicates", interactive = FALSE)
# plot_groups_overview(nts, mass = db[2:3, ])
# plot_groups_profile(nts, mass = db[2:3, ])
# get_groups_ms1(nts, mass = db[2:3, ])
# plot_groups_ms1(nts, mass = db[2:3, ], legendNames = TRUE, interactive = T)
# get_groups_ms2(nts, mass = db[2:3, ])
# plot_groups_ms2(nts, mass = db[2:3, ], legendNames = TRUE, interactive = T)
# get_components(nts, mass = db[2:3, ])
# map_components(nts, analyses = 11, mass = db, legendNames = TRUE)
# get_internal_standards(nts, average = TRUE)
# plot_internal_standards(nts)
# View(get_suspects(nts))
# plot_suspects(nts)
# get_fold_change(
#   nts,
#   replicatesIn = "influent_pos",
#   replicatesOut = "effluent_pos"
# )
# plot_fold_change(
#   nts,
#   replicatesIn = "influent_pos",
#   replicatesOut = "effluent_pos",
#   constantThreshold = 0.5,
#   eliminationThreshold = 0.25,
#   correctIntensity = TRUE,
#   fillZerosWithLowerLimit = TRUE,
#   lowerLimit = NA_real_,
#   normalized = TRUE,
#   showLegend = TRUE
# )
# get_compounds(nts)
#StreamFind::clear_cache("all")
# Access feature_list
#names(ms$NTS$feature_list)
# modify feature_list
# ms$NTS$feature_list<-new_feature_list
#fts <- ms$NTS$feature_list

ms$save("ms.rds")
#options(shiny.launch.browser = FALSE)

# ms$get_cache_size()

# like this we only need to run the three lines below after making changes in the app files
devtools::load_all()
run_app(file = "ms.rds")


ms$run_app()




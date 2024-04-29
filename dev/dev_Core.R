
## Core development

# R6 class: CoreEngine
# core <- CoreEngine$new()
# core

# S3 classes: ProjectHeaders, ProcessingSettings, Analysis
# phead <- ProjectHeaders()
# phead

# settings <- ProcessingSettings()
# settings

# ana <- Analysis()
# ana

# r1 <- RamanEngine$new(files = StreamFindData::get_raman_file_paths())
# r1$get_number_analyses()
# r1$plot_spectra(colorBy = "replicates")
# r1$get_spectra()

ms_files <- StreamFindData::get_ms_file_paths()
ms_files <- ms_files[grepl("blank|influent|o3sw", ms_files)]
ms_files_df <- data.frame(
  "file" = ms_files,
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

db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
cols <- c("name", "formula", "mass", "rt", "polarity", "fragments", "tag")
db <- db[, cols, with = FALSE]
dbis <- db[grepl("IS", db$tag), ]
dbsus <- db[!grepl("IS", db$tag), ]

ps <- list(
  
  Settings_find_features_openms(),
  
  Settings_annotate_features_StreamFind(),

  Settings_group_features_openms(),

  Settings_find_internal_standards_StreamFind(database = dbis, ppm = 8, sec = 10),

  Settings_filter_features_StreamFind(excludeIsotopes = TRUE),

  Settings_filter_features_patRoon(absMinIntensity = 5000, maxReplicateIntRSD = 30, blankThreshold = 10, absMinReplicateAbundance = 3),

  Settings_load_features_eic_StreamFind(rtExpand = 60, mzExpand = 0.0005),

  Settings_calculate_quality_StreamFind(runParallel = FALSE),

  Settings_filter_features_StreamFind(minSnRatio = 5),

  Settings_load_features_ms1_StreamFind(runParallel = FALSE),

  Settings_load_features_ms2_StreamFind(runParallel = FALSE),

  Settings_load_MSPeakLists_StreamFind(), # Check patRoon function for issues with MSPeakLists!!!
  
  # Settings_generate_formulas_genform(),
  
  # Settings_generate_compounds_metfrag()

  # TODO add suspect screning results to featureGroups?
  Settings_suspect_screening_StreamFind(database = dbsus, ppm = 5, sec = 10)
)

ms <- MassSpecEngine$new(files = ms_files_df, settings = ps)

ms$plot_spectra_bpc(interactive = F)

names(ms)

ms$run_workflow()

comp <- patRoon::as.data.table(ms$compounds)

View(comp[comp$group %in% ms$get_groups(mass = dbsus)$group, ])

View(ms$get_results("patRoon"))

ms$get_isotopes(
  analyses = 5,
  mass = db[db$name %in% c("Diclofenac", "Candesartan"), ],
  ppm = 5, sec = 10
)


ms$map_isotopes(analyses = 5, mass = db[c(3, 24)], legendNames = TRUE, filtered = TRUE)


# MS2Tox and MS2Quant



# normalize internal standards
fGroupsNorm <- patRoon::normInts(ms$featureGroups, featNorm = "tic", groupNorm = TRUE)
fGroupsNorm <- patRoon::normInts(ms$featureGroups, featNorm = "istd", standards = dbis, adduct = "[M+H]+", 
                                 ISTDRTWindow = 20, ISTDMZWindow = 200, minISTDs = 2)














# grs <- ms$get_groups(mass = db, filtered = F, sec = 10)
# 
# ms$remove_groups(groups = grs$group)
# 
# fts <- ms$features
# 
# fts <- patRoon::filter(fts, absMinIntensity = 50000)
# 
# ms$features <- fts





# ms$get_features(mass = dbis, filtered = TRUE)

# ms$features@features

# ms$filtered_features

ms$get_groups(mass = db, filtered = F, sec = 10)

ms$get_suspects()

ms$plot_internal_standards_qc()

ms$plot_spectra_eic(analyses = 13:15, mass = dbis[5, ])






View(ms$get_isotopes(
  analyses = 4,
  mass = db,
  ppm = 8, sec = 10
))

ms$map_isotopes(
  analyses = 4,
  mass = db,
  ppm = 8, sec = 10,
  filtered = TRUE,
  legendNames = TRUE
)



ms$filtered_features 



# ms$analysisInfo
# 
# ms$features
# 
# ms$featureGroups



# new_files <- StreamFindData::get_ms_file_paths()[1:3]
# 
# new_anas <- parse_MassSpecAnalysis(new_files)
# 
# ms$add_analyses(new_anas)
# 
# ms$add_files(new_files)







ms$filtered_features

ms$get_features(mass = dbis, filtered = TRUE)

ms$plot_internal_standards_qc()

# ms$has_features()
# 
# ms$has_groups()
# 
length(ms$features)

View(ms$get_results("patRoon"))


# 
# ms$featureGroups()
# 
# ms$get_feature_list()
# 
# ms$get_features(mass = dbsus)
# 
# ms$get_groups(mass = dbsus, average = TRUE, metadata = TRUE)
# 
# ms$get_features_eic(analyses = 4, mass = dbsus[4, ])
# 
# ms$get_features_ms1(analyses = 1, mass = dbis[1, ])
# 
# ms$get_spectra_ms2(analyses = 4, mass = dbsus)
# 
# ms$get_features_ms2(analyses = 4, mass = dbsus)
# 
# ms$get_groups_ms1(mass = dbis[1, ])
# 
# ms$get_groups_ms2(mass = dbis[1, ])
# 
# ms$get_isotopes(analyses = 1, features = ms$get_features(analyses = 1, mass = dbis[3, ]))
# 
# ms$get_suspects(analyses = 4, database = dbsus)
# 
ms$get_suspects(onGroups = TRUE)

ms$analysisInfo

ms$features

ms$featureGroups







ms$plot_spectra_bpc(colorBy = "replicates", levels = 1)



# patRoon::clearCache("all")
# patRoon::clearCache(c("annotate_features"))
# patRoon::clearCache(c("calculate_quality"))
# patRoon::clearCache(c("load_features_ms2"))
# patRoon::clearCache(c("load_features_ms1"))

ms$add_settings(ps)

ms$run_workflow()

ms$plot_suspects()

ms$plot_groups_overview(analyses = c(4:9, 13:18), groups = ms$get_suspects(), legendNames = TRUE, heights = c(0.25, 0.5, 0.25))














patRoon::clearCache("all")
msana <- parse_MassSpecAnalysis(StreamFindData::get_ms_file_paths()[1])[[1]]
class(msana)
sloop::s3_dispatch(print(msana))
ana


core <- CoreEngine$new()
core$add_settings(Settings_annotate_features_StreamFind())
core$add_analyses(Analysis())
core$get_analyses()










core$get_history()


uv <- UVEngine$new()
uv

r1 <- RamanEngine$new(files = StreamFindData::get_raman_file_paths())

r1

r1$add_replicate_names(c(rep("Sample", 11), rep("Blank", 11)))

r1$add_blank_names(rep("Blank", 22))

r1$get_spectra(analyses = 1)

r1$plot_spectra(colorBy = "replicates", interactive = TRUE)

# plot(r1$get_spectra(analyses = 1)[, 2:3], type = "l")
# 
# plot(r1$get_spectra(2)[1:200, 2:3])
# 
# View(r1$get_analyses())

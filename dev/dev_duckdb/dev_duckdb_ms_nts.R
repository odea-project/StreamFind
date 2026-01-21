db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
db <- db[, c("name", "formula", "mass", "rt", "fragments", "tag"), with = FALSE]
dbis <- db[grepl("IS", db$tag), ]
dbsus <- db[!grepl("IS", db$tag), ]

ms_files <- StreamFindData::get_ms_file_paths()
ms_files <- ms_files[grepl("ww_", ms_files)]
#ms_files <- ms_files[grepl("pos_", ms_files)]

root <- file.path("dev", "dev_duckdb", "data_nts")
# file.remove(list.files(root, full.names = TRUE))
# fs::dir_delete(root)

ms <- DB_MassSpecEngine$new(projectPath = root, files = ms_files)

set_replicate_names(ms$Analyses, c(
  rep("neg_blank", 3),
  rep("pos_blank", 3),
  rep("neg_influent", 3),
  rep("pos_influent", 3),
  rep("neg_effluent", 3),
  rep("pos_effluent", 3)
))

set_blank_names(ms$Analyses, c(
  rep("neg_blank", 3),
  rep("pos_blank", 3),
  rep("neg_blank", 3),
  rep("pos_blank", 3),
  rep("neg_blank", 3),
  rep("pos_blank", 3)
))

ps_ff <- DB_MassSpecMethod_FindFeatures_native(
  rtWindows = data.frame(rtmin = numeric(), rtmax = numeric()),
  ppmThreshold = 10,
  noiseThreshold = 250,
  minSNR = 3,
  minTraces = 3,
  baselineWindow = 200,
  maxWidth = 250,
  baseQuantile = 0.99,
  debugAnalysis = "",
  debugMZ = 0,
  debugSpecIdx = -1
)

ps_comp <- DB_MassSpecMethod_CreateComponents_native(
  rtWindow = c(-5, 5),
  minCorrelation = 0.9,
  debugRT = 968,
  debugAnalysis = "	02_tof_ww_is_neg_influent-r003"
)

ps_annot <- DB_MassSpecMethod_AnnotateComponents_native(
  maxIsotopes = 8,
  maxCharge = 1,
  maxGaps = 1,
  ppm = 10,
  debugComponent = "",
  debugAnalysis = ""
)

pf_istd <- DB_MassSpecMethod_FindInternalStandard_native(
  suspects = dbis,
  ppm = 10,
  sec = 15,
  ppmMS2 = 10,
  mzrMS2 = 0.008,
  minCosineSimilarity = 0.7,
  minSharedFragments = 3,
  filtered = TRUE
)

ps_gf <- DB_MassSpecMethod_GroupFeatures_native(
  method = "internal_standards", #"obi_warp"
  rtDeviation = 5,
  ppm = 10,
  minSamples = 1,
  binSize = 5,
  filtered = FALSE,
  debug = FALSE,
  debugRT = 0
)

ps_bsub <- DB_MassSpecMethod_FeatureBlankSubtraction_native(
  blankThreshold = 5,
  rtExpand = 10,
  mzExpand = 0.005
)

ps_filterf1 <- DB_MassSpecMethod_FilterFeatures_native(
  removeIsotopes = TRUE,
  removeAdducts = TRUE,
  removeLosses = TRUE
)

ps_fillf <- DB_MassSpecMethod_FillFeatures_native(
  withinReplicate = TRUE,
  filtered = FALSE,
  rtExpand = 5,
  mzExpand = 0.0005,
  maxPeakWidth = 250,
  minTracesIntensity = 250,
  minNumberTraces = 4,
  minIntensity = 1000,
  rtApexDeviation = 5,
  minSignalToNoiseRatio = 3,
  minGaussianFit = 0.8,
  debugFG = ""
)


# ps_ms1 <- DB_MassSpecMethod_LoadFeaturesMS1_native(
#   rtWindow = c(-1, 1),
#   mzWindow = c(-1, 6),
#   mzClust = 0.008,
#   presence = 0.8,
#   minIntensity = 50,
#   filtered = FALSE
# )

ps_ms2 <- DB_MassSpecMethod_LoadFeaturesMS2_native(
  isolationWindow = 1.3,
  mzClust = 0.008,
  presence = 0.8,
  minIntensity = 10,
  filtered = FALSE
)

ms$Workflow <- list(ps_ff, ps_comp) #, ps_annot, pf_istd, ps_gf, ps_bsub, ps_filterf1, ps_fillf
# clear_cache(ms$Cache, value = c("DB_FindFeatures_native"))
clear_cache(ms$Cache, value = c("DB_CreateComponents_native"))
clear_cache(ms$Cache, value = c("DB_AnnotateComponents_native"))
clear_cache(ms$Cache, value = c("DB_FindInternalStandard_native"))
clear_cache(ms$Cache, value = c("DB_GroupFeatures_native"))
clear_cache(ms$Cache, value = c("DB_FeatureBlankSubtraction_native"))
clear_cache(ms$Cache, value = c("DB_FilterFeatures_native"))
clear_cache(ms$Cache, value = c("DB_FillFeatures_native"))
ms$run_workflow()


ms$run_app()

root <- file.path("dev", "dev_duckdb", "data_nts")
ms <- DB_MassSpecEngine$new(projectPath = root)
ms$run_app()





















.plot_debug_DB_MassSpecMethod_GroupFeatures_native(
  "C:\\Users\\cunha\\Documents\\GitHub\\StreamFind\\log\\debug_log_group_features_methodobi_warp_rt915.00.log"
)

.plot_debug_DB_MassSpecMethod_GroupFeatures_native(
  "C:\\Users\\cunha\\Documents\\GitHub\\StreamFind\\log\\debug_log_group_features_methodinternal_standards_rt1157.00.log"
)

plot_debug_log(
  ps_ff,
  logFile = file.path("C:/Users/cunha/Documents/GitHub/StreamFind/log/debug_log_peak_detection_304.189301.log")
)

#DEBUG
#"03_tof_ww_is_pos_o3sw_effluent-r002"
#304.1893

#"03_tof_ww_is_pos_o3sw_effluent-r002"
#356.2657

#"02_tof_ww_is_pos_influent-r003"
#342.2639

# DEBUG ANNOTATION
#"02_tof_ww_is_neg_influent-r002", "FG761_M286_RT968_NEG", "FC60_RT968_NEG", mz 285.0802
#"02_tof_ww_is_neg_influent-r001", "FG761_M286_RT968_NEG", "FC65_RT967_NEG", mz 285.0806

get_suspects(ms$NonTargetAnalysis, suspects = dbis, ppm = 10, sec = 15)
get_features(ms$NonTargetAnalysis, mass = dbsus[2, ], ppm = 10, sec = 15)[, 1:20]

istd_dt <- get_internal_standards(ms$NonTargetAnalysis)
istd_avg_rt <- istd_dt[, .(avg_exp_rt = mean(exp_rt, na.rm = TRUE)), by = "name"]
istd_dt_with_shift <- istd_dt[istd_avg_rt, on = "name", `:=`(rt_shift = exp_rt - i.avg_exp_rt)]




#get_cache_info(ms$Cache)



View(get_suspects(ms$NonTargetAnalysis, suspects = dbsus, ppm = 10, sec = 15))




find_features_debug <- function(
  projectPath,
  file,
  rtWindows = data.frame(rtmin = numeric(), rtmax = numeric()),
  ppmThreshold = 10,
  noiseThreshold = 250,
  minSNR = 3,
  minTraces = 3,
  baselineWindow = 200,
  maxWidth = 250,
  baseQuantile = 0.99,
  debugMZ = 0,
  debugSpecIdx = -1
) {
  ms <- DB_MassSpecEngine$new(
    projectPath = projectPath,
    files = file
  )
  ps_ff <- DB_MassSpecMethod_FindFeatures_native(
    rtWindows = rtWindows,
    ppmThreshold = ppmThreshold,
    noiseThreshold = noiseThreshold,
    minSNR = minSNR,
    minTraces = minTraces,
    baselineWindow = baselineWindow,
    maxWidth = maxWidth,
    baseQuantile = baseQuantile,
    debugMZ = debugMZ,
    debugSpecIdx = debugSpecIdx
  )
  ms$Workflow <- list(ps_ff)
  ms$run_workflow()
  ms
}

ms <- find_features_debug(
  projectPath = root,
  file = ms_files[6],
  rtWindows = data.frame(rtmin = numeric(), rtmax = numeric()),
  ppmThreshold = 10,
  noiseThreshold = 250,
  minSNR = 3,
  minTraces = 3,
  baselineWindow = 200,
  maxWidth = 250,
  baseQuantile = 0.99,
  debugMZ = 0,
  debugSpecIdx = -1
)
ms$run_app()


create_components_debug <- function(
  projectPath,
  files,
  rtWindow = c(-2, 2),
  minCorrelation = 0.8,
  debugRT = 0.0,
  debugAnalysis = ""
) {
  ms <- DB_MassSpecEngine$new(
    projectPath = projectPath,
    files = files
  )
  ps_ff <- DB_MassSpecMethod_FindFeatures_native(
    rtWindows = data.frame(rtmin = numeric(), rtmax = numeric()),
    ppmThreshold = 10,
    noiseThreshold = 250,
    minSNR = 3,
    minTraces = 3,
    baselineWindow = 200,
    maxWidth = 250,
    baseQuantile = 0.99,
    debugMZ = 0,
    debugSpecIdx = -1
  )
  ps_comp <- DB_MassSpecMethod_CreateComponents_native(
    rtWindow = rtWindow,
    minCorrelation = minCorrelation,
    debugRT = debugRT,
    debugAnalysis = debugAnalysis
  )
  ms$Workflow <- list(ps_comp)
  ms$run_workflow()
}

create_components_debug(
  projectPath = root,
  files = ms_files,
  rtWindow = c(-2, 2),
  minCorrelation = 0.8,
  debugRT = 1108,
  debugAnalysis = ""
)






source("dev\\merck_peak_finding\\dev_log_plot.R")
plot_cluster_data(log_file = "log\\debug_log_peak_detection_247.176300.log")

hd <- get_spectra_headers(ms$Analyses)
hd[hd$rt >= 1119 & hd$rt <= 1120, ]

ms$clear_cache()

show(ms$NonTargetAnalysis)

fts <- get_features(
  ms$NonTargetAnalysis,
  analyses = 5,
  mass = dbsus,
  ppm = 20,
  sec = 30,
  filtered = FALSE
)[, 1:30]


get_features(
  ms$NonTargetAnalysis,
  analyses = 5,
  components = "FC_934",
  filtered = FALSE
)[, 1:30]

fts <- get_features(
  ms$NonTargetAnalysis,
  analyses = 5,
  filtered = FALSE
)
fts <- fts[fts$feature_component %in% "FC_1082", ]
plot_features(
  ms$NonTargetAnalysis,
  features = fts
)


plot_features(
  ms$NonTargetAnalysis,
  analyses = 5,
  components = "FC_1083",
  showDetails = FALSE
)

map_features(
  ms$NonTargetAnalysis,
  analyses = 5,
  components = "FC_1083",
  showDetails = TRUE
)

colnames(fts)

fts[, c("feature", "intensity", "ms1_size")]
fts[, c("feature", "intensity", "ms2_size")]

plot_features(
  ms$NonTargetAnalysis,
  analyses = 5,
  mass = dbsus,
  ppm = 20,
  sec = 30,
  filtered = FALSE,
  groupBy = c("name", "replicate"),
  showDetails = TRUE
)


colnames(fts)




plot_features_ms1(
  ms$NonTargetAnalysis,
  analyses = 5,
  mass = dbsus,
  ppm = 20,
  sec = 30,
  groupBy = c("name", "replicate")
)

plot_features_ms2(
  ms$NonTargetAnalysis,
  analyses = 5,
  mass = dbsus,
  ppm = 20,
  sec = 30,
  groupBy = c("name", "replicate")
)


# ms_files <- StreamFindData::get_ms_file_paths()[1:3]
# ms_db_path <- file.path(sf_root, "MassSpecAnalyses.duckdb")
# ms_db_obj <- MassSpecAnalysesDB(db = ms_db_path, files = ms_files)
# get_analysis_names(ms_db_obj)
# get_replicate_names(ms_db_obj)
# set_replicate_names(ms_db_obj, rep("Sample", 3))
# info(ms_db_obj)
# get_chromatograms_headers(ms_db_obj, get_analysis_names(ms_db_obj)[1])
# get_spectra_headers(ms_db_obj, get_analysis_names(ms_db_obj)[1])
# list_db_tables(ms_db_obj)
# get_db_table_info(ms_db_obj, "ChromatogramsHeaders")
# get_db_table_info(ms_db_obj, "SpectraHeaders")

# MARK: EngineDB with MassSpec tests
# EngineDB with MassSpec tests -----

sf_root <- file.path("dev", "dev_duckdb", "data")
#file.remove(list.files(sf_root, full.names = TRUE))
#ms_files <- StreamFindData::get_ms_file_paths()[1]
source(file.path("C:\\Users\\apoli\\Documents\\github\\StreamFind\\dev\\merck_peak_finding\\dev_resources.R"))
examples <- unique(files_merck_ex$example)
files_merck_ex[, 1:3]


ms <- DB_MassSpecEngine$new(
  projectPath = sf_root,
  files = files_merck_ex$file_path[1:2]
)

get_cache_info(ms$Cache)

ms$Metadata[["project"]] <- "ms-demo"

set_replicate_names(ms$Analyses, c("sample", "blank"))
set_blank_names(ms$Analyses, c("blank", "blank"))

# ms
# ms$clear_result_databases()

# ms$info_analyses()
# ms$list_db_tables()
# ms$get_db_table_info("SpectraHeaders")

# head(get_spectra_headers(ms$Analyses))
# head(get_spectra_bpc(ms$Analyses))
# head(get_spectra_tic(ms$Analyses))
# plot_spectra_tic(ms$Analyses, levels = 1, downsize = 2)
# plot_spectra_bpc(ms$Analyses, levels = 1, downsize = 2)

# get_raw_spectra(ms$Analyses, levels = 1, mass = dbis[7, ], ppm = 20, sec = 30)
# get_spectra_eic(ms$Analyses, mass = dbis[7, ], ppm = 20)
# get_spectra_ms1(ms$Analyses, mass = dbis[7, ], ppm = 20)
# get_spectra_ms2(ms$Analyses, mass = dbis[7, ], ppm = 20)

ps_ff <- DB_MassSpecMethod_FindFeatures_native(
  # rtWindows = data.table::data.table(rtmin = 300, rtmax = 3600),
  # resolution_profile = c(35000L, 35000L, 35000L),
  # noiseThreshold = 250,
  # minSNR = 3,
  # minTraces = 3,
  # baselineWindow = 200,
  # maxWidth = 100,
  # base_quantile = 0.1,
  # debug_mz = dbis$mass[1] + 1.007276
  rtWindows = data.frame(rtmin = 300, rtmax = 3400),
  resolution_profile = c(25000L, 55000L, 80000L),
  noiseThreshold = 15,
  minSNR = 3,
  minTraces = 3,
  baselineWindow = 200,
  maxWidth = 250, #100
  base_quantile = 0.99,
  debug_mz = 0
)

wf <- Workflow(list(ps_ff))
ms$Workflow <- wf
ms$run_workflow()


show(ms$NonTargetAnalysis)



run.DB_MassSpecMethod_FeatureBlankSubtraction_native(
  DB_MassSpecMethod_FeatureBlankSubtraction_native(
    blankThreshold = 10
  ),
  engine = ms
)

get_features(
  ms$NonTargetAnalysis,
  analyses = 2
)

# run(ps_ff, engine = ms)

# size(ms$Cache)
# get_cache_info(ms$Cache)
# show(ms$Workflow)
# show(ms$NonTargetAnalysis)

get_features(
  ms$NonTargetAnalysis,
  analyses = 1,
  mass = dbis, ppm = 20
)

plot_features(
  ms$NonTargetAnalysis,
  analyses = 1,
  mass = dbis,
  ppm = 20,
  legendNames = TRUE
)

plot_features_ms2(
  ms$NonTargetAnalysis,
  analyses = 1,
  mass = dbis,
  ppm = 20,
  legendNames = TRUE
)

# sf_root <- file.path("dev", "dev_duckdb", "demo.sf")
# nts_db_path <- file.path(sf_root, "MassSpecResults_NonTargetAnalysis.duckdb")
# nts <- DB_MassSpecResults_NonTargetAnalysis(db = nts_db_path)
# show(nts)
# list_db_tables(nts)


# MARK: MS Chromatograms
# MS Chromatograms -----

#file.remove(list.files(sf_root, full.names = TRUE))

# sf_root <- file.path("dev", "dev_duckdb", "demo.sf")
# ms_files <- StreamFindData::get_ms_file_paths()[29:30]
# ms <- DB_MassSpecEngine$new(
#   project_dir = sf_root,
#   files = ms_files
# )
# ms$Metadata[["project"]] <- "chromatograms-demo"
# ms

#head(ms$get_chromatograms_headers(analyses = 1))
#get_chromatograms(ms$Analyses, chromatograms = c(0, 1))
#plot_chromatograms(ms$Analyses, chromatograms = c(0, 1), interactive = FALSE)

# plot_chromatograms(
#   ms$Analyses,
#   #analyses = 2,
#   chromatograms = c(0, 1),
#   groupBy = c("analysis", "id"),
#   downsize = 3
# )






# MARK: Custom ProcessingSteps
# Custom ProcessingSteps -----

# .get_available_engines()
# length(.get_available_processing_methods(dataType = "MassSpec"))
# length(.get_available_methods(dataType = "MassSpec"))
# .list_processing_steps_metadata(dataType = "MassSpec")[, 1:4]

# # Dummy ProcessingStep S3 child for testing
# MassSpecMethod_TestStep <- function() {
#   x <- ProcessingStep(
#     type = "MassSpec",
#     method = "FindFeatures",
#     required = NA_character_,
#     algorithm = "TestStep",
#     input_class = NA_character_,
#     output_class = "DummyOutput",
#     parameters = list(dummy = TRUE),
#     number_permitted = 1,
#     version = "0.0.1",
#     software = "DummySoftware",
#     developer = "Test Developer",
#     contact = "test@example.com",
#     link = NA_character_,
#     doi = NA_character_
#   )
#   if (is.null(validate_object(x))) {
#     x
#   } else {
#     stop("Invalid parameters for MassSpecMethod_TestStep.")
#   }
# }

# validate_object.MassSpecMethod_TestStep <- function(x) {
#   # Dummy validation: always valid
#   TRUE
# }

# run.MassSpecMethod_TestStep <- function(x, engine = NULL) {
#   # Dummy run: returns a message
#   list(result = "Dummy run executed", engine = engine)
# }

# dummy_ps <- MassSpecMethod_TestStep()
# run.MassSpecMethod_TestStep(dummy_ps)

# .list_processing_steps_metadata(dataType = "MassSpec")[, 1:4]

# sf_root <- file.path("dev", "dev_duckdb", "demo.sf")
# nts_db_path <- file.path(sf_root, "MassSpecResults_NonTargetAnalysis.duckdb")
# nts <- DB_MassSpecResults_NonTargetAnalysis(db = nts_db_path)
# show(nts)

# list_db_tables(nts)




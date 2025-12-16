db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
db <- db[, c("name", "formula", "mass", "rt", "fragments", "tag"), with = FALSE]
dbis <- db[grepl("IS", db$tag), ]
dbsus <- db[!grepl("IS", db$tag), ]

# MARK: MassSpecAnalysesDB tests
# MassSpecAnalysesDB tests -----

# sf_root <- file.path("dev", "dev_duckdb", "demo.sf")
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

sf_root <- file.path("dev", "dev_duckdb", "demo.sf")
#ms_files <- StreamFindData::get_ms_file_paths()[1]
source(file.path("C:\\Users\\apoli\\Documents\\github\\StreamFind\\dev\\merck_peak_finding\\dev_resources.R"))
examples <- unique(files_merck_ex$example)
files_merck_ex[, 1:3]


ms <- DB_MassSpecEngine$new(
  project_path = sf_root,
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
# length(.get_available_processing_methods(data_type = "MassSpec"))
# length(.get_available_methods(data_type = "MassSpec"))
# .list_processing_steps_metadata(data_type = "MassSpec")[, 1:4]

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

# .list_processing_steps_metadata(data_type = "MassSpec")[, 1:4]

# sf_root <- file.path("dev", "dev_duckdb", "demo.sf")
# nts_db_path <- file.path(sf_root, "MassSpecResults_NonTargetAnalysis.duckdb")
# nts <- DB_MassSpecResults_NonTargetAnalysis(db = nts_db_path)
# show(nts)

# list_db_tables(nts)




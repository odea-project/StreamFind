db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
cols <- c("name", "formula", "mass", "rt", "fragments", "tag")
db <- db[, cols, with = FALSE]
dbis <- db[grepl("IS", db$tag), ]
dbsus <- db[!grepl("IS", db$tag), ]

# Load package if needed; adjust as appropriate for your setup
# library(StreamFind)


# Path to the StreamFind data folder (.sf). EngineDB will create/use main.duckdb inside it.
# sf_root <- file.path("dev", "dev_duckdb", "demo.sf")
# main_db <- file.path(sf_root, "main.duckdb")

# MARK: EngineDB tests
# engine <- EngineDB$new(project_dir = sf_root, data_type = "Unknown")
# engine$set_metadata(list(project = "dev-demo", user = Sys.info()["user"]))
# engine$set_configuration(list(dummy_param = 1L))
# engine$set_workflow(list(
#   data_type = list(kind = "demo"),
#   methods = list(step = "noop")
# ))
# engine$set_workflow(list(MassSpecMethod_FindFeatures_native()))
# engine$get_engine_info()
# engine$list_db_tables()
# engine$get_audit_trail()
# engine$Metadata
# engine$Workflow

# MARK: MassSpecAnalysesDB tests

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
sf_root <- file.path("dev", "dev_duckdb", "demo.sf")
ms_files <- StreamFindData::get_ms_file_paths()[1:3]

engine_ms <- MassSpecEngineDB$new(
  project_dir = sf_root,
  files = ms_files
)

engine_ms$Metadata[["project"]] <- "ms-demo"


# engine_ms$info_analyses()
# engine_ms$list_db_tables()
# engine_ms$get_db_table_info("SpectraHeaders")

# head(get_spectra_headers(engine_ms$Analyses))
# head(get_spectra_bpc(engine_ms$Analyses))
# head(get_spectra_tic(engine_ms$Analyses))
# plot_spectra_tic(engine_ms$Analyses, levels = 1, downsize = 2)
# plot_spectra_bpc(engine_ms$Analyses, levels = 1, downsize = 2)

# get_raw_spectra(engine_ms$Analyses, levels = 1, mass = dbis[7, ], ppm = 20, sec = 30)
# get_spectra_eic(engine_ms$Analyses, mass = dbis[7, ], ppm = 20)
# get_spectra_ms1(engine_ms$Analyses, mass = dbis[7, ], ppm = 20)
# get_spectra_ms2(engine_ms$Analyses, mass = dbis[7, ], ppm = 20)

ps_ff <- MassSpecMethod_FindFeaturesDB_native()
wf <- Workflow(list(ps_ff))
show(wf)

engine_ms$Workflow <- wf

engine_ms$get_audit_trail()

show(engine_ms$Workflow)

run(ps_ff, engine = engine_ms)

sf_root <- file.path("dev", "dev_duckdb", "demo.sf")
nts_db_path <- file.path(sf_root, "MassSpecResults_NonTargetAnalysis.duckdb")
nts <- MassSpecResults_NonTargetAnalysisDB(db = nts_db_path)
show(nts)

list_db_tables(nts)








.get_available_engines()
length(.get_available_processing_methods(data_type = "MassSpec"))
length(.get_available_methods(data_type = "MassSpec"))
.list_processing_steps_metadata(data_type = "MassSpec")[, 1:4]

# Dummy ProcessingStep S3 child for testing
MassSpecMethod_TestStep <- function() {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "FindFeatures",
    required = NA_character_,
    algorithm = "TestStep",
    input_class = NA_character_,
    output_class = "DummyOutput",
    parameters = list(dummy = TRUE),
    number_permitted = 1,
    version = "0.0.1",
    software = "DummySoftware",
    developer = "Test Developer",
    contact = "test@example.com",
    link = NA_character_,
    doi = NA_character_
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid parameters for MassSpecMethod_TestStep.")
  }
}

validate_object.MassSpecMethod_TestStep <- function(x) {
  # Dummy validation: always valid
  TRUE
}

run.MassSpecMethod_TestStep <- function(x, engine = NULL) {
  # Dummy run: returns a message
  list(result = "Dummy run executed", engine = engine)
}

dummy_ps <- MassSpecMethod_TestStep()
run.MassSpecMethod_TestStep(dummy_ps)

.list_processing_steps_metadata(data_type = "MassSpec")[, 1:4]

sf_root <- file.path("dev", "dev_duckdb", "demo.sf")
nts_db_path <- file.path(sf_root, "MassSpecResults_NonTargetAnalysis.duckdb")
nts <- MassSpecResults_NonTargetAnalysisDB(db = nts_db_path)
show(nts)

list_db_tables(nts)

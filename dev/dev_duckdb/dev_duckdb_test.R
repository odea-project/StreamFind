db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
db <- db[, c("name", "formula", "mass", "rt", "fragments", "tag"), with = FALSE]
dbis <- db[grepl("IS", db$tag), ]
dbsus <- db[!grepl("IS", db$tag), ]

# MARK: DataTypes
# DataTypes -----

# TODO export object and make documentation

# DataTypeObjects()
# DataTypeObjects("DB_MassSpec", showProcessingMethods = TRUE)
# .list_processing_steps_metadata(dataType = "DB_MassSpec")
# .list_processing_steps_metadata(dataType = "MassSpec")[, 1:4]

# MARK: EngineDB tests
# EngineDB tests -----

# Path to the StreamFind data folder (.sf). EngineDB will create/use main.duckdb inside it.
sf_root <- file.path("dev", "dev_duckdb", "data")
file.remove(list.files(sf_root, full.names = TRUE))
fs::dir_delete(sf_root)
!dir.exists(sf_root)

engine <- DB_Engine$new(projectPath = sf_root, dataType = "Unknown")
engine$add_metadata(list(project = "dev-demo", user = Sys.info()["user"]))
engine$add_workflow(list(MassSpecMethod_FindFeatures_native()))
engine$get_engine_info()
engine$list_db_tables()
engine$get_audit_trail()
engine$Metadata
engine$Workflow

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




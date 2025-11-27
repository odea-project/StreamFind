db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
cols <- c("name", "formula", "mass", "rt", "fragments", "tag")
db <- db[, cols, with = FALSE]
dbis <- db[grepl("IS", db$tag), ]
dbsus <- db[!grepl("IS", db$tag), ]

# Load package if needed; adjust as appropriate for your setup
# library(StreamFind)


# Path to the StreamFind data folder (.sf). EngineDB will create/use main.duckdb inside it.
sf_root <- file.path("dev", "dev_duckdb", "demo.sf")
main_db <- file.path(sf_root, "main.duckdb")

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
  analyses = ms_files
)

engine_ms$info_analyses()
engine_ms$list_db_tables()
engine_ms$get_db_table_info("SpectraHeaders")

head(get_spectra_headers(engine_ms$Analyses))

head(get_spectra_bpc(engine_ms$Analyses))

head(get_spectra_tic(engine_ms$Analyses))
plot_spectra_tic(engine_ms$Analyses, levels = 1, downsize = 2)

plot_spectra_bpc(engine_ms$Analyses, levels = 1, downsize = 2)


get_raw_spectra(engine_ms$Analyses, levels = 1, mass = dbis[7, ], ppm = 20, sec = 30)


# MARK: MS Chromatograms
# MS Chromatograms -----

db <- file.path("dev", "dev_duckdb", "data", "test.db")
proj <- Project$new(db = db, project_id = "chromatograms-demo")
proj$project_id
proj$set_metadata(
  list(
    name = "Chromatograms Demo",
    description = "A demo project for MS chromatograms"
  )
)
proj$metadata

proj$workflow


load_chroms <- MassSpecMethod_LoadChromatograms_native(
  chromatograms = c(0, 1)
)



engine <- Engine$new(db = db)
engine$Metadata
engine$AuditTrail

duckdb_list_tables(engine$db)
duckdb_list_tables(normalizePath("dev/dev_duckdb/data/test.db"))
duckdb_json_extension_info(normalizePath("dev/dev_duckdb/data/test.db"))

rcpp_json_make_example()
rcpp_json_parse(rcpp_json_make_example())

list_db_tables(engine)
get_cache_info(engine$Cache)
size(engine$Cache)



#file.remove(list.files(sf_root, full.names = TRUE))
ms_files <- StreamFindData::get_ms_file_paths()[c(29:30)]
ms <- MassSpecEngine$new(
  db = db, files = ms_files,
)
ms$Metadata[["project"]] <- "chromatograms-demo"

ms

head(get_chromatograms_headers(ms$Analyses, analyses = 1))
# get_chromatograms(ms$Analyses, chromatograms = c(0, 1))
# plot_chromatograms(ms$Analyses, chromatograms = c(0, 1), interactive = FALSE)

# plot_chromatograms(
#   ms$Analyses,
#   chromatograms = c(0, 1),
#   groupBy = c("analysis", "id"),
#   downsize = 3
# )

load_chroms <- MassSpecMethod_LoadChromatograms_native(
  chromatograms = c(0, 1)
)

ms$Workflow <- Workflow(list(load_chroms))

ms$run_workflow()

show(ms$Chromatograms)


ms <- MassSpecEngine$new(projectPath = sf_root)
ms$run_app()

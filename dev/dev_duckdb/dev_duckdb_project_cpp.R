# MARK: Project Mass Spec shared DuckDB test

dbsus <- data.table::fread(
  file.path(
    "dev",
    "dev_duckdb",
    "suspects_with_ms2_template.csv"
  )
)[, c("name", "mass")]

library(StreamFind)

db <- file.path("dev", "dev_duckdb", "data", "ms_project_test.duckdb")
if (file.exists(db)) file.remove(db)

proj <- ProjectMassSpec$new(
  db = db,
  project_id = "ms-demo"
)

proj$project_id
proj$get_domain()
proj$set_metadata(
  list(
    name = "Mass Spec shared DB demo",
    description = "Shared DuckDB project with MS domain tables"
  )
)
proj$get_metadata()
proj$list_tables()
# proj$list_analyses()
# proj$get_spectra_headers()
# proj$get_chromatograms_headers()

# Optional import test.
# Replace with real mzML/mzXML files before running this block.
ms_files <- c(
  "E:\\example_files\\ms_basic\\00_hrms_mix1_pos_cent-r001.mzML",
  "E:\\example_files\\ms_basic\\00_hrms_mix1_pos_cent-r002.mzML",
  "E:\\example_files\\ms_basic\\00_hrms_mix1_pos_cent-r003.mzML"
)

proj$import_files(ms_files)

proj$list_tables()
proj$list_analyses()



head(proj$get_spectra_headers())
head(proj$get_chromatograms_headers())


proj$get_spectra_eic(
  mass = dbsus$mass[2],
)


plot_spectra_bpc(proj)



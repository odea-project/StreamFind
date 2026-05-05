
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










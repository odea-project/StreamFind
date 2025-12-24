
# MARK: MS Chromatograms
# MS Chromatograms -----

sf_root <- file.path("dev", "dev_duckdb", "data")
#file.remove(list.files(sf_root, full.names = TRUE))
ms_files <- StreamFindData::get_ms_file_paths()[29:30]
ms <- DB_MassSpecEngine$new(
  project_path = sf_root
)
ms$Metadata[["project"]] <- "chromatograms-demo"
ms
ms$run_app()

remove_analyses(ms$Analyses, 2)



head(get_chromatograms_headers(ms$Analyses, analyses = 1))
get_chromatograms(ms$Analyses, chromatograms = c(0, 1))
plot_chromatograms(ms$Analyses, chromatograms = c(0, 1), interactive = FALSE)

plot_chromatograms(
  ms$Analyses,
  #analyses = 2,
  chromatograms = c(0, 1),
  groupBy = c("analysis", "id"),
  downsize = 3
)





# MARK: MS Chromatograms
# MS Chromatograms -----

sf_root <- file.path("dev", "dev_duckdb", "data")
#file.remove(list.files(sf_root, full.names = TRUE))
ms_files <- StreamFindData::get_ms_file_paths()[c(29:30)]

ms <- MassSpecEngine$new(
  projectPath = sf_root,
  files = ms_files,
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

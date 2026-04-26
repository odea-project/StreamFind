
# MARK: MS Chromatograms
# MS Chromatograms -----

sf_root <- file.path("dev", "dev_duckdb", "data")
#file.remove(list.files(sf_root, full.names = TRUE))

path <- "E:/example_ms_files/isoft/240819_BVCZ"
ms_files <- list.files(path = path, pattern = "\\.mzML$", full.names = TRUE)[-15]

ms <- MassSpecEngine$new(
  projectPath = sf_root,
  files = ms_files,
)

ms$Metadata[["project"]] <- "chromatograms-dad-demo"


ms

ms$run_app()

ms$clear_cache()

#add_analyses(ms$Analyses, file.choose())

ms$get_audit_trail()
remove_analyses(ms$Analyses, 2)


info(ms$Analyses)

get_spectra_headers(ms$Analyses)

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




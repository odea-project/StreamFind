db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
db <- db[, c("name", "formula", "mass", "rt", "fragments", "tag"), with = FALSE]
dbis <- db[grepl("IS", db$tag), ]
dbsus <- db[!grepl("IS", db$tag), ]

# MARK: MS Chromatograms
# MS Chromatograms -----

#file.remove(list.files(sf_root, full.names = TRUE))

sf_root <- file.path("dev", "dev_duckdb", "demo.sf")
ms_files <- StreamFindData::get_ms_file_paths()[29:30]
ms <- DB_MassSpecEngine$new(
  project_path = sf_root,
  files = ms_files
)
ms$Metadata[["project"]] <- "chromatograms-demo"
ms

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

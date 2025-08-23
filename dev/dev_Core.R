library(StreamFind)

## Metadata -----
a <- Metadata()
sloop::s3_dispatch(length(a))
a["name"] <- "Ricardo"
length(a)
names(a)
show(a)
class(a)
save(a)
a <- read(Metadata(), "metadata.json")

# Config -----
a <- ConfigParameter()
c <- ConfigCache()
size(c)
info(c)
b <- AppConfig()
e <- EngineConfig()

# AuditTrail -----
a <- AuditTrail()
a <- add(a, Metadata())
a <- add(a, EngineConfig())
show(a)

# Workflow -----
proset <- ProcessingStep()
show(proset)
wf <- Workflow()
wf[["a"]] <- MassSpecMethod_FindFeatures_openms()
show(wf)
?Workflow

# Analyses -----
a <- Analyses()

# MassSpecAnalyses -----
ms_files <- StreamFindData::get_ms_file_paths()[1:3]
db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
cols <- c("name", "formula", "mass", "rt", "fragments", "tag")
db <- db[, cols, with = FALSE]
dbis <- db[grepl("IS", db$tag), ]
dbsus <- db[!grepl("IS", db$tag), ]
a <- MassSpecAnalyses(ms_files)
a <- set_replicate_names(a, c("sample", "sample", "sample"))
get_replicate_names(a)
get_blank_names(a)
info(a)
get_spectra_tic(a, as_list = TRUE)
get_spectra_bpc(a)
plot_spectra_bpc(a, levels = 1)
get_raw_spectra(a, analyses = 1, mass = dbsus[15, ], ppm = 20, sec = 30)
plot_spectra_ms1(a, analyses = 1, mass = dbsus[15, ], ppm = 20, sec = 30)
plot_spectra_ms2(a, analyses = 1, mass = dbsus[15, ], ppm = 20, sec = 30)
plot_spectra_eic(a, analyses = 1, mass = dbsus[15, ], ppm = 20, sec = 30)
a <- load_spectra(a, analyses = 1, mass = dbsus[15, ], ppm = 20, sec = 30)
lapply(a$results, class)

## Engine ------
engine_file <- paste0("engine.json")
engine <- Engine$new()
show(engine$Metadata)
show(engine$Workflow)
show(engine$Analyses)
show(engine$AuditTrail)
show(engine$Config)
as.data.table(engine$AuditTrail)
engine$save(engine_file)
engine$Metadata[["name"]] <- "Ricardo"
engine$load("engine.json")
engine <- Engine$new(Metadata = list(file = "engine.rds"))

# MassSpecResults_Spectra -----
ms_files <- StreamFindData::get_ms_file_paths()[1:3]
db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
cols <- c("name", "formula", "mass", "rt", "fragments", "tag")
db <- db[, cols, with = FALSE]
dbis <- db[grepl("IS", db$tag), ]
dbsus <- db[!grepl("IS", db$tag), ]

engine <- MassSpecEngine$new(
  metadata = Metadata(list(name = "Ricardo")),
  analyses = ms_files,
)

engine$run(
  MassSpecMethod_LoadSpectra_native(
    mzmin = 200,
    mzmax = 300,
    rtmin = 1000,
    rtmax = 1100,
    minIntensity = 500,
    levels = 1
  )
)

#plot_spectra_3d(engine$Analyses$results$MassSpecResults_Spectra)
#class(engine$Analyses$results$MassSpecResults_Spectra)

engine$run(
  MassSpecMethod_BinSpectra_StreamFind(
    binNames = c("rt", "mz"),
    binValues = c(10, 10),
    byUnit = TRUE,
    refBinAnalysis = 1
  )
)

#plot_spectra(engine$Analyses$results$MassSpecResults_Spectra)
plot_spectra_3d(engine$Analyses$results$MassSpecResults_Spectra)

engine$Results$MassSpecResults_Spectra$spectra[[1]]

clear_cache("all")

# MassSpecResults_NonTargetAnalysis -----
## Resources ----
ms_files <- StreamFindData::get_ms_file_paths()
ms_files <- ms_files[grepl("blank|influent|o3sw", ms_files)]
ms_files_df <- data.frame(
  "file" = ms_files,
  "replicate" = c(
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("in_neg", 3),
    rep("in_pos", 3),
    rep("out_neg", 3),
    rep("out_pos", 3)
  ),
  "blank" = c(
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("blank_neg", 3),
    rep("blank_pos", 3)
  )
)
db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
cols <- c("name", "formula", "mass", "rt", "fragments", "tag")
db <- db[, cols, with = FALSE]
dbis <- db[grepl("IS", db$tag), ]
dbsus <- db[!grepl("IS", db$tag), ]

engine <- MassSpecEngine$new(
  metadata = Metadata(list(name = "Ricardo")),
  analyses = ms_files,
)

engine$save("engine.rds")

run_app(file = "engine.rds")

engine$run(MassSpecMethod_FindFeatures_openms())

engine$run(
  MassSpecMethod_CalculateFeaturesQuality_StreamFind(
    filtered = FALSE,
    rtExpand = 2,
    mzExpand = 0.0005,
    minTracesIntensity = 1000,
    minNumberTraces = 6,
    baseCut = 0
  )
)

engine$run(MassSpecMethod_GroupFeatures_openms())

engine$run(MassSpecMethod_AnnotateFeatures_StreamFind())










save(engine$Analyses, "ms_analyses.json")
data <- read(engine$Analyses, "ms_analyses.json")
show(engine$Results$NonTargetAnalysisResults)
class(engine$Results$NonTargetAnalysisResults)
engine$Results$NonTargetAnalysisResults$features[[1]]
get_features(engine$Results$NonTargetAnalysisResults, mass = dbsus, ppm = 15, sec = 60)
engine$clear_cache()
?NonTargetAnalysisResults
class(engine$Results$NonTargetAnalysisResults)
show(engine$Metadata)
show(engine$Analyses)
show(engine$Workflow)
show(engine$AuditTrail)
class(engine)




# App -----
library(StreamFind)
ms_files <- StreamFindData::get_ms_file_paths()[1:3]
db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
cols <- c("name", "formula", "mass", "rt", "fragments", "tag")
db <- db[, cols, with = FALSE]
dbis <- db[grepl("IS", db$tag), ]
dbsus <- db[!grepl("IS", db$tag), ]

engine <- MassSpecEngine$new(
  metadata = Metadata(list(name = "Ricardo")),
  analyses = ms_files,
)

engine$save("engine.rds")
show(engine$Metadata)
engine$run_app()
run_app(file = "engine.rds")


# RamanAnalyses -----
raman_files <- StreamFindData::get_raman_file_paths()[1:2]
a <- RamanAnalyses(raman_files)
info(a)
plot_spectra(a, analyses = 1)

# RamanEngine -----
raman_files <- StreamFindData::get_raman_file_paths()[1:2]
b <- RamanEngine$new(metadata = list(name = "test engine for Raman analysis"), analyses = raman_files)
b$save("raman.rds")
show(b$Metadata)
b$run_app()
run_app(file = "raman.rds")

get_spectra(
  b$Analyses,
  analyses = 1
)


# StatisticAnalyses ------
chrom_data <- data.frame(
  "T0.5"  = c(0.02, 0.03, 0.01, 0.04, 0.02),
  "T1.0"  = c(0.05, 0.06, 0.07, 0.04, 0.05),
  "T1.5"  = c(0.10, 0.12, 0.15, 0.09, 0.11),
  "T2.0"  = c(0.30, 0.28, 0.32, 0.29, 0.31),
  "T2.5"  = c(0.20, 0.21, 0.19, 0.22, 0.18),
  "T3.0"  = c(0.05, 0.06, 0.04, 0.05, 0.05)
)
rownames(chrom_data) <- c("Analysis_1", "Analysis_2", "Analysis_3", "Analysis_4", "Analysis_5")
attr(chrom_data, "chromatogram") <- "Chromatogram 1"

a <- StatisticAnalyses(chrom_data[1:4, ])
get_analysis_names(a)
a <- add(a, chrom_data[5, ])
a <- set_analysis_classes(a, c("A", "B", "C", "D", "E"))
info(a)
plot_data(a)



# StatisticEngine -----
b <- StatisticEngine$new(analyses = chrom_data[1:4, ])
b$run(StatisticMethod_MakeModel_pca_mdatools())
res <- b$Results$StatisticResults_PCA_mdatools
plot_scores(res)
res_pred <- predict(res, chrom_data[5, ])
plot_explained_variance(res_pred)
plot_cumulative_explained_variance(res_pred)
plot_scores(res_pred)
plot_loadings(res_pred)
plot_residuals(res_pred)
get_model_data(res)

b <- StatisticEngine$new(analyses = chrom_data[1:4, ])
b$run(StatisticMethod_MakeModel_mcrpure_mdatools())
res <- b$Results$StatisticResults_MCRPURE_mdatools
summary(res)
plot_overview(res)
plot_resolved_spectra(res)
plot_explained_variance(res)
plot_cumulative_explained_variance(res)
plot_contributions(res)































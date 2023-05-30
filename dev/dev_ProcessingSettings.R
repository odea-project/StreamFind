
# library(streamFind)

save_default_ProcessingSettings(
  call = "find_features",
  software = "xcms",
  algorithm = "centwave"
)

# save_default_ProcessingSettings(
#   call = "find_features",
#   algorithm = "openms"
# )

all_files <- streamFindData::msFilePaths()
files <- all_files[1:3]

ms <- MassSpecData$new(files)

ms$import_settings("./settings.json")
ms$find_features()


ms$get_features()

ms$get_settings()






db <- "E:\\Dev_20230530_Orbitrap_AFINTS\\Composition_Mix-Fusion.csv"
db <- data.table::fread(db)
# mol <- rcdk::parse.smiles(db$SMILES)
# db$mass <- vapply(mol, rcdk::get.exact.mass, NA_real_)
# write.csv(db, "E:\\Dev_20230530_Orbitrap_AFINTS\\Composition_Mix-Fusion.csv")

db$mz = db$mass + 1.00726

fl <- "E:\\Dev_20230530_Orbitrap_AFINTS\\220428_VqF-Coup_Etablierung_MixFusion_pos_02.mzML"
ms <- MassSpecData$new(fl)
ms$import_settings("./settings.json")
ms$find_features()




fts <- ms$get_features()
fts <- fts[order(fts$mz), ]

ms$get_features(features = "mz254.059_rt720_f2123")

ms$get_features(mz = db$mz[2], ppm = 2)


ms$plot_features_ms1(
  mz = db$mz[2],
  ppm = 2,
  rtWindow = c(-2, 2),
  mzWindow = c(-1, 6),
  mzClust = 0.0005
)


View(rcpp_ms_annotation_isotopes(fts))



ms$plot_bpc()
ms$get_files()


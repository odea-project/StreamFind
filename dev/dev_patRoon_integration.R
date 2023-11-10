
library(patRoon)

# Resources -------------------------------------------------------------------

pat_all_files <- StreamFindData::get_ms_file_paths()

pat_all_db <- StreamFindData::get_ms_tof_spiked_chemicals()

pat_db <- pat_all_db[!grepl("IS", pat_all_db$tag), ]
pat_cols <- c("name", "formula", "mass", "SMILES")
pat_db <- pat_db[, pat_cols, with = FALSE]
data.table::setnames(pat_db, "mass", "neutralMass")

pat_dbis <- pat_all_db[grepl("IS", pat_all_db$tag), ]
pat_cols <- c("name", "formula", "mass", "rt")
pat_dbis <- pat_dbis[, pat_cols, with = FALSE]
data.table::setnames(pat_dbis, "mass", "neutralMass")



# Test find_internal_standards -------------------------------------------------

anaInfo <- data.frame(
  "path" = dirname(pat_all_files[1]),
  "analysis" = tools::file_path_sans_ext(basename(pat_all_files[grepl("blank|influent|o3sw", pat_all_files)])),
  "group" = c(
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

fts <- findFeatures(anaInfo[c(4:6, 10:12, 16:18), ], "openms", verbose = TRUE)

gfs <- groupFeatures(fts, "openms", verbose = TRUE)




temp <- normInts(gfs, featNorm = "istd", standards = pat_dbis, adduct = "[M+H]+")


all.equal(gfs@groups, temp@groups)




sus <- screenSuspects(
  gfs,
  pat_db,
  rtWindow = 10,
  mzWindow = 0.005,
  adduct = "[M+H]+",
  skipInvalid = TRUE,
  prefCalcChemProps = TRUE,
  neutralChemProps = FALSE,
  onlyHits = FALSE
)

sustox <- predictTox(sus)

suscalctox <- calculateTox(sustox)

toxicities(suscalctox)


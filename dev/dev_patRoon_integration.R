
library(patRoon)

# Resources -------------------------------------------------------------------

pat_all_files <- StreamFindData::get_ms_file_paths()

pat_all_db <- StreamFindData::get_ms_tof_spiked_chemicals()

pat_db <- pat_all_db[!grepl("IS", pat_all_db$tag), ]
pat_cols <- c("name", "formula", "mass", "SMILES", "rt")
pat_db <- pat_db[, pat_cols, with = FALSE]
data.table::setnames(pat_db, "mass", "neutralMass")

pat_db$neutralMass <- pat_db$neutralMass + 1.0073
data.table::setnames(pat_db, "neutralMass", "mz")

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


anaInfoPos <- anaInfo[grepl("pos", anaInfo$analysis), ]

anaInfoNeg <- anaInfo[grepl("neg", anaInfo$analysis), ]






fListPos <- findFeatures(anaInfoPos, "openms")





fListNeg <- findFeatures(anaInfoNeg, "openms")

# fList <- makeSet(fListPos, fListNeg, adducts = c("[M+H]+", "[M-H]-"))
fList <- fListPos 

gfs <- groupFeatures(fListPos, "openms", verbose = TRUE)

getTICs(fListPos[2], MSLevel = 2)
getTICs(gfs[1, ])

getBPCs(fListPos)
getBPCs(gfs[3, ])

plotTICs(fListPos, colourBy = "rGroups", MSLevel = 1)
plotBPCs(fListPos, colourBy = "analyses", MSLevel = 1)

plotTICs(gfs, colourBy = "analyses", MSLevel = 1, retentionRange = c(1100, 1200))
plotBPCs(gfs, colourBy = "analyses", MSLevel = 1, retentionRange = c(1100, 1200))

plotChroms(gfs[5, 3])

plot(gfs)

plotInt(gfs)

sus <- screenSuspects(
  gfs,
  pat_db,
  rtWindow = 10,
  mzWindow = 0.005,
  skipInvalid = TRUE,
  prefCalcChemProps = TRUE,
  neutralChemProps = FALSE,
  onlyHits = FALSE
)




# View(gfs[, !names(gfs@groups) %in% gfs@features@features$`01_tof_ww_is_pos_blank-r002`$group])
# temp <- normInts(gfs, featNorm = "istd", standards = pat_dbis, adduct = "[M+H]+")
# all.equal(gfs@groups, temp@groups)
# sustox <- predictTox(sus)
# suscalctox <- calculateTox(sustox)
# toxicities(suscalctox)


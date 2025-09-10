all_files_dir <- "C:\\Users\\apoli\\Documents\\example_files\\peak_finding_files_ex"
all_files <- list.files(all_files_dir, full.names = TRUE, recursive = TRUE, pattern = "\\.mzML$")

files_tof_cent <- all_files[grepl("tof_centroid", all_files)]
files_tof_cent <- files_tof_cent[!grepl("wastewater", files_tof_cent)]
files_tof_prof <- all_files[grepl("tof_profile", all_files)]
files_orb_cent <- all_files[grepl("orbitrap_centroid", all_files)]
files_orb_prof <- all_files[grepl("orbitrap_profile", all_files)]
files_tof_ww <- all_files[grepl("wastewater", all_files)]

files <- c(
  files_tof_cent[1],
  files_tof_prof[1],
  files_orb_cent[7],
  files_orb_prof[7],
  files_tof_ww[11]
)

db_all <- StreamFindData::get_ms_tof_spiked_chemicals()
db_all <- db_all[grepl("S", db_all$tag), ]
cols <- c("name", "formula", "mass", "rt", "tag")
db_is <- db_all[db_all$tag %in% "IS", ]
db_is <- db_is[, cols, with = FALSE]
db_is <- db_is[!db_is$name %in% c("Ibuprofen-d3", "Naproxen-d3"), ]
db_is_with_mz <- data.table::copy(db_is)
db_is_with_mz$mz <- db_is_with_mz$mass + 1.007276
db_is_with_mz$mass <- NULL
db <- db_all[db_all$tag %in% "S", ]
db <- db[, cols, with = FALSE]
db_with_mz <- data.table::copy(db)
db_with_mz$mz <- db_with_mz$mass + 1.007276
db_with_mz$mass <- NULL
db_with_ms2 <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
db_with_ms2 <- db_with_ms2[db_with_ms2$tag %in% "S", ]
db_with_ms2 <- db_with_ms2[, c("name", "formula", "mass", "SMILES", "rt", "polarity", "fragments"), with = FALSE]
db_with_ms2$polarity[db_with_ms2$polarity == 1] <- "positive"
db_with_ms2$polarity[is.na(db_with_ms2$polarity)] <- "positive"
db_with_ms2$polarity[db_with_ms2$polarity == -1] <- "negative"

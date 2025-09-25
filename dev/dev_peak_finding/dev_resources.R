#all_files_dir <- "C:\\Users\\apoli\\Documents\\example_files\\peak_finding_files_ex"
all_files_dir <- "D:\\peak_finding_files_ex"
all_files <- list.files(all_files_dir, full.names = TRUE, recursive = TRUE, pattern = "\\.mzML$")

files_tof_cent <- all_files[grepl("tof_centroid", all_files)]
files_tof_cent <- files_tof_cent[!grepl("wastewater", files_tof_cent)]
files_tof_prof <- all_files[grepl("tof_profile", all_files)]
files_orb_cent <- all_files[grepl("orbitrap_centroid", all_files)]
files_orb_prof <- all_files[grepl("orbitrap_profile", all_files)]
files_tof_ww <- all_files[grepl("wastewater", all_files)]
files_merck_ex <- all_files[grepl("merck/Beispieldaten Routine/ACC1_26393", all_files)]
files_merck_ex_centroid <- all_files[grepl("Beispieldaten Routine/centroid_ACC1_26393", all_files)]

files <- c(
  files_tof_cent[1],
  files_tof_prof[1],
  files_orb_cent[7],
  files_orb_prof[7],
  files_tof_ww[11],
  #files_merck_ex[1:2],
  files_merck_ex_centroid[1:2]
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

db_merck <- fread(list.files(all_files_dir, full.names = TRUE, recursive = TRUE, pattern = "\\ACC_26393_1.csv$"))
db_merck$name <- paste0("Peak ", db_merck$MW)

# MARK: Find Peak Targets
find_peak_targets <- function(peaks_dt, db, ppm_tol = 20, rt_tol = 60) {
  results <- list()
  for (i in seq_len(nrow(db))) {
    target_mz <- db$mz[i]
    target_rt <- db$rt[i]
    target_name <- db$name[i]
    mz_diff_ppm <- abs(peaks_dt$mz - target_mz) / target_mz * 1e6
    mz_diff_ppm_wt <- abs(peaks_dt$mz_wt - target_mz) / target_mz * 1e6
    mz_diff_ppm_appex <- abs(peaks_dt$mz_appex - target_mz) / target_mz * 1e6
    rt_diff <- abs(peaks_dt$rt - target_rt)
    match_idx <- which(
      (mz_diff_ppm <= ppm_tol | mz_diff_ppm_wt <= ppm_tol | mz_diff_ppm_appex <= ppm_tol) & rt_diff <= rt_tol
    )
    if (length(match_idx) > 0) {
      # best_idx <- match_idx[which.min(rt_diff[match_idx])]
      # match_idx <- best_idx
      for (j in match_idx) {
        results[[length(results) + 1]] <- data.frame(
          peak_id = peaks_dt$id[j],
          target_name = target_name,
          target_mz = target_mz,
          target_rt = target_rt,
          peak_mz = peaks_dt$mz[j],
          peak_mz_sd = round(peaks_dt$mz_sd[j] / peaks_dt$mz[j] * 100, 3),
          peak_mz_wt = peaks_dt$mz_wt[j],
          peak_mz_wt_sd = round(peaks_dt$mz_wt_sd[j] / peaks_dt$mz_wt[j] * 100, 3),
          peak_mz_appex = peaks_dt$mz_appex[j],
          peak_mz_appex_sd = round(peaks_dt$mz_appex_sd[j] / peaks_dt$mz_appex[j] * 100, 3),
          peak_rt = peaks_dt$rt[j],
          ppm = round(mz_diff_ppm[j], 2),
          ppm_wt = round(mz_diff_ppm_wt[j], 2),
          ppm_appex = round(mz_diff_ppm_appex[j], 2),
          rt_diff = round(rt_diff[j], 2),
          n_traces = peaks_dt$n_traces[j],
          intensity = peaks_dt$intensity[j]
        )
      }
    }
  }
  if (length(results) > 0) {
    do.call(rbind, results)
  } else {
    data.frame()
  }
}

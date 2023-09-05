
all_files <- streamFindData::msFilePaths()

db <- streamFindData::msSpikedChemicals()
db <- db[grepl("S", db$tag), ]
cols <- c("name", "formula", "mass", "rt")
db <- db[, cols, with = FALSE]
db

files_df <- data.frame(
  "file" = all_files[grepl("blank|influent|o3sw", all_files)],
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

ms <- MassSpecData$new(files = files_df)

ms$add_settings(
  list(
    Settings_find_features_xcms3_centwave(),
    Settings_group_features_xcms3_peakdensity(),
    Settings_filter_features_streamFind(
      minIntensity = 5000,
      minSnRatio = 20,
      maxGroupSd = 30,
      blank = 5,
      minGroupAbundance = 3,
      excludeIsotopes = TRUE
    ),
    Settings_load_features_ms2_streamFind(),
    Settings_load_groups_ms2_streamFind()
  )
)

ms$get_history()

ms$find_features()$group_features()$filter_features()

ms

ms$plot_spectra(analyses = 10:12, mz = 254.0594, ppm = 20, allTraces = FALSE, levels = c(1, 2), colorBy = "levels")
# TODO make feature ms1 and feature group MS2 for PPT


ms$get_groups(mass = db)

suspects <- ms$get_suspects(analyses = 7:12, database = db, ppm = 10, sec = 15)

msbp <- ms$subset_analyses(10:12)
msbp$remove_features(filtered = TRUE)
msbp <- msbp$subset_features(features = suspects)

msbp$load_features_ms2()
msbp$load_groups_ms2()



sssfi <- Settings_suspect_screening_forident(addMS2 = TRUE)
msbp$suspect_screening(sssfi)



file.remove("feature_list.txt")







write.csv(suspects_g, "C:/Users/Ricardo Cunha/Desktop/suspects_g.csv", row.names = FALSE)

sink("C:/Users/Ricardo Cunha/Desktop/suspects_g.txt")
cat("\n")
cat("\n")

for (i in 1:nrow(suspects_g)) {
  cat("NAME: ")
  cat(suspects_g$Label[i])
  cat("\n")
  cat("RETENTIONTIME: ")
  cat(round(suspects_g$RT[i], digits = 3))
  cat("\n")
  cat("Mass: ")
  cat(round(suspects_g$Mass[i], digits = 4))
  cat("\n")
  cat("Formula: ")
  cat("\n")
  cat("//")
  cat("\n")
  cat("\n")
}
sink()





# sink(paste0(getwd(),"/", "forident",".txt"))
# cat("\n")
# cat("\n")
#
# for (i in 1:nrow(suspects_g)) {
#   cat("NAME: ")
#   cat(suspects_g$group[i])
#   cat("\n")
#   cat("RETENTIONTIME: ")
#   cat(round(suspects_g$rt[i] / 60, digits = 3))
#   cat("\n")
#   cat("PRECURSORMZ: ")
#   cat(round(suspects_g$mass[i] + 1.0073, digits = 4))
#   cat("\n")
#   cat("Formula: ")
#   cat("\n")
#   if (ForIdent_PeakList$MS2[i] == TRUE) {
#     tempMS2 <- MS2[[ForIdent_PeakList$group[i]]]$MSMS
#     for (j in 1:nrow(tempMS2)) {
#       cat(paste(round(tempMS2$mz[j], digits = 4),tempMS2$intensity[j], sep = " "))
#       cat(" ")
#     }
#     rm(j, tempMS2)
#   } else {
#     cat("N/A")
#   }
#   cat("\n")
#   cat("//")
#   cat("\n")
#   cat("\n")
# }
# sink()
















sss <- Settings_suspect_screening_streamFind(database = db, ppm = 5, sec = 10)




# implement export MS2 ------

slfms2 <- Settings_load_features_ms2_streamFind()
slfms2$parameters$minIntensity <- 100
slgms2 <- Settings_load_groups_ms2_streamFind()
slgms2$parameters$minIntensity <- 100
msbp <- ms$subset_analyses(4:6)


msbp$remove_features(filtered = TRUE)
msbp <- msbp$subset_features(features = suspects)
msbp$load_features_ms2(slfms2)
msbp$load_groups_ms2(slgms2)

msbp$suspect_screening(sss)

View(msbp$get_modules_data("suspect_screening"))



ms2 <- ms$get_spectra(analyses = 4, mz = 239.0629, rt = 1157.414, ppm = 20, sec = 30, levels = 2, allTraces = FALSE)
ms2$unique_id <- paste0(ms2$analysis, "_", ms2$id)


rcpp_ms_cluster_ms2(ms2, 0.005, TRUE, TRUE)



ms$get_ms2(analyses = 4, mz = 267.0702, rt = 1007.222, ppm = 20, sec = 30, mzClust = 0.01, isInAllSpectra = TRUE)

ms$get_ms2(analyses = 5, mz = 267.0702, rt = 1007.222, ppm = 20, sec = 30, mzClust = 0.01, isInAllSpectra = TRUE)


ms$plot_ms1(analyses = 4, mz = data.frame(mzmin = 239, mzmax = 245, rtmin = 1156, rtmax = 1158), isInAllSpectra = TRUE, verbose = TRUE)
ms1




View(suspects)

ms$plot_groups_ms2(groups = suspects$group[1],
  isolationWindow = 1.3,
  mzClustFeatures = 0.003,
  minIntensityFeatures = 150
)



fls <- list.files("D:/NTS/Project_230829_LINEG_LCMSMS_Scan_KA_Gesamtablauf/mzml", full.names = TRUE)
ms <- MassSpecData$new(fls[c(1, 40)]) #[grepl("neg", fls)]

ffs <- Settings_find_features_xcms3_matchedfilter(binSize = 0.5, snthresh = 40)
gfs <- Settings_group_features_xcms3_peakdensity(bw = 5, binSize = 0.5)

ms$find_features(ffs)

ms$get_features()

ms$group_features(gfs)

ms$get_groups()

rtf1 <- Settings_filter_features_streamFind(
  rtFilter = c(0, 100)
)

rtf2 <- Settings_filter_features_streamFind(
  rtFilter = c(1400, 1500)
)

ms$filter_features(rtf1)

ms$filter_features(rtf2)

ms$group_features(gfs)

View(ms$get_groups_coverage())

ms$save()

View(ms$get_features())

ms$plot_features(features = "mz438.4_rt1174_g97")

ms$get_run()

ms$plot_spectra(analyses = 2, mz = data.frame(
  mzmin = 337.5, mzmax = 339, rtmin = 1200, rtmax = 1300
))


# history ----------------------------------------------------------------------

ms$get_history()


# other ------------------------------------------------------------------------

# ms$save_settings(format = "json", name = "settings")


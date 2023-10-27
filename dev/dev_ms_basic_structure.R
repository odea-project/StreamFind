
# Tasks -----------------------------------------------------------------------

# TODO add stats to the groups (e.g., presence in each replicate, coverage)
# TODO check what happens when all MS2 centroids are added to clustering
# TODO Features and groups MS1 could have a is_pre.
# TODO add possibility to add_files in MassSpecData
# TODO add is_pre to MS1 spectra
# TODO add filter for features/groups with more than 1 representation in components
# TODO add signal to noise ratio to internal standards report, calculated on demand
# TODO check patRoon MSPeakLists and add is_pre in MS1
# TODO when subsetting on features/groups check/add if features_eics are also changed


# Resources -------------------------------------------------------------------

all_files <- StreamFindData::get_ms_file_paths()

all_db <- StreamFindData::get_ms_tof_spiked_chemicals()
db <- all_db[grepl("S", all_db$tag), ]
cols <- c("name", "formula", "mass", "rt")
db <- db[, cols, with = FALSE]





# Test find_internal_standards -------------------------------------------------

dbis <- all_db[grepl("IS", all_db$tag), ]
cols <- c("name", "formula", "mass", "rt")
dbis <- dbis[, cols, with = FALSE]

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

ms <- MassSpecData$new(files_df)

ms$add_settings(
  list(
    Settings_find_features_openms(),
    Settings_annotate_features_StreamFind(),
    Settings_group_features_openms(),
    Settings_find_internal_standards_StreamFind(database = dbis, ppm = 8, sec = 10),
    Settings_filter_features_StreamFind(
      minIntensity = 5000,
      #minSnRatio = 20,
      maxGroupSd = 30,
      blank = 5,
      minGroupAbundance = 3,
      excludeIsotopes = TRUE
    ),
    Settings_load_features_eic_StreamFind()
    # Settings_load_features_ms1_StreamFind(),
    # Settings_load_features_ms2_StreamFind(),
    # Settings_load_groups_ms1_StreamFind(),
    # Settings_load_groups_ms2_StreamFind()
  )
)

ms$run_workflow()

qlt <- ms$calculate_quality(Settings_calculate_quality_StreamFind())


suspects <- ms$get_suspects(database = db, ppm = 10, sec = 15, filtered = FALSE)


test <- qlt[qlt$feature %in% suspects$feature, ]

ft <- qlt[qlt$feature %in% "mz268.19_rt916_f50" & qlt$analysis %in% "02_tof_ww_is_pos_influent-r003", ]
ft_m <- ft$qlt_model[[1]]
plotly::plot_ly() %>%
plotly::add_trace(
  x = seq_along(ft_m$real_values),
  y = ft_m$real_values,
  name = 'real', type = 'scatter',
  mode = 'markers', marker = list(color = "black")
) %>%
plotly::add_trace(
  x = seq_along(ft_m$real_values),
  y = ft_m$predicted_values,
  type = 'scatter', name = 'predicted',
  mode = 'lines', line = list(color = "red")
)
ft$qlt_noise
ft$qlt_sn
ft$qlt_gaufit






# ms$plot_groups(
#   groups = suspects,
#   filtered = TRUE,
#   interactive = FALSE,
#   cex = 1,
# )

ms$get_groups_ms1(mass = db, filtered = FALSE)
ms$plot_groups_ms2(mass = db, filtered = FALSE, legendNames = TRUE)
ms$get_components(mass = db)




ms2 <- ms$subset_features(suspects)

anas <- ms2$get_analyses()

View(anas[[10]])






ms$plot_groups_overview(mass = db[1:3, ], filtered = TRUE, legendNames = TRUE)





ms2$add_settings(Settings_load_features_eic_StreamFind())

ms2$add_features_eic(ms2$load_features_eic())

ms2$get_features_eic(mass = db)

















ms$plot_features(features = "mz237.05_rt1158_f227")

ms$plot_features(mass = db, ppm = 10, sec = 15, rtExpand = 120, legendNames = TRUE)

# fts <- ms$get_features(analyses = 1)
# fts <- fts[order(fts$mz), ]
# which(fts$feature %in% "mz300.05_rt1255_f175")
# output <- rcpp_ms_annotation_isotopes(fts, rtWindowAlignment = 0.3, verbose = TRUE)



fg <- ms$as_patRoon_featureGroups()

db2 <- data.table::copy(all_db)
db2 <- db2[grepl("IS", db2$tag), ]
cols <- c("name", "formula", "mass", "rt")
db2 <- db2[, cols, with = FALSE]
db2[["formula"]] <- NULL
data.table::setnames(db2, "mass", "neutralMass")


?patRoon::normInts

fgn <- patRoon::normInts(
  fg,
  featNorm = "istd",
  standards = db2, #patRoonData::ISTDListPos
  adduct = "[M+H]+",
  ISTDRTWindow = 20,
  ISTDMZWindow = 200,
  minISTDs = 2
)

fgn <- patRoon::normInts(
  fg,
  featNorm = "tic",
  standards = db2, #patRoonData::ISTDListPos
  adduct = "[M+H]+",
  ISTDRTWindow = 20,
  ISTDMZWindow = 200,
  minISTDs = 2
)

patRoon::plotGraph(fgn)



# MSPeaksLists ----------------------------------------------------------------

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

# ms <- ms$subset_analyses(c(4:6, 10:12, 16:18))

ms$add_settings(
  list(
    Settings_find_features_xcms3_centwave(),
    Settings_group_features_xcms3_peakdensity(),
    Settings_filter_features_StreamFind(
      minIntensity = 10000,
      minSnRatio = 20,
      maxGroupSd = 30,
      blank = 5,
      minGroupAbundance = 3
    ),
    Settings_load_features_ms1_StreamFind(presence = 0.5),
    Settings_load_features_ms2_StreamFind(presence = 0.5),
    Settings_load_groups_ms1_StreamFind(presence = 0.5),
    Settings_load_groups_ms2_StreamFind(presence = 0.5)
  )
)

ms$find_features()
ms$group_features()
ms$filter_features()

combine_MassSpecData(ms, ms2)


# patRoon::clearCache("parsed_ms_analyses")
patRoon::clearCache("parsed_ms_spectra")
patRoon::clearCache("load_features_ms1")
patRoon::clearCache("load_features_ms2")
patRoon::clearCache("load_groups_ms1")
patRoon::clearCache("load_groups_ms2")
# patRoon::clearCache("MSPeakListsAvg")
# ms$get_history()



# ms$plot_spectra(1, xVal = "drift")

suspects <- ms$get_suspects(database = db, ppm = 10, sec = 15)
ms <- ms$subset_features(features = suspects)

ms$load_features_ms1()
ms$load_features_ms2()
ms$load_groups_ms1()
ms$load_groups_ms2()

View(ms$get_groups_ms2())

ms$plot_groups_ms1(colorBy = "targets+polarities")


# ms$get_features_ms1(loadedMS1 = T)
#
# ms$plot_features_ms2(mass = db[7, ], loadedMS2 = T)
#
# ms$get_groups_ms1(loadedFeaturesMS1 = T, loadedGroupsMS1 = T)
#
# ms$get_groups_ms2(loadedFeaturesMS2 = T, loadedGroupsMS2 = T)
#
# ms$plot_groups_ms2(mass = db[7, ], colorBy = "targets+polarities")
#
# ms$plot_groups_ms1(mass = db[7, ], colorBy = "targets+polarities")

# pat_fg <- ms$as_patRoon_featureGroups()
#
# pat_pl <- ms$as_patRoon_MSPeakLists()
#
# ms$patRoon_report()


View(ms$get_features())


ffs <- Settings_find_features_xcms3_centwave()

ffs <- Settings_filter_features_StreamFind()

sloop::s3_dispatch(validate(ffs))

validate(ffs)




comp <- patRoon::generateCompoundsMetFrag(
  pat_fg,
  pat_pl,
  # mspl,
  adduct = "[M+H]+",
  dbRelMzDev = 5,
  fragRelMzDev = 10,
  fragAbsMzDev = 0.005,
  maxCandidatesToStop = 10000
)

formulas <- patRoon::generateFormulasGenForm(
  pat_fg,
  pat_pl,
  # mspl,
  relMzDev = 10,
  adduct = "[M+H]+",
  elements = "CHNOP",
  hetero = TRUE,
  oc = FALSE,
  thrMS = NULL,
  thrMSMS = NULL,
  thrComb = NULL,
  maxCandidates = Inf,
  extraOpts = NULL,
  calculateFeatures = FALSE,
  featThreshold = 0,
  featThresholdAnn = 0.75,
  absAlignMzDev = 0.005,
  MSMode = "both",
  isolatePrec = TRUE,
  timeout = 120,
  topMost = 50,
  batchSize = 8
)



ms$get_features_ms2(
  mass = db[11, ],
  presence = 0.8,
  minIntensity = 250
)
#
ms$plot_features_ms1(
  mass = db[11, ],
  presence = 0.8,
  mzWindow = c(-1, 6),
  rtWindow = c(-2, 2),
  minIntensity = 250,
  colorBy = "analyses"
)

# ms$get_features_ms1(
#   mass = diu, rt = diu_rt,
#   presence = 0.8,
#   mzWindow = c(-1, 6),
#   rtWindow = c(-2, 2),
#   minIntensity = 250
# )

ms$plot_groups_ms1(
  mass = db[11, ],
  presenceFeatures = 0.8,
  presenceGroups = 0.8,
  mzWindow = c(-1, 4),
  rtWindow = c(-2, 2),
  minIntensityFeatures = 250,
  minIntensityGroups = 250,
  colorBy = "targets+polarities"
)
#
# ms$plot_eic(mass = diu, rt = diu_rt, colorBy = "targets+polarities")
#
# ms$get_groups_ms1(
#   mass = diu, rt = diu_rt,
#   presenceFeatures = 0.8,
#   presenceGroups = 0.8,
#   mzWindow = c(-1, 6),
#   rtWindow = c(-2, 2),
#   minIntensityFeatures = 250,
#   minIntensityGroups = 250
# )

#ms$get_features()
#ms$get_groups()

patRoon::clearCache("load_features_ms2")



library(patRoon)

pat_fg <- ms$as_patRoon_featureGroups()

pat_fg@features@features

?generateMSPeakList

mspl <- generateMSPeakListsMzR(
  pat_fg,
  maxMSRtWindow = 5,
  precursorMzWindow = 4,
  topMost = NULL,
  avgFeatParams = getDefAvgPListParams(),
  avgFGroupParams = getDefAvgPListParams()
)

# patRoon::report(pat_fg)
# pat <- readRDS("example_pat.rds")


if (ms$has_groups()) {

  plist <- lapply(ms$get_analyses(), function(x) {

    features <- x$features
    features <- features[!features$filtered, ]

    groups <- unique(features$group)
    groups <- groups[!is.na(groups)]

    glist <- lapply(groups, function(x2, features) {
      out <- list()

      MS <- features$ms1[features$group %in% x2]
      MSMS <- features$ms2[features$group %in% x2]

      if (length(MS) > 1) {
        warning("")
        MS <- MS[1]
      }

      if (length(MSMS) > 1) {
        warning("")
        MSMS <- MSMS[1]
      }

      if (!is.null(MS[[1]])) {
        names(MS) <- "MS"
        out <- c(out, MS)
      }

      if (!is.null(MSMS[[1]])) {
        names(MSMS) <- "MSMS"
        out <- c(out, MSMS)
      }

      out

    }, features = features)

    names(glist) <- groups

    glist
  })

  names(plist) <- ms$get_analysis_names()

  groups <- ms$get_groups()

  aplist <- lapply(seq_len(nrow(groups)), function(x, groups) {
    out <- list()

    MS <- groups$ms1[x]
    MSMS <- groups$ms2[x]

    if (length(MS) > 1) {
      warning("")
      MS <- MS[1]
    }

    if (length(MSMS) > 1) {
      warning("")
      MSMS <- MSMS[1]
    }

    if (!is.null(MS[[1]])) {
      names(MS) <- "MS"
      out <- c(out, MS)
    }

    if (!is.null(MSMS[[1]])) {
      names(MSMS) <- "MSMS"
      out <- c(out, MSMS)
    }

    out


  }, groups = groups)

  names(aplist) <- groups$group

  new("MSPeakLists",
    peakLists = plist,
    averagedPeakLists = aplist,
    algorithm = "StreamFind"
  )
}






ms$plot_spectra(analyses = 10:12, mz = 254.0594, ppm = 20, allTraces = FALSE, levels = c(1, 2), colorBy = "levels")



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
















sss <- Settings_suspect_screening_StreamFind(database = db, ppm = 5, sec = 10)




# implement export MS2 ------

slfms2 <- Settings_load_features_ms2_StreamFind()
slfms2$parameters$minIntensity <- 100
slgms2 <- Settings_load_groups_ms2_StreamFind()
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


ms$plot_ms1(analyses = 4,
  mz = data.frame(mzmin = 239, mzmax = 245, rtmin = 1156, rtmax = 1158),
  isInAllSpectra = TRUE, verbose = TRUE
)

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

rtf1 <- Settings_filter_features_StreamFind(
  rtFilter = c(0, 100)
)

rtf2 <- Settings_filter_features_StreamFind(
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


library(StreamFind)

# Tasks -----------------------------------------------------------------------

# TODO add export for fragments to suspectsList, think how to handle polarities
# TODO add stats to the groups (e.g., presence in each replicate, coverage)
# TODO check what happens when all MS2 centroids are added to clustering
# TODO Features and groups MS1 could have a is_pre.
# TODO add filter for features/groups with more than 1 representation in components
# TODO add signal to noise ratio to internal standards report, calculated on demand
# TODO when subsetting on features/groups check/add if features_eics are also changed
# TODO add suspects to MassSpecData matching patRoon
# TODO check the import MassSpecData from JSON, in particular the nested data.tables


# Resources -------------------------------------------------------------------

all_files <- StreamFindData::get_ms_file_paths()

all_db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
db <- all_db[!grepl("IS", all_db$tag, fixed = TRUE), ]
cols <- c("name", "formula", "mass", "SMILES", "rt", "polarity", "fragments")
# cols <- c("name", "formula", "mass", "SMILES", "rt")
db <- db[, cols, with = FALSE]
# data.table::setnames(db, "mass", "neutralMass")

# db$neutralMass <- db$neutralMass + 1.0073
# data.table::setnames(db, "neutralMass", "mz")

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

ps <- list(
  Settings_find_features_openms(),
  Settings_annotate_features_StreamFind(),
  Settings_group_features_openms(),
  Settings_find_internal_standards_StreamFind(database = dbis, ppm = 8, sec = 10),
  Settings_filter_features_StreamFind(minIntensity = 5000, maxGroupSd = 30, blank = 5, minGroupAbundance = 3, excludeIsotopes = TRUE),
  Settings_load_features_eic_StreamFind(rtExpand = 60, mzExpand = 0.0005, runParallel = FALSE),
  # Settings_load_features_ms1_StreamFind(),
  Settings_load_features_ms2_StreamFind(),
  Settings_calculate_quality_StreamFind(),
  Settings_filter_features_StreamFind(minSnRatio = 3),
  Settings_suspect_screening_StreamFind(database = db, ppm = 10, sec = 15, ppmMS2 = 10, minFragments = 3),
  Settings_filter_features_StreamFind(onlySuspects = TRUE)
)

# patRoon::clearCache("all")
# patRoon::clearCache(c("calculate_quality"))
# patRoon::clearCache(c("load_features_ms2"))
# patRoon::clearCache(c("load_features_ms1"))

ms <- MassSpecData$new(files_df) #[grepl("pos", files_df$replicate), ]
ms$add_settings(ps)
ms$run_workflow()

ms$plot_groups(interactive = F)



# Export MS2 pattern -----

# std <- MassSpecData$new(list.files("/home/cunha/Documents/example_ms_files/", pattern = "00_hrms_mix1_pos_cent", full.names = TRUE))
# std <- MassSpecData$new(all_files[1:3])
# std$add_settings(ps)
# std$run_workflow()


ms$get_features(filtered = FALSE)


View(fts$istd)
ms$get_internal_standards()



std$plot_suspects()




std$plot_eic(analyses = 1, mass = db, ppm = 10, sec = 15, legendNames = T)

std$get_features(mass = db[9, ], filtered = TRUE)




ms$plot_suspects()

ms$save()



std$plot_bpc(levels = 1)








new_db <- fread("tof_spiked_chemicals_ms2.csv")

std$get_suspects(
  database = new_db,
  ppm = 8, sec = 10
)

View(std$get_features(mass = db))



std$plot_groups(mass = db, legendNames = T)


std$plot_features_ms2(mass = db[9, ], ppm = 10, loadedMS2 = F, legendNames = T)


std$plot_groups_ms2(mass = db, ppm = 5, mzClust = 0.005, presence = 0.3, loadedFeaturesMS2 = TRUE, legendNames = T)

ms2 <- std$get_groups_ms2(mass = db, mzClust = 0.005, presence = 0.4)
db_out <- all_db
if (!"polarity" %in% colnames(db_out)) {
 db_out$polarity <- NA_integer_
}
std_names <- unique(ms2$name)
for (i in std_names) {
  t_ms2 <- ms2[ms2$name %in% i, ]
  t_ms2[["name"]] <- NULL
  t_ms2[["group"]] <- NULL
  t_ms2[["feature"]] <- NULL
  
  polarities <- unique(t_ms2$polarity)
  
  polarities_vec <- t_ms2$polarity
  t_ms2[["polarity"]] <- NULL
  t_ms2 <- split(t_ms2, polarities_vec)
  
  if ("polarity" %in% colnames(db_out)) {
    dbpol <- db_out$polarity[db_out$name %in% i]
  }
  
  # adds fragments to existing row
  if (is.na(dbpol)) {
    # adds 1st pol to existing and adds new row for 2nd pol
    if (length(polarities) > 1) {
      
      
    } else {
      db_out$polarity[db_out$name %in% i] <- polarities[1]
      db_out$fragments[db_out$name %in% i] <- paste(
        round(t_ms2[[1]]$mz, digits = 5),
        round(t_ms2[[1]]$intensity, digits = 0),
        sep = " ", collapse = "; "
      )
    }
  }
}
write.csv(db_out, "tof_spiked_chemicals_ms2.csv", row.names = FALSE)


fex <- paste(
  round(t_ms2[[1]]$mz, digits = 5),
  round(t_ms2[[1]]$intensity, digits = 0),
  sep = " ", collapse = "; "
)

fexu <- unlist(strsplit(fex, split = "; ", fixed = TRUE))
fexu <- strsplit(fexu, " ")
fexu_mz <- sapply(fexu, function(x) as.numeric(x[1]))
fexu_int <- sapply(fexu, function(x) as.numeric(x[2]))


js_database <- split(db_out, all_db$name)
names(js_database) <- db_out$name
js_database <- lapply(js_database, function(x) unlist(x, recursive = FALSE))

js_database <- jsonlite::toJSON(
  js_database,
  dataframe = "rows",
  Date = "ISO8601",
  POSIXt = "string",
  factor = "string",
  complex = "string",
  null = "null",
  na = "null",
  auto_unbox = FALSE,
  digits = 5,
  pretty = TRUE,
  force = TRUE
)

write(js_database, file = "js_database.json")

js_database_r <- fromJSON("js_database.json")



export_MS2_to_database <- function() {
  
}




std$get_suspects(database = db, ppm = 5, sec = 10)
std$get_groups(mass = db, ppm = 5, sec = 10, average = TRUE)









ms$get_groups(mass = db, metadata = TRUE, intensities = T, average = T)

ms$plot_features_ms2(mass = db[3, ], colorBy = "analyses")


ms$get_suspects()





ms$as_patRoon_featureGroups(filtered = TRUE)



ms$plot_groups_overview(mass = db[2, ])

ms$plot_internal_standards_qc()

ms$get_groups_ms1(mass = db[2, ], groupBy = "replicates")


ms$get_features()

ms$get_suspects()





pat_sus <- ms$as_patRoon_featureGroups(addSuspects = T)
pat_mspl <- ms$as_patRoon_MSPeakLists()

pat_for <- patRoon::generateFormulasGenForm(
  pat_sus,
  pat_mspl,
  relMzDev = 5,
  adduct = "[M+H]+",
  elements = "CHNOPSCl",
  hetero = TRUE,
  oc = FALSE,
  thrMS = NULL,
  thrMSMS = NULL,
  thrComb = NULL,
  maxCandidates = Inf,
  extraOpts = NULL,
  calculateFeatures = TRUE,
  featThreshold = 0,
  featThresholdAnn = 0.75,
  absAlignMzDev = 0.002,
  MSMode = "both",
  isolatePrec = TRUE,
  timeout = 120,
  topMost = 50,
  batchSize = 8
)


patRoon::screenInfo(pat_sus)



# fg <- ms$as_patRoon_featureGroups(filtered = F)

# ssus <- Settings_suspect_screening_patRoon(suspects = db)
# .s3_ms_suspect_screening.Settings_suspect_screening_patRoon(ssus, ms)


ms$has_suspects()

View(ms$get_suspects(onGroups = T))

unset(fg, "positive")










ms$plot_features(mass = db, filtered = T, legendNames = T)

ms$plot_features(features = ms$get_suspects(onGroups = F), colorBy = "analyses+targets")

fts <- ms$get_features(mass = db, filtered = F)

ms$plot_internal_standards_qc()

# Suspects should be identified by name, formula and SMILES
suspects <- ms$get_suspects(database = db, ppm = 10, sec = 15, filtered = FALSE)

ms$get_features(mass = db)















# plotly::plot_ly() %>%
#   plotly::add_trace(y = pk_ints, type = 'scatter', name = 'Data', mode = 'markers', marker = list(color = "black")) %>%
#   plotly::add_trace(y = derivative01, type = 'scatter', name = '1s D', mode = 'lines', line = list(color = "blue")) %>%


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

# if (FALSE && loadedGroupsMS1 && self$has_loaded_groups_ms1()) {
# 
#   ms1_list <- lapply(seq_len(nrow(fgs)), function(x, fgs) {
#     temp <- fgs[x, ]
# 
#     temp_ms <- temp[["ms1"]][[1]]
# 
#     if (is.null(temp_ms)) return(data.table())
# 
#     temp_ms$group <- temp$group
# 
#     temp_ms
#   }, fgs = fgs)
# 
#   ms1 <- rbindlist(ms1_list, fill = TRUE)
# 
#   add_filtered <- FALSE
# 
#   if (any(fgs$filtered)) {
#     if (nrow(ms1) == 0) {
#       add_filtered <- TRUE
#     } else if (any(!(fgs$group[fgs$filtered] %in% ms1$group))) {
#       add_filtered <- TRUE
#     }
#   }
# 
#   if (add_filtered) {
#     fgs_filtered <- fgs[fgs$filtered, ]
#     fgs_filtered <- fgs_filtered[!(fgs_filtered$group %in% ms1$group), ]
# 
#     fts_filtered <- self$get_features(features = fgs_filtered$group, filtered = TRUE)
# 
#     if (nrow(fts_filtered) > 0) {
# 
#       settings_lf <- self$get_settings("load_features_ms1")[[1]]
# 
#       if (!is.null(settings_lf)) {
#         
#         parameters_lf <- settings_lf$parameters
#       
#         if ("rtWindow" %in% names(parameters_lf)) {
#           rtWindow <- parameters_lf[["rtWindow"]]
#         }
#         
#         if ("mzWindow" %in% names(parameters_lf)) {
#           mzWindow <- parameters_lf[["mzWindow"]]
#         }
#         
#         if ("mzClust" %in% names(parameters_lf)) {
#           mzClustFeatures <- parameters_lf[["mzClust"]]
#         }
#         
#         if ("presence" %in% names(parameters_lf)) {
#           presenceFeatures <- parameters_lf[["presence"]]
#         }
#         
#         if ("minIntensity" %in% names(parameters_lf)) {
#           minIntensityFeatures <- parameters_lf[["minIntensity"]]
#         }
#       }
# 
#       settings <- self$get_settings("load_groups_ms1")[[1]]
# 
#       if (!is.null(settings)) {
#         parameters <- settings$parameters
#       
#         if ("mzClust" %in% names(parameters)) {
#           mzClustGroups <- parameters[["mzClust"]]
#         }
#         
#         if ("presence" %in% names(parameters)) {
#           presenceGroups <- parameters[["presence"]]
#         }
#         
#         if ("minIntensity" %in% names(parameters)) {
#           minIntensityGroups <- parameters[["minIntensity"]]
#         }
#       }
# 
#       feat_ms1 <- self$get_features_ms1(
#         analyses = unique(fts_filtered$analysis),
#         features = fts_filtered$feature,
#         rtWindow = rtWindow,
#         mzWindow = mzWindow,
#         mzClust = mzClustFeatures,
#         presence = presenceFeatures,
#         minIntensity = minIntensityFeatures,
#         verbose = verbose,
#         filtered = filtered,
#         loadedMS1 = loadedFeaturesMS1,
#         runParallel = runParallel
#       )
# 
#       feat_ms1 <- feat_ms1[feat_ms1$intensity > minIntensityGroups, ]
# 
#       if (nrow(feat_ms1) > 0) {
#         polarities <- unique(self$get_polarities(analyses = unique(feat_ms1$analysis)))
#         multiple_polarities <- FALSE
# 
#         if (length(polarities) > 1) multiple_polarities <- TRUE
# 
#         if ("groups" %in% groupBy) {
# 
#           if (multiple_polarities) {
#             feat_ms1$unique_id <- paste0(feat_ms1$group, "_", feat_ms1$polarity)
#             feat_ms1$analysis <- NA_character_
# 
#           } else {
#             feat_ms1$unique_id <- feat_ms1$group
#             feat_ms1$analysis <- NA_character_
#           }
# 
#         } else {
#           rpls <- self$get_replicate_names()
#           feat_ms1$analysis <- rpls[feat_ms1$analysis]
# 
#           if (multiple_polarities) {
#             feat_ms1$unique_id <- paste0(feat_ms1$analysis, "_", feat_ms1$group, "", feat_ms1$polarity)
# 
#           } else {
#             feat_ms1$unique_id <- paste0(feat_ms1$analysis, "_", feat_ms1$group)
#           }
#         }
# 
#         feat_ms1$id <- feat_ms1$group
# 
#         ms1_2_list <- rcpp_ms_cluster_spectra(feat_ms1, mzClustGroups, presenceGroups, verbose)
# 
#         ms1_2 <- rbindlist(ms1_2_list, fill = TRUE)
# 
#         setnames(ms1_2, "id", "group")
# 
#         ms1 <- list(ms1, ms1_2)
# 
#         ms1 <- rbindlist(ms1, fill = TRUE)
#       }
#     }
#   }
# 
#   if (nrow(ms1) == 0) return(data.table())
# 
#   ms1 <- ms1[order(ms1$mz), ]
# 
#   ms1 <- ms1[order(ms1$group), ]
# 
#   if ("groups" %in% groupBy) {
#     ms1[["analysis"]] <- NULL
# 
#   } else {
#     ms1 <- ms1[order(ms1$analysis), ]
#     setnames(ms1, "analysis", "replicate")
#   }
# 
#   if ("name" %in% colnames(fgs)) {
#     tar_ids <- fgs$name
#     names(tar_ids) <- fgs$group
#     ms1$name <- tar_ids[ms1$group]
#   }
# 
#   return(copy(ms1))
# }

# if (loadedGroupsMS2 & self$has_loaded_groups_ms2()) {
#   ms2_list <- lapply(seq_len(nrow(fgs)), function(x, fgs) {
#     temp <- fgs[x, ]
# 
#     temp_ms <- temp[["ms2"]][[1]]
# 
#     if (is.null(temp_ms)) return(data.table())
# 
#     temp_ms$group <- temp$group
# 
#     temp_ms
#   }, fgs = fgs)
# 
#   ms2 <- rbindlist(ms2_list, fill = TRUE)
# 
#   add_filtered <- FALSE
# 
#   if (any(fgs$filtered)) {
#     if (nrow(ms2) == 0) {
#       add_filtered <- TRUE
#     } else if (any(!(fgs$group[fgs$filtered] %in% ms2$group))) {
#       add_filtered <- TRUE
#     }
#   }
# 
#   if (add_filtered) {
#     fgs_filtered <- fgs[fgs$filtered, ]
#     fgs_filtered <- fgs_filtered[!(fgs_filtered$group %in% ms2$group), ]
# 
#     fts_filtered <- self$get_features(features = fgs_filtered$group, filtered = TRUE)
# 
#     if (nrow(fts_filtered) > 0) {
# 
#       settings_lf <- self$get_settings("load_features_ms2")[[1]]
# 
#       if (!is.null(settings_lf)) {
#         
#         parameters_lf <- settings_lf$parameters
#      
#         if ("isolationWindow" %in% names(parameters_lf)) {
#           isolationWindow <- parameters_lf[["isolationWindow"]]
#         }
#         
#         if ("mzClust" %in% names(parameters_lf)) {
#           mzClustFeatures <- parameters_lf[["mzClust"]]
#         }
#         
#         if ("presence" %in% names(parameters_lf)) {
#           presenceFeatures <- parameters_lf[["presence"]]
#         }
#         
#         if ("minIntensity" %in% names(parameters_lf)) {
#           minIntensityFeatures <- parameters_lf[["minIntensity"]]
#         }
#       }
# 
#       settings <- self$get_settings("load_groups_ms2")[[1]]
# 
#       if (!is.null(settings)) {
#         
#         parameters <- settings$parameters
#       
#         if ("mzClust" %in% names(parameters)) {
#           mzClustGroups <- parameters[["mzClust"]]
#         }
#         
#         if ("presence" %in% names(parameters)) {
#           presenceGroups <- parameters[["presence"]]
#         }
#         
#         if ("minIntensity" %in% names(parameters)) {
#           minIntensityGroups <- parameters[["minIntensity"]]
#         }
#       }
# 
#       feat_ms2 <- self$get_features_ms2(
#         analyses = unique(fts_filtered$analysis),
#         features = fts_filtered$feature,
#         isolationWindow = isolationWindow,
#         mzClust = mzClustFeatures,
#         presence = presenceFeatures,
#         minIntensity = minIntensityFeatures,
#         verbose = verbose,
#         filtered = filtered,
#         loadedMS2 = loadedFeaturesMS2,
#         runParallel = runParallel
#       )
# 
#       feat_ms2 <- feat_ms2[feat_ms2$intensity > minIntensityGroups, ]
# 
#       if (nrow(feat_ms2) > 0) {
#         polarities <- unique(self$get_polarities(analyses = unique(feat_ms2$analysis)))
#         multiple_polarities <- FALSE
# 
#         if (length(polarities) > 1) multiple_polarities <- TRUE
# 
#         if ("groups" %in% groupBy) {
# 
#           if (multiple_polarities) {
#             feat_ms2$unique_id <- paste0(feat_ms2$group, "_", feat_ms2$polarity)
#             feat_ms2$analysis <- NA_character_
# 
#           } else {
#             feat_ms2$unique_id <- feat_ms2$group
#             feat_ms2$analysis <- NA_character_
#           }
# 
#         } else {
#           rpls <- self$get_replicate_names()
#           feat_ms2$analysis <- rpls[feat_ms2$analysis]
# 
#           if (multiple_polarities) {
#             feat_ms2$unique_id <- paste0(feat_ms2$analysis, "_", feat_ms2$group, "", feat_ms2$polarity)
# 
#           } else {
#             feat_ms2$unique_id <- paste0(feat_ms2$analysis, "_", feat_ms2$group)
#           }
#         }
# 
#         feat_ms2$id <- feat_ms2$group
# 
#         ms2_2_list <- rcpp_ms_cluster_spectra(feat_ms2, mzClustGroups, presenceGroups, verbose)
# 
#         ms2_2 <- rbindlist(ms2_2_list, fill = TRUE)
# 
#         setnames(ms2_2, "id", "group")
# 
#         ms2 <- list(ms2, ms2_2)
# 
#         ms2 <- rbindlist(ms2, fill = TRUE)
#       }
#     }
#   }
# 
#   if (nrow(ms2) == 0) return(data.table())
# 
#   ms2 <- ms2[order(ms2$mz), ]
# 
#   ms2 <- ms2[order(ms2$group), ]
# 
#   if ("groups" %in% groupBy) {
#     ms2[["analysis"]] <- NULL
# 
#   } else {
#     ms2 <- ms2[order(ms2$analysis), ]
#     setnames(ms2, "analysis", "replicate", skip_absent = TRUE)
#   }
# 
#   if ("name" %in% colnames(fgs)) {
#     tar_ids <- fgs$name
#     names(tar_ids) <- fgs$group
#     ms2$name <- tar_ids[ms2$group]
#   }
# 
#   return(copy(ms2))
# }

#' #' @title Settings_load_groups_ms1_StreamFind
#' #'
#' #' @description Settings for loading MS1 spectra for feature groups.
#' #'
#' #' @template arg-ms-mzClust
#' #' @template arg-ms-presence
#' #' @template arg-ms-minIntensity
#' #' @template arg-ms-filtered
#' #' @template arg-runParallel
#' #' @template arg-verbose
#' #'
#' #' @return A ProcessingSettings S3 class object with subclass
#' #' Settings_load_groups_ms1_StreamFind.
#' #'
#' #' @export
#' #'
#' Settings_load_groups_ms1_StreamFind <- function(
#'     mzClust = 0.003,
#'     presence = 0.6,
#'     minIntensity = 1000,
#'     verbose = FALSE,
#'     filtered = FALSE,
#'     runParallel = TRUE) {
#'   
#'   settings <- list(
#'     call = "load_groups_ms1",
#'     algorithm = "StreamFind",
#'     parameters = list(
#'       "mzClust" = mzClust,
#'       "presence" = presence,
#'       "minIntensity" = minIntensity,
#'       "filtered" = filtered,
#'       "runParallel" = runParallel,
#'       "verbose" = verbose
#'     ),
#'     version = as.character(packageVersion("StreamFind")),
#'     software = "StreamFind",
#'     developer = "Ricardo Cunha",
#'     contact = "cunha@iuta.de",
#'     link = "https://odea-project.github.io/StreamFind",
#'     doi = NA_character_
#'   )
#'   
#'   settings <- as.ProcessingSettings(settings)
#'   
#'   return(settings)
#' }
#' 
#' #' @describeIn Settings_load_groups_ms1_StreamFind
#' #' Validates the object structure, returning a logical value of length one.
#' #'
#' #' @param x A Settings_load_groups_ms1_StreamFind S3 class object.
#' #'
#' #' @export
#' #'
#' validate.Settings_load_groups_ms1_StreamFind <- function(x) {
#'   all(
#'     checkmate::test_choice(x$call, "load_groups_ms1"),
#'     checkmate::test_choice(x$algorithm, "StreamFind"),
#'     checkmate::test_number(x$parameters$mzClust),
#'     checkmate::test_number(x$parameters$minIntensity),
#'     checkmate::test_logical(x$parameters$filtered, max.len = 1),
#'     checkmate::test_logical(x$parameters$runParallel, max.len = 1),
#'     checkmate::test_logical(x$parameters$verbose, max.len = 1)
#'   )
#' }
#' 
#' #' @title Settings_load_groups_ms2_StreamFind
#' #'
#' #' @description Settings for loading MS2 spectra for feature groups.
#' #'
#' #' @template arg-ms-mzClust
#' #' @template arg-ms-presence
#' #' @template arg-ms-minIntensity
#' #' @template arg-ms-filtered
#' #' @template arg-runParallel
#' #' @template arg-verbose
#' #'
#' #' @return A ProcessingSettings S3 class object with subclass
#' #' Settings_load_groups_ms2_StreamFind.
#' #'
#' #' @export
#' #'
#' Settings_load_groups_ms2_StreamFind <- function(
#'     mzClust = 0.01,
#'     presence = 0.3,
#'     minIntensity = 250,
#'     filtered = FALSE,
#'     runParallel = TRUE,
#'     verbose = FALSE) {
#'   
#'   settings <- list(
#'     call = "load_groups_ms2",
#'     algorithm = "StreamFind",
#'     parameters = list(
#'       "mzClust" = mzClust,
#'       "presence" = presence,
#'       "minIntensity" = minIntensity,
#'       "filtered" = filtered,
#'       "runParallel" = runParallel,
#'       "verbose" = verbose
#'     ),
#'     version = as.character(packageVersion("StreamFind")),
#'     software = "StreamFind",
#'     developer = "Ricardo Cunha",
#'     contact = "cunha@iuta.de",
#'     link = "https://odea-project.github.io/StreamFind",
#'     doi = NA_character_
#'   )
#'   
#'   settings <- as.ProcessingSettings(settings)
#'   
#'   return(settings)
#' }
#' 
#' #' @describeIn Settings_load_groups_ms2_StreamFind
#' #' Validates the object structure, returning a logical value of length one.
#' #'
#' #' @param x A Settings_load_groups_ms2_StreamFind S3 class object.
#' #'
#' #' @export
#' #'
#' validate.Settings_load_groups_ms2_StreamFind <- function(x) {
#'   all(
#'     checkmate::test_choice(x$call, "load_groups_ms2"),
#'     checkmate::test_choice(x$algorithm, "StreamFind"),
#'     checkmate::test_number(x$parameters$mzClust),
#'     checkmate::test_number(x$parameters$minIntensity),
#'     checkmate::test_logical(x$parameters$filtered, max.len = 1),
#'     checkmate::test_logical(x$parameters$runParallel, max.len = 1),
#'     checkmate::test_logical(x$parameters$verbose, max.len = 1)
#'   )
#' }

#' #' @description
#' #' Loads and average MS1 spectra from feature groups. If MS1
#' #' spectra from features are already loaded, the feature MS1 spectra are
#' #' used for averaging into the respective feature group. If features MS1
#' #' spectra are not present, settings for loading and averaging features MS1
#' #' spectra (i.e., settings with call name "load_features_ms1") must
#' #' be given in the `settingsFeatures` argument or added beforehand with the
#' #' `add_settings` method.
#' #'
#' #' @return Invisible.
#' #'
#' load_groups_ms1 = function(settings = NULL, settingsFeatures = NULL) {
#'   
#'   if (!any(self$has_groups())) {
#'     warning("Feature groups not found! Not loaded.")
#'     return(invisible(self))
#'   }
#' 
#'   settings <- private$.get_call_settings(settings, "load_groups_ms1")
#'   
#'   if (is.null(settings)) return(invisible(self))
#' 
#'   algorithm <- settings$algorithm
#'   
#'   parameters <- settings$parameters
#' 
#'   if ("StreamFind" %in% algorithm) {
#' 
#'     cached_ms1 <- FALSE
#' 
#'     if (.caches_data()) {
#'       ana_feats <- self$get_features(filtered = TRUE)
#'       ana_feats <- ana_feats[, c("analysis", "feature"), with = FALSE]
#'       cols_to_hash <- c("group", "rt", "mass", "rtdev", "massdev")
#'       group_ids <- self$get_groups()
#'       group_ids <- group_ids[, cols_to_hash, with = FALSE]
#'       hash <- patRoon::makeHash(ana_feats, group_ids,  parameters)
#'       ms1 <- patRoon::loadCacheData("load_groups_ms1", hash)
#' 
#'       if (!is.null(ms1)) {
#'         if (all(ms1$group %in% group_ids$group)) {
#'           message("\U2139 Groups MS1 spectra loaded from cache!")
#'           cached_ms1 <- TRUE
#'         } else {
#'           ms1 <- NULL
#'         }
#'       } else {
#'         ms1 <- NULL
#'       }
#' 
#'     } else {
#'       hash <- NULL
#'       ms1 <- NULL
#'     }
#' 
#'     if (is.null(ms1)) {
#' 
#'       if (!any(self$has_loaded_features_ms1())) {
#'         self$load_features_ms1(settings = settingsFeatures)
#'       }
#' 
#'       if (any(self$has_loaded_features_ms1())) {
#'         ms1 <- self$get_groups_ms1(
#'           rtWindow = NULL,
#'           mzWindow = NULL,
#'           mzClustFeatures = NULL,
#'           minIntensityFeatures = NULL,
#'           loadedFeaturesMS1 = TRUE,
#'           groupBy = "groups",
#'           loadedGroupsMS1 = FALSE,
#'           mzClustGroups = parameters$mzClust,
#'           presenceGroups = parameters$presence,
#'           minIntensityGroups = parameters$minIntensity,
#'           verbose = parameters$verbose,
#'           filtered = parameters$filtered,
#'           runParallel = parameters$runParallel
#'         )
#'       } else {
#'         warning("Features MS1 are not presensent and could not be loaded!")
#'       }
#'     }
#' 
#'     if (nrow(ms1) > 0) {
#' 
#'       if (!cached_ms1 & !is.null(hash)) {
#'         message("\U1f5ab Groups MS1 spectra cached!")
#'         patRoon::saveCacheData("load_groups_ms1", ms1, hash)
#'       }
#' 
#'       ms1_groups_vector <- ms1$group
#'       ms1[["group"]] <- NULL
#'       ms1_list <- split(ms1, ms1_groups_vector)
#' 
#'       groups <- self$get_groups(filtered = TRUE)
#'       groups <- groups$group
#' 
#'       groups_ms1 <- lapply(groups, function(x, ms1_list) {
#'         temp <- ms1_list[[x]]
#'         temp
#'       }, ms1_list = ms1_list)
#' 
#'       private$.groups$ms1 <- groups_ms1
#' 
#'       if (requireNamespace(settings$software, quietly = TRUE)) {
#'         version <- as.character(packageVersion(settings$software))
#'       } else {
#'         version <- NA_character_
#'       }
#' 
#'       private$.register(
#'         "loaded",
#'         "feature groups",
#'         settings$call,
#'         settings$software,
#'         version,
#'         settings$algorithm
#'       )
#' 
#'       message("\U2713 MS1 spectra added to feature groups!")
#'       
#'       if (!private$.settings_already_stored(settings)) {
#'         self$add_settings(settings)
#'       }
#' 
#'     } else {
#'       warning("Mass traces were not found for feature groups!")
#'     }
#'   }
#'   invisible(self)
#' },
#' 
#' #' @description
#' #' Loads and average MS2 spectra from feature groups. If MS2
#' #' spectra from features are already loaded, the feature MS2 spectra are
#' #' used for averaging into the respective feature group. If features MS2
#' #' spectra are not present, settings for loading and averaging features MS2
#' #' spectra (i.e., settings with call name "load_features_ms2") must
#' #' be given in the `settingsFeatures` argument or added beforehand with the
#' #' `add_settings` method.
#' #'
#' #' @return Invisible.
#' #'
#' load_groups_ms2 = function(settings = NULL, settingsFeatures = NULL) {
#'   
#'   if (!any(self$has_groups())) {
#'     warning("Feature groups not found! Not loaded.")
#'     return(invisible(self))
#'   }
#' 
#'   settings <- private$.get_call_settings(settings, "load_groups_ms2")
#'   
#'   if (is.null(settings)) return(invisible(self))
#' 
#'   algorithm <- settings$algorithm
#'   
#'   parameters <- settings$parameters
#' 
#'   if ("StreamFind" %in% algorithm) {
#' 
#'     cached_ms2 <- FALSE
#' 
#'     if (.caches_data()) {
#'       ana_feats <- self$get_features(filtered = TRUE)
#'       ana_feats <- ana_feats[, c("analysis", "feature"), with = FALSE]
#'       cols_to_hash <- c("group", "rt", "mass", "rtdev", "massdev")
#'       group_ids <- self$get_groups()
#'       group_ids <- group_ids[, cols_to_hash, with = FALSE]
#'       hash <- patRoon::makeHash(ana_feats, group_ids,  parameters)
#'       ms2 <- patRoon::loadCacheData("load_groups_ms2", hash)
#' 
#'       if (!is.null(ms2)) {
#'         if (all(ms2$group %in% group_ids$group)) {
#'           message("\U2139 Groups MS2 spectra loaded from cache!")
#'           cached_ms2 <- TRUE
#'         } else {
#'           ms2 <- NULL
#'         }
#'       } else {
#'         ms2 <- NULL
#'       }
#' 
#'     } else {
#'       hash <- NULL
#'       ms2 <- NULL
#'     }
#' 
#'     if (is.null(ms2)) {
#' 
#'       if (!all(self$has_loaded_features_ms2())) {
#'         self$load_features_ms2(settings = settingsFeatures)
#'       }
#' 
#'       if (any(self$has_loaded_features_ms2())) {
#'         ms2 <- self$get_groups_ms2(
#'           isolationWindow = NULL,
#'           mzClustFeatures = NULL,
#'           presenceFeatures = NULL,
#'           minIntensityFeatures = NULL,
#'           loadedFeaturesMS2 = TRUE,
#'           groupBy = "groups",
#'           loadedGroupsMS2 = FALSE,
#'           mzClustGroups = parameters$mzClust,
#'           presenceGroups = parameters$presence,
#'           minIntensityGroups = parameters$minIntensity,
#'           verbose = parameters$verbose,
#'           filtered = parameters$filtered,
#'           runParallel = parameters$runParallel
#'         )
#'       } else {
#'         warning("Features MS2 are not presensent and could not be loaded!")
#'       }
#'     }
#' 
#'     if (nrow(ms2) > 0) {
#' 
#'       if (!cached_ms2 & !is.null(hash)) {
#'         message("\U1f5ab Groups MS2 spectra cached!")
#'         patRoon::saveCacheData("load_groups_ms2", ms2, hash)
#'       }
#' 
#'       ms2_groups_vector <- ms2$group
#'       ms2[["group"]] <- NULL
#'       ms2_list <- split(ms2, ms2_groups_vector)
#' 
#'       groups <- self$get_groups(filtered = TRUE)
#'       groups <- groups$group
#' 
#'       groups_ms2 <- lapply(groups, function(x, ms2_list) {
#'         temp <- ms2_list[[x]]
#'         temp
#'       }, ms2_list = ms2_list)
#' 
#'       private$.groups$ms2 <- groups_ms2
#' 
#'       if (requireNamespace(settings$software, quietly = TRUE)) {
#'         version <- as.character(packageVersion(settings$software))
#'       } else {
#'         version <- NA_character_
#'       }
#' 
#'       private$.register(
#'         "loaded",
#'         "feature groups",
#'         settings$call,
#'         settings$software,
#'         version,
#'         settings$algorithm
#'       )
#' 
#'       message("\U2713 MS2 spectra added to feature groups!")
#'       
#'       if (!private$.settings_already_stored(settings)) {
#'         self$add_settings(settings)
#'       }
#' 
#'     } else {
#'       warning("Mass traces were not found for feature groups!")
#'     }
#'   }
#'   invisible(self)
#' },

#' #' @description
#' #' Removes loaded MS1 spectra from feature groups. In practice, the column
#' #' \emph{ms1} in the groups data.table is removed.
#' #'
#' #' @return Invisible.
#' #'
#' remove_groups_ms1 = function() {
#'   if (self$has_groups()) {
#'     if (any(self$has_loaded_groups_ms1())) {
#'       private$.groups$ms1 <- NULL
#' 
#'       private$.register(
#'         "removed",
#'         "feature_groups_ms1",
#'         "all",
#'         NA_character_,
#'         NA_character_,
#'         NA_character_
#'       )
#' 
#'       message("\U2713 Removed all MS1 spectra from feature groups!")
#' 
#'     } else {
#'       message("\U2717 Groups MS1 spectra not loaded!")
#'     }
#'   } else {
#'     message("\U2717 Groups not present!")
#'   }
#'   invisible(self)
#' },
#' 
#' #' @description
#' #' Removes loaded MS2 spectra from feature groups. In practice, the column
#' #' \emph{ms2} in the groups data.table is removed.
#' #'
#' #' @return Invisible.
#' #'
#' remove_groups_ms2 = function() {
#'   if (self$has_groups()) {
#'     if (any(self$has_loaded_groups_ms2())) {
#'       private$.groups$ms2 <- NULL
#' 
#'       private$.register(
#'         "removed",
#'         "feature_groups_ms2",
#'         "all",
#'         NA_character_,
#'         NA_character_,
#'         NA_character_
#'       )
#' 
#'       message("\U2713 Removed all MS2 spectra from feature groups!")
#' 
#'     } else {
#'       message("\U2717 Groups MS2 spectra not loaded!")
#'     }
#'   } else {
#'     message("\U2717 Groups not present!")
#'   }
#'   invisible(self)
#' },

#' #' @description
#' #' Checks for loaded feature groups MS1.
#' #'
#' #' @return Logical value.
#' #'
#' has_loaded_groups_ms1 = function() {
#'   has_loaded_ms1 <- FALSE
#'   
#'   if (self$has_groups()) {
#'     groups <- self$get_groups()
#'     if ("ms1" %in% colnames(groups)) {
#'       has_loaded_ms1 <- any(vapply(groups$ms1, is.data.frame, FALSE))
#'     }
#'   }
#'   has_loaded_ms1
#' }
#' 
#' #' @description
#' #' Checks for loaded feature groups MS2.
#' #'
#' #' @return Logical value.
#' #'
#' has_loaded_groups_ms2 = function() {
#'   has_loaded_ms2 <- FALSE
#'   
#'   if (self$has_groups()) {
#'     groups <- self$get_groups()
#'     if ("ms2" %in% colnames(groups)) {
#'       has_loaded_ms2 <- any(vapply(groups$ms2, is.data.frame, FALSE))
#'     }
#'   }
#'   has_loaded_ms2
#' }

# // [[Rcpp::export]]
# Rcpp::List rcpp_ms_groups_make_dataframe(Rcpp::DataFrame features,
#                                          Rcpp::CharacterVector analyses,
#                                          bool mzAsMass = true,
#                                          bool newGroupNames = true) {
#   
#   Rcpp::List list_out;
#   
#   std::vector<std::string> features_cols = features.names();
#   
#   if (features.nrows() == 0 || features_cols.size() == 0) {
#     throw std::runtime_error("Features DataFrame is empty!");
#   }
#   
#   std::vector<std::string> must_have_names = {
#     "feature", "analysis", "rt", "rtmax", "rtmin",
#     "mz", "mzmax", "mzmin", "polarity", "intensity", "group"
#   };
#   
#   std::vector<bool> has_must_have_names(11, false);
#   
#   for (size_t i = 0; i < must_have_names.size(); ++i) {
#     for (size_t j = 0; j < features_cols.size(); ++j) {
#       if (must_have_names[i] == features_cols[j]) has_must_have_names[i] = true;
#     }
#   }
#   
#   for (bool value : has_must_have_names) {
#     if (!value) {
#       throw std::runtime_error("The DataFrame does not have all required columns!");
#     }
#   }
#   
#   const std::vector<std::string>& all_groups = features["group"];
#   
#   std::set<std::string> unique_groups_set;
#   
#   for (const std::string& g : all_groups) {
#     if(!(g.empty() || g == "NA")) unique_groups_set.insert(g);
#   }
#   
#   std::vector<std::string> unique_groups(unique_groups_set.begin(), unique_groups_set.end());
#   
#   std::sort(unique_groups.begin(), unique_groups.end());
#   
#   // list_out["unique_groups"] = unique_groups;
#   
#   const std::vector<std::string>& all_analysis = features["analysis"];
#   const std::string* all_analysis_ptr = all_analysis.data();
#   
#   std::set<std::string> unique_analysis_set;
#   
#   for (const std::string& a : all_analysis) {
#     unique_analysis_set.insert(a);
#   }
#   
#   std::vector<std::string> unique_analysis(unique_analysis_set.begin(), unique_analysis_set.end());
#   
#   // list_out["unique_analysis"] = unique_analysis;
#   
#   const std::vector<double>& all_rt = features["rt"];
#   const double* all_rt_ptr = all_rt.data();
#   
#   const std::vector<double>& all_rtmin = features["rtmin"];
#   const double* all_rtmin_ptr = all_rtmin.data();
#   
#   const std::vector<double>& all_rtmax = features["rtmax"];
#   const double* all_rtmax_ptr = all_rtmax.data();
#   
#   const std::vector<double>& all_mz = features["mz"];
#   const double* all_mz_ptr = all_mz.data();
#   
#   const std::vector<double>& all_mzmin = features["mzmin"];
#   const double* all_mzmin_ptr = all_mzmin.data();
#   
#   const std::vector<double>& all_mzmax = features["mzmax"];
#   const double* all_mzmax_ptr = all_mzmax.data();
#   
#   const std::vector<double>& all_intensity = features["intensity"];
#   const double* all_intensity_ptr = all_intensity.data();
#   
#   const std::vector<int>& all_polarity = features["polarity"];
#   const int* all_polarity_ptr = all_polarity.data();
#   
#   const int number_of_groups = unique_groups.size();
#   
#   const int number_of_features = features.nrows();
#   
#   int number_of_analysis = unique_analysis.size();
#   
#   int number_of_analyses_arg = analyses.size();
#   
#   if (number_of_analysis < number_of_analyses_arg) {
#     number_of_analysis = number_of_analyses_arg;
#     unique_analysis = Rcpp::as<std::vector<std::string>>(analyses);
#   }
#   
#   std::sort(unique_analysis.begin(), unique_analysis.end());
#   
#   Rcpp::NumericMatrix ints(number_of_groups, number_of_analysis);
#   Rcpp::CharacterVector colnames_analysis(unique_analysis.begin(), unique_analysis.end());
#   Rcpp::colnames(ints) = colnames_analysis;
#   
#   std::vector<double> g_rt(number_of_groups);
#   std::vector<double> g_rtmin(number_of_groups);
#   std::vector<double> g_rtmax(number_of_groups);
#   std::vector<double> g_rtdev(number_of_groups);
#   std::vector<double> g_mz(number_of_groups);
#   std::vector<double> g_mass(number_of_groups);
#   std::vector<double> g_massmin(number_of_groups);
#   std::vector<double> g_massmax(number_of_groups);
#   std::vector<double> g_massdev(number_of_groups);
#   std::vector<int> g_polarity(number_of_groups);
#   std::vector<std::string> g_group(number_of_groups);
#   
#   std::string* g_id;
#   
#   for (int i=0; i<number_of_groups; ++i) {
#     
#     g_id = &unique_groups[i];
#     
#     std::vector<int> which_idx;
#     
#     for (int z=0; z<number_of_features; z++) {
#       if (all_groups[z] == *g_id) which_idx.push_back(z);
#     }
#     
#     const int n_idx = which_idx.size();
#     
#     std::vector<std::string> analysis(n_idx);
#     std::string* analysis_ptr = analysis.data();
#     
#     std::vector<double> rt(n_idx);
#     double* rt_ptr = rt.data();
#     
#     std::vector<double> rtmin(n_idx);
#     double* rtmin_ptr = rtmin.data();
#     
#     std::vector<double> rtmax(n_idx);
#     double* rtmax_ptr = rtmax.data();
#     
#     std::vector<double> mass(n_idx);
#     double* mass_ptr = mass.data();
#     
#     std::vector<double> massmin(n_idx);
#     double* massmin_ptr = massmin.data();
#     
#     std::vector<double> massmax(n_idx);
#     double* massmax_ptr = massmax.data();
#     
#     std::vector<double> intensity(n_idx);
#     double* intensity_ptr = intensity.data();
#     
#     for (const int& x : which_idx) {
#       *(analysis_ptr++) = *(all_analysis_ptr + x);
#       
#       *(rt_ptr++) = *(all_rt_ptr + x);
#       *(rtmin_ptr++) = *(all_rtmin_ptr + x);
#       *(rtmax_ptr++) = *(all_rtmax_ptr + x);
#       
#       if (mzAsMass) {
#         
#         *(mass_ptr++) = *(all_mz_ptr + x);
#         *(massmin_ptr++) = *(all_mzmin_ptr + x);
#         *(massmax_ptr++) = *(all_mzmax_ptr + x);
#         
#       } else {
#         
#         double mass_val;
#         
#         if (*(all_polarity_ptr + x) == 1) {
#           mass_val = *(all_mz_ptr + x) - 1.00726;
#           
#         } else if (*(all_polarity_ptr + x) == -1) {
#           mass_val = *(all_mz_ptr + x) + 1.00726;
#           
#         } else {
#           mass_val = *(all_mz_ptr + x);
#         }
#         
#         *(mass_ptr++) = mass_val;
#         
#         *(massmin_ptr++) = mass_val - (*(all_mz_ptr + x) - *(all_mzmin_ptr + x));
#         
#         *(massmax_ptr++) = mass_val + (*(all_mzmax_ptr + x) - *(all_mz_ptr + x));
#       }
#       
#       *(intensity_ptr++) = *(all_intensity_ptr + x);
#     }
#     
#     // list_out["analysis"] = analysis;
#     // list_out["feature"] = feature;
#     // list_out["rt"] = rt;
#     // list_out["mz"] = mz;
#     
#     std::set<std::string> analysis_present_set;
#     
#     for (const std::string& a : analysis) {
#       analysis_present_set.insert(a);
#     }
#     
#     std::vector<std::string> analysis_present(analysis_present_set.begin(), analysis_present_set.end());
#     
#     int n_analysis_present = analysis_present.size();
#     
#     if (n_idx > n_analysis_present) {
#       Rcpp::Rcout << "\n !! The feature group " << *g_id << " (n " << i << ")"
#       << " has more features than analyses! \n";
#     }
#     
#     double mean_rt = 0.0;
#     
#     for (double r : rt) mean_rt += r;
#     
#     g_rt[i] = mean_rt / n_idx;
#     
#     g_rt[i] = std::round(g_rt[i] / 0.001) * 0.001;
#     
#     auto rtmin_val = std::min_element(rtmin.begin(), rtmin.end());
#     auto rtmax_val = std::max_element(rtmax.begin(), rtmax.end());
#     
#     if (g_rt[i] > *rtmax_val || g_rt[i] < *rtmin_val) {
#       Rcpp::Rcout << "\n !! The feature group " << g_id << " (n " << i << ")"
#       << " does not match retention time range in features!! \n";
#     }
#     
#     g_rtdev[i] = *rtmax_val - *rtmin_val;
#     
#     g_rtdev[i] = std::round(g_rtdev[i] / 1) * 1;
#     
#     g_rtmin[i] = std::round(*rtmin_val / 0.001) * 0.001;
#     
#     g_rtmax[i] = std::round(*rtmax_val / 0.001) * 0.001;
#     
#     double mean_mass = 0.0;
#     
#     for (double m : mass) mean_mass += m;
#     
#     g_mass[i] = mean_mass / n_idx;
#     
#     g_mass[i] = std::round(g_mass[i] / 0.00000001) * 0.00000001;
#     
#     auto massmin_val = std::min_element(massmin.begin(), massmin.end());
#     auto massmax_val = std::max_element(massmax.begin(), massmax.end());
#     
#     if (g_mass[i] > *massmax_val || g_mass[i] < *massmin_val) {
#       Rcpp::Rcout << "\n !! The feature group " << g_id << " (n " << i << ")"
#       << " does not match mass range in features!! \n";
#     }
#     
#     g_massdev[i] = (*massmax_val - *massmin_val) / *massmax_val * 1E6;
#     
#     g_massdev[i] = std::round(g_massdev[i] / 0.01) * 0.01;
#     
#     if (newGroupNames) {
#       
#       double rounded_mass = std::round(g_mass[i] / 0.01) * 0.01;
#       
#       std::ostringstream mass_ostr;
#       
#       mass_ostr << std::fixed << std::setprecision(3) << rounded_mass;
#       
#       std::string g_mass_to_id = mass_ostr.str();
#       
#       double rounded_rt = std::round(g_rt[i] / 1) * 1;
#       
#       std::ostringstream rt_ostr;
#       
#       rt_ostr << std::fixed << std::setprecision(0) << rounded_rt;
#       
#       std::string g_rt_to_id = rt_ostr.str();
#       
#       std::string i_str = std::to_string(i + 1);
#       
#       g_group[i] = "m";
#       g_group[i] += g_mass_to_id;
#       g_group[i] += "_rt";
#       g_group[i] += g_rt_to_id;
#       g_group[i] += "_g";
#       g_group[i] += i_str;
#       
#     } else {
#       g_group[i] = unique_groups[i];
#     }
#     
#     // list_out["group"] = g_group[0];
#     
#     for (int f = 0; f < n_idx; ++f) {
#       for (int a=0; a < number_of_analysis; ++a) {
#         if (colnames_analysis[a] == analysis[f]) {
#           ints(i, a) = intensity[f];
#         }
#       }
#     }
#   }
#   
#   Rcpp::List list_groups;
#   
#   if (newGroupNames) list_groups["old_group"] = unique_groups;
#   
#   list_groups["group"] = g_group;
#   
#   list_groups["mass"] = g_mass;
#   
#   list_groups["rt"] = g_rt;
#   
#   for (int a = 0; a < number_of_analysis; ++a) {
#     Rcpp::NumericVector int_vals = ints( Rcpp::_ , a );
#     Rcpp::String ana = colnames_analysis[a];
#     list_groups.push_back(int_vals, ana);
#   }
#   
#   list_groups["massdev"] = g_massdev;
#   
#   list_groups["rtdev"] = g_rtdev;
#   
#   list_groups["rtmin"] = g_rtmin;
#   
#   list_groups["rtmax"] = g_rtmax;
#   
#   std::vector<bool> filtered(number_of_groups, false);
#   
#   list_groups["filtered"] = filtered;
#   
#   SEXP na = R_NaString;
#   
#   Rcpp::CharacterVector filter(number_of_groups, na);
#   
#   list_groups["filter"] = filter;
#   
#   list_groups.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
#   
#   std::vector<int> rows(g_group.size());
#   
#   std::iota(rows.begin(), rows.end(), 1);
#   
#   list_groups.attr("row.names") = rows;
#   
#   return(list_groups);
# }

#' #' @description
#' #' Adds feature groups. Note that existing features groups are
#' #' replaced!
#' #'
#' #' @param groups A data.table with feature groups from correspondence across
#' #' MS analyses as obtained by the method `get_groups()`. Note that
#' #' correspondence of features across MS analyses is performed with the
#' #' method `group_features()`.
#' #' @param feature_groups A data.table with columns analysis, feature and group.
#' #'
#' #' @return Invisible.
#' #'
#' add_groups = function(groups = NULL, feature_groups = NULL) {
#'   
#'   if (!is.null(feature_groups)) self$add_group_to_features(feature_groups)
#'   
#'   if (is.list(groups) & !is.data.frame(groups)) {
#'     if ("ms1" %in% names(groups)) {
#'       groups$ms1 <- lapply(groups$ms1, as.data.table)
#'     }
#' 
#'     if ("ms2" %in% names(groups)) {
#'       groups$ms2 <- lapply(groups$ms2, as.data.table)
#'     }
#' 
#'     groups <- as.data.table(groups)
#' 
#'   } else if (is.data.frame(groups)) {
#' 
#'     if ("ms1" %in% colnames(groups)) {
#'       groups$ms1 <- lapply(groups$ms1, as.data.table)
#'     }
#' 
#'     if ("ms2" %in% colnames(groups)) {
#'       groups$ms2 <- lapply(groups$ms2, as.data.table)
#'     }
#'   }
#' 
#'   if (is.data.frame(groups)) {
#'     must_have_cols <- c(
#'       "group", "mass", "rt", "rtdev", "massdev", "filtered",
#'       unname(self$get_analysis_names())
#'     )
#' 
#'     if (all(must_have_cols %in% colnames(groups))) {
#'       old_groups <- private$.groups
#'       private$.groups <- copy(groups)
#' 
#'       if (!self$check_correspondence()) {
#'         warning("Correspondence did not match! Groups not added.")
#'         private$.groups <- old_groups
#' 
#'       } else {
#'         private$.register(
#'           "added",
#'           "analyses",
#'           "feature groups",
#'           NA_character_,
#'           NA_character_,
#'           nrow(groups)
#'         )
#' 
#'         message(paste0("\U2713 ", nrow(groups), " feature groups added!"))
#'       }
#' 
#'     } else {
#'       warning("Columns of groups data.frame not as required! Not added.")
#'     }
#' 
#'   } else {
#'     warning("Groups must be a data.frame! Not added.")
#'   }
#'   invisible(self)
#' },

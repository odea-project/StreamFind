
# resources --------------------------------------------------------------------

## files -----------------------------------------------------------------------
all_files <- StreamFindData::get_ms_file_paths()
files <- all_files[grepl("mrm", all_files)]
files <- all_files[1:3]
files <- all_files[grepl("influent|blank", all_files)]
files <- all_files[grepl("o3sw", all_files)]



# path <- "C:/Users/apoli/Documents/example_ms_files"
# files <- list.files(path, pattern = ".mzML", full.names = TRUE)

## databases -------------------------------------------------------------------
db <- StreamFindData::get_ms_tof_spiked_chemicals()

db_cols <- c("name", "mass", "rt")
carbamazepin_d10 <- db[db$name %in% "Carbamazepin-D10", db_cols, with = FALSE]
diuron_d6 <- db[db$name %in% "Diuron-D6", db_cols, with = FALSE]
carb_pos <- carbamazepin_d10$mass + 1.007276
carb <- carbamazepin_d10$mass
carb_rt <- carbamazepin_d10$rt
diu_pos <- diuron_d6$mass + 1.007276
diu <- diuron_d6$mass
diu_rt <- diuron_d6$rt

sec_dev <- 30
ppm_dev <- 10

targets <- make_ms_targets(
  mz = data.frame(
    id = c("tg1", "tg2"),
    mz = c(carb_pos, diu_pos),
    rt = c(carb_rt, diu_rt)
  ),
  ppm = ppm_dev,
  sec = sec_dev
)

neutral_targets <- make_ms_targets(
  mz = data.frame(
    name = c("tg1", "tg2"),
    mz = c(carb, diu),
    rt = c(carb_rt, diu_rt)
  ),
  ppm = ppm_dev,
  sec = sec_dev
)

cols <- c("name", "formula", "mass", "rt")

# tof_db <- paste0(path, "/qc_MS2_pos.csv")
# tof_db <- data.table::fread(tof_db)
# tof_db <- tof_db[, cols, with = FALSE]

# afin_db <- paste0(path, "/Composition_Mix-Fusion.csv")
# afin_db <- data.table::fread(afin_db)
# afin_db <- afin_db[, cols, with = FALSE]

# ude_db <- paste0(path, "/mix1_orbitrap_ude.csv")
# ude_db <- data.table::fread(ude_db)
# ude_db <- ude_db[, cols, with = FALSE]

## settings --------------------------------------------------------------------

# settings <- list(
#   Settings_find_features_xcms3_centwave(),
#   Settings_group_features_xcms3_peakdensity(),
#   Settings_filter_features_StreamFind(
#     minIntensity = 5000,
#     minSnRatio = 20,
#     maxGroupSd = 30,
#     blank = 5,
#     minGroupAbundance = 3,
#     excludeIsotopes = TRUE
#   ),
#   Settings_load_features_ms1_StreamFind(),
#   Settings_load_features_ms2_StreamFind()
#   
# )

# cached -----------------------------------------------------------------------

# patRoon::clearCache("parsed_ms_analyses")
# patRoon::clearCache("parsed_ms_spectra")
# patRoon::clearCache("load_features_ms1")
# patRoon::clearCache("load_features_ms2")
# patRoon::clearCache("load_groups_ms1")
# patRoon::clearCache("load_groups_ms2")

# clear_cache("all")

# StreamCraft interface --------------------------------------------------------

# make a benchmark with rcpp_parse_msAnalysis(files) vs rcpp_parse_ms_analysis_v2(files) and compare the results

# testSpeed <- microbenchmark::microbenchmark(
#   rcpp_parse_ms_analysis(files[1]),
#   rcpp_parse_ms_analysis_v2(files[1]),
#   times = 3
# )
# 
# testSpeed <- microbenchmark::microbenchmark(
#   rcpp_parse_spectra_headers(files[1]),
#   rcpp_parse_ms_spectra_headers_v2(files[1]),
#   times = 3
# )

# rcpp_parse_ms_analysis(ms_files[1])




# spectra ----------------------------------------------------------------------
# clear_cache("all")


ms <- MassSpecEngine$new(files = files) #files[13:18]
# ms$find_features(Settings_find_features_openms())
ms$save(paste0(getwd(), "/ms.sqlite"))
ms$run_app()


ms$plot_spectra_tic(levels = 1, colorBy = "polarities")

rfiles <- StreamFindData::get_raman_file_paths()
raman <- RamanEngine$new(files = rfiles)
raman$save(paste0(getwd(), "/raman.sqlite"))
raman$run_app()


View(ms$get_spectra_headers()[ms$get_spectra_headers()$polarity != 0, ])

ms$remove_analyses(1)

ms$save()

ms$load()


file <- file.choose()
clear_cache("all")
ms <- MassSpecEngine$new(files = file)
ms$plot_spectra_bpc(levels = 1)

ms$get_spectra_mode()

tar <- data.frame(
  mzmin = c(268.1912 - 0.01, 237.1022 - 0.01, 130.1087 - 0.01),
  mzmax = c(268.1912 + 0.01, 237.1022 + 0.01, 130.1087 + 0.01),
  polarity = c(1, 1, 1)
)

ms$plot_spectra(mz = tar, level = 1, colorBy = "targets")




ms$plot_spectra_eic(mz = carb, ppm = 10, sec = 15, colorBy = "analyses")

ms$get_spectra_mode()

ms$get_spectra_polarity()

ms$get_spectra_headers()

ms$get_spectra_tic()

ms$plot_spectra_bpc(levels = 1, colorBy = "replicates")

ms$find_features()

ms$group_features()

tar_groups <- ms$get_groups(mass = neutral_targets)

ms2 <- ms$subset_groups(groups = unique(tar_groups$group))




pat_features <- ms$as_patRoon_features()

pat_fgroups <- ms$as_patRoon_featureGroups()



pat





diu_fts <- ms$get_features(mass = diu, rt = diu_rt, ppm = 10, sec = 10)

# ms$remove_analyses(c(1, 6))





View(ms$get_groups())

View(ms$get_features())




View(ms$get_features())



fl <- all_files[30]
srm <- MassSpecData$new(files = fl)
srm$plot_chromatograms(interactive = FALSE)


write.csv(srm$get_chromatograms(), "chromatograms.csv", row.names = FALSE)

.plot_chromatograms_interactive(chromatograms)




ms$plot_spectra_eic(mass = diu, rt = diu_rt, ppm = 5, sec = 10)

# ms$plot_spectra_bpc(levels = 1, colorBy = "analyses", interactive = F)

ms$plot_spectra_eic(mass = afin_db$mass[66], colorBy = "targets")

spec <- ms$get_spectra(mass = afin_db$mass[66], rt = 358, ppm = 3, sec = 10, levels = 1, allTraces = FALSE)
spec$unique_id <- paste0(spec$analysis, "_", spec$id, "_", spec$polarity)
spec

spec <- ms$get_spectra(mass = diu, rt = diu_rt, ppm = 3, sec = 10, levels = 2, allTraces = FALSE)
spec$unique_id <- paste0(spec$analysis, "_", spec$id, "_", spec$polarity)
spec

rcpp_ms_cluster_spectra(spec, mzClust = 0.001, presence = 0.8, verbose = TRUE)








# ms$get_spectra_ms2()

rcpp_parse_ms_analysis_spectra(ms$get_analyses()[[1]])


ms$get_run()

ms$get_spectra_polarity()

ms$get_spectra(mass = afin_db$mass[2])



ms$plot_spectra_eic(mz = afin_db$mass[2] + 1.00726, colorBy = "targets")

ms$get_spectra_ms2(mass = afin_db$mass[2])

ms$plot_spectra_bpc(levels = 1, colorBy = "analyses")







ms$get_spectra(analyses = c(2, 5), mass = diu, rt = diu_rt)







ms$get_spectra(analyses = c(2, 5), mass = diu, rt = diu_rt, sec = 120, levels = 2, allTraces = FALSE)

# ms$plot_spectra(mass = diu, rt = diu_rt, colorBy = "analyses")

# ms$plot_spectra_ms2(analyses = c(2, 5), mass = diu, rt = diu_rt, colorBy = "targets", interactive = T)

ms$plot_spectra_ms1(analyses = c(2, 5), mass = diu, rt = diu_rt, interactive = F)

ms$get_spectra_ms1(analyses = c(2, 5), mass = diu, rt = diu_rt)

ms$get_spectra_tic()

ms$plot_spectra_tic(levels = 1, colorBy = "polarities", interactive = F)

ms$plot_spectra_eic(mass = neutral_targets, colorBy = "targets", interactive = F, legendNames = TRUE)

#ms$get_spectra_eic(mass = diu)

#ms$get_spectra_eic(mz = diu_pos)






# fl <- choose.files()

rcpp_parse_ms_analysis(fl)

rcpp_parse_spectra_headers(fl)

rcpp_parse_ms_analysis_spectra(ms$get_analyses(1)[[1]])

rcpp_parse_spectra_headers(files2[1])

rcpp_parse_spectra(files2[1])











ms <- MassSpecData$new(files = all_files[10:21],
  headers = list(name = "Example 1"),
  settings = list(
    find = settings_ff,
    group = settings_gf,
    ms1ft = settingsSettings_load_features_ms1_StreamFind,
    ms2ft = settingsSettings_load_features_ms2_StreamFind,
    ms1gp = settingsSettings_load_groups_ms1_StreamFind,
    ms2gp = settingsSettings_load_groups_ms2_StreamFind
  )
)

ana <- ms$get_analyses(1)
ana1 <- parse_MassSpecAnalysis(all_files[10])
all.equal(ana, ana1)

ms1 <- ms$subset_analyses(4)
ms1$load_spectra()

spec <- ms1$get_spectra()
spec <- spec[spec$level == 1, ]
write.csv(spec, "./ms_spectra.csv")

ms1$find_features()
feat <- ms1$get_features()
write.csv(feat, "./ms_features.csv")

ps <- as.ProcessingSettings(settings_ff)

asJSON(ps)

jsonlite::toJSON(Headers(name = "Example", description = "test"))



ms$get_settings("find_features")

ms$plot_spectra(analyses = 1, mz = 239, ppm = 500)

ms1 <- ms1$subset_analyses(1)

ms1$load_spectra()

ms1$get_analyses()[[1]][["spectra"]]


# do.call(ms[["get_analysis_names"]], list(analyses = 4:6))




# temp -------------------------------------------------------------------------


input <- c(1.1, 2.1, 3.31641646564465464, 1000, 314)
rcpp_parse_xml(input)







ms <- MassSpecData$new()
ms$get_headers()



ms$get_number_analyses()





# tests ------------------------------------------------------------------------

all_files <- StreamFindData::get_ms_file_paths()
big_file_test <- "E:\\Dev_20230126_IonMobilityDataFirstTraining\\WorklistData-0001.mzML"
big_file_test <- "E:\\20230126_DA_EDA_background_evaluation\\221118_DA-EDA_solid phase background_centrifuged\\mzml\\02_QC_pos-r001.mzML"
big_file_test <- all_files[8]

## headers
init <- Sys.time()
msz <- mzR::openMSfile(big_file_test)
msz3 <- mzR::header(msz)
mzR::close(msz)
Sys.time() - init

init <- Sys.time()
ana <- rcpp_parse_msAnalysis(big_file_test)
Sys.time() - init

init <- Sys.time()
ana_withR <- parse_msAnalysis(big_file_test)
Sys.time() - init



## spectra (list)
init <- Sys.time()
msz <- mzR::openMSfile(big_file_test)
msz4 <- mzR::peaks(msz)
mzR::close(msz)
Sys.time() - init

init <- Sys.time()
spectra <- rcpp_parse_spectra(big_file_test)
Sys.time() - init

analysis <- rcpp_parse_msAnalysis(big_file_test)
init <- Sys.time()
spectra_analysis <- rcpp_parse_msAnalysis_spectra(analysis)
Sys.time() - init


## create msAnalysis
ana_mzml_neg <- rcpp_parse_msAnalysis(all_files[11])
validate_msAnalysis(ana_mzml_neg)
ana_mzml_mrm_neg <- rcpp_parse_msAnalysis(all_files[28])
validate_msAnalysis(ana_mzml_mrm_neg)
ana_mzxml_pos <- rcpp_parse_msAnalysis(all_files[4])
validate_msAnalysis(ana_mzxml_pos)
ana_mzml_prof_pos <- rcpp_parse_msAnalysis(all_files[7])
validate_msAnalysis(ana_mzml_prof_pos)
ana_mzml_orb_pos <- rcpp_parse_msAnalysis(all_files[31])
validate_msAnalysis(ana_mzml_orb_pos)
ana_mzxml_orb_pos <- rcpp_parse_msAnalysis(all_files[32])
validate_msAnalysis(ana_mzxml_orb_pos)

is.data.table(rcpp_parse_msAnalysis_spectra(ana_mzml_neg))
is.data.table(rcpp_parse_msAnalysis_spectra(ana_mzml_mrm_neg)) # empty for mrm without spectra
is.data.table(rcpp_parse_msAnalysis_spectra(ana_mzxml_pos))
is.data.table(rcpp_parse_msAnalysis_spectra(ana_mzml_prof_pos))
is.data.table(rcpp_parse_msAnalysis_spectra(ana_mzml_orb_pos))
is.data.table(rcpp_parse_msAnalysis_spectra(ana_mzxml_orb_pos))

is.data.table(rcpp_parse_spectra_headers(all_files[1])) # mzML MS/MS

is.data.table(rcpp_parse_chromatograms_headers(all_files[1])) # mzML MS/MS empty for XML without chromatograms
is.data.table(rcpp_parse_chromatograms_headers(all_files[28])) # mzML SRM
is.data.table(rcpp_parse_chromatograms_headers(all_files[4])) # mzXML empty for XML without chromatograms

ms_chrom <- MassSpecData$new(files = all_files[29])

unique(ms_chrom$get_chromatograms(index = c(1, 2))$index)
unique(ms_chrom$get_chromatograms()$index)

ms_chrom$has_loaded_chromatograms()
ms_chrom$load_chromatograms()

all_files <- StreamFindData::get_ms_file_paths()

rcpp_parse_spectra(all_files[1], index = c(1, 2))
rcpp_parse_spectra(all_files[1], index = c(2, 1))

length(rcpp_parse_spectra(all_files[1]))

ana <- rcpp_parse_msAnalysis(all_files[1])
rcpp_parse_msAnalysis_spectra(ana, index = c(1, 2))
rcpp_parse_msAnalysis_spectra(ana, index = c(2, 1))

nrow(rcpp_parse_msAnalysis_spectra(ana))

rcpp_parse_chromatograms_headers(all_files[29])

View(rcpp_parse_chromatograms(all_files[29], index = c(1, 2)))
View(rcpp_parse_chromatograms(all_files[29], index = c(2, 1)))

ana <- rcpp_parse_msAnalysis(all_files[29])
rcpp_parse_msAnalysis_chromatograms(ana, index = c(1, 2))
rcpp_parse_msAnalysis_chromatograms(ana, index = c(2, 1))

nrow(rcpp_parse_msAnalysis_chromatograms(ana))















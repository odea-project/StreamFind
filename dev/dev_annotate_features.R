# resources --------------------------------------------------------------------

ex_file_path <- "C:/Users/Ricardo Cunha/Documents/Work/example_ms_files"

ex_files <- list.files(
  ex_file_path,
  pattern = ".mzML|.mzXML",
  full.names = TRUE
)

db <- StreamFindData::get_ms_tof_spiked_chemicals()
db_cols <- c("name", "formula", "mass", "rt")
db <- db[, db_cols, with = FALSE]

db2 <- paste0(ex_file_path, "/Composition_Mix-Fusion.csv")
db2 <- data.table::fread(db2)
cols <- c("name", "formula", "mz", "rt")
db2 <- db2[, cols, with = FALSE]

db3 <- paste0(ex_file_path, "/mix1_orbitrap_ude.csv")
db3 <- data.table::fread(db3)
cols <- c("name", "formula", "mz", "rt")
db3 <- db3[, cols, with = FALSE]


# settings ---------------------------------------------------------------------

tof_ffs <- ProcessingSettings(
  call = "find_features",
  algorithm = "xcms3_centwave",
  parameters = list(
    ppm = 12,
    peakwidth = c(5, 40),
    snthresh = 10,
    prefilter = c(5, 2000),
    mzCenterFun = "wMean",
    integrate = 1,
    mzdiff = 0.0008,
    fitgauss = TRUE,
    noise = 500,
    verboseColumns = TRUE,
    firstBaselineCheck = FALSE,
    extendLengthMSW = FALSE
  )
)

orb_ffs <- ProcessingSettings(
  call = "find_features",
  algorithm = "xcms3_centwave",
  parameters = list(
    ppm = 3,
    peakwidth = c(5, 80),
    snthresh = 10,
    prefilter = c(6, 150000),
    mzCenterFun = "wMean",
    integrate = 1,
    mzdiff = 0.0002,
    fitgauss = TRUE,
    noise = 50000,
    verboseColumns = TRUE,
    firstBaselineCheck = FALSE,
    extendLengthMSW = FALSE
  )
)

orb_ffs_2 <- ProcessingSettings(
  call = "find_features",
  algorithm = "xcms3_centwave",
  parameters = list(
    ppm = 4,
    peakwidth = c(5, 80),
    snthresh = 10,
    prefilter = c(6, 50000),
    mzCenterFun = "wMean",
    integrate = 2,
    mzdiff = 0.0002,
    fitgauss = TRUE,
    noise = 50000 / 3,
    verboseColumns = TRUE,
    firstBaselineCheck = FALSE,
    extendLengthMSW = FALSE
  )
)

afs <- get_default_ProcessingSettings(
  call = "annotate_features",
  algorithm = "StreamFind"
)

afs$parameters$maxCharge <- 1
afs$parameters$rtWindowAlignment <- 0.3

# MassSpecData -----------------------------------------------------------------

tof_ms <- MassSpecData$new(
  files = ex_files[18], headers = list(name = "tof")
)

tof_ms$find_features(tof_ffs)$annotate_features(afs)

orb_ms <- MassSpecData$new(
  files = orb_file[9], headers = list(name = "orb")
)

orb_ms$find_features(orb_ffs)$annotate_features(afs)

ms_120k <- MassSpecData$new(
  files = ex_files[5], headers = list(name = "Res 120k")
)

ms_120k$find_features(orb_ffs)$annotate_features(afs)

ms_60k <- MassSpecData$new(
  files = ex_files[9], headers = list(name = "Res 60k")
)

ms_60k$find_features(orb_ffs_2)$annotate_features(afs)

ms_25k <- MassSpecData$new(
  files = ex_files[9], headers = list(name = "Res 25k")
)

ms_25k$find_features(tof_ffs)$annotate_features(afs)


# code dev ---------------------------------------------------------------------

peaklist <- ms_25k$get_features(filtered = TRUE)
peaklist <- peaklist[, c("mz", "intensity", "rt"), with = FALSE]
data.table::setnames(peaklist, "mz", "mass")
peaklist$rt <- peaklist$rt / 60

library(enviPat)
library(nontarget)

# nontarget::homol.search(
#   peaklist = pl,
#   isotopes, #data(isotopes)
#   elements = c("C", "H", "O"),
#   use_C = FALSE,
#   minmz = 5,
#   maxmz = 120,
#   minrt = -2,
#   maxrt = 2,
#   ppm = TRUE,
#   mztol = 3.5,
#   rttol = 0.5,
#   minlength = 5,
#   mzfilter = FALSE,
#   vec_size = 3E6,
#   mat_size = 3,
#   R2 = .98,
#   spar = .45,
#   plotit = FALSE,
#   deb = 0
# )

data(peaklist);
data(adducts);
data(isotopes);

iso <- nontarget::make.isos(
  isotopes,
  use_isotopes=c("13C","15N","34S","37Cl","81Br","41K","13C","15N","34S","37Cl","81Br","41K"),
  use_charges=c(1,1,1,1,1,1,2,2,2,2,2,2)
)

pattern <- nontarget::pattern.search(
peaklist,
iso,
cutint=500,
rttol=c(-0.05,0.05),
mztol=5,
mzfrac=0.1,
ppm=TRUE,
inttol=0.4,
rules=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),
deter=FALSE,
entry=50
);

plotall(pattern)
plotisotopes(pattern)


# annotation results -----------------------------------------------------------

# tof_suspects <- tof_ms$get_suspects(db, ppm = 8, sec = 10)
# orb_suspects <- orb_ms$get_suspects(db2, ppm = 3)
#
# tof_features <- tof_ms$get_features()
# orb_features <- orb_ms$get_features()
#
# tof_suspects$name[tof_suspects$formula %in% orb_suspects$formula]
# orb_suspects$name[orb_suspects$formula %in% tof_suspects$formula]
#
# tof_ms$map_components(mass = db[c(1, 4, 7, 8, 11, 12, 20), db_cols, with = FALSE], legendNames = TRUE)
# orb_ms$map_components(mz = db2[!is.na(db2$rt), ], ppm = 4, sec = 5, legendNames = TRUE)

# orb_60k_100_suspects <- ms_60k$get_suspects(analyses = 2, db3, ppm = 4, sec = 10)
# ms_60k$map_components(analyses = 2, features = "mz233.024_rt847_f1627", legendNames = TRUE)
# diuron_100_comp <- ms_60k$get_components(analyses = 2, features = "mz233.024_rt847_f1627")
# diuron_100_comp_cols <- colnames(diuron_100_comp)[grepl("iso", colnames(diuron_100_comp))]
# diuron_100_comp <- diuron_100_comp[, diuron_100_comp_cols, with = FALSE]


# eval n_carbons ---------------------------------------------------------------

# n_carbons <- 1:200
#
# df_13C_rel_int_check <- data.frame(
#   "n_carbons" = n_carbons,
#   "i1" = rep(0, 200),
#   "i2" = rep(0, 200),
#   "i3" = rep(0, 200),
#   "i4" = rep(0, 200)
# )
#
# for (i in n_carbons) {
#   output <- rcpp_ms_annotation_isotopes(fts, maxGaps = 1, maxCarbons = i)
#   df_13C_rel_int_check$i1[i] <- output$max_rel_int_13C
#   df_13C_rel_int_check$i2[i] <- output$max_rel_int_13C_13C
#   df_13C_rel_int_check$i3[i] <- output$max_rel_int_13C_13C_13C
#   df_13C_rel_int_check$i4[i] <- output$max_rel_int_13C_13C_13C_13C
# }
#
# fig <- plotly::plot_ly(df_13C_rel_int_check, x = ~n_carbons)
# fig <- fig %>% plotly::add_trace(y = ~i1, name = 'M+1', mode = 'lines')
# fig <- fig %>% plotly::add_trace(y = ~i2, name = 'M+2', mode = 'lines')
# fig <- fig %>% plotly::add_trace(y = ~i3, name = 'M+3', mode = 'lines')
# fig <- fig %>% plotly::add_trace(y = ~i4, name = 'M+4', mode = 'lines')
#
# fig <- fig %>% plotly::layout(
#   title = "StreamFind annotate_features method",
#   xaxis = list(title = "Number of <sup>13</sup>C"),
#   yaxis = list(title = "Relative abundance"))
#
# fig
#
# envipat_path <- "C:\\Users\\Ricardo Cunha\\Desktop\\isotopes_rel_int_check_res.csv"#choose.files()
# envipat_df <- data.table::fread(envipat_path)
#
# fig <- plotly::plot_ly(envipat_df, x = ~n_carbons)
# fig <- fig %>% plotly::add_trace(y = ~i1, name = 'M+1', mode = 'markers+lines')
# fig <- fig %>% plotly::add_trace(y = ~i2, name = 'M+2', mode = 'markers+lines')
# fig <- fig %>% plotly::add_trace(y = ~i3, name = 'M+3', mode = 'markers+lines')
# fig <- fig %>% plotly::add_trace(y = ~i4, name = 'M+4', mode = 'markers+lines')
#
# fig <- fig %>% plotly::layout(
#   title = "enviPat (eawag)",
#   xaxis = list(title = "Number of <sup>13</sup>C"),
#   yaxis = list(title = "Relative abundance"))
#
# fig


# mass resolution --------------------------------------------------------------
#
# fts_120k <- ms_120k$get_features()
# fts_60k_10 <- ms_60k$get_features(analyses = 1)
# fts_60k_100 <- ms_60k$get_features(analyses = 2)
# fts_25k <- ms_25k$get_features()
#
# fig <- plotly::plot_ly()
#
# fig <- fig %>% plotly::add_trace(
#   y = fts_25k$iso_mz_sd[fts_25k$iso_step == 0] / fts_25k$mz[fts_25k$iso_step == 0] * 1E6,
#   type = "box", name = "Low Resolution (25k)", showlegend = F)
#
# fig <- fig %>% plotly::add_trace(
#   y = fts_60k_100$iso_mz_sd[fts_60k_100$iso_step == 0] / fts_60k_100$mz[fts_60k_100$iso_step == 0] * 1E6,
#   type = "box", name = "Mid Resolution (60k)", showlegend = F)
#
# fig <- fig %>% plotly::add_trace(
#   y = fts_120k$iso_mz_sd[fts_120k$iso_step == 0] / fts_120k$mz[fts_120k$iso_step == 0] * 1E6,
#   type = "box", name = "High Resolution (120k)", showlegend = F)
#
# fig <- fig %>% plotly::layout(
#   title = "Mass deviation spread of features",
#   xaxis = list(title = ""),
#   yaxis = list(title = "<i>m/z</i> deviation (ppm)"))
#
# fig
#
#
#
# c13_fts_120k <- fts_120k[iso_step == 1 & iso_el %in% "13C", ]
# c13_fts_60k_10 <- fts_60k_10[iso_step == 1 & iso_el %in% "13C", ]
# c13_fts_60k_100 <- fts_60k_100[iso_step == 1 & iso_el %in% "13C", ]
# c13_fts_25k <- fts_25k[iso_step == 1 & iso_el %in% "13C", ]
#
# fig <- plotly::plot_ly()
#
# fig <- fig %>% plotly::add_trace(
#   y = c13_fts_25k$iso_mz_sd / c13_fts_25k$mz * 1E6,
#   type = "box", name = "Low Resolution (25k)", showlegend = F)
#
# fig <- fig %>% plotly::add_trace(
#   y = c13_fts_60k_100$iso_mz_sd / c13_fts_60k_100$mz * 1E6,
#   type = "box", name = "Mid Resolution (60k)", showlegend = F)
#
# fig <- fig %>% plotly::add_trace(
#   y = c13_fts_120k$iso_mz_sd / c13_fts_120k$mz * 1E6,
#   type = "box", name = "High Resolution (120k)", showlegend = F)
#
# fig <- fig %>% plotly::layout(
#   title = "",
#   xaxis = list(title = ""),
#   yaxis = list(title = "<i>m/z</i> error (ppm)"))
#
# fig


# code dev ---------------------------------------------------------------------

# TODO validate multiple charged ions
# TODO test large molecules c1k to c100k


# suspects <- ms_60k$get_suspects(analyses = 2, db3, ppm = 4, sec = 10)
# View(suspects)
#
# fts <- ms_60k$get_features(analyses = 2)
# fts <- fts[order(fts$mz), ]
# which(fts$feature %in% "mz172.133_rt352_f549")
#
# output <- rcpp_ms_annotation_isotopes(fts, rtWindowAlignment = 0.5)
#
#
# ms_60k$map_components(analyses = 2, mz = suspects[suspects$analysis %in% "orb_60000_Mix1_100_02", ], legendNames = TRUE)
#
# ms_60k$plot_features(analyses = 2,
#   features = ms_60k$get_components(analyses = 2, features = "mz172.133_rt351_f577")
# )
#
# ms_60k$plot_xic(mz = db3$mz[2], rt = db3$rt[2], ppm = 20, sec = 120)











# other ------------------------------------------------------------------------

# Gd example -------------------------------------------------------------------

# Gadoteridol C17H29GdN4O7
# orb_ms$map_components(features = c("mz560.136_rt420_f2358"))
# orb_ms$plot_features_ms1(
#   features = orb_ms$get_components(features = c("mz557.134_rt420_f2368"))[["feature"]],
#   rtWindow = c(-0.2, 0.2), mzWindow = c(-0.0005, 0.0005))
#
# suspects_res <- suspects$name
# names(suspects_res) <- suspects$feature
# suspects_for <- suspects$formula
# names(suspects_for) <- suspects$feature
# features <- ms$get_features(filtered = TRUE)
# features$name <- suspects_res[features$iso_feat]
# features$formula <- suspects_for[features$iso_feat]
# View(features)
#
# fts <- ms$get_features(analyses = 10)
# plot(fts$mz[fts$iso_step == 0], fts$iso_mz_sd[fts$iso_step == 0])
#
# suspects_res <- suspects$name
# names(suspects_res) <- suspects$feature
# suspects_for <- suspects$formula
# names(suspects_for) <- suspects$feature
# output$output$name <- suspects_res[output$output$iso_feat]
# output$output$formula <- suspects_for[output$output$iso_feat]
# View(output$output)
#
# suspects_120k <- ms_120k$get_suspects(db2[db2$name %in% "Sotalol"], ppm = 1, sec = 120)
# suspects_60k <- ms_60k$get_suspects(db3[db3$name %in% "Sotalol"], ppm = 8, sec = 120)
#
# ms_60k$get_components(features = suspects_60k$feature)
#
#
# ms_120k$map_components(features = "mz164.115_rt377_f1814")
# ms_120k$plot_features_ms1(
#   features = ms_120k$get_components(features = "mz164.115_rt377_f1814")[["feature"]],
#   rtWindow = c(-0.5, 0.5),
#   mzWindow = c(-0.005, 0.005)
# )
#
# ms_60k$plot_xic(mz = suspects_60k$mz[1], rt =  suspects_60k$rt[1], ppm = 5, sec = 60)
# ms_120k$plot_xic(mz = suspects_120k$mz, rt =  suspects_120k$rt, ppm = 5, sec = 60)
#
# plotly::plot_ly(y = fts_120k$sn, type = "box")
# plotly::plot_ly(y = fts_60k_100$intensity, type = "box")
# plotly::plot_ly(x = fts_60k_100$rt, y = fts_60k_100$intensity)
# plotly::plot_ly(x = fts_60k_100$rt, y = fts_60k_100$intensity)
#
#
#
#
#
#
#
#
#
# ms$plot_features_ms1(features = c("mz247.166_rt1075_f88"),
#   rtWindow = c(-2, 2),
#   mzWindow = c(-2, 6),
#   interactive = TRUE
# )
#
# ms$plot_features_ms1(
#   analyses = 1,
#   features = output$output$feature[output$output$iso_feat %in% c("mz213.188_rt1149_f144")],
#   rtWindow = c(-0.5, 0.5),
#   mzWindow = c(-0.005, 0.005)
# )
#
#
#
#
# ms$plot_features_ms1(
#   analyses = 1,
#   features = output$output$feature[output$output$iso_feat %in% c("mz233.025_rt1161_f162")],
#   rtWindow = c(-0.5, 0.5),
#   mzWindow = c(-0.005, 0.005)
# )
#
#
# ms$plot_features_ms1(
#   analyses = 1,
#   features = fts[fts$mz >= 254.06 & fts$mz <= 257.06, ],
#   rtWindow = c(-0.5, 0.5),
#   mzWindow = c(-0.005, 0.005)
# )
#
# fts <- ms$get_features(analyses = 1)
# fts <- fts[order(fts$mz), ]
# which(fts$feature %in% "mz254.06_rt1017_f70")
#
# output <- rcpp_ms_annotation_isotopes(fts, maxGaps = 1)
#
# suspects_res <- suspects$name
# names(suspects_res) <- suspects$feature
#
# suspects_for <- suspects$formula
# names(suspects_for) <- suspects$feature
#
# output$output$name <- suspects_res[output$output$iso_feat]
# output$output$formula <- suspects_for[output$output$iso_feat]
#
# # View(output)
#
#
#
# View(output$output)


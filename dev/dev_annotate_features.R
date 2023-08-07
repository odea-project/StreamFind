# resources --------------------------------------------------------------------

all_files <- streamFindData::msFilePaths()
db <- streamFindData::msSpikedChemicals()
files_mrm <- all_files[grepl("mrm", all_files)]
files <- all_files[1:3]
files1 <- all_files[grepl("influent|blank", all_files)]
files2 <- all_files[grepl("o3sw", all_files)]
db_cols <- c("name", "formula", "mass", "rt")

carbamazepin_d10 <- db[db$name %in% "Carbamazepin-d10", db_cols, with = FALSE]
diuron_d6 <- db[db$name %in% "Diuron-d6", db_cols, with = FALSE]
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
    id = c("tg1", "tg2"),
    mz = c(carb, diu),
    rt = c(carb_rt, diu_rt)
  ),
  ppm = ppm_dev,
  sec = sec_dev
)

iso_info <- data.table::fread(paste0(getwd(), "/dev/isotopes.csv"))

# settings ---------------------------------------------------------------------

ffs <- ProcessingSettings(
  call = "find_features",
  algorithm = "xcms3",
  parameters = xcms::CentWaveParam(
    ppm = 12, peakwidth = c(5, 40),
    snthresh = 10, prefilter = c(5, 2000),
    mzCenterFun = "wMean", integrate = 1,
    mzdiff = 0.0002, fitgauss = TRUE,
    noise = 500, verboseColumns = TRUE,
    firstBaselineCheck = FALSE,
    extendLengthMSW = FALSE
  )
)

# r6 test ----------------------------------------------------------------------
# patRoon::clearCache("parsed_ms_analyses")
# patRoon::clearCache("parsed_ms_spectra")

ms <- MassSpecData$new(
  files = all_files[2],
  headers = list(name = "Example 1"),
  settings = ffs
)

ms$find_features()

ms$get_analyses()

# code dev ---------------------------------------------------------------------

suspects <- ms$suspect_screening(db[, db_cols, with = FALSE], ppm = 8, sec = 10)

fts <- ms$get_features(analyses = 1)
fts <- fts[order(fts$mz), ]
# which(fts$feature %in% "mz254.06_rt1017_f70")
output <- rcpp_ms_annotation_isotopes(fts, maxGaps = 1)

suspects_res <- suspects$name
names(suspects_res) <- suspects$feature
suspects_for <- suspects$formula
names(suspects_for) <- suspects$feature
output$output$name <- suspects_res[output$output$iso_feat]
output$output$formula <- suspects_for[output$output$iso_feat]
View(output$output)





afs <- list(
  call = "annotate_features",
  algorithm = "streamFind",
  parameters = list()
)

ms$annotate_features(afs)

ms










ms$plot_features_ms1(features = c("mz247.166_rt1075_f88"),
  rtWindow = c(-2, 2),
  mzWindow = c(-2, 6),
  interactive = TRUE
)

ms$plot_features_ms1(
  analyses = 1,
  features = output$output$feature[output$output$iso_feat %in% c("mz213.188_rt1149_f144")],
  rtWindow = c(-0.5, 0.5),
  mzWindow = c(-0.005, 0.005)
)




ms$plot_features_ms1(
  analyses = 1,
  features = output$output$feature[output$output$iso_feat %in% c("mz233.025_rt1161_f162")],
  rtWindow = c(-0.5, 0.5),
  mzWindow = c(-0.005, 0.005)
)


ms$plot_features_ms1(
  analyses = 1,
  features = fts[fts$mz >= 254.06 & fts$mz <= 257.06, ],
  rtWindow = c(-0.5, 0.5),
  mzWindow = c(-0.005, 0.005)
)

fts <- ms$get_features(analyses = 1)
fts <- fts[order(fts$mz), ]
which(fts$feature %in% "mz254.06_rt1017_f70")

output <- rcpp_ms_annotation_isotopes(fts, maxGaps = 1)

suspects_res <- suspects$name
names(suspects_res) <- suspects$feature

suspects_for <- suspects$formula
names(suspects_for) <- suspects$feature

output$output$name <- suspects_res[output$output$iso_feat]
output$output$formula <- suspects_for[output$output$iso_feat]

# View(output)



View(output$output)










orb_files <- c(
  "E:\\20210705_OrbitrapData\\Centroid_mzML\\orb_cent_10_02.mzML",
  "E:\\20210705_OrbitrapData\\Centroid_mzML\\orb_cent_100_02.mzML"
)

orb_ms <- MassSpecData$new(orb_files)

# orb_ms$plot_bpc()
# orb_ms$plot_eic(mz = 293.071, ppm = 10, colorBy = "analyses")

ffs <- ProcessingSettings(
  call = "find_features",
  algorithm = "xcms3",
  parameters = xcms::CentWaveParam(
    ppm = 3,
    peakwidth = c(5, 80),
    snthresh = 5,
    prefilter = c(6, 75000),
    mzCenterFun = "wMean",
    integrate = 2,
    mzdiff = 0.00005,
    fitgauss = TRUE,
    noise = 25000,
    verboseColumns = TRUE,
    firstBaselineCheck = FALSE,
    extendLengthMSW = FALSE
  )
)

orb_ms$find_features(ffs)

suspects <- orb_ms$suspect_screening(db[, c("name", "formula", "mass"), with = FALSE], ppm = 5, sec = 10)

orb_ms$plot_features_ms1(
  analyses = 2,
  features = output$output$feature[output$output$iso_feat %in% c("mz254.059_rt610_f947")][1:4],
  rtWindow = c(-0.5, 0.5),
  mzWindow = c(-0.0005, 0.0005)
)


fts <- orb_ms$get_features(analyses = 2)
fts <- fts[order(fts$mz), ]
which(fts$feature %in% "mz242.133_rt801_f1374")

orb_ms$map_features(analyses = 2, mz = data.frame(mzmin = 242, mzmax = 247, rtmin = 798, rtmax = 802))

orb_ms$plot_xic(analyses = 2, mz = data.frame(mzmin = 242, mzmax = 243, rtmin = 780, rtmax = 820))

output <- rcpp_ms_annotation_isotopes(fts, maxGaps = 1)

suspects_res <- suspects$name
names(suspects_res) <- suspects$feature

suspects_for <- suspects$formula
names(suspects_for) <- suspects$feature

output$output$name <- suspects_res[output$output$iso_feat]
output$output$formula <- suspects_for[output$output$iso_feat]

View(output$output)





View(rcpp_ms_annotation_isotopes(fts)[["output"]])

out <- rcpp_ms_annotation_isotopes(fts)[["output"]]
out <- out[out$iso_gr == 0, ]

ms$map_features(1, features = out$feature)
ms$plot_features(1, features = out$feature)


max(out$intensity[out$iso_gr == 0 & out$iso_diff == 0])


View(rcpp_ms_annotation_isotopes(fts)[[2]])

ms$plot_features(analyses = 1, features = "mz247.177_rt1121_f183")

out$iso_hits
out$iso_elements_key

length(out$IsoMd)

length(unique(round(out$IsoMd, digits = 3)))


out$iso_combinations_vec_mass

View(out$iso_combined)

length(out$combinations)

test <- abs(c(0, out$iso_combined$mass_diff[-length(out$iso_combined$mass_diff)]) - out$iso_combined$mass_diff)

head(out$iso_combinations_vec_mass_ordered, 20)

out$iso_combined$mass_diff <- round(out$iso_combined$mass_diff, digits = 4)
vec <- unique(out$iso_combined$mass_diff)
vec <- vec[vec < 1.1]
weighted.mean(vec, out$iso_combined$rel_ab[out$iso_combined$mass_diff %in% vec] * c(1, 1, 10, 1, 5))


z <- 2
m1 <- 300
mz1 <- m1 / z

m2 <- 302
mz2 <- m2 / z

mz2 - mz1

unique(round(na.omit(isos$i1), digits = 3))
unique(round(na.omit(isos$i2 - isos$i0), digits = 3))
unique(round(na.omit(isos$i3 - isos$i0), digits = 3))

max(na.omit(isos$i1 - isos$i0)) - min(na.omit(isos$i1 - isos$i0))


na.omit(isos$i2 - isos$i1)
max(na.omit(isos$i2 - isos$i0)) - min(na.omit(isos$i2 - isos$i0))









N_diff <- 0.9970349
N_w <- 0.3663 / 100
C_diff <- 1.0033548378
C_w <- 1.1078 / 100
H_diff <- 1.0062767
H_w <- 0.0156 / 100
O_diff <- 1.0042169
O_w <- 0.0372 / 100
O2_diff <- 2.004246
O2_w <- 0.2 / 100
S_diff <- 0.9993878
S_w <- 0.75 / 100
S2_diff <- 1.995796
S2_w <- 4.215 / 100
S3_diff <- 3.99501
S3_w <- 0.017 / 100
Cl_diff <- 1.9970499
Cl_w <- 24.229 / 100
Br_diff <- 1.9979534
Br_w <- 49.314 / 100

elements <- c("C", "H", "N", "S", "S2", "S3", "Cl", "Br", "O", "O2")
mass_diff <- c(C_diff, H_diff, N_diff, S_diff, S2_diff, S3_diff, Cl_diff, Br_diff, O_diff, O2_diff)
weig_diff <- c(C_w, H_w, N_w, S_w, S2_w, S3_w, Cl_w, Br_w, O_w, O2_w)

df <- data.table("element" = elements, "mass_diff" = mass_diff, "weig_diff" = weig_diff, level = 1)
# df <- df[df$weig_diff > 0.001]
replicatations <- 3
offset <- 1
for (r in 1:replicatations) {
  iterations <- nrow(df)
  cat(iterations)
  cat("\n")
  for (i in offset:iterations) {
    for (z in 1:10) {
      el <- paste0(df$element[i], df$element[z])
      mass <- df$mass_diff[i] + df$mass_diff[z]
      w <- df$weig_diff[i] * df$weig_diff[z]
      df <- rbind(df,
        data.table(
          "element" = el,
          "mass_diff" = mass,
          "weig_diff" = w,
          "level" = r + 1
        )
      )
    }
  }

  df <- df[!duplicated(df[, c("mass_diff", "weig_diff", "level")]), ]

  offset <- offset + iterations
  cat(offset)
  cat("\n")
}

# for (i in 11:iterations) {
#   for (z in 1:iterations) {
#     el <- paste0(df$element[i], df$element[z])
#     mass <- df$mass_diff[i] + df$mass_diff[z]
#     w <- df$weig_diff[i] * df$weig_diff[z]
#     df <- rbind(df,
#       data.table(
#         "element" = el,
#         "mass_diff" = mass,
#         "weig_diff" = w
#       )
#     )
#   }
# }

# for (i in 111:iterations) {
#   for (z in 1:iterations) {
#     el <- paste0(df$element[i], df$element[z])
#     mass <- df$mass_diff[i] + df$mass_diff[z]
#     w <- df$weig_diff[i] * df$weig_diff[z]
#     df <- rbind(df,
#       data.table(
#         "element" = el,
#         "mass_diff" = mass,
#         "weig_diff" = w
#       )
#     )
#   }
# }

df$mass_diff <- signif(df$mass_diff, digits = 5)

vec <- unique(df$mass_diff[df$weig_diff > 0.01 / 100])
vec <- vec[vec < 1.5]
weighted.mean(vec, df$weig_diff[df$mass_diff %in% vec] * c(38, 69, 1, 0, 13))












# Define the monoisotopic mass
monoisotopic_mass <- 239.0628

# Define the maximum number of isotopes to consider
max_isotopes <- 6

# Define the isotopic masses and intensities vectors
isotopic_masses <- numeric(max_isotopes)
isotopic_intensities <- numeric(max_isotopes)

# Define the isotopic abundance ratios of carbon, hydrogen, nitrogen, oxygen, and sulfur
C_ratio <- 0.0107
H_ratio <- 0.000158
N_ratio <- 0.00368
O_ratio <- 0.00038
S_ratio <- 0.0002

# Calculate the isotopic masses and intensities
for (i in 1:max_isotopes) {
  # Calculate the mass of the ith isotope
  mass <- monoisotopic_mass + (i - 1) * 1.007276

  # Calculate the abundance of the ith isotope
  abundance <- (1 - C_ratio - H_ratio - N_ratio - O_ratio - S_ratio) ^ (i - 1) * C_ratio * (1 - C_ratio - H_ratio - N_ratio - O_ratio - S_ratio) ^ (max(0, max_isotopes - i)) * H_ratio ^ (max(0, 4 - i)) * N_ratio ^ (max(0, 2 - i)) * O_ratio ^ (max(0, 3 - i)) * S_ratio ^ (max(0, 1 - i))

  # Add the ith isotope to the isotopic masses and intensities vectors
  isotopic_masses[i] <- mass
  isotopic_intensities[i] <- abundance
}

# Print the isotopic masses and intensities
cat("Isotopic masses (intensities):\n")
for (i in 1:max_isotopes) {
  cat(isotopic_masses[i], "(", isotopic_intensities[i]*100, "%)\n")
}









for (i in 1:9) {
  col <- paste0("i", i)
  isos[[col]] <- isos[[col]] - isos$i0
  isos[[col]][isos[[col]] < 0] <- 0
  # cal <- paste0("a", i)
  # isos[[cal]] <- isos[[cal]] / 100
}

# elements <- c("C", "H", "N", "S", "Cl", "Br", "O", "Si", "F", "P", "Mg", "Zn", "Fe", "K", "Ca", "I")
elements <- c("C", "H", "N", "S", "Cl", "Br", "O", "F", "P", "I")
isos <- isos[isos$element %in% elements, ]

weighted.mean(isos$i1[isos$i1 < 1], isos$a1[isos$i1 < 1])
weighted.mean(isos$i1[isos$i1 < 1.5 & isos$i1 > 0], isos$a1[isos$i1 < 1.5 & isos$i1 > 0])

unique(round(isos$i1[isos$i1 < 1.5], digits = 2))
unique(round(na.omit(isos$i2 - isos$i0), digits = 3))
unique(round(na.omit(isos$i3 - isos$i0), digits = 3))

























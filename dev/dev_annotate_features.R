# resources --------------------------------------------------------------------

all_files <- streamFindData::msFilePaths()
db <- streamFindData::msSpikedChemicals()
files_mrm <- all_files[grepl("mrm", all_files)]
files <- all_files[1:3]
files1 <- all_files[grepl("influent|blank", all_files)]
files2 <- all_files[grepl("o3sw", all_files)]
db_cols <- c("name", "mass", "rt")

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

isos <- read.csv(paste0(getwd(), "/dev/isotopes.csv"))
elements <- c("C", "H", "N", "S", "Cl", "Br", "O")
isos <- isos[isos$element %in% elements, ]

# settings ---------------------------------------------------------------------

settings_ff <- list(
  call = "find_features",
  algorithm = "xcms3",
  parameters = list(xcms::CentWaveParam(
    ppm = 12, peakwidth = c(5, 30),
    snthresh = 10, prefilter = c(5, 1000),
    mzCenterFun = "wMean", integrate = 1,
    mzdiff = -0.0005, fitgauss = TRUE,
    noise = 500, verboseColumns = TRUE,
    firstBaselineCheck = TRUE,
    extendLengthMSW = FALSE
  ))
)

# r6 test ----------------------------------------------------------------------
# patRoon::clearCache("parsed_ms_analyses")
# patRoon::clearCache("parsed_ms_spectra")

ms <- MassSpecData$new(files = all_files[1:3],
  headers = list(name = "Example 1"),
  settings = list(settings_ff)
)

ms$find_features()

# code dev ---------------------------------------------------------------------

fts <- ms$get_features(analyses = 1)
cols_keep <- c("feature", "mass", "mz", "mzmin", "mzmax", "rt", "rtmin","rtmax", "intensity")
fts <- fts[, cols_keep, with = FALSE]

fts_tar <- ms$get_features(analyses = 1, mz = targets[2, ])
fts1 <- fts[fts$rt >= fts_tar$rtmin & fts$rt <= fts_tar$rtmax, ]
fts1 <- fts1[order(fts1$mz), ]

fts1$mz

plot_spectra_interactive(fts)

rcpp_ms_annotation_isotopes(fts)











na.omit(isos$i1 - isos$i0) - (1.0033548378 * 2)

max(na.omit(isos$i1 - isos$i0)) - min(na.omit(isos$i1 - isos$i0))


na.omit(isos$i2 - isos$i0)
max(na.omit(isos$i2 - isos$i0)) - min(na.omit(isos$i2 - isos$i0))
































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



































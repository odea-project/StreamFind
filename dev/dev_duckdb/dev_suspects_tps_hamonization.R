
# sus <- data.table::fread(
#   file.path(
#     "dev",
#     "dev_duckdb",
#     "suspects_template.csv"
#   )
# )

# sus$SMILES <- vapply(sus$SMILES, function(x) {
#   tryCatch(
#     rcdk::get.smiles(
#       rcdk::parse.smiles(x)[[1]],
#       rcdk::smiles.flavors(c("Canonical"))
#     ), error = function(e) NA_character_
#   )
# }, NA_character_)

# sus$mass <- vapply(sus$SMILES, function(x) {
#   tryCatch(
#     rcdk::get.exact.mass(
#       rcdk::parse.smiles(x)[[1]]
#     ), error = function(e) NA_real_
#   )
# }, NA_real_)

# sus$InChI <- vapply(sus$SMILES, function(x) {
#   tryCatch(
#     rJava::.jcall(
#       "org/guha/rcdk/util/Misc", "S", "getInChi",
#       rcdk::parse.smiles(x)[[1]],
#       check = FALSE
#     ), error = function(e) NA_character_
#   )
# }, NA_character_)

# sus$InChIKey <- vapply(sus$SMILES, function(x) {
#   tryCatch(
#     rJava::.jcall(
#       "org/guha/rcdk/util/Misc", "S", "getInChiKey",
#       rcdk::parse.smiles(x)[[1]],
#       check = FALSE
#     ), error = function(e) NA_character_
#   )
# }, NA_character_)

# data.table::setnames(sus, "LogP", "xLogP")
# data.table::setnames(sus, "precursor_LogP", "precursor_xLogP")

# sus$xLogP <- vapply(sus$SMILES, function(x) {
#   tryCatch(
#     rcdk::get.xlogp(
#       rcdk::parse.smiles(x)[[1]]
#     ), error = function(e) NA_real_
#   )
# }, NA_real_)

# sus <- sus[, 1:which(colnames(sus) == "xLogP")]
# sus_ms2 <- data.table::copy(sus)
# sus$polarity <- NULL
# sus$fragments <- NULL

# data.table::fwrite(sus, file.path(
#   "dev",
#   "dev_duckdb",
#   "suspects_template.csv"
# ))

# sus$polarity <- "positive"
# sus$ms2 <- sus_ms2$fragments

# data.table::fwrite(sus, file.path(
#   "dev",
#   "dev_duckdb",
#   "suspects_with_ms2_template.csv"
# ))

sus <- data.table::fread(
  file.path(
    "dev",
    "dev_duckdb",
    "suspects_template.csv"
  )
)

# tps <- data.table::fread(
#   file.path(
#     "dev",
#     "dev_duckdb",
#     "transformation_products_template.csv"
#   )
# )

tps <- search_transformation_products_biotransformer(
  parents = sus[c(2, 9), ],
  biotransformerOption = c(
    "ABIOTICBIO"
  ),
  numberOfSteps = 2,
  throttleSec = 31,
  pollDelay = 5,
  maxPoll = 60,
  excludeWithSameMass = FALSE,
  ppm = 5,
  debug = FALSE
)

data.table::fwrite(
  tps,
  file.path(
    "dev",
    "dev_duckdb",
    "transformation_products_template.csv"
  )
)


istd <- data.table::fread(
  file.path(
    "I:\\internal_standards.csv"
  )
)

get_suspects_screening_csv(
  suspects = istd,
  file = file.path(
    "I:\\internal_standards.csv"
  )
)



root <- file.path("dev", "dev_duckdb", "data_nts")
# file.remove(list.files(root, full.names = TRUE))
# fs::dir_delete(root)
ms <- DB_MassSpecEngine$new(projectPath = root, files = "I:\\mzml\\16_pos_mix_10-r002.mzML")

istd <- data.table::fread(
  file.path(
    "I:\\internal_standards.csv"
  )
)

# plot_spectra_eic(
#   ms$Analyses,
#   mass = istd,
#   ppm = 20
# )


ps_ff <- DB_MassSpecMethod_FindFeatures_native(
  rtWindows = data.frame(rtmin = numeric(), rtmax = numeric()),
  ppmThreshold = 10,
  noiseThreshold = 250,
  minSNR = 3,
  minTraces = 3,
  baselineWindow = 200,
  maxWidth = 250,
  baseQuantile = 0.99,
  debugAnalysis = "16_pos_mix_10-r002",
  debugMZ = 299.0418 + 1.007276,
  debugSpecIdx = -1
)

ms$Workflow <- list(ps_ff)
ms$run_workflow()

sus <- suspect_screening(
  ms$NonTargetAnalysis,
  suspects = istd,
  ppm = 10,
  sec = 60,
  ppmMS2 = 10,
  mzrMS2 = 0.008,
  minCosineSimilarity = 0.7,
  minSharedFragments = 3
)

sus


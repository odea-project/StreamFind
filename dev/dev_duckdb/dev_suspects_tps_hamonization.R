
sus <- data.table::fread(
  file.path(
    "dev",
    "dev_duckdb",
    "suspects_template.csv"
  )
)

sus$SMILES <- vapply(sus$SMILES, function(x) {
  tryCatch(
    rcdk::get.smiles(
      rcdk::parse.smiles(x)[[1]],
      rcdk::smiles.flavors(c("Canonical"))
    ), error = function(e) NA_character_
  )
}, NA_character_)

sus$mass <- vapply(sus$SMILES, function(x) {
  tryCatch(
    rcdk::get.exact.mass(
      rcdk::parse.smiles(x)[[1]]
    ), error = function(e) NA_real_
  )
}, NA_real_)

sus$InChI <- vapply(sus$SMILES, function(x) {
  tryCatch(
    rJava::.jcall(
      "org/guha/rcdk/util/Misc", "S", "getInChi",
      rcdk::parse.smiles(x)[[1]],
      check = FALSE
    ), error = function(e) NA_character_
  )
}, NA_character_)

sus$InChIKey <- vapply(sus$SMILES, function(x) {
  tryCatch(
    rJava::.jcall(
      "org/guha/rcdk/util/Misc", "S", "getInChiKey",
      rcdk::parse.smiles(x)[[1]],
      check = FALSE
    ), error = function(e) NA_character_
  )
}, NA_character_)

data.table::setnames(sus, "LogP", "xLogP")
data.table::setnames(sus, "precursor_LogP", "precursor_xLogP")

sus$xLogP <- vapply(sus$SMILES, function(x) {
  tryCatch(
    rcdk::get.xlogp(
      rcdk::parse.smiles(x)[[1]]
    ), error = function(e) NA_real_
  )
}, NA_real_)

sus <- sus[, 1:which(colnames(sus) == "xLogP")]
sus_ms2 <- data.table::copy(sus)
sus$polarity <- NULL
sus$fragments <- NULL

data.table::fwrite(sus, file.path(
  "dev",
  "dev_duckdb",
  "suspects_template.csv"
))

sus$polarity <- "positive"
sus$ms2 <- sus_ms2$fragments

data.table::fwrite(sus, file.path(
  "dev",
  "dev_duckdb",
  "suspects_with_ms2_template.csv"
))





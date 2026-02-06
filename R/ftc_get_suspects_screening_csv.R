#' @title Create a suspect screening CSV from SMILES
#' @description Normalizes SMILES to canonical form and derives required columns
#' for suspect screening. Writes a CSV with the same columns as
#' \code{get_template_suspect_screening_csv()}.
#' @param suspects data.table or data.frame with required column: \code{name}.
#' One of \code{SMILES} or \code{mol} is required. \code{mol} is a file path to a
#' \code{.mol} file per row. When provided, the \code{.mol} file is used to derive
#' canonical SMILES and properties. \code{rt} can be provided to populate the
#' \code{rt} column in the output.
#' @param file Character (length 1). Output CSV file path.
#' @return Invisibly returns the file path.
#' @export
get_suspects_screening_csv <- function(suspects, file) {
  if (!requireNamespace("checkmate", quietly = TRUE)) {
    stop("checkmate is required for input validation.")
  }
  checkmate::assert_data_frame(suspects)
  checkmate::assert_character(file, len = 1)

  suspects <- data.table::as.data.table(suspects)
  checkmate::assert_names(names(suspects), must.include = c("name"))
  has_smiles <- "SMILES" %in% names(suspects)
  has_mol <- "mol" %in% names(suspects)
  if (!has_smiles && !has_mol) {
    stop("suspects must include either 'SMILES' or 'mol' column.")
  }

  if (!requireNamespace("rcdk", quietly = TRUE)) {
    stop("rcdk is required to normalize SMILES and compute properties.")
  }

  has_rjava <- requireNamespace("rJava", quietly = TRUE)

  calc_props_from_smiles <- function(smiles) {
    if (is.null(smiles) || length(smiles) == 0) {
      return(list(
        smiles = NA_character_,
        formula = NA_character_,
        mass = NA_real_,
        inchi = NA_character_,
        inchikey = NA_character_,
        logp = NA_real_
      ))
    }
    smiles <- as.character(smiles[1])
    smiles <- trimws(smiles)
    if (is.na(smiles) || !nzchar(smiles)) {
      return(list(
        smiles = NA_character_,
        formula = NA_character_,
        mass = NA_real_,
        inchi = NA_character_,
        inchikey = NA_character_,
        logp = NA_real_
      ))
    }

    mol <- tryCatch(rcdk::parse.smiles(smiles)[[1]], error = function(e) NULL)
    if (is.null(mol)) {
      return(list(
        smiles = smiles,
        formula = NA_character_,
        mass = NA_real_,
        inchi = NA_character_,
        inchikey = NA_character_,
        logp = NA_real_
      ))
    }

    canonical_smiles <- tryCatch(
      rcdk::get.smiles(mol, smilesFlavor = "Canonical"),
      error = function(e) tryCatch(rcdk::get.smiles(mol), error = function(e2) smiles)
    )
    mass <- tryCatch(as.numeric(rcdk::get.exact.mass(mol)), error = function(e) NA_real_)
    logp <- tryCatch(as.numeric(rcdk::get.xlogp(mol)), error = function(e) NA_real_)
    formula <- tryCatch(rcdk::get.mol2formula(mol, charge = 0)@string, error = function(e) NA_character_)

    inchi <- NA_character_
    inchikey <- NA_character_
    if (has_rjava) {
      inchi <- tryCatch(rJava::.jcall("org/guha/rcdk/util/Misc", "S", "getInChi", mol, check = FALSE), error = function(e) NA_character_)
      inchikey <- tryCatch(rJava::.jcall("org/guha/rcdk/util/Misc", "S", "getInChiKey", mol, check = FALSE), error = function(e) NA_character_)
    }

    conv_fn <- tryCatch(getFromNamespace("convert.implicit.to.explicit", "rcdk"), error = function(e) NULL)
    if (is.function(conv_fn)) {
      tryCatch(conv_fn(mol), error = function(e) NULL)
    }

    list(
      smiles = canonical_smiles,
      formula = formula,
      mass = mass,
      inchi = inchi,
      inchikey = inchikey,
      logp = logp
    )
  }

  calc_props_from_mol <- function(mol_path) {
    if (is.null(mol_path) || length(mol_path) == 0) {
      return(list(
        smiles = NA_character_,
        formula = NA_character_,
        mass = NA_real_,
        inchi = NA_character_,
        inchikey = NA_character_,
        logp = NA_real_
      ))
    }
    mol_path <- as.character(mol_path[1])
    mol_path <- trimws(mol_path)
    if (is.na(mol_path) || !nzchar(mol_path) || !file.exists(mol_path)) {
      return(list(
        smiles = NA_character_,
        formula = NA_character_,
        mass = NA_real_,
        inchi = NA_character_,
        inchikey = NA_character_,
        logp = NA_real_
      ))
    }

    mol <- tryCatch(rcdk::load.molecules(mol_path), error = function(e) NULL)
    if (is.null(mol) || length(mol) == 0 || is.null(mol[[1]])) {
      return(list(
        smiles = NA_character_,
        formula = NA_character_,
        mass = NA_real_,
        inchi = NA_character_,
        inchikey = NA_character_,
        logp = NA_real_
      ))
    }
    mol <- mol[[1]]

    canonical_smiles <- tryCatch(
      rcdk::get.smiles(mol, smilesFlavor = "Canonical"),
      error = function(e) tryCatch(rcdk::get.smiles(mol), error = function(e2) NA_character_)
    )
    mass <- tryCatch(as.numeric(rcdk::get.exact.mass(mol)), error = function(e) NA_real_)
    logp <- tryCatch(as.numeric(rcdk::get.xlogp(mol)), error = function(e) NA_real_)
    formula <- tryCatch(rcdk::get.mol2formula(mol, charge = 0)@string, error = function(e) NA_character_)

    inchi <- NA_character_
    inchikey <- NA_character_
    if (has_rjava) {
      inchi <- tryCatch(rJava::.jcall("org/guha/rcdk/util/Misc", "S", "getInChi", mol, check = FALSE), error = function(e) NA_character_)
      inchikey <- tryCatch(rJava::.jcall("org/guha/rcdk/util/Misc", "S", "getInChiKey", mol, check = FALSE), error = function(e) NA_character_)
    }

    conv_fn <- tryCatch(getFromNamespace("convert.implicit.to.explicit", "rcdk"), error = function(e) NULL)
    if (is.function(conv_fn)) {
      tryCatch(conv_fn(mol), error = function(e) NULL)
    }

    list(
      smiles = canonical_smiles,
      formula = formula,
      mass = mass,
      inchi = inchi,
      inchikey = inchikey,
      logp = logp
    )
  }

  props <- lapply(seq_len(nrow(suspects)), function(i) {
    mol_path <- if (has_mol) suspects$mol[i] else NA_character_
    mol_props <- calc_props_from_mol(mol_path)
    if (!is.na(mol_props$smiles) && nzchar(mol_props$smiles)) {
      return(mol_props)
    }
    if (has_smiles) {
      return(calc_props_from_smiles(suspects$SMILES[i]))
    }
    list(
      smiles = NA_character_,
      formula = NA_character_,
      mass = NA_real_,
      inchi = NA_character_,
      inchikey = NA_character_,
      logp = NA_real_
    )
  })
  props <- data.table::rbindlist(lapply(props, as.data.table), fill = TRUE)

  rt_val <- if ("rt" %in% names(suspects)) as.numeric(suspects$rt) else rep(NA_real_, nrow(suspects))

  out <- data.table::data.table(
    name = as.character(suspects$name),
    formula = props$formula,
    mass = props$mass,
    rt = rt_val,
    SMILES = props$smiles,
    InChI = props$inchi,
    InChIKey = props$inchikey,
    xLogP = props$logp,
    polarity = "",
    ms2 = ""
  )

  cols <- c("name", "formula", "mass", "rt", "SMILES", "InChI", "InChIKey", "xLogP", "polarity", "ms2")
  out <- out[, ..cols]

  data.table::fwrite(out, file)
  invisible(file)
}

# MARK: MassSpecMethod_GenerateTransformationProducts_biotransformer

#' @title Mass Spectrometry Method for generating transformation products using BioTransformer via patRoon
#' @description Generates transformation products using the \href{https://biotransformer.ca/}{BioTransformer} tool via \href{https://github.com/rickhelmus/patRoon}{patRoon} integration. This method is a wrapper of the `generateTPsBioTransformer` function from the patRoon package. See more details in \href{https://rickhelmus.github.io/patRoon/reference/generateTPsBioTransformer.html}{here}.
#' @param parents The parents provided as data.table with at least 'name' and 'SMILES' columns for which transformation products.
#' @param use_suspects Logical. If TRUE suspects in the MassSpecResults_NonTargetAnalysis will be used as parents.
#' @param use_compounds Logical. If TRUE compounds in the MassSpecResults_NonTargetAnalysis will be used as parents. If more more than one structure is available for a compound, only the first one will be used as it is assumed that this is the most relevant one (i.e., the highest score).
#' @param type Character. The type of prediction. Valid values are: "env", "ecbased", "cyp450", "phaseII", "hgut", "superbio", "allHuman". Sets the -b command line option. Default is "env".
#' @param generations Integer. The number of generations (steps) for the predictions. Sets the -s command line option. Default is 2.
#' @param maxExpGenerations Integer. The maximum number of generations during hierarchy expansion. Default is generations + 2.
#' @param extraOpts Character. Extra command line options passed to the biotransformer.jar tool. Default is NULL.
#' @param skipInvalid Logical. If set to TRUE then the parents will be skipped (with a warning) for which insufficient information (e.g. SMILES) is available. Default is TRUE.
#' @param prefCalcChemProps Logical. If TRUE then calculated chemical properties such as the formula and InChIKey are preferred over what is already present in the parent suspect list. Default is TRUE.
#' @param neutralChemProps Logical. If TRUE then the neutral form of the molecule is considered to calculate SMILES, formulae etc. Default is FALSE.
#' @param neutralizeTPs Logical. If TRUE then all resulting TP structure information is neutralized. Default is TRUE.
#' @param calcSims Logical. If set to TRUE then structural similarities between the parent and its TPs are calculated. Default is FALSE.
#' @param fpType Character. The type of structural fingerprint that should be calculated. Default is "extended".
#' @param fpSimMethod Character. The method for calculating similarities. Default is "tanimoto".
#' @param MP Logical. If TRUE then multiprocessing is enabled. Default is FALSE.
#' @return A `MassSpecMethod_GenerateTransformationProducts_biotransformer` object.
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' \insertRef{biotransformer01}{StreamFind}
#'
#' \insertRef{biotransformer02}{StreamFind}
#'
#' @export
#'
MassSpecMethod_GenerateTransformationProducts_biotransformer <- function(
  parents = data.table::data.table(name = character(), SMILES = character()),
  use_suspects = FALSE,
  use_compounds = FALSE,
  type = "env",
  generations = 2,
  maxExpGenerations = generations + 2,
  extraOpts = NULL,
  skipInvalid = TRUE,
  prefCalcChemProps = TRUE,
  neutralChemProps = FALSE,
  neutralizeTPs = TRUE,
  calcSims = FALSE,
  fpType = "extended",
  fpSimMethod = "tanimoto",
  MP = FALSE
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "GenerateTransformationProducts",
    required = "FindFeatures",
    algorithm = "biotransformer",
    input_class = "MassSpecResults_NonTargetAnalysis",
    output_class = "MassSpecResults_TransformationProducts",
    parameters = list(
      parents = data.table::as.data.table(parents),
      use_suspects = as.logical(use_suspects),
      use_compounds = as.logical(use_compounds),
      type = as.character(type),
      generations = as.integer(generations),
      maxExpGenerations = as.integer(maxExpGenerations),
      extraOpts = if (is.null(extraOpts)) NULL else as.character(extraOpts),
      skipInvalid = as.logical(skipInvalid),
      prefCalcChemProps = as.logical(prefCalcChemProps),
      neutralChemProps = as.logical(neutralChemProps),
      neutralizeTPs = as.logical(neutralizeTPs),
      calcSims = as.logical(calcSims),
      fpType = as.character(fpType),
      fpSimMethod = as.character(fpSimMethod),
      MP = as.logical(MP)
    ),
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "BioTransformer",
    developer = "Yannick Djoumbou-Feunang, David S. Wishart",
    contact = "http://feedback.wishartlab.com/?site=biotransformer",
    link = "https://biotransformer.ca/",
    doi = "https://doi.org/10.1186/s13321-018-0324-5"
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecMethod_GenerateTransformationProducts_biotransformer object!")
  }
}

#' @export
#' @noRd
validate_object.MassSpecMethod_GenerateTransformationProducts_biotransformer <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "GenerateTransformationProducts")
  checkmate::assert_choice(x$algorithm, "biotransformer")
  checkmate::assert_data_table(x$parameters$parents)
  if (nrow(x$parameters$parents) > 0) {
    required_cols <- c("name", "SMILES")
    missing_cols <- setdiff(required_cols, colnames(x$parameters$parents))
    if (length(missing_cols) > 0) {
      stop(
        "Parents data.table must contain the following columns: ",
        paste(missing_cols, collapse = ", ")
      )
    }
  }
  checkmate::assert_logical(x$parameters$use_suspects, len = 1)
  checkmate::assert_logical(x$parameters$use_compounds, len = 1)
  valid_types <- c("env", "ecbased", "cyp450", "phaseII", "hgut", "superbio", "allHuman")
  checkmate::assert_choice(x$parameters$type, valid_types)
  checkmate::assert_int(x$parameters$generations, lower = 1)
  checkmate::assert_int(x$parameters$maxExpGenerations, lower = 1)
  if (!is.null(x$parameters$extraOpts)) {
    checkmate::assert_character(x$parameters$extraOpts)
  }
  checkmate::assert_logical(x$parameters$skipInvalid, len = 1)
  checkmate::assert_logical(x$parameters$prefCalcChemProps, len = 1)
  checkmate::assert_logical(x$parameters$neutralChemProps, len = 1)
  checkmate::assert_logical(x$parameters$neutralizeTPs, len = 1)
  checkmate::assert_logical(x$parameters$calcSims, len = 1)
  checkmate::assert_character(x$parameters$fpType, len = 1)
  checkmate::assert_character(x$parameters$fpSimMethod, len = 1)
  checkmate::assert_logical(x$parameters$MP, len = 1)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_GenerateTransformationProducts_biotransformer <- function(x, engine, ...) {
  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for biotransformer integration. Not done.")
    return(FALSE)
  }

  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (is.null(engine$Results$MassSpecResults_NonTargetAnalysis)) {
    warning("No MassSpecResults_NonTargetAnalysis object available! Not done.")
    return(FALSE)
  }

  nts <- engine$Results$MassSpecResults_NonTargetAnalysis

  parents <- x$parameters$parents
  if (x$parameters$use_suspects) {
    suspects <- get_suspects(nts)
    if (!all(c("SMILES", "name") %in% colnames(suspects))) {
      warning("Suspects do not have required columns 'name' and 'SMILES'! Not done.")
      return(FALSE)
    }
    suspects <- suspects[!is.na(suspects$SMILES) & suspects$SMILES != ""]
    suspects <- unique(suspects[, c("name", "SMILES")])
    parents <- data.table::rbindlist(list(parents, suspects), use.names = TRUE, fill = TRUE)
    parents <- parents[!duplicated(parents$name), ]
  }

  if (x$parameters$use_compounds) {
    compounds <- get_compounds(nts)
    data.table::setnames(compounds, old = c("compoundName"), new = c("name"))
    if (!all(c("SMILES", "name") %in% colnames(compounds))) {
      warning("Compounds do not have required columns 'name' and 'SMILES'! Not done.")
      return(FALSE)
    }
    compounds <- compounds[!is.na(compounds$SMILES) & compounds$SMILES != ""]
    compounds <- unique(compounds[, c("name", "SMILES")])
    parents <- data.table::rbindlist(list(parents, compounds), use.names = TRUE, fill = TRUE)
    parents <- parents[!duplicated(parents$name), ]
  }

  if (nrow(parents) == 0) {
    warning("No parents provided! Not done.")
    return(FALSE)
  }

  parameters <- x$parameters
  parameters$use_suspects <- NULL
  parameters$use_compounds <- NULL
  parameters$parents <- data.table::as.data.table(parents)

  if (all(is.na(parameters$parents$SMILES)) || all(parameters$parents$SMILES == "")) {
    warning("No valid SMILES structures provided in parents! Not done.")
    return(FALSE)
  }
  parameters$parents[] <- lapply(parameters$parents, as.character)
  tryCatch({
    tps <- patRoon::generateTPsBioTransformer(
      parents = parameters$parents,
      type = parameters$type,
      generations = parameters$generations,
      maxExpGenerations = parameters$maxExpGenerations,
      extraOpts = parameters$extraOpts,
      skipInvalid = parameters$skipInvalid,
      prefCalcChemProps = parameters$prefCalcChemProps,
      neutralChemProps = parameters$neutralChemProps,
      neutralizeTPs = parameters$neutralizeTPs,
      calcSims = parameters$calcSims,
      fpType = parameters$fpType,
      fpSimMethod = parameters$fpSimMethod,
      MP = parameters$MP
    )

    if (is.null(tps) || length(tps) == 0) {
      warning("No transformation products generated! Not done.")
      return(FALSE)
    }

    if (all(is.na(parents$mz))) tps@parents[["mz"]] <- NULL
    tps@parents[["mass"]]  <- tps@parents[["neutralMass"]]
    tps@parents[["neutralMass"]] <- NULL

    tps@products <- lapply(tps@products, function(tp) {
      if (all(is.na(parents$mz))) tp[["mz"]] <- NULL
      tp[["mass"]]  <- tp[["neutralMass"]]
      tp[["neutralMass"]] <- NULL
      tp$parent_name <- tp$parent_ID
      for (z in seq_len(nrow(tp))) {
        if (!is.na(tp$parent_ID[z])) {
          tp$parent_name[z] <- tp$name[tp$parent_ID[z]]
        }
      }
      tp
    })

    tps@products <- Map(function(z, y) {
      if (nrow(z) > 0) {
        z$parent_name[is.na(z$parent_name)] <- y
      }
      z
    }, tps@products, names(tps@products))

    tp_results <- MassSpecResults_TransformationProducts(
      parents = tps@parents,
      transformation_products = tps@products
    )

    engine$Results <- tp_results
    message("\U2713 Transformation products generation completed with BioTransformer!")
    return(TRUE)
  }, error = function(e) {
    warning("Error during transformation products generation: ", e$message)
    FALSE
  })
}

# MARK: MassSpecMethod_GenerateTransformationProducts_cts

#' @title Mass Spectrometry Method for generating transformation products using the Chemical Transformation Simulator (CTS) via patRoon.
#' @description Generates transformation products using the \href{https://qed.epa.gov/cts/}{Chemical Transformation Simulator (CTS)} tool via \href{https://github.com/rickhelmus/patRoon}{patRoon} integration. This method is a wrapper of the `generateTPsCTS` function from the patRoon package. See more details in \href{https://rickhelmus.github.io/patRoon/reference/generateTPsCTS.html}{here}.
#' @param parents The parents provided as data.table with at least 'name' and 'SMILES' columns for which transformation products.
#' @param use_suspects Logical. If TRUE suspects in the MassSpecResults_NonTargetAnalysis will be used as parents.
#' @param use_compounds Logical. If TRUE compounds in the MassSpecResults_NonTargetAnalysis will be used as parents. If more more than one structure is available for a compound, only the first one will be used as it is assumed that this is the most relevant one (i.e., the highest score).
#' @param transLibrary A character specifying which transformation library should be used. Currently supported are: "hydrolysis", "abiotic_reduction", "photolysis_unranked", "photolysis_ranked", "mammalian_metabolism", "combined_abioticreduction_hydrolysis", "combined_photolysis_abiotic_hydrolysis", "pfas_environmental", "pfas_metabolism".
#' @param generations Integer. The number of generations (steps) for the predictions. Default is 1.
#' @param errorRetries Integer. The number of times to retry if an error occurs during processing of a parent. Default is 3.
#' @param skipInvalid Logical. If set to TRUE then the parents will be skipped (with a warning) for which insufficient information (e.g. SMILES) is available. Default is TRUE.
#' @param prefCalcChemProps Logical. If TRUE then calculated chemical properties such as the formula and InChIKey are preferred over what is already present in the parent suspect list. Default is TRUE.
#' @param neutralChemProps Logical. If TRUE then the neutral form of the molecule is considered to calculate SMILES, formulae etc. Default is FALSE.
#' @param neutralizeTPs Logical. If TRUE then all resulting TP structure information is neutralized. Default is TRUE.
#' @param calcLogP Character. Method to use for LogP calculation. Currently supported are "rcdk" (default), "obabel" or "none".
#' @param calcSims Logical. If set to TRUE then structural similarities between the parent and its TPs are calculated. Default is FALSE.
#' @param fpType Character. The type of structural fingerprint that should be calculated. Default is "extended".
#' @param fpSimMethod Character. The method for calculating similarities. Default is "tanimoto".
#' @param parallel Logical. If TRUE then parallel processing is enabled. Default is TRUE.
#' @return A `MassSpecMethod_GenerateTransformationProducts_cts` object.
#' @export
#'
MassSpecMethod_GenerateTransformationProducts_cts <- function(
  parents = data.table::data.table(name = character(), SMILES = character()),
  use_suspects = FALSE,
  use_compounds = FALSE,
  transLibrary = character(),
  generations = 1,
  errorRetries = 3,
  skipInvalid = TRUE,
  prefCalcChemProps = TRUE,
  neutralChemProps = FALSE,
  neutralizeTPs = TRUE,
  calcLogP = "rcdk",
  calcSims = FALSE,
  fpType = "extended",
  fpSimMethod = "tanimoto",
  parallel = TRUE
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "GenerateTransformationProducts",
    required = "FindFeatures",
    algorithm = "cts",
    input_class = "MassSpecResults_NonTargetAnalysis",
    output_class = "MassSpecResults_TransformationProducts",
    parameters = list(
      parents = data.table::as.data.table(parents),
      use_suspects = as.logical(use_suspects),
      use_compounds = as.logical(use_compounds),
      transLibrary = as.character(transLibrary),
      generations = as.integer(generations),
      errorRetries = as.integer(errorRetries),
      skipInvalid = as.logical(skipInvalid),
      prefCalcChemProps = as.logical(prefCalcChemProps),
      neutralChemProps = as.logical(neutralChemProps),
      neutralizeTPs = as.logical(neutralizeTPs),
      calcLogP = as.character(calcLogP),
      calcSims = as.logical(calcSims),
      fpType = as.character(fpType),
      fpSimMethod = as.character(fpSimMethod),
      parallel = as.logical(parallel)
    ),
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "Chemical Transformation Simulator",
    developer = "U.S. Environmental Protection Agency",
    contact = "https://www.epa.gov/aboutepa/forms/contact-epa",
    link = "https://qed.epa.gov/cts/",
    doi = "https://scholarsarchive.byu.edu/cgi/viewcontent.cgi?article=1302&context=iemssconference"
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecMethod_GenerateTransformationProducts_cts object!")
  }
}

#' @export
#' @noRd
validate_object.MassSpecMethod_GenerateTransformationProducts_cts <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "GenerateTransformationProducts")
  checkmate::assert_choice(x$algorithm, "cts")
  checkmate::assert_data_table(x$parameters$parents)
  if (nrow(x$parameters$parents) > 0) {
    required_cols <- c("name", "SMILES")
    missing_cols <- setdiff(required_cols, colnames(x$parameters$parents))
    if (length(missing_cols) > 0) {
      stop(
        "Parents data.table must contain the following columns: ",
        paste(missing_cols, collapse = ", ")
      )
    }
  }
  checkmate::assert_logical(x$parameters$use_suspects, len = 1)
  checkmate::assert_logical(x$parameters$use_compounds, len = 1)
  checkmate::assert_character(x$parameters$transLibrary, min.len = 1)
  checkmate::assert_choice(
    x$parameters$transLibrary,
    choices = c(
      "hydrolysis", "abiotic_reduction", "photolysis_unranked",
      "photolysis_ranked", "mammalian_metabolism",
      "combined_abioticreduction_hydrolysis",
      "combined_photolysis_abiotic_hydrolysis",
      "pfas_environmental", "pfas_metabolism"
    ),
    null.ok = FALSE
  )
  checkmate::assert_int(x$parameters$generations, lower = 1)
  checkmate::assert_int(x$parameters$errorRetries, lower = 1)
  checkmate::assert_logical(x$parameters$skipInvalid, len = 1)
  checkmate::assert_logical(x$parameters$prefCalcChemProps, len = 1)
  checkmate::assert_logical(x$parameters$neutralChemProps, len = 1)
  checkmate::assert_logical(x$parameters$neutralizeTPs, len = 1)
  checkmate::assert_character(x$parameters$calcLogP, len = 1)
  checkmate::assert_choice(x$parameters$calcLogP, choices = c("rcdk", "obabel", "none"))
  checkmate::assert_logical(x$parameters$calcSims, len = 1)
  checkmate::assert_character(x$parameters$fpType, len = 1)
  checkmate::assert_character(x$parameters$fpSimMethod, len = 1)
  checkmate::assert_logical(x$parameters$parallel, len = 1)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_GenerateTransformationProducts_cts <- function(x, engine, ...) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (is.null(engine$Results$MassSpecResults_NonTargetAnalysis)) {
    warning("No MassSpecResults_NonTargetAnalysis object available! Not done.")
    return(FALSE)
  }

  nts <- engine$Results$MassSpecResults_NonTargetAnalysis

  parents <- x$parameters$parents
  if (x$parameters$use_suspects) {
    suspects <- get_suspects(nts)
    if (!all(c("SMILES", "name") %in% colnames(suspects))) {
      warning("Suspects do not have required columns 'name' and 'SMILES'! Not done.")
      return(FALSE)
    }
    suspects <- suspects[!is.na(suspects$SMILES) & suspects$SMILES != ""]
    suspects <- unique(suspects[, c("name", "SMILES")])
    parents <- data.table::rbindlist(list(parents, suspects), use.names = TRUE, fill = TRUE)
    parents <- parents[!duplicated(parents$name), ]
  }

  if (x$parameters$use_compounds) {
    compounds <- get_compounds(nts)
    data.table::setnames(compounds, old = c("compoundName"), new = c("name"))
    if (!all(c("SMILES", "name") %in% colnames(compounds))) {
      warning("Compounds do not have required columns 'name' and 'SMILES'! Not done.")
      return(FALSE)
    }
    compounds <- compounds[!is.na(compounds$SMILES) & compounds$SMILES != ""]
    compounds <- unique(compounds[, c("name", "SMILES")])
    parents <- data.table::rbindlist(list(parents, compounds), use.names = TRUE, fill = TRUE)
    parents <- parents[!duplicated(parents$name), ]
  }

  if (nrow(parents) == 0) {
    warning("No parents provided! Not done.")
    return(FALSE)
  }

  parameters <- x$parameters
  parameters$use_suspects <- NULL
  parameters$use_compounds <- NULL
  parameters$parents <- data.table::as.data.table(parents)

  if (any(duplicated(parameters$parents$name))) {
    parameters$parents <- parameters$parents[!duplicated(parameters$parents$name), ]
    warning("Duplicate parent names found! Duplicates removed.")
  }

  if (all(is.na(parameters$parents$SMILES)) || all(parameters$parents$SMILES == "")) {
    warning("No valid SMILES structures provided in parents! Not done.")
    return(FALSE)
  }
  parameters$parents[] <- lapply(parameters$parents, as.character)
  tryCatch({
    tps <- patRoon::generateTPsCTS(
      parents = parameters$parents,
      transLibrary = parameters$transLibrary,
      generations = parameters$generations,
      errorRetries = parameters$errorRetries,
      skipInvalid = parameters$skipInvalid,
      prefCalcChemProps = parameters$prefCalcChemProps,
      neutralChemProps = parameters$neutralChemProps,
      neutralizeTPs = parameters$neutralizeTPs,
      calcLogP = parameters$calcLogP,
      calcSims = parameters$calcSims,
      fpType = parameters$fpType,
      fpSimMethod = parameters$fpSimMethod,
      parallel = parameters$parallel
    )

    if (is.null(tps) || length(tps) == 0) {
      warning("No transformation products generated! Not done.")
      return(FALSE)
    }

    if (all(is.na(parents$mz))) tps@parents[["mz"]] <- NULL
    tps@parents[["mass"]]  <- tps@parents[["neutralMass"]]
    tps@parents[["neutralMass"]] <- NULL

    tps@products <- lapply(tps@products, function(tp) {
      if (all(is.na(parents$mz))) tp[["mz"]] <- NULL
      tp[["mass"]]  <- tp[["neutralMass"]]
      tp[["neutralMass"]] <- NULL
      tp$parent_name <- tp$parent_ID
      for (z in seq_len(nrow(tp))) {
        if (!is.na(tp$parent_ID[z])) {
          tp$parent_name[z] <- tp$name[tp$parent_ID[z]]
        }
      }
      tp
    })

    tps@products <- Map(function(z, y) {
      if (nrow(z) > 0) {
        z$parent_name[is.na(z$parent_name)] <- y
      }
      z
    }, tps@products, names(tps@products))

    tp_results <- MassSpecResults_TransformationProducts(
      parents = tps@parents,
      transformation_products = tps@products
    )

    browser()

    engine$Results <- tp_results
    message("\U2713 Transformation products generation completed with CTS!")
    return(TRUE)
  }, error = function(e) {
    warning("Error during transformation products generation: ", e$message)
    FALSE
  })
}

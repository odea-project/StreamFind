#' @title MassSpecMethod_FindTransformationProducts_native Class
#' @description Processing method for finding transformation products using a MassSpecResults_TransformationProducts object to perform suspect screening on the MassSpecResults_NonTargetAnalysis object. Returns an updated MassSpecResults_NonTargetAnalysis with the TPs added as suspects. Note that existing suspects are not replaced if they are parents.
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @return A `MassSpecMethod_FindTransformationProducts_native` object.
#' @export
#'
MassSpecMethod_FindTransformationProducts_native <- function(
  ppm = 5,
  sec = 10
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "FindTransformationProducts",
    required = c("FindFeatures", "GenerateTransformationProducts"),
    algorithm = "native",
    input_class = c("MassSpecResults_NonTargetAnalysis", "MassSpecResults_TransformationProducts"),
    output_class = "MassSpecResults_NonTargetAnalysis",
    parameters = list(
      "ppm" = as.numeric(ppm),
      "sec" = as.numeric(sec)
    ),
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecMethod_FindTransformationProducts_native object!")
  }
}

#' @export
#' @noRd
validate_object.MassSpecMethod_FindTransformationProducts_native <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "FindTransformationProducts")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_number(x$parameters$ppm)
  checkmate::assert_number(x$parameters$sec)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_FindTransformationProducts_native <- function(x, engine = NULL) {
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

  if (is.null(engine$Results$MassSpecResults_TransformationProducts)) {
    warning("No MassSpecResults_TransformationProducts object available! Not done.")
    return(FALSE)
  }

  nts <- engine$Results$MassSpecResults_NonTargetAnalysis
  tp_results <- engine$Results$MassSpecResults_TransformationProducts

  if (
    sum(vapply(nts$features, function(z) nrow(z), 0)) == 0
  ) {
    warning("MassSpecResults_NonTargetAnalysis object does not have features! Not done.")
    return(FALSE)
  }

  if (length(tp_results$transformation_products) == 0) {
    warning("MassSpecResults_TransformationProducts object does not have transformation products! Not done.")
    return(FALSE)
  }

  tp_database <- data.table::rbindlist(tp_results$transformation_products, idcol = "parents", fill = TRUE)

  if (nrow(tp_database) == 0) {
    warning("No transformation products found in MassSpecResults_TransformationProducts! Not done.")
    return(FALSE)
  }

  # Ensure the TP database has the required columns for suspect screening
  if (!("name" %in% colnames(tp_database))) {
    if ("SMILES" %in% colnames(tp_database)) {
      # Use SMILES as name if name is not available
      tp_database$name <- tp_database$SMILES
    } else {
      # Generate sequential names if no name/SMILES available
      tp_database$name <- paste0("TP_", seq_len(nrow(tp_database)))
    }
  }

  if (!("mass" %in% colnames(tp_database))) {
    if ("neutralMass" %in% colnames(tp_database)) {
      tp_database$mass <- tp_database$neutralMass
    } else if ("mz" %in% colnames(tp_database)) {
      tp_database$mass <- tp_database$mz
    } else {
      warning("Transformation products database does not have mass, neutralMass, or mz columns! Not done.")
      return(FALSE)
    }
  }

  # Combine user-provided database with transformation products database
  parameters <- x$parameters
  combined_database <- data.table::rbindlist(
    list(parameters$database, tp_database),
    fill = TRUE,
    use.names = TRUE
  )

  # Remove duplicates based on name and mass
  combined_database <- unique(combined_database, by = c("name", "mass"))

  if (nrow(combined_database) == 0) {
    warning("Combined database is empty! Not done.")
    return(FALSE)
  }

  # Perform suspect screening with transformation products
  suspect_features <- get_suspects(
    nts,
    database = combined_database,
    ppm = parameters$ppm,
    sec = parameters$sec,
    ppmMS2 = parameters$ppmMS2,
    mzrMS2 = parameters$mzrMS2,
    minCusiness = parameters$minCusiness,
    minFragments = parameters$minFragments,
    filtered = parameters$filtered
  )

  if (nrow(suspect_features) > 0) {
    suspect_cols <- colnames(suspect_features)
    suspect_features_l <- split(suspect_features, suspect_features$analysis)
    features <- nts$features
    sus_col <- lapply(
      names(features),
      function(a, features, suspect_features_l, suspect_cols) {
        suspects <- suspect_features_l[[a]]
        fts <- features[[a]]
        if (!is.null(suspects)) {
          suspects_l <- lapply(
            fts$feature,
            function(z, suspects, suspect_cols) {
              sus_idx <- which(suspects$feature %in% z)
              if (length(sus_idx) > 0) {
                sus_temp <- suspects[sus_idx, ]
                sus_temp[["analysis"]] <- NULL
                if (nrow(sus_temp) > 0) {
                  sus_temp
                } else {
                  data.table::data.table()
                }
              } else {
                data.table::data.table()
              }
            },
            suspects = suspects,
            suspect_cols = suspect_cols
          )
          suspects_l
        } else {
          lapply(fts$feature, function(x) data.table::data.table())
        }
      },
      features = features,
      suspect_features_l = suspect_features_l,
      suspect_cols = suspect_cols
    )
    names(sus_col) <- names(features)
    features <- Map(
      function(fts, i) {
        fts$suspects <- i
        fts
      },
      features,
      sus_col
    )
    nts$features <- features
    engine$Results <- nts
    message("\U2713 Transformation products screening completed! Found ", nrow(suspect_features), " suspects.")
    TRUE
  } else {
    message("\U26a0 No transformation product suspects found!")
    FALSE
  }
}

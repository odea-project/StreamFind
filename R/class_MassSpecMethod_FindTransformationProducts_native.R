#' @title MassSpecMethod_FindTransformationProducts_native Class
#' @description Processing method for finding transformation products using a MassSpecResults_TransformationProducts object to perform suspect screening on the MassSpecResults_NonTargetAnalysis object. Returns an updated MassSpecResults_NonTargetAnalysis with the TPs added as suspects. Note that existing suspects are not replaced if they are parents.
#' @template arg-ms-ppm
#' @return A `MassSpecMethod_FindTransformationProducts_native` object.
#' @export
#'
MassSpecMethod_FindTransformationProducts_native <- function(
  ppm = 10
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "FindTransformationProducts",
    required = c("FindFeatures", "GenerateTransformationProducts"),
    algorithm = "native",
    input_class = c("MassSpecResults_NonTargetAnalysis", "MassSpecResults_TransformationProducts"),
    output_class = "MassSpecResults_NonTargetAnalysis",
    parameters = list(
      "ppm" = as.numeric(ppm)
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

  if (
    sum(vapply(nts$features, function(z) nrow(z), 0)) == 0
  ) {
    warning("MassSpecResults_NonTargetAnalysis object does not have features! Not done.")
    return(FALSE)
  }

  parent_results <- engine$Results$MassSpecResults_TransformationProducts$parents
  tp_results <- engine$Results$MassSpecResults_TransformationProducts

  if (length(tp_results$transformation_products) == 0) {
    warning("MassSpecResults_TransformationProducts object does not have transformation products! Not done.")
    return(FALSE)
  }

  tp_database <- data.table::rbindlist(tp_results$transformation_products, idcol = "parent", fill = TRUE)
  if ("neutralMass" %in% colnames(tp_database)) {
    data.table::setnames(tp_database, "neutralMass", "mass")
  }

  if (nrow(tp_database) == 0) {
    warning("No transformation products found in MassSpecResults_TransformationProducts! Not done.")
    return(FALSE)
  }

  fts <- nts$features
  rpls <- nts$info$replicate
  names(rpls) <- nts$info$analysis

  message("Finding parents for transformation products in features...", appendLF = FALSE)
  parents <- lapply(fts, function(a) {
    parent_fts <- data.table::data.table()
    for (i in seq_len(nrow(parent_results))) {
      parent_name <- parent_results$name[i]
      parent_smiles <- parent_results$SMILES[i]
      suspects_fts <- vapply(a$suspects, function(z) any(z$name == parent_name | z$SMILES == parent_smiles), FALSE)
      compound_fts <- vapply(a$compounds, function(z) any(z$name == parent_name | z$SMILES == parent_smiles), FALSE)
      if (!any(suspects_fts) & !any(compound_fts)) {
        next
      }
      if (any(suspects_fts)) {
        suspect_idx <- which(suspects_fts)
        for (s_idx in suspect_idx) {
          parent_temp <- parent_results[i, ]
          parent_temp$feature <- a$feature[s_idx]
          parent_temp$group <- a$group[s_idx]
          parent_temp$mz <- a$mz[s_idx]
          parent_temp$rt <- a$rt[s_idx]
          parent_temp$intensity <- a$intensity[s_idx]
          parent_temp$area <- a$area[s_idx]
          parent_temp$correction <- a$correction[s_idx]
          parent_temp$ms2 <- a$ms2[s_idx]
          parent_fts <- rbind(parent_fts, parent_temp, fill = TRUE)
        }
      }
      if (any(compound_fts)) {
        compound_idx <- which(compound_fts)
        for (c_idx in compound_idx) {
          parent_temp <- parent_results[i, ]
          parent_temp$feature <- a$feature[c_idx]
          parent_temp$group <- a$group[c_idx]
          parent_temp$mz <- a$mz[c_idx]
          parent_temp$rt <- a$rt[c_idx]
          parent_temp$intensity <- a$intensity[c_idx]
          parent_temp$area <- a$area[c_idx]
          parent_temp$correction <- a$correction[c_idx]
          parent_temp$ms2 <- a$ms2[c_idx]
          parent_fts <- rbind(parent_fts, parent_temp, fill = TRUE)
        }
      }
    }
    parent_fts
  })
  parents <- data.table::rbindlist(parents, idcol = "analysis", fill = TRUE)
  parents$replicate <- rpls[as.character(parents$analysis)]
  message(" Done! ")
  message("Finding transformation products in features...", appendLF = FALSE)
  tps <- lapply(names(nts$features), function(a) {
    tp <- data.table::data.table()
    for (i in seq_len(nrow(tp_database))) {
      tp_mass <- tp_database$mass[i]
      tp_mass_min <- tp_mass - (tp_mass * x$parameters$ppm / 1e6)
      tp_mass_max <- tp_mass + (tp_mass * x$parameters$ppm / 1e6)
      parent <- parents[parents$name %in% tp_database$parent[i], ]
      parent_rt <- mean(parent$rt, na.rm = TRUE)
      rt_direction <- tp_database$retDir[i]
      fts_idx <- which(
        nts$features[[a]]$mass >= tp_mass_min &
        nts$features[[a]]$mass <= tp_mass_max &
        !nts$features[[a]]$filtered
      )
      if (length(fts_idx) > 0) {
        for (ft in fts_idx) {
          if (rt_direction == -1) {
            if (parent_rt < nts$features[[a]]$rt[ft]) {
              next
            }
          } else if (rt_direction == 1) {
            if (parent_rt > nts$features[[a]]$rt[ft]) {
              next
            }
          }
          tp_temp <- tp_database[i, ]
          tp_temp$feature <- nts$features[[a]]$feature[ft]
          tp_temp$group <- nts$features[[a]]$group[ft]
          tp_temp$mz <- nts$features[[a]]$mz[ft]
          tp_temp$rt <- nts$features[[a]]$rt[ft]
          tp_temp$intensity <- nts$features[[a]]$intensity[ft]
          tp_temp$area <- nts$features[[a]]$area[ft]
          tp_temp$correction <- nts$features[[a]]$correction[ft]
          tp_temp$ms2 <- nts$features[[a]]$ms2[ft]
          tp <- rbind(tp, tp_temp, fill = TRUE)
        }
      }
    }
    tp
  })
  names(tps) <- names(nts$features)
  tps <- data.table::rbindlist(tps, idcol = "analysis", fill = TRUE)
  tps$replicate <- rpls[as.character(tps$analysis)]
  n_tps <- nrow(tps)
  if (n_tps > 0) {
    parents_not_found <- parent_results[!parent_results$name %in% parents$name, ]
    tps_not_found <- tp_database[!tp_database$name %in% tps$name, ]
    if (nrow(parents_not_found) > 0) {
      parents <- rbind(parents, parents_not_found, fill = TRUE)
    }
    if (nrow(tps_not_found) > 0) {
      tps <- rbind(tps, tps_not_found, fill = TRUE)
    }
    tps <- split(tps, tps$parent)
    new_tps <- MassSpecResults_TransformationProducts(
      parents = parents,
      transformation_products = tps
    )
    engine$Results <- new_tps
    message(" Done! Found ", n_tps, " transformation products. ")
  } else {
    message(" Done! No transformation products found. ")
  }
  
  message("\U2713 Transformation products screening completed!")
  TRUE
}

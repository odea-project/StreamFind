
#' build_feature_groups_table_from_patRoon
#'
#' @param pat An object with class `featureGroups` from the package \pkg{patRoon}.
#' @param features X.
#' @param self An `msData` object. When applied within the R6, the self object.
#'
#' @return A \linkS4class{data.table} with the feature groups.
#'
#' @noRd
#'
build_feature_groups_table_from_patRoon <- function(pat, features, self) {

  fgroups <- patRoon::as.data.table(pat, average = FALSE)

  valid = TRUE

  if (!is.data.frame(fgroups)) valid <- FALSE

  if (valid & nrow(fgroups) == 0) valid <- FALSE

  if (!valid) return(NULL)

  setnames(fgroups, "ret", "rt", skip_absent = TRUE)

  return(
    update_features_groups_correspondence(fgroups, features)
  )


  fts <- copy(features)
  fts <- rbindlist(fts, idcol = "analysis")





  group_vals <- lapply(fgroups$group, function(g, fts) {
    x <- which(fts$group == g)
    return(
      c(
        "rt" = round(mean(fts$rt[x]), 3),
        "rtmin" = min(fts$rtmin[x]),
        "rtmax" = max(fts$rtmax[x]),
        "mz" = round(mean(fts$mz[x]), 8),
        "mass" = round(mean(fts$mass[x]), 8),
        "mass_left" = max((fts$mz[x] - fts$mzmin[x]) / fts$mz[x] * 1E6),
        "mass_right" = max((fts$mzmax[x] - fts$mz[x]) / fts$mz[x] * 1E6),
        "filled" = TRUE %in% fts$filled[x]
      )
    )

  }, fts = fts)

  group_vals <- as.data.frame(do.call(rbind, group_vals))

  fgroups$rt <- group_vals$rt
  fgroups$rtdev <- round(group_vals$rtmax - group_vals$rtmin, 0)
  fgroups$mass <- group_vals$mass
  fgroups$massdev <- group_vals$mass_left + group_vals$mass_right
  fgroups$index <- as.numeric(sub(".*_", "", fgroups$group))
  fgroups$filled <- group_vals$filled

  if (!"filtered" %in% colnames(fgroups)) {
    fgroups$filtered <- FALSE
    fgroups$filter <- NA_character_
  }

  d_dig <- fts$feature
  d_dig <- sub('.*\\.(.*)_rt.*', '\\1', d_dig)
  d_dig <- max(nchar(d_dig))

  if (TRUE %in% grepl("Set", is(pat))) {
    setnames(fgroups, "mz", "mass", skip_absent = TRUE)
    fgroups$neutralMass <- NULL
    fgroups$adduct <- "[M]"

    new_id <- paste0(
      "m",
      round(fgroups$mass, d_dig),
      "_rt",
      round(fgroups$rt, 0),
      "_g",
      fgroups$index
    )
  } else {
    fgroups$mz <- group_vals$mz
    adduct <- unique(fts$adduct)
    fgroups$adduct <- adduct
    if (adduct %in% "[M+H]+") fgroups$mass <- fgroups$mz - 1.007276
    if (adduct %in% "[M-H]-") fgroups$mass <- fgroups$mz + 1.007276

    new_id <- paste0(
      "mz",
      round(fgroups$mz, digits = d_dig),
      "_rt",
      round(fgroups$rt, digits = 0),
      "_g",
      fgroups$index
    )
  }

  names(new_id) <- fgroups$group
  fgroups$group <- new_id

  features_new_id <- lapply(features, function(x, new_id) {
    x$group <- new_id[x$group]
    return(x)
  }, new_id = new_id)

  fgroups <- fgroups[order(fgroups$mass), ]
  fgroups <- fgroups[order(fgroups$rt), ]
  fgroups <- fgroups[order(fgroups$filtered), ]

  list("features" = features_new_id, "groups" = fgroups)
}

#' update_features_groups_correspondence
#'
#' Updates the groups data.table based on features in remaining msAnalysis.
#'
#' @param newGroups A copy of the groups data.table with columns of the analyses
#' names remaining.
#' @param newFeatures A list with a features data.table for each analyses
#' remaining.
#'
#' @noRd
#'
update_features_groups_correspondence <- function(newGroups, newFeatures) {

  fts <- rbindlist(newFeatures, idcol = "analysis")

  valid = TRUE

  if (!is.data.frame(fts)) valid <- FALSE

  if (valid & nrow(fts) == 0) valid <- FALSE

  if (!is.data.frame(newGroups)) valid <- FALSE

  if (valid & nrow(newGroups) == 0) valid <- FALSE

  if (!valid) return(NULL)

  setnames(newGroups, "ret", "rt", skip_absent = TRUE)
  newGroups$neutralMass <- NULL

  fgs_remaining <- unique(unlist(fts$group))
  fgs_remaining <- fgs_remaining[!is.na(fgs_remaining)]

  if (!is.null(fgs_remaining)) {
    newGroups <- newGroups[newGroups$group %in% fgs_remaining, ]
  } else {
    return(NULL)
  }

  group_vals <- rcpp_ms_get_feature_groups_ranges(newGroups$group, fts)

  newGroups$rt <- group_vals$rt
  newGroups$rtdev <- round(group_vals$rtmax - group_vals$rtmin, 0)
  newGroups$mass <- group_vals$mass
  newGroups$massdev <- round(group_vals$mass_left + group_vals$mass_right, 1)
  newGroups$filled <- group_vals$filled
  newGroups$index <- seq_len(length(newGroups$group))
  #
  #   if ("[M]" %in% group_vals$adduct) {
  #     newGroups$adduct <- "[M]"
  #   } else {
  newGroups$adduct <- group_vals$adduct
  #   }

  if (!"filtered" %in% colnames(newGroups)) {
    newGroups$filtered <- FALSE
    newGroups$filter <- NA_character_
  }

  d_dig <- fts$feature
  d_dig <- sub('.*\\.(.*)_rt.*', '\\1', d_dig)
  d_dig <- max(nchar(d_dig))

  if ("[M]" %in% unique(newGroups$adduct)) {
    newGroups$mz <- NULL

    new_id <- paste0(
      "m",
      round(newGroups$mass, digits = d_dig),
      "_rt",
      round(newGroups$rt, digits = 0),
      "_g",
      newGroups$index
    )

    cols_order <- c(
      "group", "mass", "rt", unique(fts$analysis),
      "rtdev", "massdev", "index", "filled", "filtered", "filter", "adduct"
    )

  } else {
    new_id <- paste0(
      "mz",
      round(newGroups$mz, digits = d_dig),
      "_rt",
      round(newGroups$rt, digits = 0),
      "_g",
      newGroups$index
    )

    cols_order <- c(
      "group", "mz", "rt", unique(fts$analysis), "rtdev", "massdev",
      "index", "filled", "filtered", "filter", "adduct", "mass"
    )
  }

  setcolorder(newGroups, cols_order)
  names(new_id) <- newGroups$group

  newFeatures_new_id <- lapply(newFeatures, function(x, new_id) {
    which_ft_to_change <- x$group %in% names(new_id)
    x$group[which_ft_to_change] <- new_id[x$group[which_ft_to_change]]
    x
  }, new_id = new_id)

  newGroups$group <- new_id

  newGroups <- newGroups[order(newGroups$mass), ]
  newGroups <- newGroups[order(newGroups$rt), ]
  newGroups <- newGroups[order(newGroups$filtered), ]

  list("features" = newFeatures_new_id, "groups" = newGroups)
}

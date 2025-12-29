
# MARK: native
# native ------

#' @title DB_MassSpecMethod_FeatureBlankSubtraction_native class
#' @description Native StreamFind method for blank subtraction of features.
#' @param blankThreshold numeric(1) threshold multiplier for blank feature intensities to consider a feature as present in blanks. To be present a features must be at least blankThreshold times more intense in samples than in blanks.
#' @param rtExpand numeric(1) retention time expansion window in seconds around feature RT for EIC extraction.
#' @param mzExpand numeric(1) m/z expansion window in Da around feature m/z for EIC extraction.
#' @export
#'
DB_MassSpecMethod_FeatureBlankSubtraction_native <- function(
  blankThreshold = 5,
  rtExpand = 10,
  mzExpand = 0.005
) {
  x <- ProcessingStep(
    type = "DB_MassSpec",
    method = "FeatureBlankSubtraction",
    required = "FindFeatures",
    algorithm = "native",
    input_class = "DB_MassSpecResults_NonTargetAnalysis",
    output_class = "DB_MassSpecResults_NonTargetAnalysis",
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_,
    parameters = list(
      blankThreshold = 5,
      rtExpand = rtExpand,
      mzExpand = mzExpand
    )
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid parameters for DB_MassSpecMethod_FeatureBlankSubtraction_native.")
  }
}

#' @export
#' @noRd
validate_object.DB_MassSpecMethod_FeatureBlankSubtraction_native <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "FeatureBlankSubtraction")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_numeric(x$parameters$blankThreshold, len = 1, lower = 0)
  NULL
}

#' @export
#' @noRd
run.DB_MassSpecMethod_FeatureBlankSubtraction_native <- function(x, engine = NULL) {
  if (!"DB_MassSpecResults_NonTargetAnalysis" %in% class(engine$NonTargetAnalysis)) {
    warning("Engine does not contain DB_MassSpecResults_NonTargetAnalysis.")
    return(FALSE)
  }

  nts <- engine$NonTargetAnalysis
  analyses <- query_db(nts, "SELECT * FROM Analyses")

  if (nrow(analyses) == 0) {
    warning("No analyses found in the DB_MassSpecResults_NonTargetAnalysis.")
    return(FALSE)
  }

  parameters <- x$parameters

  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash <- .make_hash(x, analyses, parameters)
    cache_info <- get_cache_info(cache_manager)
    fts <- load_cache(cache_manager, hash = hash)
    if (nrow(fts) > 0) {
      message("\U2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")
      db <- file.path(engine$get_project_path(), "DB_MassSpecResults_NonTargetAnalysis.duckdb")
      DB_MassSpecResults_NonTargetAnalysis(db, analyses, headers, fts)
      return(invisible(TRUE))
    }
  }

  analyses_sel <- analyses[!analyses$replicate %in% analyses$blank, ]
  fts <- query_db(nts, "SELECT * FROM Features")
  fts_list <- split(fts, fts$analysis)
  blk_thres <- parameters$blankThreshold

  fts_list <- lapply(fts_list, function(x) {
    ana <- x$analysis[1]
    blk_analyses <- analyses$analysis[analyses$replicate == analyses$blank[analyses$analysis == ana]]
    targets <- x[, c("feature", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "intensity")]
    targets$rtmin <- targets$rtmin - parameters$rtExpand
    targets$rtmax <- targets$rtmax + parameters$rtExpand
    targets$mzmin <- targets$mzmin - parameters$mzExpand
    targets$mzmax <- targets$mzmax + parameters$mzExpand
    data.table::setnames(targets, "feature", "id")
    eic_blks <- lapply(blk_analyses, function(blk) {
      get_spectra_eic(engine$Analyses, analyses = blk, mz = targets)
    })
    eic_blks <- data.table::rbindlist(eic_blks)
    eic_blks <- eic_blks[order(intensity, decreasing = TRUE), ]
    eic_blks <- eic_blks[, .(intensity_blank = max(head(intensity, 2))), by = .(id, analysis)]
    eic_blks <- eic_blks[, .(intensity_blank = mean(intensity_blank)), by = .(id)]
    eic_blks[, id := sub(" .*", "", id)]
    merged <- merge(targets, eic_blks, by = "id", all.x = TRUE)
    merged$intensity_blank[is.na(merged$intensity_blank)] <- 0
    filtered_ids <- merged$id[merged$intensity < (merged$intensity_blank * blk_thres)]
    x$filtered[x$feature %in% filtered_ids] <- TRUE
    x$filter[x$feature %in% filtered_ids] <- "blank_subtraction"
    x
  })

  names(fts_list) <- analyses$analysis
  fts <- data.table::rbindlist(fts_list, fill = TRUE)

  # save_cache(
  #   cache_manager,
  #   name = paste0("DB_FeatureBlankSubtraction_native"),
  #   hash = .make_hash(x, analyses, parameters),
  #   description = "Features found with DB_FeatureBlankSubtraction_native method.",
  #   data = fts
  # )
  # message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")
  db <- file.path(engine$get_project_path(), "DB_MassSpecResults_NonTargetAnalysis.duckdb")
  invisible(DB_MassSpecResults_NonTargetAnalysis(db, NULL, NULL, fts))
  invisible(TRUE)
}

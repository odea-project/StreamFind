#' DB_MassSpecMethod_FilterSuspects_native Class
#'
#' @description Settings for filtering suspects in DB_MassSpecResults_NonTargetAnalysis objects based on suspect properties.
#'
#' @param names Character vector with suspect names to match (via partial matching). Empty vector means no name filtering.
#' @param minScore Numeric (length 1) with the minimum score.
#' @param maxErrorRT Numeric (length 1) with the maximum absolute retention time error in seconds.
#' @param maxErrorMass Numeric (length 1) with the maximum absolute mass error in ppm.
#' @param idLevels Integer vector with the identification levels to keep (e.g., c(1, 2, 3)). Empty vector means no level filtering.
#' @param minSharedFragments Integer (length 1) with the minimum number of shared fragments.
#' @param minCosineSimilarity Numeric (length 1) with the minimum cosine similarity (0-1).
#'
#' @return A `DB_MassSpecMethod_FilterSuspects_native` object.
#'
#' @export
#'
DB_MassSpecMethod_FilterSuspects_native <- function(
  names = character(0),
  minScore = NA_real_,
  maxErrorRT = NA_real_,
  maxErrorMass = NA_real_,
  idLevels = integer(0),
  minSharedFragments = 0,
  minCosineSimilarity = NA_real_
) {
  x <- ProcessingStep(
    type = "DB_MassSpec",
    method = "FilterSuspects",
    required = "SuspectScreening",
    algorithm = "native",
    input_class = "DB_MassSpecResults_NonTargetAnalysis",
    output_class = "DB_MassSpecResults_NonTargetAnalysis",
    parameters = list(
      names = as.character(names),
      minScore = as.numeric(minScore),
      maxErrorRT = as.numeric(maxErrorRT),
      maxErrorMass = as.numeric(maxErrorMass),
      idLevels = as.integer(idLevels),
      minSharedFragments = as.integer(minSharedFragments),
      minCosineSimilarity = as.numeric(minCosineSimilarity)
    ),
    number_permitted = Inf,
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
    stop("Invalid DB_MassSpecMethod_FilterSuspects_native object!")
  }
}

#' @export
#' @noRd
#'
validate_object.DB_MassSpecMethod_FilterSuspects_native <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "FilterSuspects")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_character(x$parameters$names)
  checkmate::assert_numeric(x$parameters$minScore, len = 1)
  checkmate::assert_numeric(x$parameters$maxErrorRT, len = 1)
  checkmate::assert_numeric(x$parameters$maxErrorMass, len = 1)
  checkmate::assert_integerish(x$parameters$idLevels)
  checkmate::assert_integerish(x$parameters$minSharedFragments, len = 1)
  checkmate::assert_numeric(x$parameters$minCosineSimilarity, len = 1)
  NULL
}

#' @export
#' @noRd
#'
run.DB_MassSpecMethod_FilterSuspects_native <- function(
  x,
  engine = NULL
) {
  if (!is(engine, "DB_MassSpecEngine")) {
    warning("Engine is not a DB_MassSpecEngine object!")
    return(FALSE)
  }

  if (is.null(engine$NonTargetAnalysis)) {
    warning("No DB_MassSpecResults_NonTargetAnalysis object available! Not done.")
    return(FALSE)
  }

  nts <- engine$NonTargetAnalysis
  analyses_info <- info(engine$Analyses)
  parameters <- x$parameters

  # Check cache
  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash <- .make_hash(x, analyses_info, parameters, engine$Workflow)
    cache_info <- get_cache_info(cache_manager)
    if (nrow(cache_info) > 0) {
      sus <- load_cache(cache_manager, hash = hash)
      if (!is.null(sus) && is.data.frame(sus)) {
        if (nrow(sus) > 0) {
          message("\U2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")
          # Ensure id_level is integer
          sus$id_level <- as.integer(sus$id_level)
          conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
          on.exit(DBI::dbDisconnect(conn), add = TRUE)
          DBI::dbExecute(conn, "DELETE FROM Suspects")
          DBI::dbWriteTable(conn, "Suspects", sus, append = TRUE)
          message("\U2713 Suspects written to database.")
          return(invisible(TRUE))
        }
      }
    }
  }

  # Query all suspects from database
  sus <- query_db(nts, "SELECT * FROM Suspects")

  if (nrow(sus) == 0) {
    warning("No suspects found in DB_MassSpecResults_NonTargetAnalysis! Not done.")
    return(FALSE)
  }

  # Ensure id_level is integer
  sus$id_level <- as.integer(sus$id_level)

  # Count suspects before filtering
  n_before <- nrow(sus)
  message("\U2139 Filtering ", n_before, " suspects...")
  message("\U2139 ID level distribution before filtering: ", paste(names(table(sus$id_level)), "=", table(sus$id_level), collapse=", "))

  analyses_db <- query_db(engine$Analyses, "SELECT * FROM Analyses")

  suspect_list <- lapply(analyses_db$analysis, function(ana) {
    ana_suspects <- sus[sus$analysis == ana, ]
    if (nrow(ana_suspects) == 0) {
      return(sus[0, ])
    }
    ana_suspects
  })
  names(suspect_list) <- analyses_db$analysis

  sus_list <- rcpp_nts_filter_suspects(
    info = analyses_db,
    suspect_list = suspect_list,
    names = parameters$names,
    minScore = parameters$minScore,
    maxErrorRT = parameters$maxErrorRT,
    maxErrorMass = parameters$maxErrorMass,
    idLevels = parameters$idLevels,
    minSharedFragments = parameters$minSharedFragments,
    minCosineSimilarity = parameters$minCosineSimilarity
  )

  if (is.null(sus_list) || length(sus_list) == 0) {
    warning("Suspect filtering failed.")
    return(FALSE)
  }

  names(sus_list) <- analyses_db$analysis
  sus <- data.table::rbindlist(sus_list, fill = TRUE)

  # Count suspects after filtering
  n_after <- nrow(sus)
  n_filtered <- n_before - n_after

  message(sprintf("\u2713 FilterSuspects complete: %d suspects filtered, %d remaining", n_filtered, n_after))
  if (n_after > 0) {
    message("\U2139 ID level distribution after filtering: ", paste(names(table(sus$id_level)), "=", table(sus$id_level), collapse=", "))
  }

  # Save to cache
  if (!is.null(cache_manager)) {
    save_cache(
      cache_manager,
      name = paste0("DB_FilterSuspects_native"),
      hash = .make_hash(x, analyses_info, parameters, engine$Workflow),
      description = "Suspects filtered with DB_FilterSuspects_native method",
      data = as.data.frame(sus)
    )
    message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")
  }

  # Write to database
  conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # Clear existing data
  DBI::dbExecute(conn, "DELETE FROM Suspects")

  # Ensure id_level is integer before writing
  if (nrow(sus) > 0) {
    sus$id_level <- as.integer(sus$id_level)
  }

  # Write new data
  DBI::dbWriteTable(conn, "Suspects", sus, append = TRUE)

  message("\U2713 Suspects written to database.")
  invisible(TRUE)
}

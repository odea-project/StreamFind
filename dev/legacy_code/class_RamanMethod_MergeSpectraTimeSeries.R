#' @title RamanMethod_MergeSpectraTimeSeries_StreamFind
#'
#' @description Merges Raman spectra based on time series data. It collapses data files into a single file.
#'
#' @param preCut The number of pre Raman scans to exclude when merging.
#'
#' @return A RamanMethod_MergeSpectraTimeSeries_StreamFind object.
#'
#' @export
#'
RamanMethod_MergeSpectraTimeSeries_StreamFind <- function(preCut = 2) {
  x <- ProcessingStep(
    type = "Raman",
    method = "MergeSpectraTimeSeries",
    required = NA_character_,
    algorithm = "StreamFind",
    parameters = list(preCut = preCut),
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
    stop("Invalid RamanMethod_MergeSpectraTimeSeries_StreamFind object!")
  }
}

#' @export
#' @noRd
validate_object.RamanMethod_MergeSpectraTimeSeries_StreamFind <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "MergeSpectraTimeSeries")
  checkmate::assert_choice(x$algorithm, "StreamFind")
  checkmate::assert_number(x$parameters$preCut)
  NextMethod()
  NULL
}


#' @export
#' @noRd
run.RamanMethod_MergeSpectraTimeSeries_StreamFind <- function(
  x,
  engine = NULL
) {
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  if (is.null(engine$Results[["RamanResults_Spectra"]])) {
    engine$Results <- RamanResults_Spectra(
      lapply(engine$Analyses$analyses, function(a) a$spectra)
    )
  }
  spec_obj <- engine$Results[["RamanResults_Spectra"]]
  preCut <- x$parameters$preCut
  rpls <- get_replicate_names(engine$Analyses)
  urpls <- unique(rpls)
  unified <- lapply(urpls, function(x) {
    anas <- names(rpls)[rpls %in% x]
    anasl <- engine$Analyses$analyses[anas]
    cached_merged_analysis <- FALSE
    merged_analysis <- NULL
    cache <- .load_cache_sqlite("merged_raman_analysis", x, anas, anasl)
    if (!is.null(cache$data)) {
      merged_analysis <- cache$data
      if (!is.null(merged_analysis)) {
        message("\U2139 Merged Raman analysis loaded from cache!")
        cached_merged_analysis <- TRUE
      }
    } else {
      merged_analysis <- NULL
    }
    if (is.null(merged_analysis) & !cached_merged_analysis) {
      rtvec <- vapply(
        anasl,
        function(z) as.numeric(z$metadata$`Accumulate Cycle Time (secs)`),
        NA_real_
      )
      rtvec <- cumsum(unname(rtvec))
      spectral <- lapply(anasl, function(z) z$spectra)
      spectral <- spectral[-(1:preCut)]
      names(spectral) <- as.character(rtvec[-(1:preCut)])
      spectra <- data.table::rbindlist(spectral, idcol = "rt")
      spectra$rt <- as.numeric(spectra$rt)
      data.table::setcolorder(spectra, c("rt"))
      message("\U2699 Writting unified analysis file...", appendLF = FALSE)
      ana_name <- x
      ana_dir <- dirname(anasl[[1]]$file)
      ana_ext <- file_ext(anasl[[1]]$file)
      new_file <- paste0(ana_dir, "/", ana_name, ".", ana_ext)
      ana_metadata <- anasl[[1]]$metadata
      if (file.exists(new_file)) {
        file.remove(new_file)
      }
      rcpp_write_asc_file(file = new_file, ana_metadata, as.matrix(spectra))
      merged_analysis <- list(
        "name" = ana_name,
        "replicate" = ana_name,
        "blank" = NA_character_,
        "file" = new_file,
        "metadata" = ana_metadata,
        "spectra" = spectra
      )
      message(" Done!")
      if (!is.null(cache$hash)) {
        .save_cache_sqlite("merged_raman_analysis", merged_analysis, cache$hash)
        message("\U1f5ab Merged Raman analysis cached!")
      }
    } else {
      if (!file.exists(merged_analysis$file)) {
        message("\U2699 Writting unified analysis file...", appendLF = FALSE)
        rcpp_write_asc_file(
          file = merged_analysis$file,
          merged_analysis$metadata,
          as.matrix(merged_analysis$spectra)
        )
        message(" Done!")
      }
    }
    merged_analysis
  })
  names(unified) <- urpls
  if (!is.null(unified)) {
    if (
      all(
        vapply(unified, function(x) is(x), NA_character_) %in% "RamanAnalysis"
      )
    ) {
      to_remove <- names(engine$Analyses)[
        get_replicate_names(engine$Analyses) %in% names(unified)
      ]
      suppressMessages(remove(engine$Analyses, to_remove))
      engine$Analyses <- add(engine$Analyses, unified)
      TRUE
    } else {
      FALSE
    }
  } else {
    FALSE
  }
}

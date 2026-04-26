# MARK: MassSpecMethod_LoadSpectra_native
#' @title MassSpecMethod_LoadSpectra_native class
#' @description Load MS spectra from analysis files into a DuckDB-backed Spectra results object.
#' @param levels  MS levels to load (default c(1L, 2L)).
#' @param minIntensityMS1  Minimum intensity for MS1 peaks.
#' @param minIntensityMS2  Minimum intensity for MS2 peaks.
#' @export
MassSpecMethod_LoadSpectra_native <- function(
    levels          = c(1L, 2L),
    minIntensityMS1 = 0.0,
    minIntensityMS2 = 0.0) {
  x <- ProcessingStep(
    type        = "MassSpec",
    method      = "LoadSpectra",
    required    = NA_character_,
    algorithm   = "native",
    input_class = "MassSpecAnalyses",
    output_class = "MassSpecResults_Spectra",
    number_permitted = 1,
    version     = as.character(packageVersion("StreamFind")),
    software    = "StreamFind",
    developer   = "Ricardo Cunha",
    contact     = "cunha@iuta.de",
    link        = "https://odea-project.github.io/StreamFind",
    doi         = NA_character_,
    parameters  = list(
      levels          = as.integer(levels),
      minIntensityMS1 = as.numeric(minIntensityMS1),
      minIntensityMS2 = as.numeric(minIntensityMS2)
    )
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_LoadSpectra_native parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_LoadSpectra_native <- function(x) {
  checkmate::assert_choice(x$method, "LoadSpectra")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_integerish(x$parameters$levels, min.len = 1)
  checkmate::assert_numeric(x$parameters$minIntensityMS1, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$minIntensityMS2, len = 1, lower = 0)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_LoadSpectra_native <- function(x, engine = NULL) {
  if (!"MassSpecAnalyses" %in% class(engine$Analyses)) {
    warning("Engine does not contain MassSpecAnalyses.")
    return(FALSE)
  }
  analyses <- query_db(engine$Analyses, "SELECT * FROM Analyses")
  if (nrow(analyses) == 0) { warning("No analyses found."); return(FALSE) }

  headers <- query_db(engine$Analyses, "SELECT * FROM SpectraHeaders")
  headers_split <- split(headers, headers$analysis)

  p <- x$parameters
  targets_empty <- MassSpecTargets()  # default empty targets

  spec_list <- lapply(seq_len(nrow(analyses)), function(i) {
    aname <- analyses$analysis[i]
    arep  <- analyses$replicate[i]
    afile <- analyses$file[i]
    hd_a  <- headers_split[[aname]]
    if (is.null(hd_a) || nrow(hd_a) == 0) return(NULL)
    tryCatch({
      message("\U2699 Loading spectra from ", basename(afile), "...", appendLF = FALSE)
      raw <- rcpp_streamcraft_parse_ms_spectra(
        list(file = afile, spectra_headers = hd_a),
        p$levels,
        targets_empty,
        p$minIntensityMS1,
        p$minIntensityMS2
      )
      message(" Done!")
      if (nrow(raw) == 0) return(NULL)
      dt <- data.table::as.data.table(raw)
      dt$analysis  <- aname
      dt$replicate <- arep
      if (!"level" %in% colnames(dt)) dt$level <- 1L
      data.table::setcolorder(dt, c("analysis", "replicate"))
      dt
    }, error = function(e) {
      message(" FAILED: ", e$message)
      NULL
    })
  })

  spec_dt <- data.table::rbindlist(spec_list, fill = TRUE)
  if (nrow(spec_dt) == 0) { warning("No spectra loaded."); return(FALSE) }

  analyses_for_db <- analyses[, c("analysis", "replicate", "blank", "type",
                                   "polarity", "concentration"), drop = FALSE]

  engine$Spectra <- MassSpecResults_Spectra(
    projectPath = engine$get_project_path(),
    analyses    = analyses_for_db,
    spectra     = spec_dt
  )
  invisible(TRUE)
}


# MARK: MassSpecMethod_LoadSpectra_chrompeaks
#' @title MassSpecMethod_LoadSpectra_chrompeaks class
#' @description Load MS2 spectra for chromatographic peak windows identified by FindChromPeaks / IntegrateChromatograms.
#' @param levels   MS levels to load (default 2L for MS2).
#' @param minIntensityMS1 Minimum MS1 intensity filter.
#' @param minIntensityMS2 Minimum MS2 intensity filter.
#' @export
MassSpecMethod_LoadSpectra_chrompeaks <- function(
    levels          = 2L,
    minIntensityMS1 = 0.0,
    minIntensityMS2 = 0.0) {
  x <- ProcessingStep(
    type        = "MassSpec",
    method      = "LoadSpectra",
    required    = c("LoadChromatograms", "FindChromPeaks"),
    algorithm   = "chrompeaks",
    input_class = c("MassSpecAnalyses", "MassSpecResults_Chromatograms"),
    output_class = "MassSpecResults_Spectra",
    number_permitted = 1,
    version     = as.character(packageVersion("StreamFind")),
    software    = "StreamFind",
    developer   = "Ricardo Cunha",
    contact     = "cunha@iuta.de",
    link        = "https://odea-project.github.io/StreamFind",
    doi         = NA_character_,
    parameters  = list(
      levels          = as.integer(levels),
      minIntensityMS1 = as.numeric(minIntensityMS1),
      minIntensityMS2 = as.numeric(minIntensityMS2)
    )
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_LoadSpectra_chrompeaks parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_LoadSpectra_chrompeaks <- function(x) {
  checkmate::assert_choice(x$method, "LoadSpectra")
  checkmate::assert_choice(x$algorithm, "chrompeaks")
  checkmate::assert_integerish(x$parameters$levels, min.len = 1)
  checkmate::assert_numeric(x$parameters$minIntensityMS1, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$minIntensityMS2, len = 1, lower = 0)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_LoadSpectra_chrompeaks <- function(x, engine = NULL) {
  if (!"MassSpecAnalyses" %in% class(engine$Analyses)) {
    warning("Engine does not contain MassSpecAnalyses.")
    return(FALSE)
  }
  chrom_results <- engine$Chromatograms
  if (is.null(chrom_results)) { warning("No Chromatograms results found."); return(FALSE) }

  analyses <- query_db(engine$Analyses, "SELECT * FROM Analyses")
  if (nrow(analyses) == 0) { warning("No analyses found."); return(FALSE) }

  peaks <- query_db(chrom_results, "SELECT * FROM Peaks")
  if (nrow(peaks) == 0) { warning("No peaks found in Chromatograms."); return(FALSE) }

  headers  <- query_db(engine$Analyses, "SELECT * FROM SpectraHeaders")
  headers_split <- split(headers, headers$analysis)

  p <- x$parameters

  # Build per-analysis target windows from chromatographic peaks
  spec_list <- lapply(seq_len(nrow(analyses)), function(i) {
    aname <- analyses$analysis[i]
    arep  <- analyses$replicate[i]
    afile <- analyses$file[i]
    hd_a  <- headers_split[[aname]]
    if (is.null(hd_a) || nrow(hd_a) == 0) return(NULL)
    pks_a <- peaks[peaks$analysis == aname, ]
    if (nrow(pks_a) == 0) return(NULL)
    # Build targets from peaks (rt windows)
    targets <- data.table::data.table(
      analysis = aname,
      mz       = pks_a$pre_mz,
      mzmin    = 0,
      mzmax    = max(hd_a$highmz, na.rm = TRUE),
      rt       = (pks_a$rtmin + pks_a$rtmax) / 2,
      rtmin    = pks_a$rtmin,
      rtmax    = pks_a$rtmax,
      id       = pks_a$id,
      polarity = pks_a$polarity
    )
    targets$mz[is.na(targets$mz)] <- 0
    tryCatch({
      message("\U2699 Loading spectra from ", basename(afile), "...", appendLF = FALSE)
      raw <- rcpp_streamcraft_parse_ms_spectra(
        list(file = afile, spectra_headers = hd_a),
        p$levels,
        as.data.frame(targets),
        p$minIntensityMS1,
        p$minIntensityMS2
      )
      message(" Done!")
      if (nrow(raw) == 0) return(NULL)
      dt <- data.table::as.data.table(raw)
      dt$analysis  <- aname
      dt$replicate <- arep
      if (!"level" %in% colnames(dt)) dt$level <- as.integer(p$levels[1])
      data.table::setcolorder(dt, c("analysis", "replicate"))
      dt
    }, error = function(e) {
      message(" FAILED: ", e$message)
      NULL
    })
  })

  spec_dt <- data.table::rbindlist(spec_list, fill = TRUE)
  if (nrow(spec_dt) == 0) { warning("No spectra loaded."); return(FALSE) }

  analyses_for_db <- analyses[, c("analysis", "replicate", "blank", "type",
                                   "polarity", "concentration"), drop = FALSE]
  engine$Spectra <- MassSpecResults_Spectra(
    projectPath = engine$get_project_path(),
    analyses    = analyses_for_db,
    spectra     = spec_dt
  )
  invisible(TRUE)
}

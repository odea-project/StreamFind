# MARK: MassSpecMethod_LoadChromatograms_native
#' @title MassSpecMethod_LoadChromatograms_native class
#' @description Load chromatograms from Mass Spec analysis files into a DuckDB backend.
#' @param chromatograms Integer vector of chromatogram indices or character vector of chromatogram IDs to load (NULL = all).
#' @param rtmin Numeric. Minimum retention time in seconds to keep (0 = no lower limit).
#' @param rtmax Numeric. Maximum retention time in seconds to keep (0 = no upper limit).
#' @param minIntensity Numeric. Minimum intensity threshold; data points below this value are removed (0 = no filter).
#' @export
MassSpecMethod_LoadChromatograms_native <- function(
  chromatograms = NULL,
  rtmin = 0,
  rtmax = 0,
  minIntensity = 0
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "LoadChromatograms",
    required = NA_character_,
    algorithm = "native",
    input_class = "MassSpecAnalyses",
    output_class = "MassSpecResults_Chromatograms",
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_,
    parameters = list(
      chromatograms = chromatograms,
      rtmin         = as.numeric(rtmin),
      rtmax         = as.numeric(rtmax),
      minIntensity  = as.numeric(minIntensity)
    )
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_LoadChromatograms_native parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_LoadChromatograms_native <- function(x) {
  checkmate::assert_choice(x$method, "LoadChromatograms")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_number(x$parameters$rtmin, lower = 0)
  checkmate::assert_number(x$parameters$rtmax, lower = 0)
  checkmate::assert_number(x$parameters$minIntensity, lower = 0)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_LoadChromatograms_native <- function(x, engine = NULL) {
  if (!"MassSpecAnalyses" %in% class(engine$Analyses)) {
    warning("Engine Analyses is not MassSpecAnalyses.")
    return(FALSE)
  }
  analyses <- query_db(engine$Analyses, "SELECT * FROM Analyses")
  if (nrow(analyses) == 0) {
    warning("No analyses found.")
    return(FALSE)
  }

  # All headers — must be passed unfiltered to the C++ function because it uses
  # idx values as positions into the header vectors.
  all_chrom_headers <- query_db(engine$Analyses, "SELECT * FROM ChromatogramsHeaders")
  all_chrom_headers_split <- split(all_chrom_headers, all_chrom_headers$analysis)

  chromatograms_param <- x$parameters$chromatograms
  rtmin        <- x$parameters$rtmin
  rtmax        <- x$parameters$rtmax
  minIntensity <- x$parameters$minIntensity

  chrom_list <- lapply(seq_len(nrow(analyses)), function(i) {
    aname <- analyses$analysis[i]
    arep  <- analyses$replicate[i]
    afile <- analyses$file[i]
    hd_a  <- all_chrom_headers_split[[aname]]
    if (is.null(hd_a) || nrow(hd_a) == 0) {
      message("\U2717 No chromatogram headers for ", aname, " \u2014 skipping.")
      return(NULL)
    }

    # Determine which indices to extract (passed to C++ as positional selectors)
    if (is.null(chromatograms_param) || length(chromatograms_param) == 0) {
      idx_use <- as.integer(hd_a$index)
    } else if (is.numeric(chromatograms_param)) {
      idx_use <- as.integer(chromatograms_param)
    } else if (is.character(chromatograms_param)) {
      idx_use <- as.integer(hd_a$index[hd_a$id %in% chromatograms_param])
    } else {
      idx_use <- as.integer(hd_a$index)
    }
    if (length(idx_use) == 0) {
      message("\U2717 No matching chromatograms for ", aname, " \u2014 skipping.")
      return(NULL)
    }

    tryCatch({
      message("\U2699 Loading chromatograms from ", basename(afile), "...", appendLF = FALSE)
      raw <- rcpp_streamcraft_parse_ms_chromatograms(
        list(file = afile, chromatograms_headers = hd_a),
        idx_use
      )
      message(" Done!")
      if (nrow(raw) == 0) return(NULL)
      dt <- data.table::as.data.table(raw)
      dt$analysis  <- aname
      dt$replicate <- arep
      data.table::setcolorder(dt, c("analysis", "replicate"))
      dt
    }, error = function(e) {
      message(" FAILED: ", e$message)
      NULL
    })
  })

  chrom_dt <- data.table::rbindlist(chrom_list, fill = TRUE)
  if (nrow(chrom_dt) == 0) { warning("No chromatograms loaded."); return(FALSE) }

  # Apply RT and intensity filters in R after parsing
  if (is.numeric(minIntensity) && minIntensity > 0) {
    chrom_dt <- chrom_dt[chrom_dt$intensity >= minIntensity, ]
  }
  if (is.numeric(rtmin) && is.numeric(rtmax) && rtmax > 0) {
    chrom_dt <- chrom_dt[chrom_dt$rt >= rtmin & chrom_dt$rt <= rtmax, ]
  }
  if (nrow(chrom_dt) == 0) { warning("No chromatogram data remaining after filtering."); return(FALSE) }

  # Ensure required columns exist
  for (col in c("baseline", "raw")) {
    if (!col %in% colnames(chrom_dt)) chrom_dt[[col]] <- NA_real_
  }

  analyses_for_db <- analyses[, c("analysis", "replicate", "blank", "file", "format", "type",
                                   "polarity", "spectra_number", "chromatograms_number",
                                   "concentration"), drop = FALSE]

  engine$Chromatograms <- MassSpecResults_Chromatograms(
    projectPath   = engine$get_project_path(),
    analyses      = analyses_for_db,
    chromatograms = chrom_dt
  )
  invisible(TRUE)
}

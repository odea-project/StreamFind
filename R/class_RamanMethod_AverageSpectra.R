#' Raman Method to Average Spectra (native algorithm)
#'
#' @description Averages spectra based on variables.
#'
#' @param by Character (length 1) with the grouping variable for averaging. Possible variables are
#' `replicates`, `chrom_peaks`, `rt`, `replicates+chrom_peaks`, `replicates+rt`, `chrom_peaks+rt`,
#' `replicates+chrom_peaks+rt`.
#'
#' @return A RamanMethod_AverageSpectra_native object.
#'
#' @export
#'
RamanMethod_AverageSpectra_native <- function(by = "replicates") {
  x <- ProcessingStep(
    type = "Raman",
    method = "AverageSpectra",
    required = NA_character_,
    algorithm = "native",
    parameters = list(
      by = by
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
    stop("Invalid RamanMethod_AverageSpectra_native object!")
  }
}

#' @describeIn RamanMethod_AverageSpectra_native Validate the RamanMethod_AverageSpectra_native object, returning NULL if valid.
#' @param x A RamanMethod_AverageSpectra_native object.
#' @export
#'
validate_object.RamanMethod_AverageSpectra_native <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "AverageSpectra")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_choice(
    x$parameters$by,
    c(
      "replicates",
      "chrom_peaks",
      "rt",
      "replicates+chrom_peaks",
      "replicates+rt",
      "chrom_peaks+rt",
      "replicates+chrom_peaks+rt"
    )
  )
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.RamanMethod_AverageSpectra_native <- function(x, engine = NULL) {
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
  if (spec_obj$is_averaged) {
    warning("Spectra are already averaged! Not done.")
    return(FALSE)
  }
  spectra_list <- spec_obj$spectra
  spectra <- data.table::rbindlist(
    spectra_list,
    idcol = "analysis",
    fill = TRUE
  )
  rpl <- get_replicate_names(engine$Analyses)
  spectra$replicate <- rpl[spectra$analysis]
  groupCols <- "shift"
  if (grepl("chrom_peaks", x$parameters$by, fixed = FALSE)) {
    groupCols <- c("chrom_peaks", groupCols)
  }
  if (grepl("rt", x$parameters$by, fixed = FALSE)) {
    groupCols <- c("rt", groupCols)
  }
  if (grepl("replicates", x$parameters$by, fixed = FALSE)) {
    groupCols <- c("replicate", groupCols)
  }
  if ("chrom_peaks" %in% groupCols) {
    if (length(spec_obj$chrom_peaks) > 0) {
      if (!"id" %in% colnames(spectra)) {
        warning(
          "Filter spectra to keep only from chromatographic peaks 
          using RamanMethod_FilterSpectra_native! Not done."
        )
        return(FALSE)
      }
      if ("group" %in% colnames(spectra)) {
        groupCols <- c("group", groupCols)
      } else {
        groupCols <- c("id", groupCols)
      }
      groupCols <- groupCols[!groupCols %in% "chrom_peaks"]
    } else {
      warning("No chromatographic peaks found! Not done.")
      return(FALSE)
    }
  }
  if ("replicate" %in% groupCols) {
    spectra$analysis <- NULL
  } else {
    groupCols <- c("analysis", groupCols)
    spectra$replicate <- NULL
  }
  if ("id" %in% colnames(spectra) && !"id" %in% groupCols) {
    spectra$id <- NULL
  }
  if ("group" %in% colnames(spectra) && !"group" %in% groupCols) {
    spectra$group <- NULL
  }
  .SD <- NULL
  shift <- NULL
  grouped_spectra <- spectra[, lapply(.SD, mean), by = groupCols]
  if ("replicate" %in% groupCols) {
    data.table::setorder(grouped_spectra, shift, replicate)
    split_str <- grouped_spectra$replicate
    grouped_spectra$replicate <- NULL
    grouped_spectra_list <- split(grouped_spectra, split_str)
    names_spectra <- unique(get_replicate_names(engine$Analyses))
    grouped_spectra_list <- grouped_spectra_list[names_spectra]
  } else {
    data.table::setorder(grouped_spectra, shift, analysis)
    split_str <- grouped_spectra$analysis
    grouped_spectra$analysis <- NULL
    grouped_spectra_list <- split(grouped_spectra, split_str)
    names_spectra <- names(engine$Analyses)
    grouped_spectra_list <- grouped_spectra_list[names_spectra]
  }
  spec_obj$spectra <- grouped_spectra_list
  if ("replicate" %in% groupCols) {
    spec_obj$is_averaged <- TRUE
  }
  engine$Results <- spec_obj
  message(paste0("\U2713 ", "Averaged spectra!"))
  invisible(TRUE)
}

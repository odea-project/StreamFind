#' **RamanSettings_AverageSpectra_StreamFind**
#'
#' @description Averages spectra based on variables.
#' 
#' @param by Character (length 1) with the grouping variable for averaging. Possible variables are 
#' `replicates`, `chrom_peaks`, `rt`, `replicates+chrom_peaks`, `replicates+rt`, `chrom_peaks+rt`,
#' `replicates+chrom_peaks+rt`.
#'
#' @return A RamanSettings_AverageSpectra_StreamFind object.
#'
#' @export
#'
RamanSettings_AverageSpectra_StreamFind <- S7::new_class(
  "RamanSettings_AverageSpectra_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  constructor = function(by = "replicates") {
    S7::new_object(
      ProcessingSettings(
        engine = "Raman",
        method = "AverageSpectra",
        required = NA_character_,
        algorithm = "StreamFind",
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
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "Raman")
    checkmate::assert_choice(self@method, "AverageSpectra")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_choice(
      self@parameters$by,
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
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanSettings_AverageSpectra_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_spectra()) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  if (engine$spectra$is_averaged) {
    warning("Spectra are already averaged! Not done.")
    return(FALSE)
  }
  
  spectra_list <- engine$spectra$spectra
  
  spectra <- data.table::rbindlist(spectra_list, idcol = "analysis", fill = TRUE)
  
  if (engine$spectra$is_averaged) {
    spectra$replicate <- spectra$analysis
  } else {
    rpl <- engine$analyses$replicates
    spectra$replicate <- rpl[spectra$analysis]
  }
  
  groupCols <- "shift"
  
  if (grepl("replicates", x$parameters$by, fixed = FALSE)) groupCols <- c("replicate", groupCols)
  if (grepl("chrom_peaks", x$parameters$by, fixed = FALSE)) groupCols <- c("chrom_peaks", groupCols)
  if (grepl("rt", x$parameters$by, fixed = FALSE)) groupCols <- c("rt", groupCols)
  
  if ("chrom_peaks" %in% groupCols) {
    if (engine$spectra$has_chrom_peaks) {
      if (!"id" %in% colnames(spectra)) {
        warning(
          "Filter spectra to keep only from chromatographic peaks 
          using RamanSettings_FilterSpectra_StreamFind! Not done."
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
  }
  
  if ("id" %in% colnames(spectra) && !"id" %in% groupCols) {
    spectra$id <- NULL
  }
  
  if ("group" %in% colnames(spectra) && !"group" %in% groupCols) {
    spectra$group <- NULL
  }
  
  grouped_spectra <- spectra[, lapply(.SD, mean), by = groupCols]
  
  if ("replicate" %in% groupCols) {
    setorder(grouped_spectra, shift, replicate)
    split_str <- grouped_spectra$replicate
    grouped_spectra$replicate <- NULL
    grouped_spectra_list <- split(grouped_spectra, split_str)
    names_spectra <- unique(engine$analyses$replicates)
    grouped_spectra_list <- grouped_spectra_list[names_spectra]
  } else {
    setorder(grouped_spectra, shift, analysis)
    split_str <- grouped_spectra$analysis
    grouped_spectra$analysis <- NULL
    grouped_spectra_list <- split(grouped_spectra, split_str)
    names_spectra <- names(engine$analyses)
    grouped_spectra_list <- grouped_spectra_list[names_spectra]
  }
  
  spectra <- engine$spectra
  spectra$spectra <- grouped_spectra_list
  spectra$is_averaged <- TRUE
  engine$spectra <- spectra
  message(paste0("\U2713 ", "Averaged spectra!"))
  invisible(TRUE)
}

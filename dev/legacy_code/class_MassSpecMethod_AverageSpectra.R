#' @title MassSpecMethod_AverageSpectra_StreamFind Class
#'
#' @description Averages spectra based on variables.
#'
#' @param by Character (length 1) with the grouping variable for averaging. Possible variables are
#' `replicates`, `chrom_peaks`, `rt`, `replicates+chrom_peaks`, `replicates+rt`, `chrom_peaks+rt`,
#' `replicates+chrom_peaks+rt`.
#' @param weightedAveraged Logical (length 1) for weighted averaging.
#'
#' @return A MassSpecMethod_AverageSpectra_StreamFind object.
#'
#' @export
#'
MassSpecMethod_AverageSpectra_StreamFind <- function(
  by = "replicates",
  weightedAveraged = TRUE
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "AverageSpectra",
    required = "LoadSpectra",
    algorithm = "StreamFind",
    parameters = list(
      by = by,
      weightedAveraged = weightedAveraged
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
    stop("Invalid MassSpecMethod_AverageSpectra_StreamFind object!")
  }
}

#' @export
#' @noRd
#'
validate_object.MassSpecMethod_AverageSpectra_StreamFind <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "AverageSpectra")
  checkmate::assert_choice(x$algorithm, "StreamFind")
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
  checkmate::assert_logical(x$parameters$weightedAveraged, max.len = 1)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_AverageSpectra_StreamFind <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  if (is.null(engine$Results[["MassSpecResults_Spectra"]])) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  spec_obj <- engine$Results[["MassSpecResults_Spectra"]]
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
  rpl <- spec_obj$replicates
  spectra$replicate <- rpl[spectra$analysis]
  groupCols <- c("mz", "mass", "bins")
  if (grepl("replicates", x$parameters$by, fixed = FALSE)) {
    groupCols <- c("replicate", groupCols)
  }
  if (grepl("chrom_peaks", x$parameters$by, fixed = FALSE)) {
    groupCols <- c("chrom_peaks", groupCols)
  }
  if (grepl("rt", x$parameters$by, fixed = FALSE)) {
    groupCols <- c("rt", groupCols)
  }
  groupCols <- groupCols[groupCols %in% colnames(spectra)]
  if ("chrom_peaks" %in% groupCols) {
    if (!is.null(engine$Results[["MassSpecResults_Chromatograms"]])) {
      chrom_obj <- engine$Results[["MassSpecResults_Chromatograms"]]
      if (length(chrom_obj$peaks) > 0) {
        if (!"id" %in% colnames(spectra)) {
          warning(
            "Filter spectra to keep only from chromatographic peaks ",
            "using MassSpecMethod_FilterSpectra_StreamFind! Not done."
          )
          return(FALSE)
        }
        groupCols <- c("id", groupCols)
        groupCols <- groupCols[!groupCols %in% "chrom_peaks"]
      } else {
        warning("No chromatographic peaks found! Not done.")
        return(FALSE)
      }
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
  if (!"rt" %in% groupCols) {
    spectra$rt <- NULL
  }
  if (x$parameters$weightedAveraged) {
    intensity <- NULL
    grouped_spectra <- spectra[,
      lapply(.SD, weighted.mean, w = intensity),
      by = groupCols
    ]
  } else {
    other_cols <- setdiff(colnames(spectra), groupCols)
    # grouped_spectra <- spectra[,
    #   .(intensity = mean(intensity)),
    #   by = groupCols,
    #   .SDcols = other_cols
    # ]
    grouped_spectra <- spectra[
      ,
      lapply(.SD, mean),
      by = groupCols,
      .SDcols = other_cols
    ]
  }
  if ("replicate" %in% groupCols) {
    if ("mz" %in% colnames(grouped_spectra)) {
      setorder(grouped_spectra, mz, replicate)
    } else {
      setorder(grouped_spectra, mass, replicate)
    }
    split_str <- grouped_spectra$replicate
    grouped_spectra$replicate <- NULL
    grouped_spectra_list <- split(grouped_spectra, split_str)
    for (r in unique(get_replicate_names(engine$Analyses))) {
      if (!r %in% names(grouped_spectra_list)) {
        grouped_spectra_list[[r]] <- data.table::data.table()
      }
    }
    grouped_spectra_list <- grouped_spectra_list[unique(get_replicate_names(
      engine$Analyses
    ))]
  } else {
    if ("mz" %in% colnames(grouped_spectra)) {
      setorder(grouped_spectra, mz, analysis)
    } else {
      setorder(grouped_spectra, mass, analysis)
    }
    split_str <- grouped_spectra$analysis
    grouped_spectra$analysis <- NULL
    grouped_spectra_list <- split(grouped_spectra, split_str)
    for (a in names(engine$Analyses)) {
      if (!a %in% names(grouped_spectra_list)) {
        grouped_spectra_list[[a]] <- data.table::data.table()
      }
    }
    grouped_spectra_list <- grouped_spectra_list[names(engine$Analyses)]
  }
  spec_obj$spectra <- grouped_spectra_list
  spec_obj$is_averaged <- TRUE
  engine$Results <- spec_obj
  message(paste0("\U2713 ", "Averaged spectra!"))
  invisible(TRUE)
}

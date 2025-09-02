# MARK: MassSpecAnalyses
#' @title Analyses class and methods for handling Mass Spectrometry data
#' @description The `MassSpecAnalyses` class represents mass spectrometry (MS) raw data files and holds results from processing MS data. It is a subclass of the [StreamFind::Analyses] class and provides methods to manage and inspect MS data. The `MassSpecAnalyses` class is built from a character vector of file paths to MS raw data files. The possible file formats are *mzML* and *mzXML*. If `msconvert` from \href{https://proteowizard.sourceforge.io/}{ProteoWizard} is installed and found via CLI (i.e., must be added to the environmental variables), the engine can also load vendor formats (i.e., Agilent MassHunter .d, Thermo Scientific RAW, Shimadzu LCD (except ITOF), Sciex WIFF/WIFF2) by direct conversion to *mzML*. Note that conversion of vendor formats is only possible under Windows OS.
#' @template arg-ms-files
#' @template arg-ms-centroid
#' @template arg-ms-levels
#'
#' @return An object of class `MassSpecAnalyses` as a list of two elements: `analyses` and `results`. Each element in `analyses` is a list with the following elements:
#'  - `name`: The name of the analysis.
#'  - `replicate`: The name of the replicate for the analysis.
#'  - `blank`: The name of the blank for the analysis.
#'  - `file`: The file path of the analysis.
#'  - `format`: The file format of the analysis.
#'  - `type`: The type of the analysis (e.g., "MS", MS/MS).
#'  - `spectra_number`: The number of spectra in the analysis.
#'  - `spectra_headers`: A `data.table` with the headers of the spectra in the analysis.
#'  - `chromatograms_number`: The number of chromatograms in the analysis.
#'  - `chromatograms_headers`: A `data.table` with the headers of the chromatograms in the analysis.
#'  - `metadata`: A list with metadata information for the analysis.
#'  - `concentration`: The concentration for the analysis.
#'  The `results` element is a list of results, where each element is a specific [StreamFind::Results] child class. Currently, the `MassSpecAnalyses` class supports the following results: [StreamFind::MassSpecResults_NonTargetAnalysis], [StreamFind::MassSpecSpectra], and [StreamFind::Chromatograms].
#'
#' @references
#' \insertRef{pugixml01}{StreamFind}
#'
#' \insertRef{proteo01}{StreamFind}
#'
#' \insertRef{proteo02}{StreamFind}
#'
#' @export
#'
MassSpecAnalyses <- function(files = NULL, centroid = FALSE, levels = c(1, 2)) {
  analyses <- .get_MassSpecAnalysis_from_files(files, centroid, levels)
  x <- structure(
    list(
      analyses = analyses,
      results = list(),
      type = "MassSpec",
      formats = DataTypes()$file_formats$MassSpec
    ),
    class = c("MassSpecAnalyses", "Analyses")
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecAnalyses object!")
  }
}

#' @describeIn MassSpecAnalyses Validate the MassSpecAnalyses object, returning `NULL` if valid or an error if not.
#' @template arg-x-MassSpecAnalyses
#' @export
#'
validate_object.MassSpecAnalyses = function(x) {
  checkmate::assert_class(x, "MassSpecAnalyses")
  checkmate::assert_true(identical(
    x$formats,
    DataTypes()$file_formats$MassSpec
  ))
  if (length(x$analyses) > 0) {
    lapply(x$analyses, function(a) {
      if (!is.list(a)) {
        stop("All analyses must be list objects!")
      }
      checkmate::assert_names(
        names(a),
        must.include = c(
          "name", "replicate", "blank", "file", "format",
          "type", "spectra_number", "spectra_headers",
          "chromatograms_number", "chromatograms_headers",
          "metadata", "concentration"
        )
      )
    })
  }
  NextMethod()
  NULL
}

# MARK: Methods

# MARK: get_analysis_names
#' @describeIn MassSpecAnalyses Get the names of the analyses in the `MassSpecAnalyses` object.
#' @template arg-x-MassSpecAnalyses
#' @export
#'
get_analysis_names.MassSpecAnalyses <- function(x) {
  vapply(x$analyses, function(x) x$name, NA_character_)
}

# MARK: get_replicate_names
#' @describeIn MassSpecAnalyses Get the replicates of the analyses in the `MassSpecAnalyses` object.
#' @template arg-x-MassSpecAnalyses
#' @export
#'
get_replicate_names.MassSpecAnalyses <- function(x) {
  vapply(x$analyses, function(x) x$replicate, NA_character_)
}

# MARK: set_replicate_names
#' @describeIn MassSpecAnalyses Set the replicates of the analyses in the `MassSpecAnalyses` object. The argument `value` must be a character vector with the same length as the number of analyses in the object.
#' @template arg-x-MassSpecAnalyses
#' @template arg-value
#' @export
#'
set_replicate_names.MassSpecAnalyses <- function(x, value) {
  if (length(value) != length(x$analyses)) {
    stop("Length of value must be equal to the number of analyses!")
  }
  for (i in seq_along(x$analyses)) {
    x$analyses[[i]]$replicate <- value[i]
  }
  x
}

# MARK: get_blank_names
#' @describeIn MassSpecAnalyses Get the blanks of the analyses in the `MassSpecAnalyses` object.
#' @template arg-x-MassSpecAnalyses
#' @export
#'
get_blank_names.MassSpecAnalyses <- function(x) {
  vapply(x$analyses, function(x) x$blank, NA_character_)
}

# MARK: set_blank_names
#' @describeIn MassSpecAnalyses Set the blanks of the analyses in the `MassSpecAnalyses` object. The argument `value` must be a character vector with the same length as the number of analyses in the object.
#' @template arg-x-MassSpecAnalyses
#' @template arg-value
#' @export
#'
set_blank_names.MassSpecAnalyses <- function(x, value) {
  if (length(value) != length(x$analyses)) {
    stop("Length of value must be equal to the number of analyses!")
  }
  for (i in seq_along(x$analyses)) {
    x$analyses[[i]]$blank <- value[i]
  }
  x
}

# MARK: get_concentrations
#' @describeIn MassSpecAnalyses Get the concentrations of the analyses in the `MassSpecAnalyses` object.
#' @template arg-x-MassSpecAnalyses
#' @export
#'
get_concentrations.MassSpecAnalyses <- function(x) {
  vapply(x$analyses, function(x) x$concentration, NA_real_)
}

# MARK: set_concentrations
#' @describeIn MassSpecAnalyses Set the concentrations of the analyses in the `MassSpecAnalyses` object. The argument `value` must be a numeric vector with the same length as the number of analyses in the object.
#' @template arg-x-MassSpecAnalyses
#' @template arg-value
#' @export
#'
set_concentrations.MassSpecAnalyses <- function(x, value) {
  if (length(value) != length(x$analyses)) {
    stop("Length of value must be equal to the number of analyses!")
  }
  if (!is.numeric(value)) {
    stop("Value must be a numeric vector!")
  }
  for (i in seq_along(x$analyses)) {
    x$analyses[[i]]$concentration <- value[i]
  }
  x
}

# MARK: info
#' @describeIn MassSpecAnalyses Get a summary of the `MassSpecAnalyses` object.
#' @template arg-x-MassSpecAnalyses
#' @export
#' 
info.MassSpecAnalyses <- function(x) {
  if (length(x$analyses) > 0) {
    df <- data.table::data.table(
      "analysis" = vapply(x$analyses, function(x) x$name, ""),
      "replicate" = vapply(x$analyses, function(x) x$replicate, ""),
      "blank" = vapply(x$analyses, function(x) x$blank, ""),
      "type" = vapply(x$analyses, function(x) x$type, ""),
      "polarity" = vapply(x$analyses, function(x) {
        paste(unique(x$spectra_headers$polarity), collapse = ", ")
      }, ""),
      "spectra" = vapply(x$analyses, function(x) round(x$spectra_number, digits = 0), 0),
      "chromatograms" = vapply(x$analyses, function(x) round(x$chromatograms_number, digits = 0), 0),
      "concentration" = vapply(x$analyses, function(x) x$concentration, 0)
    )
    row.names(df) <- seq_len(nrow(df))
    df
  } else {
    data.table::data.table()
  }
}

# MARK: add
#' @describeIn MassSpecAnalyses Add analyses to the `MassSpecAnalyses` object. The argument `value` can be a character vector with file paths to MS raw data files or a list of `MassSpecAnalysis` objects. If the files are valid, they will be converted to `MassSpecAnalysis` objects.
#' @template arg-x-MassSpecAnalyses
#' @template arg-value
#' @export
#'
add.MassSpecAnalyses <- function(x, value) {
  if (is.character(value)) {
    if (all(tools::file_ext(value) %in% x$formats)) {
      value <- .get_MassSpecAnalysis_from_files(value)
    } else {
      warning("File/s not valid!")
      return(x)
    }
  }
  if (!all(vapply(value, function(a) is(a, "MassSpecAnalysis"), FALSE))) {
    # TODO add validation for MassSpecAnalysis
    warning("Analysis/s not valid!")
    return(x)
  }
  if (any(vapply(value, function(a) a$name %in% get_analysis_names(x), FALSE))) {
    warning("Analysis names already exist!")
    return(x)
  }
  analyses <- c(x$analyses, value)
  names(analyses) <- vapply(analyses, function(a) a$name, NA_character_)
  analyses <- analyses[order(names(analyses))]
  if (length(analyses) > length(x$analyses)) {
    if (length(x$results) > 0) {
      warning("All results removed!")
      x$results <- list()
    }
  }
  x$analyses <- analyses
  x
}

# MARK: remove
#' @describeIn MassSpecAnalyses Remove analyses from the `MassSpecAnalyses` object. The argument `value` can be a character vector with the names of the analyses to remove or a numeric vector with the indices of the analyses to remove. The method will also remove the corresponding results from `MassSpecResults_NonTargetAnalysis`, `Spectra`, and `Chromatograms` if available.
#' @template arg-x-MassSpecAnalyses
#' @template arg-value
#' @export
#'
remove.MassSpecAnalyses <- function(x, value) {
  if (is.character(value)) {
    x$analyses <- x$analyses[!get_analysis_names(x) %in% value]
    x$analyses <- x$analyses[order(names(x$analyses))]
    if (!is.null(x$results[["MassSpecResults_NonTargetAnalysis"]])) {
      x$results$MassSpecResults_NonTargetAnalysis <- x$results$MassSpecResults_NonTargetAnalysis[
        !get_analysis_names(x) %in% value
      ]
    }
    if (!is.null(x$results[["MassSpecSpectra"]])) {
      x$results$MassSpecSpectra <- x$results$MassSpecSpectra[!get_analysis_names(x) %in% value]
    }
    if (!is.null(x$results[["Chromatograms"]])) {
      x$results$Chromatograms <- x$results$Chromatograms[
        !get_analysis_names(x) %in% value
      ]
    }
  } else if (is.numeric(value)) {
    x$analyses <- x$analyses[-value]
    x$analyses <- x$analyses[order(names(x$analyses))]
    if (!is.null(x$results[["MassSpecResults_NonTargetAnalysis"]])) {
      x$results$MassSpecResults_NonTargetAnalysis <- x$results$MassSpecResults_NonTargetAnalysis[
        -value
      ]
    }
    if (!is.null(x$results[["MassSpecSpectra"]])) {
      x$results$MassSpecSpectra <- x$results$MassSpecSpectra[-value]
    }
    if (!is.null(x$results[["Chromatograms"]])) {
      x$results$Chromatograms <- x$results$Chromatograms[-value]
    }
  }
  x
}

# MARK: `[`
#' @describeIn MassSpecAnalyses Subset the `MassSpecAnalyses` object by indices, including the result elements:
#' `MassSpecResults_NonTargetAnalysis`, `Spectra`, and `Chromatograms`. The argument `i` can be a numeric vector with the indices of the analyses to keep.
#' @template arg-x-MassSpecAnalyses
#' @template arg-i
#' @export
#' 
`[.MassSpecAnalyses` <- function(x, i) {
  x$analyses <- x$analyses[i]
  if (!is.null(x$results[["MassSpecResults_NonTargetAnalysis"]])) {
    x$results$MassSpecResults_NonTargetAnalysis <- x$results$MassSpecResults_NonTargetAnalysis[i]
  }
  if (!is.null(x$results[["MassSpecSpectra"]])) {
    x$results$MassSpecSpectra <- x$results$MassSpecSpectra[i]
  }
  if (!is.null(x$results[["Chromatograms"]])) {
    x$results$Chromatograms <- x$results$Chromatograms[i]
  }
  x
}

# MARK: `[<-`
#' @describeIn MassSpecAnalyses Add analyses to the `MassSpecAnalyses` object by indices. The argument `i` can be a numeric vector with the indices of the analyses to keep, and `value` can be a character vector with file paths to MS raw data files or a list of `MassSpecAnalysis` objects.
#' @template arg-x-MassSpecAnalyses
#' @template arg-i
#' @template arg-value
#' @export
#' 
`[<-.MassSpecAnalyses` <- function(x, i, value) {
  x <- add(x, value)
  x
}

# MARK: `[[`
#' @describeIn MassSpecAnalyses Subset the `MassSpecAnalyses` object by indices, including the result elements:
#' `MassSpecResults_NonTargetAnalysis`, `Spectra`, and `Chromatograms`. The argument `i` can be a numeric value with the index of the analysis to keep. The method returns a `MassSpecAnalyses` object with only the specified analysis.
#' @template arg-x-MassSpecAnalyses
#' @template arg-i
#' @export
#' 
`[[.MassSpecAnalyses` <- function(x, i) {
  x$analyses <- x$analyses[[i]]
  if (!is.null(x$results[["MassSpecResults_NonTargetAnalysis"]])) {
    x$results$MassSpecResults_NonTargetAnalysis <- x$results$MassSpecResults_NonTargetAnalysis[[i]]
  }
  if (!is.null(x$results[["MassSpecSpectra"]])) {
    x$results$MassSpecSpectra <- x$results$MassSpecSpectra[[i]]
  }
  if (!is.null(x$results[["Chromatograms"]])) {
    x$results$Chromatograms <- x$results$Chromatograms[[i]]
  }
  x
}

# MARK: `[[<-`
#' @describeIn MassSpecAnalyses Add analyses to the `MassSpecAnalyses` object by indices. The argument `i` can be a numeric value with the index of the analysis to modify, and `value` can be a character vector with file path to an MS raw data file.
#' @template arg-x-MassSpecAnalyses
#' @template arg-i
#' @template arg-value
#' @export
#' 
`[[<-.MassSpecAnalyses` <- function(x, i, value) {
  x <- add(x, value)
  x
}

# MARK: read
#' @describeIn MassSpecAnalyses Save a `MassSpecAnalyses` object to a file. The `file` can be in JSON or RDS format.
#' @template arg-x-MassSpecAnalyses
#' @template arg-file
#' @export
#' 
save.MassSpecAnalyses <- function(x, file) {
  format <- tools::file_ext(file)
  if (format %in% "json") {
    x <- .convert_to_json(x)
    write(x, file)
  } else if (format %in% "rds") {
    saveRDS(x, file)
  } else {
    warning("Invalid format!")
  }
  invisible(NULL)
}

#' @describeIn MassSpecAnalyses Convert a valid list to a `MassSpecAnalyses` object. This method is used internally and should not be called directly.
#' @template arg-value
#' @export
#' 
as.MassSpecAnalyses <- function(value) {
  if (is(value, "MassSpecAnalyses")) {
    if (is.null(validate_object(value))) {
      return(value)
    } else {
      stop("Invalid MassSpecAnalyses object!")
    }
  }
  if (is.list(value)) {
    if (!all(names(value) %in% c("analyses", "results", "type", "formats"))) {
      stop("Value must be a list with 'analyses' and 'results' elements!")
    }
    value$analyses <- lapply(
      value$analyses,
      function(x) {
        if (is.list(x) && all(c(
          "name", "replicate", "blank", "file", "format",
          "type", "spectra_number", "spectra_headers",
          "chromatograms_number", "chromatograms_headers",
          "metadata", "concentration"
        ) %in% names(x))) {
          return(list(
            name = x$name,
            replicate = x$replicate,
            blank = x$blank,
            file = x$file,
            format = x$format,
            type = x$type,
            spectra_number = x$spectra_number,
            spectra_headers = data.table::as.data.table(x$spectra_headers),
            chromatograms_number = x$chromatograms_number,
            chromatograms_headers = data.table::as.data.table(x$chromatograms_headers),
            metadata = x$metadata,
            concentration = x$concentration
          ))
        } else {
          stop("Invalid MassSpecAnalysis object in list!")
        }
      }
    )
    if (length(value$results) > 0) {
      possible_results <- DataTypes()$results$MassSpec
      if (!all(names(value$results) %in% possible_results)) {
        stop(paste("Invalid results in MassSpecAnalyses:", paste(names(value$results), collapse = ", ")))
      }
      value$results <- lapply(
        value$results,
        function(res) {
          if (!is(res, "Results")) {
            call_name <- res$name
            call_name <- paste0("as.", call_name)
            res <- do.call(call_name, list(res))
          }
          if (!is.null((validate_object(res)))) {
            stop(paste("Invalid Results object:", res$name))
          }
          res
        }
      )
    }
    value <- structure(
      value,
      class = c("MassSpecAnalyses", "Analyses")
    )
    if (is.null(validate_object(value))) {
      return(value)
    } else {
      stop("Invalid MassSpecAnalyses object!")
    }
  }
  stop("Value must be a list or MassSpecAnalyses object!")
}

# MARK: read
#' @describeIn MassSpecAnalyses Read a `MassSpecAnalyses` object from a file. The `file` can be in JSON or RDS format.
#' @template arg-x-MassSpecAnalyses
#' @template arg-file
#' @export
#' 
read.MassSpecAnalyses <- function(x, file) {
  if (grepl(".json", file)) {
    if (file.exists(file)) {
      data <- jsonlite::fromJSON(file)
      data <- as.MassSpecAnalyses(data)
      if (is.null(validate_object(data))) {
        return(data)
      } else {
        stop("Invalid MassSpecAnalyses object!")
      }
    }
  } else if (grepl(".rds", file)) {
    res <- readRDS(file)
    if (is(res, "MassSpecAnalyses")) {
      return(res)
    }
  }
  NULL
}

# MARK: get_spectra_tic
#' @describeIn MassSpecAnalyses Get the total ion current (TIC) spectra for the specified analyses.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-rt
#' @template arg-as_list
#' @export
#' 
get_spectra_tic.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  levels = c(1, 2),
  rt = NULL,
  as_list = FALSE
) {
  analyses <- .check_analyses_argument(x$analyses, analyses)
  value <- lapply(x$analyses[analyses], function(z) {
    res <- z$spectra_headers[,
      c("polarity", "level", "rt", "tic"),
      with = FALSE
    ]
    res$replicate <- z$replicate
    colnames(res) <- c("polarity", "level", "rt", "intensity", "replicate")
    data.table::setcolorder(res, "replicate")
    res <- res[res$level %in% levels, ]
    if (!is.null(rt)) {
      if (length(rt) == 2 && is.numeric(rt)) {
        rt <- sort(rt)
        sel <- res$rt >= rt[1] & res$rt <= rt[2]
        res <- res[sel, ]
      }
    }
    res
  })
  if (as_list) return(value)
  data.table::rbindlist(value, idcol = "analysis", fill = TRUE)
}

# MARK: get_spectra_bpc
#' @describeIn MassSpecAnalyses Get the base peak chromatograms (BPC) spectra for the specified analyses.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-rt
#' @template arg-as_list
#' @export
#' 
get_spectra_bpc.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  levels = c(1, 2),
  rt = NULL,
  as_list = FALSE
) {
  analyses <- .check_analyses_argument(x$analyses, analyses)
 value <- lapply(x$analyses[analyses], function(z) {
    res <- z$spectra_headers[,
      c("polarity", "level", "rt", "bpmz", "bpint"),
      with = FALSE
    ]
    res$replicate <- z$replicate
    colnames(res) <- c("polarity", "level", "rt", "mz", "intensity", "replicate")
    data.table::setcolorder(res, "replicate")
    res <- res[res$level %in% levels, ]
    if (!is.null(rt)) {
      if (length(rt) == 2 && is.numeric(rt)) {
        rt <- sort(rt)
        sel <- res$rt >= rt[1] & res$rt <= rt[2]
        res <- res[sel, ]
      }
    }
    res
  })
  if (as_list) return(value)
  data.table::rbindlist(value, idcol = "analysis", fill = TRUE)
}

# MARK: get_raw_spectra
#' @describeIn MassSpecAnalyses Get raw spectra data from specified analyses, returning a `data.table` with the spectra data.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-allTraces
#' @template arg-ms-isolationWindow
#' @template arg-ms-minIntensityMS1
#' @template arg-ms-minIntensityMS2
#' @export
#' 
get_raw_spectra.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  levels = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 20,
  sec = 60,
  millisec = 5,
  id = NULL,
  allTraces = TRUE,
  isolationWindow = 1.3,
  minIntensityMS1 = 0,
  minIntensityMS2 = 0
) {
  analyses <- .check_analyses_argument(x$analyses, analyses)
  if (is.null(analyses)) {
    return(data.table::data.table())
  }

  if (!any(is.numeric(minIntensityMS1) | is.integer(minIntensityMS1))) {
    minIntensityMS1 <- 0
  }

  if (!any(is.numeric(minIntensityMS2) | is.integer(minIntensityMS2))) {
    minIntensityMS2 <- 0
  }

  if (is.data.frame(mz)) {
    if ("analysis" %in% colnames(mz)) {
      analyses <- mz$analysis
    }
  }

  if (is.data.frame(mass)) {
    if ("analysis" %in% colnames(mass)) {
      analyses <- mass$analysis
    }
  }

  polarities <- vapply(x$analyses[analyses], function(a) {
    paste0(unique(a$spectra_headers$polarity), collapse = ", ")
  }, NA_character_)

  targets <- MassSpecTargets(
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    analyses,
    polarities
  )

  num_cols <- c(
    "mz",
    "rt",
    "mobility",
    "mzmin",
    "mzmax",
    "rtmin",
    "rtmax",
    "mobilitymin",
    "mobilitymax"
  )

  if (
    all(
      apply(targets[, num_cols, with = FALSE], 1, function(z) {
        sum(z, na.rm = TRUE)
      }) !=
        0
    )
  ) {

    spectra_highest_mz <- max(vapply(x$analyses[analyses], function(a) {
      max(a$spectra_headers$highmz, na.rm = TRUE)
    }, 0))

    spectra_lowest_mz <- min(vapply(x$analyses[analyses], function(a) {
      min(a$spectra_headers$lowmz, na.rm = TRUE)
    }, 0))

    spectra_highest_rt <- max(vapply(x$analyses[analyses], function(a) {
      max(a$spectra_headers$rt, na.rm = TRUE)
    }, 0))

    spectra_lowest_rt <- min(vapply(x$analyses[analyses], function(a) {
      min(a$spectra_headers$rt, na.rm = TRUE)
    }, 0))

    has_ion_mobility <- any(vapply(x$analyses[analyses], function(a) {
      any(a$spectra_headers$mobility > 0)
    }, logical(1)))

    if (TRUE %in% is.na(targets$mz)) {
      targets$mz[is.na(targets$mz)] <- 0
    }
    if (TRUE %in% is.na(targets$mzmax)) {
      targets$mzmax[is.na(targets$mzmax)] <- spectra_highest_mz
    }
    if (TRUE %in% is.na(targets$mzmin)) {
      targets$mzmin[is.na(targets$mzmin)] <- spectra_lowest_mz
    }
    if (TRUE %in% (targets$mzmax == 0)) {
      targets$mzmax[targets$mzmax == 0] <- spectra_highest_mz
    }
    if (TRUE %in% is.na(targets$rt)) {
      targets$rt[is.na(targets$rt)] <- 0
    }
    if (TRUE %in% is.na(targets$rtmax)) {
      targets$rtmax[is.na(targets$rtmax)] <- spectra_highest_rt
    }
    if (TRUE %in% is.na(targets$rtmin)) {
      targets$rtmin[is.na(targets$rtmin)] <- spectra_lowest_rt
    }
    if (TRUE %in% (targets$rtmax == 0)) {
      targets$rtmax[targets$rtmax == 0] <- spectra_highest_rt
    }
    if (TRUE %in% is.na(targets$mobility)) {
      targets$mobility[is.na(targets$mobility)] <- 0
    }
    if (
      TRUE %in% is.na(targets$mobilitymax) && has_ion_mobility
    ) {

      spectra_highest_mobility <- max(vapply(
        x$analyses[analyses],
        function(a) {
          max(a$spectra_headers$mobility, na.rm = TRUE)
        },
        0
      ))

      targets$mobilitymax[is.na(targets$mobilitymax)] <- spectra_highest_mobility
    }
    if (
      TRUE %in% is.na(targets$mobilitymin) && has_ion_mobility
    ) {

      spectra_lowest_mobility <- min(vapply(
        x$analyses[analyses],
        function(a) {
          min(a$spectra_headers$mobility, na.rm = TRUE)
        },
        0
      ))

      targets$mobilitymin[is.na(targets$mobilitymin)] <- spectra_lowest_mobility
    }
    if (
      TRUE %in% (targets$mobilitymax == 0) && has_ion_mobility
    ) {

      spectra_highest_mobility <- max(vapply(
        x$analyses[analyses],
        function(a) {
          max(a$spectra_headers$mobility, na.rm = TRUE)
        },
        0
      ))

      targets$mobilitymax[targets$mobilitymax == 0] <- spectra_highest_mobility
    }
  } else {
    targets$id <- targets$analysis
  }

  if (is.null(levels)) {
    levels <- vapply(x$analyses[analyses], function(a) {
      paste0(unique(a$spectra_headers$level), collapse = ", ")
    }, NA_character_)
    levels <- as.numeric(unlist(strsplit(levels, ", ")))
  }

  if (!2 %in% levels) {
    allTraces <- TRUE
  }

  if (!is.logical(allTraces)) {
    allTraces <- TRUE
  }

  if (nrow(targets) > 0) {
    if ("polarity" %in% colnames(targets)) {
      targets$polarity <- as.numeric(targets$polarity)
    }
    targets$precursor <- FALSE
    if (!allTraces) {
      if (!any(is.numeric(isolationWindow) | is.integer(isolationWindow))) {
        isolationWindow <- 0
      }
      targets$precursor <- TRUE
      targets$mzmin <- targets$mzmin - (isolationWindow / 2)
      targets$mzmax <- targets$mzmax + (isolationWindow / 2)
      # TODO make case for DIA when pre_mz is not available
    }
  }

  spec_list <- lapply(
    x$analyses[analyses],
    function(a, levels, targets) {
      if ("analysis" %in% colnames(targets)) {
        targets <- targets[targets$analysis %in% a$name, ]
      }
      cache <- lapply(seq_len(nrow(targets)), function(i) {
        # .load_cache_sqlite(
        #   paste0(
        #     "parsed_ms_spectra_",
        #     gsub("-|[/]|[.]|[() ]", "", targets$id[i])
        #   ),
        #   a$file,
        #   levels,
        #   targets[i, ],
        #   minIntensityMS1,
        #   minIntensityMS2
        # )
      })

      names(cache) <- targets$id
      cached_targets_sel <- vapply(cache, function(z) !is.null(z$data), FALSE)
      cached_targets <- cache[cached_targets_sel]
      no_cached_targets <- targets[!cached_targets_sel, ]
      if (nrow(no_cached_targets) > 0) {
        message(
          "\U2699 Parsing spectra from ",
          basename(a$file),
          "...",
          appendLF = FALSE
        )

        if (nrow(no_cached_targets) == 1) {
          num_cols <- c(
            "mz",
            "rt",
            "mobility",
            "mzmin",
            "mzmax",
            "rtmin",
            "rtmax",
            "mobilitymin",
            "mobilitymax"
          )
          if (
            apply(no_cached_targets[, num_cols, with = FALSE], 1, function(z) {
              sum(z, na.rm = TRUE)
            }) ==
              0
          ) {
            no_cached_targets <- no_cached_targets[0, ]
          }
        }

        spec <- rcpp_parse_ms_spectra(
          a,
          levels,
          no_cached_targets,
          minIntensityMS1,
          minIntensityMS2
        )

        message(" Done!")

        if (nrow(spec) > 0) {
          if (!any(spec$mobility > 0)) {
            spec$mobility <- NULL
          }
          if (!"analysis" %in% colnames(spec)) {
            spec$analysis <- a$name
          }
          if (!"replicate" %in% colnames(spec)) {
            spec$replicate <- a$replicate
          }

          if ("id" %in% colnames(spec)) {
            spec <- split(spec, spec$id)
          } else {
            spec <- list(spec)
          }

          for (i in names(spec)) {
            if (nrow(spec[[i]]) > 0) {
              if (!is.null(cache[[i]]$hash)) {
                .save_cache_sqlite(
                  paste0("parsed_ms_spectra_", gsub("-|[/]|[.]|[() ]", "", i)),
                  spec[[i]],
                  cache[[i]]$hash
                )
                message("\U1f5ab Parsed spectra for ", i, " cached!")
              }
            }
          }

          spec <- data.table::rbindlist(spec, fill = TRUE)
        } else {
          spec <- data.table::data.table()
        }
      } else {
        spec <- data.table::data.table()
      }
      if (length(cached_targets) > 0) {
        cached_targets_dt <- data.table::rbindlist(
          lapply(cached_targets, function(z) data.table::as.data.table(z$data)),
          fill = TRUE
        )
        spec <- data.table::rbindlist(
          c(list(cached_targets_dt), list(spec)),
          fill = TRUE
        )
        message("\U2139 Spectra loaded from cache!")
      }
      if (nrow(spec) == 0) {
        return(data.table::data.table())
      }
      if ("id" %in% colnames(spec)) {
        data.table::setorder(spec, id, rt, mz)
      } else {
        data.table::setorder(spec, rt, mz)
      }
      gc()
      spec
    },
    levels = levels,
    targets = targets
  )

  if (length(spec_list) == length(analyses)) {
    spec <- data.table::rbindlist(spec_list, fill = TRUE)
    if (nrow(spec) > 0) {
      data.table::setcolorder(spec, c("analysis", "replicate"))
    }
    spec
  } else {
    warning("Defined analyses not found!")
    data.table::data.table()
  }
}

# MARK: get_spectra_eic
#' @describeIn MassSpecAnalyses Get extracted ion chromatograms (EIC) for the specified analyses and targets.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @export
#' 
get_spectra_eic.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 20,
  sec = 60,
  millisec = 5,
  id = NULL
) {
  eic <- get_raw_spectra(
    x,
    analyses,
    levels = 1,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    allTraces = TRUE,
    isolationWindow = 1.3,
    minIntensityMS1 = 0,
    minIntensityMS2 = 0
  )

  if (nrow(eic) > 0) {
    intensity <- NULL
    eic <- data.table::as.data.table(eic)
    if (!"id" %in% colnames(eic)) {
      eic$id <- NA_character_
    }
    if (!"polarity" %in% colnames(eic)) {
      eic$polarity <- 0
    }
    cols_summary <- c("analysis", "replicate", "polarity", "id", "rt")
    intensity <- NULL
    mz <- NULL
    eic <- eic[,
      .(intensity = max(intensity), mz = mean(mz)),
      by = cols_summary
    ]
    sel_cols <- c(
      "analysis",
      "replicate",
      "id",
      "polarity",
      "rt",
      "mz",
      "intensity"
    )
    eic <- eic[, sel_cols, with = FALSE]
    eic <- unique(eic)
  }

  eic
}

# MARK: get_spectra_ms1
#' @describeIn MassSpecAnalyses Get MS1 spectra for the specified analyses and targets.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @export
#' 
get_spectra_ms1.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 20,
  sec = 60,
  millisec = 5,
  id = NULL,
  mzClust = 0.003,
  presence = 0.8,
  minIntensity = 1000
) {
  ms1 <- get_raw_spectra(
    x,
    analyses,
    levels = 1,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    allTraces = TRUE,
    minIntensityMS1 = minIntensity,
    minIntensityMS2 = 0
  )

  if (nrow(ms1) == 0) {
    return(ms1)
  }

  if (!"id" %in% colnames(ms1)) {

    has_ion_mobility <- any(vapply(x$analyses[analyses], function(a) {
      any(a$spectra_headers$mobility > 0)
    }, logical(1)))

    if (has_ion_mobility) {
      ms1$id <- paste(
        round(min(ms1$mz), 4),
        "-",
        round(max(ms1$mz), 4),
        "/",
        round(max(ms1$rt), 0),
        "-",
        round(min(ms1$rt), 0),
        "/",
        round(max(ms1$mobility), 0),
        "-",
        round(min(ms1$mobility), 0),
        sep = ""
      )
    } else {
      ms1$id <- paste(
        round(min(ms1$mz), 4),
        "-",
        round(max(ms1$mz), 4),
        "/",
        round(max(ms1$rt), 0),
        "-",
        round(min(ms1$rt), 0),
        sep = ""
      )
    }
  }

  if (!is.numeric(mzClust)) {
    mzClust <- 0.01
  }

  ms1$unique_id <- paste0(ms1$analysis, "_", ms1$id, "_", ms1$polarity)

  ms1_list <- rcpp_ms_cluster_spectra(ms1, mzClust, presence, FALSE)

  ms1_df <- data.table::rbindlist(ms1_list, fill = TRUE)

  ms1_df <- ms1_df[order(ms1_df$mz), ]

  ms1_df <- ms1_df[order(ms1_df$id), ]

  ms1_df <- ms1_df[order(ms1_df$analysis), ]

  ms1_df$replicate <- get_replicate_names(x)[ms1_df$analysis]

  data.table::setcolorder(ms1_df, c("analysis", "replicate"))

  ms1_df
}

# MARK: get_spectra_ms2
#' @describeIn MassSpecAnalyses Get MS2 spectra for the specified analyses and targets.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @export
#' 
get_spectra_ms2.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 20,
  sec = 60,
  millisec = 5,
  id = NULL,
  isolationWindow = 1.3,
  mzClust = 0.005,
  presence = 0.8,
  minIntensity = 0
) {
  ms2 <- get_raw_spectra(
    x,
    analyses,
    levels = 2,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    isolationWindow = isolationWindow,
    allTraces = FALSE,
    minIntensityMS1 = 0,
    minIntensityMS2 = minIntensity
  )

  if (nrow(ms2) == 0) {
    return(ms2)
  }

  if (!"id" %in% colnames(ms2)) {

    has_ion_mobility <- any(vapply(x$analyses[analyses], function(a) {
      any(a$spectra_headers$mobility > 0)
    }, logical(1)))

    if (has_ion_mobility) {
      ms2$id <- paste(
        round(min(ms2$mz), 4),
        "-",
        round(max(ms2$mz), 4),
        "/",
        round(max(ms2$rt), 0),
        "-",
        round(min(ms2$rt), 0),
        "/",
        round(max(ms2$mobility), 0),
        "-",
        round(min(ms2$mobility), 0),
        sep = ""
      )
    } else {
      ms2$id <- paste(
        round(min(ms2$mz), 4),
        "-",
        round(max(ms2$mz), 4),
        "/",
        round(max(ms2$rt), 0),
        "-",
        round(min(ms2$rt), 0),
        sep = ""
      )
    }
  }

  if (!is.numeric(mzClust)) {
    mzClust <- 0.01
  }

  ms2$unique_id <- paste0(ms2$analysis, "_", ms2$id, "_", ms2$polarity)

  ms2_list <- rcpp_ms_cluster_spectra(ms2, mzClust, presence, FALSE)

  ms2_df <- data.table::rbindlist(ms2_list, fill = TRUE)

  ms2_df <- ms2_df[order(ms2_df$mz), ]

  ms2_df <- ms2_df[order(ms2_df$id), ]

  ms2_df <- ms2_df[order(ms2_df$analysis), ]

  ms2_df$replicate <- get_replicate_names(x)[ms2_df$analysis]

  data.table::setcolorder(ms2_df, c("analysis", "replicate"))

  ms2_df
}

# MARK: plot_spectra_tic
#' @describeIn MassSpecAnalyses Plot total ion current (TIC) spectra for the specified analyses.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-rt
#' @template arg-labs
#' @template arg-title
#' @template arg-ms-colorBy
#' @template arg-legendNames
#' @template arg-ms-downsize
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#' 
plot_spectra_tic.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  levels = c(1, 2),
  rt = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "analyses",
  legendNames = NULL,
  downsize = 1,
  interactive = TRUE,
  renderEngine = "webgl"
) {
  intensity <- NULL
  level <- NULL

  tic <- get_spectra_tic(x, analyses, levels, rt)
  tic$rt <- floor(tic$rt / downsize) * downsize
  groupCols <- c("rt", "analysis", "replicate", "polarity", "level")
  tic <- tic[, .(intensity = mean(intensity)), by = groupCols]

  if (nrow(tic) == 0) {
    message("\U2717 TIC not found for the analyses!")
    return(NULL)
  }

  if (is.null(xLab)) {
    xLab <- "Retention time / seconds"
  }
  if (is.null(yLab)) {
    yLab <- "Intensity / counts"
  }

  tic <- .make_colorBy_varkey(tic, colorBy, legendNames)

  tic$loop <- paste0(tic$analysis, tic$replicate, tic$id, tic$var)

  cl <- .get_colors(unique(tic$var))

  if (!interactive) {
    ggplot2::ggplot(tic, ggplot2::aes(x = rt, y = intensity, group = loop)) +
      ggplot2::geom_line(ggplot2::aes(color = var)) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black")
    )
    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black")
    )

    loop <- NULL

    plot <- tic %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines+markers",
        line = list(width = 0.5),
        marker = list(size = 2),
        text = ~ paste(
          "<br>analysis: ",
          analysis,
          "<br>replicate: ",
          replicate,
          "<br>id: ",
          id,
          "<br>polarity: ",
          polarity,
          "<br>level: ",
          level,
          "<br>rt: ",
          rt,
          "<br>intensity: ",
          intensity
        ),
        hoverinfo = "text"
      ) %>%
      plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )

    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }

    plot
  }
}

# MARK: plot_spectra_bpc
#' @describeIn MassSpecAnalyses Plot base peak chromatograms (BPC) spectra for the specified analyses.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-rt
#' @template arg-labs
#' @template arg-title
#' @template arg-ms-colorBy
#' @template arg-legendNames
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#' 
plot_spectra_bpc.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  levels = c(1, 2),
  rt = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "analyses",
  legendNames = NULL,
  interactive = TRUE,
  renderEngine = "webgl"
) {
  bpc <- get_spectra_bpc(x, analyses, levels, rt)

  if (nrow(bpc) == 0) {
    message("\U2717 BPC not found for the analyses!")
    return(NULL)
  }

  if (is.null(xLab)) {
    xLab <- "Retention time / seconds"
  }
  if (is.null(yLab)) {
    yLab <- "Intensity / counts"
  }

  bpc <- .make_colorBy_varkey(bpc, colorBy, legendNames)

  bpc$loop <- paste0(bpc$analysis, bpc$replicate, bpc$id, bpc$var)

  cl <- .get_colors(unique(bpc$var))

  if (!interactive) {
    ggplot2::ggplot(bpc, ggplot2::aes(x = rt, y = intensity, group = loop)) +
      ggplot2::geom_line(ggplot2::aes(color = var)) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black")
    )
    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black")
    )

    loop <- NULL

    plot <- bpc %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines+markers",
        line = list(width = 0.5),
        marker = list(size = 2),
        text = ~ paste(
          "<br>analysis: ",
          analysis,
          "<br>replicate: ",
          replicate,
          "<br>id: ",
          id,
          "<br>polarity: ",
          polarity,
          "<br>level: ",
          level,
          "<br>mz: ",
          mz,
          "<br>rt: ",
          rt,
          "<br>intensity: ",
          intensity
        ),
        hoverinfo = "text"
      ) %>%
      plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )

    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }

    plot
  }
}

# MARK: plot_spectra_eic
#' @describeIn MassSpecAnalyses Plot extracted ion chromatograms (EIC) for the specified analyses and targets.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-colorBy
#' @template arg-legendNames
#' @template arg-labs
#' @template arg-title
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#' 
plot_spectra_eic.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 20,
  sec = 60,
  millisec = 5,
  id = NULL,
  legendNames = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "targets",
  interactive = TRUE,
  renderEngine = "webgl"
) {
  eic <- get_spectra_eic(
    x,
    analyses,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id
  )

  if (nrow(eic) == 0) {
    message("\U2717 Traces not found for the targets!")
    return(NULL)
  }

  if (is.null(xLab)) {
    xLab <- "Retention time / seconds"
  }
  if (is.null(yLab)) {
    yLab <- "Intensity / counts"
  }

  eic <- .make_colorBy_varkey(eic, colorBy, legendNames)

  eic$loop <- paste0(eic$analysis, eic$replicate, eic$id, eic$var)

  cl <- .get_colors(unique(eic$var))

  if (!interactive) {
    ggplot2::ggplot(eic, ggplot2::aes(x = rt, y = intensity, group = loop)) +
      ggplot2::geom_line(ggplot2::aes(color = var)) +
      ggplot2::geom_point(ggplot2::aes(color = var), size = 1) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black")
    )
    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black")
    )

    loop <- NULL

    plot <- eic %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines+markers",
        line = list(width = 0.5),
        marker = list(size = 2),
        text = ~ paste(
          "<br>analysis: ",
          analysis,
          "<br>replicate: ",
          replicate,
          "<br>id: ",
          id,
          "<br>polarity: ",
          polarity,
          "<br>mz: ",
          mz,
          "<br>rt: ",
          rt,
          "<br>intensity: ",
          intensity
        ),
        hoverinfo = "text"
      ) %>%
      plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )

    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }

    plot
  }
}

# MARK: plot_spectra_xic
#' @describeIn MassSpecAnalyses Plot spectra extract ion chromatograms (EIC) and \emph{m/z} vs retention time from the analyses.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-legendNames
#' @template arg-ms-plotTargetMark
#' @template arg-ms-targetsMark
#' @template arg-ms-ppmMark
#' @template arg-ms-secMark
#' @template arg-ms-numberRows
#' @template arg-renderEngine
#' @export
#' 
plot_spectra_xic.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 20,
  sec = 60,
  millisec = 5,
  id = NULL,
  legendNames = NULL,
  plotTargetMark = TRUE,
  targetsMark = NULL,
  ppmMark = 5,
  secMark = 10,
  numberRows = 1,
  renderEngine = "webgl"
) {
  xic <- get_raw_spectra(
    x,
    analyses,
    levels = 1,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    allTraces = TRUE,
    isolationWindow = 1.3,
    minIntensityMS1 = 0,
    minIntensityMS2 = 0
  )

  if (nrow(xic) == 0) {
    message("\U2717 Traces not found for the targets!")
    return(NULL)
  }

  if (!"id" %in% colnames(xic)) {
    xic$id <- NA_character_
  }

  ids <- unique(xic$id)
  if (is.character(legendNames) & length(legendNames) == length(ids)) {
    names(legendNames) <- ids
    xic$id <- legendNames[xic$id]
  }

  if (plotTargetMark) {
    plotTargetMark <- FALSE
    if (is.data.frame(targetsMark)) {
      if (
        nrow(targetsMark) == length(ids) &&
          "mz" %in% colnames(targetsMark) &&
          "rt" %in% colnames(targetsMark)
      ) {
        tgmMZ <- as.numeric(targetsMark$mz)
        names(tgmMZ) <- ids
        tgmRT <- as.numeric(targetsMark$rt)
        names(tgmRT) <- ids
        xic$mz_id <- tgmMZ[xic$id]
        xic$rt_id <- tgmRT[xic$id]

        plotTargetMark <- TRUE
      }
    }
  }

  ids <- unique(xic$id)

  xic <- split(xic, by = "id")

  colors <- colorRamp(c("#383E47", "#5E8CAA", "#16B9E5", "#16E5C9", "#16E54C"))

  line <- list(
    type = "line",
    line = list(color = "red", dash = "dash", width = 0.5),
    xref = "x",
    yref = "y"
  )

  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Retention time / seconds",
    titlefont = list(size = 10, color = "black"),
    tickfont = list(size = 10)
  )

  yaxis1 <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Intensity / counts",
    titlefont = list(size = 10, color = "black"),
    tickfont = list(size = 10)
  )

  yaxis2 <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = paste("<i>m/z</i> / Da"),
    titlefont = list(size = 10, color = "black"),
    tickfont = list(size = 10)
  )

  mainPlot <- list()

  for (t in ids) {
    plotList <- list()
    vline1 <- list()
    vline2 <- list()
    hline <- list()
    rect <- list()

    xic_s <- xic[[t]]

    rtmin <- min(xic_s$rt, na.rm = TRUE)
    rtmax <- max(xic_s$rt, na.rm = TRUE)
    mzmin <- min(xic_s$mz, na.rm = TRUE)
    mzmax <- max(xic_s$mz, na.rm = TRUE)
    maxInt <- max(xic_s$intensity, na.rm = TRUE) * 1.1

    xic_s <- split(xic_s, by = "analysis")

    if (any(unlist(lapply(xic_s, nrow)) > 20000)) {
      warning(paste0(
        "The MS area to be plotted seems rather large. ",
        "It is suggested to restrict the data first using",
        " narrow mz and rt deviations."
      ))
      return(NULL)
    }

    for (s in seq_len(length(xic_s))) {
      temp <- xic_s[[s]]

      if (plotTargetMark && is.numeric(temp$mz_id) && is.numeric(temp$rt_id)) {
        plotTargetMark_loop <- TRUE

        vline1 <- list(
          x0 = temp$rt_id[1],
          x1 = temp$rt_id[1],
          y0 = 0,
          y1 = max(temp$intensity, na.rm = TRUE)
        )

        vline2 <- list(
          x0 = temp$rt_id[1],
          x1 = temp$rt_id[1],
          y0 = mzmin,
          y1 = mzmax
        )

        hline <- list(
          x0 = min(temp$rt, na.rm = TRUE),
          x1 = max(temp$rt, na.rm = TRUE),
          y0 = temp$mz_id[1],
          y1 = temp$mz_id[1]
        )

        rect <- list(
          type = "rect",
          fillcolor = "red",
          line = list(color = "red"),
          opacity = 0.1,
          x0 = temp$rt_id[1] - secMark,
          x1 = temp$rt_id[1] + secMark,
          xref = "x",
          y0 = temp$mz_id[1] - ((ppmMark / 1E6) * temp$mz_id[1]),
          y1 = temp$mz_id[1] + ((ppmMark / 1E6) * temp$mz_id[1]),
          yref = "y"
        )
      } else {
        plotTargetMark_loop <- FALSE
      }

      p1 <- plot_ly(
        data = temp,
        x = temp$rt,
        y = temp$intensity,
        type = "scatter",
        mode = "markers",
        color = temp$intensity,
        colors = colors,
        marker = list(size = 8, line = list(color = "white", width = 0.5)),
        name = paste0(s, "p1")
      )

      if (plotTargetMark_loop) {
        p1 <- p1 %>% plotly::layout(shapes = c(vline1, line))
      }

      p1 <- p1 %>%
        add_annotations(
          text = paste(unique(temp$analysis), t, sep = " - "),
          x = 0.05,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          align = "center",
          showarrow = FALSE,
          font = list(size = 10)
        )

      p2 <- plot_ly(
        temp,
        x = temp$rt,
        y = temp$mz,
        type = "scatter",
        mode = "markers",
        color = temp$intensity,
        colors = colors,
        marker = list(size = 8, line = list(color = "white", width = 0.5)),
        name = paste0(s, "p2")
      )

      if (plotTargetMark_loop) {
        p2 <- p2 %>%
          plotly::layout(
            shapes = list(c(vline2, line), c(hline, line), rect)
          )
      }

      p1 <- p1 %>% plotly::layout(xaxis = xaxis, yaxis = yaxis1)
      p2 <- p2 %>% plotly::layout(xaxis = xaxis, yaxis = yaxis2)

      plotList[[paste0("p1", s)]] <- p1
      plotList[[paste0("p2", s)]] <- p2
    }

    plotList <- plotList[order(names(plotList))]

    plot <- subplot(
      plotList,
      nrows = 2,
      margin = 0.01,
      shareX = TRUE,
      shareY = TRUE,
      which_layout = "merge"
    )

    plot <- hide_colorbar(plot)

    plot <- hide_legend(plot)

    mainPlot[[t]] <- plot
  }

  if (length(ids) > 1) {
    finalplot <- subplot(
      mainPlot,
      titleY = TRUE,
      titleX = TRUE,
      nrows = numberRows,
      margin = 0.05,
      shareY = FALSE,
      shareX = FALSE,
      which_layout = "merge"
    )
  } else {
    finalplot <- mainPlot[[1]]
  }

  if (renderEngine %in% "webgl") {
    finalplot <- finalplot %>% plotly::toWebGL()
  }

  finalplot
}

# MARK: plot_spectra_ms1
#' @describeIn MassSpecAnalyses Plot MS1 spectra for the specified analyses and targets.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-colorBy
#' @template arg-legendNames
#' @template arg-labs
#' @template arg-title
#' @template arg-showText
#' @template arg-interactive
#' @export
#' 
plot_spectra_ms1.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 20,
  sec = 60,
  millisec = 5,
  id = NULL,
  mzClust = 0.003,
  presence = 0.8,
  minIntensity = 1000,
  legendNames = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "targets",
  showText = FALSE,
  interactive = TRUE
) {
  ms1 <- get_spectra_ms1(
    x,
    analyses,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    mzClust,
    presence,
    minIntensity
  )

  if (nrow(ms1) == 0) {
    message("\U2717 MS1 traces not found for the targets!")
    return(NULL)
  }

  ms1 <- .make_colorBy_varkey(ms1, colorBy, legendNames)

  ms1$loop <- paste0(ms1$analysis, ms1$replicate, ms1$id, ms1$var)

  cl <- .get_colors(unique(ms1$var))

  if (!interactive) {
    if (is.null(xLab)) {
      xLab <- expression(italic("m/z ") / " Da")
    }
    if (is.null(yLab)) {
      yLab <- "Intensity / counts"
    }

    plot <- ggplot2::ggplot(
      ms1,
      ggplot2::aes(x = mz, y = intensity, group = loop)
    ) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = mz, yend = 0, color = var),
        linewidth = 1
      )

    if (showText) {
      plot <- plot +
        ggplot2::geom_text(
          ggplot2::aes(label = round(mz, 4)),
          vjust = 0.2,
          hjust = -0.2,
          angle = 90,
          size = 2,
          show.legend = FALSE
        )
    }

    plot <- plot +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, max(ms1$intensity) * 1.5)
      ) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)

    plot
  } else {
    if (is.null(xLab)) {
      xLab <- "<i>m/z</i> / Da"
    }
    if (is.null(yLab)) {
      yLab <- "Intensity / counts"
    }

    ticksMin <- plyr::round_any(min(ms1$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(ms1$mz, na.rm = TRUE) * 1.1, 10)

    title <- list(text = title, font = list(size = 12, color = "black"))

    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(ms1$mz) / 10), -1),
      ticks = "outside"
    )

    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black"),
      range = c(0, max(ms1$intensity) * 1.5)
    )

    loop <- NULL

    plot <- ms1 %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~mz,
        y = ~intensity,
        type = "bar",
        color = ~var,
        colors = cl,
        marker = list(line = list(width = 0.01)),
        text = ~ paste0(round(mz, digits = 4), "  "),
        textposition = "outside",
        textangle = 90,
        textfont = list(size = 9)
      ) %>%
      plotly::layout(
        bargap = 1,
        title = title,
        xaxis = xaxis,
        yaxis = yaxis,
        barmode = "overlay",
        uniformtext = list(minsize = 6, mode = "show")
      )

    plot
  }
}

# MARK: plot_spectra_ms2
#' @describeIn MassSpecAnalyses Plot MS2 spectra for the specified analyses and targets.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-colorBy
#' @template arg-legendNames
#' @template arg-labs
#' @template arg-title
#' @template arg-showText
#' @template arg-interactive
#' @export
#'
plot_spectra_ms2.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 20,
  sec = 60,
  millisec = 5,
  id = NULL,
  isolationWindow = 1.3,
  mzClust = 0.005,
  presence = 0.8,
  minIntensity = 0,
  legendNames = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "targets",
  showText = TRUE,
  interactive = TRUE
) {
  ms2 <- get_spectra_ms2(
    x,
    analyses,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    isolationWindow,
    mzClust,
    presence,
    minIntensity
  )

  if (nrow(ms2) == 0) {
    message("\U2717 MS2 traces not found for the targets!")
    return(NULL)
  }

  ms2 <- .make_colorBy_varkey(ms2, colorBy, legendNames)

  loop <- NULL

  ms2$loop <- paste0(ms2$analysis, ms2$replicate, ms2$id, ms2$var)

  cl <- .get_colors(unique(ms2$var))

  if (showText) {
    ms2$text_string <- paste0(round(ms2$mz, 4))
    ms2$text_string[ms2$is_pre] <- paste0("Pre ", ms2$text_string[ms2$is_pre])
  } else {
    ms2$text_string <- ""
  }

  if (!interactive) {
    if (is.null(xLab)) {
      xLab <- expression(italic("m/z ") / " Da")
    }
    if (is.null(yLab)) {
      yLab <- "Intensity / counts"
    }

    ms2$linesize <- 1
    ms2$linesize[ms2$is_pre] <- 2

    plot <- ggplot2::ggplot(
      ms2,
      ggplot2::aes(x = mz, y = intensity, group = loop)
    ) +
      ggplot2::geom_segment(ggplot2::aes(
        xend = mz,
        yend = 0,
        color = var,
        linewidth = linesize
      ))

    if (showText) {
      plot <- plot +
        ggplot2::geom_text(
          ggplot2::aes(label = text_string),
          vjust = 0.2,
          hjust = -0.2,
          angle = 90,
          size = 2,
          show.legend = FALSE
        )
    }

    plot <- plot +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, max(ms2$intensity) * 1.5)
      ) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::scale_linewidth_continuous(range = c(1, 2), guide = "none") +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)

    plot
  } else {
    if (is.null(xLab)) {
      xLab <- "<i>m/z</i> / Da"
    }
    if (is.null(yLab)) {
      yLab <- "Intensity / counts"
    }

    ms2$linesize <- 0.01
    ms2$linesize[ms2$is_pre] <- 2

    ticksMin <- plyr::round_any(min(ms2$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(ms2$mz, na.rm = TRUE) * 1.1, 10)

    title <- list(text = title, font = list(size = 12, color = "black"))

    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(ms2$mz) / 10), -1),
      ticks = "outside"
    )

    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black"),
      range = c(0, max(ms2$intensity) * 1.5)
    )

    plot <- ms2 %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~mz,
        y = ~intensity,
        type = "bar",
        color = ~var,
        colors = cl,
        marker = list(line = list(width = ~linesize)),
        text = ~ paste0(text_string, "  "),
        textposition = "outside",
        textangle = 90,
        textfont = list(size = 9)
      ) %>%
      plotly::layout(
        bargap = 1,
        title = title,
        xaxis = xaxis,
        yaxis = yaxis,
        barmode = "overlay",
        uniformtext = list(minsize = 6, mode = "show")
      )

    plot
  }
}

# MARK: get_raw_chromatograms
#' @describeIn MassSpecAnalyses Get raw chromatograms from the analyses.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-chromatograms
#' @template arg-ms-rtmin
#' @template arg-ms-rtmax
#' @template arg-ms-minIntensity
#' @export
#' 
get_raw_chromatograms.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  chromatograms = NULL,
  rtmin = 0,
  rtmax = 0,
  minIntensity = NULL
) {
  analyses <- .check_analyses_argument(x$analyses, analyses)
  if (is.null(analyses)) {
    return(data.table::data.table())
  }
  chroms_list <- lapply(
    x$analyses[analyses],
    function(z, chromatograms) {
      if (nrow(z$chromatograms_headers) == 0) {
        return(data.frame())
      }
      idx <- z$chromatograms_headers$index
      if (is.numeric(chromatograms)) {
        idx <- idx[chromatograms + 1]
      } else if (is.character(chromatograms)) {
        cid <- z$chromatograms_headers$id
        which_chroms <- cid %in% chromatograms
        idx <- idx[which_chroms]
      } else if (!is.null(chromatograms)) {
        return(data.table::data.table())
      }
      # cache <- StreamFind:::.load_cache_sqlite(
      #   "parsed_ms_chromatograms",
      #   z$file,
      #   idx
      # )
      cache <- NULL
      if (!is.null(cache$data)) {
        message("\U2139 Chromatograms loaded from cache!")
        return(cache$data)
      }

      message(
        "\U2699 Parsing chromatograms from ",
        basename(z$file),
        "...",
        appendLF = FALSE
      )

      chrom <- rcpp_parse_ms_chromatograms(z, idx)
      message(" Done!")

      if (nrow(chrom) == 0) {
        warning("Parsing chromatograms failed!")
        return(data.table::data.table())
      }
      if (!"analysis" %in% colnames(chrom)) {
        chrom$analysis <- z$name
      }
      if (!"replicate" %in% colnames(chrom)) {
        chrom$replicate <- z$replicate
      }
      if (!is.null(cache$hash)) {
        StreamFind:::.save_cache_sqlite(
          "parsed_ms_chromatograms",
          chrom,
          cache$hash
        )
        message("\U1f5ab Parsed chromatograms cached!")
      }
      chrom
    },
    chromatograms = chromatograms
  )

  if (length(chroms_list) == length(analyses)) {
    chroms <- data.table::rbindlist(chroms_list, fill = TRUE)
    if (nrow(chroms) > 0) {
      data.table::setcolorder(chroms, c("analysis", "replicate"))
    }
    if (is.numeric(minIntensity)) {
      chroms <- chroms[chroms$intensity > minIntensity, ]
    }
    if (is.numeric(rtmin) && is.numeric(rtmax)) {
      if (rtmax > 0) chroms <- chroms[chroms$rt >= rtmin & chroms$rt <= rtmax]
    }
    chroms
  } else {
    warning("Defined analyses or chromatograms not found!")
    data.table::data.table()
  }
}

# MARK: load_spectra
#' @describeIn MassSpecAnalyses Load spectra from the analyses, adding them to the results as a `MassSpecSpectra` object.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-allTraces
#' @template arg-ms-isolationWindow
#' @template arg-ms-minIntensityMS1
#' @template arg-ms-minIntensityMS2
#' @export
#' 
load_spectra.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  levels = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 20,
  sec = 60,
  millisec = 5,
  id = NULL,
  allTraces = TRUE,
  isolationWindow = 1.3,
  minIntensityMS1 = 0,
  minIntensityMS2 = 0
) {
  cache <- load_cache(
    "load_spectra",
    info(x),
    analyses,
    levels,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    allTraces,
    isolationWindow,
    minIntensityMS1,
    minIntensityMS2
  )
  
  if (!is.null(cache$data)) {
    x$results[[class(cache$data)[1]]] <- cache$data
    if (!is.null(validate_object(x))) {
      stop("Loaded spectra are not valid!")
    }
    message("\U2139 Spectra loaded from cache!")
    return(x)
  }

  spec <- get_raw_spectra(
    x,
    analyses,
    levels,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    allTraces,
    isolationWindow,
    minIntensityMS1,
    minIntensityMS2
  )

  if (nrow(spec) > 0) {
    split_vector <- spec$analysis
    spec$analysis <- NULL
    spec$replicate <- NULL
    spec <- split(spec, split_vector)

    for (a in get_analysis_names(x)) {
      if (!a %in% names(spec)) {
        spec[[a]] <- data.table::data.table()
      }
    }

    spec <- spec[get_analysis_names(x)]

    spec <- MassSpecResults_Spectra(
      spec,
      replicates = get_replicate_names(x)[names(spec)],
      is_averaged = FALSE
    )
    if (!is.null(cache$hash)) {
      if (save_cache("load_spectra", spec, cache$hash)) {
        message("\U1f5ab Spectra cached!")
      }
    }
    x$results[[class(spec)[1]]] <- spec
    if (!is.null(validate_object(x))) {
      stop("Loaded spectra are not valid!")
    }
  } else {
    warning("Not done! Spectra not found.")
  }
  x
}

# MARK: load_chromatograms
#' @describeIn MassSpecAnalyses Load chromatograms from the analyses, adding them to the results as a `Chromatograms` object.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-chromatograms
#' @template arg-ms-rtmin
#' @template arg-ms-rtmax
#' @template arg-ms-minIntensity
#' @export
#' 
load_chromatograms.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  chromatograms = NULL,
  rtmin = 0,
  rtmax = 0,
  minIntensity = NULL
) {
  chroms <- get_raw_chromatograms(
    x,
    analyses,
    chromatograms,
    rtmin,
    rtmax,
    minIntensity
  )

  if (nrow(chroms) > 0) {
    split_vector <- chroms$analysis
    chroms$analysis <- NULL
    chroms$replicate <- NULL
    chroms <- split(chroms, split_vector)
    chroms <- MassSpecResults_Chromatograms(
      chroms,
      replicates = get_replicate_names(x)[names(chroms)],
      is_averaged = FALSE
    )
    if (!is.null(validate_object(chroms))) {
      stop("Loaded chromatograms are not valid!")
    }
    x$results[["MassSpecResults_Chromatograms"]] <- chroms
  } else {
    warning("Not done! Chromatograms not found.")
  }
  x
}

# MARK: get_matrix_suppression
#' @describeIn MassSpecAnalyses Get TIC matrix suppression for the analyses using the blank replicate analyses as reference for the suppression factor. The `rtWindowVal` argument defines the retention time window in seconds for the suppression factor calculation. The calculation is based on the work from \href{https://pubs.acs.org/doi/10.1021/acs.analchem.1c00357}{Tisler et al. (2021)}.
#' @template arg-x-MassSpecAnalyses
#' @template arg-ms-rtWindowVal
#' @export
#' 
#' @references
#' \insertRef{tisler01}{StreamFind}
#' 
get_matrix_suppression.MassSpecAnalyses <- function(x, rtWindowVal = 10) {
  mpList <- .calculate_tic_matrix_suppression(x, rtWindowVal)
  if (is.null(mpList)) {
    return(data.table::data.table())
  }
  data.table::rbindlist(mpList, fill = TRUE)
}

# MARK: plot_matrix_suppression
#' @describeIn MassSpecAnalyses Plot TIC matrix suppression for the specified analyses. The `rtWindowVal` argument defines the retention time window in seconds for the suppression factor calculation. The calculation is based on the work from \href{https://pubs.acs.org/doi/10.1021/acs.analchem.1c00357}{Tisler et al. (2021)}.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-rtWindowVal
#' @template arg-labs
#' @template arg-title
#' @template arg-ms-colorBy
#' @template arg-legendNames
#' @template arg-ms-downsize
#' @template arg-interactive
#' @template arg-showLegend
#' @template arg-renderEngine
#' @export
#' 
#' @references
#' \insertRef{tisler01}{StreamFind}
#' 
plot_matrix_suppression.MassSpecAnalyses <- function(
  x,
  analyses = NULL,
  rtWindowVal = 10,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "analyses",
  legendNames = NULL,
  downsize = 1,
  interactive = TRUE,
  showLegend = TRUE,
  renderEngine = "webgl"
) {
  analyses <- .check_analyses_argument(x$analyses, analyses)
  mp <- get_matrix_suppression(x, rtWindowVal)
  if (nrow(mp) == 0) {
    message("\U2717 TIC matrix suppression not found for the analyses!")
    return(NULL)
  }
  sel <- mp$analysis %in% analyses
  mp <- mp[sel, ]
  if (nrow(mp) == 0) {
    message("\U2717 TIC matrix suppression not found for the analyses!")
    return(NULL)
  }
  if (!"id" %in% colnames(mp)) {
    mp$id <- mp$analysis
  }
  mp$intensity <- mp$mp
  if (is.null(yLab)) {
    yLab <- "Supression Factor"
  }
  if (is.null(xLab)) {
    xLab <- "Retention time / seconds"
  }

  mp <- .make_colorBy_varkey(mp, colorBy, legendNames)
  mp$loop <- paste0(mp$analysis, mp$replicate, mp$id, mp$var)
  cl <- .get_colors(unique(mp$var))

  if (!interactive) {
    ggplot2::ggplot(mp, ggplot2::aes(x = rt, y = intensity, group = loop)) +
      ggplot2::geom_line(ggplot2::aes(color = var)) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black")
    )
    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black")
    )

    loop <- NULL

    plot <- mp %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines", #+markers
        line = list(width = 2),
        # marker = list(size = 2.5),
        text = ~ paste(
          "<br>analysis: ",
          analysis,
          "<br>replicate: ",
          replicate,
          "<br>id: ",
          id,
          "<br>polarity: ",
          polarity,
          "<br>level: ",
          level,
          "<br>rt: ",
          rt,
          "<br>suppression: ",
          intensity
        ),
        hoverinfo = "text",
        showlegend = showLegend
      ) %>%
      plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )

    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }

    plot
  }
}

# MARK: Utility functions
# Utility functions -----

# MARK: .get_MassSpecAnalysis_from_files
#' @noRd
.get_MassSpecAnalysis_from_files <- function(
  files = NULL,
  centroid = FALSE,
  levels = c(1, 2)
) {
  if (!is.null(files)) {
    if (is.data.frame(files)) {
      if (all(c("path", "analysis") %in% colnames(files))) {
        files$file <- vapply(
          seq_len(nrow(files)),
          function(x) {
            list.files(
              files$path[x],
              pattern = files$analysis[x],
              full.names = TRUE,
              recursive = FALSE
            )
          },
          ""
        )
      }

      if ("file" %in% colnames(files)) {
        if ("replicate" %in% colnames(files)) {
          replicates <- as.character(files$replicate)
        } else if ("group" %in% colnames(files)) {
          replicates <- as.character(files$group)
        } else {
          replicates <- rep(NA_character_, nrow(files))
        }

        if ("blank" %in% colnames(files)) {
          blanks <- as.character(files$blank)
        } else {
          blanks <- rep(NA_character_, nrow(files))
        }

        files <- files$file
      } else {
        files <- NA_character_
      }
    } else {
      replicates <- rep(NA_character_, length(files))
      blanks <- rep(NA_character_, length(files))
    }

    possible_ms_file_formats <- ".mzML$|.mzXML$|.d$|.raw$|.wiff$"

    valid_files <- vapply(
      files,
      FUN.VALUE = FALSE,
      function(x, possible_ms_file_formats) {
        if (!file.exists(x)) {
          return(FALSE)
        }
        if (FALSE %in% grepl(possible_ms_file_formats, x)) {
          return(FALSE)
        }
        TRUE
      },
      possible_ms_file_formats = possible_ms_file_formats
    )

    if (!all(valid_files)) {
      warning("File/s not valid!")
      return(NULL)
    }

    names(replicates) <- as.character(files)
    names(blanks) <- as.character(files)

    files_to_convert <- vapply(
      files,
      function(x) grepl("d|raw|wiff", tools::file_ext(x)),
      FALSE
    )

    if (any(files_to_convert)) {
      files_to_convert <- files[files_to_convert]
      files_converted <- gsub(".d$", ".mzML", files_to_convert)
      files_converted <- gsub(".raw$", ".mzML", files_converted)
      files_converted <- gsub(".wiff$", ".mzML", files_converted)

      for (i in seq_along(files_converted)) {
        if (file.exists(files_converted[i])) {
          if (files_converted[i] %in% files) {
            files <- files[!files %in% files_to_convert[i]]
          } else {
            files[files == files_to_convert[i]] <- files_converted[i]
          }
        } else {
          dir_search <- dirname(files_converted[i])

          fl_already_converted <- list.files(
            dir_search,
            pattern = paste0(
              "^",
              basename(tools::file_path_sans_ext(files_converted[i])),
              "-.*\\.mzML$"
            ),
            full.names = TRUE
          )

          if (length(fl_already_converted) == 1) {
            files_converted[i] <- fl_already_converted
            files <- files[!files %in% fl_already_converted]
            files[files == files_to_convert[i]] <- fl_already_converted
          } else if (length(fl_already_converted) > 1) {
            warning(paste0(
              "Multiple converted files found for: ",
              basename(tools::file_path_sans_ext(files_to_convert[i])),
              ". Please check the files!"
            ))
            return(NULL)
          }
        }
      }

      files_to_convert_sel <- vapply(
        files,
        function(x) {
          grepl("d|raw|wiff", tools::file_ext(x))
        },
        FALSE
      )

      files_to_convert <- files[files_to_convert_sel]

      if (length(files_to_convert) > 0) {
        filter <- ""

        if (centroid) {
          filter <- "peakPicking vendor"
        }

        if (centroid && is.numeric(levels) && length(levels) > 0) {
          levels <- paste(levels, collapse = "-")
          levels <- paste("msLevel=", levels, collapse = "", sep = "")
          if (filter != "") {
            filter <- paste(filter, levels, sep = " ")
          } else {
            filter <- levels
          }
        }

        optList <- list()
        if (filter != "") {
          optList <- list(filter = filter)
        }

        tryCatch(
          {
            StreamFind::convert_ms_files(
              files = files_to_convert,
              outputFormat = "mzML",
              outputPath = NULL,
              optList = optList
            )

            files <- files[!files %in% files_to_convert]

            for (i in seq_along(files_to_convert)) {
              dir_search <- dirname(files_to_convert[i])

              fl_converted_as_is <- list.files(
                dir_search,
                pattern = paste0(
                  basename(tools::file_path_sans_ext(files_to_convert[i])),
                  ".mzML$"
                ),
                full.names = TRUE
              )

              if (length(fl_converted_as_is) == 1) {
                files <- c(files, fl_converted_as_is)
              } else if (length(fl_converted_as_is) > 1) {
                warning(paste0(
                  "Multiple converted files found for: ",
                  basename(tools::file_path_sans_ext(files_to_convert[i])),
                  ". Please check the files!"
                ))
              } else if (length(fl_converted_as_is) == 0) {
                fl_converted <- list.files(
                  dir_search,
                  pattern = paste0(
                    "^",
                    basename(tools::file_path_sans_ext(files_to_convert[i])),
                    "-.*\\.mzML$"
                  ),
                  full.names = TRUE
                )

                if (length(fl_converted) == 1) {
                  files <- c(files, fl_converted)
                } else if (length(fl_converted) > 1) {
                  warning(paste0(
                    "Multiple converted files found for: ",
                    basename(tools::file_path_sans_ext(files_to_convert[i])),
                    ". Please check the files!"
                  ))
                }
              }
            }

            exist_files <- vapply(files, function(x) file.exists(x), FALSE)
            files <- files[exist_files]
          },
          error = function(e) {
            warning("Error converting files!")
            files <- files[!files %in% files_to_convert]
          },
          warning = function(w) {
            warning("Warning converting files!")
            files <- files[!files %in% files_to_convert]
          }
        )
      }
    }

    analyses <- lapply(files, function(x) {
      cache <- .load_chache_rds("parsed_ms_analyses", x)
      # cache <- .load_cache_sqlite("parsed_ms_analyses", x)
      if (!is.null(cache$data)) {
        message("\U2139 ", basename(x), " analysis loaded from cache!")
        cache$data
      } else {
        message("\U2699 Parsing ", basename(x), "...", appendLF = FALSE)
        ana <- rcpp_parse_ms_analysis(x)
        class_ana <- class(ana)[1]

        if (!class_ana %in% "MassSpecAnalysis") {
          message(" Not Done!")
          return(NULL)
        }

        message(" Done!")

        rpl <- replicates[x]

        if (is.na(rpl)) {
          rpl <- ana$name
          rpl <- sub("-[^-]+$", "", rpl)
        }

        ana$replicate <- rpl
        blk <- blanks[x]

        if (!is.na(blk)) {
          ana$blank <- blk
        }

        ana$blank <- blk

        concentration <- suppressWarnings(as.numeric(ana$name))

        if (is.na(concentration)) {
          ana$concentration <- NA_real_
        } else {
          ana$concentration <- concentration
        }

        if (!is.null(cache$hash)) {
          .save_cache_rds("parsed_ms_analyses", ana, cache$hash)
          # .save_cache_sqlite("parsed_ms_analyses", ana, cache$hash)
          message("\U1f5ab Parsed analysis cached!")
        }

        ana
      }
    })

    names(analyses) <- vapply(analyses, function(x) x[["name"]], "")
    analyses <- analyses[order(names(analyses))]

    if (
      all(vapply(analyses, function(x) "MassSpecAnalysis" %in% is(x), FALSE))
    ) {
      analyses
    } else {
      list()
    }
  } else {
    list()
  }
}

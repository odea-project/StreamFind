#' **MassSpecAnalysis** S3 class constructor, methods and functions
#'
#' @description
#' Creates a *MassSpecAnalysis* S3 class object.
#'
#' @param name *mzML* or *mzXML* file name without extension.
#' @param replicate Character with length one, representing the analysis replicate group name.
#' @param blank Character with length one, representing the associated blank replicate group name.
#' @param file *mzML* or *mzXML* full file path (with extension).
#' @param format Character with length one. One of *mzML* or *mzXML*.
#' @param type Character with length one defining the type of analysis.
#' @param spectra_number Integer with the number of spectra in the file.
#' @param spectra_headers data.table run information for each spectrum.
#' @param spectra data.table with the raw spectra (only present if loaded).
#' @param chromatograms_number Integer with the number of chromatograms in the file.
#' @param chromatograms_headers data.table headers information for each chromatogram.
#' @param chromatograms data.table with the raw chromatograms (only present if loaded).
#' @param metadata List with flexible storage for experimental metadata (e.g., concentration, location, etc.).
#'
#' @return An *MassSpecAnalysis* S3 class object.
#'
#' @export
#'
MassSpecAnalysis <- function(name = NA_character_,
                             replicate = NA_character_,
                             blank = NA_character_,
                             file = NA_character_,
                             format = NA_character_,
                             type = NA_character_,
                             spectra_number = NA_integer_,
                             spectra_mode = NA_character_,
                             spectra_levels = NA_integer_,
                             spectra_headers = data.table(),
                             spectra = data.table(),
                             chromatograms_number = NA_integer_,
                             chromatograms_headers = data.table(),
                             chromatograms = data.table(),
                             metadata = list()) {
  
  x <- Analysis(name, replicate, blank)

  x <- c(x, list(
    "file" = as.character(file),
    "format" = as.character(format),
    "type" = as.character(type),
    "spectra_number" = as.integer(spectra_number),
    "spectra_mode" = as.integer(spectra_mode),
    "spectra_levels" = as.integer(spectra_levels),
    "spectra_headers" = as.data.table(spectra_headers),
    "spectra" = as.data.table(spectra),
    "chromatograms_number" = as.integer(chromatograms_number),
    "chromatograms_headers" = as.data.table(chromatograms_headers),
    "chromatograms" = as.data.table(chromatograms),
    "metadata" = metadata
  ))

  if (validate.MassSpecAnalysis(x)) {
    
    x <- structure(x, class = c("MassSpecAnalysis", "Analysis"))
    
    x
    
  } else {
    NULL
  }
}

#' @export
#' @noRd
#'
validate.MassSpecAnalysis <- function(x = NULL) {
  
  valid <- validate.Analysis(x)

  if (valid) {
    name <- x$name
    
    if (length(x$file) != 1 || !is.character(x$file)) {
      warning("Analysis file path entry not conform!")
      valid <- FALSE
    } else if (!is.na(x$file) && !file.exists(x$file)) {
      warning(paste0(x$file, " does not exist!"))
      valid <- FALSE
    }
    
    if (length(x$format) != 1) {
      warning("Analysis format not conform!")
      valid <- FALSE
    } else if (!(x$format %in% c("mzML", "mzXML"))) {
      warning("Analysis format must be 'mzML' ot 'mzXML'!")
      valid <- FALSE
    }

    if (length(x$type) != 1) {
      warning("Analysis type entry not conform!")
      valid <- FALSE
      
    } else if (!(x$type %in% c("MS", "IM-MS", "MS/MS-DDA", "MS/MS-DIA", "MS/MS-AllIons", "IM-MS/MS-DDA", "IM-MS/MS-DIA", "IM-MS/MS-AllIons", "SRM"))) {
      warning("Analysis type must be 'MS', 'IM-MS', 'MS/MS-DDA', 'MS/MS-DIA', 'MS/MS-AllIons', 'IM-MS/MS-DDA', 'IM-MS/MS-DIA', 'IM-MS/MS-AllIons', or 'SRM'!")
      valid <- FALSE
    }

    if (!is.integer(x$spectra_number) &&
        length(x$spectra_number) != 1) {
      warning("Analysis spectra_numebr entry not conform!")
      valid <- FALSE
    }

    if (!is.integer(x$chromatograms_number) &&
        length(x$chromatograms_number) != 1) {
      warning("Analysis chromatograms_number entry not conform!")
      valid <- FALSE
    }
    
    if (!is.data.frame(x$spectra_headers)) {
      warning("Analysis spectra_headers entry not conform!")
      valid <- FALSE
    }
    
    if (!is.data.frame(x$chromatograms_headers)) {
      warning("Analysis chromatograms_headers entry not conform!")
      valid <- FALSE
    }

    if (!is.data.frame(x$spectra)) {
      warning("Analysis spectra entry not conform!")
      valid <- FALSE
    }

    if (!is.data.frame(x$chromatograms)) {
      warning("Analysis chromatograms entry not conform!")
      valid <- FALSE
    }

    if (!is.list(x$metadata)) {
      warning("Analysis metadata entry not conform!")
      valid <- FALSE
    }
    
    if (!valid) warning("Issue/s found with analysis ", x$name, "!")
  }

  valid
}

#' @export
#' @noRd
#'
print.MassSpecAnalysis <- function(x, ...) {
  cat("\n")
  cat(
    " ", class(x), "\n"
  )
  cat(
    "  name              ", x$name, "\n",
    "  replicate         ", x$replicate, "\n",
    "  blank             ", x$blank, "\n",
    "  format            ", x$format, "\n",
    "  type              ", x$type, "\n",
    "  spectra number    ", x$spectra_number, "\n",
    "  spectra mode      ", paste(unique(x$spectra_headers$mode), collapse = " "), "\n",
    "  spectra levels    ", paste(unique(x$spectra_headers$level), collapse = " "), "\n",
    "  chromatograms     ", x$chromatograms_number, "\n",
    "  has ion mobility  ", any(x$spectra_headers$drift > 0), "\n",
    "  polarity          ", paste(unique(x$spectra_headers$polarity), collapse = "; "), "\n",
    sep = ""
  )
  cat("\n")
}

#' @export
#' @noRd
#'
as.MassSpecAnalysis <- function(value) {
  if (length(value) == 1 & is.list(value)) value <- value[[1]]
  do.call(MassSpecAnalysis, value)
}

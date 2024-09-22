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
                             spectra_headers = data.table::data.table(),
                             spectra = data.table::data.table(),
                             chromatograms_number = NA_integer_,
                             chromatograms_headers = data.table::data.table(),
                             chromatograms = data.table::data.table(),
                             metadata = list()) {
  
  x <- list(
    "name" = as.character(name),
    "replicate" = as.character(replicate),
    "blank" = as.character(blank),
    "file" = as.character(file),
    "format" = as.character(format),
    "type" = as.character(type),
    "spectra_number" = as.integer(spectra_number),
    "spectra_headers" = data.table::as.data.table(spectra_headers),
    "spectra" = data.table::as.data.table(spectra),
    "chromatograms_number" = as.integer(chromatograms_number),
    "chromatograms_headers" = data.table::as.data.table(chromatograms_headers),
    "chromatograms" = data.table::as.data.table(chromatograms),
    "metadata" = metadata
  )

  x <- structure(x, class = c("MassSpecAnalysis", "Analysis"))

  x
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

#' @noRd
as.MassSpecAnalysis <- function(value) {
  if (length(value) == 1 & is.list(value)) value <- value[[1]]
  do.call(MassSpecAnalysis, value)
}

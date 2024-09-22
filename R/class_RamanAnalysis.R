#' **RamanAnalysis** S3 class constructor, methods and functions
#'
#' @description Creates a *RamanAnalysis* S3 class object.
#'
#' @param name *asc* file name without extension.
#' @param replicate Character with length one, representing the analysis replicate group name.
#' @param blank Character with length one, representing the associated blank replicate group name.
#' @param file *asc* full file path (with extension).
#' @param type String with the type of analysis.
#' @param metadata List of analysis metadata taken from the *asc* file or added.
#' @param spectra data.table with the raw spectra.
#'
#' @return An *RamanAnalysis* S3 class object.
#'
#' @export
#'
RamanAnalysis <- function(name = NA_character_,
                          replicate = NA_character_,
                          blank = NA_character_,
                          file = NA_character_,
                          type = NA_character_,
                          metadata = list(),
                          spectra = data.table::data.table()) {
  
  x <- list(
    "name" = as.character(name),
    "replicate" = as.character(replicate),
    "blank" = as.character(blank),
    "file" = as.character(file),
    "type" = as.character(type),
    "metadata" = metadata,
    "spectra" = data.table::as.data.table(spectra)
  )
  
  x <- structure(x, class = c("RamanAnalysis", "Analysis"))
  
  x
}

#' @export
#' @noRd
print.RamanAnalysis <- function(x, ...) {
  cat("\n")
  cat(
    " ", class(x), "\n"
  )
  cat(
    "  name              ", x$name, "\n",
    "  replicate         ", x$replicate, "\n",
    "  blank             ", x$blank, "\n",
    "  type              ", x$type, "\n",
    sep = ""
  )
  cat("\n")
}

#' @noRd
as.RamanAnalysis <- function(value) {
  if (length(value) == 1 & is.list(value)) value <- value[[1]]
  do.call(RamanAnalysis, value)
}

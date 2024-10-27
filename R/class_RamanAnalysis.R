#' @export
#' @noRd
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

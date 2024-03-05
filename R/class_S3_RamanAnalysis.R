#' **RamanAnalysis** S3 class constructor, methods and functions
#'
#' @description Creates a *RamanAnalysis* S3 class object.
#'
#' @param name *asc* file name without extension.
#' @param replicate Character with length one, representing the analysis replicate group name.
#' @param blank Character with length one, representing the associated blank replicate group name.
#' @param file *asc* full file path (with extension).
#' @param type 
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
                          spectra = data.table()) {
  
  x <- Analysis(name, replicate, blank)

  x <- c(x, list(
    "file" = as.character(file),
    "type" = as.character(type),
    "metadata" = metadata,
    "spectra" = as.data.table(spectra)
  ))

  if (validate.RamanAnalysis(x)) {
    
    x <- structure(x, class = c("RamanAnalysis", "Analysis"))
    
    x
    
  } else {
    NULL
  }
}

#' @describeIn RamanAnalysis
#' S3 method to validate a *RamanAnalysis* S3 class object, returning a logical value of length one.
#'
#' @param x A *RamanAnalysis* S3 class object.
#'
#' @export
#'
validate.RamanAnalysis <- function(x = NULL) {
  
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

    if (!is.data.frame(x$spectra)) {
      warning("Analysis spectra entry not conform!")
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

#' @describeIn RamanAnalysis
#' S3 method to print the *RamanAnalysis* S3 class object in the console.
#'
#' @param ... Not used.
#'
#' @export
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

#' @describeIn RamanAnalysis
#' S3 method to converts a *RamanAnalysis* S3 class object into a JSON string.
#'
#' @export
asJSON.RamanAnalysis <- function(x) {
  toJSON(
    x,
    dataframe = "columns",
    Date = "ISO8601",
    POSIXt = "string",
    factor = "string",
    complex = "string",
    null = "null",
    na = "null",
    auto_unbox = FALSE,
    digits = 8,
    pretty = TRUE,
    force = TRUE
  )
}

#' @describeIn RamanAnalysis
#' S3 method to convert the argument value in a *RamanAnalysis* S3 class object.
#'
#' @param value A list to be checked and/or converted to *RamanAnalysis* S3 class.
#'
#' @export
as.RamanAnalysis <- function(value) {
  if (length(value) == 1 & is.list(value)) value <- value[[1]]
  do.call(RamanAnalysis, value)
}

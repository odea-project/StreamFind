#' **StatisticAnalysis** S3 class constructor, methods and functions
#'
#' @description Creates a *StatisticAnalysis* S3 class object.
#'
#' @param name Character (length 1) with the unique identifier.
#' @param replicate Character (length 1), representing the analysis replicate group name.
#' @param blank Character (length 1), representing the associated blank replicate group name.
#' @param data Matrix with 1 row and columns equal to the number of variables.
#' @param classes Character vector with the analysis classes.
#'
#' @return An *StatisticAnalysis* S3 class object.
#'
#' @export
#'
StatisticAnalysis <- function(name = NA_character_,
                              replicate = NA_character_,
                              blank = NA_character_,
                              data = matrix(),
                              classes = NA_character_) {
  
  x <- Analysis(name, replicate, blank)

  x <- c(x, list("data" = data, "classes" = as.character(classes)))

  if (validate.StatisticAnalysis(x)) {
    x <- structure(x, class = c("StatisticAnalysis", "Analysis"))
    x
  } else {
    NULL
  }
}

#' @export
#' @noRd
#'
validate.StatisticAnalysis <- function(x = NULL) {
  
  valid <- validate.Analysis(x)

  if (valid) {
    
    if (is.matrix(x$data)) {
      
      if (nrow(x$data) != 1) {
        warning("Analysis data columns not conform!")
        valid <- FALSE
      }
      
      if (rownames(x$data)[1] != x$name) {
        warning("Analysis data row name not conform!")
        valid <- FALSE
      }
      
    } else {
      warning("Analysis data entry not conform!")
      valid <- FALSE
    }

    if (!is.character(x$classes)) {
      warning("Analysis classes entry not conform!")
      valid <- FALSE
    }
    
    if (!valid) warning("Issue/s found with analysis ", x$name, "!")
  }

  valid
}

#' @export
#' @noRd
#'
print.StatisticAnalysis <- function(x, ...) {
  cat("\n")
  cat(
    " ", class(x), "\n"
  )
  cat(
    "  name              ", x$name, "\n",
    "  replicate         ", x$replicate, "\n",
    "  blank             ", x$blank, "\n",
    "  N. variables      ", ncol(x$data), "\n",
    "  classes           ", paste(x$classes, collapse = ", "), "\n",
    sep = ""
  )
  cat("\n")
}

#' @export
#' @noRd
#'
as.StatisticAnalysis <- function(value) {
  if (length(value) == 1 & is.list(value)) value <- value[[1]]
  do.call(StatisticAnalysis, value)
}

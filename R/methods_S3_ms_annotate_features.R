
#' @title .s3_ms_annotate_features.streamFind
#'
#' @description Annotates features with isotopes.
#'
#' @noRd
#'
.s3_ms_annotate_features.streamFind <- function(settings, self) {

  if (!any(self$has_features())) {
    warning("Features were not found! Run find_features method first!")
    return(NULL)
  }

  print(class(self))

  # TODO implement the rcpp interface with parallel processing and cached interface

  FALSE
}

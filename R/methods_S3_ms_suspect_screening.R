#' @title .s3_ms_suspect_screening.Settings_suspect_screening_streamFind
#'
#' @description Makes suspect screening on features.
#'
#' @noRd
#'
.s3_ms_suspect_screening.Settings_suspect_screening_streamFind <- function(settings, self) {

  if (!validate(settings)) return(FALSE)

  suspect_features <- self$get_suspects(
    database = settings$parameters$database,
    ppm = settings$parameters$ppm,
    sec = settings$parameters$sec
  )

  suspect_groups <- self$get_groups(groups = unique(suspect_features$group))

  if (nrow(suspects) > 0) {
    suspects_data <- list(
      "suspect_features" = suspect_features,
      "suspect_groups" = suspect_groups,
      "database" = settings$parameters$database
    )

    output <- list(suspects_data)
    names(output) <- settings$call

    self$add_modules_data(output)

    TRUE
  } else {
    FALSE
  }
}

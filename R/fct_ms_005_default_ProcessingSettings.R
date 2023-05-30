
#' @title get_default_ProcessingSettings
#'
#' @description X.
#'
#' @param call X.
#' @param software X.
#' @param algorithm X.
#'
#' @return A ProcessingSettings S3 class object.
#'
#' @export
#'
get_default_ProcessingSettings <- function(call = NA_character_,
                                           software = NA_character_,
                                           algorithm = NA_character_) {

  settings <- NULL

  if ("find_features" %in% call) {

    if ("centwave" %in% algorithm) {
      settings <- .default_find_features_xcms3_centwave()
    }


  }

  return(settings)
}

#' @title .default_find_features_xcms3_centwave
#'
#' @description X.
#'
#' @return X.
#'
#' @noRd
#'
.default_find_features_xcms3_centwave <- function() {

  if (!requireNamespace("xcms", quietly = TRUE)) {
    warning("xcms package required but not installed!")
    return(NULL)
  }

  settings <- list(
    call = "find_features",
    algorithm = "xcms3",
    parameters = xcms::CentWaveParam(),
    software = "xcms",
    developer = "Ralf Tautenhahn, Johannes Rainer",
    contact = "rtautenh@ipb-halle.de",
    link = "https://rdrr.io/bioc/xcms/man/findChromPeaks-centWave.html",
    doi = "https://doi.org/10.1186/1471-2105-9-504"
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @title save_default_ProcessingSettings
#'
#' @description X.
#'
#' @param call X.
#' @param software X.
#' @param algorithm X.
#' @param format X.
#' @param name X.
#' @param path X.
#'
#' @return Creates a json/rds files on the defined path.
#'
#' @export
#'
save_default_ProcessingSettings <- function(call = NA_character_,
                                            software = NA_character_,
                                            algorithm = NA_character_,
                                            format = "json",
                                            name = "settings",
                                            path = getwd()) {

  settings <- get_default_ProcessingSettings(call, software, algorithm)

  if (format %in% "json") {
    settings <- toJSON(
      settings,
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
    write(settings, file = paste0(path, "/", name, ".json"))
  }

  if (format %in% "rds") {
    saveRDS(settings, file = paste0(path, "/", name, ".rds"))
  }
}

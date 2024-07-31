
#' @title get_default_ProcessingSettings
#'
#' @description Gets the ProcessingSettings S3 class object as defined by `call` and `algorithm`.
#'
#' @param call Character (length 1) with the method call name.
#' @param algorithm Character (length 1) with the algorithm name.
#'
#' @return A ProcessingSettings S3 class object with subclass as defined by `call` and `algorithm`.
#'
#' @export
#'
get_default_ProcessingSettings <- function(engine = NA_character_, call = NA_character_, algorithm = NA_character_) {
  class_string <- paste0(engine,"Settings_", call, "_", algorithm)
  do.call(class_string, list())
}

#' @title save_default_ProcessingSettings
#'
#' @description Saves on disk a ProcessingSettings S3 class object for `call` and `algorithm` as the defined `format` 
#' in the defined `path` and with the defined `name`.
#'
#' @param call Character (length 1) with the method call name.
#' @param algorithm Character (length 1) with the algorithm name.
#' @param format Character (length 1) with the format of the saved file. Possible are "json" and "rds".
#' @param name Character (length 1) with the name of the file without extension.
#' @param path Character (length 1) with the saving path. The default is the `getwd()` path.
#'
#' @return Creates a json/rds files on the defined path.
#'
#' @export
#'
save_default_ProcessingSettings <- function(engine = NA_character_,
                                            call = NA_character_,
                                            algorithm = NA_character_,
                                            format = "json", 
                                            name = "settings",
                                            path = getwd()) {

  settings <- get_default_ProcessingSettings(engine, call, algorithm)

  if (format %in% "json") {
    
    settings_js <- toJSON(
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
    
    write(settings_js, file = paste0(path, "/", name, ".json"))
  }

  if (format %in% "rds") saveRDS(settings, file = paste0(path, "/", name, ".rds"))
}

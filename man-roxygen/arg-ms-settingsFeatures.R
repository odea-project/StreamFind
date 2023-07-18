#' @param settingsFeatures A ProcessingSettings S3 class object with the call name
#' applicable to the processing method for features. Alternatively, a named
#' list with `call`, `algorithm` and `parameters` to be transformed and used as
#' ProcessingSettings S3 class object. When not given, settings for the
#' features processing method will be searched within the `MassSpecData` object.

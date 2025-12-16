#' @title DataTypeObjects Class
#' @description Constructor function for DataTypeObjects class, defining data types used in the package, associated file formats, and result classes.
#' @param data_type Optional; specific data type to retrieve information for. If NULL, returns all data types.
#' @param showProcessingMethods Logical; if TRUE and data_type is provided, includes available processing methods for that data type.
#' @return An object of class DataTypeObjects containing data types, engines, analyses, results, and formats.
#' @export
#' 
DataTypeObjects <- function(data_type = NULL, showProcessingMethods = FALSE) {
  res <- structure(
    list(
      data_types = c("MassSpec", "Raman", "Statistic", "DB_MassSpec"),
      engine = list(
        "MassSpec" = "MassSpecEngine",
        "Raman" = "RamanEngine",
        "Statistic" = "StatisticEngine",
        "DB_MassSpec" = "DB_MassSpecEngine"
      ),
      analyses = list(
        "MassSpec" = "MassSpecAnalyses",
        "Raman" = "RamanAnalyses",
        "Statistic" = "StatisticAnalyses",
        "DB_MassSpec" = "DB_MassSpecAnalyses"
      ),
      results = list(
        "MassSpec" = c("MassSpecResults_NonTargetAnalysis", "MassSpecResults_Spectra", "MassSpecResults_Chromatograms"),
        "Raman" = c("RamanResults_Spectra"),
        "Statistic" = c("StatisticResults"),
        "DB_MassSpec" = c("DB_MassSpecResults_NonTargetAnalysis", "DB_MassSpecResults_Chromatograms")
      ),
      formats = list(
        "MassSpec" = c("mzML", "mzXML", "d", "raw"),
        "Raman" = c("asc", "sif", "json", "wdf", "sdf", "csv", "txt"),
        "Statistic" = c("csv"),
        "DB_MassSpec" = c("mzML", "mzXML", "d", "raw")
      )
    ),
    class = "DataTypeObjects"
  )

  if (!is.null(data_type)) {
    if (!(data_type %in% res$data_types)) {
      stop(
        paste0(
          "Data type '",
          data_type,
          "' is not recognized. Available types are: ",
          paste(res$types, collapse = ", "),
           "."
        )
      )
    }
    res <- list(
      engine = res$engine[[data_type]],
      analyses = res$analyses[[data_type]],
      results = res$results[[data_type]],
      formats = res$formats[[data_type]]
    )
    if (isTRUE(showProcessingMethods)) {
      processing_methods <- .list_processing_steps_metadata(data_type = data_type)
      res[["processing_methods"]] <- processing_methods
    }
  }
  res
}

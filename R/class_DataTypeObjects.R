#' @title DataTypeObjects Class
#' @description Constructor function for DataTypeObjects class, defining data types used in the package, associated file formats, and result classes.
#' @param dataType Optional; specific data type to retrieve information for. If NULL, returns all data types.
#' @param showProcessingMethods Logical; if TRUE and dataType is provided, includes available processing methods for that data type.
#' @return An object of class DataTypeObjects containing data types, engines, analyses, results, and formats.
#' @export
#'
DataTypeObjects <- function(dataType = NULL, showProcessingMethods = FALSE) {
  res <- structure(
    list(
      data_types = c("MassSpec", "Raman", "Statistic"),
      engine = list(
        "MassSpec" = "MassSpecEngine",
        "Raman" = "RamanEngine",
        "Statistic" = "StatisticEngine"
      ),
      analyses = list(
        "MassSpec" = "MassSpecAnalyses",
        "Raman" = "RamanAnalyses",
        "Statistic" = "StatisticAnalyses"
      ),
      results = list(
        "MassSpec" = c("MassSpecResults_NonTargetAnalysis", "MassSpecResults_Spectra", "MassSpecResults_Chromatograms"),
        "Raman" = c("RamanResults_Spectra"),
        "Statistic" = c("StatisticResults")
      ),
      formats = list(
        "MassSpec" = c("mzML", "mzXML", "d", "raw"),
        "Raman" = c("asc", "sif", "json", "wdf", "sdf", "csv", "txt"),
        "Statistic" = c("csv")
      )
    ),
    class = "DataTypeObjects"
  )

  if (!is.null(dataType)) {
    if (!(dataType %in% res$data_types)) {
      stop(
        paste0(
          "Data type '",
          dataType,
          "' is not recognized. Available types are: ",
          paste(res$types, collapse = ", "),
           "."
        )
      )
    }
    res <- list(
      engine = res$engine[[dataType]],
      analyses = res$analyses[[dataType]],
      results = res$results[[dataType]],
      formats = res$formats[[dataType]]
    )
    if (isTRUE(showProcessingMethods)) {
      processing_methods <- .list_processing_steps_metadata(dataType = dataType)
      res[["processing_methods"]] <- processing_methods
    }
  }
  res
}

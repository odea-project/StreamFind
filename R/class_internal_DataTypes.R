#' @title DataTypeObjects Class
#' @description Constructor function for DataTypeObjects class, defining data types used in the package, associated file formats, and result classes.
#' @param data_type Optional; specific data type to retrieve information for. If NULL, returns all data types.
#' @export
#' 
DataTypeObjects <- function(data_type = NULL) {
  res <- structure(
    list(
      data_types = c("MassSpec", "Raman", "Statistic", "DB_MassSpec"),
      engine = list(
        "MassSpec" = "MassSpecEngine",
        "Raman" = c("RamanEngine"),
        "Statistic" = c("StatisticEngine"),
        "DB_MassSpec" = c("DB_MassSpecEngine")
      ),
      analyses = list(
        "MassSpec" = c("MassSpecAnalyses"),
        "Raman" = c("RamanAnalyses"),
        "Statistic" = c("StatisticAnalyses"),
        "DB_MassSpec" = c("DB_MassSpecAnalyses")
      ),
      results = list(
        "MassSpec" = c("MassSpecResults_NonTargetAnalysis", "MassSpecSpectra", "Chromatograms"),
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
    
    processing_methods <- .list_processing_steps_metadata(data_type = data_type)

    return(
      list(
        engine = res$engine[[data_type]],
        analyses = res$analyses[[data_type]],
        processing_methods = processing_methods,
        result = res$results[[data_type]],
        formats = res$formats[[data_type]]
      )
    )
  }
}

#' DataTypes Class
#' @description Internal function to define data types used in the package, associated file formats, and result classes.
#' @noRd
DataTypes <- function(data_type = NULL) {
  res <- structure(
    list(
      types = c("MassSpec", "Raman", "Statistic", "DB_MassSpec"),
      engine = list(
        "MassSpec" = "MassSpecEngine",
        "Raman" = c("RamanEngine"),
        "Statistic" = c("StatisticEngine"),
        "DB_MassSpec" = c("DB_MassSpecEngine")
      ),
      file_formats = list(
        "MassSpec" = c("mzML", "mzXML", "d", "raw"),
        "Raman" = c("asc", "sif", "json", "wdf", "sdf", "csv", "txt"),
        "Statistic" = c("csv"),
        "DB_MassSpec" = c("mzML", "mzXML", "d", "raw")
      ),
      results = list(
        "MassSpec" = c("MassSpecResults_NonTargetAnalysis", "MassSpecSpectra", "Chromatograms"),
        "Raman" = c("RamanResults_Spectra"),
        "Statistic" = c("StatisticResults"),
        "DB_MassSpec" = c("DB_MassSpecResults_NonTargetAnalysis", "DB_MassSpecResults_Chromatograms")
      )
    ),
    class = "DataTypes"
  )

  if (!is.null(data_type)) {
    if (!(data_type %in% res$types)) {
      stop(paste0("Data type '", data_type, "' is not recognized. Available types are: ", paste(res$types, collapse = ", "), "."))
    }
    return(list(
      engines = res$engines[[data_type]],
      file_formats = res$file_formats[[data_type]],
      results = res$results[[data_type]]
    ))
  }
}

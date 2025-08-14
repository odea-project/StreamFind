#' @noRd
DataTypes <- function() {
  structure(
    list(
      "types" = c("MassSpec", "Raman", "Statistic"),
      file_formats = list(
        "MassSpec" = c("mzML", "mzXML", "d", "raw"),
        "Raman" = c("asc", "sif", "json", "wdf", "sdf", "csv", "txt"),
        "Statistic" = c("csv")
      ),
      results = list(
        "MassSpec" = c("MassSpecResults_NonTargetAnalysis", "MassSpecSpectra", "Chromatograms"),
        "Raman" = c("RamanResults_Spectra"),
        "Statistic" = c("StatisticResults")
      )
    ),
    class = "DataTypes"
  )
}
